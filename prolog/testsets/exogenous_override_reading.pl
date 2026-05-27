% ============================================================================
% CONSTRAINT STORY: exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exogenous_override_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: exogenous_override_reading
 *   human_readable: Exogenous Override Reading: State Decree Displaces Prior Practice Through Legal Authority
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint models the state's deployment of legal authority to
 *   displace entrenched prior practices (calendar systems, dress codes, land
 *   tenure, language, or ritual norms). The exogenous override reading
 *   asserts that state decree authority is sufficient to achieve practice
 *   displacement, with compliance following from legal mandate regardless of
 *   whether affected populations internalize the new norms. This reading
 *   stands in contrast to endogenous readings (that practice change requires
 *   gradual internalization and adaptation) and hybrid readings (that decree
 *   works best when scaffolded with social engagement). The constraint
 *   exhibits Tangled Rope classification at the primary level because it
 *   combines a genuine coordination function (establishing uniform state
 *   standards) with asymmetric extraction (imposing adjustment costs on
 *   populations without consultation). The measurement trajectory shows
 *   extractiveness rising sharply during implementation (years 0-6) then
 *   plateauing as enforcement stabilizes (years 6-10). Theater ratio
 *   increases over time as the state shifts from forced compliance to
 *   performative compliance, suggesting that initial coercive enforcement
 *   gives way to institutionalized accommodation where populations comply
 *   publicly while preserving prior practice privately.
 *
 * KEY AGENTS:
 *   - State Modernization Apparatus: Primary beneficiary (institutional/arbitrage) — captures coordinating function and standardization gains; experiences decree as efficiency mechanism
 *   - Rural Populations: Primary victims (powerless/trapped) — absorb adjustment costs, lose agency in norm-setting, face enforcement penalties for non-compliance
 *   - Prior Practice Institutions: Secondary victims/Piton actors (institutional/mobile) — formally displaced but persist through workarounds and performative accommodation
 *   - Local Administrative Intermediaries: Constrained moderate actors — translate decree into local implementation; experience both coordination and enforcement burden
 *   - Organized Resistance: Organized moderate actors — coordinate collective defense against decree imposition; experience extraction pressure but retain agency through coalition
 *   - Analytical Observer: Civilizational analytical position — risks naturalizing contingent state authority as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exogenous_override_reading, 0.58).
domain_priors:suppression_score(exogenous_override_reading, 0.68).
domain_priors:theater_ratio(exogenous_override_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exogenous_override_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(exogenous_override_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(exogenous_override_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(exogenous_override_reading, "Exogenous Override Reading: State Decree Displaces Prior Practice Through Legal Authority").
narrative_ontology:topic_domain(exogenous_override_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exogenous_override_reading, '087609a9-d36b-4803-b66b-f77fde8351ae').
narrative_ontology:cs_created_at('087609a9-d36b-4803-b66b-f77fde8351ae', '').
narrative_ontology:cs_kernel_codification('087609a9-d36b-4803-b66b-f77fde8351ae', formalized).
narrative_ontology:cs_authority_grounding('087609a9-d36b-4803-b66b-f77fde8351ae', extraction).
narrative_ontology:cs_interpretation_layer_present('087609a9-d36b-4803-b66b-f77fde8351ae').
narrative_ontology:cs_kernel_id(exogenous_override_reading, legitimacy_of_imposed_practice).
narrative_ontology:cs_reading_relation('087609a9-d36b-4803-b66b-f77fde8351ae', endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('087609a9-d36b-4803-b66b-f77fde8351ae', hybrid_scaffolding_reading, influences).
narrative_ontology:cs_axiom('087609a9-d36b-4803-b66b-f77fde8351ae', foundational, decree_sufficiency_without_internalization).
narrative_ontology:cs_axiom_status(decree_sufficiency_without_internalization, holdable).
narrative_ontology:cs_axiom_grounding('087609a9-d36b-4803-b66b-f77fde8351ae', decree_sufficiency_without_internalization, empirically_contingent).
narrative_ontology:cs_axiom('087609a9-d36b-4803-b66b-f77fde8351ae', foundational, enforcement_mechanism_primary_displacement_vector).
narrative_ontology:cs_axiom_status(enforcement_mechanism_primary_displacement_vector, holdable).
narrative_ontology:cs_axiom_grounding('087609a9-d36b-4803-b66b-f77fde8351ae', enforcement_mechanism_primary_displacement_vector, instrumental).
narrative_ontology:cs_reference_frame('087609a9-d36b-4803-b66b-f77fde8351ae', state_legal_supremacy_framework).
narrative_ontology:cs_drift_state('087609a9-d36b-4803-b66b-f77fde8351ae', contemporary_postcolonial_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exogenous_override_reading, state_modernization_apparatus).
narrative_ontology:constraint_victim(exogenous_override_reading, rural_populations_bearing_adjustment_costs).
narrative_ontology:constraint_victim(exogenous_override_reading, prior_practice_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL POPULATIONS (SNARE) — Face legal mandate displacing established practice (e.g., calendar change, dress codes, land tenure norms) with no prior consultation or adjustment period. Trapped by legal enforcement and economic dependency on state authority. Maximum experienced extraction: absorb adjustment costs while benefiting groups see modernization gains. No meaningful exit option.
constraint_indexing:constraint_classification(exogenous_override_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL ADMINISTRATORS (TANGLED ROPE) — Must translate state decree into local practice while managing compliance resistance. Constrained by dual accountability (upward to state, outward to communities). Experience both coordination (translating decree into workable local norms) and extraction (enforcement burden, local resentment, career risk for failed implementation). Significant but not maximal extraction.
constraint_indexing:constraint_classification(exogenous_override_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE MODERNIZATION APPARATUS (ROPE) — Experiences decree as coordination mechanism: legal mandate efficiently displaces prior practice, establishes uniform standards, integrates periphery into state administrative framework. Net beneficiary with arbitrage options (can moderate enforcement, exempt loyal regions, adjust timing by locale). Extraction runs toward this agent; they experience the constraint as solving coordination problems.
constraint_indexing:constraint_classification(exogenous_override_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PRIOR PRACTICE INSTITUTIONS (PITON) — Religious authorities, craft guilds, or traditional governing councils that historically legitimated the superseded practice. Decree formally displaces their authority, but many persist through performative accommodation (officially comply while preserving practice through workarounds, secret transmission, or rebranding). Theater ratio high (public compliance, private continuity). Function attenuated but structure persists through inertia.
constraint_indexing:constraint_classification(exogenous_override_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ORGANIZED RESISTANCE (TANGLED ROPE) — Regional leaders, religious authorities, or merchants organized to resist decree imposition coordinate defensive mechanisms (non-compliance, parallel institutions, concealment). Experience mixed extraction (enforcement pressure) and coordination (organizing collective defense). Constrained by state power differential but retain agency through collective action. Coordination function is resistance, not acceptance.
constraint_indexing:constraint_classification(exogenous_override_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — State decree authority is treated as a fixed, immutable feature of state capacity: legal mandate sufficient to displace practice regardless of internalization, rational choice, or cultural adaptation. From civilizational scope, state authority appears as natural law governing political structures. However, structural data reveals this as a false summit: the decree's effectiveness depends on enforcement capacity, internalization rate, and absence of organized resistance — all contingent factors that vary dramatically across cases. State authority is not inherent; its sufficiency is constructed.
constraint_indexing:constraint_classification(exogenous_override_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exogenous_override_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(exogenous_override_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(exogenous_override_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(exogenous_override_reading, TR),
    TR >= 0.70.

:- end_tests(exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The decree imposes adjustment costs on rural populations and prior-practice institutions while benefiting state modernization apparatus. The extractiveness is not maximal (0.70+) because decree authority does generate coordination benefits: establishing uniform standards, integrating administrative periphery, creating predictable institutional frameworks. However, these benefits flow primarily to the state apparatus, not to affected populations. The measurement trajectory (0.35 → 0.62 → 0.58) reflects rising extraction during implementation as enforcement effort peaks, then plateauing as the new regime stabilizes and resistance costs settle into permanent baseline. Suppression (0.68): Moderate-high. State authority backed by legal enforcement, threat of penalty, and withdrawal of state services creates substantial barriers to continued prior practice. However, suppression is not total (0.85+) because clandestine practice persists and organized resistance can negotiate enforcement terms. The suppression is enforcement-dependent: where state presence is sparse (remote regions, maritime areas, mountains), suppression is weaker. Theater ratio (0.52): Moderate. Early enforcement is predominantly functional (genuine attempt to displace practice through penalty and disruption). Over time, theater increases as the state shifts to performative compliance models — populations formally adopt new norms while preserving prior practice in unsurveilled contexts. The state benefits from public compliance (which signals control) even without internalization.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the state apparatus (Rope) and rural populations (Snare) is maximal: the same constraint appears as coordination solution to one party and pure extraction to the other. The state sees legal mandate as sufficient mechanism; populations see it as coercive imposition. Prior-practice institutions see degradation through piton mechanism: formally displaced but persisting through performative accommodation (public compliance, private preservation). Organized resistance sees mixed extraction and coordination (Tangled Rope from their perspective: organizing collective defense is coordination function; state enforcement is extraction pressure). Local administrators experience the constraint as Tangled Rope: translating decree is genuine coordination; enforcement burden is extraction. The analytical observer risks a false summit (Mountain): naturalizing contingent institutional imposition as immutable feature of state capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from agents' structural positions relative to this specific constraint. State modernization apparatus benefits from decree authority and experiences low extraction (high arbitrage options for adjusting enforcement pace, exempting allies, modulating penalties). Rural populations and prior-practice institutions are structural targets: they lack exit options and internalize no benefits from the new regime. Local administrators occupy a middle position: constrained by dual accountability, they neither fully benefit nor fully bear costs. Organized resistance has exit via coalition (agency through collective action). The analytical observer at civilizational scope risks seeing the decree as natural law (Mountain) rather than contingent institutional imposition. The engine's false summit detector identifies this as naturalization: the decree's sufficiency depends on enforcement capacity, resistance absence, and state infrastructure — all contingent factors varying dramatically across historical cases. State authority is not inherent to political structure; its sufficiency is constructed and contestable.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (whether this is mixed coordination or pure extraction) is resolved by distinguishing the beneficiary's experience (Rope: decree is coordination mechanism) from the victim's experience (Snare: decree is extraction mechanism) and recognizing that BOTH are structurally true. The state genuinely coordinates standards through decree authority. The rural populations genuinely bear extraction costs. The constraint is Tangled Rope because it contains both functions simultaneously, with asymmetric distribution: beneficiaries receive coordination benefit, victims receive extraction cost. The exogenous override reading does not assume away the mandatrophy; it addresses a different question: whether decree authority is SUFFICIENT for practice displacement, not whether the displacement is just or efficient.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_threshold,
    'What minimum enforcement capacity is required for decree authority to suffice for practice displacement, and does this threshold vary by practice type (calendar vs. dress vs. land tenure)?',
    'Historical comparative analysis: cases where decree enforcement succeeded (calendar change in Orthodox Russia 1918, metric system in France 1795) vs. failed (dress codes in Ottoman periphery, land reform in colonial settings). Quantify enforcement investment (troops, inspectors, penalties) required per compliance rate achieved.',
    'If threshold is very low: decree sufficiency is robust (reading holds across settings). If threshold is high and practice-dependent: decree sufficiency is conditional on enforcement infrastructure (reading applies only to well-administered states with specific practice domains).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_threshold, empirical, 'Minimum enforcement capacity required for decree-driven practice displacement').

omega_variable(
    internalization_vs_compliance_gap,
    'Is the distinction between external compliance and internalized acceptance meaningful for evaluating whether decree displaces prior practice, or does the reading treat these as equivalent?',
    'Longitudinal ethnographic/administrative data: tracking compliance rates over generations; identifying divergence between public behavior (compliant) and private practice (resistant); measuring second-generation voluntary adoption rates.',
    'If meaningful gap exists: decree authority suffices for compliance but NOT for displacement (reading requires refinement — may be rope or piton rather than tangled_rope). If gap is immaterial: decree authority achieves functional displacement even without internalization (reading holds as stated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_vs_compliance_gap, conceptual, 'Whether distinction between compliance and internalization affects practice displacement assessment').

omega_variable(
    prior_practice_institutional_persistence,
    'When prior practice institutions persist through performative accommodation (public compliance, private preservation), has the decree actually displaced the practice or created a dual structure?',
    'Structural analysis of post-decree institution configuration: do prior-practice authorities continue to function (even clandestinely) or are they genuinely dissolved? Measure: frequency of enforcement actions against prior-practice practitioners; continuity of prior-authority legitimacy claims; frequency of practice performance in controlled (non-state-surveilled) settings.',
    'If genuine displacement: reading holds (decree authority sufficient). If dual structure: practice displacement is incomplete; decree authority establishes state monopoly on legitimate performance but not elimination of prior practice (reading understates contingency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prior_practice_institutional_persistence, empirical, 'Whether prior practice institutions are genuinely displaced or persists through performative accommodation').

omega_variable(
    reading_boundary_decree_vs_internalization,
    'Does this reading claim that legal mandate suffices WITHOUT internalization, and if so, what counts as evidence that the practice has been displaced rather than merely driven underground?',
    'Conceptual clarification followed by empirical test: define ''displacement'' operationally (no public performance? no institutional transmission? no intergenerational continuity?). Measure each definition against historical cases where decree succeeded and failed.',
    'If reading means decree eliminates public performance: high confidence (true for well-enforced decrees). If reading means decree eliminates practice entirely including clandestine continuity: very low confidence (almost never achieved). Affects whether reading is defensible or overreaches.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_decree_vs_internalization, conceptual, 'Operational definition of practice displacement and evidence required').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exogenous_override_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exog_tr_t0, exogenous_override_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(exog_tr_t3, exogenous_override_reading, theater_ratio, 3, 0.38).
narrative_ontology:measurement(exog_tr_t6, exogenous_override_reading, theater_ratio, 6, 0.55).
narrative_ontology:measurement(exog_tr_t10, exogenous_override_reading, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(exog_be_t0, exogenous_override_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(exog_be_t3, exogenous_override_reading, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(exog_be_t6, exogenous_override_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(exog_be_t10, exogenous_override_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(exogenous_override_reading, endogenous_climb_reading).
narrative_ontology:affects_constraint(exogenous_override_reading, hybrid_scaffolding_reading).

% DUAL FORMULATION NOTE:
% This constraint is ONE READING of the kernel 'legitimacy_of_imposed_practice'. The sibling readings (endogenous_climb, hybrid_scaffolding) are separate constraints with different ε values and different classification profiles. The exogenous override reading claims decree sufficiency; the endogenous reading claims decree necessity is insufficient without internalization; the hybrid reading claims decree plus social engagement is optimal. Each reading has its own structure story. They are linked via network.affects_constraints because they compete in the same policy domain and their relative empirical success determines which reading's authority becomes institutionalized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exogenous_override_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
