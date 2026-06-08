% ============================================================================
% CONSTRAINT STORY: temporal_validation_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temporal_validation_asymmetry, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: temporal_validation_asymmetry
 *   human_readable: Aneyoshi Stone Directive: Temporal Validation Asymmetry in Catastrophe-Derived Land-Use Constraint
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi stone directive represents a catastrophe-derived land-use
 *   constraint that operated without validation feedback across 78 years
 *   (1933-2011) and three generations. Following the 1933 Sanriku tsunami,
 *   survivors inscribed a stone marker: 'High dwellings are the peace and
 *   harmony of our descendants. Remember the calamity of the great tsunamis.
 *   Do not build your homes below this point.' The community maintained
 *   near-perfect compliance through 2011 despite no intervening tsunami
 *   validating the constraint. When the 2011 Tōhoku tsunami struck, Aneyoshi
 *   recorded zero casualties while nearby coastal communities suffered
 *   extensive losses. The constraint exhibits mountain characteristics from
 *   all perspectives during the inter-catastrophe period: negligible
 *   extraction (residents are beneficiaries, not victims), negligible
 *   suppression (no enforcement apparatus), low theater (the stone is
 *   functional, not performative), and near-complete accessibility collapse
 *   (building below the stone is not a considered alternative once the
 *   physical geography is understood). However, the constraint's
 *   classification as mountain with declared beneficiaries triggers the false
 *   summit detection protocol — the engine must evaluate whether this is a
 *   genuine natural law or a constructed norm that naturalizes a contingent
 *   cultural practice.
 *
 * KEY AGENTS:
 *   - Aneyoshi Residents (1933-2011): Primary beneficiaries (powerless to moderate / trapped to constrained) — avoided catastrophe through compliance; experienced the constraint as protective rather than extractive
 *   - Founding Generation (1933 survivors): Authority source (powerless / trapped) — direct catastrophe experience grounds the directive's legitimacy; their structural position (powerless, trapped by geography and economic ties) is critical to understanding why the constraint is not extractive
 *   - Community Elders Council: Organized transmission mechanism (organized / mobile) — maintained oral tradition and stone maintenance across generations; experienced the constraint as coordination
 *   - Regional Development Planners: Institutional observers (moderate to institutional / constrained to arbitrage) — saw Aneyoshi as successful case requiring no intervention
 *   - National Disaster Prevention Authority: Institutional beneficiary (institutional / arbitrage) — benefited from community self-governance reducing need for top-down enforcement
 *   - Analytical Observer: Civilizational perspective (analytical / analytical) — sees the constraint as encoding physical geography; risks naturalizing what may be contingent cultural practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_validation_asymmetry, 0.08).
domain_priors:suppression_score(temporal_validation_asymmetry, 0.12).
domain_priors:theater_ratio(temporal_validation_asymmetry, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_validation_asymmetry, extractiveness, 0.08).
narrative_ontology:constraint_metric(temporal_validation_asymmetry, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(temporal_validation_asymmetry, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temporal_validation_asymmetry, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(temporal_validation_asymmetry, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_validation_asymmetry, mountain).
narrative_ontology:human_readable(temporal_validation_asymmetry, "Aneyoshi Stone Directive: Temporal Validation Asymmetry in Catastrophe-Derived Land-Use Constraint").
narrative_ontology:topic_domain(temporal_validation_asymmetry, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(temporal_validation_asymmetry).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temporal_validation_asymmetry, '15a8af8f-729d-48f3-8255-833824e9dd88').
narrative_ontology:cs_kernel_codification('15a8af8f-729d-48f3-8255-833824e9dd88', fixed_text).
narrative_ontology:cs_authority_grounding('15a8af8f-729d-48f3-8255-833824e9dd88', lineage).
narrative_ontology:cs_interpretation_layer_present('15a8af8f-729d-48f3-8255-833824e9dd88').
narrative_ontology:cs_reading_relation('15a8af8f-729d-48f3-8255-833824e9dd88', temporal_validation_asymmetry__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('15a8af8f-729d-48f3-8255-833824e9dd88', foundational, catastrophe_norms_persist_through_transmission).
narrative_ontology:cs_axiom_status(catastrophe_norms_persist_through_transmission, holdable).
narrative_ontology:cs_axiom_grounding('15a8af8f-729d-48f3-8255-833824e9dd88', catastrophe_norms_persist_through_transmission, empirically_contingent).
narrative_ontology:cs_axiom('15a8af8f-729d-48f3-8255-833824e9dd88', secondary, physical_geography_determines_safe_settlement).
narrative_ontology:cs_axiom_status(physical_geography_determines_safe_settlement, holdable).
narrative_ontology:cs_axiom_grounding('15a8af8f-729d-48f3-8255-833824e9dd88', physical_geography_determines_safe_settlement, empirically_contingent).
narrative_ontology:cs_reference_frame('15a8af8f-729d-48f3-8255-833824e9dd88', catastrophe_derived_prohibition_1933).
narrative_ontology:cs_drift_state('15a8af8f-729d-48f3-8255-833824e9dd88', pre_2011_validation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('15a8af8f-729d-48f3-8255-833824e9dd88', '2025-01-09T00:00:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_validation_asymmetry, aneyoshi_residents_1933_2011).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temporal_validation_asymmetry, community_elders_council).
narrative_ontology:constraint_beneficiary(temporal_validation_asymmetry, national_disaster_prevention_authority).
narrative_ontology:constraint_vindicates(temporal_validation_asymmetry, catastrophe_derived_norms_persist_without_validation).
narrative_ontology:constraint_vindicates(temporal_validation_asymmetry, physical_geography_determines_safe_settlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Residents of Aneyoshi village across three generations (1933-2011) who maintained compliance with the stone directive. Structurally powerless (no control over tsunami risk or regional development policy) and trapped (limited economic mobility, tied to local fishing and agriculture). The stone directive protects them from catastrophe rather than extracting from them — they are beneficiaries despite their powerless/trapped position. Experienced the constraint as immutable: 'do not build below this point' was internalized as physical geography, not policy. The 2011 tsunami validated the constraint — Aneyoshi recorded zero casualties while nearby communities suffered extensive losses.
narrative_ontology:constraint_stakeholder(temporal_validation_asymmetry, aneyoshi_residents_1933_2011, beneficiary,
    powerless, biographical, trapped, local).

% Survivors of the 1933 Sanriku tsunami who inscribed the stone marker and established the land-use prohibition. Structurally powerless (no institutional authority, no economic resources) and trapped (could not relocate after catastrophe). Their direct catastrophe experience grounds the directive's authority for descendants. They set the agenda (inscribed the stone, established the norm) but did not extract from it — they are beneficiaries of their own coordination mechanism. Their powerless/trapped position is critical to understanding why the constraint is not extractive: they had no capacity to extract rents from the prohibition.
narrative_ontology:constraint_stakeholder(temporal_validation_asymmetry, founding_generation_1933_survivors, agenda_setter,
    powerless, immediate, trapped, local).

% Organized group maintaining oral tradition, stone maintenance, and norm transmission across generations (1933-2011). Mobile exit options (elders can relocate) but choose to stay. Dual role: agenda-setters (maintain the tradition, socialize younger generations) and beneficiaries (protected from catastrophe, benefit from community cohesion). Experience the constraint as coordination mechanism: the stone solves the collective action problem of transmitting catastrophe knowledge across the validation gap. Bear the coordination cost (time, effort, social pressure to maintain compliance) but benefit from the life-saving function.
narrative_ontology:constraint_stakeholder(temporal_validation_asymmetry, community_elders_council, agenda_setter,
    organized, generational, mobile, local).
narrative_ontology:stakeholder_secondary_role(temporal_validation_asymmetry, community_elders_council, beneficiary).

% Regional planners working across multiple coastal communities (1960s-2000s). Constrained by regional development pressures and competing land-use demands. Observe Aneyoshi as successful case of community-level resilience requiring no institutional intervention. Neither collect from nor pay into the constraint — it is self-maintaining. Experience the constraint as coordination around physical geography: the stone codifies what topography already dictates.
narrative_ontology:constraint_stakeholder(temporal_validation_asymmetry, regional_development_planners, observer,
    moderate, generational, constrained, regional).

% National-level disaster prevention institution observing Aneyoshi and similar communities (1950s-2011). Arbitrage exit — can redirect resources to communities with weaker norms. Benefits from Aneyoshi's self-governance: the community's compliance reduces the need for top-down enforcement, freeing institutional resources for other areas. Does not extract from the constraint — the constraint aligns with institutional goals (catastrophe prevention) and operates without institutional cost.
narrative_ontology:constraint_stakeholder(temporal_validation_asymmetry, national_disaster_prevention_authority, beneficiary,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone directive solves the collective action problem of transmitting catastrophe knowledge across generational timescales when validation events are rare. Without the stone and oral tradition, each generation would need to independently discover the tsunami risk through direct experience (catastrophic) or infer it from topography alone (unreliable, as nearby communities without stones failed to maintain safe settlement patterns).
% TRANSFER_FUNCTION: The constraint transfers catastrophe-prevention knowledge from the founding generation (1933 survivors) to descendants across 78 years. It also transfers opportunity cost: residents forgo economically rational coastal development (closer to fishing grounds, more productive agricultural land) in exchange for catastrophe avoidance. However, the transfer is protective rather than extractive — residents are net beneficiaries.
% ABSENT_VOICES: Potential coastal developers or fishing industry interests who might have benefited from denser coastal settlement are absent from the Aneyoshi governance structure. However, there is no evidence these voices were suppressed — the community's small size and economic structure (subsistence fishing and agriculture) meant no organized development interest existed. The absent voice is hypothetical rather than excluded.
% DISAPPEARANCE_RATIONALE: If the stone directive disappeared overnight (stone destroyed, oral tradition lost), settlement patterns would rearrange. Comparative evidence: nearby communities without tsunami stones developed denser coastal settlement and suffered extensive casualties in 2011. Economic rationality (proximity to fishing grounds, flat coastal land) would drive development below the historical high-water mark absent the cultural constraint. The 2011 event provides strong counterfactual evidence: Aneyoshi's zero casualties vs nearby losses demonstrates that arrangements (settlement patterns) depend on the constraint.
% FOUNDING_PROBLEM: The founding problem was catastrophe-knowledge transmission across generational timescales when validation events (tsunamis) are rare. The 1933 Sanriku tsunami killed thousands in nearby communities. Survivors recognized that future generations would not have direct experience of the catastrophe and would face economic pressure to develop coastal land. The stone directive was built to solve this: encode the catastrophe-derived prohibition in a physical marker and oral tradition that would persist across the validation gap.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (catastrophe-knowledge transmission across rare validation events) remains live. The 2011 Tōhoku tsunami demonstrated that the problem persists: communities without effective transmission mechanisms (no stones, degraded oral traditions) suffered extensive casualties despite modern tsunami warning systems. Corroboration sources: (1) Disaster anthropology research (Yamori 2013, Suppasri et al. 2015) documenting differential outcomes based on cultural memory; (2) Aneyoshi residents interviewed post-2011 who explicitly credited the stone directive for their survival; (3) Regional disaster prevention authorities who identified cultural transmission as critical factor in differential outcomes. Notably, the corroboration comes from outside the beneficiary set (academic researchers, regional authorities) as well as from beneficiaries (residents), strengthening the claim that the founding problem is genuinely live rather than a cover story.
narrative_ontology:disappearance_verdict(temporal_validation_asymmetry, world_rearranges).
narrative_ontology:founding_problem_status(temporal_validation_asymmetry, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL RESIDENT BIOGRAPHICAL VIEW (MOUNTAIN) — Resident born after 1933, living through the inter-catastrophe period. The stone directive appears as immutable constraint: 'do not build below this point' is experienced as physical geography, not policy. No extraction perceived — the constraint protects rather than extracts. Trapped by local economic ties but the constraint itself is not the trap.
constraint_indexing:constraint_classification(temporal_validation_asymmetry, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REGIONAL PLANNER GENERATIONAL VIEW (MOUNTAIN) — Planner working across multiple coastal communities 1960s-2000s. The Aneyoshi stone directive is one of many tsunami markers; compliance is near-universal without enforcement. Experiences the constraint as coordination around physical geography — the stone codifies what the topography already dictates. Constrained by regional development pressures but sees the directive as immutable baseline.
constraint_indexing:constraint_classification(temporal_validation_asymmetry, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NATIONAL AUTHORITY GENERATIONAL VIEW (MOUNTAIN) — National-level disaster prevention institution observing Aneyoshi and similar communities 1950s-2011. The stone directive is a successful case of community-level resilience requiring no institutional intervention. Arbitrage exit — can redirect resources to communities with weaker norms. Experiences negligible extraction — the constraint is self-maintaining and aligns with institutional goals.
constraint_indexing:constraint_classification(temporal_validation_asymmetry, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER CIVILIZATIONAL VIEW (MOUNTAIN) — From a civilizational perspective spanning multiple catastrophe cycles, the constraint appears as physical geography constraint: tsunami inundation zones are determined by bathymetry, coastal morphology, and seismic characteristics. The stone directive is a cultural encoding of a natural law. Accessibility collapse is near-complete — once the physical geography is understood, building below the stone is not a viable alternative. Resistance is negligible — the 2011 validation event confirmed zero casualties in compliant areas.
constraint_indexing:constraint_classification(temporal_validation_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 5: COMMUNITY ELDERS GENERATIONAL VIEW (ROPE) — Organized group maintaining oral tradition and stone maintenance across generations. Experiences the constraint as coordination mechanism: the stone solves the collective action problem of transmitting catastrophe knowledge across the validation gap. Mobile exit — elders can relocate but choose to stay. Low extraction — the coordination function (preserving life-saving knowledge) dominates any cost of maintaining the tradition.
constraint_indexing:constraint_classification(temporal_validation_asymmetry, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_validation_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_validation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_validation_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(temporal_validation_asymmetry, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_validation_asymmetry, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temporal_validation_asymmetry, ExtMetricName, E),
    domain_priors:suppression_score(temporal_validation_asymmetry, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temporal_validation_asymmetry),
    narrative_ontology:constraint_metric(temporal_validation_asymmetry, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temporal_validation_asymmetry, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temporal_validation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The constraint protects residents from catastrophe rather than extracting from them. The slight non-zero value reflects opportunity cost of elevated land (reduced agricultural productivity, longer access to fishing grounds) and the cognitive load of maintaining the tradition. However, these costs are negligible compared to the catastrophe-avoidance benefit, and residents are net beneficiaries. The value increased slightly during the inter-catastrophe period (0.05 to 0.09) as generational distance from 1933 reduced the salience of the threat, making the opportunity costs more noticeable, then decreased slightly after 2011 validation. Suppression (0.12): Very low. No formal enforcement apparatus existed. Compliance was maintained through cultural transmission, community social pressure, and internalized norms. The non-zero value reflects that deviation from the norm would face social sanction, but this is mild compared to legal or economic coercion. Theater ratio (0.15): Very low. The stone directive is functional — it encodes life-saving information and compliance demonstrably prevents casualties. The slight non-zero value reflects that stone maintenance rituals acquired some ceremonial overlay during the inter-catastrophe period (annual cleaning, school visits), but the core function (land-use constraint) remained operational. The theater ratio increased modestly from 0.10 (1933) to 0.16 (1993) as commemorative practices accumulated, then decreased slightly to 0.15 (2011) as the validation event re-emphasized function over ceremony. Accessibility collapse (0.92): Near-complete. Once the physical geography and tsunami risk are understood, building below the stone is not a viable alternative — the constraint forecloses the option almost entirely. The non-1.0 value reflects that economic pressure or generational memory decay could theoretically erode compliance. Resistance (0.08): Negligible. The constraint met almost no active resistance during 1933-2011. The non-zero value reflects minor tensions (some residents questioned the restriction during periods of land scarcity) but no organized opposition.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap during the inter-catastrophe period — all perspectives classify as mountain or rope, with no snare or tangled_rope perspectives. The powerless coastal resident, the moderate regional planner, the institutional national authority, and the analytical observer all experience the constraint as immutable or near-immutable, with negligible extraction. The only non-mountain perspective is the organized community elders, who see rope (coordination mechanism for transmitting catastrophe knowledge). This near-uniform classification is itself diagnostic: it suggests either (1) a genuine natural law where all observers converge on the same structural reality, or (2) a false summit where a constructed norm has been so successfully naturalized that even powerless agents experience it as immutable. The false summit detection protocol evaluates this by checking for beneficiary presence (present: Aneyoshi residents 1933-2011) and examining whether the constraint's persistence depends on cultural transmission (yes: the stone and oral tradition are necessary) or would occur from physical geography alone (unclear: the omega variable on economic pressure counterfactual addresses this). The 2011 validation event provides partial resolution — the constraint's function was vindicated, but this does not settle whether it was always a natural law or became one through 78 years of successful cultural transmission.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for this constraint is unusual because the primary 'victims' of the constraint (residents restricted from coastal development) are actually its beneficiaries (residents protected from tsunami). This creates a negative or near-zero directionality for all resident perspectives — the constraint flows toward them (protection) rather than away from them (extraction). The founding generation (powerless/trapped) has d ≈ 0.0 — they are full beneficiaries despite their structural powerlessness. Later generations (powerless to moderate / trapped to constrained) have slightly higher d values (0.05-0.10) as generational distance reduces the perceived benefit, but they remain net beneficiaries. The community elders (organized/mobile) have d ≈ 0.1 — they bear the coordination cost of maintaining the tradition but benefit from the life-saving function. Institutional actors (moderate to institutional / constrained to arbitrage) have d ≈ 0.0 to -0.1 — they are beneficiaries of the community's self-governance. The analytical observer (analytical/analytical) has d ≈ 0.0 — no extraction experienced. The absence of victims and the universal beneficiary structure is what triggers the false summit detection protocol — a mountain with beneficiaries requires omega documentation of the natural-law vs constructed-norm ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy in the traditional sense (a coordination mechanism whose original function has been superseded but which persists as extraction). Instead, it exhibits the opposite pattern: a constraint whose function (catastrophe prevention) was never superseded and was dramatically vindicated by the 2011 event. However, the constraint does raise a mandatrophy-adjacent question: during the 78-year inter-catastrophe period, was the constraint's function latent (waiting for validation) or active (preventing development that would have otherwise occurred)? If the former, the constraint was a scaffold with an implicit sunset (it would have decayed without validation). If the latter, the constraint was a mountain throughout (it continuously prevented casualties by preventing exposure). The 2011 event truncated this question — we cannot observe what would have happened at year 100 or 150 without validation. The mandatrophy analysis here is about temporal validation asymmetry: how do we classify a constraint whose function is catastrophe-prevention when catastrophes are rare? The constraint's low theater ratio and low extractiveness during the inter-catastrophe period suggest it was not a piton (degraded function maintained theatrically) but rather a mountain or scaffold whose function was latent but real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_with_beneficiaries_ambiguity,
    'Is the Aneyoshi stone directive a genuine natural law (physical geography constraint) or a constructed social norm that benefits identifiable agents (residents who avoided catastrophe)?',
    'Counterfactual analysis: Would compliance have occurred without the stone? Comparative analysis: Do communities without tsunami stones show similar settlement patterns based on topography alone? The 2011 event provides partial resolution — Aneyoshi had zero casualties while nearby communities without stones suffered extensive losses, suggesting the stone encoded information not derivable from topography alone.',
    'If genuine natural law: Mountain classification holds; beneficiaries are incidental (everyone benefits from not building in tsunami zones). If constructed norm: False summit — the constraint naturalizes a contingent cultural practice that happened to align with physical geography, and the beneficiary structure (survivors) is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_with_beneficiaries_ambiguity, conceptual, 'Natural law vs constructed norm with beneficiary structure').

omega_variable(
    validation_event_necessity,
    'Could the stone directive have persisted indefinitely without the 2011 validation event, or was decay inevitable across generational timescales?',
    'Longitudinal study of other tsunami stones across Japan with varying inter-catastrophe durations. Measurement of compliance rates vs time-since-last-tsunami. The 2011 event truncated the natural experiment — we cannot observe what would have happened at year 100 or 150 without validation.',
    'If decay inevitable: The constraint was always a scaffold with an implicit sunset (generational memory limits). If persistence indefinite: The constraint is a genuine mountain (cultural transmission can preserve catastrophe-derived norms without reinforcement).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(validation_event_necessity, empirical, 'Whether validation events are necessary for norm persistence').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the physical stone inscription (fixed text) or the catastrophe-derived prohibition it encodes (distributed practice)? Does authority ground in the founding generation''s direct experience (lineage) or in the physical geography the stone points to (self-enforcing)?',
    'Comparative analysis of communities where stones were destroyed or moved vs communities where stones remained. If compliance persists without the physical stone, the kernel is the distributed prohibition and authority is self-enforcing (physical geography). If compliance decays when stones are removed, the kernel is the fixed text and authority is lineage-based.',
    'Fixed-text + lineage framing: The stone is a commitment system with interpretive drift risk (commemorative husk reading becomes more plausible). Distributed + self-enforcing framing: The stone is a coordination device for a natural law (behavioral competence reading becomes more plausible). The choice of framing determines which reading''s structural delta is more defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Kernel and authority framing alternatives in CS structure').

omega_variable(
    economic_pressure_counterfactual,
    'Would economically rational coastal development have occurred in Aneyoshi during 1933-2011 absent the stone directive?',
    'Comparative analysis of nearby communities with similar topography and economic conditions but without tsunami stones. Measurement of coastal development density vs distance from historical high-water marks. Economic modeling of land value differentials between coastal and elevated parcels.',
    'If development would have occurred: The stone directive suppressed economically rational behavior, increasing extractiveness and challenging the mountain classification. If development would not have occurred: The stone merely codified what topography and local knowledge already dictated, supporting the mountain classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_pressure_counterfactual, empirical, 'Whether economic incentives would have driven non-compliant development').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_validation_asymmetry, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_theater_1933, temporal_validation_asymmetry, theater_ratio, 0, 0.1).
narrative_ontology:measurement(aneyoshi_theater_1953, temporal_validation_asymmetry, theater_ratio, 20, 0.12).
narrative_ontology:measurement(aneyoshi_theater_1973, temporal_validation_asymmetry, theater_ratio, 40, 0.14).
narrative_ontology:measurement(aneyoshi_theater_1993, temporal_validation_asymmetry, theater_ratio, 60, 0.16).
narrative_ontology:measurement(aneyoshi_theater_2011, temporal_validation_asymmetry, theater_ratio, 78, 0.15).

% Extraction over time
narrative_ontology:measurement(aneyoshi_extract_1933, temporal_validation_asymmetry, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(aneyoshi_extract_1953, temporal_validation_asymmetry, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(aneyoshi_extract_1973, temporal_validation_asymmetry, base_extractiveness, 40, 0.07).
narrative_ontology:measurement(aneyoshi_extract_1993, temporal_validation_asymmetry, base_extractiveness, 60, 0.09).
narrative_ontology:measurement(aneyoshi_extract_2011, temporal_validation_asymmetry, base_extractiveness, 78, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_validation_asymmetry, information_standard).

% DUAL FORMULATION NOTE:
% The Aneyoshi stone directive is a single constraint, not a decomposed family. However, it is one reading of a contested kernel (aneyoshi_stone_directive) with a sibling reading (commemorative_husk_reading) that would be authored as a separate constraint story with substantially different epsilon and beneficiary structure. The two readings are mutually exclusive interpretations of the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
