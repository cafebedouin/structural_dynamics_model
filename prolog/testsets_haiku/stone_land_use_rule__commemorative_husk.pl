% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__commemorative_husk, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Stone Memorial Land-Use Rule (Commemorative Husk Reading)
 *   domain: disaster_anthropology/institutional_memory/land_governance
 *
 * SUMMARY:
 *   A stone memorial marks a historical disaster zone. Originally, a formal
 *   land-use rule prohibited building within a designated perimeter to
 *   preserve the space as commemorative. Over decades, the rule has decayed
 *   into a symbolic gesture: it persists in zoning code and historical
 *   commission oversight, but development interests routinely obtain
 *   variances, the stone's authority is invoked as justification for
 *   exceptions rather than as an enforcer of restrictions, and the landscape
 *   has filled with buildings near the stone. This story instantiates the
 *   COMMEMORATIVE_HUSK reading of the contested kernel: the stone's function
 *   is now entirely performative — maintaining memory through ritual (annual
 *   ceremonies, historical society stewardship) rather than through spatial
 *   control. The behavioral_competence sibling reading treats the stone as a
 *   live land-use prohibition where daily spatial practice enforces
 *   compliance; this reading treats it as an atrophied enforcement mechanism
 *   maintained theatrically. These are structurally distinct constraints with
 *   different epsilon values and different stakeholder situations.
 *
 * KEY AGENTS:
 *   - waterfront_development_interests: primary beneficiary; extracts deference via variances; nominally constrained but practically mobile
 *   - historical_commission: agenda setter; administers the rule and performs ceremonial upkeep; lacks enforcement capacity
 *   - descendants_and_remembrance_community: payer (loses quiet memorial space to development); beneficiary (the stone's existence vindicates their memory claims)
 *   - land_use_planners: payer (absorb friction from competing directives); moderate power
 *   - general_public: observer; unaware of the rule; experience stone as artifact only
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.71).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.38).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.71).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Stone Memorial Land-Use Rule (Commemorative Husk Reading)").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, 'e2d5147e-37a4-4e7d-a49d-68e7a18319e2').
narrative_ontology:cs_kernel_codification('e2d5147e-37a4-4e7d-a49d-68e7a18319e2', fixed_text).
narrative_ontology:cs_authority_grounding('e2d5147e-37a4-4e7d-a49d-68e7a18319e2', extraction).
narrative_ontology:cs_interpretation_layer_present('e2d5147e-37a4-4e7d-a49d-68e7a18319e2').
narrative_ontology:cs_reading_relation('e2d5147e-37a4-4e7d-a49d-68e7a18319e2', stone_land_use_rule__behavioral_competence, forecloses).
narrative_ontology:cs_axiom('e2d5147e-37a4-4e7d-a49d-68e7a18319e2', foundational, memory_maintenance_via_symbol_not_enforcement).
narrative_ontology:cs_axiom_status(memory_maintenance_via_symbol_not_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('e2d5147e-37a4-4e7d-a49d-68e7a18319e2', memory_maintenance_via_symbol_not_enforcement, deontological).
narrative_ontology:cs_axiom('e2d5147e-37a4-4e7d-a49d-68e7a18319e2', secondary, spatial_control_authority_dissolved_to_narrative_authority).
narrative_ontology:cs_axiom_status(spatial_control_authority_dissolved_to_narrative_authority, holdable).
narrative_ontology:cs_axiom_grounding('e2d5147e-37a4-4e7d-a49d-68e7a18319e2', spatial_control_authority_dissolved_to_narrative_authority, conventional).
narrative_ontology:cs_reference_frame('e2d5147e-37a4-4e7d-a49d-68e7a18319e2', spatial_land_use_prohibition).
narrative_ontology:cs_drift_state('e2d5147e-37a4-4e7d-a49d-68e7a18319e2', contemporary_post_development_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2d5147e-37a4-4e7d-a49d-68e7a18319e2', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_development_interests).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, civic_memory_institutions).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio (0.82 at interval end) is very high: the rule's primary function is now ceremonial rather than spatial. Annual remembrance events, historical commission meeting minutes, and symbolic objections to development are the enacted performances; actual zoning decisions proceed with development near the stone. Extractiveness is high (0.71) and rising because the rule extracts deference (time, legitimacy, symbolic respect) from planners and remembrance communities while granting development interests arbitrage access (build via variance, cite the stone's commemorative value as justification, frame the waiver as respect for memory). Suppression is low (0.38) because there is no active suppressive machinery — the rule persists by inertia and administrative habit, not by enforcement. Accessibility_collapse is very low (0.25): alternatives are wholly accessible; developers exit whenever they choose via variance. Resistance is high (0.72) because remembrance communities and planners actively object to development, hold ceremonies, and maintain the stone — their resistance is structural rather than suppressed, which is diagnostic of a piton (the constraint persists despite active resistance, not because it overcomes resistance). The measurement series traces the decay: theater ratio rises as the performative fraction grows, extractiveness rises as development interests gain practiced arbitrage over the rule, and the regime stabilizes at a high-theater steady state by t=42. One shared time grid: every measurement point is authored for every metric at the same seven waypoints plus start and end.
 *
 * PERSPECTIVAL GAP:
 *   Development interests and the historical commission should compute very differently from this structure. Developers experience the stone rule as a nominal constraint that is routinely waived — high exit options, therefore low directionality toward extraction. The historical commission experiences it as their mandate and identity — constrained, performing the rule even as it erodes, therefore moderate directionality toward the extraction (they bear the cost of the rule's symbolic maintenance). Planners sit between: nominally administering the rule, practically accommodating development, absorbing friction from both sides. The engine computes each seat's type from power + exit + beneficiary/victim status; the authored claim (piton) reflects the structural fact that no seat is hurt enough or benefits enough to sustain the rule — the historical commission would preserve it, but lacks power; developers benefit from its decay, and have power; planners administer it, but are constrained by competing directives. A piton is exactly this: no concentrated beneficiary capturing the extraction; no concentrated victim suffering enough to fix it; maintenance is administrative inertia and performance rather than active interest.
 *
 * DIRECTIONALITY LOGIC:
 *   Waterfront developers: structural beneficiary (gain arbitrage, development flexibility). Directionality low (beneficiary end). Historical commission: payer (expend effort maintaining a rule with no teeth) and nominal administrator (set the rule). Directionality moderate (carry the symbolic obligation). Remembrance community: payer (lose quiet memorial space) and secondary beneficiary (the stone's existence matters to them). Directionality mixed; moderate on average. Planners: payer (absorb friction, make judgment calls, face contradictions). Directionality moderate-high. General public: observer (no structural extraction). Directionality analytical. The beneficiary status of development interests (they extract arbitrage) and the victim status of remembrance communities and planners (they expend effort and bear friction) derive the directionality spread. Development interests are mobile (arbitrage exit), so their d is dampened toward the beneficiary end; remembrance communities are constrained (can only object, not override), so their d is amplified toward the payer end. Planners are constrained by administrative obligation, so their d sits at moderate-high.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (mandate outliving function) is exactly the structure here. The founding mandate was to preserve a commemorative zone via land-use prohibition. The founding problem (disaster + need for memory) persists, but the rule's functional relationship to that problem has decayed. Memory is now maintained through ceremony and symbolic presence rather than through spatial control. The rule persists because (a) it costs little to maintain in code and (b) it extracts performative deference from planners and remembrance communities, and (c) no single party is motivated to formally abolish it (developers prefer it as-is, waivable; memory communities prefer it as a symbol even if unenforced). The mandatrophy classification prevents misreading this as either a live Snare (it lacks real extraction power) or a live Rope (it lacks genuine coordination function). It is a Piton: atrophied enforcement, persistent symbolic maintenance, no concentrated beneficiary or victim sufficient to change it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rule_enforcement_vs_symbol,
    'Is the stone rule still a live land-use constraint that developers genuinely constrain themselves around, or has it become purely symbolic authority invoked to justify exceptions?',
    'Zoning variance audit: compare the rate of variance applications for sites near the stone versus sites far from it; if variance rates are equal or nearly equal, the rule has no binding effect. Interviews with development interests: direct questions about whether the rule shapes their projects or whether they plan around it. Spatial analysis of completed buildings: measure actual compliance distance from the stone across time periods.',
    'If live constraint: reclassify toward behavioral_competence reading and Snare type (extraction is suppression cost, not arbitrage). If symbolic: the commemorative_husk reading holds, and the constraint is a Piton (high theater, extractive arbitrage, no enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rule_enforcement_vs_symbol, empirical, 'Whether the stone rule functionally constrains land-use decisions or merely provides narrative cover for exceptions.').

omega_variable(
    remembrance_community_exit,
    'If the stone rule were formally abolished, would the remembrance community relocate the stone, create an alternative memorial, or accept that memory is now maintained through non-spatial means?',
    'Interviews with remembrance community leaders and descendants; historical precedent from other memorials; observation of their response if any jurisdiction formally removes or relaxes the rule.',
    'If the community would fight to preserve the spatial rule, the rule extracts identity-based attachment and resistance becomes a structural feature. If the community would adapt to memorial forms other than spatial restriction, the rule extracts less and the theater fraction is higher. If the community is ambivalent, the suppression measured may be partly internalized identity-lock rather than structural coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remembrance_community_exit, empirical, 'Whether the remembrance community''s commitment to the rule is structural identity-lock or negotiable preference for one form of memory practice.').

omega_variable(
    kernel_reading_stability,
    'As development pressure intensifies and more buildings cluster near the stone, at what point would observable land-use practice transition from the commemorative_husk reading (symbolic, waivable) to a behavioral_competence reading (live, enforced), or would it instead transition to a snare reading (symbolic cover for extraction by development interests)?',
    'Longitudinal zoning audit over the next 10–20 years; observation of whether (a) planners and political pressure eventually restore enforcement (behavioral_competence emerges), (b) development interests formalize the rule''s symbolic status in code and stop pretending to compliance (Snare emerges), or (c) the rule is formally abolished.',
    'If the reading transitions, the constraint family gains a temporal ordering: commemorative_husk is the transient reading, a way-station between behavioral_competence (the founding state) and either renewed enforcement or formalized extraction. If it stabilizes, commemorative_husk is a durable equilibrium state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, empirical, 'Whether the commemorative_husk reading is a stable endpoint or a transitional state in the kernel''s evolution.').

omega_variable(
    suppression_internalization,
    'Is the remembrance community''s acceptance of the rule''s decay (their continued ceremonies and symbolic objections despite lost spatial control) structural suppression (they are coerced by development power and legal powerlessness) or internalized commitment (they have accepted that memory can be maintained non-spatially and choose to do so)?',
    'Post-exit measurement: if the rule were formally abolished, would the remembrance community''s commemorative practice persist, shift forms, or dissolve? Longitudinal interviews tracking whether their framing of the rule shifts from ''constraint that protects the space'' to ''symbol we perform''.',
    'If structural: suppression is higher than the authored 0.38 (it persists because they lack exit options). If internalized: suppression is accurately measured (they choose performative memory). The distinction informs whether the constraint is trapping an unwilling community or accommodating their chosen memorial form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether the remembrance community''s constraint acceptance is coerced or consensual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ston_tr_t7, stone_land_use_rule__commemorative_husk, theater_ratio, 7, 0.52).
narrative_ontology:measurement(ston_tr_t14, stone_land_use_rule__commemorative_husk, theater_ratio, 14, 0.61).
narrative_ontology:measurement(ston_tr_t21, stone_land_use_rule__commemorative_husk, theater_ratio, 21, 0.71).
narrative_ontology:measurement(ston_tr_t28, stone_land_use_rule__commemorative_husk, theater_ratio, 28, 0.78).
narrative_ontology:measurement(ston_tr_t35, stone_land_use_rule__commemorative_husk, theater_ratio, 35, 0.81).
narrative_ontology:measurement(ston_tr_t42, stone_land_use_rule__commemorative_husk, theater_ratio, 42, 0.82).
narrative_ontology:measurement(ston_tr_t50, stone_land_use_rule__commemorative_husk, theater_ratio, 50, 0.82).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ston_be_t7, stone_land_use_rule__commemorative_husk, base_extractiveness, 7, 0.45).
narrative_ontology:measurement(ston_be_t14, stone_land_use_rule__commemorative_husk, base_extractiveness, 14, 0.52).
narrative_ontology:measurement(ston_be_t21, stone_land_use_rule__commemorative_husk, base_extractiveness, 21, 0.58).
narrative_ontology:measurement(ston_be_t28, stone_land_use_rule__commemorative_husk, base_extractiveness, 28, 0.64).
narrative_ontology:measurement(ston_be_t35, stone_land_use_rule__commemorative_husk, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(ston_be_t42, stone_land_use_rule__commemorative_husk, base_extractiveness, 42, 0.7).
narrative_ontology:measurement(ston_be_t50, stone_land_use_rule__commemorative_husk, base_extractiveness, 50, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(stone_land_use_rule__commemorative_husk, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, identity_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__commemorative_husk, 0.12).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).

% DUAL FORMULATION NOTE:
% The stone_land_use_rule kernel decomposes into two readings with structurally distinct epsilon values and stakeholder situations. The behavioral_competence reading treats the stone as a live land-use prohibition enforced through daily spatial practice and developer compliance costs; the commemorative_husk reading treats the same stone as an atrophied enforcement mechanism maintained theatrically. These are not the same constraint viewed from two angles — their epsilon values differ by a wide margin, their failure modes are opposite (one fails if development proceeds; one fails if development is somehow blocked despite variances), and their empirical statuses are contested. The two stories are linked via this network edge so the corpus can measure which reading fits the observed land-use pattern and constraint evolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stone_land_use_rule__commemorative_husk, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
