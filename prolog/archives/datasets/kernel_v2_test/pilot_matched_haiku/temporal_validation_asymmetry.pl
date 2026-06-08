% ============================================================================
% CONSTRAINT STORY: temporal_validation_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Temporal Validation Asymmetry in Catastrophe-Derived Land-Use Governance
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   In 1933, the Sanriku tsunami devastated coastal communities in
 *   northeastern Japan, killing thousands. In the village of Aneyoshi,
 *   survivors erected a stone marker inscribed with the directive 'Do not
 *   build your homes below this point.' This stone became the kernel of a
 *   catastrophe-derived commitment system: a physical, cultural, and
 *   institutional constraint on coastal development grounded in the founding
 *   generation's direct experience of catastrophic loss. For 78 years
 *   (1933-2011), the constraint persisted without validation events — no
 *   tsunami struck to confirm the directive's necessity. During this
 *   inter-catastrophe period, the constraint operated under conditions of
 *   temporal validation asymmetry: the founding generation's
 *   catastrophe-derived authority had to sustain behavioral force across two
 *   subsequent generations who had not experienced the founding event. The
 *   constraint's persistence across this 78-year gap without direct
 *   validation is the core structural puzzle. Did the stone directive retain
 *   its behavioral force as a binding land-use constraint, maintained through
 *   intergenerational transmission of catastrophe-prevention norms? Or did it
 *   decay into a commemorative artifact, a memorial to the 1933 event whose
 *   behavioral force eroded as generational distance increased and
 *   development interests suppressed enforcement? The 2011 Sanriku tsunami
 *   provided a validation event that would resolve this ambiguity — but the
 *   constraint's classification depends on which reading of the stone
 *   directive is structurally correct.
 *
 * KEY AGENTS:
 *   - Founding Generation (1933): Direct experience of catastrophe; established the stone directive with maximum authority
 *   - Second-Generation Descendants (1950s-1970s): Heard stories of 1933 but did not experience the tsunami; maintained the stone through cultural reverence
 *   - Third-Generation Descendants (1980s-2011): No direct knowledge of 1933; experienced the stone as a cultural artifact and zoning constraint
 *   - Development Interests: Sought coastal development opportunities during the inter-catastrophe period; benefited from the constraint's potential decay
 *   - Municipal Government: Enforced land-use restrictions based on the stone directive; experienced the constraint as coordination mechanism
 *   - Coastal Community: Benefited from the constraint's prevention of catastrophic development; vulnerable to the constraint's decay
 *   - Analytical Observer: Sees the temporal validation asymmetry as a structural feature of catastrophe-derived institutional memory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temporal_validation_asymmetry, 0.15).
domain_priors:suppression_score(temporal_validation_asymmetry, 0.08).
domain_priors:theater_ratio(temporal_validation_asymmetry, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temporal_validation_asymmetry, extractiveness, 0.15).
narrative_ontology:constraint_metric(temporal_validation_asymmetry, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temporal_validation_asymmetry, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temporal_validation_asymmetry, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(temporal_validation_asymmetry, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temporal_validation_asymmetry, mountain).
narrative_ontology:human_readable(temporal_validation_asymmetry, "Temporal Validation Asymmetry in Catastrophe-Derived Land-Use Governance").
narrative_ontology:topic_domain(temporal_validation_asymmetry, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:emerges_naturally(temporal_validation_asymmetry).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temporal_validation_asymmetry, 'a1b4d621-4a96-494b-ae62-1c5523fa64fd').
narrative_ontology:cs_kernel_codification('a1b4d621-4a96-494b-ae62-1c5523fa64fd', fixed_text).
narrative_ontology:cs_authority_grounding('a1b4d621-4a96-494b-ae62-1c5523fa64fd', lineage).
narrative_ontology:cs_interpretation_layer_present('a1b4d621-4a96-494b-ae62-1c5523fa64fd').
narrative_ontology:cs_reading_relation('a1b4d621-4a96-494b-ae62-1c5523fa64fd', temporal_validation_asymmetry__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('a1b4d621-4a96-494b-ae62-1c5523fa64fd', foundational, catastrophe_derived_authority_persists_across_generations).
narrative_ontology:cs_axiom_status(catastrophe_derived_authority_persists_across_generations, holdable).
narrative_ontology:cs_axiom_grounding('a1b4d621-4a96-494b-ae62-1c5523fa64fd', catastrophe_derived_authority_persists_across_generations, empirically_contingent).
narrative_ontology:cs_axiom('a1b4d621-4a96-494b-ae62-1c5523fa64fd', foundational, intergenerational_norm_transmission_sustains_behavioral_force).
narrative_ontology:cs_axiom_status(intergenerational_norm_transmission_sustains_behavioral_force, holdable).
narrative_ontology:cs_axiom_grounding('a1b4d621-4a96-494b-ae62-1c5523fa64fd', intergenerational_norm_transmission_sustains_behavioral_force, empirically_contingent).
narrative_ontology:cs_axiom('a1b4d621-4a96-494b-ae62-1c5523fa64fd', secondary, physical_marker_stone_inscription_embeds_constraint_in_landscape).
narrative_ontology:cs_axiom_status(physical_marker_stone_inscription_embeds_constraint_in_landscape, holdable).
narrative_ontology:cs_axiom_grounding('a1b4d621-4a96-494b-ae62-1c5523fa64fd', physical_marker_stone_inscription_embeds_constraint_in_landscape, empirically_contingent).
narrative_ontology:cs_reference_frame('a1b4d621-4a96-494b-ae62-1c5523fa64fd', catastrophe_prevention_through_spatial_constraint).
narrative_ontology:cs_drift_state('a1b4d621-4a96-494b-ae62-1c5523fa64fd', contemporary_2011, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a1b4d621-4a96-494b-ae62-1c5523fa64fd', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temporal_validation_asymmetry, development_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temporal_validation_asymmetry, second_generation_descendants).
narrative_ontology:constraint_beneficiary(temporal_validation_asymmetry, third_generation_descendants).
narrative_ontology:constraint_beneficiary(temporal_validation_asymmetry, coastal_community).
narrative_ontology:constraint_victim(temporal_validation_asymmetry, development_interests).
narrative_ontology:constraint_vindicates(temporal_validation_asymmetry, catastrophe_prevention_through_spatial_constraint).
narrative_ontology:constraint_vindicates(temporal_validation_asymmetry, intergenerational_norm_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Survivors of the 1933 Sanriku tsunami who directly experienced catastrophic loss. Established the stone directive as a binding land-use constraint based on their direct experience of tsunami devastation. Possessed maximum authority to set the agenda for coastal development because their experience was recent and undeniable. Trapped by the immediate aftermath of catastrophe; no exit from the need to prevent future disasters.
narrative_ontology:constraint_stakeholder(temporal_validation_asymmetry, founding_generation_1933, agenda_setter,
    organized, immediate, trapped, local).

% Children and grandchildren of the founding generation who heard stories of the 1933 tsunami but did not experience it directly. Maintained the stone directive through cultural reverence and institutional embedding in zoning law. Benefited from the constraint's disaster-prevention function without bearing the cost of direct catastrophe experience. Constrained by cultural obligation to honor the founding generation's memory and by institutional zoning restrictions.
narrative_ontology:constraint_stakeholder(temporal_validation_asymmetry, second_generation_descendants, beneficiary,
    moderate, biographical, constrained, local).

% Grandchildren and great-grandchildren of the founding generation with no direct knowledge of the 1933 tsunami. Experienced the stone directive as a cultural artifact and zoning constraint rather than as a response to lived catastrophe. Benefited from the constraint's disaster-prevention function but may have questioned its necessity during the 78-year inter-catastrophe period. Constrained by institutional zoning law and cultural tradition, but with weaker internalization of the constraint's founding rationale.
narrative_ontology:constraint_stakeholder(temporal_validation_asymmetry, third_generation_descendants, beneficiary,
    moderate, biographical, constrained, local).

% Commercial and residential developers seeking to build on coastal land in Aneyoshi. Bore the cost of the stone directive's suppression of profitable development opportunities. Had mobile exit options (could develop elsewhere) but faced institutional and cultural barriers to coastal development in Aneyoshi. Potentially benefited from the constraint's decay during the inter-catastrophe period if enforcement weakened, allowing some development to proceed.
narrative_ontology:constraint_stakeholder(temporal_validation_asymmetry, development_interests, payer,
    powerful, biographical, mobile, regional).

% Local government responsible for zoning and land-use enforcement. Embedded the stone directive in municipal zoning law and enforced coastal development restrictions. Benefited from the constraint's coordination function (disaster prevention) and from the cultural authority of the stone (which did the legitimation work for zoning restrictions). Had arbitrage options (could theoretically permit development, but chose not to) and experienced the constraint as a coordination mechanism rather than as extraction.
narrative_ontology:constraint_stakeholder(temporal_validation_asymmetry, municipal_government, agenda_setter,
    institutional, generational, arbitrage, local).

% Residents of Aneyoshi and surrounding coastal communities who benefited from the stone directive's prevention of catastrophic coastal development. Trapped by geography (living in a tsunami-prone zone) and by the constraint's suppression of development alternatives. Benefited from the constraint's disaster-prevention function without bearing the cost of foregone development (which fell on developers, not residents). Vulnerable to the constraint's decay if enforcement weakened during the inter-catastrophe period.
narrative_ontology:constraint_stakeholder(temporal_validation_asymmetry, coastal_community, beneficiary,
    powerless, generational, trapped, local).

% External analyst examining the temporal validation asymmetry as a structural feature of catastrophe-derived institutional memory. Sees the constraint as a natural law of how human societies encode and transmit survival knowledge across generations. Observes that the constraint persisted without validation events (no tsunami for 78 years) yet maintained behavioral force through intergenerational transmission of catastrophe-prevention norms. Neither collects from nor pays into the constraint; occupies a purely analytical position.
narrative_ontology:constraint_stakeholder(temporal_validation_asymmetry, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevention of catastrophic coastal development in a tsunami-prone zone. The 1933 Sanriku tsunami demonstrated that uncontrolled coastal settlement creates massive casualties. The stone directive solves the collective-action problem of coordinating land-use restrictions across generations without requiring constant justification or enforcement — the cultural authority of the stone does the legitimation work.
% TRANSFER_FUNCTION: The constraint transfers foregone development value from developers (who cannot build on coastal land) to the coastal community (who benefit from disaster prevention). The transfer is not extraction because it is justified by the coordination function (disaster prevention), not by asymmetric rent-seeking. However, if the constraint's behavioral force decayed during the inter-catastrophe period, development interests may have extracted value through suppression of enforcement.
% ABSENT_VOICES: The 1933 founding generation is absent from the inter-catastrophe period (1933-2011) — their direct experience of catastrophe cannot validate the constraint's necessity. The third-generation descendants (1980s-2011) may have questioned the constraint's necessity during the 78-year period without validation events, but their voices are not recorded in the historical record. Development interests may have suppressed their objections to the constraint during this period.
% DISAPPEARANCE_RATIONALE: If the stone directive disappeared, coastal development would proceed unchecked in Aneyoshi, creating catastrophic vulnerability to tsunami. The 2011 Sanriku tsunami demonstrated that the constraint's disappearance would have resulted in massive casualties — the constraint's presence prevented development that would have been devastated by the 2011 event. The world rearranges because the constraint's absence would enable development that creates catastrophic vulnerability.
% FOUNDING_PROBLEM: The 1933 Sanriku tsunami killed thousands of coastal residents in northeastern Japan. Survivors in Aneyoshi recognized that uncontrolled coastal settlement created massive casualties and established the stone directive to prevent future catastrophic development. The founding problem was the coordination failure that allowed settlement in a tsunami-prone zone without adequate disaster-prevention measures.
% FOUNDING_PROBLEM_CORROBORATION: The 2011 Sanriku tsunami (78 years after the founding event) confirmed that the founding problem remains live — coastal areas without disaster-prevention constraints experienced massive casualties. The 2011 event validated the founding generation's recognition that tsunami risk requires coordinated land-use restrictions. Corroboration comes from the 2011 tsunami's impact on coastal communities that lacked constraints equivalent to the stone directive.
narrative_ontology:disappearance_verdict(temporal_validation_asymmetry, world_rearranges).
narrative_ontology:founding_problem_status(temporal_validation_asymmetry, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL RESIDENT / BEHAVIORAL COMPETENCE (MOUNTAIN) — The stone directive is experienced as an immutable physical/cultural fact. The resident cannot exit the constraint — it is embedded in land-use law, cultural memory, and the geography itself. From the biographical horizon of someone living in Aneyoshi, the constraint appears as natural law: 'do not build below the stone' is as unchangeable as the coastline. The constraint persists without validation events because it is grounded in catastrophe-derived authority that transcends individual lifespans.
constraint_indexing:constraint_classification(temporal_validation_asymmetry, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SECOND-GENERATION DESCENDANT / COMMEMORATIVE HUSK (PITON) — The stone is experienced as a memorial artifact whose behavioral force has decayed. The descendant knows the story ('my grandfather saw the wave') but has not experienced the catastrophe directly. The constraint persists through institutional inertia and cultural reverence for the stone, not through active enforcement or genuine coordination function. Theater ratio is elevated because the constraint's primary function is now performative — maintaining the memorial tradition — rather than preventing coastal development. The constraint is maintained theatrically because alternatives (coastal development) have become economically attractive and the founding problem (catastrophe prevention) has not recurred to validate the directive.
constraint_indexing:constraint_classification(temporal_validation_asymmetry, piton,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: MUNICIPAL GOVERNMENT / INSTITUTIONAL COORDINATION (ROPE) — The municipality experiences the stone directive as a coordination mechanism that solves a genuine collective-action problem: preventing coastal development in a tsunami-prone zone. The constraint is low-extraction from the municipal perspective because it aligns with the government's own interest in disaster prevention and public safety. The municipality has arbitrage options (it could theoretically permit development, but chooses not to) and benefits from the constraint's coordination function. The stone directive enables the municipality to enforce land-use restrictions without constant justification — the cultural authority of the stone does the legitimation work.
constraint_indexing:constraint_classification(temporal_validation_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: DEVELOPMENT INTEREST / EXTRACTION BENEFICIARY (TANGLED ROPE) — Development interests experience the constraint as mixed coordination and extraction. The stone directive coordinates disaster prevention (genuine function) but also extracts value by preventing economically rational coastal development. The development interest has mobile exit options (can develop elsewhere) but experiences the constraint as asymmetric: the coordination benefit (disaster prevention) flows to the community; the extraction cost (foregone development) flows to the developer. The constraint requires active enforcement (municipal zoning, cultural enforcement) to persist. From this perspective, the constraint is tangled: it solves a real problem (tsunami risk) while simultaneously suppressing profitable alternatives.
constraint_indexing:constraint_classification(temporal_validation_asymmetry, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / CIVILIZATIONAL (MOUNTAIN) — From a civilizational perspective, the temporal validation asymmetry is a structural feature of catastrophe-derived constraints: the constraint persists across the inter-catastrophe period (1933-2011, 78 years) without validation events, yet maintains behavioral force through intergenerational transmission of catastrophe-prevention norms. The constraint appears as a natural law of institutional memory: catastrophe-derived authority can persist across multiple generations without direct experience of the founding catastrophe, because the constraint is embedded in physical markers (the stone), cultural narratives, and institutional practice. The analytical observer sees the constraint as immutable — not because it cannot be changed, but because the mechanism that sustains it (intergenerational norm transmission grounded in catastrophe memory) is a fundamental feature of how human societies encode and transmit survival knowledge.
constraint_indexing:constraint_classification(temporal_validation_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_validation_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(temporal_validation_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_validation_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
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

test(piton_threshold) :-
    domain_priors:theater_ratio(temporal_validation_asymmetry, TR),
    TR >= 0.70.

:- end_tests(temporal_validation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Very low. The constraint is grounded in catastrophe prevention, not in extraction from any identifiable beneficiary. The stone directive coordinates disaster prevention — a genuine collective-action problem. The low extractiveness reflects that the constraint solves a real problem (tsunami risk) without asymmetric extraction. However, the measurement trajectory shows slight increase over the inter-catastrophe period (0.08 → 0.18 → 0.15), reflecting the possibility that development interests gradually suppressed the constraint's enforcement as generational distance from 1933 increased. The slight uptick in extractiveness during the middle period (1950s-1980s) suggests that the constraint's behavioral force may have weakened, allowing development interests to extract value through suppression of enforcement. Suppression (0.08): Very low. The constraint operates through cultural authority and institutional embedding, not through coercive suppression. The stone directive is experienced as a binding norm, not as an externally imposed restriction. The suppression trajectory shows decline over the inter-catastrophe period (0.12 → 0.06), reflecting that the constraint's behavioral force may have weakened as generational distance increased — less active enforcement was needed because the constraint was becoming more commemorative and less binding. Theater ratio (0.22): Low-moderate. The constraint has some performative content (maintaining the stone as a memorial, reciting the directive's history) but retains genuine functional content (preventing coastal development). The theater ratio increases over the inter-catastrophe period (0.08 → 0.28), suggesting that the constraint's primary function shifted from behavioral (preventing development) to performative (maintaining the memorial tradition). The peak at time point 60 (0.28) reflects the period of maximum commemorative emphasis and minimum validation events. The slight decrease at time point 78 (0.22) may reflect anticipatory strengthening of the constraint as the 2011 tsunami approached, or it may reflect the constraint's actual state at the moment of validation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a fundamental perspectival gap between the commemorative-husk reading and the behavioral-competence reading. From the commemorative-husk perspective (second-generation descendant), the stone is a memorial artifact whose behavioral force has decayed — the constraint is a piton, maintained through cultural reverence and institutional inertia rather than through genuine disaster-prevention function. From the behavioral-competence perspective (coastal resident, municipal government), the stone directive is a binding land-use constraint that retained its behavioral force across the inter-catastrophe period — the constraint is a mountain, an immutable institutional memory grounded in catastrophe-derived authority. The analytical observer sees both readings as coherent but mutually exclusive: either the constraint retained behavioral force (mountain) or it decayed into commemoration (piton), but not both simultaneously. The 2011 tsunami provides a validation event that will resolve this perspectival gap — if the constraint prevented coastal development in Aneyoshi, the behavioral-competence reading is correct; if development had proceeded unchecked, the commemorative-husk reading is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality is unusual because it is grounded in catastrophe prevention rather than in extraction from identifiable beneficiaries. The coastal community benefits from the constraint (disaster prevention), but this benefit is not extraction — it is coordination. Development interests are suppressed by the constraint, but this suppression is not extraction — it is prevention of a harmful activity. The constraint's directionality is therefore near-zero (d ≈ 0.0) from the coastal community's perspective (beneficiary of coordination) and near-one (d ≈ 1.0) from the development interest's perspective (suppressed by the constraint). However, the development interest's suppression is not experienced as extraction because the constraint is grounded in legitimate disaster prevention, not in asymmetric rent-seeking. The constraint's low extractiveness (0.15) reflects this: the suppression of development is justified by the coordination function (disaster prevention), not by extraction from the development interest. The beneficiary structure is therefore inverted from typical extraction constraints: the beneficiary is the coastal community (who benefit from disaster prevention), not the development interest (who are suppressed). The constraint's persistence across the inter-catastrophe period without validation events creates a directionality ambiguity: if the constraint's behavioral force decayed (commemorative-husk reading), then development interests may have gradually extracted value through suppression of enforcement; if the constraint retained behavioral force (behavioral-competence reading), then the suppression of development is justified by the coordination function.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that catastrophe-derived institutional memory can persist across generations without direct validation events. The founding mandate (prevent coastal development to avoid tsunami casualties) has not been superseded by a new mandate — the constraint's purpose remains unchanged across the 78-year inter-catastrophe period. However, the constraint's behavioral force may have decayed during this period, creating a mandatrophy risk: the mandate persists (disaster prevention) but the mechanism that enforces it (intergenerational transmission of catastrophe-prevention norms) may have weakened as generational distance from the founding catastrophe increased. The 2011 tsunami provides a validation event that will determine whether mandatrophy has occurred: if the constraint prevented development in Aneyoshi, the mandate and mechanism are both intact (no mandatrophy); if development had proceeded, the mandate persists but the mechanism has decayed (mandatrophy). The constraint's classification as a mountain (immutable institutional memory) depends on the behavioral-competence reading being correct — that the constraint retained its behavioral force across the inter-catastrophe period through intergenerational transmission of catastrophe-prevention norms. If the commemorative-husk reading is correct (the constraint decayed into commemoration), then the constraint is a piton (degraded ritual maintained through inertia), and mandatrophy has occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_authority,
    'Is the temporal validation asymmetry a natural law of catastrophe-derived institutional memory, or a contingent institutional arrangement that benefits development interests by naturalizing the constraint?',
    'Comparative analysis of catastrophe-derived constraints across cultures and time periods. Do constraints without physical markers (stone inscriptions) persist as effectively? Do constraints in cultures with different intergenerational transmission mechanisms show different persistence patterns? Does the constraint''s persistence depend on the specific cultural/institutional context, or is it universal?',
    'If natural law: the constraint is immutable and the beneficiary structure is incidental. If contingent: the constraint is a false summit — development interests benefit from the naturalization of what is actually a constructed institutional arrangement. The 2011 Sanriku tsunami validation event will determine which reading is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_authority, empirical, 'Whether temporal validation asymmetry is natural law or constructed institutional arrangement').

omega_variable(
    commemorative_decay_vs_behavioral_persistence,
    'During the 78-year inter-catastrophe period (1933-2011), did the stone directive retain behavioral force as a binding land-use constraint, or did it decay into a commemorative artifact while development interests suppressed the constraint''s enforcement?',
    'Historical analysis of land-use decisions in Aneyoshi during 1933-2011. Were development proposals rejected based on the stone directive? Did municipal zoning explicitly reference the stone? Did cultural narratives about the stone emphasize its binding authority or its commemorative significance? Post-2011 analysis: did the constraint''s behavioral force increase after the validation event, or did it remain at the same level?',
    'If behavioral persistence: the constraint is a mountain (immutable institutional memory). If commemorative decay: the constraint is a piton (degraded ritual maintained through inertia). The 2011 tsunami provides a validation event that will resolve this ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commemorative_decay_vs_behavioral_persistence, empirical, 'Whether stone directive retained behavioral force or decayed into commemoration during inter-catastrophe period').

omega_variable(
    beneficiary_structure_ambiguity,
    'Who benefits from the constraint''s persistence across the inter-catastrophe period? Development interests (who benefit from the constraint''s decay and eventual suppression), or the coastal community (who benefit from the constraint''s behavioral force preventing catastrophic development)?',
    'Analysis of development pressure and zoning decisions in Aneyoshi during 1933-2011. Were development interests actively suppressed by the stone directive, or did the constraint decay sufficiently to permit some coastal development? Post-2011: did the validation event strengthen the constraint''s behavioral force, reducing development interests'' ability to suppress it?',
    'If development interests benefit: the constraint is a false summit (natural law framing that masks extraction). If coastal community benefits: the constraint is a genuine mountain (immutable disaster-prevention mechanism). The beneficiary structure determines whether FSM (false summit detection) should reclassify the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether development interests or coastal community benefits from constraint persistence').

omega_variable(
    intergenerational_transmission_mechanism,
    'What mechanism sustains the stone directive''s behavioral force across generations without direct experience of the founding catastrophe? Is it cultural narrative transmission, institutional embedding in zoning law, physical presence of the stone marker, or some combination?',
    'Ethnographic analysis of how Aneyoshi residents learn about and internalize the stone directive. Interviews with residents of different generations about their understanding of the constraint''s origin and authority. Analysis of municipal zoning documents and their references to the stone. Comparison with other catastrophe-derived constraints that lack physical markers or institutional embedding.',
    'If cultural narrative is primary: the constraint is vulnerable to narrative decay and reinterpretation (piton risk). If institutional embedding is primary: the constraint is robust across generational turnover (mountain). If physical marker is primary: the constraint persists as long as the stone remains visible (contingent on material preservation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_mechanism, empirical, 'Mechanism sustaining stone directive''s behavioral force across generations').

omega_variable(
    validation_event_impact,
    'The 2011 Sanriku tsunami struck 78 years after the 1933 event that generated the stone directive. Did the 2011 event validate the constraint''s behavioral force, or did it reveal that the constraint had decayed into a commemorative artifact?',
    'Post-2011 analysis: Did the constraint prevent coastal development in Aneyoshi? Did residents who had not experienced the 1933 tsunami follow the stone directive during the 2011 event? Did the constraint''s behavioral force increase after the validation event, or remain unchanged? Comparison with other coastal communities that lacked catastrophe-derived constraints.',
    'If validation confirmed behavioral force: the constraint is a mountain (immutable institutional memory). If validation revealed decay: the constraint is a piton (degraded ritual that failed to prevent development). The 2011 event is the critical empirical test that resolves the commemorative-husk vs behavioral-competence ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(validation_event_impact, empirical, 'Whether 2011 tsunami validated or revealed decay of stone directive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temporal_validation_asymmetry, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tva_tr_t0, temporal_validation_asymmetry, theater_ratio, 0, 0.08).
narrative_ontology:measurement(tva_tr_t20, temporal_validation_asymmetry, theater_ratio, 20, 0.15).
narrative_ontology:measurement(tva_tr_t40, temporal_validation_asymmetry, theater_ratio, 40, 0.22).
narrative_ontology:measurement(tva_tr_t60, temporal_validation_asymmetry, theater_ratio, 60, 0.28).
narrative_ontology:measurement(tva_tr_t78, temporal_validation_asymmetry, theater_ratio, 78, 0.22).

% Extraction over time
narrative_ontology:measurement(tva_be_t0, temporal_validation_asymmetry, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(tva_be_t20, temporal_validation_asymmetry, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(tva_be_t40, temporal_validation_asymmetry, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(tva_be_t60, temporal_validation_asymmetry, base_extractiveness, 60, 0.18).
narrative_ontology:measurement(tva_be_t78, temporal_validation_asymmetry, base_extractiveness, 78, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tva_su_t0, temporal_validation_asymmetry, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(tva_su_t20, temporal_validation_asymmetry, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(tva_su_t40, temporal_validation_asymmetry, suppression_requirement, 40, 0.08).
narrative_ontology:measurement(tva_su_t60, temporal_validation_asymmetry, suppression_requirement, 60, 0.06).
narrative_ontology:measurement(tva_su_t78, temporal_validation_asymmetry, suppression_requirement, 78, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temporal_validation_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(temporal_validation_asymmetry, coastal_development_suppression).
narrative_ontology:affects_constraint(temporal_validation_asymmetry, intergenerational_norm_transmission).
narrative_ontology:affects_constraint(temporal_validation_asymmetry, catastrophe_memory_institutional_embedding).

% DUAL FORMULATION NOTE:
% The temporal validation asymmetry is downstream of the 1933 Sanriku tsunami (the founding catastrophe) and upstream of the 2011 Sanriku tsunami (the validation event). The constraint's classification depends on whether the stone directive retained behavioral force across the 78-year inter-catastrophe period. The commemorative-husk reading (piton) and behavioral-competence reading (mountain) are structurally distinct constraints with different ε values and different persistence mechanisms. The behavioral-competence reading (authored here) assumes the constraint retained behavioral force; the commemorative-husk reading would assume the constraint decayed into commemoration. These readings should be authored as separate constraint stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
