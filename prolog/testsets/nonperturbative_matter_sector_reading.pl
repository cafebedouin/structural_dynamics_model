% ============================================================================
% CONSTRAINT STORY: nonperturbative_matter_sector_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nonperturbative_matter_sector_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nonperturbative_matter_sector_reading
 *   human_readable: Nonperturbative Composite-Monopole Sector Reading of alpha_m Supercriticality
 *   domain: theoretical_physics/cosmology/speculative_astrophysics
 *
 * SUMMARY:
 *   This constraint concerns the interpretive stance taken toward an
 *   anomalously large coupling constant, alpha_m, arising in treatments of
 *   magnetic monopoles. Rather than reading the size of alpha_m as a sign
 *   that perturbative field theory has broken down (the inconsistency
 *   reading, a sibling constraint), this reading treats the largeness as
 *   diagnostic of a real physical regime: magnetic matter must be modeled
 *   nonperturbatively as strongly-bound composite states from the outset. The
 *   payoff claimed for this reading is generative — the same compositeness
 *   that makes perturbation theory inapplicable is proposed as the mechanism
 *   that screens monopole charge (evading free-monopole search bounds) and
 *   seeds cosmologically relevant structures: droplets, macroscopic dark
 *   matter candidates, and black hole seeds. This is a genuine
 *   research-coordination function (it gives several downstream programs a
 *   shared foundational premise to build from) but it also asymmetrically
 *   benefits the groups whose models depend on the mechanism being real, at
 *   the expense of parsimony norms and of experimental programs whose
 *   completed null results get reinterpreted rather than left standing.
 *
 * KEY AGENTS:
 *   - composite_monopole_research_program: agenda_setter (organized/constrained) — defines and defends the nonperturbative reading
 *   - macro_dark_matter_model_builders: beneficiary (organized/constrained) — gains a generative mechanism for candidate models
 *   - primordial_black_hole_seed_theorists: beneficiary (moderate/constrained) — gains a structure-formation pathway
 *   - parsimony_arguments: payer, non-agent (analytical/analytical) — the ontological economy standard overridden by this reading
 *   - free_monopole_search_experimentalists: payer (organized/constrained) — sees completed null results reinterpreted as uninformative
 *   - early_career_researchers_outside_program: payer (powerless/constrained) — career risk from program dominance shifts
 *   - theoretical_physics_community: observer (institutional/analytical) — evaluates competing readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nonperturbative_matter_sector_reading, 0.61).
domain_priors:suppression_score(nonperturbative_matter_sector_reading, 0.42).
domain_priors:theater_ratio(nonperturbative_matter_sector_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nonperturbative_matter_sector_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(nonperturbative_matter_sector_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(nonperturbative_matter_sector_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nonperturbative_matter_sector_reading, accessibility_collapse, 0.47).
narrative_ontology:constraint_metric(nonperturbative_matter_sector_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nonperturbative_matter_sector_reading, tangled_rope).
narrative_ontology:human_readable(nonperturbative_matter_sector_reading, "Nonperturbative Composite-Monopole Sector Reading of alpha_m Supercriticality").
narrative_ontology:topic_domain(nonperturbative_matter_sector_reading, "theoretical_physics/cosmology/speculative_astrophysics").

domain_priors:requires_active_enforcement(nonperturbative_matter_sector_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nonperturbative_matter_sector_reading, '791d3797-c2f3-4838-b9eb-76ba140d7bd2').
narrative_ontology:cs_kernel_codification('791d3797-c2f3-4838-b9eb-76ba140d7bd2', distributed).
narrative_ontology:cs_authority_grounding('791d3797-c2f3-4838-b9eb-76ba140d7bd2', expertise).
narrative_ontology:cs_interpretation_layer_present('791d3797-c2f3-4838-b9eb-76ba140d7bd2').
narrative_ontology:cs_reading_relation('791d3797-c2f3-4838-b9eb-76ba140d7bd2', nonperturbative_matter_sector_reading__inconsistency_reading, coexists_with).
narrative_ontology:cs_reading_relation('791d3797-c2f3-4838-b9eb-76ba140d7bd2', nonperturbative_matter_sector_reading__phenomenological_program_reading, influences).
narrative_ontology:cs_reading_relation('791d3797-c2f3-4838-b9eb-76ba140d7bd2', nonperturbative_matter_sector_reading__mirror_sector_alternative_reading, coexists_with).
narrative_ontology:cs_axiom('791d3797-c2f3-4838-b9eb-76ba140d7bd2', foundational, large_coupling_is_diagnostic_feature).
narrative_ontology:cs_axiom_status(large_coupling_is_diagnostic_feature, holdable).
narrative_ontology:cs_axiom_grounding('791d3797-c2f3-4838-b9eb-76ba140d7bd2', large_coupling_is_diagnostic_feature, empirically_contingent).
narrative_ontology:cs_axiom('791d3797-c2f3-4838-b9eb-76ba140d7bd2', secondary, ontological_expansion_justified_by_generative_payoff).
narrative_ontology:cs_axiom_status(ontological_expansion_justified_by_generative_payoff, holdable).
narrative_ontology:cs_axiom_grounding('791d3797-c2f3-4838-b9eb-76ba140d7bd2', ontological_expansion_justified_by_generative_payoff, instrumental).
narrative_ontology:cs_reference_frame('791d3797-c2f3-4838-b9eb-76ba140d7bd2', perturbative_qed_consistency_standard).
narrative_ontology:cs_drift_state('791d3797-c2f3-4838-b9eb-76ba140d7bd2', post_large_alpha_m_discovery, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('791d3797-c2f3-4838-b9eb-76ba140d7bd2', '').
narrative_ontology:cs_kernel_id(nonperturbative_matter_sector_reading, alpha_m_supercriticality_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nonperturbative_matter_sector_reading, composite_monopole_research_program).
narrative_ontology:constraint_beneficiary(nonperturbative_matter_sector_reading, macro_dark_matter_model_builders).
narrative_ontology:constraint_beneficiary(nonperturbative_matter_sector_reading, primordial_black_hole_seed_theorists).
narrative_ontology:constraint_victim(nonperturbative_matter_sector_reading, parsimony_arguments).
narrative_ontology:constraint_victim(nonperturbative_matter_sector_reading, free_monopole_search_experimentalists).
narrative_ontology:constraint_victim(nonperturbative_matter_sector_reading, early_career_researchers_outside_program).
narrative_ontology:constraint_vindicates(nonperturbative_matter_sector_reading, nonperturbative_treatment_necessity).
narrative_ontology:constraint_vindicates(nonperturbative_matter_sector_reading, compositeness_as_charge_screening_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Builds the machinery treating magnetic matter as strongly-bound composites from the outset. Sets the research agenda for how the large alpha_m coupling should be interpreted and defended, publishing the frameworks (droplets, macro-DM candidates, BH seed mechanisms) that other groups must engage with or ignore. Its continued funding and citation standing depend on the large-coupling reading remaining a live, fundable interpretation rather than being dismissed as an artifact of an inconsistent theory.
narrative_ontology:constraint_stakeholder(nonperturbative_matter_sector_reading, composite_monopole_research_program, agenda_setter,
    organized, generational, constrained, global).

% Uses the compositeness mechanism to construct dark matter candidates (macroscopic composite droplets) that would otherwise have no theoretical home. Depends on the nonperturbative reading being taken seriously to justify its parameter space and its distinct observational signatures; without this reading, its candidate models lose their generative mechanism.
narrative_ontology:constraint_stakeholder(nonperturbative_matter_sector_reading, macro_dark_matter_model_builders, beneficiary,
    organized, generational, constrained, global).

% Borrows the composite-monopole mechanism to source BH seed formation channels in the early universe. Gains a new structure-formation pathway if the compositeness reading holds; loses a candidate mechanism (and associated papers/grants) if the sector is instead read as symptomatic of an inconsistency.
narrative_ontology:constraint_stakeholder(nonperturbative_matter_sector_reading, primordial_black_hole_seed_theorists, beneficiary,
    moderate, generational, constrained, global).

% Ockham's-razor-style preference for minimal ontological commitment is the abstract casualty of this reading: accepting an ontologically expansive new composite sector (droplets, macro-DM, BH seeds) to rescue the large coupling is precisely the move parsimony would counsel against. Not an actor, but a standard that is structurally overridden whenever this reading is adopted rather than the inconsistency reading.
narrative_ontology:constraint_stakeholder(nonperturbative_matter_sector_reading, parsimony_arguments, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(nonperturbative_matter_sector_reading, parsimony_arguments).

% Runs direct searches for free monopoles calibrated against bounds that assume a certain charge visibility. If compositeness hides monopole charge as this reading claims, existing null results are reinterpreted as uninformative about the true parameter space rather than as exclusions, devaluing completed search programs and forcing costly redesign of detection strategy around composite, charge-screened states.
narrative_ontology:constraint_stakeholder(nonperturbative_matter_sector_reading, free_monopole_search_experimentalists, payer,
    organized, biographical, constrained, global).

% Must position dissertations and early publications relative to whichever reading of alpha_m supercriticality currently commands citation and funding attention. If the nonperturbative composite reading becomes dominant, work built on the inconsistency reading or on simpler perturbative treatments risks appearing obsolete regardless of its own merits, and switching programs mid-career is costly.
narrative_ontology:constraint_stakeholder(nonperturbative_matter_sector_reading, early_career_researchers_outside_program, payer,
    powerless, biographical, constrained, national).

% Evaluates competing readings of the large alpha_m coupling through peer review, conference consensus, and citation patterns, without itself being a direct beneficiary or payer of any one reading's success.
narrative_ontology:constraint_stakeholder(nonperturbative_matter_sector_reading, theoretical_physics_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nonperturbative_matter_sector_reading, composite_monopole_research_program).
narrative_ontology:fixing_cost_class(nonperturbative_matter_sector_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a research program around a specific technical claim: that the large alpha_m coupling is not evidence of theoretical breakdown but a signal that magnetic matter must be modeled nonperturbatively as composites, giving disparate groups (dark matter model-builders, BH seed theorists) a shared mechanism to build on rather than each inventing incompatible fixes.
% TRANSFER_FUNCTION: Moves attention, funding, and citation currency toward the composite-monopole program and its downstream applications, and away from competing readings (inconsistency, phenomenological, mirror-sector) and away from parsimony-favoring minimal ontologies; also devalues completed free-monopole search results by reframing their null bounds as inapplicable to charge-screened composites.
% ABSENT_VOICES: Proponents of the inconsistency reading, who would argue the large alpha_m is a symptom that the theory has left the regime where perturbative QED-style treatment is even meaningful, are present in the literature but structurally disadvantaged in this reading's framing, which treats their diagnosis as premature abandonment rather than a live alternative. Experimentalists whose search bounds get reinterpreted are not consulted before the reinterpretation is proposed.
% DISAPPEARANCE_RATIONALE: If this reading were withdrawn, the composite/droplet/macro-DM/BH-seed research program would lose its foundational justification overnight — grant proposals, PhD projects, and follow-on phenomenology papers built on treating alpha_m supercriticality as a feature rather than a bug would need to either migrate to a different reading or be abandoned. Free-monopole search programs would regain straightforward interpretability of their existing null results without needing the charge-screening caveat.
% FOUNDING_PROBLEM: Standard perturbative treatments of magnetic monopoles produce an anomalously large coupling constant (alpha_m) that renders perturbation theory formally inapplicable, and the field needed an interpretive stance on whether this signals a breakdown of the theory or a feature to be exploited.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the composite-monopole program attest the large coupling is best read as a nonperturbative feature enabling new physics. Outside the beneficiary set, theorists working the inconsistency reading and phenomenologists cautious about underdetermined model-building attest the same large-alpha_m fact is at minimum ambiguous and possibly diagnostic of a genuine theoretical limit rather than a discovery opportunity; no fully independent, non-aligned third party (e.g., a dedicated review committee) has adjudicated between the readings.
narrative_ontology:disappearance_verdict(nonperturbative_matter_sector_reading, world_rearranges).
narrative_ontology:founding_problem_status(nonperturbative_matter_sector_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nonperturbative_matter_sector_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-11',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(nonperturbative_matter_sector_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nonperturbative_matter_sector_reading, 0.61, 'claude-sonnet-5', 'dirac_magnetic_matter_2026_20260811_143746', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nonperturbative_matter_sector_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nonperturbative_matter_sector_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nonperturbative_matter_sector_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a substantial-but-not-extreme 0.61: the reading does real coordination work (unifying droplet, macro-DM, and BH-seed programs under one mechanism) but that same unification asymmetrically channels funding, citation, and interpretive authority toward the beneficiary programs while imposing reinterpretation costs on completed experimental work and on early-career researchers who bet on other readings. Suppression (0.42) is moderate: this is a live scientific dispute, not a coercively enforced orthodoxy, but the framing does actively work to foreclose the inconsistency reading by insisting the large coupling is a 'feature.' Theater ratio rises modestly over the interval (0.20 to 0.38) as the program matures and produces increasingly polished phenomenological narratives (droplets, macro-DM, BH seeds) whose direct empirical contact remains thin relative to the theoretical apparatus built around them. Accessibility collapse (0.47) and resistance (0.70) reflect a genuinely contested theoretical claim: alternative readings remain fully articulable and are actively defended, so alternatives have not collapsed, and the reading meets substantial resistance from proponents of the inconsistency and phenomenological-program readings.
 *
 * DIRECTIONALITY LOGIC:
 *   The composite-monopole research program and its downstream beneficiaries (macro-DM builders, BH-seed theorists) sit near the beneficiary end of directionality: the reading was constructed to justify and sustain their machinery, and their professional standing is enhanced by its acceptance. Parsimony arguments, as a non-agent standard, and free-monopole search experimentalists sit near the target end: the former is structurally overridden by the reading's ontological expansiveness, and the latter's completed work is reinterpreted away from them without their control over that reinterpretation. Early-career researchers outside the program are moderate targets — powerless, constrained exit, bearing career risk from a shift in which reading commands attention.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination function here (a shared mechanism letting disparate speculative programs build on one another) is genuine and should not be dismissed merely because it also serves the interests of those who proposed it — that would mislabel real theoretical coordination as pure extraction. Conversely, treating the reading as costless coordination would obscure the real asymmetry: the reading's persistence is not neutral with respect to who benefits from its acceptance, and the reinterpretation of experimental null results is a real cost imposed on parties who did not choose it. The tangled_rope classification captures both halves without collapsing them into either a pure Rope (ignoring the asymmetric costs) or a pure Snare (ignoring the genuine unifying theoretical work).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compositeness_mechanism_reality,
    'Is the compositeness mechanism a genuine feature of the physical theory of magnetic matter, or a constructed interpretive move that happens to rescue a formally inconsistent large-coupling regime and to benefit specific research programs?',
    'Independent nonperturbative lattice-style or numerical calculations of the composite bound-state spectrum, cross-checked against predictions the inconsistency reading would NOT make; convergence with an independent derivation not reliant on the same model assumptions would support the mechanism''s reality.',
    'If the mechanism is genuine, the beneficiary programs'' extraction is closer to earned coordination surplus from real theoretical progress; if it is a constructed rescue, the extraction is closer to pure rent-seeking on an ambiguous formal situation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compositeness_mechanism_reality, empirical, 'Whether compositeness is a real physical mechanism or an interpretive rescue of an inconsistent regime.').

omega_variable(
    sibling_reading_foreclosure_degree,
    'Does adopting this reading merely compete with the inconsistency_reading and phenomenological_program_reading for attention and funding (coexistence), or does its technical apparatus actively make the inconsistency_reading''s diagnosis harder to sustain within the same theoretical framework (foreclosure)?',
    'Track whether papers advancing the inconsistency reading can still be published and cited productively within venues dominated by the composite-sector program, or whether the technical vocabulary itself becomes incompatible.',
    'If foreclosure is occurring, the tangled_rope''s enforcement dimension is stronger than currently authored (extraction includes silencing a viable alternative, not just resource competition); if mere coexistence, the current tangled_rope reading with moderate suppression is well-calibrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_degree, conceptual, 'Whether this reading forecloses or merely competes with the inconsistency reading.').

omega_variable(
    parsimony_cost_magnitude,
    'How large is the actual cost of the ontological expansion (droplets, macro-DM, BH seeds) this reading requires, relative to the explanatory power gained — is the new sector minimal-but-novel or maximally speculative?',
    'Formal comparison of the new sector''s degrees of freedom and free parameters against the phenomena it is invoked to explain, using standard theory-comparison metrics (e.g., number of free parameters per explained observable).',
    'A minimal expansion would reduce the authored extractiveness (the parsimony cost would be small); a maximally speculative expansion would support the current extractiveness level or higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parsimony_cost_magnitude, conceptual, 'Magnitude of the ontological cost this reading imposes on parsimony norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nonperturbative_matter_sector_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nonp_tr_t0, nonperturbative_matter_sector_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nonp_tr_t4, nonperturbative_matter_sector_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(nonp_tr_t8, nonperturbative_matter_sector_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(nonp_tr_t12, nonperturbative_matter_sector_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(nonp_tr_t16, nonperturbative_matter_sector_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(nonp_tr_t20, nonperturbative_matter_sector_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(nonp_be_t0, nonperturbative_matter_sector_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nonp_be_t4, nonperturbative_matter_sector_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(nonp_be_t8, nonperturbative_matter_sector_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(nonp_be_t12, nonperturbative_matter_sector_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(nonp_be_t16, nonperturbative_matter_sector_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(nonp_be_t20, nonperturbative_matter_sector_reading, base_extractiveness, 20, 0.61).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nonperturbative_matter_sector_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nonperturbative_matter_sector_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nonperturbative_matter_sector_reading, 0.08).
narrative_ontology:affects_constraint(nonperturbative_matter_sector_reading, inconsistency_reading).
narrative_ontology:affects_constraint(nonperturbative_matter_sector_reading, phenomenological_program_reading).
narrative_ontology:affects_constraint(nonperturbative_matter_sector_reading, mirror_sector_alternative_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints instantiating the alpha_m_supercriticality_kernel. The kernel is the shared fact: magnetic monopole theory produces an anomalously large alpha_m under standard perturbative treatment. Four readings diverge on what this fact means: this reading (nonperturbative_matter_sector_reading) treats it as a generative feature requiring composite treatment; inconsistency_reading treats it as a symptom of theoretical breakdown; phenomenological_program_reading treats it as license for model-building agnostic about the underlying mechanism; mirror_sector_alternative_reading locates the resolution in a different sector. Each reading is authored as its own constraint with its own ε, beneficiaries, and victims, per the ε-invariance principle — this story's high theoretical/institutional extraction of 0.61 is specific to the resource-and-attention asymmetries this particular reading generates, and is not a measurement of the underlying physics fact shared by all four readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
