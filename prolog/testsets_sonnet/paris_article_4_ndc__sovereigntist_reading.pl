% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__sovereigntist_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: paris_article_4_ndc__sovereigntist_reading
 *   human_readable: Paris Agreement Article 4 NDC Regime (Sovereigntist Reading)
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   Under the sovereigntist reading, Article 4 of the Paris Agreement is read
 *   as a deliberate and legitimate architectural choice: after the binding
 *   top-down failure of Kyoto, the international community converged on
 *   nationally determined, self-set targets precisely because binding
 *   external constraint on sovereign emissions and development pathways
 *   proved politically unworkable and normatively contestable. On this
 *   reading, the pledge-and-review architecture is not a watered-down
 *   substitute for a 'real' climate treaty but the correct form for an
 *   agreement among sovereign states with radically different development
 *   stages, resource endowments, and energy transition costs. States retain
 *   full exit and revision freedom (Article 4.11 permits upward — and in
 *   practice, in the absence of binding floors, effectively any — revision);
 *   fossil-dependent economies preserve their chosen development pathway;
 *   global enforcement mechanisms are structurally thin by design, not by
 *   drift. This is a distinct constraint from the supranational reading
 *   (which treats the same architecture as a binding ratchet toward net-zero)
 *   and the equity reading (which treats the absence of differentiated legal
 *   obligation as a defect). Each reading is its own constraint with its own
 *   epsilon; this one carries a low, stable epsilon consistent with genuine
 *   voluntarism.
 *
 * KEY AGENTS:
 *   - national_governments_setting_targets: Primary agenda-setters (institutional/arbitrage) — draft and revise their own pledges with no external override
 *   - fossil_dependent_developing_states: Beneficiary of preserved sovereignty (organized/constrained) — retain development pathway autonomy
 *   - major_emitter_incumbent_industries: Secondary beneficiary (powerful/arbitrage) — insulated from binding external emissions ceilings
 *   - small_island_and_climate_vulnerable_states: Excluded voice (powerless/trapped) — bear physical climate impacts but hold no veto over others' voluntary ambition levels
 *   - unfccc_secretariat_and_technical_review_bodies: Observer (institutional/analytical) — administers reporting and review without enforcement power
 *   - future_generations: Excluded voice (powerless/trapped) — bear compounding physical consequences of any ambition gap, with no seat in the pledge-setting process
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__sovereigntist_reading, 0.18).
domain_priors:suppression_score(paris_article_4_ndc__sovereigntist_reading, 0.12).
domain_priors:theater_ratio(paris_article_4_ndc__sovereigntist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(paris_article_4_ndc__sovereigntist_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__sovereigntist_reading, rope).
narrative_ontology:human_readable(paris_article_4_ndc__sovereigntist_reading, "Paris Agreement Article 4 NDC Regime (Sovereigntist Reading)").
narrative_ontology:topic_domain(paris_article_4_ndc__sovereigntist_reading, "international_climate_governance/treaty_law/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__sovereigntist_reading, '16cee59c-2846-440d-90ae-ff30f2ec2cae').
narrative_ontology:cs_kernel_codification('16cee59c-2846-440d-90ae-ff30f2ec2cae', fixed_text).
narrative_ontology:cs_authority_grounding('16cee59c-2846-440d-90ae-ff30f2ec2cae', distributed).
narrative_ontology:cs_reading_relation('16cee59c-2846-440d-90ae-ff30f2ec2cae', paris_article_4_ndc__supranational_reading, coexists_with).
narrative_ontology:cs_reading_relation('16cee59c-2846-440d-90ae-ff30f2ec2cae', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('16cee59c-2846-440d-90ae-ff30f2ec2cae', foundational, sovereign_self_determination_over_binding_target).
narrative_ontology:cs_axiom_status(sovereign_self_determination_over_binding_target, holdable).
narrative_ontology:cs_axiom_grounding('16cee59c-2846-440d-90ae-ff30f2ec2cae', sovereign_self_determination_over_binding_target, conventional).
narrative_ontology:cs_axiom('16cee59c-2846-440d-90ae-ff30f2ec2cae', secondary, voluntarism_produces_superior_participation_to_binding_form).
narrative_ontology:cs_axiom_status(voluntarism_produces_superior_participation_to_binding_form, holdable).
narrative_ontology:cs_axiom_grounding('16cee59c-2846-440d-90ae-ff30f2ec2cae', voluntarism_produces_superior_participation_to_binding_form, empirically_contingent).
narrative_ontology:cs_reference_frame('16cee59c-2846-440d-90ae-ff30f2ec2cae', post_kyoto_voluntary_pledge_consensus).
narrative_ontology:cs_drift_state('16cee59c-2846-440d-90ae-ff30f2ec2cae', post_2023_global_stocktake, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('16cee59c-2846-440d-90ae-ff30f2ec2cae', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_developing_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, major_emitter_incumbent_industries).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__sovereigntist_reading, national_governments_setting_targets).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, national_sovereignty_over_energy_policy).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__sovereigntist_reading, bottom_up_pledge_and_review_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft, submit, and revise their own NDCs on five-year cycles with no external body empowered to reject, override, or bind the content. Can weaken or strengthen ambition at each revision without penalty beyond reputational exposure through the transparency framework.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, national_governments_setting_targets, agenda_setter,
    institutional, generational, arbitrage, global).

% Retain the ability to sequence energy transition against development priorities — electrification, industrialization, poverty reduction — without an externally imposed emissions ceiling. Can point to CBDR language for political cover while facing no binding sanction for slow transition.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, fossil_dependent_developing_states, beneficiary,
    organized, generational, mobile, national).

% Operate within jurisdictions whose pledged targets they substantially help shape through domestic political influence, insulated from any binding international ceiling that could force faster asset stranding than domestic politics would otherwise produce.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, major_emitter_incumbent_industries, beneficiary,
    powerful, biographical, arbitrage, global).

% Face existential physical exposure to warming driven substantially by aggregate ambition levels they have no power to bind upward. Participate in COP negotiations and stocktakes but hold no mechanism to compel any other state's pledge revision.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, small_island_and_climate_vulnerable_states, excluded,
    powerless, civilizational, trapped, regional).

% Administer NDC registries, the enhanced transparency framework, and global stocktakes. Compile and publish aggregate ambition-gap analyses but possess no enforcement or sanction authority over any submitting state.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, unfccc_secretariat_and_technical_review_bodies, observer,
    institutional, generational, analytical, global).

% Bear the compounding physical and economic consequences of any gap between aggregate voluntary ambition and required emissions pathways, with no seat, vote, or proxy in the pledge-setting or revision process.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__sovereigntist_reading, future_generations, excluded,
    powerless, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__sovereigntist_reading, diffuse).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__sovereigntist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem that no binding top-down emissions treaty among sovereign states with radically unequal development stages and resource endowments proved politically viable (Kyoto's binding-target model produced non-ratification and withdrawal); voluntary self-determined pledges with peer transparency instead secured near-universal participation.
% TRANSFER_FUNCTION: Under this reading, the architecture is designed to minimize involuntary transfer: no state is bound to a target or pace it did not select. Any transfer that occurs is diffuse and physical (climate-vulnerable states and future generations bear compounding exposure from aggregate ambition shortfalls) rather than a transfer running through the constraint's coordination mechanism itself.
% ABSENT_VOICES: Small island and climate-vulnerable states participate in negotiations but cannot bind other states' pledge levels; future generations have no representative seat in the pledge-setting or revision process at all. Both would object to being exposed to aggregate ambition-gap risk with no corresponding voice over aggregate ambition.
% DISAPPEARANCE_RATIONALE: If the NDC pledge-and-review architecture disappeared overnight, the near-universal participation it secured would likely fracture back toward the binding-treaty non-ratification pattern seen under Kyoto, or toward no international framework at all; states would lose the common reporting format, the five-year ratchet cycle, and the transparency framework that currently structures mutual expectations, even though no state is currently bound by it in a hard legal sense.
% FOUNDING_PROBLEM: The binding top-down emissions-target model under the Kyoto Protocol produced major-emitter non-ratification, withdrawal, and stalled universal participation; Paris Article 4 was built to solve the participation problem by trading binding force for near-universal voluntary buy-in.
% FOUNDING_PROBLEM_CORROBORATION: Independent political-science and international-relations scholarship on treaty design (comparative analysis of Kyoto ratification failure versus Paris near-universal participation) corroborates that the participation problem was real and that the voluntary architecture measurably increased participation relative to the binding predecessor; this corroboration comes from academic observers outside the negotiating states themselves, not solely from the governments that benefit from the low-epsilon design.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__sovereigntist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__sovereigntist_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__sovereigntist_reading_tests).
:- end_tests(paris_article_4_ndc__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) and rising only modestly over the interval because the sovereigntist reading holds that no party is coercively extracting from another through this architecture — each state sets and can revise its own terms, so there is no asymmetric transfer riding on the same structure the way there would be in a binding-enforcement reading. Theater ratio is authored moderately high (0.42) and rising because the review, transparency, and stocktake apparatus increasingly performs the appearance of accountability (global stocktakes, enhanced transparency framework) without possessing binding teeth — a real but limited function (peer pressure, reputational signaling, benchmarking) increasingly dressed in procedural form. Suppression is authored low (0.12) because no state is coerced into a target it did not choose, and accessibility_collapse is authored low (0.2) because alternative pledge levels, timelines, and mechanisms remain fully available to any state at any revision cycle.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (national governments), the architecture is functioning coordination: sovereignty preserved, commitments credible because self-authored, review mechanisms provide reputational discipline without loss of control. From the excluded seat (climate-vulnerable states and future generations), the same architecture looks structurally hollow: they bear the physical cost of any global ambition shortfall but hold no seat that can bind others' pledges upward. The engine should compute divergent seat types from this same structural data; the sovereigntist claim of 'rope' describes the agenda-setter's experience, not a story-level average.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and fossil-dependent developing states sit near the beneficiary end of directionality: the constraint subsidizes their autonomy over pathway and pace. Major emitter incumbent industries benefit indirectly through the absence of binding external ceilings. Climate-vulnerable states and future generations are not named as base_properties victims in THIS reading because the sovereigntist account does not treat them as targets of an extractive transfer through this specific structure — their exposure is to climate physics, not to the NDC architecture's coordination mechanism as such. This is a substantive claim of the reading, not an oversight; the equity and supranational sibling readings characterize this differently, which is exactly the kind of framing divergence Rule 2 routes to omega.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing near-universal participation after Kyoto's binding-target approach produced defection and non-ratification — remains at least partially live: universal participation under Paris exceeds Kyoto's, and no major emitter has withdrawn the underlying pledge architecture even where specific pledges have been weakened. This reading holds that the low-epsilon voluntary structure is still doing the coordination work it was built for, distinguishing it from a piton (atrophied function, retained by inertia) — the function is intact by this reading's own lights, not degraded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_versus_atrophy_ambiguity,
    'Is the self-determined pledge structure a genuine expression of state sovereignty over development pathways, or a designed mechanism by which enforcement capacity was traded away at Paris in exchange for universal participation, thereby producing an emissions trajectory inconsistent with stated temperature goals?',
    'Compare aggregate NDC ambition trajectories against required emissions pathways over successive five-year ratchet cycles (2020, 2025, 2030); if the gap widens despite universal participation, the sovereignty framing functions as cover for weak commitment rather than genuine self-determination.',
    'If the gap widens structurally rather than closing, this reading''s claim that low epsilon reflects legitimate sovereignty (rather than designed unenforceability) weakens, and the constraint moves toward the tangled_rope reading shared with the supranational and equity siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_versus_atrophy_ambiguity, empirical, 'Whether voluntarism reflects legitimate sovereignty or engineered non-enforceability.').

omega_variable(
    committer_kernel_disagreement_location,
    'This constraint is one reading of the paris_article_4_ndc kernel. The sovereigntist reading holds that the pledge-and-review architecture is the settled, legitimate form of international climate cooperation given the impossibility of binding sovereign emissions targets without near-universal defection risk. The supranational reading holds the same text as a transitional binding architecture whose ratchet mechanism is meant to converge on enforceable net-zero commitments. The equity reading holds that the same voluntary structure must be read through Common But Differentiated Responsibilities, making the absence of differentiated obligations a structural defect rather than a feature. Where exactly does the disagreement sit?',
    'Track whether COP decisions and subsequent state practice treat NDC content requirements as hardening (supranational reading strengthens), as increasingly differentiated by development status (equity reading strengthens), or as stable voluntary self-determination without differentiation or ratchet-hardening (this reading strengthens).',
    'The three readings cannot all be correct simultaneously about the same treaty text''s binding character and differentiation structure; state practice over the next several ratchet cycles will empirically favor one reading''s account of what Article 4 actually is.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_disagreement_location, conceptual, 'Location of the sovereigntist/supranational/equity disagreement: binding character and differentiation, not factual dispute over text.').

omega_variable(
    developmental_pathway_preservation_genuineness,
    'Do fossil-dependent developing states genuinely benefit from preserved sovereignty over energy pathways, or does the absence of binding external constraint simply leave them exposed to climate impacts they are least equipped to absorb, making ''sovereignty'' a benefit that is real in form but hollow in substance?',
    'Assess whether states exercising NDC flexibility to preserve fossil-based development have achieved measurable development gains attributable to that flexibility, versus states that adopted more binding-style commitments.',
    'If flexibility does not translate into development gains, the beneficiary declaration for fossil_dependent_developing_states is weaker than claimed, and part of the measured extraction may be misattributed as benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developmental_pathway_preservation_genuineness, empirical, 'Whether sovereignty-preserving flexibility produces real developmental benefit or merely nominal autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__sovereigntist_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(pari_tr_t2019, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2019, 0.35).
narrative_ontology:measurement(pari_tr_t2023, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2023, 0.4).
narrative_ontology:measurement(pari_tr_t2027, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2027, 0.42).
narrative_ontology:measurement(pari_tr_t2031, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2031, 0.42).
narrative_ontology:measurement(pari_tr_t2035, paris_article_4_ndc__sovereigntist_reading, theater_ratio, 2035, 0.42).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2015, 0.1).
narrative_ontology:measurement(pari_be_t2019, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2019, 0.13).
narrative_ontology:measurement(pari_be_t2023, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2023, 0.16).
narrative_ontology:measurement(pari_be_t2027, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2027, 0.17).
narrative_ontology:measurement(pari_be_t2031, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2031, 0.18).
narrative_ontology:measurement(pari_be_t2035, paris_article_4_ndc__sovereigntist_reading, base_extractiveness, 2035, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(paris_article_4_ndc__sovereigntist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__sovereigntist_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__sovereigntist_reading, 0.1).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__supranational_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__sovereigntist_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint-family members sharing the Article 4 NDC kernel text. paris_article_4_ndc__supranational_reading reads the same text as a binding ratchet architecture (higher epsilon, victims = non-complying states facing accountability mechanisms). paris_article_4_ndc__equity_reading reads the same text through CBDR, treating undifferentiated obligations as a structural defect (victims = developing states bearing disproportionate undifferentiated burden). This sovereigntist reading is authored with the lowest epsilon of the three, consistent with its claim that voluntarism and exit freedom are the legitimate, intended form rather than a diluted compromise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__sovereigntist_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
