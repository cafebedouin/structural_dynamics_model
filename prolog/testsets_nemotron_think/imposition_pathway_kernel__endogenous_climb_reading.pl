% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__endogenous_climb_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: imposition_pathway_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Climb Pathway as Universal Commitment Displacement Mechanism
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The endogenous_climb_reading of the imposition_pathway_kernel claims that
 *   ALL commitment displacement occurs through fringe adoption and gradual
 *   climb, and that apparent top-down impositions (Meiji calendar/dress,
 *   Atatürk's reforms, Soviet collectivization decrees) are 'compressed
 *   climbs with invisible fringe stages.' This reading dominates historical
 *   sociology and state formation theory. As a constraint, it coordinates
 *   interpretation across cases but extracts by suppressing rival readings
 *   (exogenous_override, hybrid_cascade) and erasing imposed populations'
 *   experience. The Meiji case is the reading's canonical proof: it claims
 *   pre-decree fringe adoption in treaty ports, merchant class, and military
 *   modernizers meant the 1872-1873 decrees only accelerated an existing
 *   climb. Critics argue the Meiji state created the fringe (conscript army,
 *   state bureaucracy) and imposed on a resistant population — making it a
 *   hybrid cascade or exogenous override. The reading's persistence depends
 *   on active enforcement: definitional control of 'fringe,' editorial
 *   gatekeeping, and the unfalsifiability of 'invisible' stages.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, 0.68).
domain_priors:suppression_score(imposition_pathway_kernel__endogenous_climb_reading, 0.72).
domain_priors:theater_ratio(imposition_pathway_kernel__endogenous_climb_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(imposition_pathway_kernel__endogenous_climb_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__endogenous_climb_reading, "Endogenous Climb Pathway as Universal Commitment Displacement Mechanism").
narrative_ontology:topic_domain(imposition_pathway_kernel__endogenous_climb_reading, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__endogenous_climb_reading, 'b8a071b9-6938-4a49-bcff-26c59041e7ff').
narrative_ontology:cs_kernel_codification('b8a071b9-6938-4a49-bcff-26c59041e7ff', formalized).
narrative_ontology:cs_authority_grounding('b8a071b9-6938-4a49-bcff-26c59041e7ff', lineage).
narrative_ontology:cs_interpretation_layer_present('b8a071b9-6938-4a49-bcff-26c59041e7ff').
narrative_ontology:cs_reading_relation('b8a071b9-6938-4a49-bcff-26c59041e7ff', imposition_pathway_kernel__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('b8a071b9-6938-4a49-bcff-26c59041e7ff', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('b8a071b9-6938-4a49-bcff-26c59041e7ff', foundational, fringe_adoption_necessary_for_displacement).
narrative_ontology:cs_axiom_status(fringe_adoption_necessary_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('b8a071b9-6938-4a49-bcff-26c59041e7ff', fringe_adoption_necessary_for_displacement, empirically_contingent).
narrative_ontology:cs_axiom('b8a071b9-6938-4a49-bcff-26c59041e7ff', foundational, state_decree_only_accelerates_never_initiates).
narrative_ontology:cs_axiom_status(state_decree_only_accelerates_never_initiates, holdable).
narrative_ontology:cs_axiom_grounding('b8a071b9-6938-4a49-bcff-26c59041e7ff', state_decree_only_accelerates_never_initiates, empirically_contingent).
narrative_ontology:cs_reference_frame('b8a071b9-6938-4a49-bcff-26c59041e7ff', organic_commitment_displacement).
narrative_ontology:cs_drift_state('b8a071b9-6938-4a49-bcff-26c59041e7ff', meiji_restoration_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b8a071b9-6938-4a49-bcff-26c59041e7ff', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, endogenous_climb_scholars).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, state_legitimacy_narratives).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__endogenous_climb_reading, institutional_continuity_advocates).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, exogenous_override_analysts).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, imposed_populations).
narrative_ontology:constraint_victim(imposition_pathway_kernel__endogenous_climb_reading, hybrid_cascade_proponents).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__endogenous_climb_reading, organic_change_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(imposition_pathway_kernel__endogenous_climb_reading, state_as_ratifier_not_imposer).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built the dominant interpretive framework for commitment displacement in historical sociology; careers, journals, and institutional prestige depend on the fringe-adoption pathway being the exclusive mechanism. They set the research agenda, define what counts as evidence, and control the canonical case library (Meiji, Tanzimat, Meiji-equivalents). Exit means abandoning a professional identity constituted through this framework.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, endogenous_climb_scholars, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__endogenous_climb_reading, endogenous_climb_scholars, beneficiary).

% State-building elites and their historiographers benefit when every decree can be framed as ratification of organic change rather than imposition. This narrative converts coercion into continuity, reducing resistance and international scrutiny. The proposition collects no rents directly but enables the extraction of compliance.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, state_legitimacy_narratives, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(imposition_pathway_kernel__endogenous_climb_reading, state_legitimacy_narratives).

% Bureaucrats, jurists, and traditional elites who need state action to appear continuous with tradition. They use the endogenous climb reading to legitimize reforms as organic evolution. Their position depends on the reading's authority; exit means losing the legitimating vocabulary for their projects.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, institutional_continuity_advocates, beneficiary,
    organized, biographical, constrained, national).

% Scholars and analysts who document cases where state capacity displaced commitments without detectable fringe adoption (Soviet collectivization, Meiji land reform decrees, Atatürk's hat law enforcement). Their work is marginalized in canonical journals, excluded from the case library, and treated as 'exceptional' rather than falsifying. They pay in professional invisibility and the cost of maintaining a separate evidentiary base.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, exogenous_override_analysts, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__endogenous_climb_reading, exogenous_override_analysts, excluded).

% Communities that experienced commitment displacement as violent imposition (peasant communes dissolved by decree, religious minorities subjected to dress codes, colonized peoples forced into new legal systems). The endogenous climb reading erases their experience by declaring the fringe stages 'invisible' — their resistance is reclassified as 'late adoption' rather than rejection. They bear the material costs of displacement while the framework denies the mechanism that hurt them.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, imposed_populations, payer,
    powerless, biographical, trapped, local).

% Analysts who argue state imposition creates artificial fringes (state employees, conscript armies, party cadres) that then climb organically. They occupy a middle position but are pressured by both sides: endogenous climbers dismiss the artificial fringe as 'still organic,' exogenous override proponents dismiss the cascade as 'still imposition.' They pay in theoretical fragmentation — their mechanism is squeezed from both sides.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, hybrid_cascade_proponents, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__endogenous_climb_reading, hybrid_cascade_proponents, excluded).

% Sees the full structure: a kernel with three live readings, each held by different coalitions, each with distinct beneficiary and victim sets. The endogenous reading coordinates interpretation across cases but suppresses the exogenous and hybrid readings by definitional fiat — 'no fringe' cases are ruled out of scope rather than explained. The coordination function is real (unified case library, shared vocabulary); the extraction is the exclusion of rival readings and the erasure of imposed populations' experience.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__endogenous_climb_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, portable explanatory framework for commitment displacement across all historical cases: instead of adjudicating each case as imposition vs. organic, analysts apply the fringe-adoption template. This coordinates scholarly work, state legitimization narratives, and comparative historical analysis around one mechanism.
% TRANSFER_FUNCTION: Moves interpretive authority and legitimating capital from exogenous-override and hybrid-cascade analysts to endogenous-climb scholars and state legitimacy narratives. Moves the experience of imposed populations from 'violent displacement' to 'late adoption' — a semantic transfer that converts resistance into lag. Moves the burden of proof: any case that looks like top-down imposition must prove invisible fringe stages existed, rather than the reading proving they did.
% ABSENT_VOICES: Imposed populations (peasant communes, religious minorities, colonized peoples) are structurally excluded — their experience of imposition as violence is reclassified by the framework itself. Exogenous override analysts are excluded from the canonical case library and major journals. Hybrid cascade proponents are squeezed out by both poles. All three would object that the 'invisible fringe' claim is unfalsifiable and functions as a universal exemption for the reading.
% DISAPPEARANCE_RATIONALE: If the endogenous climb reading vanished, state legitimization narratives would lose their primary vocabulary for converting coercion into continuity. Historical sociology would fracture into case-by-case adjudication of imposition vs. organic change. Imposed populations' experience would become admissible as evidence of imposition rather than being pre-classified as 'late adoption.' The exogenous and hybrid readings would compete openly without the endogenous reading's definitional dominance.
% FOUNDING_PROBLEM: Late 19th century comparative sociology needed a general mechanism to explain why some state reforms succeeded while others failed, without admitting that state violence was the variable. The endogenous climb pathway solved this by making 'social readiness' (fringe adoption) the explanatory variable — a variable that could be measured, ranked, and used to predict reform outcomes, while keeping the state's role as ratifier rather than imposer.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the endogenous school's own founding texts (Weber on legitimate order, Eisenstadt on modernization). But the status 'contested' is corroborated by exogenous override analysts (Scott on state simplification, Tilly on coercion-capital) and hybrid cascade proponents (Migdal on state-in-society) who argue the founding problem was mis-specified: state capacity to impose without fringe is the real variable, not fringe readiness. No neutral arbiter has settled this; the endogenous reading's dominance is institutional, not evidentiary.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_pathway_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__endogenous_climb_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the reading captures interpretive authority and legitimating capital across the field — it is the default framework, and alternatives must justify their existence against it. Suppression (0.72) is higher still because the reading's core move ('invisible fringe stages') makes it unfalsifiable: any case without visible fringe is declared to have invisible fringe, not a counterexample. Theater (0.45) is moderate: the coordination function (unified case library, portable mechanism) is real, but a growing share of the framework's activity is defending the 'invisible fringe' claim against accumulating counterevidence. Accessibility collapse (0.62) reflects that once you accept the framework, alternative readings look like category errors rather than live options. Resistance (0.58) is significant: exogenous and hybrid readings persist, imposed populations' oral histories contradict the framework, and new cases (post-colonial state formation) resist the template.
 *
 * PERSPECTIVAL GAP:
 *   From the endogenous_climb_scholar seat, the constraint is a rope (genuine coordination, portable mechanism, net benefit). From the imposed_population seat, it is a snare (erasure of experience, unfalsifiable cover for imposition). From the exogenous_override_analyst seat, it is a tangled_rope (coordinates the field but extracts by suppressing their reading). The engine computes this divergence from the structural data — the declared roles, power, exit, and the beneficiary/victim arrays.
 *
 * DIRECTIONALITY LOGIC:
 *   Endogenous_climb_scholars are agenda_setters and beneficiaries (identity_locked — professional identity fused with framework). State_legitimacy_narratives and institutional_continuity_advocates are beneficiaries (the framework converts their coercion into continuity). Exogenous_override_analysts, imposed_populations, and hybrid_cascade_proponents are payers: they bear the cost of exclusion, erasure, and theoretical squeezing. The analytical_observer sees the full structure. Directionality derives from who controls the case library and definitional boundaries (scholars, states) vs. who is ruled out by those boundaries (critics, victims). Exit options differentiate: scholars are identity_locked (career = framework), imposed populations are trapped (no epistemic exit), analysts are constrained (can publish but not in canonical venues).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (explaining reform success/failure without admitting state violence) is contested: exogenous analysts say state violence IS the variable; hybrid proponents say state-created fringes are the mechanism. The endogenous reading persists because it solves a coordination problem for scholars (one framework) and a legitimization problem for states (coercion as continuity). But its mandate has atrophied: the 'invisible fringe' move makes it unfalsifiable, and accumulating cases (Atatürk, Soviet, post-colonial) strain the template. The reading is not a piton (it has concentrated beneficiaries who actively maintain it) but a tangled_rope with rising extraction — the coordination function is real but the extraction is growing as counterevidence mounts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fringe_visibility_falsifiability,
    'Is the ''invisible fringe stages'' claim falsifiable, or does it function as a universal immunization against counterevidence?',
    'Systematic case audit: for each canonical case (Meiji, Tanzimat, Atatürk, Soviet, post-colonial), identify the specific evidence for pre-decree fringe adoption vs. state-created fringe. If ''invisible fringe'' is invoked for cases where archival research shows no fringe, the claim is unfalsifiable.',
    'If unfalsifiable, the reading''s coordination function is parasitic — it coordinates by ruling out alternatives by definition, not by evidence. This would raise extractiveness and suppression, confirming tangled_rope or snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fringe_visibility_falsifiability, conceptual, 'Whether the core theoretical move (''invisible fringe'') is evidentiary or definitional.').

omega_variable(
    meiji_fringe_evidence,
    'Did the Meiji calendar and dress decrees (1872-1873) have genuine pre-decree fringe adoption in treaty ports/merchant class/military, or was the fringe created by the state (conscript army, state bureaucracy, mission schools)?',
    'Archival research on adoption curves: merchant adoption of Gregorian calendar before 1872, Western dress in treaty ports vs. state-mandated uniform adoption, military modernizers as state-created fringe.',
    'If Meiji fringe was state-created, the reading''s canonical case becomes a hybrid cascade or exogenous override, undermining the reading''s empirical anchor. If genuine pre-decree fringe existed, the reading gains empirical support but the ''invisible fringe'' move remains unfalsifiable for other cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_fringe_evidence, empirical, 'Empirical status of the reading''s flagship case.').

omega_variable(
    state_legitimacy_extraction,
    'How much of the reading''s persistence is driven by its utility to state legitimization narratives vs. its explanatory power?',
    'Citation network analysis: trace adoption of the endogenous climb framework in state-commissioned histories, constitutional preambles, and reform justifications vs. independent scholarly work. Measure correlation between state capacity and framework adoption.',
    'If state legitimization is the primary driver, the reading is a snare (coordination story is cover for extraction). If scholarly coordination is primary with state benefit as side effect, it remains tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_legitimacy_extraction, preference, 'Primary driver of the reading''s institutional dominance.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the imposition_pathway_kernel admit only these three readings, or are there other structurally distinct pathways (e.g., crisis-driven displacement, demographic displacement, technological displacement) that the kernel''s framing excludes?',
    'Systematic literature review for displacement mechanisms not captured by fringe-adoption, exogenous-override, or hybrid-cascade. Code each mechanism for structural distinctness.',
    'If the kernel''s three-reading framing excludes real mechanisms, the kernel itself is a false frame — the contest is not between three readings but between an incomplete typology and reality. This would reframe all three readings as partial rather than competing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s reading-set is exhaustive or artificially constrained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__endogenous_climb_reading, 1868, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_tr_t1868, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1868, 0.15).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_tr_t1900, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1900, 0.22).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_tr_t1930, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1930, 0.31).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_tr_t1960, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1960, 0.38).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_tr_t1990, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 1990, 0.42).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_tr_t2024, imposition_pathway_kernel__endogenous_climb_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_be_t1868, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1868, 0.25).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_be_t1900, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_be_t1930, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1930, 0.45).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_be_t1960, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_be_t1990, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_be_t2024, imposition_pathway_kernel__endogenous_climb_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_su_t1868, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1868, 0.3).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_su_t1900, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1900, 0.42).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_su_t1930, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1930, 0.55).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_su_t1960, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1960, 0.62).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_su_t1990, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(imposition_pathway_kernel__endogenous_climb_reading_su_t2024, imposition_pathway_kernel__endogenous_climb_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__endogenous_climb_reading, 0.08).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__endogenous_climb_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the imposition_pathway_kernel. The kernel decomposes into three constraint stories (endogenous_climb, exogenous_override, hybrid_cascade) linked by affects_constraints. The endogenous reading claims universal scope (all displacement is fringe-climb); the exogenous reading claims a distinct mechanism exists; the hybrid claims a two-stage mechanism. Their ε values differ: endogenous has high extraction (suppresses alternatives), exogenous has moderate (specialized mechanism), hybrid has moderate (two-stage complexity). The decomposition follows the ε-invariance principle: one kernel label, multiple structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
