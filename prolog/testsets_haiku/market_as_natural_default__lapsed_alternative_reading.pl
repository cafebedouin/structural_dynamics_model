% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market as Natural Default (Lapsed Alternative Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This constraint describes market organization appearing natural and
 *   inevitable through a reading that emphasizes historical amnesia rather
 *   than active closure or beneficiary maintenance. The kernel
 *   'market_as_natural_default' is contested across three readings: (1) the
 *   beneficiary_maintained_reading frames market naturalization as actively
 *   defended post-hoc by incumbents who benefit from market organization; (2)
 *   the hybrid_amnesia_reading views initial lapsed memory as creating the
 *   conditions for subsequent beneficiary capture; (3) this
 *   lapsed_alternative_reading (instantiated here) treats the naturalization
 *   as a D3 artifact—an emergent property of ordinary processes of
 *   forgetting, not of deliberate suppression or beneficiary maintenance.
 *   Under this reading, market dominance persists not because anyone is
 *   actively defending it or because powerful actors suppress alternatives,
 *   but because the institutional memory of non-market alternatives has
 *   decayed through generational turnover, archival loss, language death, and
 *   educational canonization. The constraint has low extractiveness (ε=0.08)
 *   because there is no concentrated beneficiary class extracting rents;
 *   there is minimal active suppression because alternatives are forgotten
 *   rather than forbidden; there is no theater because no one is maintaining
 *   an elaborate performance—the forgetting is passive and structural.
 *
 * KEY AGENTS:
 *   - contemporary_economic_actors: participate in market systems experienced as natural/inevitable; low consciousness of alternatives
 *   - economic_historians_and_archivists: maintain and recover records of non-market systems; their scholarship is the primary mechanism for undoing forgetting
 *   - institutional_memory_holders: carry fragmented knowledge of non-market practices; increasingly isolated and marginalized
 *   - educational_and_textual_systems: institutional agenda-setters that canonize market-centric history through curriculum emphasis
 *   - archive_and_library_institutions: control which historical documents are preserved and accessible; systematic loss accelerates amnesia
 *   - language and translation systems: structural phenomenon encoding market vocabulary disproportionately; non-market economic concepts untranslated or lost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.08).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.12).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market as Natural Default (Lapsed Alternative Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '86cc0c3f-f570-4ead-9c94-c345e573ec69').
narrative_ontology:cs_kernel_codification('86cc0c3f-f570-4ead-9c94-c345e573ec69', implicit).
narrative_ontology:cs_authority_grounding('86cc0c3f-f570-4ead-9c94-c345e573ec69', diffuse_epistemic).
narrative_ontology:cs_reading_relation('86cc0c3f-f570-4ead-9c94-c345e573ec69', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('86cc0c3f-f570-4ead-9c94-c345e573ec69', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('86cc0c3f-f570-4ead-9c94-c345e573ec69', foundational, market_naturalization_via_passive_forgetting).
narrative_ontology:cs_axiom_status(market_naturalization_via_passive_forgetting, holdable).
narrative_ontology:cs_axiom_grounding('86cc0c3f-f570-4ead-9c94-c345e573ec69', market_naturalization_via_passive_forgetting, empirically_contingent).
narrative_ontology:cs_axiom('86cc0c3f-f570-4ead-9c94-c345e573ec69', foundational, alternatives_recoverable_through_scholarship).
narrative_ontology:cs_axiom_status(alternatives_recoverable_through_scholarship, holdable).
narrative_ontology:cs_axiom_grounding('86cc0c3f-f570-4ead-9c94-c345e573ec69', alternatives_recoverable_through_scholarship, empirically_contingent).
narrative_ontology:cs_reference_frame('86cc0c3f-f570-4ead-9c94-c345e573ec69', pre_market_integration_institutional_diversity).
narrative_ontology:cs_drift_state('86cc0c3f-f570-4ead-9c94-c345e573ec69', contemporary_market_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('86cc0c3f-f570-4ead-9c94-c345e573ec69', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, historical_research_community).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely low (0.08) because this reading denies any concentrated extraction mechanism. No actor deliberately closes off alternatives; no coalition maintains market naturalization for rent collection. The constraint persists through passive forgetting: generational turnover means knowledge is not passed on; archives are not explicitly destroyed but decay through neglect; minority languages carrying non-market concepts die as speakers shift to dominant languages; educational curricula emphasize market examples without active censorship of alternatives. Suppression (0.12) is minimal because it is not active exclusion but informational absence. Theater_ratio is zero because no performative maintenance is required—the constraint operates through the natural decay of institutional memory. Accessibility_collapse is very high (0.92) because once alternatives are forgotten, they are nearly inaccessible to those who have never encountered them; recovering them requires specialized historical scholarship. Resistance is minimal (0.05) because there is no focal point to resist—you cannot resist something you do not know exists. The measurement series are flat across all six time points because this reading claims the mechanism is structural and generational, not drifting. Extractiveness does not accumulate because no one is actively layering additional rents onto the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of contemporary economic actors, the market appears natural and inevitable—they do not consciously perceive the constraint as a constraint because it shapes what they take to be the bounds of reality. From the perspective of historians and archivists, the constraint is visible as a problem of loss and recovery; they see forgetting happening in real time (minority languages dying, archives being deaccessioned, institutional histories being forgotten). From the perspective of institutional memory holders in diaspora or indigenous communities, the constraint is directly experienced as pressure: their knowledge of non-market systems is devalued, not transmitted to children who are educated in market terms, and isolated from the broader intellectual community. Educational administrators experience their curriculum choices as neutral factual coverage ('we teach what economists agree on') rather than as choices that embed particular naturalizations. The engine should compute this divergence as a signature of a genuinely structural constraint that lacks a focal extractive beneficiary—the constraint persists not because any party is defending it, but because ordinary institutional processes embed it.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading produces no meaningful directionality computation in the traditional sense because there is no extraction mechanism and no consolidated beneficiary. The stakeholders listed include (a) contemporary economic actors who are observers/analysts rather than targets—they are shaped by the constraint but not deliberately harmed; (b) historians and archivists listed as 'beneficiary' in a technical sense because scholarship that recovers alternatives is the countermeasure to the constraint, so those who do that work benefit (in the sense that they have a role to play that exists because of the constraint's operation), but they do not capture rents from the constraint—they work against it; (c) educational and archival systems listed as agenda-setters because their choices (what to preserve, what to teach) shape the constraint's operation, but they are not modeled as deliberately maintaining it for extraction purposes—their agenda-setting happens through ordinary institutional operation, not through coordinated conspiracy. The directionality is near zero across most agents because the constraint is not extractive in the traditional sense. The 'beneficiary' label on historians is a technical anomaly arising from the requirement to label all constraint stakeholders; it is addressed in the omega variable on beneficiary ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (gap between founding problem and current function) is explicitly addressed in this reading but in a novel way. The founding problem identified by this reading is not 'the market solves coordination problems' but rather 'institutional memory of non-market alternatives is naturally lost.' This founding problem is live and persistent—every generation experiences information loss and must relearn or recover what was forgotten. There is no mandatrophy gap because the constraint's function (generating natural forgetting through ordinary processes) persists unchanged. This distinguishes the lapsed_alternative_reading from the beneficiary_maintained_reading (which would identify mandatrophy as the market solving its original coordination problem while persisting as extraction) and from the hybrid_amnesia_reading (which would identify a gap between initial forgetting and subsequent beneficiary capture).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_status_of_historians,
    'Should historians and archivists who recover alternatives be classified as beneficiaries when their work counters the constraint rather than maintains it?',
    'Clarify the beneficiary classification: a beneficiary typically means one who profits from or benefits incidentally from the constraint''s operation. Historians benefit in the sense that their role exists because of the constraint''s operation, but they work to undo it. This is a labeling ambiguity in the stakeholder surface, not a substantive disagreement about the constraint''s mechanism.',
    'If historians are removed from beneficiary classification, the constraint has zero declared beneficiaries, which flags (via FSM) the possibility that this is a false mountain—a constraint claimed as natural law but operating as an artifact of institutional choices about what to preserve. The FSM signature would be correct under the lapsed_alternative_reading: the constraint is not a natural law, but it is also not actively maintained by identifiable beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_status_of_historians, conceptual, 'Ambiguity in the beneficiary label when the constraint''s countermeasure is a stakeholder''s primary function.').

omega_variable(
    passive_vs_designed_forgetting,
    'Is the observed memory loss adequately explained as ordinary decay (passive forgetting) or does the evidence support deliberate erasure and active suppression by beneficiaries?',
    'Historical investigation of archival practices, educational curriculum decisions, and language policies. If systematic evidence of deliberate suppression emerges (e.g., colonial authorities deliberately destroying records of indigenous non-market systems; educational boards explicitly excluding alternative models; publishing industries deprioritizing works on non-market history), the reading would shift toward beneficiary_maintained_reading or hybrid_amnesia_reading.',
    'This omega is structural to the three-reading contest. If passive forgetting is the mechanism, this reading (lapsed_alternative) is correct and extractiveness remains ~0.08. If deliberate suppression is evidenced, extractiveness should be reclassified higher and beneficiary identification becomes possible, shifting to one of the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passive_vs_designed_forgetting, empirical, 'Whether observed memory loss is best explained as passive structural decay or as deliberate suppression.').

omega_variable(
    recovery_reversibility,
    'Are forgotten alternatives in principle recoverable through archival research and historical scholarship, or have some alternatives been permanently erased?',
    'Attempts to recover documented non-market systems from archival sources. Success at recovery indicates alternatives were forgotten but not destroyed; permanent gaps in the record indicate irreversible erasure.',
    'If alternatives are largely recoverable, the constraint operates at the level of informational access, suggesting low barriers to reversal. If many alternatives have been permanently erased, the constraint has a destructive component and is less naturally reversible than the lapsed_alternative_reading suggests, making the constraint more robust and possibly shifting the type toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recovery_reversibility, empirical, 'Whether forgotten alternatives are recoverable or permanently lost.').

omega_variable(
    readings_as_kernel_contest,
    'Are the three readings (beneficiary_maintained, hybrid_amnesia, lapsed_alternative) genuinely three incompatible interpretations of the same kernel, or do they model different constraints altogether?',
    'The three readings share a kernel claim: ''market organization appears natural and inevitable in contemporary consciousness.'' They differ on mechanism: (1) active beneficiary maintenance, (2) initial forgetting enabling beneficiary capture, (3) passive forgetting without beneficiary maintenance. If evidence emerges that each reading is measuring a different constraint (different ε, different stakeholders, different causal paths), they should be decomposed as separate constraints rather than as readings of one kernel.',
    'If the readings are genuinely alternative interpretations of one kernel, the presence of all three in the corpus enables calibration of the reading-selection machinery. If they are actually separate constraints, they should be issued as independent files with network links, not as readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readings_as_kernel_contest, conceptual, 'Whether the three readings are alternative framings of one kernel or distinct constraints requiring decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(mark_tr_t10, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 10, 0.0).
narrative_ontology:measurement(mark_tr_t20, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 20, 0.0).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 30, 0.0).
narrative_ontology:measurement(mark_tr_t40, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 40, 0.0).
narrative_ontology:measurement(mark_tr_t50, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 50, 0.0).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(mark_be_t10, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 10, 0.08).
narrative_ontology:measurement(mark_be_t20, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement(mark_be_t40, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(mark_be_t50, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 50, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(mark_su_t10, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(mark_su_t20, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(mark_su_t40, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(mark_su_t50, market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, information_standard).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__lapsed_alternative_reading, 0.08).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'market_as_natural_default'. All three readings describe how market organization comes to appear natural and inevitable, but they differ on mechanism: (1) lapsed_alternative_reading (this file) attributes naturalization to passive forgetting of non-market alternatives through ordinary institutional processes; (2) beneficiary_maintained_reading attributes naturalization to active post-hoc defense by incumbent beneficiaries; (3) hybrid_amnesia_reading treats initial forgetting as creating conditions for beneficiary capture. The three readings have different ε values (this reading ≤0.15), different beneficiary structures (this reading has none or only historians as technical anomaly), and different persistence mechanisms. They are linked via affects_constraints to enable contrastive analysis of the evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
