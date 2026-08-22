% ============================================================================
% CONSTRAINT STORY: takings_clause_boundary__categorical_takings_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_takings_clause_boundary__categorical_takings_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: takings_clause_boundary__categorical_takings_reading
 *   human_readable: Categorical Takings Doctrine — Per Se Rules at Poles, Penn Central in Middle
 *   domain: constitutional/property/regulatory
 *
 * SUMMARY:
 *   This constraint story instantiates the categorical_takings_reading of the
 *   takings_clause_boundary kernel: permanent physical occupations (Loretto)
 *   and total value eliminations (Lucas) are per se takings requiring
 *   compensation; all other regulatory impacts are evaluated under the Penn
 *   Central ad hoc balancing test. The reading creates a three-tier structure
 *   — bright-line rules at the poles for predictability, contextual balancing
 *   in the middle for flexibility. Property owners gain certainty at the
 *   extremes but face significant uncertainty in the vast middle ground where
 *   most regulations operate. The constraint is actively enforced through
 *   judicial review; its persistence depends on courts maintaining the
 *   categorical distinctions while applying Penn Central. This reading
 *   competes with the physical_appropriation_reading (narrower, only physical
 *   seizures count) and regulatory_takings_reading (broader, 'too far'
 *   standard without bright lines).
 *
 * KEY AGENTS:
 *   - property_owners_at_poles: Beneficiaries of bright-line predictability (powerful/constrained) — receive clear compensation rights for Loretto/Lucas-category regulations
 *   - property_owners_in_middle: Victims of Penn Central unpredictability (moderate/constrained) — bear costs of regulatory uncertainty, litigation risk, and asymmetric outcomes
 *   - regulators_facing_uncertainty: Victims of doctrinal complexity (institutional/constrained) — must navigate three-tier framework, face litigation risk at poles and unpredictability in middle
 *   - courts_as_adjudicators: Agenda-setters and beneficiaries (institutional/analytical) — control doctrinal boundaries, collect institutional legitimacy from administering the framework
 *   - repeat_player_litigants: Secondary beneficiaries (organized/arbitrage) — exploit Penn Central indeterminacy through resource advantages
 *   - academic_observers: Observers (analytical/analytical) — track doctrinal evolution, boundary migration, and systemic effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(takings_clause_boundary__categorical_takings_reading, 0.45).
domain_priors:suppression_score(takings_clause_boundary__categorical_takings_reading, 0.65).
domain_priors:theater_ratio(takings_clause_boundary__categorical_takings_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(takings_clause_boundary__categorical_takings_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(takings_clause_boundary__categorical_takings_reading, tangled_rope).
narrative_ontology:human_readable(takings_clause_boundary__categorical_takings_reading, "Categorical Takings Doctrine — Per Se Rules at Poles, Penn Central in Middle").
narrative_ontology:topic_domain(takings_clause_boundary__categorical_takings_reading, "constitutional/property/regulatory").

domain_priors:requires_active_enforcement(takings_clause_boundary__categorical_takings_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(takings_clause_boundary__categorical_takings_reading, '459217dd-7ab2-4eff-b69c-89b148564743').
narrative_ontology:cs_kernel_codification('459217dd-7ab2-4eff-b69c-89b148564743', fixed_text).
narrative_ontology:cs_authority_grounding('459217dd-7ab2-4eff-b69c-89b148564743', lineage).
narrative_ontology:cs_interpretation_layer_present('459217dd-7ab2-4eff-b69c-89b148564743').
narrative_ontology:cs_reading_relation('459217dd-7ab2-4eff-b69c-89b148564743', takings_clause_boundary__physical_appropriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('459217dd-7ab2-4eff-b69c-89b148564743', takings_clause_boundary__regulatory_takings_reading, coexists_with).
narrative_ontology:cs_axiom('459217dd-7ab2-4eff-b69c-89b148564743', foundational, permanent_physical_occupation_per_se_taking).
narrative_ontology:cs_axiom_status(permanent_physical_occupation_per_se_taking, holdable).
narrative_ontology:cs_axiom_grounding('459217dd-7ab2-4eff-b69c-89b148564743', permanent_physical_occupation_per_se_taking, conventional).
narrative_ontology:cs_axiom('459217dd-7ab2-4eff-b69c-89b148564743', foundational, total_value_elimination_per_se_taking).
narrative_ontology:cs_axiom_status(total_value_elimination_per_se_taking, holdable).
narrative_ontology:cs_axiom_grounding('459217dd-7ab2-4eff-b69c-89b148564743', total_value_elimination_per_se_taking, conventional).
narrative_ontology:cs_axiom('459217dd-7ab2-4eff-b69c-89b148564743', foundational, penn_central_balancing_for_middle_ground).
narrative_ontology:cs_axiom_status(penn_central_balancing_for_middle_ground, holdable).
narrative_ontology:cs_axiom_grounding('459217dd-7ab2-4eff-b69c-89b148564743', penn_central_balancing_for_middle_ground, conventional).
narrative_ontology:cs_reference_frame('459217dd-7ab2-4eff-b69c-89b148564743', categorical_takings_framework).
narrative_ontology:cs_drift_state('459217dd-7ab2-4eff-b69c-89b148564743', contemporary_doctrinal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('459217dd-7ab2-4eff-b69c-89b148564743', '2026-08-20T14:30:00Z').
narrative_ontology:cs_kernel_id(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, property_owners_at_poles).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, courts_as_adjudicators).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, regulators_facing_uncertainty).
narrative_ontology:constraint_victim(takings_clause_boundary__categorical_takings_reading, property_owners_in_middle).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, regulators_facing_uncertainty).
narrative_ontology:constraint_beneficiary(takings_clause_boundary__categorical_takings_reading, repeat_player_litigants).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, property_rights_as_fundamental).
narrative_ontology:constraint_vindicates(takings_clause_boundary__categorical_takings_reading, regulatory_state_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Property owners whose regulations fall into the Loretto (permanent physical occupation) or Lucas (total value elimination) categories. They receive clear, predictable compensation entitlements without balancing tests. Their exit is constrained — they cannot leave the constitutional framework — but the bright-line rules give them strong leverage in negotiation and litigation.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_at_poles, beneficiary,
    powerful, biographical, constrained, national).

% Property owners facing regulations that diminish value but do not reach total wipeout or permanent physical occupation. They must litigate under Penn Central's unpredictable multi-factor balancing test. They bear high litigation costs, planning uncertainty, and systematic disadvantage against repeat-player regulators and developers. Exit is constrained — they cannot avoid the regulatory state — and the framework's indeterminacy extracts a risk premium from their property values.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, property_owners_in_middle, payer,
    moderate, biographical, constrained, national).

% Government actors at all levels who must design regulations within the three-tier framework. They benefit from the middle ground's flexibility (can pursue public goals without automatic compensation) but pay high compliance costs: litigation defense, planning uncertainty, risk of per se liability at poles, and inability to use legislative fixes for constitutionalized doctrine. Their exit is constrained — they must govern — but they have institutional resources to navigate the framework.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, regulators_facing_uncertainty, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(takings_clause_boundary__categorical_takings_reading, regulators_facing_uncertainty, beneficiary).

% Judicial actors who administer the three-tier framework: police the per se boundaries, apply Penn Central factors, and resolve doctrinal tensions. They collect institutional legitimacy from providing a 'workable' constitutional structure. Their analytical exit means they can observe the framework's operation without being bound by its constraints as a regulated party. They set the agenda through case selection, boundary-drawing, and factor-weighting in Penn Central.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, courts_as_adjudicators, agenda_setter,
    institutional, generational, analytical, national).

% Sophisticated developers, institutional landowners, and government entities who litigate repeatedly under Penn Central. They exploit the framework's indeterminacy through resource advantages: better experts, strategic case selection, amicus networks, and ability to shape precedent. Their arbitrage-grade exit means they can choose when and where to litigate, forum-shop, and absorb losses as portfolio costs. They are not declared in base_properties.beneficiaries because their benefit is structural (emergent from the framework's design) not doctrinal (the framework does not name them as intended beneficiaries).
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, repeat_player_litigants, beneficiary,
    organized, biographical, arbitrage, national).

% Legal scholars, economists, and political scientists who track doctrinal evolution, measure boundary migration, and analyze systemic effects. They have analytical exit (no stake in outcomes) and civilizational time horizon (track multi-generational doctrinal cycles). Their situation is descriptive: they document whether the per se categories hold, whether Penn Central converges or diverges, and whether the framework coordinates or extracts.
narrative_ontology:constraint_stakeholder(takings_clause_boundary__categorical_takings_reading, academic_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional framework that balances property protection against regulatory necessity by establishing clear compensation rules for the most extreme government actions (permanent physical occupation, total value elimination) while preserving regulatory flexibility for the vast middle ground through contextual balancing.
% TRANSFER_FUNCTION: Moves litigation risk, planning uncertainty, and compliance costs from property owners at the poles (who gain clear compensation rights) to property owners in the middle (who bear unpredictable Penn Central outcomes) and regulators (who bear framework navigation costs). Repeat players extract advantage from middle-ground indeterminacy. Courts collect institutional legitimacy from administering the system.
% ABSENT_VOICES: Future property owners and future regulatory subjects — those who will inherit the doctrinal framework but have no voice in its current calibration. Also absent: small-scale property owners who lack resources to litigate Penn Central cases and thus never shape the balancing factors' evolution. They would object to a system where their vulnerability is calibrated by repeat players' litigation strategies.
% DISAPPEARANCE_RATIONALE: If the categorical/Penn Central framework vanished overnight, the regulatory state would face immediate constitutional vacuum: no clear rule for when compensation is required. Legislatures would need to enact statutory compensation schemes; courts would revert to ad hoc due process or develop new constitutional tests. Property owners would lose both the per se protections at poles and the (imperfect) middle-ground framework. The world would rearrange significantly — this is not a natural law but a constructed doctrinal architecture.
% FOUNDING_PROBLEM: The post-New Deal constitutional crisis: how to police regulatory overreach without either paralyzing the regulatory state (Lochner-era substantive due process) or abandoning property protection entirely (complete deference). The categorical/Penn Central framework was built to stabilize this tension — bright lines for egregious cases, flexibility for governance.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (regulatory state legitimacy vs. property protection) is attested as still live by property rights advocates, originalist scholars, and state attorneys general. It is attested as substantially evolved by progressive constitutional scholars, regulatory agencies, and planning organizations who argue the regulatory state's legitimacy is settled and the framework now serves rent-seeking. No consensus corroboration exists — the dispute is the doctrinal battlefield.
narrative_ontology:disappearance_verdict(takings_clause_boundary__categorical_takings_reading, world_rearranges).
narrative_ontology:founding_problem_status(takings_clause_boundary__categorical_takings_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(takings_clause_boundary__categorical_takings_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(takings_clause_boundary__categorical_takings_reading, 'none', 1).
narrative_ontology:epsilon_provenance(takings_clause_boundary__categorical_takings_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(takings_clause_boundary__categorical_takings_reading_tests).
:- end_tests(takings_clause_boundary__categorical_takings_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the asymmetric cost distribution: property owners at poles gain clear entitlements (low extraction for them), but the large middle ground generates litigation costs, planning uncertainty, and repeat-player advantages that extract from less-resourced owners and regulators. Suppression (0.65) is substantial — the constraint actively prevents legislative solutions by constitutionalizing the framework, and courts police the boundaries through judicial review. Theater ratio (0.35) captures the gap between the doctrine's stated goal (predictability + flexibility) and its operation: the per se categories are narrowing (Loretto's permanent occupation requirement, Lucas's total wipeout standard) while Penn Central expands unpredictably. Accessibility collapse (0.55) is moderate — alternative regulatory designs exist but are constrained by the three-tier framework. Resistance (0.60) is significant: academic criticism, legislative pushback, and judicial dissents all contest the framework's coherence.
 *
 * PERSPECTIVAL GAP:
 *   Property owners at poles experience this as a rope (clear coordination benefit: predictable compensation for clear harms). Property owners in the middle experience it as a snare (unpredictable balancing, high litigation costs, repeat-player disadvantage). Regulators experience it as a tangled rope (genuine need for constitutional constraint on overreach, but the framework's complexity and unpredictability extract compliance costs). Courts experience it as a scaffold (transitional structure meant to balance competing commitments, but the transition has lasted 40+ years). The engine computes these seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations plus exit modulation. Property owners at poles (beneficiaries) have constrained exit (cannot leave constitutional framework) but gain clear entitlements — d near 0.2. Property owners in middle (victims) face constrained exit with high litigation exposure — d near 0.75. Regulators (victims) have institutional power but constrained exit (must govern within framework) — d near 0.6. Courts (agenda_setters) have analytical exit and institutional control — d near 0.15. Repeat players (not declared as beneficiaries/victims) have arbitrage-grade exit and resource advantages — d would be near 0.1 if declared. The three-tier structure creates this dispersion: same nominal 'property owner' category splits into diametrically opposed structural positions based on where their regulation falls.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (balancing property protection against regulatory necessity) remains live but the categorical/Penn Central framework has developed extraction pathologies: the bright lines have narrowed while the middle has expanded unpredictably, creating a system that coordinates at the poles but extracts in the middle. The constraint is not a false summit (it acknowledges its constructed nature), but it shows mandatrophy symptoms: the original Penn Central factors have become ritualized, the per se categories shrink under pressure, and the middle ground's unpredictability benefits repeat players. This is not a piton — the framework still resolves real disputes — but it has significant tangential extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the categorical_takings_reading foreclose, coexist with, or influence the physical_appropriation_reading and regulatory_takings_reading within a single constitutional framework?',
    'Doctrinal analysis of whether any single judicial coalition could simultaneously hold the core premises of multiple readings; historical tracking of whether courts adopt hybrid or pure readings.',
    'If forecloses: readings are mutually exclusive structural commitments; if coexists_with: different parties legitimately hold different readings; if influences: downstream doctrinal pressure without logical resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship of this reading to sibling readings of the takings_clause_boundary kernel').

omega_variable(
    penn_central_indeterminacy,
    'Is the Penn Central balancing test''s unpredictability in the ''middle ground'' a genuine coordination cost or an extraction mechanism that benefits sophisticated litigants?',
    'Empirical study of litigation outcomes by party type and resource level; analysis of whether unpredictability correlates with repeat-player advantage.',
    'If coordination cost: theater_ratio lower, constraint more rope-like; if extraction mechanism: theater_ratio higher, constraint more snare-like for middle-ground property owners.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(penn_central_indeterminacy, empirical, 'Whether Penn Central''s flexibility functions as genuine coordination or as asymmetric extraction via unpredictability').

omega_variable(
    per_se_boundary_stability,
    'Do the per se categories (permanent physical occupation, total value elimination) remain stable bright lines, or do they migrate under litigation pressure?',
    'Longitudinal case law analysis tracking the boundaries of Loretto and Lucas categories over time; measurement of boundary contraction or expansion.',
    'If boundaries migrate toward middle: categorical rules are performative, constraint drifts toward piton; if stable: genuine coordination anchors exist at poles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(per_se_boundary_stability, empirical, 'Whether the bright-line categories at the poles resist erosion or collapse into the Penn Central middle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(takings_clause_boundary__categorical_takings_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(takings_categorical_tr_t0, takings_clause_boundary__categorical_takings_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(takings_categorical_tr_t10, takings_clause_boundary__categorical_takings_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(takings_categorical_tr_t20, takings_clause_boundary__categorical_takings_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(takings_categorical_tr_t30, takings_clause_boundary__categorical_takings_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(takings_categorical_tr_t40, takings_clause_boundary__categorical_takings_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement(takings_categorical_tr_t50, takings_clause_boundary__categorical_takings_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(takings_categorical_be_t0, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(takings_categorical_be_t10, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(takings_categorical_be_t20, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(takings_categorical_be_t30, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(takings_categorical_be_t40, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(takings_categorical_be_t50, takings_clause_boundary__categorical_takings_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(takings_categorical_su_t0, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(takings_categorical_su_t10, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(takings_categorical_su_t20, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(takings_categorical_su_t30, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(takings_categorical_su_t40, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(takings_categorical_su_t50, takings_clause_boundary__categorical_takings_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(takings_clause_boundary__categorical_takings_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(takings_clause_boundary__categorical_takings_reading, 0.12).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__physical_appropriation_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, takings_clause_boundary__regulatory_takings_reading).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, penn_central_balancing_test).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, loretto_permanent_occupation_doctrine).
narrative_ontology:affects_constraint(takings_clause_boundary__categorical_takings_reading, lucas_total_wipeout_doctrine).

% DUAL FORMULATION NOTE:
% The takings_clause_boundary kernel decomposes into three constraint stories: this categorical_takings_reading (three-tier: per se at poles, Penn Central in middle), physical_appropriation_reading (single-tier: only physical appropriation counts), and regulatory_takings_reading (single-tier: 'too far' balancing without bright lines). Each has distinct ε, beneficiaries, and victims. They are linked via network.affects_constraints. The ε-invariance principle requires this decomposition: measuring 'the Takings Clause' as one constraint would conflate structurally distinct claims with different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(takings_clause_boundary__categorical_takings_reading, institutional, 0.15).
constraint_indexing:directionality_override(takings_clause_boundary__categorical_takings_reading, powerful, 0.2).
constraint_indexing:directionality_override(takings_clause_boundary__categorical_takings_reading, moderate, 0.75).
constraint_indexing:directionality_override(takings_clause_boundary__categorical_takings_reading, organized, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
