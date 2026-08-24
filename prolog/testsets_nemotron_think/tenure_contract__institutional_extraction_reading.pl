% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__institutional_extraction_reading, []).

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
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure as Institutional Extraction
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This reading of the tenure contract treats tenure not as a shield for
 *   academic freedom but as a structural rent: a permanent, non-revocable
 *   claim on institutional resources awarded to early-career winners of a
 *   tournament that has become increasingly decoupled from ongoing
 *   contribution. The extraction is three-sided: tenured faculty capture
 *   salary and governance rents; contingent faculty pay through precarized
 *   labor that subsidizes the tenure line's resource floor; students pay
 *   through tuition that funds the rigidity. The coordination function
 *   (long-term research insulation) is real but shrinking as a share of the
 *   system's operation — theater_ratio rises as tenure-review rituals and
 *   'collegiality' votes perform governance while contingentization expands.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.78).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.72).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Institutional Extraction").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '0104847f-ecd0-4e37-a305-94e4fdee5eba').
narrative_ontology:cs_kernel_codification('0104847f-ecd0-4e37-a305-94e4fdee5eba', formalized).
narrative_ontology:cs_authority_grounding('0104847f-ecd0-4e37-a305-94e4fdee5eba', extraction).
narrative_ontology:cs_interpretation_layer_present('0104847f-ecd0-4e37-a305-94e4fdee5eba').
narrative_ontology:cs_reading_relation('0104847f-ecd0-4e37-a305-94e4fdee5eba', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('0104847f-ecd0-4e37-a305-94e4fdee5eba', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('0104847f-ecd0-4e37-a305-94e4fdee5eba', foundational, tenure_as_permanent_resource_claim).
narrative_ontology:cs_axiom_status(tenure_as_permanent_resource_claim, holdable).
narrative_ontology:cs_axiom_grounding('0104847f-ecd0-4e37-a305-94e4fdee5eba', tenure_as_permanent_resource_claim, empirically_contingent).
narrative_ontology:cs_axiom('0104847f-ecd0-4e37-a305-94e4fdee5eba', foundational, employment_rigidity_prevents_reallocation).
narrative_ontology:cs_axiom_status(employment_rigidity_prevents_reallocation, holdable).
narrative_ontology:cs_axiom_grounding('0104847f-ecd0-4e37-a305-94e4fdee5eba', employment_rigidity_prevents_reallocation, empirically_contingent).
narrative_ontology:cs_axiom('0104847f-ecd0-4e37-a305-94e4fdee5eba', secondary, contingent_labor_subsidizes_tenure_floor).
narrative_ontology:cs_axiom_status(contingent_labor_subsidizes_tenure_floor, holdable).
narrative_ontology:cs_axiom_grounding('0104847f-ecd0-4e37-a305-94e4fdee5eba', contingent_labor_subsidizes_tenure_floor, empirically_contingent).
narrative_ontology:cs_reference_frame('0104847f-ecd0-4e37-a305-94e4fdee5eba', tenure_as_earned_property_right).
narrative_ontology:cs_drift_state('0104847f-ecd0-4e37-a305-94e4fdee5eba', contemporary_contingent_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0104847f-ecd0-4e37-a305-94e4fdee5eba', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, administrators).
narrative_ontology:constraint_vindicates(tenure_contract__institutional_extraction_reading, institutional_stability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent employment claims with salary and resource guarantees that persist regardless of departmental need or enrollment shifts. Control curriculum, hiring, and governance through faculty senates and tenure-review committees. Exit means forfeiting a lifetime accumulation of status, pension, and institutional capital — structurally constrained rather than trapped.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, tenured_faculty, agenda_setter).

% Teach a majority of undergraduate courses on term-limited contracts with low pay, no benefits, and no path to tenure. Absorb enrollment fluctuations and curricular demands that tenured faculty decline. Exit options are limited: adjuncting elsewhere replicates the same precarity; leaving academia means abandoning specialized training. Their labor subsidizes the resource floor that tenure protects.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    moderate, immediate, constrained, national).

% Bear tuition increases that fund tenured faculty salaries and institutional overhead while instructional contact shifts toward under-supported contingent faculty. Have no voice in governance and limited ability to exit — transferring institutions costs credits, time, and money. The credential's labor-market value forces continued participation.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students, payer,
    powerless, biographical, constrained, national).

% Manage the tenure system's budgetary impact by expanding contingent appointments and deferring maintenance. Benefit from tenure's legitimacy signal for accreditation and fundraising. Can move between institutions; their exit is mobile. They administer the extraction but do not own the permanent claim.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, administrators, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, administrators, beneficiary).

% Require tenure-line faculty ratios as a quality proxy. Their standards legitimize the tenure structure without bearing its costs. They observe from outside the resource flow.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, accreditation_bodies, observer,
    institutional, generational, analytical, national).

% Provide public funding but have no direct say in tenure policy. Would object to rigid labor costs if they saw the internal allocation. Their exit is arbitrage: they can redirect funds to non-tenured sectors (community colleges, vocational) but rarely do.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, legislative_funders, excluded,
    powerful, biographical, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes long-term research agendas and institutional memory by insulating a subset of faculty from short-term political or market pressure — a genuine coordination problem in knowledge production.
% TRANSFER_FUNCTION: Moves salary, benefits, governance authority, and curricular control from the institutional budget (funded by tuition and state appropriations) to tenured faculty as a permanent claim. Contingent faculty absorb the teaching load at fraction of the compensation; students pay the tuition premium.
% ABSENT_VOICES: Contingent faculty unions (where they exist) are often excluded from tenure-policy decisions; student governments have advisory votes only; state legislatures fund the system but do not govern its internal labor rules.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, institutions would immediately reallocate faculty lines to match enrollment and research priorities, contingent contracts would become the universal norm, and tuition pressure would shift — but the coordination function (long-term research insulation) would also disappear, likely reducing high-risk inquiry.
% FOUNDING_PROBLEM: Early 20th-century academic dismissals for political views and controversial research created a credibility crisis for universities as truth-seeking institutions. Tenure was built to protect scholars from external interference.
% FOUNDING_PROBLEM_CORROBORATION: AAUP's 1940 Statement of Principles attests the founding problem; contemporary AAUP reports argue it remains live. Critics (including contingent-faculty coalitions and state policy centers) attest the problem is substantially solved by existing speech protections and that tenure now primarily serves resource capture. No neutral arbiter exists.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__institutional_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__institutional_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the permanent claim on resources is decoupled from marginal productivity — tenured faculty receive full salary and governance weight regardless of teaching load, grant activity, or curricular need. Suppression (0.72) is high because the constraint actively prevents reallocation: tenure-review committees, faculty senates, and AAUP censure mechanisms block contraction of tenure lines even as enrollments shift. Theater_ratio (0.45) reflects that the academic-freedom justification is performed more intensely than the freedom itself is exercised — most tenured faculty pursue low-risk research. Accessibility_collapse (0.58) is moderate: alternatives (contract systems, multi-year reviews) exist but are institutionally suppressed. Resistance (0.55) is moderate: contingent organizing and legislative pressure exist but have not reversed the trend.
 *
 * PERSPECTIVAL GAP:
 *   From the tenured faculty seat, the constraint appears as rope — a coordination mechanism they built and maintain for truth-seeking. From the contingent faculty seat, it appears as snare — a permanent extraction they cannot escape. From the student seat, it appears as tangled_rope — they get credential value (coordination) but pay inflated cost (extraction). The engine computes this divergence from the structural data; this reading's claim (tangled_rope) reflects the aggregate structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are structural beneficiaries (d ~ 0.15): they collect the permanent claim, set the rules, and face constrained but not trapped exit. Contingent faculty are targets (d ~ 0.85): they bear the flexibility costs, have constrained exit, and lack governance voice. Students are targets (d ~ 0.8): they pay the tuition premium with no governance role and constrained exit. Administrators sit near symmetric (d ~ 0.5): they manage the extraction but also bear political risk. Accreditation bodies are observers (d ~ 0.0). Legislative funders are excluded beneficiaries of the broader system but payers of this constraint's rigidity (d ~ 0.6).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (political protection for scholars) is contested as live vs. solved. If dead, the constraint is a piton — a degraded coordination mechanism persisting by inertia. If live, it remains tangled_rope with a genuine coordination function. The mandatrophy question is whether the coordination function justifies the extraction floor. This reading treats the coordination as real but subordinate to extraction; the demographic_reproduction_reading would argue the coordination is a cover for gatekeeping; the academic_freedom_reading treats coordination as primary. The engine's per-seat computation will reveal whether any seat experiences this as pure coordination (rope) or pure extraction (snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the tenure contract a single constraint with observer-dependent classification, or three structurally distinct constraints (one per reading) that share a label?',
    'Apply the ε-invariance test: if measuring extraction via tenured-faculty resource capture yields high ε but measuring via academic-freedom protection yields low ε, the label ''tenure'' covers multiple constraints. Decompose into separate stories per reading and link via network.affects_constraints.',
    'If one constraint, the engine must reconcile contradictory classifications from different seats. If three constraints, each gets its own ε, stakeholders, and classification — the contested label is disambiguated into structural precision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the tenure kernel decomposes into multiple ε-invariant constraints per reading.').

omega_variable(
    coordination_vs_extraction_boundary,
    'What fraction of tenure''s resource allocation serves genuine long-term research insulation vs. rent capture by incumbent faculty?',
    'Compare tenure-line resource consumption (salary, space, governance time) against measurable high-risk research output. If the ratio diverges from comparable non-tenured research institutes (e.g., Max Planck, HHMI), the coordination function is subsidiary.',
    'If coordination is <30% of resource flow, the constraint is snare with coordination cover. If >60%, it is genuine tangled_rope. The boundary determines whether the academic_freedom_reading describes the same structure or a different one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the coordination function is substantial or performative.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of contingent-faculty alternatives structural (budget rules, accreditation standards) or internalized (contingent faculty accepting precarity as ''paying dues'')?',
    'Track contingent-faculty organizing outcomes: if structural barriers (labor law, accreditation) block unionization and contract conversion, suppression is structural. If organizing succeeds but contingent faculty still self-select into the tournament, internalization plays a role.',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint persists even if formal barriers fall. This affects whether the constraint is snare (coercion-dependent) or piton (inertial).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in the contingent-faculty labor market.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1970, tenure_contract__institutional_extraction_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(tenu_tr_t1985, tenure_contract__institutional_extraction_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__institutional_extraction_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(tenu_tr_t2010, tenure_contract__institutional_extraction_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(tenu_tr_t2020, tenure_contract__institutional_extraction_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(tenu_tr_t2025, tenure_contract__institutional_extraction_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1970, tenure_contract__institutional_extraction_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement(tenu_be_t1985, tenure_contract__institutional_extraction_reading, base_extractiveness, 1985, 0.51).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__institutional_extraction_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(tenu_be_t2010, tenure_contract__institutional_extraction_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(tenu_be_t2020, tenure_contract__institutional_extraction_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(tenu_be_t2025, tenure_contract__institutional_extraction_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1970, tenure_contract__institutional_extraction_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(tenu_su_t1985, tenure_contract__institutional_extraction_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__institutional_extraction_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(tenu_su_t2010, tenure_contract__institutional_extraction_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(tenu_su_t2020, tenure_contract__institutional_extraction_reading, suppression_requirement, 2020, 0.71).
narrative_ontology:measurement(tenu_su_t2025, tenure_contract__institutional_extraction_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__institutional_extraction_reading, 0.1).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% The tenure_contract kernel decomposes into three readings with divergent ε: academic_freedom_reading (low ε, mountain/rope), demographic_reproduction_reading (moderate ε, snare/tangled_rope), institutional_extraction_reading (high ε, tangled_rope). This reading claims the extraction function dominates; the others claim coordination or gatekeeping dominates. All three share the same institutional apparatus but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
