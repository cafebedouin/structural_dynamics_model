% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure as Permanent Rent Extraction (Institutional Extraction Reading)
 *   domain: higher_education_governance / labor_economics / institutional_theory
 *
 * SUMMARY:
 *   Tenure is a permanent employment guarantee awarded after a probationary
 *   period, ostensibly to protect academic freedom. This reading
 *   (institutional_extraction_reading) interprets tenure as a mechanism where
 *   early winners — those who secured tenure during periods of expansion —
 *   capture a permanent claim on institutional resources (salary lines,
 *   laboratory space, governance power) that cannot be reallocated as
 *   priorities shift. The rigidity loads flexibility costs onto contingent
 *   faculty (adjuncts, lecturers, postdocs) who now perform 70%+ of
 *   instructional labor at a fraction of the compensation, and onto students
 *   via tuition increases that fund the tenured salary structure while
 *   instructional investment per student declines. The constraint persists
 *   through active enforcement: peer review gatekeeping, accreditation
 *   standards requiring tenure-line ratios, shared governance structures that
 *   give tenured faculty veto power over structural reform.
 *
 * KEY AGENTS:
 *   - tenured_faculty: Primary beneficiary (institutional/powerful/arbitrage) — holds permanent resource claim, controls governance, faces near-zero exit cost
 *   - senior_administration: Secondary beneficiary (institutional/powerful/arbitrage) — uses tenure system to stabilize budgeting, legitimize hierarchy, deflect political pressure
 *   - contingent_faculty: Primary victim (organized/moderate/identity_locked) — bears instructional load, faces precarity, exit blocked by professional identity fusion
 *   - graduate_students: Secondary victim (moderate/identity_locked/trapped) — trained for tenure-track that largely doesn't exist, socialized into the system's logic
 *   - undergraduate_students: Diffuse victim (powerless/immediate/constrained) — pay rising tuition for declining instructional investment, no voice in governance
 *   - analytical_observer: Observer (analytical/civilizational/analytical) — sees full structure across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.78).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.72).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Permanent Rent Extraction (Institutional Extraction Reading)").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education_governance / labor_economics / institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '1570f9fb-c5da-4b56-af35-1b0c2562a48f').
narrative_ontology:cs_kernel_codification('1570f9fb-c5da-4b56-af35-1b0c2562a48f', formalized).
narrative_ontology:cs_authority_grounding('1570f9fb-c5da-4b56-af35-1b0c2562a48f', lineage).
narrative_ontology:cs_interpretation_layer_present('1570f9fb-c5da-4b56-af35-1b0c2562a48f').
narrative_ontology:cs_reading_relation('1570f9fb-c5da-4b56-af35-1b0c2562a48f', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('1570f9fb-c5da-4b56-af35-1b0c2562a48f', tenure_contract__demographic_reproduction_reading, influences).
narrative_ontology:cs_axiom('1570f9fb-c5da-4b56-af35-1b0c2562a48f', foundational, permanent_resource_claim_is_extractive).
narrative_ontology:cs_axiom_status(permanent_resource_claim_is_extractive, holdable).
narrative_ontology:cs_axiom_grounding('1570f9fb-c5da-4b56-af35-1b0c2562a48f', permanent_resource_claim_is_extractive, empirically_contingent).
narrative_ontology:cs_axiom('1570f9fb-c5da-4b56-af35-1b0c2562a48f', foundational, contingent_labor_subsidizes_tenure_structure).
narrative_ontology:cs_axiom_status(contingent_labor_subsidizes_tenure_structure, holdable).
narrative_ontology:cs_axiom_grounding('1570f9fb-c5da-4b56-af35-1b0c2562a48f', contingent_labor_subsidizes_tenure_structure, empirically_contingent).
narrative_ontology:cs_reference_frame('1570f9fb-c5da-4b56-af35-1b0c2562a48f', aaap_1940_principles_framework).
narrative_ontology:cs_drift_state('1570f9fb-c5da-4b56-af35-1b0c2562a48f', contingent_majority_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1570f9fb-c5da-4b56-af35-1b0c2562a48f', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, senior_administration).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, graduate_students).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, undergraduate_students).
narrative_ontology:constraint_vindicates(tenure_contract__institutional_extraction_reading, resource_allocation_stability_claim).
narrative_ontology:constraint_vindicates(tenure_contract__institutional_extraction_reading, institutional_memory_preservation_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent employment guarantees awarded after 6-7 year probationary period. Control departmental hiring, curriculum, and governance through shared governance structures. Receive salary, benefits, research support, and institutional prestige that cannot be reduced. Can move laterally to other institutions with tenure portability. The constraint subsidizes their position — they are net recipients of the locked-in resource allocation.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary,
    institutional, biographical, arbitrage, national).

% Administer the tenure system: set probationary standards, allocate tenure lines, negotiate with accreditation bodies. Benefit from budgetary predictability (fixed salary obligations) and a hierarchy that legitimates administrative authority. Use tenure protections to deflect political pressure ('faculty governance protects academic freedom'). Could reform the system but face veto from tenured faculty governance bodies.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, senior_administration, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, senior_administration, beneficiary).

% Perform 70%+ of undergraduate instruction on semester-to-semester contracts, no research time, no benefits, pay 40-60% of tenure-line per-course equivalent. Professional identity is fused to the tenure-track ideal — 'adjunct' is a failed academic identity, not a chosen career. Exit means abandoning 7-10 years of doctoral training and vocational self-concept. Unionization efforts growing but fragmented across institutions and states.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    organized, biographical, identity_locked, national).

% Train for 5-7 years in doctoral programs structured around the tenure-track model. Socialized into the system's norms: research productivity, grant capture, publication metrics. Face a job market where tenure-track positions are <20% of PhD output. Exit during or after training means identity rupture — 'leaving academia' is framed as failure, not pivot. Bear opportunity costs and debt while subsidizing the research output of tenured faculty.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, graduate_students, payer,
    moderate, biographical, identity_locked, national).

% Pay tuition that has risen 3-4x inflation since 1980 while instructional spending per student has declined. Taught increasingly by contingent faculty with no office hours, no curriculum input, no long-term relationship. No governance voice — student representation is advisory only. Exit means transferring (credit loss, delay) or dropping out (credential loss, debt without degree).
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, undergraduate_students, payer,
    powerless, immediate, constrained, national).

% Sees the full constraint structure across all three kernel readings. Observes that the institutional_extraction_reading captures the resource-allocation rigidity and cost-loading that the academic_freedom_reading treats as epiphenomenal and the demographic_reproduction_reading treats as secondary to identity gatekeeping. The analytical seat computes per-seat classifications from the structural data without committing to any reading's normative frame.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes long-term research commitments and preserves institutional memory by guaranteeing that faculty cannot be removed for unpopular findings or administrative convenience. Solves the hold-up problem in specialized human capital investment: researchers invest in field-specific skills that have no outside value; tenure protects that investment.
% TRANSFER_FUNCTION: Moves salary lines, laboratory space, governance authority, and instructional control from reallocation to permanent claim by tenured faculty. The flexibility cost (teaching load variance, enrollment shifts, field obsolescence) is transferred to contingent faculty (who absorb instructional volatility) and students (who pay tuition that funds the fixed cost structure). Resources that could fund new fields, expanded access, or instructional innovation are locked into the tenure salary structure.
% ABSENT_VOICES: Would-be reform administrators (presidents, provosts, deans who see the rigidity but cannot overcome shared governance vetoes); alternative credentialing providers (bootcamps, microcredential platforms, apprenticeship models) excluded by accreditation gatekeeping; prospective faculty from underrepresented groups who would enter under different incentive structures; taxpayers in public university systems who fund the fixed cost structure without representation.
% DISAPPEARANCE_RATIONALE: If tenure and its enforcement vanished overnight, universities would immediately face pressure to reallocate salary lines to high-demand fields, convert instructional labor to flexible contracts, and reduce tuition or redirect funds to student services. Contingent faculty would gain bargaining power. The academic labor market would reorganize around multi-year renewable contracts, project-based funding, and teaching-stream careers. The transition would be disruptive — research continuity would suffer in some fields — but the world would rearrange, not stay the same.
% FOUNDING_PROBLEM: Early 20th century: faculty dismissed for political views (anti-war, socialist, evolution teaching), religious nonconformity, or administrative disfavor. The AAUP 1915 Declaration and 1940 Statement of Principles established tenure as protection against external and internal threats to inquiry. The problem was genuine: without protection, researchers cannot pursue unpopular or long-horizon truth-seeking.
% FOUNDING_PROBLEM_CORROBORATION: AAUP and tenured faculty organizations attest the problem remains live (citing political attacks on CRT, gender studies, climate research). Contingent faculty unions (AAUP-CBC, SEIU), student debt organizers (Debt Collective), and independent analysts (Delta Cost Project, AAUP's own contingent faculty reports) attest the founding problem has substantially shifted: the primary threat to inquiry is now structural precarity that prevents contingent faculty from pursuing any research at all, and the tenure system itself generates the rigidity that blocks adaptation. Legislative hearings in multiple states (Florida, Texas, Wisconsin) show political actors using tenure as a target while defunding public higher education — the threat has mutated, not persisted unchanged.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness is high (0.78) because tenure lines represent locked-in resource claims decoupled from marginal productivity — the permanent salary obligation cannot be adjusted when enrollment shifts, research fields wax/wane, or instructional needs change. Suppression is high (0.72) because the system actively prevents reallocation: accreditation standards, shared governance vetoes, and the tenure-track pipeline itself suppress alternatives (phasing out tenure lines, converting to teaching-focused contracts, modular credentialing). Theater ratio is moderate (0.41) — the academic freedom justification is real but increasingly performative as tenure lines shrink and the protected class becomes a minority of instructional labor. Accessibility collapse (0.63) reflects that alternatives (multi-year contracts, teaching-stream tenure, project-based funding) are structurally visible but institutionally blocked. Resistance (0.58) is substantial — contingent faculty unions, student debt movements, legislative pressure — but fragmented across jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   From the tenured faculty seat (beneficiary, institutional power, arbitrage exit), tenure appears as necessary coordination: it stabilizes long-term inquiry, protects dissent, and preserves institutional memory. From the contingent faculty seat (victim, organized power, identity_locked exit), the same structure operates as enforced extraction: their labor subsidizes the tenured salary structure, their professional identity is fused to a pipeline that excludes them, and exit means abandoning their vocational self-concept. From the student seat (diffuse victim, powerless, constrained exit), the constraint is invisible infrastructure — tuition funds a cost structure they cannot see or challenge. The engine computes these seat divergences from the structural data; the claimed_type (tangled_rope) captures the hybrid coordination/extraction nature from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are structural beneficiaries: they collect permanent resource claims (salary, space, governance weight) and face near-zero exit cost (d ≈ 0.1). Senior administration benefits indirectly: tenure stabilizes budgeting and provides a legitimating hierarchy (d ≈ 0.2). Contingent faculty are targets: they bear the flexibility costs (teaching overload, no research time, no benefits) with identity_locked exit — leaving academia means abandoning professional identity formed through years of training (d ≈ 0.85). Graduate students are targets with trapped/identity_locked exit: the tenure-track ideal structures their entire socialization (d ≈ 0.9). Undergraduate students are diffuse targets: they fund the system via tuition with no governance voice and constrained exit (transfer costs, credential dependence) (d ≈ 0.6). Rival claimants (would-be reform administrators, alternative credentialing providers) are excluded.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stabilizing inquiry against political/economic pressure) is contested — tenured faculty attest it remains live; contingent faculty, students, and independent analysts attest it has shifted to rent extraction. The arrangement persists despite the founding problem's contested status because the beneficiary coalition (tenured faculty + administration) controls the veto points. This is mandatrophy: the coordination function (academic freedom protection) has atrophied relative to the extraction function (resource capture), but the constraint persists because the beneficiaries administer it and the victims are identity-locked or diffuse. The classification prevents mislabeling by requiring both coordination beneficiaries AND extraction victims to be named — a pure snare reading would miss the genuine academic freedom coordination; a pure rope reading would miss the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the tenure contract instantiate a genuine academic freedom protection, a demographic gatekeeping mechanism, or an institutional extraction arrangement — or all three simultaneously?',
    'Structural decomposition: if removing the permanent claim on resources preserves academic freedom outcomes, the extraction reading is validated; if academic freedom collapses, the academic_freedom_reading holds; if demographic composition remains unchanged, the demographic_reproduction_reading is weakened.',
    'If the extraction reading is structurally valid, the constraint classifies as tangled_rope with high ε for multiple victim groups; if academic_freedom_reading dominates, the constraint approaches rope with near-zero ε; if demographic_reproduction_reading dominates, it is a snare with identity-locked victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the tenure contract''s kernel admits multiple simultaneous readings or a single true function.').

omega_variable(
    extraction_vs_coordination_boundary,
    'How much of tenure''s employment rigidity is necessary coordination (stabilizing long-term research, preserving institutional memory) versus pure rent extraction (preventing reallocation, loading costs onto contingent labor)?',
    'Counterfactual analysis: jurisdictions with weaker tenure protections (e.g., UK post-1988, US community colleges, European fixed-term systems) compared on research output, instructional quality, and contingent labor share.',
    'If rigidity is mostly coordination, ε drops and the constraint shifts toward rope; if mostly extraction, ε remains high and tangled_rope/snare classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'The coordination-extraction boundary within the tenure constraint.').

omega_variable(
    suppression_mechanism_tenure,
    'Is the suppression experienced by contingent faculty structural (contractual precarity, market structure) or internalized (professional identity fusion with the tenure-track ideal, belief that precarity is a meritocratic filter)?',
    'Post-exit trajectory study: track contingent faculty who exit to non-academic roles — does suppression (precarity internalization, professional devaluation) persist after structural barriers are removed?',
    'If internalized, effective suppression is higher than structural measure suggests — targets carry the constraint with them after exit, amplifying χ for identity-locked agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_tenure, empirical, 'Structural vs. internalized suppression mechanism for contingent faculty under tenure regime.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1970, tenure_contract__institutional_extraction_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(tenu_tr_t1985, tenure_contract__institutional_extraction_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__institutional_extraction_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(tenu_tr_t2010, tenure_contract__institutional_extraction_reading, theater_ratio, 2010, 0.34).
narrative_ontology:measurement(tenu_tr_t2020, tenure_contract__institutional_extraction_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(tenu_tr_t2024, tenure_contract__institutional_extraction_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1970, tenure_contract__institutional_extraction_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(tenu_be_t1985, tenure_contract__institutional_extraction_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__institutional_extraction_reading, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement(tenu_be_t2010, tenure_contract__institutional_extraction_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(tenu_be_t2020, tenure_contract__institutional_extraction_reading, base_extractiveness, 2020, 0.74).
narrative_ontology:measurement(tenu_be_t2024, tenure_contract__institutional_extraction_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1970, tenure_contract__institutional_extraction_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(tenu_su_t1985, tenure_contract__institutional_extraction_reading, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__institutional_extraction_reading, suppression_requirement, 2000, 0.51).
narrative_ontology:measurement(tenu_su_t2010, tenure_contract__institutional_extraction_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(tenu_su_t2020, tenure_contract__institutional_extraction_reading, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement(tenu_su_t2024, tenure_contract__institutional_extraction_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tenure_contract__institutional_extraction_reading, 0.18).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, academic_labor_market_segmentation).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, university_cost_disease).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, credential_inflation).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% Tenure contract decomposes into three constraint stories (a constraint family) per ε-invariance: academic_freedom_reading (low ε, rope/mountain), demographic_reproduction_reading (moderate ε, snare), institutional_extraction_reading (high ε, tangled_rope). This is the extraction reading; the academic freedom reading coordinates inquiry protection with minimal extraction; the demographic reading extracts via identity gatekeeping. All three share the same institutional kernel but differ in ε, victims, and type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, institutional, 0.15).
constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, organized, 0.85).
constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, moderate, 0.9).
constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, powerless, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
