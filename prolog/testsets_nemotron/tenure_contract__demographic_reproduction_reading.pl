% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__demographic_reproduction_reading, []).

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
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   Tenure peer review is formally a meritocratic quality filter for
 *   permanent academic appointments. This reading documents its operation as
 *   a demographic reproduction mechanism: 'fit' and 'collegiality' criteria,
 *   unmoored from measurable research productivity, function as cultural
 *   affinity gates that privilege the dominant demographic group (white,
 *   male, elite-educated, conventionally socialized). The constraint
 *   coordinates the allocation of scarce tenure lines — a genuine
 *   coordination function — but extracts positional and reputational rents
 *   for the dominant group while structurally excluding underrepresented
 *   candidates. The academic freedom doctrine is vindicated as cover: the
 *   constraint's persistence is justified by the academic freedom claim, but
 *   its operation protects demographic closure rather than inquiry. The
 *   engine will compute per-seat classifications from the structural data;
 *   this reading's claimed type (tangled_rope) asserts genuine coordination
 *   coexisting with asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.68).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.62).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, '704a6dc7-16a4-470b-a463-3f8a3f72b645').
narrative_ontology:cs_kernel_codification('704a6dc7-16a4-470b-a463-3f8a3f72b645', formalized).
narrative_ontology:cs_authority_grounding('704a6dc7-16a4-470b-a463-3f8a3f72b645', lineage).
narrative_ontology:cs_interpretation_layer_present('704a6dc7-16a4-470b-a463-3f8a3f72b645').
narrative_ontology:cs_reading_relation('704a6dc7-16a4-470b-a463-3f8a3f72b645', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('704a6dc7-16a4-470b-a463-3f8a3f72b645', tenure_contract__institutional_extraction_reading, influences).
narrative_ontology:cs_axiom('704a6dc7-16a4-470b-a463-3f8a3f72b645', foundational, peer_review_gate_reproduces_demographic_hierarchy).
narrative_ontology:cs_axiom_status(peer_review_gate_reproduces_demographic_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('704a6dc7-16a4-470b-a463-3f8a3f72b645', peer_review_gate_reproduces_demographic_hierarchy, empirically_contingent).
narrative_ontology:cs_axiom('704a6dc7-16a4-470b-a463-3f8a3f72b645', foundational, fit_collegiality_criteria_are_cultural_affinity_filters).
narrative_ontology:cs_axiom_status(fit_collegiality_criteria_are_cultural_affinity_filters, holdable).
narrative_ontology:cs_axiom_grounding('704a6dc7-16a4-470b-a463-3f8a3f72b645', fit_collegiality_criteria_are_cultural_affinity_filters, empirically_contingent).
narrative_ontology:cs_reference_frame('704a6dc7-16a4-470b-a463-3f8a3f72b645', meritocratic_peer_review_ideal).
narrative_ontology:cs_drift_state('704a6dc7-16a4-470b-a463-3f8a3f72b645', contemporary_dei_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('704a6dc7-16a4-470b-a463-3f8a3f72b645', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, tenured_dominant_demographic).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, department_chairs).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, senior_administrators).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, early_career_scholars_marginalized).
narrative_ontology:constraint_vindicates(tenure_contract__demographic_reproduction_reading, academic_freedom_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold secure positions and set evaluation standards through peer review committees. Benefit from 'fit' and 'collegiality' criteria that implicitly privilege their own demographic profile, communication style, and institutional socialization. Can move to peer institutions with minimal friction; their exit is a credentialed lateral transfer, not a career rupture.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, tenured_dominant_demographic, beneficiary,
    institutional, biographical, arbitrage, national).

% Administer the tenure review process, appoint committees, and shape 'collegiality' definitions. Benefit from maintaining departmental cohesion as they define it — which often means replicating existing demographic and intellectual culture. Could reform criteria but face collective-action pressure from tenured faculty and risk accusations of lowering standards.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, department_chairs, agenda_setter,
    institutional, biographical, constrained, local).

% Deans and provosts who approve tenure cases and set institutional diversity goals. Benefit from the prestige and stability of a tenured faculty that matches the institution's historical profile. Publicly endorse diversity initiatives but structurally depend on the same peer review system that reproduces the dominant demographic. Their exit is movement between elite administrative posts.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, senior_administrators, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, senior_administrators, beneficiary).

% PhD holders and junior faculty from underrepresented groups who enter the tenure pipeline. Face evaluation criteria ('fit', 'collegiality', 'impact') that are tacitly calibrated to dominant-group norms. Their research is assessed through frameworks that marginalize their methodologies, communities, and citation networks. Exit means leaving academia entirely — their professional identity is fused to the career they are being filtered out of.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates, payer,
    powerless, biographical, identity_locked, national).

% Adjuncts, lecturers, and term-contract faculty who perform the bulk of teaching. The tenure system's rigidity creates the structural demand for their precarious labor. They are excluded from the peer review gate entirely — not evaluated by it, but governed by its consequences. Exit is geographic or sectoral; within academia, they are structurally trapped.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, contingent_faculty, payer,
    powerless, immediate, trapped, local).

% Junior faculty (including those from dominant demographics) whose work challenges disciplinary orthodoxies or uses non-standard methodologies. The 'fit' criterion operates as ideological conformity enforcement. They have some mobility (other institutions, alt-ac) but the constraint's logic replicates across the sector. Exit is costly and uncertain.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, early_career_scholars_marginalized, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__demographic_reproduction_reading, early_career_scholars_marginalized, excluded).

% Institutional staff tasked with diversity, equity, and inclusion initiatives. They document disparities and propose reforms but have no authority over tenure criteria or peer review committees. Their recommendations are adopted as 'aspirational' while the gatekeeping structure remains intact. They would object to the constraint's operation but are structurally excluded from the decision room.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, diversity_officers, excluded,
    organized, biographical, constrained, local).

% Professional societies that set field-wide norms for evaluation, publish journals, and confer awards. Their guidelines increasingly acknowledge bias in 'fit' and 'collegiality' but they lack enforcement power over institutional tenure policies. They observe the constraint's operation across the sector and produce the evidence base for reform.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, disciplinary_associations, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Tenure peer review coordinates the allocation of permanent academic positions by certifying scholarly merit through expert evaluation, ostensibly ensuring that long-term institutional investments go to researchers who will sustain the enterprise.
% TRANSFER_FUNCTION: Moves secure, high-status, resource-rich positions (tenure lines, research funding, governance power) from the pool of candidates to the demographically dominant group via evaluation criteria that appear neutral but function as cultural affinity filters. The transfer is not monetary but positional and reputational.
% ABSENT_VOICES: Underrepresented faculty candidates are evaluated by committees that rarely include scholars from their communities; contingent faculty are governed by a system they never vote on; diversity officers are consulted but not empowered. The absent voices are those whose careers the constraint filters out before they reach the room where tenure is decided.
% DISAPPEARANCE_RATIONALE: If the current peer review gate vanished overnight, the demographic composition of tenured faculties would not instantly change — but the structural mechanism reproducing it would be gone. Hiring and promotion would reorganize around whatever criteria replace it (market metrics, student evaluations, administrative fiat, or genuinely reformed peer review). The constraint's disappearance would trigger a sector-wide reorganization of academic labor.
% FOUNDING_PROBLEM: The original tenure system was built to protect scholars from political and donor pressure, ensuring that controversial or long-horizon inquiry could survive institutional displeasure. The demographic gatekeeping function was not the founding problem — it emerged as the composition of the professoriate stabilized around a narrow demographic profile and 'fit' became a proxy for 'one of us'.
% FOUNDING_PROBLEM_CORROBORATION: Historians of higher education (e.g., Metzger, American Academic Freedom) document the founding problem as political protection, not demographic curation. Current tenured faculty and administrators often assert the founding problem is still 'academic freedom' — but the demographic reproduction reading is corroborated by the divergence between stated criteria (productivity, impact) and operational criteria (fit, collegiality), documented in audit studies (e.g., Rivera 2015, Posselt 2016) and the persistent demographic stasis of tenured ranks despite decades of diverse PhD cohorts. No source outside the beneficiary set attests that demographic homogeneity was ever the founding problem.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__demographic_reproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tenure_contract__demographic_reproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the gate's operation transfers career-defining positions along demographic lines while presenting as meritocratic. Suppression (0.62) is substantial because alternatives (alternative evaluation frameworks, cluster hires, reformed criteria) are actively resisted by beneficiaries and agenda-setters. Theater ratio (0.44) is elevated because diversity initiatives and bias training perform the appearance of reform while the gate's core logic persists. Accessibility collapse (0.58) reflects that candidates who internalize the criteria's tacit norms can sometimes pass, but the alternative path (changing the criteria) is institutionally blocked. Resistance (0.51) is moderate: organized challenges exist (union drives, student protests, federal pressure) but have not displaced the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (tenured dominant demographic), the constraint appears as a mountain or rope: 'this is how we ensure excellence.' From the payer seats (underrepresented candidates, contingent faculty), it computes as snare or tangled_rope: 'this is how you keep people like me out.' The agenda-setter seats (chairs, deans) experience it as a tangled_rope they administer but cannot easily change — genuine coordination function, real extraction they benefit from indirectly. The engine computes this divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The tenured dominant demographic and senior administrators are structural beneficiaries (d near 0.0-0.2): they collect the positional rents and control the gate. Department chairs are agenda-setters with d ~0.3: they administer the system and benefit from its stability but face some pressure to reform. Underrepresented candidates are full targets (d ~0.9): identity-locked into a career the constraint filters them out of. Contingent faculty are trapped payers (d ~0.85): they bear the system's downstream costs (precarious labor market) with no voice in it. Early-career marginalized scholars are constrained payers (d ~0.7): they have some exit but the constraint replicates sector-wide. Diversity officers are excluded (d undefined — not in the extraction flow). Disciplinary associations are analytical observers (d ~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting inquiry from political pressure) is contested: beneficiaries assert it is live; structural evidence suggests the constraint has drifted into demographic reproduction. The mandate has not been resolved — the academic freedom cover remains operative — but the constraint's actual operation serves a different function. This is a classic mandatrophy case: the arrangement persists because its cover story is institutionally sacred, while its extraction logic has become the dominant function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fit_criteria_operationalization,
    'To what extent are ''fit'' and ''collegiality'' criteria explicitly defined vs. tacitly understood as cultural affinity markers? Can they be operationalized without demographic bias?',
    'Content analysis of tenure guidelines across institutions; audit studies with matched candidate profiles varying only demographic signals; natural experiments from institutions that have reformed or eliminated ''fit'' criteria.',
    'If ''fit'' is inherently a cultural affinity marker, the constraint''s coordination function is inseparable from its extraction function — it is tangled_rope at best, snare at worst. If ''fit'' can be rigorously defined (e.g., ''willingness to mentor,'' ''departmental citizenship''), the coordination function might be separable from demographic extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fit_criteria_operationalization, empirical, 'Whether the gate''s criteria are structurally biased or reformably neutral.').

omega_variable(
    academic_freedom_cover_persistence,
    'Does the academic freedom doctrine genuinely constrain the constraint''s operation, or has it become a purely performative cover for demographic reproduction?',
    'Track cases where academic freedom is invoked to defend scholars from underrepresented groups vs. dominant group scholars; measure correlation between academic freedom rhetoric and actual protection of controversial inquiry across demographic lines.',
    'If the doctrine operates symmetrically, the constraint retains a genuine coordination function (rope-like). If it operates only to protect the dominant demographic''s gatekeeping, the constraint is a snare with academic freedom as theatrical cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(academic_freedom_cover_persistence, conceptual, 'Whether the vindicated proposition (academic_freedom_doctrine) is operative or captured.').

omega_variable(
    kernel_reading_boundary,
    'Is the demographic reproduction reading a distinct constraint from the academic freedom reading, or are they two observable facets of the same constraint evaluated differently?',
    'Apply the epsilon-invariance test: would measuring the constraint by ''protection of controversial inquiry'' yield a different epsilon than measuring by ''demographic pass rate''? If yes, they are distinct constraints (per DP-001).',
    'If distinct, the kernel decomposition is valid and each reading gets its own constraint story. If the same constraint, the decomposition is an authoring artifact and the engine would compute one classification from one epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the kernel decomposition respects epsilon-invariance (DP-001).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1970, tenure_contract__demographic_reproduction_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(tenu_tr_t1985, tenure_contract__demographic_reproduction_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__demographic_reproduction_reading, theater_ratio, 2000, 0.31).
narrative_ontology:measurement(tenu_tr_t2010, tenure_contract__demographic_reproduction_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(tenu_tr_t2020, tenure_contract__demographic_reproduction_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(tenu_tr_t2025, tenure_contract__demographic_reproduction_reading, theater_ratio, 2025, 0.44).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1970, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(tenu_be_t1985, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement(tenu_be_t2010, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement(tenu_be_t2020, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(tenu_be_t2025, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1970, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(tenu_su_t1985, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(tenu_su_t2010, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2010, 0.56).
narrative_ontology:measurement(tenu_su_t2020, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(tenu_su_t2025, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__demographic_reproduction_reading, 0.12).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, contingent_labor_market_rigidity).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, academic_hiring_audit_pipeline).

% DUAL FORMULATION NOTE:
% Tenure contract kernel decomposes into three readings with distinct epsilon profiles: academic_freedom_reading (low epsilon, mountain-like for dominant group); demographic_reproduction_reading (high epsilon for underrepresented groups, tangled_rope); institutional_extraction_reading (high epsilon for contingent faculty, snare-like). The demographic reproduction reading creates the demographic closure that the institutional extraction reading exploits — the tenure gate produces the precarious labor pool. All three stories link to each other via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__demographic_reproduction_reading, institutional, 0.15).
constraint_indexing:directionality_override(tenure_contract__demographic_reproduction_reading, powerless, 0.88).
constraint_indexing:directionality_override(tenure_contract__demographic_reproduction_reading, moderate, 0.68).
constraint_indexing:directionality_override(tenure_contract__demographic_reproduction_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
