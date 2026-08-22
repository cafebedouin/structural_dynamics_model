% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   domain: higher_education/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint describes tenure peer review as a mechanism for
 *   demographic gatekeeping within higher education. While ostensibly
 *   designed to ensure academic quality and freedom, this reading argues that
 *   the system, particularly through subjective criteria like 'fit' and
 *   'collegiality,' primarily serves to reproduce the existing demographic
 *   composition of faculty, benefiting dominant groups and systematically
 *   excluding underrepresented scholars. The claimed type is 'snare' because
 *   the coordination story (quality assurance) is seen as cover for a highly
 *   extractive and suppressive system that traps victims within a precarious
 *   academic labor market.
 *
 * KEY AGENTS:
 *   - demographically_dominant_faculty: Primary beneficiary (institutional/arbitrage) — benefits from preferential evaluation and network effects.
 *   - underrepresented_faculty_candidates: Primary target/victim (powerless/identity_locked) — bears the costs of exclusion and systemic bias.
 *   - university_administrators: Agenda setter (institutional/constrained) — manages the system, benefits from stability, avoids challenging entrenched power.
 *   - contingent_faculty: Secondary target/victim (powerless/trapped) — bears the costs of institutional rigidity and limited tenure-line openings.
 *   - academic_freedom_advocates: Excluded voice (organized/analytical) — would object to the system's deviation from its stated purpose.
 *   - diversity_equity_inclusion_officers: Observer (moderate/constrained) — documents disparities but struggles against systemic inertia.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.85).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.78).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, snare).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, 'a21e5644-cff2-4ade-b6b8-b6893cc7fcab').
narrative_ontology:cs_kernel_codification('a21e5644-cff2-4ade-b6b8-b6893cc7fcab', formalized).
narrative_ontology:cs_authority_grounding('a21e5644-cff2-4ade-b6b8-b6893cc7fcab', lineage).
narrative_ontology:cs_interpretation_layer_present('a21e5644-cff2-4ade-b6b8-b6893cc7fcab').
narrative_ontology:cs_reading_relation('a21e5644-cff2-4ade-b6b8-b6893cc7fcab', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('a21e5644-cff2-4ade-b6b8-b6893cc7fcab', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('a21e5644-cff2-4ade-b6b8-b6893cc7fcab', foundational, demographic_homogeneity_as_quality).
narrative_ontology:cs_axiom_status(demographic_homogeneity_as_quality, holdable).
narrative_ontology:cs_axiom_grounding('a21e5644-cff2-4ade-b6b8-b6893cc7fcab', demographic_homogeneity_as_quality, conventional).
narrative_ontology:cs_axiom('a21e5644-cff2-4ade-b6b8-b6893cc7fcab', secondary, subjective_fit_as_merit).
narrative_ontology:cs_axiom_status(subjective_fit_as_merit, holdable).
narrative_ontology:cs_axiom_grounding('a21e5644-cff2-4ade-b6b8-b6893cc7fcab', subjective_fit_as_merit, conventional).
narrative_ontology:cs_reference_frame('a21e5644-cff2-4ade-b6b8-b6893cc7fcab', homogenous_collegial_academy).
narrative_ontology:cs_drift_state('a21e5644-cff2-4ade-b6b8-b6893cc7fcab', contemporary_diversity_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a21e5644-cff2-4ade-b6b8-b6893cc7fcab', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, university_administrators).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, contingent_faculty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a system that preferentially evaluates candidates who fit existing demographic and cultural norms, reinforcing their own positions and networks. They often serve on tenure review committees, applying subjective criteria like 'fit' and 'collegiality'.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, demographically_dominant_faculty, beneficiary,
    institutional, generational, arbitrage, national).

% Bear the costs of a system that systematically excludes them based on non-meritocratic criteria. They face significant barriers to entry and advancement, often leaving academia due to lack of opportunity despite strong research records. Their identity is often deeply tied to their academic aspirations.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_faculty_candidates, payer,
    powerless, biographical, identity_locked, national).

% Administer the tenure system, often publicly defending it as meritocratic while privately managing diversity initiatives that struggle against its gatekeeping effects. They benefit from a stable, if demographically homogenous, faculty body and avoid the costs of challenging entrenched power structures.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, university_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Are kept in precarious, non-tenure-track positions, bearing the costs of institutional rigidity and the demographic gatekeeping that limits tenure-line openings. They often perform the bulk of teaching with little job security or institutional support.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, contingent_faculty, payer,
    powerless, immediate, trapped, local).

% Would argue that tenure's original purpose was to protect intellectual inquiry, not to reproduce existing demographics. They are often marginalized in discussions about tenure reform when the focus shifts to 'fit' rather than scholarly independence.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, academic_freedom_advocates, excluded,
    organized, generational, analytical, national).

% Observe and document the demographic disparities perpetuated by the tenure system. Their efforts to promote diversity are often undermined by the subjective and opaque nature of tenure review, leading to frustration and limited impact.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, diversity_equity_inclusion_officers, observer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the selection and retention of faculty, ostensibly to ensure high-quality research and teaching within academic institutions.
% TRANSFER_FUNCTION: Transfers career security, institutional power, and control over academic discourse to a demographically dominant group, at the expense of underrepresented scholars who are excluded from these benefits.
% ABSENT_VOICES: Underrepresented scholars who have left academia due to systemic exclusion, and those who would advocate for a tenure system based purely on meritocratic research and teaching metrics, rather than subjective 'fit' criteria.
% DISAPPEARANCE_RATIONALE: If tenure peer review as demographic gatekeeping vanished, the composition of faculty would likely diversify more rapidly, leading to shifts in research priorities, pedagogical approaches, and institutional culture. The power dynamics within universities would fundamentally alter.
% FOUNDING_PROBLEM: The original problem tenure was designed to solve was protecting academic freedom from political interference and ensuring faculty retention for long-term scholarly projects.
% FOUNDING_PROBLEM_CORROBORATION: University administrators and some senior faculty attest the problem of academic freedom protection is still live. However, many junior faculty, contingent faculty, and external critics (e.g., labor economists, critical race theorists) argue that the system's current operation has largely decoupled from this founding problem, instead serving as a mechanism for demographic and ideological reproduction, as evidenced by persistent disparities in faculty composition and the subjective nature of 'fit' criteria in review processes.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.85) reflects the immense cost to excluded scholars in terms of lost careers, intellectual contributions, and economic opportunity. Suppression (0.78) is high due to the opaque nature of review processes, the lack of alternative career paths within academia, and the 'identity_locked' nature of many aspiring academics. The theater ratio (0.65) indicates that a significant portion of the 'peer review' process is performative, maintaining the illusion of meritocracy while serving gatekeeping functions. The rising extractiveness and theater ratio over time reflect the increasing precarity of academic labor and the growing divergence between tenure's stated purpose and its actual demographic outcomes.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of demographically dominant faculty, the system appears as a 'rope' or even a 'mountain'—a natural and fair process for maintaining academic standards. For underrepresented faculty candidates and contingent faculty, it operates as a 'snare,' actively extracting their labor and denying them access to secure positions under the guise of merit. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Demographically dominant faculty are clear beneficiaries, as the system reinforces their status and networks (low d). Underrepresented faculty candidates and contingent faculty are targets, facing systemic exclusion and precarity (high d). University administrators, while managing the system, also benefit from its stability and the avoidance of conflict (low d). Academic freedom advocates are excluded, and DEI officers are observers, neither directly benefiting nor paying in the same structural way.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling demographic gatekeeping as genuine coordination. By identifying the high extractiveness and suppression, and the significant theater ratio, it highlights how a system originally intended to protect academic freedom has atrophied into a mechanism for demographic reproduction. The 'snare' classification directly challenges the 'rope' or 'mountain' framing often used to defend the status quo, indicating that the mandate has been captured by a subset of beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subjectivity_of_fit_criteria,
    'To what extent are ''fit'' and ''collegiality'' criteria in tenure review genuinely predictive of academic success and collaboration, versus proxies for demographic or cultural similarity?',
    'Longitudinal studies correlating subjective review scores with objective post-tenure productivity and interdisciplinary collaboration, controlling for demographic factors. Blinded review processes for ''fit'' criteria.',
    'If found to be proxies, the extractiveness and suppression metrics would be further validated, strengthening the ''snare'' classification. If genuinely predictive, the coordination function would be more salient, potentially shifting the classification towards ''tangled_rope''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subjectivity_of_fit_criteria, empirical, 'Ambiguity in subjective tenure criteria.').

omega_variable(
    academic_freedom_vs_demographic_closure,
    'Does the current operation of tenure primarily protect academic freedom (as claimed by the ''academic_freedom_reading'') or enable demographic closure (as argued by this reading)?',
    'Analysis of tenure denial cases: are denials primarily due to controversial research, or to non-conformity with departmental culture/demographics? Comparative analysis of faculty diversity in systems with and without tenure.',
    'If academic freedom is demonstrably protected, the ''snare'' classification is weakened. If demographic closure is the dominant outcome, the ''snare'' classification is reinforced, and the ''academic_freedom_reading'' is revealed as a cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academic_freedom_vs_demographic_closure, conceptual, 'Contested primary function of tenure.').

omega_variable(
    internalized_suppression_in_academia,
    'Is the suppression experienced by underrepresented faculty primarily structural (e.g., lack of tenure lines, biased review processes) or internalized (e.g., self-censorship, imposter syndrome due to systemic messaging)?',
    'Qualitative studies and post-exit surveys of underrepresented scholars. Analysis of retention rates after structural reforms are implemented.',
    'If internalized suppression is significant, the effective suppression is higher than structural measures suggest, as individuals carry the burden even if external barriers are reduced. This would reinforce the ''snare'' classification by highlighting the deep-seated nature of the extractive mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_in_academia, empirical, 'Structural vs. internalized suppression mechanism for underrepresented faculty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1970, tenure_contract__demographic_reproduction_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(tenu_tr_t1985, tenure_contract__demographic_reproduction_reading, theater_ratio, 1985, 0.45).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__demographic_reproduction_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(tenu_tr_t2010, tenure_contract__demographic_reproduction_reading, theater_ratio, 2010, 0.6).
narrative_ontology:measurement(tenu_tr_t2024, tenure_contract__demographic_reproduction_reading, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1970, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(tenu_be_t1985, tenure_contract__demographic_reproduction_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2000, 0.78).
narrative_ontology:measurement(tenu_be_t2010, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(tenu_be_t2024, tenure_contract__demographic_reproduction_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1970, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(tenu_su_t1985, tenure_contract__demographic_reproduction_reading, suppression_requirement, 1985, 0.65).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(tenu_su_t2010, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(tenu_su_t2024, tenure_contract__demographic_reproduction_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, academic_labor_market_precarity).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, university_research_priorities).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'tenure_contract' kernel, focusing on its role in demographic reproduction. It is linked to the 'academic_freedom_reading' and 'institutional_extraction_reading' as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
