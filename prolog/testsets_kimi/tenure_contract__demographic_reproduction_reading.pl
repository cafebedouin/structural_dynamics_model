% ============================================================================
% CONSTRAINT STORY: tenure_contract__demographic_reproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: tenure_contract__demographic_reproduction_reading
 *   human_readable: Tenure Peer Review as Demographic Gatekeeping
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   Tenure peer review is a contested kernel in U.S. higher education. The
 *   academic freedom reading frames it as protecting inquiry; the
 *   institutional extraction reading frames it as intergenerational rent
 *   capture; this readingâdemographic_reproductionâframes it as a
 *   gatekeeping mechanism that reproduces dominant-group composition through
 *   subjective fit and collegiality criteria. Under this reading, the
 *   coordination story (merit-based quality control) is cover for a snare
 *   that extracts career survival from underrepresented scholars and
 *   transfers it to historically advantaged faculty. The constraint is
 *   actively enforced by tenure committees, and alternatives (standardized
 *   rubrics, open hiring) are suppressed through appeals to academic autonomy
 *   and the immeasurability of collegiality. The authored metrics describe a
 *   highly extractive, theatrical operation; the claimed type is snare.
 *
 * KEY AGENTS:
 *   - tenure_committees: agenda_setter â administer the peer review gate and enforce fit criteria
 *   - dominant_group_faculty: beneficiary â receives preferential evaluation through homophily and cultural match
 *   - underrepresented_scholars: payer â bears exclusion and precarity costs despite comparable productivity
 *   - contingent_instructors: excluded â absent from deliberations but affected by scarcity of tenure lines
 *   - critical_higher_ed_researchers: observer â documents demographic reproduction and decoupling from merit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__demographic_reproduction_reading, 0.82).
domain_priors:suppression_score(tenure_contract__demographic_reproduction_reading, 0.78).
domain_priors:theater_ratio(tenure_contract__demographic_reproduction_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tenure_contract__demographic_reproduction_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__demographic_reproduction_reading, snare).
narrative_ontology:human_readable(tenure_contract__demographic_reproduction_reading, "Tenure Peer Review as Demographic Gatekeeping").
narrative_ontology:topic_domain(tenure_contract__demographic_reproduction_reading, "higher_education_governance/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__demographic_reproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__demographic_reproduction_reading, '7c4314d0-4aae-4013-888c-3bff8be0cd33').
narrative_ontology:cs_kernel_codification('7c4314d0-4aae-4013-888c-3bff8be0cd33', formalized).
narrative_ontology:cs_authority_grounding('7c4314d0-4aae-4013-888c-3bff8be0cd33', lineage).
narrative_ontology:cs_interpretation_layer_present('7c4314d0-4aae-4013-888c-3bff8be0cd33').
narrative_ontology:cs_reading_relation('7c4314d0-4aae-4013-888c-3bff8be0cd33', tenure_contract__academic_freedom_reading, influences).
narrative_ontology:cs_reading_relation('7c4314d0-4aae-4013-888c-3bff8be0cd33', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('7c4314d0-4aae-4013-888c-3bff8be0cd33', foundational, tenure_reproduces_dominant_group_composition).
narrative_ontology:cs_axiom_status(tenure_reproduces_dominant_group_composition, holdable).
narrative_ontology:cs_axiom_grounding('7c4314d0-4aae-4013-888c-3bff8be0cd33', tenure_reproduces_dominant_group_composition, empirically_contingent).
narrative_ontology:cs_axiom('7c4314d0-4aae-4013-888c-3bff8be0cd33', secondary, collegiality_functions_as_demographic_proxy).
narrative_ontology:cs_axiom_status(collegiality_functions_as_demographic_proxy, holdable).
narrative_ontology:cs_axiom_grounding('7c4314d0-4aae-4013-888c-3bff8be0cd33', collegiality_functions_as_demographic_proxy, empirically_contingent).
narrative_ontology:cs_reference_frame('7c4314d0-4aae-4013-888c-3bff8be0cd33', meritocratic_tenure_ideal).
narrative_ontology:cs_drift_state('7c4314d0-4aae-4013-888c-3bff8be0cd33', contemporary_peer_review_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7c4314d0-4aae-4013-888c-3bff8be0cd33', '').
narrative_ontology:cs_kernel_id(tenure_contract__demographic_reproduction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__demographic_reproduction_reading, dominant_group_faculty).
narrative_ontology:constraint_victim(tenure_contract__demographic_reproduction_reading, underrepresented_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Evaluate tenure candidates using research, teaching, and collegiality criteria. They control the gate to permanent employment and enforce the standard of fit through collective deliberation, vote, and appeal to peer review autonomy.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, tenure_committees, agenda_setter,
    organized, biographical, constrained, national).

% Occupy the tenured ranks whose demographic composition is reproduced by peer review. They benefit from homophily in fit assessments, face lower scrutiny on collegiality, and their career security is maintained by the closure of the tenure track.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, dominant_group_faculty, beneficiary,
    powerful, biographical, mobile, national).

% Seek tenure through peer review but face elevated standards on fit and collegiality that correlate with dominant-group culture. They bear the cost of exclusion or prolonged precarity when committees conclude they are not a good fit despite comparable research productivity.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, underrepresented_scholars, payer,
    powerless, biographical, trapped, national).

% Perform teaching labor without access to the tenure track or peer review deliberations. They would object to the scarcity of tenure lines and the demographic filtering of permanent positions, but are excluded from faculty governance and the peer review room.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, contingent_instructors, excluded,
    powerless, immediate, trapped, national).

% Analyze tenure demographics and peer review outcomes using quantitative and qualitative methods. They document the decoupling of fit from research productivity and testify to the gatekeeping function, but do not participate in the constraint's operation.
narrative_ontology:constraint_stakeholder(tenure_contract__demographic_reproduction_reading, critical_higher_ed_researchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__demographic_reproduction_reading, diffuse).
narrative_ontology:fixing_cost_class(tenure_contract__demographic_reproduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the allocation of permanent faculty positions and the reproduction of departmental culture through peer evaluation of research, teaching, and collegial fit.
% TRANSFER_FUNCTION: Moves job security, institutional resources, and professional status from underrepresented scholars to dominant-group faculty by filtering tenure candidates through subjective criteria that correlate with demographic identity rather than research productivity.
% ABSENT_VOICES: Contingent faculty who never enter the tenure track, denied underrepresented scholars who have left academia, and graduate students from marginalized backgrounds are structurally excluded from peer review deliberations; their absence naturalizes fit as a neutral professional judgment.
% DISAPPEARANCE_RATIONALE: If tenure peer review vanished, departments would lose the primary mechanism for reproducing dominant-group composition; hiring and retention patterns would shift toward more diverse outcomes, and the academic labor market would reorganize around alternative evaluation protocols or employment structures.
% FOUNDING_PROBLEM: How to protect long-term scholarly inquiry from political interference, economic pressure, and short-term institutional demands by granting job security to researchers who demonstrate sustained productivity.
% FOUNDING_PROBLEM_CORROBORATION: Critical race and feminist institutional theorists, organizational sociologists studying homophily, and labor economists analyzing academic demographics attest that the founding problem has been superseded by demographic gatekeeping. The benefiting parties assert the problem remains live, but this is self-interested. Independent empirical studies from outside the beneficiary set document the decoupling of tenure outcomes from research productivity.
narrative_ontology:disappearance_verdict(tenure_contract__demographic_reproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__demographic_reproduction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__demographic_reproduction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__demographic_reproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__demographic_reproduction_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.82) is high because the constraint systematically denies job security to productive scholars on the basis of identity-correlated fit criteria, transferring the full value of tenure to dominant-group incumbents. Suppression (0.78) is high because the persistence of collegiality and fit criteria depends on excluding alternative evaluation protocols and silencing dissenting committee members through norms of departmental loyalty. Theater ratio (0.70) is high because elaborate review procedures (dossiers, external letters, committee deliberations) perform rigor while producing predictable demographic outcomes. Accessibility collapse (0.60) reflects that alternatives (alt-ac, standardized evaluation, open hiring) are structurally degraded or stigmatized. Resistance (0.50) captures growing diversity-equity pressures and litigation, still partially neutralized by institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (tenure committees) experiences the constraint as a necessary professional judgment about departmental culture and long-term fit; the payer seat (underrepresented scholars) experiences it as an opaque identity test that extracts their biographical investment in academia. The beneficiary seat (dominant-group faculty) may not perceive the extraction at all, experiencing only the coordination benefit of a comfortable, familiar department. The engine computes this divergence from the structural asymmetry in exit options (mobile versus trapped) and power.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenure committees derive structural power from their agenda-setting role but are not the primary beneficiaries of the demographic reproduction; their directionality is moderate. Dominant-group faculty are beneficiaries (low d, low effective extraction). Underrepresented scholars are full targets (high d, high effective extraction). Contingent instructors are excluded and trapped, bearing diffuse costs without even entering the evaluation stream.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as snare prevents mislabeling the constraint as a rope (pure coordination of merit) or a scaffold (transitional support). The founding problemâprotecting inquiry from political interferenceâis dead in this reading: the mechanism now protects demographic closure. The divergence between founding_problem_status=dead and disappearance_verdict=world_rearranges triggers the mandatrophy flag, identifying the constraint as a zombie institution sustained by inertia and beneficiary resistance rather than by its original justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_closure_vs_inquiry_protection,
    'Does tenure currently function to protect scholarly inquiry from external pressure, or has it been captured as a mechanism for demographic closure within the professoriate?',
    'Comparative demographic analysis of tenure rates controlling for research productivity, citation impact, and grant funding across identity categories; if productivity-adjusted tenure rates show significant demographic disparity, the inquiry-protection claim is falsified for the current institutional configuration.',
    'If captured for demographic closure, the constraint is a snare or tangled rope rather than a rope or scaffold, and the academic freedom kernel reading is contradicted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_closure_vs_inquiry_protection, empirical, 'Whether tenure protects inquiry or demographic closure.').

omega_variable(
    collegiality_criteria_constructedness,
    'Are fit and collegiality criteria in peer review structurally inseparable from merit evaluation, or are they constructed screens that can be removed without damaging research quality assessment?',
    'Natural experiment from departments that have eliminated collegiality criteria or standardized evaluation rubrics: if research quality and departmental function persist or improve, the criteria are separable constructed screens.',
    'If separable, the extraction component can be excised while preserving any genuine coordination function; if inseparable, the constraint is a more deeply tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collegiality_criteria_constructedness, conceptual, 'Whether collegiality is a constructed screen separable from merit.').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the suppression of underrepresented scholars in tenure peer review maintained primarily through structural barriers (exclusion from networks, biased letters) or through internalized self-censorship and identity fusion with the institution?',
    'Post-tenure trajectory studies and interview data from scholars who left the tenure track: if exit does not resolve the psychological constraints, suppression is partially internalized.',
    'If internalized, effective extraction exceeds the structural measure because the target carries the suppression after exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__demographic_reproduction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcdr_tr_t0, tenure_contract__demographic_reproduction_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(tcdr_tr_t10, tenure_contract__demographic_reproduction_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(tcdr_tr_t20, tenure_contract__demographic_reproduction_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(tcdr_tr_t30, tenure_contract__demographic_reproduction_reading, theater_ratio, 30, 0.61).
narrative_ontology:measurement(tcdr_tr_t40, tenure_contract__demographic_reproduction_reading, theater_ratio, 40, 0.66).
narrative_ontology:measurement(tcdr_tr_t50, tenure_contract__demographic_reproduction_reading, theater_ratio, 50, 0.7).

% Extraction over time
narrative_ontology:measurement(tcdr_be_t0, tenure_contract__demographic_reproduction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(tcdr_be_t10, tenure_contract__demographic_reproduction_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(tcdr_be_t20, tenure_contract__demographic_reproduction_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(tcdr_be_t30, tenure_contract__demographic_reproduction_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(tcdr_be_t40, tenure_contract__demographic_reproduction_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(tcdr_be_t50, tenure_contract__demographic_reproduction_reading, base_extractiveness, 50, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(tcdr_su_t0, tenure_contract__demographic_reproduction_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(tcdr_su_t10, tenure_contract__demographic_reproduction_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement(tcdr_su_t20, tenure_contract__demographic_reproduction_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(tcdr_su_t30, tenure_contract__demographic_reproduction_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(tcdr_su_t40, tenure_contract__demographic_reproduction_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(tcdr_su_t50, tenure_contract__demographic_reproduction_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__demographic_reproduction_reading, identity_coordination).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__demographic_reproduction_reading, institutional_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the demographic_reproduction_reading of the tenure_contract kernel. It isolates the claim that peer review reproduces dominant-group composition through subjective fit criteria. Sibling readings isolate the academic_freedom and institutional_extraction functions. The epsilon values differ because the empirical claims are structurally distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
