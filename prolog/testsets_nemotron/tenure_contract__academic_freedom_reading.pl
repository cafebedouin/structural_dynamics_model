% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract__academic_freedom_reading, []).

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
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Tenure as Academic Freedom Guarantee
 *   domain: higher_education_governance/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the academic_freedom_reading of the
 *   tenure_contract kernel. It models tenure as a coordination mechanism that
 *   decouples researcher survival from institutional or political
 *   displeasure, enabling high-risk inquiry. The reading claims low
 *   extractiveness for faculty (coordination benefit), high effective
 *   suppression difficulty for external political actors, and neutral benefit
 *   for students via research quality. The sibling readings
 *   (institutional_extraction_reading, demographic_reproduction_reading) are
 *   separate constraint stories with their own ε and structural claims; they
 *   are not folded into this one.
 *
 * KEY AGENTS:
 *   - tenured_faculty: Primary beneficiary (institutional/mobile) — receives employment security enabling independent inquiry
 *   - students: Neutral beneficiary (organized/constrained) — benefits indirectly from research quality and curricular stability
 *   - external_political_actors: Target of suppression difficulty (powerful/trapped) — finds suppression of tenured inquiry structurally costly
 *   - contingent_faculty: Excluded (moderate/constrained) — bears precarity that tenure system structurally depends on; not a party to this reading's coordination claim
 *   - university_administration: Agenda setter (institutional/arbitrage) — administers tenure process, balances external pressures against faculty autonomy
 *   - analytical_observer: Observer (analytical/analytical) — evaluates structural claims across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.18).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.12).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Tenure as Academic Freedom Guarantee").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education_governance/labor_economics/institutional_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, 'f85299e0-6d8a-4ce1-a1ec-a6228541a0ff').
narrative_ontology:cs_kernel_codification('f85299e0-6d8a-4ce1-a1ec-a6228541a0ff', formalized).
narrative_ontology:cs_authority_grounding('f85299e0-6d8a-4ce1-a1ec-a6228541a0ff', practice).
narrative_ontology:cs_interpretation_layer_present('f85299e0-6d8a-4ce1-a1ec-a6228541a0ff').
narrative_ontology:cs_reading_relation('f85299e0-6d8a-4ce1-a1ec-a6228541a0ff', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f85299e0-6d8a-4ce1-a1ec-a6228541a0ff', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('f85299e0-6d8a-4ce1-a1ec-a6228541a0ff', foundational, inquiry_independence_requires_employment_security).
narrative_ontology:cs_axiom_status(inquiry_independence_requires_employment_security, holdable).
narrative_ontology:cs_axiom_grounding('f85299e0-6d8a-4ce1-a1ec-a6228541a0ff', inquiry_independence_requires_employment_security, instrumental).
narrative_ontology:cs_axiom('f85299e0-6d8a-4ce1-a1ec-a6228541a0ff', foundational, peer_review_is_epistemic_not_demographic).
narrative_ontology:cs_axiom_status(peer_review_is_epistemic_not_demographic, holdable).
narrative_ontology:cs_axiom_grounding('f85299e0-6d8a-4ce1-a1ec-a6228541a0ff', peer_review_is_epistemic_not_demographic, deontological).
narrative_ontology:cs_reference_frame('f85299e0-6d8a-4ce1-a1ec-a6228541a0ff', aaip_1940_statement_principles).
narrative_ontology:cs_drift_state('f85299e0-6d8a-4ce1-a1ec-a6228541a0ff', contemporary_contingent_majority_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f85299e0-6d8a-4ce1-a1ec-a6228541a0ff', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, external_political_actors).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, truth_seeking_requires_institutional_independence).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, high_risk_inquiry_requires_employment_security).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent appointments with strong due-process protections against dismissal. This employment security enables pursuit of long-horizon, high-risk, or politically sensitive research without fear of termination for unpopular conclusions. They administer the tenure evaluation process for junior colleagues, shaping the pipeline. Their exit options include moving to other institutions, research institutes, or industry — the credential is portable.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    institutional, biographical, mobile, national).

% Receive instruction and mentorship from faculty whose research is insulated from short-term political or administrative pressure. Benefit from curricular stability and exposure to frontier inquiry. Their exit is constrained by enrollment commitments, transfer costs, and institutional prestige signaling. They do not participate in tenure governance and bear no direct cost of the constraint.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, students, beneficiary,
    organized, immediate, constrained, national).

% Legislators, donors, interest groups, or administrators seeking to suppress or redirect research findings that threaten their interests. Tenure makes this structurally difficult: they cannot directly fire tenured faculty, must work through lengthy and public processes (legislative hearings, budget pressure, accreditation threats), and face reputational costs for visible interference. Their 'payment' is the frustration of their suppression capacity; they are trapped in the sense that the constraint is a structural feature of the university they cannot easily dismantle.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, external_political_actors, payer,
    powerful, biographical, trapped, national).

% Hold fixed-term, renewable appointments without tenure-track pathways. Perform a majority of undergraduate teaching. Their precarity is the demographic_reproduction_reading's and institutional_extraction_reading's referent; this reading excludes them from its beneficiary structure. They would object to being structurally necessary for the tenure system's cost structure while denied its protections. Exit is constrained by the adjunct labor market's oversupply and lack of portable benefits.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, contingent_faculty, excluded,
    moderate, immediate, constrained, national).

% Administers tenure policies, evaluates candidates, manages external political pressure, and balances budgetary rigidity from tenured lines against instructional needs. They are the institutional seat that could reform or abolish tenure but face massive political and cultural costs for doing so. Their exit options are high — they move between institutions, systems, and sectors — making them arbitrage-grade relative to this constraint.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% Evaluates the structural claims of all three readings against longitudinal data on research output, demographic flows, cost structures, and political interference patterns. Does not collect rents or bear costs from the tenure system. Provides the cross-reading comparison that the kernel framework requires.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__academic_freedom_reading, diffuse).
narrative_ontology:fixing_cost_class(tenure_contract__academic_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of insulating truth-seeking from power: individual researchers cannot credibly commit to pursuing unpopular truths if their survival depends on institutional or political approval; tenure creates a structural commitment device that makes this credible at the institutional level.
% TRANSFER_FUNCTION: Moves employment security and inquiry insulation from the institution's administrative discretion to a rule-governed, peer-reviewed status. The institution gives up unilateral termination power; faculty gain career-long protection. No direct monetary transfer — the transfer is of decision rights over researcher survival.
% ABSENT_VOICES: Contingent faculty (excluded stakeholder) are the primary absent voice: they perform the teaching labor that subsidizes tenured research time but are denied the insulation tenure provides. Early-career researchers on the tenure track are partially present (as candidates) but their precarity during the probationary period is the mechanism that selects for conformity — they would object if the selection pressure were framed as extraction rather than merit evaluation. Taxpayers and legislators who fund public universities are partially present as external_political_actors but their interest in institutional accountability is framed as 'political interference' in this reading.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, universities would lose their primary structural insulation for high-risk inquiry. Faculty would shift toward safer, fundable, politically palatable research. Contingent faculty would not automatically gain security — the institution would likely replace tenure lines with more contingent appointments. Political actors would gain direct leverage over research agendas through hiring/funding control. The research ecosystem would reorganize toward short-horizon, low-risk, externally aligned work.
% FOUNDING_PROBLEM: Early 20th-century academic freedom controversies (e.g., Edward Ross at Stanford, 1900; AAUP 1915 Declaration) established that researchers investigating corporate power, labor conditions, or unpopular social theories were fired at trustee/donor behest. The founding problem: how to make inquiry independent of the wealth and orthodoxy that fund the university.
% FOUNDING_PROBLEM_CORROBORATION: AAUP historical records, legislative hearing transcripts from the 1900-1940 period, and court cases (e.g., Sweezy v. New Hampshire 1957, Keyishian v. Board of Regents 1967) document the founding problem from outside the beneficiary set. Contemporary corroboration: ongoing legislative attacks on tenure (Florida 2022, Texas 2023, Wisconsin 2015) and donor-driven research agenda setting demonstrate the founding problem persists. The institutional_extraction_reading and demographic_reproduction_reading contest whether the current arrangement still solves it or has been captured.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__academic_freedom_reading_tests).
:- end_tests(tenure_contract__academic_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.18) because the reading's referent is the coordination function — tenure as infrastructure for truth-seeking, not the rent or demographic effects the sibling readings identify. The metrics describe the constraint AS THIS READING SEES IT: a rope with genuine coordination, modest theater (some performative 'shared governance' rituals), low suppression (the constraint doesn't coerce; it insulates), moderate accessibility collapse (alternatives like research institutes exist but are scarce), and moderate resistance (contingent faculty resistance and political attacks on tenure). The slight upward drift in extractiveness and theater over the interval reflects the reading's own acknowledgment that the system has accumulated administrative overhead and performative compliance rituals.
 *
 * PERSPECTIVAL GAP:
 *   From the tenured faculty seat, tenure is a rope (coordination with negligible extraction). From the contingent faculty seat (excluded here), the same institution computes as snare or tangled_rope. From external political actors, it computes as a mountain (they cannot move it, only route around). The engine computes this divergence from the structural data; this reading authors only its own structural claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are structural beneficiaries (d ~ 0.15): the constraint subsidizes their independence. Students are near-neutral beneficiaries (d ~ 0.45): they gain from research quality but bear no direct cost. External political actors are targets of the constraint's insulation (d ~ 0.85 for them): the constraint makes their suppression costly. Contingent faculty are excluded from this reading's beneficiary set — their precarity is the demographic_reproduction_reading's referent, not this one's. Administration sits near symmetric (d ~ 0.5): it both maintains the constraint and bears its rigidities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (insulating inquiry from orthodoxy enforcement) remains live — political interference in research has not disappeared. The reading's mandated function has not atrophied; the coordination problem persists. However, the rising theater_ratio and the existence of sibling readings that claim the structure now serves extraction/reproduction indicate a contested mandatrophy status: the arrangement may be solving its original problem while simultaneously acquiring extractive/reproductive functions the academic_freedom_reading does not claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is tenure a genuine coordination mechanism for truth-seeking, or a cover for extraction/reproduction as the sibling readings claim?',
    'Longitudinal comparison of research output variance, demographic flows, and cost structures across tenure vs. contingent-track systems; adjudication of which reading''s predicted pattern matches observed outcomes.',
    'If academic_freedom_reading is correct, tenure''s ε for faculty should be low and its coordination function empirically detectable; if institutional_extraction_reading or demographic_reproduction_reading are correct, the same institution would show high ε for contingent faculty and demographic stasis despite formal academic freedom guarantees.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether this reading''s coordination claim holds structurally or is a legitimating narrative for extractive/reproductive functions.').

omega_variable(
    external_actor_suppression_mechanism,
    'What specific mechanisms make political suppression ''difficult'' under tenure, and do they operate equally for all political vectors?',
    'Case-study tracking of political interference attempts (legislative, donor, administrative) against tenured vs. non-tenured faculty across jurisdictions and issue domains.',
    'If suppression difficulty is asymmetrical (e.g., protects only certain viewpoints), the reading''s universal coordination claim fractures; the constraint would function as selective insulation rather than general truth-seeking infrastructure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_actor_suppression_mechanism, empirical, 'Whether tenure''s political insulation is structurally symmetric or selectively effective.').

omega_variable(
    student_beneficiary_causality,
    'Do students actually benefit from tenure-enabled research quality, or is this a projected beneficiary claim with weak causal linkage?',
    'Natural experiments comparing student outcomes (learning, placement, research exposure) across institutions with varying tenure densities and contingent faculty ratios, controlling for selectivity and resources.',
    'If student benefit is negligible or negative, the reading''s coordination function narrows to faculty-only; the claimed rope structure loses its broadest beneficiary class.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(student_beneficiary_causality, empirical, 'Whether the student beneficiary claim has empirical support or is a legitimating extension.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_academic_freedom_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(tenure_academic_freedom_tr_t10, tenure_contract__academic_freedom_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(tenure_academic_freedom_tr_t20, tenure_contract__academic_freedom_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(tenure_academic_freedom_tr_t30, tenure_contract__academic_freedom_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(tenure_academic_freedom_tr_t40, tenure_contract__academic_freedom_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(tenure_academic_freedom_tr_t50, tenure_contract__academic_freedom_reading, theater_ratio, 50, 0.22).

% Extraction over time
narrative_ontology:measurement(tenure_academic_freedom_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(tenure_academic_freedom_be_t10, tenure_contract__academic_freedom_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(tenure_academic_freedom_be_t20, tenure_contract__academic_freedom_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(tenure_academic_freedom_be_t30, tenure_contract__academic_freedom_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement(tenure_academic_freedom_be_t40, tenure_contract__academic_freedom_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement(tenure_academic_freedom_be_t50, tenure_contract__academic_freedom_reading, base_extractiveness, 50, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(tenure_academic_freedom_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(tenure_academic_freedom_su_t10, tenure_contract__academic_freedom_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement(tenure_academic_freedom_su_t20, tenure_contract__academic_freedom_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement(tenure_academic_freedom_su_t30, tenure_contract__academic_freedom_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement(tenure_academic_freedom_su_t40, tenure_contract__academic_freedom_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement(tenure_academic_freedom_su_t50, tenure_contract__academic_freedom_reading, suppression_requirement, 50, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__academic_freedom_reading, 0.08).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% This is the academic_freedom_reading of the tenure_contract kernel. The kernel decomposes into three structurally distinct constraints with different ε and beneficiary/victim structures. This reading claims the coordination function (truth-seeking insulation) is the kernel's primary structure; the sibling readings claim extraction and demographic reproduction are primary. They are linked via affects_constraints because the academic_freedom_reading's legitimacy is often cited to defend the institutional structure that the other readings identify as extractive/reproductive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__academic_freedom_reading, institutional, 0.35).
constraint_indexing:directionality_override(tenure_contract__academic_freedom_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
