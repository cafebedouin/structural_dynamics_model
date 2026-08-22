% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: tenure_contract__institutional_extraction_reading
 *   human_readable: Tenure as Permanent Rent Extraction by Early Winners
 *   domain: higher_education_governance/labor_economics
 *
 * SUMMARY:
 *   This story instantiates the institutional-extraction reading of the
 *   tenure-contract kernel: tenure as a permanent, non-expiring claim on
 *   institutional resources won by an early cohort under conditions of
 *   academic labor supply and demand that no longer hold, whose maintenance
 *   requires shifting flexibility costs onto contingent faculty, junior
 *   faculty competing for an artificially constrained supply of tenure-track
 *   lines, and tuition-paying students. This is a distinct constraint from
 *   the academic-freedom reading (which reads the same kernel as protecting
 *   inquiry from institutional retaliation) and the demographic-reproduction
 *   reading (which reads tenure review as a gatekeeping mechanism reproducing
 *   group composition). All three share the tenure_contract kernel but author
 *   different beneficiary/victim structures, different epsilon values, and
 *   different classifications — per the epsilon-invariance principle, they
 *   are separate files linked by network edges, not one story averaged across
 *   readings.
 *
 * KEY AGENTS:
 *   - tenured_faculty_incumbents: Primary beneficiary (institutional/arbitrage) — holds the permanent claim
 *   - contingent_adjunct_faculty: Primary victim (powerless/trapped) — absorbs flexibility cost
 *   - untenured_junior_faculty: Secondary victim (moderate/constrained) — competes for artificially scarce openings
 *   - tuition_paying_students: Diffuse victim (powerless/constrained) — funds legacy compensation via tuition
 *   - university_administration: Institutional agenda-setter, partially self-victimized by the enforcement it maintains
 *   - prospective_phd_labor_market_entrants: Excluded voice — oversupplied by the same constraint, no governance seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.72).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.58).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Permanent Rent Extraction by Early Winners").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education_governance/labor_economics").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '3f4dcf7d-b337-489a-83ca-1e3aded1a518').
narrative_ontology:cs_kernel_codification('3f4dcf7d-b337-489a-83ca-1e3aded1a518', formalized).
narrative_ontology:cs_authority_grounding('3f4dcf7d-b337-489a-83ca-1e3aded1a518', practice).
narrative_ontology:cs_interpretation_layer_present('3f4dcf7d-b337-489a-83ca-1e3aded1a518').
narrative_ontology:cs_reading_relation('3f4dcf7d-b337-489a-83ca-1e3aded1a518', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f4dcf7d-b337-489a-83ca-1e3aded1a518', tenure_contract__demographic_reproduction_reading, influences).
narrative_ontology:cs_axiom('3f4dcf7d-b337-489a-83ca-1e3aded1a518', foundational, resource_claims_require_periodic_revalidation).
narrative_ontology:cs_axiom_status(resource_claims_require_periodic_revalidation, holdable).
narrative_ontology:cs_axiom_grounding('3f4dcf7d-b337-489a-83ca-1e3aded1a518', resource_claims_require_periodic_revalidation, instrumental).
narrative_ontology:cs_axiom('3f4dcf7d-b337-489a-83ca-1e3aded1a518', foundational, permanent_tenure_is_labor_rigidity_not_merit_protection).
narrative_ontology:cs_axiom_status(permanent_tenure_is_labor_rigidity_not_merit_protection, holdable).
narrative_ontology:cs_axiom_grounding('3f4dcf7d-b337-489a-83ca-1e3aded1a518', permanent_tenure_is_labor_rigidity_not_merit_protection, empirically_contingent).
narrative_ontology:cs_reference_frame('3f4dcf7d-b337-489a-83ca-1e3aded1a518', probationary_merit_screening_model).
narrative_ontology:cs_drift_state('3f4dcf7d-b337-489a-83ca-1e3aded1a518', contemporary_adjunctification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3f4dcf7d-b337-489a-83ca-1e3aded1a518', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty_incumbents).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_adjunct_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, tuition_paying_students).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, untenured_junior_faculty).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, university_administration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a permanent claim on a departmental line and budget share won under conditions that may no longer reflect current enrollment, research demand, or institutional priorities. Sit on the committees that govern hiring, promotion, program cuts, and curriculum, effectively setting the terms under which their own positions are reviewed (which is: not substantively reviewed). Can decline service, redirect teaching load downward, or coast on research output with near-zero risk of termination for cause short of severe misconduct or full program elimination. Their exit options are strong even without leaving: they can shift burdens onto colleagues internally.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty_incumbents, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, tenured_faculty_incumbents, agenda_setter).

% Teach a large and rising share of course sections, often the same courses semester after semester, without job security, benefits, or a path to permanence, because the tenured lines that would otherwise be created or converted are locked by incumbents. Reappointment is discretionary and short-term (semester or year contracts), so the actual flexibility the institution needs is absorbed here rather than being distributed. Cannot bargain from security; leaving the institution usually means leaving academia, since the adjunct labor market is oversupplied and cross-institution mobility does not improve terms.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_adjunct_faculty, payer,
    powerless, immediate, trapped, national).

% Compete for a shrinking number of tenure-track lines that are not created because incumbent tenured lines are never surrendered, and are then evaluated for tenure by the very incumbents whose positions are insulated from equivalent scrutiny. Bear years of probationary risk and relocation costs to win a prize whose supply is artificially constrained by the permanence of the prior cohort's claims. Exit means leaving the tenure track entirely, usually into contingent status or outside academia.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, untenured_junior_faculty, payer,
    moderate, biographical, constrained, national).

% Pay tuition that increasingly funds legacy salary and benefit obligations to tenured incumbents in low-enrollment or declining fields, while instructional delivery in high-enrollment courses is increasingly staffed by underpaid contingent faculty as a cost-offset. Cannot renegotiate what fraction of tuition funds instruction versus legacy compensation; switching institutions does not escape the structural pattern, only its specific incumbents.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tuition_paying_students, payer,
    powerless, immediate, constrained, national).

% Sets budgets and negotiates the enforcement mechanism (tenure policy, grievance procedures, accreditation compliance) that makes tenured claims durable, but is itself constrained by that same durability when trying to reallocate resources toward growing programs. Can expand contingent hiring to buy flexibility at the margin, and can freeze or eliminate tenure lines only through costly program discontinuance processes, which makes administration a partial victim of the rigidity it also helps enforce through shared governance structures.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administration, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, university_administration, payer).

% Are trained in numbers that vastly exceed the tenure-track openings that the permanence of prior claims allows to exist, but have no seat in tenure or hiring policy decisions and are not consulted when departments decide to preserve legacy lines rather than convert them to new hires. Their oversupply is a direct downstream consequence of a labor market whose exit doors are structurally narrowed by the constraint under study.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, prospective_phd_labor_market_entrants, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty_incumbents).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Tenure review does perform some genuine screening at the point of award — a probationary period during which research and teaching quality are assessed before a permanent commitment is made.
% TRANSFER_FUNCTION: Moves budget share, job security, and institutional voice from contingent and junior faculty (and indirectly from tuition-paying students) to a fixed cohort of incumbents who cleared the tenure bar under earlier, often more favorable, conditions and whose claim on resources does not expire or get periodically re-tested against current institutional need.
% ABSENT_VOICES: Contingent faculty unions have some voice but limited bargaining leverage; prospective PhD entrants and the broader pool of qualified non-tenured scholars who would fill converted tenure-track lines have no representation in the governance bodies (tenured faculty senates, promotion committees) that decide whether existing lines are preserved, converted, or eliminated.
% DISAPPEARANCE_RATIONALE: If permanent tenure claims vanished overnight and all faculty positions became periodically re-competed, departments would immediately reallocate lines toward growing fields, contingent faculty would compete on more even footing for open positions, and tuition allocation toward legacy compensation would shift toward current instructional need — a substantial reorganization of resource flows within a single budget cycle.
% FOUNDING_PROBLEM: Tenure was built to protect scholars from dismissal for unpopular research findings, political views, or administrative displeasure, ensuring that inquiry into controversial or institutionally inconvenient questions could proceed without threat to livelihood.
% FOUNDING_PROBLEM_CORROBORATION: Tenured faculty associations (e.g., AAUP) attest the protective function remains live and cite ongoing cases of politically motivated dismissal attempts at public universities. Independent labor economists and several university system audits attest that the protective function, while occasionally invoked, is empirically dwarfed by tenure's operation as a permanent resource claim disconnected from any ongoing performance or need test — this reading is authored from that outside-corroborated position, distinct from the AAUP's self-interested defense.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.72, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.72 at interval end) and rising because the value captured by incumbent tenure holders (salary continuity, resource claims, governance control) grows relative to institutional output as fields shift and enrollment patterns change, while the claim itself does not adjust. Suppression is moderate (0.58): there is no violent coercion, but shared-governance structures give incumbents structural veto power over the reallocation that would otherwise occur, and grievance/legal protections around tenure make removal procedurally very costly regardless of institutional need. Theater ratio rises to 0.42 as post-tenure review processes proliferate without meaningfully triggering non-renewal or reallocation — a performative compliance layer around a claim that in practice remains untouched. Accessibility collapse is moderate (0.5): alternative arrangements (renewable contracts, periodic re-evaluation) exist and are used elsewhere, so the extraction is not total-collapse but persistent given the entrenched governance veto. Resistance is moderate (0.55): contingent faculty organizing, some administrative pressure toward program discontinuance, and periodic reform proposals meet consistent incumbent-faculty-senate resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the tenured-incumbent seat, the arrangement is a rope: a hard-won protection that coordinates around legitimate academic-freedom and stability concerns. From the contingent-faculty and junior-faculty seats, the identical structure is experienced as tangled-rope-to-snare: a genuine original coordination function (probationary screening, freedom from retaliation) now overlaid with asymmetric extraction that specifically loads flexibility costs onto the powerless seats. The engine should compute this divergence from the declared power/exit/beneficiary structure rather than from any single narrative frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured incumbents are declared beneficiaries with institutional power and arbitrage-grade exit (they can shift burdens internally without needing to leave), driving their derived directionality toward the low end (net beneficiary). Contingent faculty are declared victims with powerless power and trapped exit — the academic labor market for adjuncts offers no exit that escapes the pattern — driving directionality toward the high end (full target). Junior faculty sit between: moderate power, constrained exit, and real but risk-laden mobility, producing a directionality closer to target than beneficiary but with more agency than contingent faculty. Students are powerless with constrained exit and no bargaining leverage over the tuition-to-compensation pipeline, again driving toward the target end despite not being a labor-market party. University administration carries a genuine secondary-payer role: it enforces the constraint but also bears its rigidity costs, which the structural data captures via its dual role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protection from retaliatory dismissal) is still cited by incumbents as live, but the founding_problem_status is authored as contested rather than dead, because retaliatory-dismissal cases do still occur and the protective function is not wholly fictional — this prevents mislabeling the arrangement as pure extraction with no remaining coordination content, which would misclassify it as a snare rather than tangled_rope. The classification is tangled_rope rather than snare precisely because a genuine coordination function (probationary screening, some retained protection value) persists alongside the asymmetric extraction; collapsing to snare would erase the coordination half the schema requires the classifier to test for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protective_function_residual_value,
    'How much of tenure''s original academic-freedom-protective function remains operative today, versus being invoked rhetorically to defend a resource claim that no longer requires that justification?',
    'Comparative study of dismissal-for-cause and non-renewal cases at tenure-track vs. long-term-renewable-contract institutions, controlling for political/administrative retaliation attempts, to isolate how much unique protective value tenure adds beyond what strong contract law or academic-freedom statutes would provide alone.',
    'If residual protective value is low, this reading''s tangled_rope classification would trend toward snare, since the coordination-function gate would weaken substantially. If residual value is high, some of the measured extraction should be reattributed as the price of a genuinely needed protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_function_residual_value, empirical, 'Whether academic freedom protection is a live residual function or a legitimating cover story for the resource claim.').

omega_variable(
    kernel_reading_decomposition_boundary,
    'Is the institutional-extraction dynamic separable from the academic-freedom and demographic-gatekeeping dynamics, or do all three operate simultaneously through the same tenure decision such that no single reading captures the full causal structure?',
    'Process-tracing of individual tenure and post-tenure-review cases to identify whether extraction, freedom-protection, and demographic-gatekeeping effects are independently detectable or are inseparably fused in the same institutional act.',
    'If inseparable, the ε-invariance decomposition into three sibling stories is a modeling convenience rather than a discovery of three distinct mechanisms, and the network edges between the readings should be read as strong mutual reinforcement rather than three independent structures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_decomposition_boundary, conceptual, 'Whether the three kernel readings describe independently operating mechanisms or one fused mechanism viewed from three angles.').

omega_variable(
    resource_reallocation_counterfactual,
    'Would eliminating permanent tenure actually result in resources being reallocated toward growing fields and improved contingent-faculty terms, or would administrations simply capture the freed flexibility as cost savings without reinvesting in instructional quality?',
    'Comparative analysis of institutions that have already moved toward renewable multi-year contracts in place of tenure, tracking whether freed budget was reinvested in instructional lines/compensation or redirected to non-instructional spending.',
    'If administrations capture the freed flexibility rather than reallocating it to contingent faculty or students, the extraction is partially administrative rather than purely a tenured-incumbent phenomenon, which would argue for adding administration as a co-beneficiary rather than treating it solely as agenda_setter/payer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_reallocation_counterfactual, empirical, 'Whether removing the constraint would benefit contingent faculty and students or simply shift capture to administration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tenu_tr_t8, tenure_contract__institutional_extraction_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(tenu_tr_t16, tenure_contract__institutional_extraction_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(tenu_tr_t24, tenure_contract__institutional_extraction_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(tenu_tr_t32, tenure_contract__institutional_extraction_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__institutional_extraction_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tenu_be_t8, tenure_contract__institutional_extraction_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(tenu_be_t16, tenure_contract__institutional_extraction_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(tenu_be_t24, tenure_contract__institutional_extraction_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(tenu_be_t32, tenure_contract__institutional_extraction_reading, base_extractiveness, 32, 0.69).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__institutional_extraction_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(tenu_su_t8, tenure_contract__institutional_extraction_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(tenu_su_t16, tenure_contract__institutional_extraction_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(tenu_su_t24, tenure_contract__institutional_extraction_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(tenu_su_t32, tenure_contract__institutional_extraction_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__institutional_extraction_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tenure_contract__institutional_extraction_reading, 0.12).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories sharing the tenure_contract kernel. academic_freedom_reading authors low ε for tenured faculty as genuine coordination beneficiaries protecting inquiry; demographic_reproduction_reading authors high ε concentrated on excluded demographic groups via 'fit' criteria in peer review; institutional_extraction_reading (this story) authors high ε for tenured incumbents as resource-claim beneficiaries and high ε for contingent faculty, junior faculty, and students as cost-bearers of employment rigidity. Each story keeps its own stable ε per the ε-invariance principle; they are linked here rather than merged because the underlying observable (what is being extracted, and from whom) differs structurally across readings even though all three describe effects of the same tenure decision.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
