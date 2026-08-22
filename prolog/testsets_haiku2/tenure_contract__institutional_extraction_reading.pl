% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Tenure as Institutional Rent Extraction
 *   domain: institutional/economic/labor
 *
 * SUMMARY:
 *   This constraint story instantiates the institutional_extraction_reading
 *   of the tenure_contract kernel. Tenure, understood through this reading,
 *   functions as a permanent claim on institutional resources by tenured
 *   faculty cohorts, protected by peer review and academic culture, while
 *   employment flexibility costs are loaded onto contingent faculty and
 *   instructional quality is constrained by resource lock-in. The reading
 *   does NOT claim tenure has no legitimate function (the
 *   academic_freedom_reading attests its role in protecting inquiry); it DOES
 *   claim that the standing arrangement operates structurally to extract
 *   rents from powerless labor-market participants and cost-bearing students.
 *   This is a contested reading—sibling readings emphasize academic freedom
 *   protection and demographic gatekeeping respectively. The authored metrics
 *   describe high extractiveness and active suppression (maintaining dual
 *   labor tiers and preventing resource redeployment); the claimed type is
 *   tangled_rope because the constraint coordinates peer oversight while
 *   asymmetrically extracting from contingent faculty and students.
 *
 * KEY AGENTS:
 *   - tenured_faculty_cohort: primary beneficiaries (organized/arbitrage exit) — hold permanent resource claims and governance authority
 *   - contingent_faculty: primary victims (powerless/trapped exit) — bear employment flexibility and precarity costs
 *   - students: secondary victims (powerless/constrained exit) — absorb tuition costs and reduced instructional investment
 *   - department_administration: enforcer (moderate/constrained exit) — maintains dual labor system and tenure deference
 *   - university_administration: secondary beneficiary (powerful/mobile exit) — tolerates tenure because it enables preferred flexibility strategies
 *   - academic_freedom_advocates: excluded analytical seat — would contest this reading's framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.78).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.71).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Institutional Rent Extraction").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "institutional/economic/labor").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '8a017475-9c7b-4c8e-8f21-6f0a193449f5').
narrative_ontology:cs_kernel_codification('8a017475-9c7b-4c8e-8f21-6f0a193449f5', formalized).
narrative_ontology:cs_authority_grounding('8a017475-9c7b-4c8e-8f21-6f0a193449f5', extraction).
narrative_ontology:cs_interpretation_layer_present('8a017475-9c7b-4c8e-8f21-6f0a193449f5').
narrative_ontology:cs_reading_relation('8a017475-9c7b-4c8e-8f21-6f0a193449f5', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a017475-9c7b-4c8e-8f21-6f0a193449f5', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('8a017475-9c7b-4c8e-8f21-6f0a193449f5', foundational, founding_problem_tenure_necessity_dead).
narrative_ontology:cs_axiom_status(founding_problem_tenure_necessity_dead, holdable).
narrative_ontology:cs_axiom_grounding('8a017475-9c7b-4c8e-8f21-6f0a193449f5', founding_problem_tenure_necessity_dead, empirically_contingent).
narrative_ontology:cs_axiom('8a017475-9c7b-4c8e-8f21-6f0a193449f5', foundational, permanent_employment_as_extraction_mechanism).
narrative_ontology:cs_axiom_status(permanent_employment_as_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('8a017475-9c7b-4c8e-8f21-6f0a193449f5', permanent_employment_as_extraction_mechanism, instrumental).
narrative_ontology:cs_reference_frame('8a017475-9c7b-4c8e-8f21-6f0a193449f5', tenure_as_academic_freedom_protection).
narrative_ontology:cs_drift_state('8a017475-9c7b-4c8e-8f21-6f0a193449f5', contemporary_neoliberal_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8a017475-9c7b-4c8e-8f21-6f0a193449f5', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty_cohort).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent contracts insulating them from termination except for cause. They control hiring committees, curriculum design, and peer-review processes that determine who enters the tenured track. They defend tenure as essential to academic freedom while the constraint operates to lock in resource claims regardless of research productivity or instructional necessity. Their exit cost is near-zero (they can move between institutions or retire with security); their benefit from tenure persistence is maximal (permanent income and professional autonomy).
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty_cohort, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, tenured_faculty_cohort, agenda_setter).

% Teach the same courses as tenured faculty but on renewable (often year-to-year) contracts with no job security, no healthcare or retirement benefits, and no voice in departmental governance. They absorb the employment flexibility the tenure constraint requires: when budgets tighten, contingent positions are cut first; when enrollments shift, contingent faculty are reassigned or terminated. Their exit options are limited: leaving academia means abandoning credentialed identity; staying means accepting structural precarity. They bear the cost of institutional rigidity.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    powerless, biographical, trapped, national).

% Pay tuition that funds the tenured faculty salaries and the administrative overhead required to maintain dual labor systems (contingent and permanent). They also absorb reduced instructional investment: departments cannot redeploy resources from underenrolled tenured-faculty courses to high-demand areas without navigating tenure protections. Course availability, section sizes, and curriculum responsiveness are all constrained by the tenure system's rigidity. Their exit is choosing a different institution or forgoing higher education—a high-cost decision.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students, payer,
    powerless, immediate, constrained, national).

% Manages both the formal deference to tenure (protecting tenured faculty from termination) and the informal enforcement (keeping contingent faculty precarious to absorb flexibility costs). They administer hiring to maintain a tenured core, manage contingent contract renewals, and justify budget allocations. They enforce the constraint's rules while absorbing complaints from both contingent faculty and students. They have constrained exit: leaving the institution means forgoing administrative prestige and institutional knowledge.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, department_administration, agenda_setter,
    moderate, biographical, constrained, regional).

% Sets overall budget and hiring strategy. They benefit from tenure because it allows them to maintain a stable faculty core while using contingent labor to absorb cost shocks and enrollment volatility. They could eliminate or substantially reform tenure but choose not to—partly deference to academic culture, partly because contingent labor provides institutional flexibility they value. Their exit is abundant (they can move to corporate strategy, government, or other universities); their incentive to maintain tenure is medium (it enables their preferred operating model).
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, university_administration, agenda_setter,
    powerful, generational, mobile, national).

% Would argue that tenure's primary function is protecting inquiry from political or institutional pressure, not locking in resource claims. They recognize the democratic and epistemic value of academic freedom but contest the institutional_extraction reading's framing—they would claim that decoupling tenure from performance review is necessary precisely to protect research integrity. Excluded from this reading's extraction frame: not invited into the story of how tenure operates as rent capture.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, academic_freedom_advocates, excluded,
    organized, generational, analytical, national).

% Would argue that tenure's primary function is gatekeeping: peer review processes that determine tenure eligibility reproduce dominant demographic groups through opaque 'fit' and 'collegiality' criteria, not through merit. They would contest the extraction reading's focus on resource reallocation—they see tenure primarily as a mechanism for reproducing privilege, not as economically extractive rent. Excluded from this reading: not the frame used here.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, demographic_justice_advocates, excluded,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty_cohort).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Peer oversight of research quality and hiring standards: a community of scholars, insulated from short-term institutional pressure, evaluates each other's work and determines who enters the permanent faculty. Theoretically, this produces less politics-responsive evaluation than institutional administrators would perform.
% TRANSFER_FUNCTION: Transfers permanent income security, professional autonomy, and governance authority from the broader labor pool and student-payer base to the tenured faculty cohort. Simultaneously transfers employment flexibility costs to contingent faculty and instructional-investment constraints to students.
% ABSENT_VOICES: Contingent faculty are structurally excluded from tenure decisions and governance; students are absent from hiring and curriculum oversight; future job-seekers in academic labor markets are absent from the constraint negotiation. Their perspective: the constraint protects existing faculty at the cost of their precarity and opportunity closure.
% DISAPPEARANCE_RATIONALE: If tenure disappeared, universities would shift to at-will employment or rolling contracts for all faculty, redeploying savings to salary raises, course caps, curriculum responsiveness, or contingent-faculty stabilization. Labor markets would reorganize: researchers would pursue funding-based independence (grants, sabbaticals) instead of institutional employment security. Contingent faculty would gain contract stability or exit the labor pool. Students would see course availability shift to match enrollment. The resource lock is real; its removal enables substantial reorganization.
% FOUNDING_PROBLEM: Academic research requires insulation from institutional or political pressure to pursue unpopular or risky inquiry. Early tenured-track systems stabilized scholars against dismissal for heterodox views or controversial findings.
% FOUNDING_PROBLEM_CORROBORATION: Modern employment law, research funding independence (NSF, NIH, private foundations), international research networks, and academic reputation mechanisms now provide substantial insulation from institutional pressure without tenure. This assessment comes from economic analysis by labor economists outside the tenure-benefiting seat, international comparative studies from universities without tenure, and the testimony of contingent-faculty advocates and student representative bodies who do not benefit from tenure persistence. Tenured faculty and academic-freedom advocates outside this reading contest the 'dead' verdict and provide alternative corroboration for 'live' status—but their corroboration is self-interested (they benefit from tenure), which is precisely why the reading identifies them as the extraction beneficiaries rather than neutral assessors.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.78 at interval end) because the constraint locks in permanent income and governance authority for tenured faculty while making no productivity or performance requirement after tenure is granted. The resource claim is durable and insensitive to labor-market conditions or institutional need. Suppression is high (0.71) because the constraint's persistence depends actively on maintaining contingent faculty precarity—removing tenure protection while raising contingent faculty stability would immediately dissolve the flexibility advantage the system provides. The dual labor tier is not accidental; it is the structural mechanism through which tenure operates. Theater ratio rises from 0.28 to 0.48 across the interval, indicating that an increasing share of tenure-defense activity is performative: defending 'academic freedom' and 'research integrity' functions as cover for protecting resource claims even as external employment protections (legal employment law, grant-based independence) have substantially reduced the original founding problem's severity. The time-series reflects cumulative precarity in contingent labor markets and successive waves of administrative emphasis on 'flexibility' and 'accountability' while tenure protections remain formally unchanged.
 *
 * PERSPECTIVAL GAP:
 *   See directionality_logic section.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from three structural facts. (1) Beneficiary identification: tenured faculty explicitly collect from the constraint (permanent income, governance authority, professional autonomy). (2) Victim identification: contingent faculty and students explicitly bear costs (precarity, tuition, instructional degradation). (3) Exit options: tenured faculty have arbitrage-level exit (can move between institutions or exit to other fields; staying is choice); contingent faculty have trapped exit (no alternative employment market values their credentials as highly; staying is necessity); students have constrained exit (can choose a different institution or forgo higher ed; both costly). Directionality for tenured faculty: low (d near 0.0, beneficiary-end). Directionality for contingent faculty: high (d near 1.0, target-end). Directionality for students: intermediate (d ~0.6, partly constrained, diffuse cost). The engine will compute effective extraction (χ) higher for the high-d agents (contingent faculty) and lower or inverted for low-d agents (tenured faculty), capturing how the same constraint operates differently across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the false classification that tenure is 'just coordination'—it is coordination (peer oversight, research protection) layered with extraction (permanent resource claims, employment rigidity). The tangled_rope type is structurally accurate: (1) genuine coordination function exists (peer review arguably does produce better hiring and quality oversight than administrators alone), (2) asymmetric extraction is present (tenured faculty collect; contingent faculty and students pay), (3) active enforcement is required (maintaining contingent precarity, preventing resource redeployment to high-demand areas). Without the extraction layer, tenure would be pure rope (coordination with participants as net beneficiaries). The suppression metric (0.71) and the high extractiveness (0.78) establish that something more than coordination is being maintained. The mandatrophy challenge—'is this really extraction or just the coordination cost'—is addressed by asking: do contingent faculty and students benefit from tenure enough to justify their costs? The answer across all empirical labor-market studies is no; they are not net beneficiaries of the arrangement, which makes their role as victims defensible, not as coordination participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'To what degree has the founding problem (political suppression of research) been solved by modern employment law, research funding independence, and academic reputation mechanisms? Does tenure remain necessary for academic freedom, or is it now purely extractive rent?',
    'Comparative analysis of academic freedom outcomes and research integrity across sectors and countries with different tenure regimes (e.g., research outcomes in European universities with weaker tenure, U.S. research institutes with no tenure, corporate R&D with employment-at-will). If research quality, freedom, and integrity are comparable without tenure, the founding problem is dead.',
    'If the founding problem is dead, the constraint reclassifies from tangled_rope (coordination + extraction) toward pure snare (extraction with coordination cover). The legitimacy basis for the arrangement shifts entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether tenure remains functionally necessary for academic freedom or persists as ritual defending extraction.').

omega_variable(
    alternative_institutional_designs,
    'Could peer-review quality be maintained (the coordination function) without permanent employment protection? For example, rolling 5-year contracts with mandatory external review, performance-independent salary floors, but no tenure permanence?',
    'Pilot programs or natural experiments in universities adopting tenure reform while maintaining peer oversight. Monitor hiring quality, research integrity, and retention of high performers.',
    'If peer-review quality persists without tenure permanence, the extraction can be separated from the coordination, making the current arrangement indefensible on coordination grounds alone. The constraint becomes pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_institutional_designs, empirical, 'Whether the coordination function of tenure review is technically separable from the extraction function of permanence.').

omega_variable(
    contingent_faculty_identity_lock,
    'Is the measured suppression of contingent faculty (trapped exit) structural (no alternative labor markets value their credentials) or internalized (they have internalized the self-concept of contingency and believe they deserve precarity)?',
    'Post-exit trajectory studies: do contingent faculty who leave academia report that their precarity beliefs persist after the structural constraint is removed? Do they seek re-entry?',
    'If suppression is internalized, the effective suppression on contingent faculty is higher than the structural measure suggests—they carry the suppression with them even if they exit. This would increase the extraction calculation for contingent faculty victims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contingent_faculty_identity_lock, empirical, 'Structural vs. internalized mechanisms in contingent faculty employment precarity.').

omega_variable(
    reading_foreclosure_vector,
    'Does the institutional_extraction_reading structurally foreclose the academic_freedom_reading, or do both remain live even if only one is correct about the founding problem''s status?',
    'Logical analysis: the academic_freedom reading asserts tenure is necessary for freedom; the extraction reading asserts the founding problem is dead so freedom doesn''t require it. These are empirically testable (see omega_1) but logically coexist if uncertainty about necessity is admitted. Only if we know with certainty that freedom persists without tenure does extraction_reading foreclose academic_freedom_reading.',
    'If both readings remain live despite empirical disagreement, the relation is coexists_with; if the empirical evidence settles necessity, the relation becomes forecloses (institutional_extraction forecloses academic_freedom). Affects cs_structure.reading_relations authoring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vector, conceptual, 'Whether this reading''s core premise (founding problem dead) logically forecloses the sibling academic_freedom_reading or merely contradicts it empirically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(tenu_tr_t0, observed).
narrative_ontology:measurement(tenu_tr_t5, tenure_contract__institutional_extraction_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(tenu_tr_t5, observed).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__institutional_extraction_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(tenu_tr_t10, observed).
narrative_ontology:measurement(tenu_tr_t15, tenure_contract__institutional_extraction_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(tenu_tr_t15, observed).
narrative_ontology:measurement(tenu_tr_t20, tenure_contract__institutional_extraction_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement_basis(tenu_tr_t20, observed).
narrative_ontology:measurement(tenu_tr_t25, tenure_contract__institutional_extraction_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement_basis(tenu_tr_t25, observed).
narrative_ontology:measurement(tenu_tr_t30, tenure_contract__institutional_extraction_reading, theater_ratio, 30, 0.47).
narrative_ontology:measurement_basis(tenu_tr_t30, observed).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__institutional_extraction_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(tenu_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(tenu_be_t0, observed).
narrative_ontology:measurement(tenu_be_t5, tenure_contract__institutional_extraction_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(tenu_be_t5, observed).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__institutional_extraction_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement_basis(tenu_be_t10, observed).
narrative_ontology:measurement(tenu_be_t15, tenure_contract__institutional_extraction_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement_basis(tenu_be_t15, observed).
narrative_ontology:measurement(tenu_be_t20, tenure_contract__institutional_extraction_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(tenu_be_t20, observed).
narrative_ontology:measurement(tenu_be_t25, tenure_contract__institutional_extraction_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement_basis(tenu_be_t25, observed).
narrative_ontology:measurement(tenu_be_t30, tenure_contract__institutional_extraction_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement_basis(tenu_be_t30, observed).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__institutional_extraction_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(tenu_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(tenu_su_t0, observed).
narrative_ontology:measurement(tenu_su_t5, tenure_contract__institutional_extraction_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(tenu_su_t5, observed).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__institutional_extraction_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(tenu_su_t10, observed).
narrative_ontology:measurement(tenu_su_t15, tenure_contract__institutional_extraction_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(tenu_su_t15, observed).
narrative_ontology:measurement(tenu_su_t20, tenure_contract__institutional_extraction_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(tenu_su_t20, observed).
narrative_ontology:measurement(tenu_su_t25, tenure_contract__institutional_extraction_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(tenu_su_t25, observed).
narrative_ontology:measurement(tenu_su_t30, tenure_contract__institutional_extraction_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(tenu_su_t30, observed).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__institutional_extraction_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(tenu_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tenure_contract__institutional_extraction_reading, 0.12).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the tenure_contract kernel. The academic_freedom_reading emphasizes protection of inquiry from institutional pressure; the demographic_reproduction_reading emphasizes demographic gatekeeping through peer review; the institutional_extraction_reading (this story) emphasizes resource lock-in and employment rigidity. All three share the formal kernel (peer-determined permanence) but instantiate different constraints because they identify different beneficiaries/victims and assess extractiveness differently. The stories are linked via network.affects_constraints because adoption of one reading's empirical claims (e.g., that the founding problem is dead) would modulate the classification of the others. Decomposition per ε-invariance principle: each reading gets its own ε because the structural assessment of what tenure *is for* differs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
