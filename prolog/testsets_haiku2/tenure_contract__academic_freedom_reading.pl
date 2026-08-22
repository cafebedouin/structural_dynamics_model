% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Academic Tenure as Truth-Seeking Stabilizer (Freedom Reading)
 *   domain: institutional/academic/labor
 *
 * SUMMARY:
 *   Academic tenure, under the academic-freedom reading, is a coordination
 *   mechanism that stabilizes truth-seeking by decoupling researcher survival
 *   from institutional displeasure, donor pressure, or political backlash.
 *   This reading interprets tenure as a solution to an epistemic problem:
 *   without insulation from employment threat, researchers face incentive to
 *   self-censor or redirect inquiry toward politically safe and
 *   institutionally profitable topics. Tenure solves this by making research
 *   direction independent of institutional preferences—tenured scholars can
 *   pursue high-risk inquiry (controversial findings, long-term projects,
 *   unpopular topics) without fear of retaliation. This reading is contested:
 *   the institutional-extraction reading views tenure as permanent rent
 *   collection; the demographic-reproduction reading views tenure review as
 *   gatekeeping. The claim/metric gap is intentional: the constraint is
 *   CLAIMED as rope (genuine coordination) while the authored metrics show
 *   modest extractiveness (0.28 end-state) and low suppression (0.15)—the
 *   engine's measurement is what separates this reading's framing from the
 *   others' claims about the same institutional kernel.
 *
 * KEY AGENTS:
 *   - Tenured faculty: hold protected employment status; benefit from independence to pursue high-risk research; their institutional position is the constraint's primary object.
 *   - Research universities: set and enforce tenure standards; benefit from the prestige of independent scholarship; maintain the arrangement through professional norms.
 *   - Political and donor actors: lose direct leverage over research direction; bear the cost of not being able to suppress findings that displease them.
 *   - Graduate and teaching students: incidental beneficiaries via access to mentors whose independence is protected; no control over the arrangement.
 *   - Contingent faculty (adjuncts, postdocs): structurally excluded; bear precarity while tenure holders capture security.
 *   - Analytical observer: measures the constraint's actual operation across competing readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.28).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.15).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Academic Tenure as Truth-Seeking Stabilizer (Freedom Reading)").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "institutional/academic/labor").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, 'b50b64e7-73d0-492a-9ccb-119b798ea723').
narrative_ontology:cs_kernel_codification('b50b64e7-73d0-492a-9ccb-119b798ea723', formalized).
narrative_ontology:cs_authority_grounding('b50b64e7-73d0-492a-9ccb-119b798ea723', practice).
narrative_ontology:cs_interpretation_layer_present('b50b64e7-73d0-492a-9ccb-119b798ea723').
narrative_ontology:cs_reading_relation('b50b64e7-73d0-492a-9ccb-119b798ea723', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b50b64e7-73d0-492a-9ccb-119b798ea723', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('b50b64e7-73d0-492a-9ccb-119b798ea723', foundational, epistemic_integrity_requires_employment_protection).
narrative_ontology:cs_axiom_status(epistemic_integrity_requires_employment_protection, holdable).
narrative_ontology:cs_axiom_grounding('b50b64e7-73d0-492a-9ccb-119b798ea723', epistemic_integrity_requires_employment_protection, empirically_contingent).
narrative_ontology:cs_axiom('b50b64e7-73d0-492a-9ccb-119b798ea723', foundational, truth_seeking_suppression_by_institutional_pressure_is_real).
narrative_ontology:cs_axiom_status(truth_seeking_suppression_by_institutional_pressure_is_real, holdable).
narrative_ontology:cs_axiom_grounding('b50b64e7-73d0-492a-9ccb-119b798ea723', truth_seeking_suppression_by_institutional_pressure_is_real, empirically_contingent).
narrative_ontology:cs_reference_frame('b50b64e7-73d0-492a-9ccb-119b798ea723', protected_independent_inquiry).
narrative_ontology:cs_drift_state('b50b64e7-73d0-492a-9ccb-119b798ea723', contemporary_political_polarization, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b50b64e7-73d0-492a-9ccb-119b798ea723', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, research_universities).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, knowledge_production_ecosystem).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, graduate_students).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, teaching_students).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, political_actors).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, institutional_donors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain employment security that decouples survival from institutional or political pressure. Can pursue high-risk, controversial, or slow-payoff research without fear of retaliation for findings that displease donors, administrators, or political actors. Exit is available (other universities, other sectors) but tenure value is substantial — the institutional independence it provides is what they remain for.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    moderate, generational, arbitrage, national).

% Set and enforce tenure standards; admit some scholars into the protected cohort and exclude others. Benefit from the reputation of researchers who can pursue risky inquiry without institutional fear; benefit from the institutional prestige that comes from hosting scholars known for independence. The arrangement is self-enforcing via professional norms: a university that violated tenure would lose recruitment power.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, research_universities, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(tenure_contract__academic_freedom_reading, research_universities, beneficiary).

% Trained by researchers who can model high-risk inquiry without institutional fear. Gain access to mentors whose independence is protected. Benefit from the quality of research that tenure-protection enables — slower, deeper work on hard problems. No direct control over the arrangement; their benefit is incidental to the faculty protection.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, graduate_students, beneficiary,
    powerless, biographical, constrained, national).

% Cannot suppress academic research findings they dislike through direct pressure on employment. Tenured researchers are insulated from backlash (legislative, public, donor-driven). The constraint prevents political actors from steering research toward preferred narratives via threat of retaliation. They bear the cost of lost influence, not a direct economic cost.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, political_actors, payer,
    powerful, biographical, trapped, national).

% Cannot redirect faculty research via threat of funding withdrawal. If a tenured researcher publishes findings that contradict donor preferences, the donor's leverage is limited — the researcher cannot be easily displaced. Donors can restrict new funding but cannot undo tenure. The constraint limits donor influence over active inquiry.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, institutional_donors, payer,
    powerful, biographical, constrained, national).

% Taught by researchers whose intellectual independence is protected. Encounter faculty who can model critical thinking and teach controversial material without institutional fear of retaliation. Their benefit is incidental; they do not control the arrangement.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, teaching_students, beneficiary,
    powerless, biographical, constrained, local).

% Institutional arrangements (at-will employment, performance-based contracts, political appointment) that would allow governance by employers or external actors are structurally foreclosed within universities that have tenure. They would be competitors for institutional form; tenure rules them out.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, alternative_institutional_models, excluded,
    powerful, biographical, trapped, global).

% Observes the institutional arrangement from the outside, measures its structural properties, and compares its effects across different readings.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__academic_freedom_reading, research_universities).
narrative_ontology:fixing_cost_class(tenure_contract__academic_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes truth-seeking by insulating researchers from institutional, political, and donor pressure. Solves the collective-action problem that arises when powerful external actors have incentive to suppress findings they dislike: tenure removes the mechanism of suppression (employment threat) and enables inquiry that serves epistemic goals rather than political preferences.
% TRANSFER_FUNCTION: Transfers employment security from all other workers (who remain at-will) to a protected cohort of scholars. The transfer is from institutional authority (which gives up the power to dismiss for finding-displeasure) to protected faculty (who gain permanent claim on salary and position). Secondary transfer: from political and donor actors who lose leverage over active inquiry.
% ABSENT_VOICES: Contingent instructors and adjuncts are structurally excluded from the conversation—they are the institutional counterpart to tenured faculty, bearing precarity while tenure holders capture security. They would argue that tenure creates a two-tier labor market and that diffusing job security more broadly would be more equitable. Non-faculty researchers (postdocs, research scientists) also remain excluded from tenure protection despite doing research under similar pressures.
% DISAPPEARANCE_RATIONALE: If tenure vanished overnight, universities would become responsive to political and donor pressure on research direction. Scholars studying politically contested topics (climate, race, sexuality, inequality) would face institutional pressure to self-censor or find employment elsewhere. Research on slow-payoff, high-risk problems would shift toward politically safe and commercially quick-payoff topics. The composition of scholars would change as political and donor preferences sorted admissions. The ecosystem would reorganize around institutional survival rather than epistemic goals.
% FOUNDING_PROBLEM: Epistemic integrity: truth-seeking is undermined when researchers face employment threat for findings that displease powerful external actors. Without insulation, inquiry becomes hostage to political and donor preferences, and high-risk research becomes economically impossible.
% FOUNDING_PROBLEM_CORROBORATION: This reading is attested by the historical record of political suppression of research (evolutionary biology, climate science, epidemiology, racial science debunkings) where untenured or vulnerable researchers faced backlash. Corroborated by international examples: countries without tenure have seen research suppressed in politically sensitive fields. Corroborated by researchers in contingent positions who report self-censoring on controversial topics due to employment precarity.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.28 at end, rising slightly from 0.18 at start as institutional pressure increases over the interval) because the arrangement's primary function—enabling independent inquiry—is genuine coordination that benefits the broader epistemic ecosystem. The rising extractiveness trajectory reflects institutional drift: as political polarization increases and donor pressure on universities intensifies, the constraint's protective function becomes more costly to maintain, requiring more active institutional defense. Suppression is very low (0.15) because tenure operates by removing the mechanism of suppression, not by suppressing alternatives—it does not prevent external actors from trying to influence research; it only prevents them from succeeding via employment threat. Theater is minimal (0.12 at end) because the institutional machinery is genuinely functional: the constraint actually does what it claims (protect independent inquiry) rather than performing protection while extracting for other purposes. Accessibility collapse is low (0.22) because alternatives remain available: scholars can choose non-tenure-track positions, move to different institutions, shift to applied work, or exit academia entirely—tenure is not the only path. Resistance is moderate (0.35) because the arrangement faces real contestation: political actors resist the constraint's foreclosure of their influence; institutional actors resist tenure's inflexibility for resource reallocation; and alternative institutional models remain live proposals. The metrics trace one shared time grid: every metric is authored at every time point (0, 10, 20, 30, 40, 50) to prevent temporal misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (political actors, donors) should compute tenure very differently from the beneficiary seats (tenured faculty). From the political actor's position, tenure is a constraint on their legitimate institutional authority and a loss of influence—they experience high directionality toward the target end. From the faculty position, tenure is genuine coordination providing the independence necessary for research integrity—they experience low directionality, near the beneficiary end. The agenda-setter (research universities) occupies a dual position: they benefit from the prestige of protected scholars but also bear the cost of institutional inflexibility and the obligation to defend faculty against external pressure. The engine computes these divergences from the structural data (power, exit options, beneficiary/victim declarations) without reconciling them to a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty are structural beneficiaries (d near 0.0-0.3): they receive employment security and independence; they have arbitrage exit (can move to other universities or sectors if tenure is threatened locally, but tenure value is high). Political and donor actors are structural targets (d near 0.7-0.9): they lose leverage over research direction; they are trapped (cannot exit the public sphere or replace research universities with their own institutions without losing legitimacy as neutral knowledge producers). Research universities are agenda-setters (d near 0.5): they set tenure terms but also depend on tenure's institutional prestige; they benefit from protected scholars but bear the cost of defending them against external pressure. Graduate and teaching students are incidental beneficiaries (d near 0.2-0.3): they benefit from better teaching and research but do not directly control or bear the constraint. No directionality overrides are needed; the derivation chain (beneficiary/victim + exit options + power) produces accurate d values across all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (epistemic integrity under external pressure) remains live and structural—it is not a solved problem that the constraint now exploits for extraction. The constraint's extractiveness (0.28) is modest and stable, not accumulating, which indicates the constraint is maintaining its coordination function rather than shifting toward pure extraction. The low theater ratio (0.12) confirms the institutional machinery is doing what it claims: protecting inquiry rather than performing protection while extracting for other purposes. The constraint avoids the mandatrophy trap (founding problem dead, constraint persists theatrically) because the founding problem is continuously active—political pressure on universities and donor influence attempts are ongoing, making the protection function perpetually necessary. This distinguishes the academic-freedom reading from the institutional-extraction reading, which would argue that the founding problem is solved (universities are now stable institutions, the 20th-century crisis of academic autonomy is over) and tenure persists as rent extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    independence_vs_capture_drift,
    'Is the measured increase in extractiveness (0.18 to 0.28 over the interval) evidence of genuine institutional drift toward extraction, or does it reflect rising cost-of-defense as external pressure increases while the constraint''s protective function remains intact?',
    'Distinguish the constraint''s output (independent research remains produced despite external pressure) from the cost-of-maintenance (universities must spend more effort and resources defending tenure against external attack). Compare research output and topic diversity across the interval; if these hold stable while maintenance cost rises, the drift is cost-of-defense, not functional erosion.',
    'If drift is cost-of-defense, the constraint remains a genuine coordination mechanism (extractiveness rise reflects defensive spending, not shift toward extraction). If drift is toward capture, the constraint is shifting from coordination toward institutional-extraction reading. This affects whether the constraint certifies as rope or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independence_vs_capture_drift, empirical, 'Whether rising extractiveness reflects increasing maintenance cost or functional drift toward extraction.').

omega_variable(
    contingent_faculty_suppression_mechanism,
    'The constraint protects some faculty (tenured) from employment threat but creates precarity for contingent faculty (adjuncts, postdocs) who do similar research work. Is the protection of tenure structurally dependent on the precarity of contingency, or are they separable arrangements?',
    'Institutional experiments: do universities that attempt to extend tenure-like protections to contingent faculty lose research independence (because the cost of protection becomes unsustainable), or do they maintain both protections with different resource structures?',
    'If the protections are inseparable (tenure requires contingency), then the constraint''s extractiveness is actually higher than measured—the full cost includes the suppression of contingent faculty, and the beneficiary set is smaller (only tenured cohort, not the research ecosystem broadly). If separable, the constraint is genuinely lower-extractive rope, and the contingent-faculty precarity is a distinct constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contingent_faculty_suppression_mechanism, empirical, 'Whether tenure''s protection is structurally dependent on contingent-faculty precarity or whether both could be protected independently.').

omega_variable(
    political_suppression_counterfactual,
    'This reading claims tenure protects against political suppression. In the absence of tenure, would political actors actually suppress inconvenient research, or would researchers self-select away from politically sensitive topics without direct institutional suppression?',
    'Historical case studies of research suppression in countries without tenure; comparison of research topics and findings across tenure vs. non-tenure systems; post-exit interviews with researchers who left academia due to political pressure.',
    'If political suppression is active and direct (institutions fire researchers for findings), tenure''s protection is structurally necessary and the coordination claim is strong. If suppression is primarily self-selection and self-censorship, then tenure''s role is smaller—it reduces internalized suppression rather than blocking active institutional suppression—and the constraint''s extractiveness classification may shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_suppression_counterfactual, empirical, 'Whether external political actors actively suppress research or whether suppression operates primarily through researcher self-selection.').

omega_variable(
    kernel_framing_under_determination,
    'Is tenure''s core function epistemic (protecting truth-seeking from external pressure) or distributional (protecting early-career winners against institutional reorganization and contingent workers from precarity)? The kernel admits both framings; this reading adopts the epistemic frame. What signals or institutional evidence support one framing over the other?',
    'Examine the founding documents and historical justifications for tenure (how it was originally defended and by whom); examine actual research outcomes under tenure vs. non-tenure systems; examine where political pressure on research has been strongest and whether tenure has successfully resisted it.',
    'If the epistemic framing is correct, tenure is a coordination mechanism (rope) whose extractiveness is modest and stable. If the distributional framing is correct, tenure is better understood as institutional-extraction reading or demographic-reproduction reading. The choice of framing affects the constraint''s type certification and its beneficiary/victim structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Whether tenure''s primary function is epistemic (protecting truth-seeking) or distributional (protecting incumbent positions and precarity).').

omega_variable(
    reading_differentiation_mechanism,
    'This reading attributes tenure''s persistence to its epistemic function (truth-seeking requires protection). The institutional-extraction reading attributes persistence to rent-seeking (incumbents defend tenure because it protects their positions). What would falsify each reading? What observable would distinguish genuine epistemic protection from rationalization of incumbency protection?',
    'Observe whether tenure institutions (a) maintain research independence in politically contested fields (epistemic protection claim), (b) protect underperforming or unproductive scholars (extraction claim), and (c) block demographic diversification through gatekeeping (reproduction claim). Track which reading''s predictions match observed outcomes across these three dimensions.',
    'Falsification would route through cs_structure: if epistemic protection is falsified (research independence erodes despite tenure), the reading''s core axiom (epistemic_integrity_requires_employment_protection) becomes holdable but empirically challenged, opening the gate to axiom_overriding drift_state classification. This does NOT foreclose the reading (foreclosure would require the axiom to be directly contradicted by another reading, not merely empirically falsified), but it would flag the reading as empirically degraded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_differentiation_mechanism, empirical, 'Differentiating the epistemic-protection framing from the incumbency-protection rationalization through observable research outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(tenu_tr_t0, observed).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__academic_freedom_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(tenu_tr_t10, observed).
narrative_ontology:measurement(tenu_tr_t20, tenure_contract__academic_freedom_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(tenu_tr_t20, observed).
narrative_ontology:measurement(tenu_tr_t30, tenure_contract__academic_freedom_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement_basis(tenu_tr_t30, observed).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__academic_freedom_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(tenu_tr_t40, observed).
narrative_ontology:measurement(tenu_tr_t50, tenure_contract__academic_freedom_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(tenu_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(tenu_be_t0, observed).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__academic_freedom_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement_basis(tenu_be_t10, observed).
narrative_ontology:measurement(tenu_be_t20, tenure_contract__academic_freedom_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement_basis(tenu_be_t20, observed).
narrative_ontology:measurement(tenu_be_t30, tenure_contract__academic_freedom_reading, base_extractiveness, 30, 0.27).
narrative_ontology:measurement_basis(tenu_be_t30, observed).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__academic_freedom_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(tenu_be_t40, observed).
narrative_ontology:measurement(tenu_be_t50, tenure_contract__academic_freedom_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement_basis(tenu_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(tenu_su_t0, observed).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__academic_freedom_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement_basis(tenu_su_t10, observed).
narrative_ontology:measurement(tenu_su_t20, tenure_contract__academic_freedom_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement_basis(tenu_su_t20, observed).
narrative_ontology:measurement(tenu_su_t30, tenure_contract__academic_freedom_reading, suppression_requirement, 30, 0.15).
narrative_ontology:measurement_basis(tenu_su_t30, observed).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__academic_freedom_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement_basis(tenu_su_t40, observed).
narrative_ontology:measurement(tenu_su_t50, tenure_contract__academic_freedom_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement_basis(tenu_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tenure_contract__academic_freedom_reading, 0.12).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the TENURE_CONTRACT kernel. Three structurally distinct readings decompose the contested concept: (1) ACADEMIC_FREEDOM_READING (this file)—tenure as epistemic coordination, enabling truth-seeking under external pressure; (2) INSTITUTIONAL_EXTRACTION_READING—tenure as rent extraction, preventing resource reallocation and loading costs onto contingent faculty; (3) DEMOGRAPHIC_REPRODUCTION_READING—tenure peer review as demographic gatekeeping. Each reading observes the same institutional kernel (formal tenure rules, peer review, employment protection) but interprets its function, beneficiary structure, and ε differently. They coexist as live positions held by different institutional constituencies; none logically forecloses the others. The readings form a network through affects_constraints: the epistemic reading influences the others because if tenure's primary function is epistemic protection, then institutional-extraction and demographic-reproduction readings must account for why such a vital function has those side effects. Compare ε values: academic-freedom (0.28), institutional-extraction (expected high, ~0.65-0.75), demographic-reproduction (expected moderate-high, ~0.55-0.65). The divergence tests the readings' structural claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
