% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Tenure as Academic Freedom Protection
 *   domain: higher_education/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   Under the academic freedom reading of tenure, the constraint operates as
 *   a coordination mechanism that protects researchers from political and
 *   institutional retaliation, enabling truth-seeking inquiry that would
 *   otherwise be suppressed by pressure from political actors, donors, or
 *   ideological movements. Faculty gain autonomy; the research community
 *   gains access to high-risk knowledge production; students benefit from
 *   exposure to paradigm-challenging research. Institutions bear the cost of
 *   rigidity and defending controversial faculty. Contingent workers bear the
 *   structural cost of tenure-track scarcity. The constraint's extractiveness
 *   is moderate (0.28) because faculty autonomy is a genuine benefit that
 *   justifies some institutional cost, but the distribution is asymmetric:
 *   beneficiaries (faculty) have secure exit (arbitrage for some, certainly
 *   better exit than contingent workers), while payers (institutions and
 *   contingent workers) lack alternatives. This reading treats tenure as a
 *   rope with modest asymmetric costs, not as pure extraction.
 *
 * KEY AGENTS:
 *   - tenured_faculty: beneficiary of employment security and research autonomy; moderate power, long time horizon, arbitrage exit
 *   - institutional_administrators: payer of employment rigidity and pressure defense; powerful, but constrained by norms of academic freedom
 *   - contingent_faculty: payer of scarcity-induced precarity; powerless, immediate horizon, trapped exit
 *   - political_actors: excluded from direct control; would prefer at-will employment enabling ideological hiring filters
 *   - research_community: non-agent beneficiary; gains paradigm-challenging knowledge
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
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Tenure as Academic Freedom Protection").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '3f40bcf1-975a-4c8d-81e1-9a823833c7c5').
narrative_ontology:cs_kernel_codification('3f40bcf1-975a-4c8d-81e1-9a823833c7c5', formalized).
narrative_ontology:cs_authority_grounding('3f40bcf1-975a-4c8d-81e1-9a823833c7c5', lineage).
narrative_ontology:cs_interpretation_layer_present('3f40bcf1-975a-4c8d-81e1-9a823833c7c5').
narrative_ontology:cs_reading_relation('3f40bcf1-975a-4c8d-81e1-9a823833c7c5', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f40bcf1-975a-4c8d-81e1-9a823833c7c5', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('3f40bcf1-975a-4c8d-81e1-9a823833c7c5', foundational, political_independence_protects_truth_seeking).
narrative_ontology:cs_axiom_status(political_independence_protects_truth_seeking, holdable).
narrative_ontology:cs_axiom_grounding('3f40bcf1-975a-4c8d-81e1-9a823833c7c5', political_independence_protects_truth_seeking, empirically_contingent).
narrative_ontology:cs_axiom('3f40bcf1-975a-4c8d-81e1-9a823833c7c5', foundational, peer_governance_legitimacy_requires_tenure_security).
narrative_ontology:cs_axiom_status(peer_governance_legitimacy_requires_tenure_security, holdable).
narrative_ontology:cs_axiom_grounding('3f40bcf1-975a-4c8d-81e1-9a823833c7c5', peer_governance_legitimacy_requires_tenure_security, instrumental).
narrative_ontology:cs_reference_frame('3f40bcf1-975a-4c8d-81e1-9a823833c7c5', political_independence_for_research).
narrative_ontology:cs_drift_state('3f40bcf1-975a-4c8d-81e1-9a823833c7c5', contemporary_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3f40bcf1-975a-4c8d-81e1-9a823833c7c5', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, doctoral_students).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, research_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, institutional_administrators).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, contingent_faculty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive multi-year or permanent employment security once tenure is granted, which decouples their research direction and public statements from immediate institutional or political pressure. Can pursue unpopular, risky, or paradigm-challenging research without fear of retaliation for findings or opinions. The security enables long-term project investment and controversial publication.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    moderate, generational, arbitrage, national).

% Train under faculty whose research direction is not dictated by political or institutional popularity contests. Inherit an intellectual environment where hypothesis-driven inquiry is protected by advisors' tenure, enabling exposure to cutting-edge, high-risk research programs. Quality of mentorship depends on faculty freedom to pursue genuine intellectual problems.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, doctoral_students, beneficiary,
    powerless, biographical, constrained, national).

% A collective entity (not an agent) that benefits from the production of knowledge unconstrained by political or institutional expedience. Tenure enables paradigm-challenging research that is filtered by peers, not by political authorities or funders.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, research_community, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(tenure_contract__academic_freedom_reading, research_community).

% Bear the cost of employment rigidity: tenured faculty cannot be easily dismissed, reassigned, or replaced to meet changing institutional priorities or resource constraints. Must defend controversial faculty against external pressure (political actors, donors, activists) and cannot quickly redirect labor to institutional growth areas. Face reputational damage if they appear to chill faculty speech.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, institutional_administrators, payer,
    powerful, biographical, mobile, national).

% Bear the structural cost of tenure-track scarcity: institutions minimize tenure-track lines to reduce permanent obligations, shifting teaching and research labor to adjuncts, postdocs, and term instructors without job security. As tenure-track positions remain shielded, contingent positions expand, inverting the labor pyramid and creating a two-tier system where the majority of academic work is unprotected.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, contingent_faculty, payer,
    powerless, immediate, trapped, national).

% Are structurally barred from directly controlling academic content or dismissing faculty for political heterodoxy. Tenure prevents the state, donors, or movement-aligned actors from using employment as a lever to suppress disfavored research or enforce ideological conformity. They can attack tenure publicly, defund institutions, or support alternative funding models, but cannot unilaterally revoke tenure security.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, political_actors, excluded,
    institutional, biographical, trapped, national).

% Monitor institutional compliance with academic freedom standards, which are often operationalized through tenure protection. They evaluate whether institutions maintain research integrity by protecting faculty from retaliation for findings, and whether tenure is administered fairly.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, accreditation_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__academic_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: absent tenure security, individual faculty face incentives to avoid politically risky research, leading to a tragedy of the commons where the production of socially necessary but unpopular knowledge declines. Tenure aggregates faculty into a protected class whose cumulative research output includes high-risk, paradigm-challenging inquiry that no individual faculty member would risk alone.
% TRANSFER_FUNCTION: Transfers employment security and research autonomy to tenured faculty; transfers rigidity and the cost of defending controversial research to institutions; transfers scarcity and precarity to contingent workers who do not hold tenure-track positions.
% ABSENT_VOICES: Contingent faculty, who bear the structural cost of tenure-track scarcity but are not in the conversation about tenure's justification or reform. Political actors who perceive tenure as protecting disfavored research and would argue for at-will employment are formally excluded from faculty governance. Students (particularly undergraduates) have no voice in tenure decisions despite being the primary audience for teaching by tenure-track faculty.
% DISAPPEARANCE_RATIONALE: If tenure disappeared overnight, institutions would rapidly consolidate contingent labor, faculty research portfolios would shift toward fundable and non-controversial topics, political actors could directly suppress disfavored research through hiring and firing, and the institutional capacity for long-term, risky inquiry would contract substantially. The research ecosystem would reorganize around political and funder preferences rather than peer-adjudicated intellectual merit.
% FOUNDING_PROBLEM: Early 20th-century research universities faced political and religious pressure to suppress evolutionary biology, secularism, and labor organizing. Faculty faced firing for research findings and political speech; knowledge production was being filtered through institutional and political gatekeepers rather than peer review.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary instances of political pressure on academic hiring and research (documented by FIRE, AAUP, and independent journalism on state legislative interference, donor pressure campaigns, and student activism targeting faculty removal) attest the founding problem persists. Faculty accounts and institutional case studies from outside the tenure-benefiting parties confirm the pressure remains real. The debate is not whether political pressure exists, but whether tenure is the correct institutional response.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low-to-moderate (0.28) because the constraint solves a real coordination problem (political pressure on research) AND produces asymmetric distribution (faculty benefit more than institutions lose, relative to the founding problem). Suppression is very low (0.15) because the constraint's persistence does NOT depend primarily on coercion; it persists because faculty, student bodies, and accreditation bodies endorse the principle of academic freedom, and because institutional prestige correlates with research output enabled by tenure security. Theater ratio is very low (0.12) because the constraint's functional purpose (enabling risky research) and its actual operation (faculty pursue unpopular research) are largely aligned; there is no large performative component masking atrophy. Accessibility collapse is moderate (0.42) because alternatives to tenure (performance-based contracts, at-will employment with strong norms, external funding security) are cognitively available but structurally difficult to implement (require coordination across institutions, break career expectations, invite political capture). Resistance is moderate (0.38) because external political pressure on tenure is real and sustained, but institutional commitment to the principle remains strong. The measurement series show slow upward drift in extractiveness and suppression over the interval, reflecting modest intensification of political pressure and institutional defensiveness, but not fundamental change. This differs from the institutional extraction reading, which would show higher extraction metrics and rising theater ratio as the coordination function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (institutional administrators) and the beneficiary seat (tenured faculty) compute different types from the same structural data. From the administrator's position, tenure is a coordination mechanism they maintain (rope, moderate extraction cost justified by benefits). From the political actor's position (excluded seat), tenure is a protection against legitimate institutional control (high extraction cost, viewed as blocking merit-based hiring and ideological diversity). From the contingent faculty position, tenure is a scarcity mechanism that loads costs onto the precariat to subsidize security for a minority. The engine computes these seats' divergent directionalities from the power atoms and exit options: faculty directionality near beneficiary end (moderate power, arbitrage exit, named as beneficiary); administrators near payer end (powerful, but constrained by norms); political actors excluded; contingent workers at full-target end (powerless, trapped exit, named as payer). The authorized claim is rope; the metrics describe modest asymmetric coordination. If computed type diverges from claimed type, that divergence is the measurement the corpus takes — a claimed rope that computes as tangled_rope would indicate the asymmetry is higher than this reading allows.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (tenured faculty, doctoral students, research community) have low directionality (near 0.2–0.3): faculty autonomy and research independence reduce cost while increasing benefit; doctoral students inherit protected mentorship (pure benefit, constrained exit but aligned interests); research_community is non-agent, so no directionality computation. Payers (institutions, contingent faculty) have high directionality (0.6–0.9): institutions are powerful but constrained by norms and prestige pressure (override to 0.55 from baseline 0.7 to reflect norm constraint); contingent faculty are powerless and trapped (directionality near 0.85, full target). The excluded political_actors seat would compute high directionality if authorized (they want to unblock what tenure blocks), but as excluded they are not in the stakeholder set. The asymmetry between beneficiary and payer directionality drives the measured asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT resolve mandatrophy because the founding problem (political pressure on research) remains live and the constraint's primary function (enabling unpopular research) remains operationally active. Where mandatrophy arises is in the institutional_extraction reading, which would argue that tenure's original protective function (shielding research from political pressure) is no longer the live problem, while its secondary effects (employment rigidity, demographic gatekeeping, cost loading onto contingent workers) have become dominant. The academic_freedom reading disputes this diagnosis: it argues the founding problem is still live (contemporary political pressure on research is well documented), the function still operates (faculty do pursue high-risk research they would not pursue under at-will employment), and the constraint should be maintained despite its costs. This is not a resolved mandatrophy case; it is an active normative dispute about which reading captures the constraint's current balance of function and dysfunction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_pressure_counterfactual,
    'Would faculty research portfolios shift significantly toward non-controversial, fundable, or politically aligned topics if tenure were abolished and replaced with at-will employment?',
    'Natural experiment from institutions or jurisdictions that transition away from tenure (e.g., some private universities, contingent-labor-heavy departments). Measure research topic distribution, risk levels, and publication patterns before and after tenure removal.',
    'If portfolios shift substantially toward safe/fundable topics, it demonstrates tenure''s core function (enabling risky research despite pressure) is operationally real. If portfolios remain unchanged, the founding problem no longer constrains faculty research choices and the constraint''s protective function may have atrophied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_pressure_counterfactual, empirical, 'Whether tenure protection enables research that would not be pursued under at-will employment.').

omega_variable(
    institutional_cost_distribution,
    'Is the cost of tenure-induced employment rigidity borne equally across institution types, or concentrated on certain sectors?',
    'Institutional analysis comparing tenure prevalence, contingent labor ratios, and financial stress across research universities, teaching colleges, and for-profit institutions.',
    'If concentrated costs (e.g., teaching-intensive institutions bear more precarity while research universities maintain tenure security), the constraint functions as risk transfer from beneficiaries to the most vulnerable payers. If distributed, the asymmetry may be justified by collective benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_cost_distribution, empirical, 'Whether the cost of tenure rigidity is disproportionately borne by contingent workers and resource-constrained institutions.').

omega_variable(
    alternative_protection_mechanisms,
    'Are there structurally equivalent protections for academic freedom that do not require permanent employment security (e.g., strong norms, legal statute, research-funding security)?',
    'Comparative institutional analysis of jurisdictions with academic freedom protections decoupled from tenure (e.g., some European systems with strong constitutional protections and collective bargaining, rather than individual permanent contracts).',
    'If alternatives exist and function comparably, tenure may be a sufficient but not necessary protection, and the asymmetric costs could be reduced. If alternatives fail or are weaker, tenure''s particular design (permanent security) is essential to the protection function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_protection_mechanisms, conceptual, 'Whether academic freedom protection requires permanent employment or can be achieved through alternative institutional designs.').

omega_variable(
    reading_boundary_extraction_vs_protection,
    'At what threshold of employment rigidity cost does tenure transition from being a coordination mechanism (modest asymmetric cost justified by collective benefit) to being pure extraction (asymmetry unjustified by coordination function)?',
    'Comparative measurement of extractiveness, theater_ratio, and contingent labor prevalence across constraint cases. Establish empirical threshold where the institutional_extraction_reading''s higher metrics become more descriptively accurate than the academic_freedom_reading''s moderate metrics.',
    'This omega documents the boundary between the two readings: if institutional_extraction_reading''s metrics (extractiveness > 0.55, theater_ratio > 0.4, contingent labor > 60% of teaching) are observed, the academic_freedom reading may no longer accurately describe the constraint, and institutional_extraction_reading becomes the correct reading for that context.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_extraction_vs_protection, conceptual, 'The reading-selection problem: under what conditions does the academic_freedom reading cease to be accurate and institutional_extraction_reading become more descriptive?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t1990, tenure_contract__academic_freedom_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(tenu_tr_t2000, tenure_contract__academic_freedom_reading, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(tenu_tr_t2008, tenure_contract__academic_freedom_reading, theater_ratio, 2008, 0.1).
narrative_ontology:measurement(tenu_tr_t2016, tenure_contract__academic_freedom_reading, theater_ratio, 2016, 0.11).
narrative_ontology:measurement(tenu_tr_t2024, tenure_contract__academic_freedom_reading, theater_ratio, 2024, 0.12).

% Extraction over time
narrative_ontology:measurement(tenu_be_t1990, tenure_contract__academic_freedom_reading, base_extractiveness, 1990, 0.22).
narrative_ontology:measurement(tenu_be_t2000, tenure_contract__academic_freedom_reading, base_extractiveness, 2000, 0.24).
narrative_ontology:measurement(tenu_be_t2008, tenure_contract__academic_freedom_reading, base_extractiveness, 2008, 0.26).
narrative_ontology:measurement(tenu_be_t2016, tenure_contract__academic_freedom_reading, base_extractiveness, 2016, 0.27).
narrative_ontology:measurement(tenu_be_t2024, tenure_contract__academic_freedom_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t1990, tenure_contract__academic_freedom_reading, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(tenu_su_t2000, tenure_contract__academic_freedom_reading, suppression_requirement, 2000, 0.13).
narrative_ontology:measurement(tenu_su_t2008, tenure_contract__academic_freedom_reading, suppression_requirement, 2008, 0.14).
narrative_ontology:measurement(tenu_su_t2016, tenure_contract__academic_freedom_reading, suppression_requirement, 2016, 0.145).
narrative_ontology:measurement(tenu_su_t2024, tenure_contract__academic_freedom_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tenure_contract__academic_freedom_reading, 0.12).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% The tenure_contract kernel decomposes into three constraint stories, each a different reading of the same institutional practice. The academic_freedom_reading isolates tenure's protective function against political pressure on research; the institutional_extraction_reading isolates the employment rigidity and labor-market asymmetry; the demographic_reproduction_reading isolates the gatekeeping mechanisms in peer review. All three share the same referent (the tenure system) but measure different ε values because they prioritize different aspects of the system's operation and different beneficiary/victim structures. The readings coexist as live positions held by different institutional constituencies and remain mutually unconcealed (coexists_with relations) because the tenure system genuinely produces both protection (for favored research) and extraction (for labor markets) simultaneously. Policymakers must arbitrate between them; the framework models the structural dispute, not the resolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__academic_freedom_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
