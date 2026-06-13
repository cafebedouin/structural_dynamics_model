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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   The academic tenure system, under this reading, is a coordination
 *   mechanism that solves a genuine alignment problem: enabling truth-seeking
 *   (which is costly, slow, and politically risky) by decoupling researcher
 *   survival from institutional displeasure. Tenured faculty are freed to
 *   pursue research agendas driven by epistemic standards rather than
 *   institutional reputation management. This reading claims tenure is
 *   fundamentally a public-goods coordination mechanism producing high-value
 *   research. It is one reading of a contested kernel — other readings frame
 *   tenure as demographic gatekeeping (see demographic_reproduction_reading)
 *   or as permanent rent extraction by early winners (see
 *   institutional_extraction_reading). This story instantiates ONLY the
 *   academic freedom reading and captures its structural logic: how tenure
 *   benefits faculty and the knowledge ecosystem by increasing the cost for
 *   institutional actors and external political actors to suppress unwelcome
 *   scholarship.
 *
 * KEY AGENTS:
 *   - Tenured faculty: primary beneficiaries; receive employment security and research autonomy decoupled from institutional approval
 *   - Research students: secondary beneficiaries; access to advisors with freedom to pursue honest research agendas
 *   - Knowledge ecosystem (non-agent): vindicated proposition; the disciplinary and epistemic systems benefit from protected high-risk inquiry
 *   - Institutional administrators: structural payers; lose control over research agendas and cannot use termination to suppress scholarship
 *   - External political actors: excluded from suppression lever; cannot demand institutional firing of inconvenient scholars
 *   - Contingent academic labor: excluded from protection; operate under survival-dependent contracts that incentivize self-censorship
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
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Tenure as Academic Freedom Protection").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher_education/labor_economics/institutional_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '091a2439-bda6-420a-8d94-0162b4009dce').
narrative_ontology:cs_kernel_codification('091a2439-bda6-420a-8d94-0162b4009dce', fixed_text).
narrative_ontology:cs_authority_grounding('091a2439-bda6-420a-8d94-0162b4009dce', lineage).
narrative_ontology:cs_interpretation_layer_present('091a2439-bda6-420a-8d94-0162b4009dce').
narrative_ontology:cs_reading_relation('091a2439-bda6-420a-8d94-0162b4009dce', tenure_contract__institutional_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('091a2439-bda6-420a-8d94-0162b4009dce', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('091a2439-bda6-420a-8d94-0162b4009dce', foundational, research_freedom_enables_truth_seeking).
narrative_ontology:cs_axiom_status(research_freedom_enables_truth_seeking, holdable).
narrative_ontology:cs_axiom_grounding('091a2439-bda6-420a-8d94-0162b4009dce', research_freedom_enables_truth_seeking, instrumental).
narrative_ontology:cs_axiom('091a2439-bda6-420a-8d94-0162b4009dce', foundational, institutional_suppression_of_research_is_empirically_documented).
narrative_ontology:cs_axiom_status(institutional_suppression_of_research_is_empirically_documented, holdable).
narrative_ontology:cs_axiom_grounding('091a2439-bda6-420a-8d94-0162b4009dce', institutional_suppression_of_research_is_empirically_documented, empirically_contingent).
narrative_ontology:cs_reference_frame('091a2439-bda6-420a-8d94-0162b4009dce', academic_freedom_protection).
narrative_ontology:cs_drift_state('091a2439-bda6-420a-8d94-0162b4009dce', contemporary_neoliberal_university, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('091a2439-bda6-420a-8d94-0162b4009dce', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, research_students).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, knowledge_ecosystem).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, institutional_administrators).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, academic_freedom_as_public_good).
narrative_ontology:constraint_vindicates(tenure_contract__academic_freedom_reading, institutional_autonomy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive permanent employment with freedom to pursue research directions that may offend administrators, political actors, or disciplinary orthodoxies without fear of dismissal or demotion. Exit options are exceptional: tenured positions are rare, but once acquired, the exit cost to leave academia entirely is the primary constraint on mobility, not job security within it. The arrangement decouples career survival from institutional approval.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    institutional, generational, arbitrage, national).

% Benefit from advisors with the freedom to pursue intellectually honest research agendas rather than agendas shaped by political pressure or institutional reputation management. Their training depends on access to truth-seeking rather than theater-optimized inquiry.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, research_students, beneficiary,
    powerless, biographical, constrained, national).

% The collective system of disciplines, journals, and citation networks benefits from the production of intellectually challenging, controversial, and politically unwelcome research that would be suppressed in a system where academic survival depended on palatability.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, knowledge_ecosystem, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(tenure_contract__academic_freedom_reading, knowledge_ecosystem).

% Bear the constraint of not being able to dismiss faculty whose research embarrasses the institution, attracts legal liability, or alienates donors. The constraint prevents administrative control over the research agenda; administrators cannot use employment termination as a tool to shape institutional messaging or reputation.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, institutional_administrators, payer,
    institutional, generational, constrained, national).

% Are structurally unable to suppress research by demanding the university fire inconvenient scholars. Their leverage is the threat of defunding, public attack, or legal action; tenure insulates the researcher from these external pressures being converted into employment consequences. They would prefer the ability to suppress scholarship through institutional pressure rather than public discourse.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, external_political_actors, excluded,
    powerful, biographical, trapped, global).

% Are excluded from tenure protections and operate under survival-dependent contract renewal, meaning they self-censor research to avoid offense and manage institutional relationships for re-employment. Their exclusion is a structural precondition of the tenure system's isolation of protected faculty from institutional pressure.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, contingent_academic_labor, excluded,
    powerless, immediate, trapped, national).

% The collective epistemic standards of a field or discipline. Tenure allows individual researchers to challenge consensus; disciplines with weaker consensus mechanisms may experience tenure-enabled fragmentation or epistemic chaos. Disciplines with strong external political pressure benefit most from tenure's protection.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, disciplinary_consensus, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(tenure_contract__academic_freedom_reading, disciplinary_consensus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__academic_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the alignment problem between truth-seeking (high-risk, politically contentious, slow-to-reward) and institutional survival (reputation-sensitive, donor-responsive, reputationally brittle). Tenure decouples these: faculty survive regardless of whether their research is politically pleasing, enabling pursuit of research whose value is epistemic rather than institutional-PR-positive.
% TRANSFER_FUNCTION: Moves employment security and research freedom FROM institutional administrators and external political actors TO tenured faculty and, derivatively, to their students and the knowledge ecosystem. The constraint prevents institutional actors from using termination as a tool to suppress scholarship.
% ABSENT_VOICES: Contingent academic labor (adjuncts, postdocs, contract instructors) would object that tenure benefits a protected elite while leaving precarious workers undefended and creating pressure on the institution to hire them cheap and fire them fast. External political actors (donors, elected officials, social movements) would object that research becomes insulated from accountability to the publics that fund or live with its consequences.
% DISAPPEARANCE_RATIONALE: If tenure protections vanished overnight, faculty would immediately self-censor research with political risk; hiring would shift toward compliance-ready scholars; research agendas would realign with institutional and donor preferences; politically controversial findings would not be published from universities; the cost to institutional reputation of hosting unpopular scholarship would vanish. The knowledge ecosystem would reorganize around palatability to institutional and political actors.
% FOUNDING_PROBLEM: Academic research requires freedom from institutional punishment for findings and arguments that threaten institutional reputation, donor relationships, or political support. Without this freedom, research agendas optimize for institutional approval rather than truth-seeking.
% FOUNDING_PROBLEM_CORROBORATION: Historians of academia document the founding problem across the 20th century: cases like the Scopes trial, McCarthy-era purges, civil-rights-era firings of southern scholars, and post-9/11 dismissals of Middle East scholars all exemplify the founding problem. Academic freedom organizations (AAUP, PEN America) attest the problem is live through contemporary cases. External actors do not corroborate the problem; they dispute its existence, arguing that accountability to institutional mission should supersede researcher autonomy.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).

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
 *   Extractiveness is LOW (0.28 at interval end) because tenure's primary function is coordination (solving an alignment problem between truth-seeking and survival), not extraction. The constraint benefits faculty, students, and the knowledge ecosystem more than it harms administrators. Suppression is VERY LOW (0.15) because the constraint operates via positive incentive (employment security) rather than coercion; it enables rather than forbids. Theater ratio is LOW (0.12) because the research productivity that justifies tenure is real, not performative — the constraint's function is genuine. Accessibility collapse is HIGH (0.72) because once faculty understand tenure's value for research freedom, non-tenure-track alternatives (contingent labor) become unattractive; tenure-track positions have become the natural aspiration in academia. Resistance is MODERATE (0.38) because the constraint faces real pushback from administrators (budget pressure, loss of control) and external actors (donors, elected officials) without meeting equivalent countervailing force from contingent labor, whose exclusion removes potential coalition partners. The measurement series shows slight extractiveness drift (0.18→0.28) over the 80-year interval reflecting: (1) gradual reduction of research freedom as universities compete for reputation and donor approval, (2) rising administrative density and compliance burden even on tenured faculty, (3) increasing institutional risk aversion. This drift is modest because tenure's core structure has not changed; the erosion is slow.
 *
 * PERSPECTIVAL GAP:
 *   This reading should compute very differently at different seats. From the tenured faculty seat: tenure is low-extraction coordination yielding enormous research freedom — this reading captures their experience and produces a rope classification. From the institutional administrator seat: tenure is a constraint limiting control over hiring and research direction, a cost of institutional autonomy — extractiveness rises, suppression shifts from the researchers being suppressed to the administrators being constrained. From the external political actor seat (donor, elected official, partisan): tenure is an obstacle to accountability, enabling the university to shield researchers from public pressure — high extractiveness, high suppression of the external actor's preferred outcomes. From the contingent labor seat: tenure is a benefit to others that, by creating a protected class, intensifies pressure on the unprotected — the apparent beneficiary structure for tenured faculty is purchased partly through precarity of non-tenured workers. The engine computes directionality from beneficiary/victim declarations and exit options; this reading's structural data emphasizes the beneficiary side (tenured faculty, students, knowledge) and the payer side (administrators lose control). An institutional_extraction_reading would swap the beneficiary/victim declarations and produce a snare classification at the same sites.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty: d near 0.0 (beneficiary). They receive employment security and research freedom; their exit options improve dramatically because tenure eliminates the reputational risk of controversial research. Research students: d near 0.1 (secondary beneficiary). They benefit from advisor autonomy but are powerless and subject to advisor preferences; their time horizon is shorter (biographical vs. generational). Institutional administrators: d near 0.6 (partial target). They lose the ability to use termination as a control mechanism; their time horizon aligns with tenure's (generational), so they experience the constraint's costs across their careers. External political actors: d near 0.9 (near-full target). They are excluded from the suppression lever tenure removes; their preferences are most directly opposed by the constraint. Contingent labor: d near 0.7 (target). They are excluded from tenure's benefits and experience intensified precarity as a structural consequence of tenure's existence (the institution uses contingent labor to offset tenure's labor costs). Knowledge ecosystem (non-agent, vindicated proposition): no d value. Disciplinary consensus: d near 0.3 (partial beneficiary). Strong consensus can be challenged by tenure-protected scholars; weak consensus can be destabilized. This reading assumes consensus benefits from contestation; an institutional_extraction_reading would assume it suffers.
 *
 * MANDATROPHY ANALYSIS:
 *   Tenure's founding problem (enabling truth-seeking against institutional and political pressure) is LIVE under this reading. The constraint's classification as rope is consistent with a live founding problem — the coordination function persists because the alignment problem persists (research is still costly, slow, and risky; institutional pressure still exists). Under a dead-founding-problem reading, tenure would appear as inertial performance (a piton) — a zombie institution maintained for tradition after the external pressure that justified it has subsided. This reading does not claim that founding problem; it claims the founding problem is live, which is why rope classification is coherent. Under the institutional_extraction_reading (sibling), the founding problem would be classified as dead (institutional rigidity and labor precarity are the costs; suppression of external political pressure is the window dressing), and tenure would compute as piton or snare. The mandatrophy question—whether tenure is coordination or extraction—is the kernel contest itself. This reading resolves it in favor of coordination by emphasizing the genuine research-freedom benefit and the cost to legitimate institutional control. An omega variable addresses the alternative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Is the founding problem (institutional and political pressure to suppress research) actually live, or has it substantially subsided and tenure persists as inertial performance?',
    'Historical analysis of attempted suppression incidents: measure the frequency, severity, and institutional origin of pressure to suppress scholarship over decades. Compare suppression pressure against tenure-removal attempts; if suppression attempts have declined, the founding problem has attenuated.',
    'If the founding problem is dead, tenure shifts from rope (coordination mechanism) toward piton (inertial performance) or snare (rent extraction). If live, rope classification is coherent and the constraint''s persistence is justified. This is the key disagreement with the institutional_extraction_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem tenure was designed to solve remains active or has become obsolete.').

omega_variable(
    contingent_labor_causation,
    'Does tenure''s existence structurally cause the expansion of contingent academic labor, or is contingent labor expansion driven by independent budget pressures and would occur regardless?',
    'Counterfactual institutional analysis: compare universities that abolished tenure with those retaining it, controlling for budget constraints and enrollment patterns. If contingent labor expanded equally in both groups, tenure did not cause it. If contingent expansion was slower in tenure-retaining institutions, tenure may have cushioned the shift.',
    'If tenure causes contingent expansion, the beneficiary structure shifts: tenured faculty benefit from contingency of others (at least partly), making them partial extractors; the classification remains rope but with extraction asymmetry. If contingent expansion is independent, tenure is more purely coordinating and does not trade one group''s protection for another''s precarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingent_labor_causation, empirical, 'Whether tenure''s existence is causally responsible for contingent labor expansion or if the two trends are independent.').

omega_variable(
    political_suppression_asymmetry,
    'Does tenure equally protect scholarship across political and ideological lines, or does it selectively protect scholars whose findings align with dominant institutional/disciplinary commitments while leaving others exposed?',
    'Comparative analysis of tenure outcomes and suppression incidents by field and political orientation. Track both termination attempts and peer-review gatekeeping by disciplinary politics. If protection is asymmetric (e.g., tenure protects scholars in favored fields but fails to protect contrarian voices), tenure operates partly as demographic gatekeeping (see demographic_reproduction_reading).',
    'Asymmetric protection would undermine the academic_freedom_reading''s claim that tenure solves the alignment problem universally. Instead, tenure would solve it for mainstream scholars while contingent-ifying contrarians, which is extraction dressed as freedom.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_suppression_asymmetry, empirical, 'Whether tenure''s protection is uniformly extended across political and epistemological positions or is selective.').

omega_variable(
    sibling_reading_alternative_framing,
    'Which reading of the tenure contract kernel is structurally true: academic_freedom (this reading), institutional_extraction, or demographic_reproduction?',
    'None—this is a committer-frame omega documenting the kernel contest itself. The readings coexist as live positions. Resolution is available only through policy choice (abolish, reform, or defend tenure) and subsequent observation of outcomes.',
    'The reading choice determines ε (and thus type classification) across all seats. Academic_freedom_reading produces low ε (coordination-heavy); institutional_extraction_reading produces high ε (extraction-heavy); demographic_reproduction_reading emphasizes gatekeeping function. The omega documents that this choice is irreducible—no empirical observation can definitively resolve which reading is ''correct'' because the readings are alternative structural analyses of the same institutional arrangement, not different measurements of the same constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_alternative_framing, conceptual, 'The kernel contest is irreducible: alternative structural readings of tenure that coexist in policy discourse and institutional practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 80).

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
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__academic_freedom_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement_basis(tenu_tr_t40, observed).
narrative_ontology:measurement(tenu_tr_t50, tenure_contract__academic_freedom_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement_basis(tenu_tr_t50, observed).
narrative_ontology:measurement(tenu_tr_t60, tenure_contract__academic_freedom_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement_basis(tenu_tr_t60, observed).
narrative_ontology:measurement(tenu_tr_t80, tenure_contract__academic_freedom_reading, theater_ratio, 80, 0.12).
narrative_ontology:measurement_basis(tenu_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(tenu_be_t0, observed).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__academic_freedom_reading, base_extractiveness, 10, 0.2).
narrative_ontology:measurement_basis(tenu_be_t10, observed).
narrative_ontology:measurement(tenu_be_t20, tenure_contract__academic_freedom_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement_basis(tenu_be_t20, observed).
narrative_ontology:measurement(tenu_be_t30, tenure_contract__academic_freedom_reading, base_extractiveness, 30, 0.26).
narrative_ontology:measurement_basis(tenu_be_t30, observed).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__academic_freedom_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement_basis(tenu_be_t40, observed).
narrative_ontology:measurement(tenu_be_t50, tenure_contract__academic_freedom_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement_basis(tenu_be_t50, observed).
narrative_ontology:measurement(tenu_be_t60, tenure_contract__academic_freedom_reading, base_extractiveness, 60, 0.28).
narrative_ontology:measurement_basis(tenu_be_t60, observed).
narrative_ontology:measurement(tenu_be_t80, tenure_contract__academic_freedom_reading, base_extractiveness, 80, 0.28).
narrative_ontology:measurement_basis(tenu_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(tenu_su_t0, observed).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__academic_freedom_reading, suppression_requirement, 10, 0.13).
narrative_ontology:measurement_basis(tenu_su_t10, observed).
narrative_ontology:measurement(tenu_su_t20, tenure_contract__academic_freedom_reading, suppression_requirement, 20, 0.14).
narrative_ontology:measurement_basis(tenu_su_t20, observed).
narrative_ontology:measurement(tenu_su_t30, tenure_contract__academic_freedom_reading, suppression_requirement, 30, 0.14).
narrative_ontology:measurement_basis(tenu_su_t30, observed).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__academic_freedom_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement_basis(tenu_su_t40, observed).
narrative_ontology:measurement(tenu_su_t50, tenure_contract__academic_freedom_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement_basis(tenu_su_t50, observed).
narrative_ontology:measurement(tenu_su_t60, tenure_contract__academic_freedom_reading, suppression_requirement, 60, 0.15).
narrative_ontology:measurement_basis(tenu_su_t60, observed).
narrative_ontology:measurement(tenu_su_t80, tenure_contract__academic_freedom_reading, suppression_requirement, 80, 0.15).
narrative_ontology:measurement_basis(tenu_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__academic_freedom_reading, 0.12).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, tenure_contract__demographic_reproduction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, contingent_labor_precarity).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, institutional_reputation_management).

% DUAL FORMULATION NOTE:
% This story is one reading of the contested tenure-contract kernel. The institutional_extraction_reading and demographic_reproduction_reading are sibling constraints instantiating alternative framings of the same institutional arrangement. The three stories form a constraint family: they share the same institutional kernel (tenure contract) but disagree on its structural function and beneficiary distribution. All three should be generated as separate stories with explicit network links. The academic_freedom_reading emphasizes coordination benefits for faculty; the extraction_reading emphasizes rent capture; the demographic_reproduction_reading emphasizes gatekeeping. See narrative_context for the kernel contest description.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__academic_freedom_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
