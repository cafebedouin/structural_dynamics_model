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
 *   human_readable: Tenure as Institutional Rent Extraction and Employment Rigidity
 *   domain: higher_education/labor_economics/institutional_governance
 *
 * SUMMARY:
 *   This constraint instantiates the institutional-extraction reading of the
 *   tenure kernel. Tenure is presented (and often genuinely functions) as
 *   stabilization of intellectual work against external pressure; this
 *   reading focuses on how tenure operates in practice as a permanent income
 *   claim by early winners, creating institutional rigidity that prevents
 *   resource reallocation and concentrates flexibility costs onto contingent
 *   labor. The constraint structures a two-tier faculty labor market: a small
 *   protected class with near-universal job security, salary growth, research
 *   time, and curricular control; and a large precarious class with
 *   semester-to-semester contracts, minimal benefits, no research time, and
 *   no institutional voice. Students pay tuition that subsidizes tenured
 *   salaries locked into prior hiring decisions; contingent faculty absorb
 *   the flexibility that tenure forecloses. The claim/metric gap is
 *   intentional: tenure is claimed by the institution as a coordination
 *   mechanism (academic freedom protection) and by tenured faculty as a
 *   professional norm; the authored metrics describe substantially
 *   extractive, actively enforced operation where the coordination function
 *   is increasingly separable from the extraction. The engine will measure
 *   this divergence across seats.
 *
 * KEY AGENTS:
 *   - Tenured faculty: Primary beneficiary. Collects permanent income claim, research time, curricular authority, and freedom from termination. Organized seat with high power and exit options (arbitrage — can relocate to peer institutions, private industry, or retirement).
 *   - Contingent faculty: Primary victim. Bears structural flexibility, low and variable compensation, no benefits, teaching-only load. Powerless seat with trapped exit (career path dependence within academia, no outside market for their specialized training).
 *   - Students: Secondary victim. Pay tuition that funds tenured salary lines; receive reduced mentorship from tenured faculty who have reduced teaching load; face tuition growth driven by fixed tenure commitments.
 *   - Administrators: Agenda-setter managing budget under tenured salary constraints. Enforce tenure rules procedurally while adapting to constraint through tuition increases and contingent hiring expansion.
 *   - Emerging fields (interdisciplinary, data science, climate): Excluded from rapid reallocation of resources; compete against tenure-protected departmental territories for salary lines.
 *   - State legislatures: Excluded institutional actor; possess statutory authority but face political opposition from organized tenured faculty and academic unions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.81).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.72).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Institutional Rent Extraction and Employment Rigidity").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education/labor_economics/institutional_governance").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac').
narrative_ontology:cs_kernel_codification('9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac', formalized).
narrative_ontology:cs_authority_grounding('9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac', extraction).
narrative_ontology:cs_interpretation_layer_present('9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac').
narrative_ontology:cs_reading_relation('9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac', tenure_contract__demographic_reproduction_reading, influences).
narrative_ontology:cs_axiom('9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac', foundational, permanent_income_claim_is_rent).
narrative_ontology:cs_axiom_status(permanent_income_claim_is_rent, holdable).
narrative_ontology:cs_axiom_grounding('9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac', permanent_income_claim_is_rent, empirically_contingent).
narrative_ontology:cs_axiom('9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac', foundational, allocation_rigidity_concentrates_costs_onto_precariat).
narrative_ontology:cs_axiom_status(allocation_rigidity_concentrates_costs_onto_precariat, holdable).
narrative_ontology:cs_axiom_grounding('9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac', allocation_rigidity_concentrates_costs_onto_precariat, empirically_contingent).
narrative_ontology:cs_reference_frame('9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac', academic_independence_via_permanent_contract).
narrative_ontology:cs_drift_state('9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac', contemporary_higher_ed_precarity_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9f3dbbe9-b43b-4c55-b74d-6f83f3dfc2ac', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent contracts with near-universal job security and reputational capture of institutional resources. Set curriculum priorities, hiring criteria, and departmental resource allocation; protect tenure status through collegial peer review that prioritizes cultural fit and seniority preservation. Collect stable compensation, research funding, and deference across their career span while institutional constraints prevent redeployment of their salary lines to emerging fields or contingent positions.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, tenured_faculty, agenda_setter).

% Bear the structural flexibility that tenured positions foreclose: semester-to-semester contracts, no benefits, no research time, teaching-only loads with preparation across multiple courses. Absorb enrollment volatility and budget cuts that tenured budgets insulate. Generate the same instructional value as tenured faculty but accumulate no security, career continuity, or institutional voice. Exit options are constrained to adjunct networks within academia (low pay, geographic mobility required) or exit the profession entirely; the tenured structure actively prevents salary-line conversion to stable contingent positions.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    powerless, immediate, trapped, national).

% Pay tuition that subsidizes tenured faculty salary lines held constant across enrollment cycles, receiving instruction from a two-tier faculty labor market in which the junior tier (contingent) carries pedagogical load while senior tier (tenured) controls curriculum, grading standards, and degree requirements. Receive less research mentorship than prior cohorts in fields where tenured faculty have reduced course load while retaining salary. Face tuition growth driven partly by fixed tenured compensation and partly by institutional expansion of administrative burden to manage the rigidity.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students, payer,
    moderate, biographical, constrained, national).

% Manage the institutional budget subject to tenured salary commitments that cannot be reallocated without cause or consent. Respond to enrollment decline by increasing tuition or expanding contingent hiring (transferring flexibility burden to contingent tier) rather than adjusting tenured headcount. Enforce tenure rules through formal grievance and dismissal procedures that create legal and reputational risk if challenged. Must maintain the procedural legitimacy of tenure peer review even as its gatekeeping function diverges from merit.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, administrators, agenda_setter,
    institutional, generational, constrained, national).

% Fields requiring rapid intellectual reorientation (data science, climate adaptation, computational humanities) cannot recruit aggressively into tenure because existing tenure lines in slower-moving fields are locked. Interdisciplinary work competes for resources against tenure-protected departmental territories. Would benefit from flexible reallocation of salary lines across fields; this flexibility is structurally unavailable under tenure.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, emerging_fields_excluded, excluded,
    moderate, biographical, constrained, national).

% Face identical tenure constraints, producing isomorphic employment structures and institutional rigidity across the sector. Collectively, the peer set ratifies tenure norms through accreditation standards and hiring expectations, making individual exit costly; coordinated reform is blocked by collective action problems and the power of tenured faculty within each institution.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, peer_institutions, observer,
    institutional, generational, analytical, national).

% Have statutory authority to regulate tenure at public universities but face organized political opposition from tenured faculty and academic unions. Reduced appropriations force universities to increase tuition and shift costs to contingent labor; legislatures lack political capital to mandate tenure reform and instead allow institutional adaptation through contingentization.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, state_legislatures, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Tenure solves a coordination problem: insulating research and teaching from short-term political, market, or administrative pressure, enabling intellectual work to proceed on decade-scale timelines. A tenured scholar can undertake a ten-year research program without fear of termination for unpopular findings or methodological controversy.
% TRANSFER_FUNCTION: Moves permanent employment status, seniority-weighted salary, research time, and curricular authority from the broader institutional budget to tenured faculty, extracting flexibility from contingent labor (who bear hiring/firing volatility) and from students (who pay tuition funding salary lines that cannot be redeployed). The transfer is defended as the price of academic freedom; it operates as a permanent first-mover advantage for early cohorts.
% ABSENT_VOICES: Contingent faculty have minimal formal voice in tenure decisions and institutional governance; they participate in teaching committees but lack voting power on departmental budget or hiring. Students have voice through representation channels but no direct seat at tenure deliberation. Emerging fields lack institutional seats and compete for resources against tenure-protected departments. Prospective faculty who will never enter the institution (displaced by the salary structure) are absent entirely.
% DISAPPEARANCE_RATIONALE: If tenure as a permanent contract disappeared, universities would reallocate salary lines across fields, shift compensation from flat-scale tenure to performance-modulated contracts, hire aggressively into growing fields, and reduce tuition pressure by deploying labor more flexibly. Contingent faculty would face even greater precarity in the short term (loss of tenure's institutional constraint on their working conditions) but would over time benefit from salary-line conversion; students would face lower tuition if institutions could reallocate rather than lock resources into protected salaries.
% FOUNDING_PROBLEM: Before tenure became institutionalized (early-to-mid 20th century), scholars were dismissed for political speech, controversial research findings, or institutional displeasure; intellectual inquiry was subordinated to institutional or donor preference. Tenure was established to protect the scholar's independence from retaliation.
% FOUNDING_PROBLEM_CORROBORATION: Tenured faculty and academic freedom advocates attest the founding problem remains live: institutional and political pressure on research continues globally, and tenure remains necessary protection. Contingent faculty, administrators, and economists attest the founding problem is substantially solved within the tenured tier but has been solved by shifting precarity to contingent faculty rather than by establishing universal employment security; legislative testimony and peer-reviewed labor economics research from outside the benefiting parties support that tenure's protective function could be separated from its permanent-contract form (e.g., multi-year renewable contracts with procedural dismissal protections achieve stability without permanent lock).
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.81) and rising over the interval because the constraint's primary function is now income protection rather than intellectual freedom protection. Early tenure adoption (1920s-1970s) served genuine coordination (protecting scholars from retaliation); as tenure became universal and competition for resources intensified, the constraint's function shifted to permanent income preservation regardless of research output or teaching quality. Suppression is substantial (0.72) because the constraint's persistence depends on actively excluding alternative employment forms: institutions maintain single-tier hiring bans, prevent conversion of tenured salary lines to multi-year renewable contracts or contingent positions, and enforce peer-review gatekeeping that privileges existing tenured faculty composition. Theater is moderate (0.48) and rising: the stated function is academic freedom, but empirical audits show tenure dismissal for cause is extraordinarily rare (~0.2% per year across all sectors), suggesting performative maintenance of the academic freedom rationale while extraction operates through income lock and gatekeeping. The measurement series show extractiveness rising sharply in the first 15 years (0.68→0.78, as enrollment stagnates and institutions adapt by increasing contingent hiring rather than tenure reallocation) then plateauing (0.78→0.81 over the final 20 years) — the system reaches an equilibrium where contingent hiring absorbs all adjustment pressure. Suppression rises in parallel (institutional enforcement of tenure rules increases as challenges mount) before plateauing. Theater rises throughout (rising ratio of stated rationale to actual function) and continues rising even after extractiveness plateaus, suggesting the constraint is becoming increasingly theatrical — the performance of academic freedom justification persists as the actual coordination function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   Tenured faculty and administrators perceive tenure as a settled institutional norm justified by academic freedom; from this seat, challenges to tenure are attacks on intellectual freedom itself. Contingent faculty perceive tenure as institutional theft of their labor flexibility and career stability, justified by cover stories about permanent commitment that do not apply to them. Students perceive tenure as opaque cost imposed through tuition, justified by institutional claims they cannot verify. The engine will compute different effective extraction (χ) for each seat: tenured faculty sit at d≈0.1 (full beneficiary) making χ highly negative (subsidy rather than extraction); contingent faculty sit at d≈0.95 (full target) making χ near maximal given the base ε; students sit at d≈0.65 (partial target) with moderate χ. Administrators sit near d≈0.5 (forced symmetric burden: they manage the constraint's ripple effects across the institution but do not capture its extraction). The divergence across seats is the structural story the constraint tells.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations plus power and exit differentiation. Tenured faculty benefit directly (they collect the permanent income claim, controlling its terms via peer review) and hold high power (organized institutional seat) and arbitrage exit (can move to other universities, retire, or leave academia entirely). This places them near d=0.0 (full beneficiary), making their effective extraction χ highly negative—they are subsidized by the constraint. Contingent faculty are declared victims: they bear the flexibility costs, hold low power (unorganized, distributed across institutions and precarious positions), and face trapped exit (specialized training, career path dependence, family location constraints). This places them near d=1.0 (full target), making their effective extraction χ close to the base ε. Students are secondary victims: they pay tuition funding the salary lock, hold moderate power (some collective voice through student representation, but limited institutional voting), and face constrained exit (geographic and financial factors limit switching between institutions mid-degree). This places them near d=0.65-0.75, producing moderate χ. Administrators are forced symmetric: they manage budget under constraints they do not set, facing pressure from both tenured faculty (who defend tenure) and students/contingent faculty (who bear costs); their power is institutional but their exit is constrained (administrative positions are tied to the institution and sensitive to faculty opposition). This places them near d≈0.50. The engine derives d automatically from these structural positions; the commentary documents the reasoning so the per-seat type computations are auditable.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint satisfies the tangled_rope gate: it has beneficiaries (tenured faculty who collect directly), victims (contingent faculty and students who bear costs), and requires active enforcement (tenure peer review, salary-line protection, hiring bans). The coordination function (protecting intellectual work from short-term pressure) is real and remains live for the tenured tier, but the transfer function (permanent income lock) has become the primary extractive mechanism. Mandatrophy (gap between founding problem and current function) is substantial but not terminal: the founding problem (political retaliation against researchers) has been partially solved but not eliminated globally; it persists in some contexts (authoritarian regimes, politically captured institutions) while being solved in others (peer-review communities, professional associations with independence norms). The constraint persists in its original form even where the founding problem is solved, suggesting the form serves functions beyond the founding problem (income protection, demographic gatekeeping). The institutional-extraction reading prevents mislabeling this as pure rope (which it no longer is in most high-resource institutions) or as pure snare (the coordination function remains real for the tenured tier, distinguishing it from extraction mechanisms like debt peonage that lack coordination). Tangled rope correctly captures the hybrid: coordination in form, extraction in operation, requiring active enforcement to maintain the gatekeeping that prevents structural reallocation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_separability,
    'Is intellectual-freedom protection structurally inseparable from permanent employment contracts, or could multi-year renewable contracts with procedural dismissal protections provide equivalent stabilization without permanent lock?',
    'Comparative institutional analysis: examine peer-review systems in non-tenure-track institutions (research institutes, European universities with renewable contracts) and measure research independence outcomes against tenured institutions.',
    'If protection and permanence are separable, the measured extraction is partly the price of coordination (necessary) and partly pure rent-seeking (contingent on institutional choice). The constraint would decompose into two: a protection contract (lower extraction, genuine coordination) and an employment rigidity mechanism (higher extraction, pure lock). This reading would shift from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_separability, empirical, 'Whether academic freedom requires permanent contracts or only procedural employment security.').

omega_variable(
    suppression_structure_vs_internalization,
    'How much of contingent faculty''s suppression is structural (economic dependency, hiring rules, geographic constraints) versus internalized (professional identity, aspiration to tenure, belief in temporary status)?',
    'Post-exit interviews with contingent faculty who leave academia: does suppression persist after structural exit? Are career choices reorganized or do identity constraints remain?',
    'If suppression is primarily structural, contingent faculty''s measured d approaches 1.0 (full target) from external constraint alone; if substantially internalized, the same d is reached through self-reinforcing belief patterns that persist even after structural exit. In the first case, structural reform (salary-line conversion, contract securitization) would yield rapid improvement; in the second, reform alone would not eliminate suppression from the population.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structure_vs_internalization, empirical, 'The composition of contingent faculty suppression between structural and internalized mechanisms.').

omega_variable(
    founding_problem_persistence_and_scope,
    'For which institutional and geographic contexts is the founding problem (researcher retaliation for political or methodological speech) still live, and for which contexts is it substantially solved?',
    'Audit of researcher protections across sectors and countries: measure political/administrative retaliation rates, legal remedies, and peer-review independence in contexts with and without tenure.',
    'If the founding problem is solved in high-resource, peer-review-governed institutions but remains live in authoritarian or politically captured contexts, tenure''s justification map is context-dependent. The institutional_extraction reading would be most accurate for the solved contexts; the academic_freedom_reading would be most accurate for the unsolved contexts. This would imply tenure reform should be differentiated by institutional context, not universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence_and_scope, empirical, 'Geographic and sectoral variation in the persistence of tenure''s founding problem.').

omega_variable(
    alternative_extraction_mechanisms,
    'If tenure were abolished and replaced with renewable multi-year contracts, what extraction mechanisms would replace it? Would contingent labor costs simply shift from precarity to within-contract flexibility demands?',
    'Natural experiment from jurisdictions that abolished tenure or transitioned to contract-based systems; measure labor precarity, wage stability, and resource allocation outcomes over 10-year windows.',
    'If tenure abolition simply relocates extraction (from permanent lock to contract renegotiation vulnerability), the constraint is not so much removed as transformed. The institutional_extraction reading assumes permanent lock is the primary extraction mechanism; if lock were removed and extraction persisted through contracts, the reading would need refinement—the extraction would be shown as intrinsic to academic labor markets rather than contingent on tenure form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_extraction_mechanisms, empirical, 'Whether tenure abolition eliminates extraction or transforms its mechanism.').

omega_variable(
    reading_contest_foreclosure,
    'Does the institutional_extraction reading logically foreclose the academic_freedom_reading, or do they coexist as readings held by different institutional constituencies?',
    'Discourse analysis: do institutional defenders of tenure read themselves as defending academic freedom, or do they acknowledge extraction and defend it anyway? Can both readings be held simultaneously by the same actor?',
    'If readings foreclose each other (cannot both be held in one institution), then resolving which is true would require empirical adjudication and one would be vindicated. If they coexist (different constituencies hold different readings as live positions), then both are correct from their respective seats and conflict is fundamentally political rather than empirical. This affects whether the constraint is fundamentally contestable or fundamentally resolvable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure, conceptual, 'Whether the institutional_extraction and academic_freedom readings are logically foreclosed or coexisting positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(tenu_tr_t5, tenure_contract__institutional_extraction_reading, theater_ratio, 5, 0.36).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__institutional_extraction_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(tenu_tr_t15, tenure_contract__institutional_extraction_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement(tenu_tr_t25, tenure_contract__institutional_extraction_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(tenu_tr_t35, tenure_contract__institutional_extraction_reading, theater_ratio, 35, 0.48).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(tenu_be_t5, tenure_contract__institutional_extraction_reading, base_extractiveness, 5, 0.71).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__institutional_extraction_reading, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(tenu_be_t15, tenure_contract__institutional_extraction_reading, base_extractiveness, 15, 0.78).
narrative_ontology:measurement(tenu_be_t25, tenure_contract__institutional_extraction_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(tenu_be_t35, tenure_contract__institutional_extraction_reading, base_extractiveness, 35, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(tenu_su_t5, tenure_contract__institutional_extraction_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__institutional_extraction_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(tenu_su_t15, tenure_contract__institutional_extraction_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(tenu_su_t25, tenure_contract__institutional_extraction_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(tenu_su_t35, tenure_contract__institutional_extraction_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tenure_contract__institutional_extraction_reading, 0.18).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__demographic_reproduction_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, contingent_labor_precarity__institutional_boundary).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tuition_cost_escalation__institutional_budget_lock).

% DUAL FORMULATION NOTE:
% The tenure kernel decomposes into three constraint stories, each instantiating a different reading: academic_freedom_reading (coordination-dominant, low extraction), demographic_reproduction_reading (gatekeeping-dominant, asymmetric extraction), and institutional_extraction_reading (rent-extraction-dominant, high extraction). All three share the formal referent (the tenure contract) but have different ε values and beneficiary/victim structures. They are linked as siblings in a constraint family via network.affects_constraints edges. This story (institutional_extraction_reading) influences the sibling stories by establishing the resource-lock claim that both academic_freedom and demographic_reproduction readings must account for in their own ε derivations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
