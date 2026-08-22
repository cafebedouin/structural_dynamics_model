% ============================================================================
% CONSTRAINT STORY: tenure_contract__institutional_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Tenure as Institutional Rent Extraction: Permanent Resource Claim Loading Costs onto Contingent Labor
 *   domain: higher_education/labor_economics/institutional_theory
 *
 * SUMMARY:
 *   Tenure is a contested institutional kernel — a stabilized commitment
 *   grounded in the principle that researchers need employment security to
 *   pursue truth. This reading instantiates tenure as permanent rent
 *   extraction by early winners: faculty hired into abundant positions lock
 *   in lifetime claims on institutional revenue, which rigidifies budgets and
 *   shifts flexibility costs onto contingent workers and students. The
 *   reading does not deny that tenure provides academic freedom protection;
 *   it asserts that freedom protection is now decoupled from the extraction
 *   function. Tenure persists as institutional practice, reinforced by
 *   faculty governance structures and the legal status of tenure contracts,
 *   while the founding problem (research threats from political pressure) has
 *   substantially diminished in liberal democracies with tenure-independent
 *   free speech law. The measurement series tracks rising extractiveness and
 *   theater ratio over 40 years, indicating extraction has accumulated faster
 *   than its coordination justification.
 *
 * KEY AGENTS:
 *   - Tenured faculty (organized, generational time horizon, arbitrage exit): permanent resource claimants; control tenure review and governance; primary beneficiaries of the constraint.
 *   - Contingent faculty (powerless, biographical horizon, trapped exit): bear flexibility costs; no governance voice; primary victims bearing the adjustment margin.
 *   - Students (moderate power, biographical horizon, constrained exit): pay tuition funding tenure obligations; benefit from research prestige; bear cost through reduced instructional investment and tuition pressure.
 *   - Academic administrators (institutional power, generational horizon, constrained exit): enforce the system but face budget pressure; use contingent hiring to manage margins.
 *   - Boards of trustees (powerful, generational horizon, mobile exit): hold fiduciary authority but face tension between budget control and tenure legitimacy.
 *   - Precarious scholars (powerless, biographical horizon, trapped exit): excluded from positions by the permanent cohort; structurally voiceless.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__institutional_extraction_reading, 0.79).
domain_priors:suppression_score(tenure_contract__institutional_extraction_reading, 0.71).
domain_priors:theater_ratio(tenure_contract__institutional_extraction_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, extractiveness, 0.79).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(tenure_contract__institutional_extraction_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__institutional_extraction_reading, tangled_rope).
narrative_ontology:human_readable(tenure_contract__institutional_extraction_reading, "Tenure as Institutional Rent Extraction: Permanent Resource Claim Loading Costs onto Contingent Labor").
narrative_ontology:topic_domain(tenure_contract__institutional_extraction_reading, "higher_education/labor_economics/institutional_theory").

domain_priors:requires_active_enforcement(tenure_contract__institutional_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__institutional_extraction_reading, '48155c59-3e9d-45ee-934f-600753cb0235').
narrative_ontology:cs_kernel_codification('48155c59-3e9d-45ee-934f-600753cb0235', formalized).
narrative_ontology:cs_authority_grounding('48155c59-3e9d-45ee-934f-600753cb0235', lineage).
narrative_ontology:cs_interpretation_layer_present('48155c59-3e9d-45ee-934f-600753cb0235').
narrative_ontology:cs_reading_relation('48155c59-3e9d-45ee-934f-600753cb0235', tenure_contract__academic_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('48155c59-3e9d-45ee-934f-600753cb0235', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('48155c59-3e9d-45ee-934f-600753cb0235', foundational, permanent_resource_claims_enable_extraction).
narrative_ontology:cs_axiom_status(permanent_resource_claims_enable_extraction, holdable).
narrative_ontology:cs_axiom_grounding('48155c59-3e9d-45ee-934f-600753cb0235', permanent_resource_claims_enable_extraction, empirically_contingent).
narrative_ontology:cs_axiom('48155c59-3e9d-45ee-934f-600753cb0235', foundational, founding_problem_substantially_resolved).
narrative_ontology:cs_axiom_status(founding_problem_substantially_resolved, holdable).
narrative_ontology:cs_axiom_grounding('48155c59-3e9d-45ee-934f-600753cb0235', founding_problem_substantially_resolved, empirically_contingent).
narrative_ontology:cs_reference_frame('48155c59-3e9d-45ee-934f-600753cb0235', tenure_as_research_protection_mechanism).
narrative_ontology:cs_drift_state('48155c59-3e9d-45ee-934f-600753cb0235', contemporary_contingency_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('48155c59-3e9d-45ee-934f-600753cb0235', '').
narrative_ontology:cs_kernel_id(tenure_contract__institutional_extraction_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, contingent_faculty).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tenure_contract__institutional_extraction_reading, students).
narrative_ontology:constraint_victim(tenure_contract__institutional_extraction_reading, boards_of_trustees).
narrative_ontology:constraint_vindicates(tenure_contract__institutional_extraction_reading, institutional_resource_scarcity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent contracts entitling them to seniority-indexed salaries, retirement benefits, and due-process protection against termination except for cause. They control the tenure review process (voting on promotion dossiers) and sit on governance committees that allocate discretionary resources. Their exit options are strong: they can move institutions, sabbaticals, external grants move research off-campus. The constraint provides them a claim on institutional resources that persists regardless of labor market conditions or institutional solvency.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, tenured_faculty, beneficiary,
    organized, generational, arbitrage, national).

% Employed on year-to-year or multi-year limited contracts without due process, security, or governance voice. They teach the same courses and students as tenured faculty but at 40-60% compensation, no health insurance, no retirement contributions, and no job security. They bear the adjustment costs when enrollment fluctuates, budget tightens, or programs restructure: they are the margin of flexibility. Career exit means leaving academia entirely; labor-market arbitrage within academia is blocked by the tenure system itself (every position filled by tenure-track opens requires displacing a contingent worker).
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, contingent_faculty, payer,
    powerless, biographical, trapped, national).

% Pay tuition that funds the salary and benefits of tenured faculty whose permanent claims on revenue make the institution inflexible in deploying resources to instruction. They receive instruction from contingent faculty at lower instructional quality (contingent faculty have less time for preparation, mentoring, research-informed pedagogy due to workload). They benefit from the research productivity and prestige that tenure protects; they bear the cost through tuition that subsidizes tenured salaries and retirement obligations.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, students, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, students, beneficiary).

% Administer the tenure system: they enforce the contracts, conduct tenure reviews, and manage the budget constraints it creates. They face pressure from three directions: tenured faculty who defend the system as freedom protection, contingent faculty and students who bear the costs, and boards of trustees who demand budget control. Administrators cannot unilaterally dismantle tenure (faculty governance and law prevent it), so they manage the margin by expanding contingent hiring, which raises extraction by shifting risk to those with least power.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, academic_administrators, agenda_setter,
    institutional, generational, constrained, national).

% Hold fiduciary authority over institutional assets and budgets. They set compensation levels and tenure policies (within faculty governance constraints) but have delegated day-to-day enforcement to administrators. They benefit from tenure indirectly (it signals academic legitimacy and attracts tenured researchers); they bear the cost when rigid labor expense prevents budget flexibility, requiring them to raise tuition or cut programs. Their exit option is implicit: if tenure makes an institution unsustainable, they can dismantle it (rare, requires faculty agreement) or privatize/convert the institution (slower, last resort).
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, boards_of_trustees, agenda_setter,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__institutional_extraction_reading, boards_of_trustees, payer).

% PhDs and advanced scholars unable to secure even contingent positions because the tenure system creates a permanent, fixed cohort of positions that persist regardless of demographic change or labor-market supply. They are structurally excluded from the institutional conversation about tenure reform; the constraint persists partly by their silence (they have no inside voice, no governance seat).
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, precarious_scholars_outside_academia, excluded,
    powerless, biographical, trapped, global).

% Evaluate institutions against standards that include faculty governance and academic freedom protections that tenure operationalizes. They observe whether tenure serves its stated purposes; they can (rarely) deny accreditation or issue warnings for institutions that hollow out tenure protections through contingent over-hiring, creating a feedback that reinforces the system's persistence.
narrative_ontology:constraint_stakeholder(tenure_contract__institutional_extraction_reading, accreditation_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__institutional_extraction_reading, tenured_faculty).
narrative_ontology:fixing_cost_class(tenure_contract__institutional_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects researchers from institutional retaliation or political backlash by decoupling employment security from leadership approval or ideological compliance. Enables high-risk, controversial, or commercially uninteresting research by guaranteeing income independent of current productivity or institutional favor.
% TRANSFER_FUNCTION: Moves permanent, indexed claims on future institutional revenue from institutional administrators to tenured faculty. In effect, tenure is a compounding annuity: early winners (faculty hired when positions were abundant) receive escalating lifetime salaries, benefits, and retirement obligations that persist regardless of whether their research productivity justifies the cost or whether the institution faces revenue stress. These permanent claims create rigidity that forces costs onto students (tuition to fund obligations) and contingent workers (whose compensation and job security shrink as permanent claims grow).
% ABSENT_VOICES: Precarious PhD holders and early-career scholars excluded from positions entirely; international scholars whose visa sponsorship depends on institutional employment stability, making contingency a permanent trap; future students who will bear the cost of today's tenure commitments; workers in other sectors whose tax dollars may fund public universities but have no voice in governance.
% DISAPPEARANCE_RATIONALE: If tenure disappeared overnight, institutions would immediately shift from permanent to performance-evaluated contracts, restructure salary grids (eliminating seniority escalation), redeploy resources to instruction and technology. Labor reallocation would accelerate: contingent positions would convert or disappear, compensation would equilibrate across career stages. Tuition pressure would ease as institutions gained budget flexibility. The labor market for academic positions would open to new cohorts. Tenured faculty would face immediate income loss and reduced security.
% FOUNDING_PROBLEM: Academic inquiry requires protection from political, commercial, and institutional pressure to conform. Early universities faced threats from church authority and monarch oversight; the founding problem was how to enable scholarship critical of power.
% FOUNDING_PROBLEM_CORROBORATION: Defenders of tenure (academic freedom advocacy organizations, faculty unions) attest the founding problem is live: faculty still face pressure to conform. Critics and economists attest the founding problem was substantially solved by mid-20th century in liberal democracies with free speech law; the present system persists as rent defense by early winners, not protection of inquiry — this reading is attested by labor economists, education administrators, and contingent faculty organizations outside the tenure-holding cohort.
narrative_ontology:disappearance_verdict(tenure_contract__institutional_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__institutional_extraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__institutional_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tenure_contract__institutional_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__institutional_extraction_reading, 0.79, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness reaches 0.79 because the constraint creates permanent, indexed claims on institutional revenue that persist independent of the claimant's productivity, market conditions, or institutional solvency. Tenured faculty receive automatic annual increases, post-retirement health coverage, and pension obligations that compound over decades. These claims are enforced as legal entitlements, not discretionary. Suppression is high (0.71) because the system's persistence depends on active enforcement: contingent hiring is the margin of flexibility, enabled by the constraint's structure; removing contingency would force institutions to either raise tuition, cut programs, or reduce tenured salaries — all politically costly. Theater ratio reaches 0.48 because the stated coordination function (protecting inquiry from institutional pressure) does not match the operational function (providing early winners with lifetime income security). Accreditation reviews claim to evaluate academic freedom, but they do not distinguish between tenure-for-freedom and tenure-as-extraction; faculty governance reviews focus on peer fit and collegiality, not research productivity; teaching quality reviews are applied inconsistently across contingent and tenured tracks. The measurement series shows extractiveness rising from 0.58 to 0.79 as contingent hiring expanded (1980–2020 period), shifting the cost margin onto the least-protected workers while tenured salaries and benefits grew. Theater ratio rises as the coordination narrative becomes detached from actual operation.
 *
 * PERSPECTIVAL GAP:
 *   Tenured faculty (beneficiary seat) perceive the constraint as essential freedom protection and institutional fairness — a collective agreement that protects junior scholars from unfair retaliation and ensures research independence. They do not see extraction; they see reciprocal obligation earned by years of service. From the contingent faculty seat (victim), the same structure is a closed-gate mechanism that reserves permanent, secure employment for a fixed cohort while loading adjustment risk onto those outside that cohort. From the student seat, tenure is an opaque institutional cost driver — they see tuition rising but cannot trace it to tenure obligations. The engine's per-seat computation should show these seats computing different types from the same structural data: tenured faculty compute rope (coordination with minimal coercion from their vantage); contingent faculty compute snare (persistent extraction with blocked exits); students compute tangled_rope (genuine coordination benefit in research prestige paired with extractive tuition costs). The beneficiary seat's exit options (arbitrage) and power level (organized) make the constraint appear more symmetrical than it is to the trapped payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Tenured faculty occupy d ≈ 0.1–0.2: they are primary beneficiaries, have strong arbitrage exits (can move institutions, sabbaticals, external funding), organized power, and long time horizons that allow them to wait out budget crises. Contingent faculty occupy d ≈ 0.85–0.95: they are primary victims, have trapped exits (labor market blocked by tenure structure, visa sponsorship for international scholars, geographic constraints), powerless position, and biographical time horizons that make job insecurity acute. Students occupy d ≈ 0.55–0.65: they are mixed — they benefit from the research prestige and faculty stability that tenure provides, but they bear costs through tuition and reduced instructional quality (contingent faculty teach most sections). The secondary victims are precarious scholars outside academia (excluded, not seated), whose d does not compute here but whose existence validates the extraction: the permanent cohort forecloses positions that would otherwise flow to new cohorts. Administrators occupy d ≈ 0.45: they enforce the constraint but also bear pressure from budget stress and trustee demands; they are neither beneficiaries nor pure victims. Boards occupy d ≈ 0.35: they hold power but face binding legal constraints and faculty governance rights that limit their actual leverage.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting researchers from institutional and political pressure) was live in 1960–1970 when McCarthy-era loyalty oaths and ideological conformity pressures existed. By 2020, the founding problem was substantially dead: First Amendment law, institutional peer review norms, and international academic mobility all provide research protection independent of tenure. A researcher can publish controversial work, move institutions, secure external funding, and maintain research security without tenure. The constraint persists not because the founding problem demands it but because the beneficiary cohort has governance power and the institutions have legal obligation to honor existing contracts. This is mandatrophy: the coordination mandate has outlived its justification, but the constraint remains through institutional inertia and stakeholder entrenchment. The classification as tangled_rope (not snare) depends on the coordination function remaining partially live — some research is still protected by tenure; some students do benefit from tenured faculty prestige and stability. But the ratio has inverted: the extraction function now dominates the coordination function. A snare reading would be defensible if the research protection were purely theatrical and the constraint persisted only by coercion and exit blocking — that may be true empirically, but this reading stops at tangled_rope to preserve the empirical coordinate that some coordination remains real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (protecting researchers from institutional and political pressure) substantially solved by legal and institutional factors independent of tenure, or does tenure remain structurally necessary for research protection in contemporary liberal democracies?',
    'Cross-national comparison of research freedom and innovation outputs in jurisdictions with and without tenure systems; analysis of tenure holders who face no actual institutional pressure (do they exercise the protection tenure provides, or is it performative?); documentation of research threats in non-tenured systems and whether they differ from pre-tenure historical threats.',
    'If the founding problem is solved by legal/cultural factors independent of tenure, the constraint reclassifies from tangled_rope (coordination+extraction) to snare (pure extraction with cover story). If tenure remains necessary, the constraint may reclassify downward toward rope or toward tangled_rope with lower ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether tenure''s founding problem persists or has been rendered obsolete by other institutional/legal protections of research freedom.').

omega_variable(
    coordination_function_separability,
    'Can research protection (the coordination function) be decoupled from permanent, indexed employment (the extraction mechanism)? Could fixed-term contracts with due process, strong reappointment presumptions, and peer review provide equivalent research protection at lower extraction cost?',
    'Pilot programs in universities transitioning from tenure to long-term contracts with strong reappointment protections; comparison of research output, freedom, and researcher satisfaction pre/post transition; international examples (many universities outside North America use fixed contracts with equivalent due process).',
    'If decoupling succeeds, the constraint can be restructured to extract less without losing coordination function (moving toward rope). If decoupling fails, research protection demands the extraction (moving toward tangled_rope with acceptance of the cost-benefit tradeoff).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_separability, empirical, 'Whether tenure''s coordination benefits are inseparable from its extraction structure, or whether alternatives could provide equivalent protection at lower cost.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of contingent faculty exit (trapped option) structural (legal barriers, market closure, visa sponsorship lock-in) or internalized (contingent faculty have internalized the status hierarchy and believe permanent positions are not for them)?',
    'Exit-cohort studies: when contingent faculty do exit academia, what happens to suppression? Do they report reduced constraint experience after departure? Comparative analysis of international scholars versus domestic scholars to isolate visa-lock effect.',
    'If suppression is primarily structural, it persists even after individual exit (high effective suppression). If primarily internalized, removal of the structural barrier (opening permanent positions) would reduce suppression sharply. This affects the computation of effective extraction for trapped payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether contingent faculty''s trapped exit is structural (institutional barriers) or partly internalized (identity fusion with precarious role).').

omega_variable(
    reading_foreclosure_from_academic_freedom_reading,
    'Does the institutional_extraction_reading foreclose the academic_freedom_reading, or can they coexist as different framings of the same institutional practice?',
    'Logical examination: can a scholar coherently hold both that tenure protects research freedom AND that tenure has become a pure extraction mechanism for early winners? The answer is yes if the readings refer to different time periods (tenure was justified by freedom protection in 1960, now persists as extraction) or if research protection remains real for some subset of researchers while extraction dominates the system-level function.',
    'If coexistence is coherent, the relation is coexists_with (both readings remain live in different institutional seats). If one reading''s core premise directly contradicts the other such that no framework could hold both, the relation is forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_from_academic_freedom_reading, conceptual, 'Logical relationship between the institutional_extraction reading and the academic_freedom reading of tenure.').

omega_variable(
    demographic_reproduction_entanglement,
    'To what degree does the demographic_reproduction_reading (tenure as demographic gatekeeping) and this reading (tenure as resource extraction) describe the same constraint, and to what degree do they describe structurally distinct constraints?',
    'Decomposition analysis: measure the demographic closure effect (homogeneity of tenured cohort relative to precarious cohort) independent of the resource extraction effect (salary/benefit claims). If both effects are substantial and independent, the readings refer to two constraints, not one.',
    'If the readings describe a single constraint from different angles, they should produce the same ε and differ only in framing. If they describe distinct constraints (one about resource extraction, one about demographic reproduction), each should have its own story with its own ε-invariance analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_reproduction_entanglement, conceptual, 'Whether demographic reproduction dynamics and resource extraction dynamics are aspects of one constraint or separate constraints entangled in the same institutional practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__institutional_extraction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenu_tr_t0, tenure_contract__institutional_extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(tenu_tr_t0, observed).
narrative_ontology:measurement(tenu_tr_t5, tenure_contract__institutional_extraction_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(tenu_tr_t5, observed).
narrative_ontology:measurement(tenu_tr_t10, tenure_contract__institutional_extraction_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement_basis(tenu_tr_t10, observed).
narrative_ontology:measurement(tenu_tr_t15, tenure_contract__institutional_extraction_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(tenu_tr_t15, observed).
narrative_ontology:measurement(tenu_tr_t25, tenure_contract__institutional_extraction_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement_basis(tenu_tr_t25, observed).
narrative_ontology:measurement(tenu_tr_t40, tenure_contract__institutional_extraction_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(tenu_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(tenu_be_t0, tenure_contract__institutional_extraction_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(tenu_be_t0, observed).
narrative_ontology:measurement(tenu_be_t5, tenure_contract__institutional_extraction_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement_basis(tenu_be_t5, observed).
narrative_ontology:measurement(tenu_be_t10, tenure_contract__institutional_extraction_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(tenu_be_t10, observed).
narrative_ontology:measurement(tenu_be_t15, tenure_contract__institutional_extraction_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(tenu_be_t15, observed).
narrative_ontology:measurement(tenu_be_t25, tenure_contract__institutional_extraction_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement_basis(tenu_be_t25, observed).
narrative_ontology:measurement(tenu_be_t40, tenure_contract__institutional_extraction_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement_basis(tenu_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(tenu_su_t0, tenure_contract__institutional_extraction_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(tenu_su_t0, observed).
narrative_ontology:measurement(tenu_su_t5, tenure_contract__institutional_extraction_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement_basis(tenu_su_t5, observed).
narrative_ontology:measurement(tenu_su_t10, tenure_contract__institutional_extraction_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(tenu_su_t10, observed).
narrative_ontology:measurement(tenu_su_t15, tenure_contract__institutional_extraction_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(tenu_su_t15, observed).
narrative_ontology:measurement(tenu_su_t25, tenure_contract__institutional_extraction_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement_basis(tenu_su_t25, observed).
narrative_ontology:measurement(tenu_su_t40, tenure_contract__institutional_extraction_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(tenu_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__institutional_extraction_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(tenure_contract__institutional_extraction_reading, 0.12).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__academic_freedom_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tenure_contract__demographic_reproduction_reading).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, contingency_labor_supply__precarity_trap).
narrative_ontology:affects_constraint(tenure_contract__institutional_extraction_reading, tuition_escalation__cost_transfer_mechanism).

% DUAL FORMULATION NOTE:
% This story is the institutional_extraction_reading of the contested kernel 'tenure_contract'. Two sibling readings exist as separate constraint stories: academic_freedom_reading (tenure as essential research protection) and demographic_reproduction_reading (tenure as demographic gatekeeping mechanism). All three are linked through network.affects_constraints. The ε-invariance principle requires each reading to have its own stable ε value and independent structural story; they are not different observers of the same constraint but different constraints instantiated from the same kernel. Constraint families permit this decomposition: the kernel is the contested commitment; the readings are the structurally distinct constraints that different stakeholders instantiate from it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, organized, 0.15).
constraint_indexing:directionality_override(tenure_contract__institutional_extraction_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
