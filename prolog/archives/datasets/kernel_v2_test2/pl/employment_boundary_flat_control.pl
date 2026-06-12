% ============================================================================
% CONSTRAINT STORY: employment_boundary_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary_flat_control, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: employment_boundary_flat_control
 *   human_readable: Employment Boundary Classification System
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The employment boundary classification system determines which work
 *   relationships trigger employer obligations (social insurance
 *   contributions, job security protections, minimum wage, overtime, workers'
 *   compensation, unemployment insurance) versus which relationships are
 *   treated as independent contracting arrangements exempt from these
 *   obligations. This boundary is a legal and administrative construct, not a
 *   natural category — the distinction between 'employee' and 'independent
 *   contractor' is drawn by multi-factor tests (IRS 20-factor test, ABC test,
 *   economic realities test) that attempt to operationalize contested
 *   normative premises about subordination, control, economic dependence, and
 *   entrepreneurial autonomy. The constraint exhibits tangled rope structure:
 *   it solves a genuine coordination problem (distinguishing subordinate
 *   employment from independent business activity, enabling legitimate
 *   contracting relationships, providing clear triggers for insurance
 *   contributions) while simultaneously enabling substantial extraction
 *   through enforcement asymmetry, regulatory arbitrage, and systematic
 *   misclassification. The platform economy has stress-tested the boundary by
 *   creating work relationships that combine employee-like subordination
 *   (algorithmic management, unilateral terms, economic dependence) with
 *   contractor-like features (task-level flexibility, own equipment, multiple
 *   platforms). The resulting ambiguity is not accidental — it reflects deep
 *   disagreement about whether worker autonomy is genuine or performative,
 *   whether flexibility is a benefit or a cost-shifting mechanism, and
 *   whether platform control through algorithms differs meaningfully from
 *   traditional employer control through supervisors.
 *
 * KEY AGENTS:
 *   - Misclassified Gig Workers: Primary victims (powerless/trapped) — bear full cost of misclassification through lost protections, cannot exit due to economic necessity and credential barriers
 *   - Platform Companies: Primary beneficiaries (institutional/arbitrage) — capture labor cost savings through contractor classification, can exit to employee model if necessary but current boundary is highly favorable
 *   - High-Skill Independent Contractors: Secondary beneficiaries (powerful/mobile) — genuinely prefer contractor status for autonomy and tax advantages, have real alternatives
 *   - Traditional Employers: Mixed position (moderate/constrained) — benefit from coordination function but pay through competitive disadvantage when rivals misclassify
 *   - Labor Union Coalition: Organized agents (organized/constrained) — see both coordination failure and extraction, constrained by legal barriers to organizing contractors
 *   - Social Insurance Systems: Institutional victims (institutional/constrained) — benefit from clear contribution triggers but pay through revenue erosion from misclassification
 *   - Administrative Classification System: Institutional actor (institutional/constrained) — maintains degraded multi-factor tests that map poorly to platform work (piton perspective)
 *   - Labor Arbitrage Firms: Secondary beneficiaries (institutional/arbitrage) — staffing agencies and labor brokers that profit from boundary ambiguity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary_flat_control, 0.62).
domain_priors:suppression_score(employment_boundary_flat_control, 0.68).
domain_priors:theater_ratio(employment_boundary_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary_flat_control, extractiveness, 0.62).
narrative_ontology:constraint_metric(employment_boundary_flat_control, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(employment_boundary_flat_control, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary_flat_control, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(employment_boundary_flat_control, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary_flat_control, tangled_rope).
narrative_ontology:human_readable(employment_boundary_flat_control, "Employment Boundary Classification System").
narrative_ontology:topic_domain(employment_boundary_flat_control, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary_flat_control).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary_flat_control, 'cf84fdfb-9d6d-4320-87eb-090188c4b160').
narrative_ontology:cs_kernel_codification('cf84fdfb-9d6d-4320-87eb-090188c4b160', formalized).
narrative_ontology:cs_authority_grounding('cf84fdfb-9d6d-4320-87eb-090188c4b160', lineage).
narrative_ontology:cs_interpretation_layer_present('cf84fdfb-9d6d-4320-87eb-090188c4b160').
narrative_ontology:cs_created_at('cf84fdfb-9d6d-4320-87eb-090188c4b160', '').

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(employment_boundary_flat_control, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary_flat_control, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary_flat_control, high_skill_independent_contractors).
narrative_ontology:constraint_beneficiary(employment_boundary_flat_control, labor_arbitrage_firms).
narrative_ontology:constraint_victim(employment_boundary_flat_control, misclassified_workers).
narrative_ontology:constraint_victim(employment_boundary_flat_control, social_insurance_systems).
narrative_ontology:constraint_victim(employment_boundary_flat_control, traditional_employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(employment_boundary_flat_control, misclassified_gig_workers).
narrative_ontology:constraint_victim(employment_boundary_flat_control, labor_union_coalition).
narrative_ontology:constraint_vindicates(employment_boundary_flat_control, entrepreneurial_autonomy_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary_flat_control, flexibility_premium_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers performing app-based delivery, rideshare, or task work under conditions functionally equivalent to employment (algorithmic management, unilateral terms, economic dependence) but legally classified as independent contractors. Bear full cost of misclassification: no health insurance, no unemployment protection, no retirement contributions, no minimum wage guarantee, no overtime, no workers' compensation. Cannot exit to traditional employment due to credential barriers, geographic constraints, care responsibilities, or lack of alternative opportunities. Income volatility and lack of protections create biographical-scale precarity.
narrative_ontology:constraint_stakeholder(employment_boundary_flat_control, misclassified_gig_workers, payer,
    powerless, biographical, trapped, national).

% Technology platforms (rideshare, delivery, task marketplaces) that structure work relationships to avoid employee classification. Set the terms unilaterally through algorithmic management, rating systems, and terms of service. Capture labor cost savings worth 20-30% of total compensation by avoiding employer obligations. Can exit to employee model if regulatory pressure becomes severe (some platforms have done so in specific jurisdictions) but current contractor classification is highly favorable for scaling and profitability. Operate globally and can shift operations to jurisdictions with favorable classification rules.
narrative_ontology:constraint_stakeholder(employment_boundary_flat_control, platform_companies, agenda_setter,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary_flat_control, platform_companies, beneficiary).

% Professionals (consultants, software developers, designers, writers) who genuinely prefer independent contractor status for autonomy, tax advantages, and rate premiums. Have real bargaining power, can negotiate terms, can decline unfavorable engagements, and can exit to traditional employment if desired. Benefit from the boundary's flexibility and from tax treatment that allows business expense deductions. This group's genuine preference for contractor status is often cited to justify classification of workers who lack similar autonomy and alternatives.
narrative_ontology:constraint_stakeholder(employment_boundary_flat_control, high_skill_independent_contractors, beneficiary,
    powerful, biographical, mobile, national).

% Businesses that classify workers as employees and bear full employer obligations (payroll taxes, benefits, job security protections, compliance costs). Face competitive pressure from platform firms and labor arbitrage operations that undercut on price by misclassifying workers. Benefit from the boundary's coordination function (clear rules about when obligations trigger, ability to use legitimate contractors for specialized work) but pay through competitive disadvantage when rivals avoid obligations through misclassification. Constrained by compliance requirements and by reputational risk of misclassification.
narrative_ontology:constraint_stakeholder(employment_boundary_flat_control, traditional_employers, payer,
    moderate, biographical, constrained, national).

% Labor unions, worker centers, and advocacy organizations attempting to organize platform workers and challenge misclassification. Benefit from the boundary's existence (provides legal hook for organizing campaigns and misclassification litigation) but pay through membership erosion as workers are pushed into non-union contractor status. Constrained by legal barriers to organizing independent contractors (NLRA excludes contractors from collective bargaining rights), by platform companies' superior resources for legal defense and lobbying, and by atomized workforce structure that makes traditional organizing difficult.
narrative_ontology:constraint_stakeholder(employment_boundary_flat_control, labor_union_coalition, payer,
    organized, generational, constrained, national).

% Government agencies administering unemployment insurance, workers' compensation, social security, and disability insurance. Benefit from the boundary's coordination function (clear triggers for contribution requirements enable insurance pool management) but pay through revenue erosion as systematic misclassification shrinks the contribution base. Constrained by enforcement resource limits (audit capacity has not scaled with workforce growth), by political pressure from platform companies and business lobbies, and by legal ambiguity that makes misclassification cases difficult to win.
narrative_ontology:constraint_stakeholder(employment_boundary_flat_control, social_insurance_systems, payer,
    institutional, generational, constrained, national).

% Staffing agencies, labor brokers, and outsourcing firms that profit from boundary ambiguity by providing workers classified as contractors to client companies. Capture margin between what clients pay and what workers receive, with additional profit from avoiding employer obligations. Can restructure business models if classification rules tighten. Operate in gray areas of joint employment and labor brokering.
narrative_ontology:constraint_stakeholder(employment_boundary_flat_control, labor_arbitrage_firms, beneficiary,
    institutional, biographical, arbitrage, national).

% Labor departments, tax agencies, and courts applying multi-factor tests (IRS 20-factor test, ABC test, economic realities test) to classify work relationships. Originally designed for industrial-era employment, now applied to platform economy with substantial mismatch. Factors like 'furnishing own tools' (workers provide smartphones) and 'set hours' (algorithmic dispatch creates effective scheduling) map poorly to app-based work. The classification process persists through institutional inertia despite low functional accuracy. Enforcement agencies see their own process as degraded but maintain it because no consensus alternative exists.
narrative_ontology:constraint_stakeholder(employment_boundary_flat_control, administrative_classification_system, agenda_setter,
    institutional, civilizational, constrained, national).

% Researchers studying employment relationships, labor market structure, and classification systems across jurisdictions and time periods. Observe that the boundary solves a genuine coordination problem (distinguishing subordinate employment from independent business activity) but embeds substantial extraction through enforcement asymmetry and systematic misclassification. Document that boundary placement reflects contested normative premises about autonomy, obligation, and social insurance rather than technical criteria. Analytical position enables cross-jurisdictional comparison and historical perspective but does not resolve normative disagreement about where the boundary should be drawn.
narrative_ontology:constraint_stakeholder(employment_boundary_flat_control, analytical_labor_economists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The employment boundary distinguishes subordinate employment relationships (where employer obligations like social insurance, job security, and minimum wage apply) from independent business relationships (where they do not). This solves a real coordination problem: without a clear boundary, there would be no trigger for employer obligations, legitimate independent contracting would be impossible, and social insurance contribution requirements would be undefined. The boundary enables businesses to engage specialized contractors for project work without assuming full employment obligations, and it enables genuinely independent professionals to operate as businesses with autonomy and tax advantages.
% TRANSFER_FUNCTION: The boundary transfers costs and risks from employers to workers when relationships are classified as independent contracting. Specifically: social insurance costs (unemployment insurance, workers' compensation, payroll taxes) shift from employer to worker or to social insurance systems through reduced contribution base; income risk shifts from employer (who would bear cost of idle time, training, benefits) to worker (who bears full income volatility); job security protections (wrongful termination, notice requirements) are eliminated. The transfer runs from misclassified workers and social insurance systems toward platform companies and labor arbitrage firms, who capture cost savings worth 20-30% of total compensation.
% ABSENT_VOICES: Misclassified workers are structurally under-represented in the policy process that sets classification rules. Platform companies have superior resources for lobbying, litigation, and regulatory capture. Workers are atomized (no union representation for contractors), face retaliation risk (deactivation from platforms), and lack information about legal rights. Enforcement agencies are under-resourced and face political pressure. The result: classification rules are set through processes where beneficiaries (platform companies, labor arbitrage firms) have far more voice than those who bear costs (misclassified workers, social insurance systems). High-skill contractors who genuinely prefer contractor status are over-represented in policy discourse relative to their share of the contractor workforce, and their preferences are used to justify classification of workers who lack similar autonomy.
% DISAPPEARANCE_RATIONALE: If the employment boundary disappeared overnight (all work relationships treated as employment, or all treated as independent contracting), the world would rearrange substantially. Under universal employment classification: platform business models would restructure (higher labor costs, different scaling dynamics), social insurance contribution base would expand, workers would gain protections but lose task-level flexibility, labor market would shift toward traditional employment relationships. Under universal contractor classification: employer obligations would collapse, social insurance systems would need alternative funding, workers would bear full income and health risk, labor market would shift toward spot-market transactions. The boundary is not a natural fact — it is a legal construct that organizes substantial economic activity, and its removal or repositioning would force major rearrangements. Multiple stakeholders (platform companies, traditional employers, workers, social insurance systems, unions) have organized their operations around the current boundary placement.
% FOUNDING_PROBLEM: The employment boundary was originally constructed to distinguish subordinate employment relationships (where workers lacked bargaining power and needed protective legislation) from independent business relationships (where parties had roughly equal bargaining power and could negotiate terms). The founding problem was: how to allocate employer obligations (social insurance, job security, minimum wage) to relationships where workers were economically dependent and lacked autonomy, while preserving freedom of contract for genuinely independent business relationships. The boundary emerged through early 20th century labor legislation, New Deal social insurance programs, and common-law employment tests developed for vicarious liability and tax purposes.
% FOUNDING_PROBLEM_CORROBORATION: The status is contested between two positions, each with institutional backing: (1) Platform companies and business groups argue the founding problem is obsolete — modern workers value flexibility over security, technology enables new forms of autonomy, and protective legislation designed for industrial employment is inappropriate for digital economy. They cite high-skill contractors' revealed preferences and worker surveys showing demand for flexible scheduling. (2) Labor unions, worker advocacy groups, and labor economists argue the founding problem is more live than ever — platform workers face economic dependence and lack of autonomy comparable to traditional employees, algorithmic management is a new form of subordination, and flexibility rhetoric masks cost-shifting. They cite misclassification litigation outcomes, wage studies showing contractor earnings below minimum wage after expenses, and worker testimony about lack of genuine autonomy. Enforcement agencies are internally divided: some investigators see widespread misclassification as evidence the founding problem persists; others see platform work as genuinely novel and existing tests as obsolete. No neutral arbiter exists — the question 'is this subordinate employment or independent business activity?' is itself the contested normative premise the boundary was built to operationalize.
narrative_ontology:disappearance_verdict(employment_boundary_flat_control, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary_flat_control, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MISCLASSIFIED GIG WORKER (SNARE) — Trapped by economic necessity in platform work that is functionally employment but legally classified as independent contracting. Bears full cost of misclassification: no health insurance, no unemployment protection, no retirement contributions, no job security. Cannot exit to traditional employment due to credential barriers, geographic constraints, or care responsibilities. The boundary appears as pure extraction — a legal fiction that strips protections while maintaining functional control.
constraint_indexing:constraint_classification(employment_boundary_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRADITIONAL EMPLOYER (TANGLED ROPE) — Constrained by compliance costs and competitive pressure from platform firms that avoid employer obligations. Benefits from the boundary's coordination function (clear rules about when obligations trigger) but pays through competitive disadvantage when rivals misclassify workers to cut costs. Experiences both coordination (the boundary enables legitimate contracting relationships) and extraction (enforcement asymmetry allows competitors to undercut through misclassification).
constraint_indexing:constraint_classification(employment_boundary_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM COMPANY (ROPE) — Benefits from boundary ambiguity that enables labor cost arbitrage. Experiences the constraint as coordination: the independent contractor classification allows flexible scaling, avoids legacy obligations, and enables global expansion. Can exit to traditional employment model if regulatory pressure becomes severe, but current boundary placement is highly favorable. Net beneficiary — the boundary's ambiguity is a feature, not a bug.
constraint_indexing:constraint_classification(employment_boundary_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR UNION COALITION (TANGLED ROPE) — Organized agents (unions, worker centers, advocacy groups) see both coordination failure and extraction. The boundary solves a real problem (distinguishing genuine independent business owners from disguised employees) but current enforcement is asymmetric and under-resourced. Benefits from the boundary's existence (provides a legal hook for organizing campaigns) but pays through membership erosion as workers are pushed into non-union contractor status. Constrained by legal barriers to organizing independent contractors and by platform companies' superior resources.
constraint_indexing:constraint_classification(employment_boundary_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HIGH-SKILL INDEPENDENT CONTRACTOR (ROPE) — Mobile professional (consultant, software developer, creative) who genuinely prefers independent contractor status for tax advantages, autonomy, and rate premiums. Benefits from the boundary's flexibility and can exit to traditional employment if desired. Experiences the constraint as coordination — it enables a legitimate business model. Low effective extraction because this agent has real bargaining power and genuine alternatives.
constraint_indexing:constraint_classification(employment_boundary_flat_control, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: SOCIAL INSURANCE SYSTEM (TANGLED ROPE) — Government agencies administering unemployment insurance, workers' compensation, social security. Benefits from the boundary's coordination function (clear contribution triggers) but pays through revenue erosion as misclassification shrinks the contribution base. Constrained by enforcement resource limits and by political pressure from platform companies. Experiences both coordination (the boundary enables insurance pool management) and extraction (systematic misclassification undermines system solvency).
constraint_indexing:constraint_classification(employment_boundary_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ADMINISTRATIVE CLASSIFICATION SYSTEM (PITON) — The multi-factor tests (IRS 20-factor test, ABC test, economic realities test) that purport to distinguish employees from contractors. Originally designed for industrial-era work relationships, now applied to platform economy with substantial theater: factors like 'furnishing tools' and 'set hours' map poorly to app-based work. The classification ritual persists through institutional inertia despite low functional accuracy. Enforcement agencies see their own process as degraded — maintained because no consensus alternative exists, not because it works well.
constraint_indexing:constraint_classification(employment_boundary_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the employment boundary solves a genuine coordination problem (distinguishing subordinate employment from independent business activity) but embeds substantial extraction through enforcement asymmetry, regulatory arbitrage, and systematic misclassification. The boundary's ambiguity is not accidental — it reflects contested normative premises about worker autonomy, employer obligation, and the proper scope of social insurance. Analytical classification: tangled rope, because both coordination function and asymmetric extraction are structurally present and neither can be removed without dissolving the constraint.
constraint_indexing:constraint_classification(employment_boundary_flat_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(employment_boundary_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(employment_boundary_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(employment_boundary_flat_control, TR),
    TR >= 0.70.

:- end_tests(employment_boundary_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Substantial. Platform companies and labor arbitrage firms capture significant cost savings by classifying workers as independent contractors, avoiding employer obligations worth 20-30% of labor costs (social insurance, benefits, job security). Workers bear these costs through lost protections and income volatility. The extraction has increased over the 40-year interval as platform business models scaled and enforcement resources failed to keep pace. However, extraction is not maximal because some workers (high-skill contractors) genuinely benefit from the flexibility, and some coordination function is real. Suppression (0.68): High. Workers face substantial barriers to challenging misclassification: economic dependence on platform income, information asymmetry about legal rights, retaliation risk (deactivation), collective action barriers (atomized workforce, legal obstacles to organizing contractors), and under-resourced enforcement agencies. Suppression has increased as platform companies have grown more sophisticated in structuring relationships to avoid employee status. Theater ratio (0.58): Moderate-high. The multi-factor tests (IRS 20-factor, ABC test, economic realities test) are substantially performative when applied to platform work. Factors like 'furnishing own tools' (workers provide smartphones) and 'set hours' (algorithmic dispatch creates effective scheduling) map poorly to app-based work. Administrative classification involves ritualistic application of industrial-era criteria to digital economy relationships. The theater has increased as the gap between test design and work reality has widened. Accessibility collapse (0.42): Moderate. Alternative arrangements exist (employee status, third-category proposals, sectoral bargaining) but face significant political and economic barriers. The boundary is not inevitable — other jurisdictions draw it differently — but path dependence and beneficiary resistance make alternatives difficult to access. Resistance (0.71): High. The boundary faces substantial organized resistance from labor unions, worker advocacy groups, misclassified workers, and some enforcement agencies. Multiple legal challenges, legislative proposals, and ballot initiatives contest current boundary placement. High resistance indicates this is a contested construct, not a natural law.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same legal boundary appears radically different depending on structural position. Platform companies see coordination — the boundary enables flexible scaling and legitimate contracting relationships. High-skill contractors see coordination — the boundary enables autonomy and entrepreneurial opportunity. Misclassified gig workers see pure extraction — a legal fiction that strips protections while maintaining functional control. Traditional employers see mixed coordination and extraction — the boundary enables legitimate contracting but also enables competitors to undercut through misclassification. Social insurance systems see mixed coordination and extraction — the boundary provides clear contribution triggers but also enables systematic revenue erosion. Labor unions see mixed coordination and extraction — the boundary provides a legal hook for organizing but also enables membership erosion. The administrative system sees its own degraded ritual — multi-factor tests that no longer map to work reality. The analytical observer sees tangled rope — genuine coordination function and substantial extraction are structurally inseparable, and the boundary's ambiguity reflects contested normative premises rather than technical uncertainty. The perspectival gap is not resolvable by better measurement — it reflects genuine structural differences in how the boundary affects different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from each agent's structural position relative to the employment boundary. Platform companies are primary beneficiaries with arbitrage exit options — they capture cost savings from contractor classification and can restructure if necessary, yielding low d and low or negative effective extraction (they experience the boundary as coordination). Misclassified gig workers are primary victims with trapped exit options — they bear full cost of lost protections and cannot exit due to economic necessity, yielding high d and high effective extraction (they experience the boundary as pure extraction, snare classification). High-skill independent contractors are secondary beneficiaries with mobile exit options — they genuinely prefer contractor status and have real alternatives, yielding low d and low effective extraction (rope classification). Traditional employers are in a mixed position — they benefit from the boundary's coordination function but pay through competitive disadvantage when rivals misclassify, yielding moderate d (tangled rope classification). Social insurance systems are institutional victims with constrained exit options — they benefit from clear contribution triggers but pay through revenue erosion, yielding moderate-high d (tangled rope classification). The labor union coalition is organized with constrained exit — they see both coordination failure and extraction, yielding moderate d (tangled rope classification). The administrative classification system sees its own process as degraded (piton classification derives from theater gate, not from high chi). The analytical observer sees both genuine coordination function and substantial extraction as structurally inseparable (tangled rope classification).
 *
 * MANDATROPHY ANALYSIS:
 *   The employment boundary has not resolved mandatrophy — its original mandate (distinguishing subordinate employment from independent business activity to allocate employer obligations appropriately) remains live, but the constraint's operation has accumulated substantial extraction as platform companies have exploited boundary ambiguity for labor cost arbitrage. The boundary solves a real coordination problem: without it, there would be no clear trigger for employer obligations, and legitimate independent contracting relationships would be impossible. However, the coordination function coexists with systematic extraction: enforcement asymmetry, regulatory arbitrage, and under-resourced agencies allow widespread misclassification that shifts costs from employers to workers and social insurance systems. The tangled rope classification captures this dual structure — neither pure coordination (rope) nor pure extraction (snare) but both simultaneously. The constraint's theater ratio (0.58) reflects that the multi-factor tests are partly performative, but they are not purely theatrical (piton) because they still channel some disputes and enable some enforcement. The boundary's extractiveness has increased over the 40-year interval (0.38 to 0.62) as platform business models scaled faster than enforcement capacity, but it has not reached snare levels because genuine coordination function persists and some workers genuinely benefit. Mandatrophy would be resolved if either: (1) enforcement resources scaled to deter misclassification, restoring the boundary to its coordination function (rope), or (2) the boundary collapsed entirely and all work relationships were treated as employment (mountain of labor law), or (3) a third legal category with partial protections successfully separated genuine contractors from disguised employees (scaffold with sunset). None of these paths is currently dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_vs_autonomy_threshold,
    'What degree of platform control over work processes (algorithmic management, rating systems, route assignment) is compatible with genuine independent contractor status?',
    'Comparative analysis of contractor autonomy across industries; empirical studies of worker decision-making latitude under different platform governance models; legal precedent analysis from jurisdictions with different control thresholds',
    'If threshold is low (minimal control required for employee status): most platform workers reclassified as employees, platform business models restructure. If threshold is high (substantial control permitted for contractors): current classifications largely upheld, extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_vs_autonomy_threshold, conceptual, 'Threshold of platform control compatible with contractor status').

omega_variable(
    flexibility_premium_empirical_status,
    'Do workers actually receive compensating wage premiums for flexibility and autonomy under independent contractor arrangements, or is the ''flexibility premium'' a cover story for cost-shifting?',
    'Wage comparisons controlling for skill, experience, and working conditions between employees and contractors in same occupation; longitudinal tracking of workers who transition between statuses; revealed preference studies of worker choices when both options available at same firm',
    'If premium exists and is substantial: contractor classification has genuine mutual benefit, coordination function is real. If premium is absent or negative: flexibility rhetoric is extraction cover, snare classification more accurate from more perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(flexibility_premium_empirical_status, empirical, 'Whether flexibility premium compensates for lost protections').

omega_variable(
    enforcement_resource_sufficiency,
    'Are current enforcement resources (labor department investigators, audit capacity, penalty structures) sufficient to deter systematic misclassification, or is under-enforcement structural?',
    'Audit rate analysis; penalty-to-benefit ratio calculations; comparison of enforcement intensity across jurisdictions with different misclassification rates; budget allocation trends relative to workforce size',
    'If resources sufficient: enforcement failure is political choice, not structural constraint. If resources structurally insufficient: extraction is baked into the system regardless of formal rules, and the boundary functions as intended cover for cost-shifting.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_resource_sufficiency, empirical, 'Whether enforcement resources can deter misclassification').

omega_variable(
    third_category_viability,
    'Would a third legal category (dependent contractor, worker, intermediate status) with partial protections resolve the coordination-extraction tension, or would it create new arbitrage opportunities?',
    'Analysis of jurisdictions that have implemented third categories (UK, Canada, Spain); study of classification gaming and boundary manipulation in multi-tier systems; assessment of whether intermediate protections are stable or degrade toward contractor status',
    'If viable: scaffold perspective gains support, sunset path exists through institutional innovation. If non-viable: binary boundary is structurally necessary, and extraction is inherent to any line-drawing exercise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_category_viability, empirical, 'Whether intermediate legal category resolves tension').

omega_variable(
    algorithmic_management_visibility,
    'Does algorithmic management (automated dispatch, dynamic pricing, rating systems) constitute employer control even when workers can decline individual tasks?',
    'Behavioral economics analysis of choice architecture in platform systems; comparison of worker behavior under algorithmic vs human management; legal analysis of control doctrine applied to algorithmic systems',
    'If algorithmic management constitutes control: most platform workers are employees under existing legal tests, boundary is being systematically misapplied. If it does not: platform model is genuinely novel and existing tests are obsolete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_management_visibility, conceptual, 'Whether algorithmic management constitutes employer control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary_flat_control, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empbound_theater_1980, employment_boundary_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(empbound_theater_1990, employment_boundary_flat_control, theater_ratio, 10, 0.42).
narrative_ontology:measurement(empbound_theater_2000, employment_boundary_flat_control, theater_ratio, 20, 0.48).
narrative_ontology:measurement(empbound_theater_2010, employment_boundary_flat_control, theater_ratio, 30, 0.55).
narrative_ontology:measurement(empbound_theater_2020, employment_boundary_flat_control, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(empbound_extract_1980, employment_boundary_flat_control, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(empbound_extract_1990, employment_boundary_flat_control, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(empbound_extract_2000, employment_boundary_flat_control, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(empbound_extract_2010, employment_boundary_flat_control, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(empbound_extract_2020, employment_boundary_flat_control, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(empbound_suppress_1980, employment_boundary_flat_control, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(empbound_suppress_1990, employment_boundary_flat_control, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(empbound_suppress_2000, employment_boundary_flat_control, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(empbound_suppress_2010, employment_boundary_flat_control, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(empbound_suppress_2020, employment_boundary_flat_control, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary_flat_control, enforcement_mechanism).
narrative_ontology:affects_constraint(employment_boundary_flat_control, platform_algorithmic_management).
narrative_ontology:affects_constraint(employment_boundary_flat_control, social_insurance_solvency).
narrative_ontology:affects_constraint(employment_boundary_flat_control, labor_market_monopsony).

% DUAL FORMULATION NOTE:
% The employment boundary is upstream of multiple labor market constraints. Platform algorithmic management depends on contractor classification to avoid employment law constraints on scheduling and supervision. Social insurance solvency depends on the employment boundary to define the contribution base. Labor market monopsony power is amplified when workers are classified as contractors and thus excluded from collective bargaining protections.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
