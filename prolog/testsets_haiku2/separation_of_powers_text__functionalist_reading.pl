% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__functionalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Separation of Powers (Functionalist Reading): Flexible Overlapping Authority via Intelligible Delegation
 *   domain: constitutional/political/administrative
 *
 * SUMMARY:
 *   The functionalist reading of separation of powers interprets the
 *   constitutional text as establishing flexible authority-sharing and
 *   outcome-protective principles rather than rigid tripartite division.
 *   Under this reading, Congress may delegate rulemaking authority to
 *   agencies as long as intelligible principle boundaries exist; the
 *   President may direct executive agencies and appoint their heads; courts
 *   review agency action for statutory conformity via deference frameworks
 *   (Chevron-like standards). This reading legitimizes the modern
 *   administrative state and permits overlapping functional authority. It
 *   competes with formalist readings (strict boundary doctrine) and unitary
 *   executive readings (all executive power in President alone), though the
 *   functionalist reading is institutionally dominant in U.S. constitutional
 *   law. The constraint story describes the functional coordination the
 *   reading provides (expert implementation, uniform standards, legislative
 *   workload solution) alongside the extractive elements it permits (agency
 *   discretion, regulatory uncertainty, lock-in for regulated parties).
 *
 * KEY AGENTS:
 *   - Executive agencies: institutional beneficiary, derive delegation legitimacy from this reading
 *   - Congress: institutional agenda-setter, delegates detail via statutes with intelligible principle bounds
 *   - The Presidency: institutional agenda-setter, shares executive authority with agencies rather than monopolizing it
 *   - Federal judiciary: institutional agenda-setter, interprets statutory bounds and reviews agency conformity
 *   - Regulated industries: powerful payers, bear compliance costs and regulatory lock-in
 *   - Formalist doctrine adherents: moderate payers, operate in framework they view as constitutionally illegitimate
 *   - Unitary executive theory adherents: moderate payers, same structural complaint as formalists
 *   - Administrative law practitioners: moderate beneficiaries, derive professional work from framework
 *   - General public: powerless bifurcated stakeholders, beneficiary (regulation) and payer (compliance costs)
 *   - Excluded strict constitutional framers: analytical observers whose intent is overridden
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.38).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.42).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Separation of Powers (Functionalist Reading): Flexible Overlapping Authority via Intelligible Delegation").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional/political/administrative").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, '3caa42e4-0609-4c53-b6cc-b91f78ce182c').
narrative_ontology:cs_kernel_codification('3caa42e4-0609-4c53-b6cc-b91f78ce182c', formalized).
narrative_ontology:cs_authority_grounding('3caa42e4-0609-4c53-b6cc-b91f78ce182c', lineage).
narrative_ontology:cs_interpretation_layer_present('3caa42e4-0609-4c53-b6cc-b91f78ce182c').
narrative_ontology:cs_reading_relation('3caa42e4-0609-4c53-b6cc-b91f78ce182c', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3caa42e4-0609-4c53-b6cc-b91f78ce182c', separation_of_powers_text__unitary_executive_reading, influences).
narrative_ontology:cs_axiom('3caa42e4-0609-4c53-b6cc-b91f78ce182c', foundational, separation_of_powers_prevents_tyranny_via_branch_balance).
narrative_ontology:cs_axiom_status(separation_of_powers_prevents_tyranny_via_branch_balance, holdable).
narrative_ontology:cs_axiom_grounding('3caa42e4-0609-4c53-b6cc-b91f78ce182c', separation_of_powers_prevents_tyranny_via_branch_balance, deontological).
narrative_ontology:cs_axiom('3caa42e4-0609-4c53-b6cc-b91f78ce182c', foundational, overlapping_authority_and_delegation_compatible_with_structural_protection).
narrative_ontology:cs_axiom_status(overlapping_authority_and_delegation_compatible_with_structural_protection, holdable).
narrative_ontology:cs_axiom_grounding('3caa42e4-0609-4c53-b6cc-b91f78ce182c', overlapping_authority_and_delegation_compatible_with_structural_protection, deontological).
narrative_ontology:cs_reference_frame('3caa42e4-0609-4c53-b6cc-b91f78ce182c', flexible_tripartite_authority_sharing).
narrative_ontology:cs_drift_state('3caa42e4-0609-4c53-b6cc-b91f78ce182c', contemporary_originalist_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3caa42e4-0609-4c53-b6cc-b91f78ce182c', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, executive_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congressional_delegating_bodies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, regulatory_state).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, formal_separation_doctrine_adherents).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, unitary_executive_theory_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_law_practitioners).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, the_general_public).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, regulated_industries).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, formalist_doctrine_adherents).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, the_general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate under delegated authority from Congress to implement statutory schemes through rulemaking and adjudication. The functionalist reading legitimizes their existence and scope by permitting overlapping authority and treating delegation as constitutionally permissible within intelligible principle bounds. Agencies depend on this reading's acceptance to justify their regulatory authority. They set rules, interpret statutes within their jurisdiction, and face judicial review for statutory conformity.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, executive_agencies, beneficiary,
    institutional, generational, constrained, national).

% Delegates legislative detail to agencies via statutes with intelligible principle boundaries. The functionalist reading permits this delegation and thereby solves a legislative workload problem: Congress can enact broad statutory frameworks while agencies handle technical rulemaking and factual application. Congress retains oversight authority via appropriations, statutory amendment, and confirmation power. Congress benefits from the constraint by avoiding the impossible task of legislating all regulatory detail.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, congress, beneficiary).

% Appoints agency heads and can direct agency action within statutory bounds (or claim supervisory power over independent agencies, contested). The functionalist reading permits this shared executive authority rather than requiring all execution flow through the President alone. Presidential power is shared with Congress (which sets delegation bounds) and with agencies (which interpret and execute). The President can attempt to redirect agencies through appointment and directive but cannot unilaterally revise statutory scope.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, the_presidency, agenda_setter,
    institutional, generational, arbitrage, national).

% Interprets the statutory bounds of delegation and reviews agency action for conformity. The Chevron deference framework (now amended by Loper Pickering and subsequent doctrine) operationalizes judicial review under the functionalist reading: courts defer to reasonable agency interpretations within statutory scope. Judiciary maintains boundary enforcement through standing, ripeness, and statutory interpretation doctrines. Judges do not directly benefit but perform the enforcement function that the reading requires.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Bear the costs of agency regulation: compliance costs, litigation risk, uncertainty from regulatory change, and operational constraint. They benefit from a stable regulatory environment but pay through reduced profit margins and operational flexibility. The functionalist reading locks them into the regulatory state's existence and scope; alternatives (strict formalism, no delegation) are constitutionally remote. They have geographic and sectoral arbitrage options but cannot exit the national regulatory system.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulated_industries, payer,
    powerful, biographical, constrained, national).

% Scholarly and judicial actors (originalists, textualists, formalists) who believe separation of powers requires strict, impermeable boundaries and that broad delegation violates constitutional form. They argue the functionalist reading permits erosion of structural limits and treats the administrative state as constitutionally illegitimate. They carry the cost of operating within a framework they view as wrong but cannot unilaterally change. Their objections appear in dissents and scholarship but do not control institutional outcomes.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_doctrine_adherents, payer,
    moderate, generational, constrained, national).

% Judicial and scholarly actors (originalists emphasizing presidential power, some Republican administrations) who believe all executive power vests solely in the President and that independent agencies violate this principle. The functionalist reading permits shared executive authority and independent agency structures, which adherents view as constitutionally problematic. They bear the institutional cost of non-compliance with their theory. Their reading has greater institutional influence during Republican administrations but remains minoritarian in courts and law.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, unitary_executive_theory_adherents, payer,
    moderate, generational, constrained, national).

% Lawyers and consultants who advise on compliance with agency regulation and judicial review of agency action. The functionalist reading creates stable, predictable boundaries for this practice: intelligible principle as the standard, deference doctrines as the norm, agency action as presumptively valid within statutory scope. The framework generates substantial legal work and professional authority. Practitioners can shift practice areas if the reading changed but have substantial specialization incentives.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_law_practitioners, beneficiary,
    moderate, biographical, mobile, national).

% Receives regulation of consumer protection, environmental safety, financial stability, workplace conditions, and food safety via agency action. Coordination benefit is genuine: uniform national standards for safety and fairness, achieved through expert rulemaking rather than ad-hoc litigation or state-by-state variation. The public also bears costs through regulatory compliance passed to prices, foregone innovation due to regulatory burden, and reduced choice as industries concentrate to meet regulatory costs. Exit is not an option; exit to another jurisdiction is costly and limited.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, the_general_public, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, the_general_public, payer).

% Historical actors (the Framers, originalist interpreters of founding intent) would object that the functionalist reading departs from the original structural design by permitting overlapping authority and broad delegation. Their objection is recorded in dissenting opinions (Justice Thomas, Justice Gorsuch's Gundy concurrence) and originalist scholarship but does not enter the operative constitutional framework. They are excluded from the decision-making structure despite having standing in the intellectual debate.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, excluded_strict_constitutional_framers, excluded,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__functionalist_reading, executive_agencies).
narrative_ontology:fixing_cost_class(separation_of_powers_text__functionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the legislative-detail problem: Congress identifies policy goals and statutory boundaries, agencies develop technical rules and apply law to particular facts, courts review for statutory bounds. Expert administrative implementation at scale; uniform national standards across jurisdictions; responsiveness to changing factual conditions and technical developments without requiring full congressional re-legislation; specialization of knowledge and capacity. Achieved through a flexible constitutional framework permitting overlapping functional authority rather than a rigid tripartite division.
% TRANSFER_FUNCTION: Transfers regulatory authority from Congress (which lacks technical expertise and bandwidth) to agencies (which specialize in implementation). Transfers judicial authority to interpret agencies' statutory bounds via deference frameworks rather than direct judicial lawmaking. Transfers compliance costs from the general public (who benefits from regulation) to regulated industries and diffuses implementation burden across a federal system. The functionalist reading legitimizes these transfers as constitutional delegation, not structural violation.
% ABSENT_VOICES: Formalist doctrine adherents and unitary executive theory adherents are present in scholarly discourse, dissenting opinions, and some circuit courts but do not control the operative constitutional framework. Originalist jurists (Justice Thomas, Justice Gorsuch's Gundy concurrence, some Trump administration originalist appointments) argue the functionalist reading betrays structural intent, but they are outnumbered institutionally. Their objections are recorded in dissents and influence some regulatory decisions but do not reshape the dominant framework. States, if they held a formal seat in constitutional interpretation, might object to federal agency authority that preempts state law — but federalism is a separate structural constraint.
% DISAPPEARANCE_RATIONALE: If the functionalist reading vanished overnight and a strict formalist framework took its place, the modern administrative state would cease to function: agencies would lose delegation legitimacy, Congress would be constitutionally barred from broad delegation, and Congress would need to legislate every regulatory detail directly (an institutional impossibility given the volume of technical regulation required). Alternatively, constitutional amendment would be required to preserve agency authority. The entire modern allocation of authority among branches — centuries of executive agency development, delegation doctrine, deference frameworks — would reorganize around strict separation. Environmental protection, financial regulation, workplace safety, food and drug approval, and telecommunications would all require complete statutory re-engineering or would lapse into unregulated status.
% FOUNDING_PROBLEM: The Constitution grants Congress legislative power but does not explicitly specify whether Congress may delegate rule-making authority to executive agencies. The New Deal and postwar administrative expansion created a factual reality of delegated authority (agencies writing regulations affecting millions, interpreting ambiguous statutes, applying rules to particular cases) that the strict formalist reading could not accommodate. The Supreme Court initially struck down the National Industrial Recovery Act (1935) on delegation grounds, creating a constitutional crisis: the regulatory state appeared unconstitutional. The functionalist reading solved this crisis by reinterpreting separation of powers as flexible and outcome-protective rather than form-rigid, legitimizing delegation within intelligible principle bounds.
% FOUNDING_PROBLEM_CORROBORATION: Federal courts (Supreme Court, appeals courts) have affirmed the functionalist framework for 90+ years via the intelligible principle doctrine. No statute has been struck down on delegation grounds since NIRA (1935), suggesting near-complete validation of delegation legitimacy. Scholarly consensus in administrative law, constitutional law, and political science treats functionalism as the settled doctrine. Congressional practice (hundreds of statutes delegating detail to agencies) relies on functionalism. However, formalist and originalist dissents (Justice Gorsuch in Gundy v. United States, 2019; Justice Thomas's concurring opinions emphasizing delegation limits; Loper Pickering Holdings v. SEC, 2024, constraining Chevron deference) represent minority but growing institutional positions, particularly in originalist appointments. The founding problem persists: formalists and originalists argue the delegation violates separation of powers; the majoritarian institutional answer (courts, agencies, Congress, administrators) is that functionalism is the legitimate reading. Outside corroboration: Independent constitutional scholars and political scientists (e.g., Huq, Whittington, Sunstein in various works) attest that Congress continues to face legislative bandwidth constraints and that agencies continue to exercise delegated authority. The regulatory state's functioning depends on delegation legitimacy. No neutral observer disputes that the founding problem (Congress cannot legislate all detail) remains true; dispute is only about whether functionalism correctly solves it.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).
:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38, rising from 0.28 at t=0 to peak of 0.40 at t=60, then declining slightly to 0.38 at t=90). The trajectory reflects increasing regulatory scope and agency authority over the 90-period interval, then stabilization as political forces balance expansion. The reading permits this extraction by legitimizing delegation; a formalist reading would constrain it. Suppression (0.42) is moderate because regulated parties cannot exit the national regulatory system entirely, but they retain arbitrage (litigation, lobbying, regulatory shopping across jurisdictions, compliance strategies). Theater ratio (0.28, modest but rising from 0.18) reflects the growing gap between the legitimacy narrative (expert, flexible, outcome-protective) and the actual operation (increasingly fixed agency rule-sets, deference doctrines that limit meaningful judicial review, regulatory inertia). Accessibility_collapse (0.62) is moderate-high: once the functionalist reading is accepted, alternatives (strict separation, no delegation) appear politically and institutionally remote; but the reading itself is contested within the legal system, so alternatives are not completely closed off. Resistance (0.71) is substantial because formalist and originalist jurists actively oppose the reading, generate dissenting doctrine, and mount periodic institutional challenges (especially when Republican administrations emphasize unitary executive or originalist appointments). The claimed type is tangled rope: the reading provides genuine coordination (expert regulation, uniform standards, legislative workload solution) while permitting overlapping authority that beneficiaries (agencies, Congress, practitioners) exploit for extraction-like rent collection via regulatory uncertainty and discretion lock-in. The measurement series trace a partial lifecycle: extractiveness and suppression rise as the regulatory state expands, then stabilize; theater ratio rises as the gap between legitimacy narrative and operational discretion widens; resistance remains high due to doctrinal contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the executive agencies' and Congress's seats, the constraint is genuine coordination solving the detail/expertise problem. From the regulated industries' seat, the same constraint is lock-in: they cannot challenge the delegation without challenging the constitutional reading, which is institutionally out of reach. From the formalist jurists' seat, the constraint is a constitutional violation dressed in functional rhetoric. From the unitary executive theorists' seat, the constraint violates executive structure. The engine should compute these seats as seeing different types: agenda-setters see coordination (lower d, lower χ), payers see extraction (higher d, higher χ), excluded voices see violation (theoretical, no χ). This divergence is the core of the per-seat computation; the authored claim (tangled rope) asserts that the reading BOTH provides real coordination AND permits extractive authority-sharing, which is true for some seats (beneficiaries, payers) and false for others (excluded voices).
 *
 * DIRECTIONALITY LOGIC:
 *   Directional mapping: Executive agencies (d ≈ 0.15, near beneficiary): they directly benefit from delegation legitimacy, face no exit, and are the reading's main institutional support — full beneficiary position. Congress (d ≈ 0.25, weak beneficiary): genuinely solves a legislative problem for Congress but Congress retains override authority and must justify delegation statutes, so Congress is not fully captured — weak beneficiary. The Presidency (d ≈ 0.35, symmetric-weak beneficiary): benefits from the reading's permission of executive agency action and appointment power, but shares authority with Congress and agencies rather than monopolizing it — weak beneficiary leaning toward symmetric. Regulated industries (d ≈ 0.75, near full target): bear substantive compliance costs, cannot exit (national scope, no jurisdiction arbitrage for many industries), internalize agency authority as legitimate, and face regulatory lock-in — strong target. Formalist/originalist adherents (d ≈ 0.80, full target): operate entirely within a framework they view as constitutionally wrong, cannot unilaterally change it, and bear the institutional cost of non-compliance with their theory — full target by value-basis (not power-basis), though moderate power moderates the effective d. General public (d ≈ 0.50, symmetric): genuine benefit from regulation, but also diffuse compliance costs and regulatory burden; complex multi-directional flow. No directionality overrides are needed; the derivation chain (beneficiary/victim + exit options + power) produces plausible d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the legislative-detail problem: Congress cannot legislate all technical regulatory detail) is live (it remains genuine; Congress still faces bandwidth constraints). The disappearance verdict is world_rearranges: if the functionalist reading vanished and strict formalism took over, the regulatory state would collapse or require constitutional amendment. The mismatch (status='live' + verdict='world_rearranges') indicates NO mandatrophy resolution: the constraint's founding problem persists and the arrangement depends on its solution. This is NOT a case of a founding problem gone dead while the arrangement persists due to inertia (the piton/zombie signature). The constraint may exhibit some theater ratio rise (gap between narrative and operation growing), but theater is not dominant (0.28, not 0.60+), so it does not flag as piton. The constraint is legitimately tangled rope: coordination function (detail/expertise problem) + extractive authority-sharing (discretion, regulatory lock-in, regulated-party lock-in) + active enforcement (judicial review, statutory boundaries, agency interpretation maintained via deference doctrine). No mandatrophy resolution is warranted; the constraint is functioning as designed by the reading and satisfies its stated purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_contest_formalist,
    'What structural claim does the formalist reading make that this functionalist reading forecloses or displaces?',
    'Textual and doctrinal analysis: the formalist reading asserts that separation of powers requires impermeable boundaries and forbids broad delegation to agencies. The functionalist reading asserts that separation of powers is outcome-protective (prevents tyranny by branch dominance) and accommodates overlapping authority and delegation within intelligible principle bounds. These readings occupy different parties'' constitutional commitments: originalist and formalist jurists hold the formalist reading; mainstream constitutional law scholarship and the institutional Court majority hold the functionalist reading. They coexist but compete.',
    'If foreclosure were established (one reading logically rules out the other in a single framework), the winning reading would establish a dominant constitutional interpretation. Since both remain live positions across the institutional system, they coexist. The engine''s reading_relations field records the structural relationship.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_contest_formalist, conceptual, 'Logical relationship between functionalist and formalist readings of separation of powers.').

omega_variable(
    sibling_reading_contest_unitary_executive,
    'What structural claim does the unitary executive reading make that this functionalist reading permits or forecloses?',
    'The unitary executive reading asserts that all executive power vests solely in the President and that independent agencies violate this principle. The functionalist reading permits shared executive authority (Congress sets bounds, President appoints and may direct, agencies execute within statutory scope, courts review). The unitary reading argues this violates the structural principle; the functionalist reading treats it as a permissible allocation within the separation framework. They coexist but diverge on executive authority boundaries.',
    'If the unitary executive reading foreclosed the functionalist reading, independent agencies would be unconstitutional and executive authority would be non-delegable. Since unitary theory remains a minority position in courts and law (though ascendant in some periods and administrations), both readings coexist. The functionalist reading influences (creates downstream pressure on) the unitary reading by establishing the institutional baseline of agency authority that the unitary reading must overcome.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_contest_unitary_executive, conceptual, 'Logical relationship between functionalist and unitary executive readings of separation of powers.').

omega_variable(
    delegation_breadth_boundary_ambiguity,
    'What count as sufficiently intelligible principles to satisfy the functionalist reading''s constitutionality requirement for delegation?',
    'Case-by-case judicial review of statutes: the Supreme Court has never held a statute unconstitutional on delegation grounds since National Industrial Recovery Act (1935), suggesting the ''intelligible principle'' standard is very permissive. But recent originalist opinions (Gundy concurrence, Jarkesy dissent) argue modern delegation statutes fail intelligible principle review. Testing requires either a Supreme Court decision striking down a delegation or a scholarly consensus on what Congress is constitutionally barred from delegating.',
    'A narrower intelligible principle test (stricter boundaries on what Congress may delegate) would reduce extraction, as agencies would have less delegated authority. A broader test (more permissive delegation boundaries) maintains the current state. The ambiguity is the functionalist reading''s operative question: where does flexibility end and unconstitutional abdication begin?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_breadth_boundary_ambiguity, empirical, 'Boundary condition for intelligible principle delegation standard.').

omega_variable(
    functional_vs_doctrinal_extraction_reading,
    'Is the measured extractiveness (0.38, moderate) correctly attributed to the functionalist reading''s permit of overlapping authority, or is it an artifact of how agencies use that authority independent of the reading?',
    'Comparative analysis: measure the extractiveness of the same regulatory functions under a hypothetical strict formalist framework (Congress legislates all detail, no delegation) versus the functionalist framework (agencies delegate-execute). If extractiveness remains similar (the functional content of regulation drives extraction regardless of constitutional framing), the reading''s extractiveness is low and the constraint is primarily coordination. If extractiveness drops substantially under the formalist framework (regulated parties face less uncertainty, more predictable rules, less agency discretion), the reading''s extractiveness is real and the constraint is tangled rope.',
    'If the extractiveness is reading-dependent (formalism = less extraction), then the functionalist reading genuinely enables agencies to extract more, and the constraint is extractive. If extractiveness is reading-independent (functional regulation extracts the same whether constitutionally framed as delegation or direct legislation), then the reading is pure coordination with minimal extractive content, and the claimed type should revise downward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(functional_vs_doctrinal_extraction_reading, conceptual, 'Whether the measured extractiveness is attributable to the functionalist reading''s permissiveness or to the underlying regulatory function.').

omega_variable(
    agency_discretion_as_internalized_suppression,
    'Does the functionalist framework''s permission of agency discretion (within intelligible principle bounds) constitute structural suppression of regulated parties'' exit options, or is the suppression internalized (parties internalize agency authority as legitimate)?',
    'Post-exit survey or behavioral data: if regulated parties can articulate a belief that agency authority is illegitimate even after the constraint is removed, the suppression was internalized (cognitive capture). If exit brings immediate recognition that alternatives were available, suppression was structural. For this reading specifically, the test is whether formalist doctrine or strict originalism, if institutionally adopted, would be experienced as liberation (internalized suppression) or as neutral reframing (structural suppression).',
    'If internalized, the effective suppression is higher than the authored 0.42 metric suggests; regulated parties carry the suppression frame into alternative environments. If structural, the suppression drops if the authority structure changes. Internalized suppression argues for higher theater ratio and hints at cognitive capture via the legitimacy frame that functionalism provides.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_discretion_as_internalized_suppression, empirical, 'Suppression mechanism: structural vs. internalized in separation-of-powers framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sopow_func_tr_t0, separation_of_powers_text__functionalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sopow_func_tr_t15, separation_of_powers_text__functionalist_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(sopow_func_tr_t30, separation_of_powers_text__functionalist_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(sopow_func_tr_t45, separation_of_powers_text__functionalist_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement(sopow_func_tr_t60, separation_of_powers_text__functionalist_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(sopow_func_tr_t75, separation_of_powers_text__functionalist_reading, theater_ratio, 75, 0.27).
narrative_ontology:measurement(sopow_func_tr_t90, separation_of_powers_text__functionalist_reading, theater_ratio, 90, 0.28).

% Extraction over time
narrative_ontology:measurement(sopow_func_be_t0, separation_of_powers_text__functionalist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sopow_func_be_t15, separation_of_powers_text__functionalist_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(sopow_func_be_t30, separation_of_powers_text__functionalist_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(sopow_func_be_t45, separation_of_powers_text__functionalist_reading, base_extractiveness, 45, 0.38).
narrative_ontology:measurement(sopow_func_be_t60, separation_of_powers_text__functionalist_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(sopow_func_be_t75, separation_of_powers_text__functionalist_reading, base_extractiveness, 75, 0.39).
narrative_ontology:measurement(sopow_func_be_t90, separation_of_powers_text__functionalist_reading, base_extractiveness, 90, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sopow_func_su_t0, separation_of_powers_text__functionalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(sopow_func_su_t15, separation_of_powers_text__functionalist_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(sopow_func_su_t30, separation_of_powers_text__functionalist_reading, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(sopow_func_su_t45, separation_of_powers_text__functionalist_reading, suppression_requirement, 45, 0.42).
narrative_ontology:measurement(sopow_func_su_t60, separation_of_powers_text__functionalist_reading, suppression_requirement, 60, 0.43).
narrative_ontology:measurement(sopow_func_su_t75, separation_of_powers_text__functionalist_reading, suppression_requirement, 75, 0.42).
narrative_ontology:measurement(sopow_func_su_t90, separation_of_powers_text__functionalist_reading, suppression_requirement, 90, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__functionalist_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the SEPARATION_OF_POWERS_TEXT kernel family comprising three sibling readings: (1) functionalist_reading (this story) — flexible, overlapping authority, intelligible delegation; ε ≈ 0.38, tangled rope. (2) formalist_reading — strict impermeable boundaries, no broad delegation; ε lower, rope-type. (3) unitary_executive_reading — all executive power in President, independent agencies unconstitutional; ε low for independent agencies, changes agency constraint types. All three share the same kernel text but instantiate different constraints with different structural relationships, ε values, and types. Decomposition is per the ε-invariance principle: measuring the same constitutional text via different readings yields structurally distinct constraints. Link these stories with network.affects_constraints; do not merge them into one story with measurement parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
