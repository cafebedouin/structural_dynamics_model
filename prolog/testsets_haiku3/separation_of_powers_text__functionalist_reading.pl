% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Functionalist Separation of Powers: Delegated Authority Framework
 *   domain: constitutional/administrative
 *
 * SUMMARY:
 *   The functionalist reading of separation of powers is a
 *   legal-constitutional commitment that legitimates administrative
 *   delegation and flexible authority-sharing among Congress, the President,
 *   and agencies. It treats the separation-of-powers clause not as a fixed
 *   architectural blueprint but as a structural principle permitting overlap
 *   when an intelligible principle constrains the delegation and meaningful
 *   accountability mechanisms exist. This reading has dominated American
 *   constitutional law since the New Deal, shaping the institutional reality
 *   of the regulatory state. The story models this reading as a rope—genuine
 *   coordination solving a real governance problem (technical complexity,
 *   responsiveness, expertise) coupled with extraction (agencies and
 *   executive gain structural power, regulated parties bear compliance costs,
 *   Congress partially abandons legislative specificity). The constraint is
 *   CLAIMED as rope; the metrics describe modest extractiveness and low
 *   suppression, consistent with coordination that has acquired some
 *   asymmetry over time.
 *
 * KEY AGENTS:
 *   - administrative_agencies: Institutional beneficiaries of the delegation authority; their legitimacy rides on the functionalist reading
 *   - congress: Agenda-setter and beneficiary; delegates but retains oversight authority
 *   - president: Beneficiary through appointive and removal power; constrained by statutory delegations
 *   - courts: Observer with deference doctrine; minimal policing role
 *   - regulated_actors: Payers bearing compliance costs and regulatory constraints
 *   - formalist critics: Excluded from the reading's own framework; treat rigid separation as mandatory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.38).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.22).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Functionalist Separation of Powers: Delegated Authority Framework").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional/administrative").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, '869da5ec-1d2c-4698-b07b-5e294bc34e42').
narrative_ontology:cs_kernel_codification('869da5ec-1d2c-4698-b07b-5e294bc34e42', fixed_text).
narrative_ontology:cs_authority_grounding('869da5ec-1d2c-4698-b07b-5e294bc34e42', lineage).
narrative_ontology:cs_interpretation_layer_present('869da5ec-1d2c-4698-b07b-5e294bc34e42').
narrative_ontology:cs_reading_relation('869da5ec-1d2c-4698-b07b-5e294bc34e42', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('869da5ec-1d2c-4698-b07b-5e294bc34e42', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('869da5ec-1d2c-4698-b07b-5e294bc34e42', foundational, delegation_legitimacy_via_intelligible_principle).
narrative_ontology:cs_axiom_status(delegation_legitimacy_via_intelligible_principle, holdable).
narrative_ontology:cs_axiom_grounding('869da5ec-1d2c-4698-b07b-5e294bc34e42', delegation_legitimacy_via_intelligible_principle, deontological).
narrative_ontology:cs_axiom('869da5ec-1d2c-4698-b07b-5e294bc34e42', foundational, institutional_flexibility_enables_responsive_governance).
narrative_ontology:cs_axiom_status(institutional_flexibility_enables_responsive_governance, holdable).
narrative_ontology:cs_axiom_grounding('869da5ec-1d2c-4698-b07b-5e294bc34e42', institutional_flexibility_enables_responsive_governance, instrumental).
narrative_ontology:cs_reference_frame('869da5ec-1d2c-4698-b07b-5e294bc34e42', new_deal_flexible_authority_sharing).
narrative_ontology:cs_drift_state('869da5ec-1d2c-4698-b07b-5e294bc34e42', contemporary_regulatory_state_entrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('869da5ec-1d2c-4698-b07b-5e294bc34e42', '2026-06-11T14:32:15Z').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, administrative_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, democratic_responsiveness).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, president).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, private_actors_regulated).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derive legitimacy to issue binding regulations, conduct adjudications, and enforce statutory schemes under delegated authority from Congress. The functionalist reading permits this delegation provided Congress supplies an intelligible principle and retains meaningful oversight. Agencies benefit from the framework because it sustains their institutional authority; without it, delegation would be constitutionally suspect and their rulemaking would face systematic challenge.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, administrative_agencies, beneficiary,
    institutional, generational, analytical, national).

% Sets the substantive policy agenda through legislation, delegates implementation to agencies via statutes that supply intelligible principles, retains oversight powers (appropriations, sunset review, legislative veto analogs, confirmations). Benefits by off-loading technical and empirical rulemaking to expert bodies while maintaining democratic accountability through the delegation structure itself. The functionalist frame permits Congress to share legislative function with agencies in service of responsive governance.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__functionalist_reading, congress, beneficiary).

% Appoints agency heads and cabinet officials, exercises removal power (with limits), directs agency policy through executive orders within delegated statutory bounds, participates in regulatory oversight. Under functionalism, presidential authority over the executive branch flows from the Constitution's Vesting Clause but is constrained by Congress's delegations and statutory limits. Retains meaningful control without requiring all executive power to vest exclusively in the presidency.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, president, beneficiary,
    institutional, generational, analytical, national).

% Review agency actions for compliance with statutory delegation standards (intelligible principle), procedural regularity (APA), and constitutionality. The functionalist frame assigns courts a modest policing role: ensuring delegation is genuine (not a blank check) and procedures are fair, but deferring to agency expertise on policy and to Congress on the delegation's scope. Courts do not strike down delegations or agency action merely because separation of powers could be more rigid.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, courts, observer,
    institutional, generational, analytical, national).

% Subject to agency regulations, orders, and enforcement actions. They argue the functionalist frame permits agencies too much discretion and distributes the costs of regulation unevenly. Their formal recourse is judicial review (challenging the regulation's basis, procedure, or constitutionality) or legislative amendment. Exit via non-compliance risks enforcement; migration is constrained by regulatory scope.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, private_actors_regulated, payer,
    powerful, biographical, constrained, national).

% The constitutional role of courts is ambiguous in the functionalist frame: are courts guardians of meaningful delegation boundaries, or do they defer almost entirely to Congressional and agency judgments? The functionalist reading emphasizes deference; critics argue it leaves courts with no real check on legislative-executive collusion. Courts themselves are split on how active a policing role to play.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, courts_structural_role, excluded,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(separation_of_powers_text__functionalist_reading, courts_structural_role).

% The functionalist frame justifies delegation as a means of preserving democratic responsiveness: technical decisions are made by expert bodies accountable through the President and Congress, not by unelected courts or constitutional prohibition. This is the normative claim that sustains the reading—that flexibility preserves legitimacy. Non-agent entry here to track the vindicated normative proposition.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, democratic_responsiveness, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(separation_of_powers_text__functionalist_reading, democratic_responsiveness).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__functionalist_reading, diffuse).
narrative_ontology:fixing_cost_class(separation_of_powers_text__functionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the governance coordination problem: technical policy domains (environmental protection, financial regulation, workplace safety, telecommunications) require continuous expert adjustment beyond what generalist legislators can provide, and separation into rigid branches would prevent responsive lawmaking. The functionalist reading coordinates these three constitutional actors (Congress, President, Courts) around a shared acceptance of flexibility, delegated expertise, and deference doctrines.
% TRANSFER_FUNCTION: Transfers implementation authority from Congress to agencies, in exchange for statutory bounds (intelligible principle) and oversight mechanisms (appropriations control, sunset review, presidential direction). Transfers adjudicatory authority from courts to agencies in specialized domains (administrative law judges, agency appeals processes). Transfers political accountability from direct legislative control to a mixed structure of Congressional delegation + Presidential appointment + agency expertise.
% ABSENT_VOICES: Formalist critics (strict separationists who demand rigid boundaries) are structurally excluded from the functionalist reading's own framework—the reading treats their objection as a misunderstanding of constitutional text and practice, not a live alternative. Voices arguing for stronger judicial review of delegations are marginalized by functionalist deference doctrines. Non-delegators and those favoring direct democratic input (referenda, town halls, direct regulation without agency intermediary) are not represented in the institutional framing.
% DISAPPEARANCE_RATIONALE: If the functionalist frame vanished and strict formalism took its place, regulatory agencies would lose legitimacy for their legislative and adjudicatory functions; Congressional delegation would face constitutional challenge; the modern administrative state would collapse or be radically restructured. Congress would be forced to legislate with greater specificity and frequency, the President would lose appointive control over vast executive domains, and courts would face cascading constitutional suits. Governance would reorganize around either rigid separation or expanded Presidential unitary authority, not remain in the functionalist flexibility state.
% FOUNDING_PROBLEM: Mid-20th century (post-New Deal): rapid social and technological change (labor relations, environmental degradation, financial markets, communications) outpaced legislative capacity; generalist Congress could not produce detailed, responsive, empirically grounded regulation without expert bureaucracy. The founding problem was: how to legitimate non-legislative rulemaking and executive adjustment without abandoning constitutional structure?
% FOUNDING_PROBLEM_CORROBORATION: Functionalist scholars (Herring, Sunstein, others) attest the founding problem persists: modern governance is impossible without delegation and flexible boundaries. Formalists argue the problem is overstated and results from legislative abdication, not structural necessity. Non-US democracies (UK, Canada, Australia) use parliamentary oversight rather than separation-of-powers courts to govern delegation, suggesting the problem is real but solvable via alternate mechanisms. The founding problem itself—whether legislative capacity is genuinely limited—is corroborated outside the functionalist beneficiary set by comparative institutional analysis and by historical evidence of legislative drafting difficulty in complex domains.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness starts low (0.28) because the constraint is genuinely responsive to a real coordination problem (agencies solve governance problems legislators cannot). It rises gradually to 0.40 by t=40, as agencies accumulate discretionary authority, regulatory scope expands, and the discretion becomes decoupled from specific Congressional guidance (Goodhart drift: theater increases, reflecting agency policy-making rather than pure implementation). The rise then plateaus and even reverses slightly (0.38 at t=70), as Congressional oversight mechanisms (confirmations, appropriations riders, oversight hearings) and judicial review reestablish bounds on agency discretion—the system self-corrects when extraction becomes too obvious. Suppression is persistently low (0.22 at interval end) because the constraint persists through deference doctrines and legitimacy claims, not through coercive force. The functional arrangement is defended intellectually and institutionally, not imposed against resistance. Theater is modest and relatively stable (0.08 to 0.19): the legitimacy framing is real but has grown more performative as the original coordination problem (technical complexity) has become entrenched in institutional power rather than a genuine need.
 *
 * PERSPECTIVAL GAP:
 *   From the agency and Congressional seats, the constraint is genuine coordination: agencies genuinely solve hard problems (environmental law, telecommunications, labor relations) that generalist legislators cannot handle with specificity and responsiveness. From the regulated-actor seat (businesses, individuals subject to agency rules), the same structure operates as extraction: compliance costs are high, discretion is broad, and exit is constrained. From the formalist critic seat (excluded from the functionalist frame), the constraint is a constitutional violation—an illegitimate delegation of legislative power. Courts sit in a peculiar position: the functionalist reading assigns them a minimal policing role (deference), so they experience the constraint as requiring them to suppress their own structural authority (checking power) in service of institutional flexibility. The engine should compute different types per seat: agencies and Congress see rope; regulated actors see tangled_rope or snare; formalists see constraint-under-contest; courts see scaffold (transitional support for a delegating Congress and powerful President, but the delegation's permanence leaves courts without a termination condition).
 *
 * DIRECTIONALITY LOGIC:
 *   Agencies: d near 0.1–0.2 (strong beneficiaries—direct legitimacy and discretion gain; low exit: their entire institutional existence depends on the reading). Congress: d near 0.3–0.4 (beneficiary overall: off-loads hard work; moderate exit: can revoke or tighten delegation, but only at high political cost). President: d near 0.2–0.3 (beneficiary through control; moderate exit: bound by statutory delegations but can redirect priorities through appointment and executive order). Courts: d near 0.4–0.5 (symmetric to slightly extractive: must accept deference doctrines that bind them, but gain legitimacy from institutional stability and the avoidance of constitutional deadlock). Regulated actors: d near 0.8–0.9 (full targets: compliance costs fall on them; constrained exit: litigation and migration are costly, non-compliance brings enforcement). The override mechanism: if formalist judicial pressure rises, courts' d should shift toward 0.6–0.7 (courts become targets of institutional pressure to enforce rigid separation, extracting from them the cost of saying 'no' to delegation). These overrides would capture formalist challenges (Nondelegation Doctrine revival, Unitary Executive pressure) as redistributing directionality from agencies to courts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legislative capacity under technical complexity) was live in the New Deal and mid-20th century. By the late 20th century, it is contested: some attest agencies have become so powerful that the delegation story is cover for executive aggrandizement, not a response to legislative incapacity. Formalist revival (since 2020s) argues the founding problem is overblown and the constraint persists as inertia and institutional capture. The functionalist reading does NOT claim the founding problem is solved—it claims flexibility is how you preserve responsive governance when problems are persistent and technical. But the measurement series shows theater increasing from 0.08 to 0.19, suggesting the legitimacy framing (flexibility serving responsiveness) is becoming performative: agencies are described as technical problem-solvers, but their actual discretion often reflects policy choices not empirical necessity. This is the classic mandatrophy signal: the constraint persists but its original justification has been partially overwritten. Measured extractiveness plateaus rather than rising monotonically, suggesting Congress and courts have periodically tightened the constraint when it became too obviously extractive—a cyclical correction mechanism that prevents total capture but does not restore the original balance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intelligible_principle_standard_ambiguity,
    'What counts as an intelligible principle sufficiently constraining agency delegation? Is ''public interest,'' ''just and reasonable,'' or ''consumer welfare'' adequately specific, or do they permit virtually any agency choice?',
    'Historical review of delegation challenges: which statutory standards survived or failed judicial nondelegation review? Empirical study of agency discretion in practice under vague standards.',
    'If vague standards are sufficient, the constraint provides little real bound on delegation and is closer to snare than rope (intelligible principle becomes mere cover). If courts enforce strict specificity, the constraint is genuinely coordinative (Congress must be precise, agencies truly bounded). The resolution determines whether the reading''s legitimacy claim is substantive or performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intelligible_principle_standard_ambiguity, empirical, 'Whether intelligible principle doctrine actually constrains agency discretion or is a façade.').

omega_variable(
    deference_doctrine_stability,
    'Is judicial deference to agency action (Chevron, Auer doctrines) a stable feature of functionalist separation of powers, or is deference itself contested and subject to revision?',
    'Track Supreme Court doctrine on agency review standards: does deference persist, narrow, or reverse? Monitor formalist revival pressure and its success in reshaping deference rules.',
    'If deference is stable, courts genuinely accept the functionalist frame and enforce modest bounds. If deference erodes, courts shift from observer to potential contestant, and the constraint''s extractiveness may rise (agencies face stronger challenges). The reading''s own structural stability depends on deference persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deference_doctrine_stability, empirical, 'Whether deference doctrine remains binding or is subject to formalist challenge and revision.').

omega_variable(
    congressional_capacity_vs_delegation_cover,
    'Is delegation genuinely necessary because legislative capacity is limited, or does delegation persist because Congress prefers to avoid hard political choices and delegates responsibility to agencies?',
    'Comparative legislative analysis: do other democracies (UK, Canada, Australia) legislate with greater specificity while achieving equivalent technical responsiveness? Historical counterfactual: could Congress legislate more specifically if it chose to?',
    'If delegation is necessity-driven, the functionalist reading is coordinative and the constraint deserves the rope classification. If delegation is political preference-driven, the reading is cover for legislative abdication, and extraction is higher than the metrics suggest (theater increases, legitimacy is thin).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congressional_capacity_vs_delegation_cover, conceptual, 'Whether delegation reflects genuine legislative capacity limits or political choice to avoid difficult legislating.').

omega_variable(
    formalist_reading_foreclosure,
    'Is the functionalist reading''s acceptance of delegation and flexible boundaries logically incompatible with the formalist reading''s demand for rigid, impermeable separation, or can both coexist in a single constitutional framework (different parties, different times)?',
    'Constitutional doctrine history: when formalist doctrine (nondelegation doctrine revival, strict separation cases) has pressed, has the functionalist frame folded, or have both persisted in tension? Can a single judge or institution hold both?',
    'If they truly foreclose each other, one reading is the gate to the other''s invalidation—foreclosure in the cs_structure.reading_relations sense. If they coexist in tension (different factions of the Court, different administrations), the relation is coexists_with. This affects the network topology: are the siblings rival constraints or occupants of the same contested domain?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formalist_reading_foreclosure, conceptual, 'Whether functionalism and formalism are logically incompatible or persistently coexistent doctrines.').

omega_variable(
    agency_capture_rent_seeking,
    'To what extent does the functionalist frame enable agency capture by regulated industries, where agencies come to represent industry interests rather than public welfare?',
    'Empirical study of agency-industry relationship formation, revolving door patterns, lobbying influence on rulemaking. Survey of scholars and practitioners on capture prevalence.',
    'High capture indicates the constraint''s extraction component is larger than the metrics suggest—the constraint is not just permitting beneficial delegation but enabling rent-seeking coalitions. The coordination function (solving technical problems) masks extraction (agencies benefiting industry). This would shift the classification boundary toward tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_capture_rent_seeking, empirical, 'Whether the functionalist delegation framework is systematically captured by regulated interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__functionalist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sepa_tr_t10, separation_of_powers_text__functionalist_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(sepa_tr_t20, separation_of_powers_text__functionalist_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(sepa_tr_t30, separation_of_powers_text__functionalist_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(sepa_tr_t40, separation_of_powers_text__functionalist_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(sepa_tr_t50, separation_of_powers_text__functionalist_reading, theater_ratio, 50, 0.19).
narrative_ontology:measurement(sepa_tr_t60, separation_of_powers_text__functionalist_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(sepa_tr_t70, separation_of_powers_text__functionalist_reading, theater_ratio, 70, 0.18).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__functionalist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sepa_be_t10, separation_of_powers_text__functionalist_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(sepa_be_t20, separation_of_powers_text__functionalist_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(sepa_be_t30, separation_of_powers_text__functionalist_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(sepa_be_t40, separation_of_powers_text__functionalist_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(sepa_be_t50, separation_of_powers_text__functionalist_reading, base_extractiveness, 50, 0.39).
narrative_ontology:measurement(sepa_be_t60, separation_of_powers_text__functionalist_reading, base_extractiveness, 60, 0.38).
narrative_ontology:measurement(sepa_be_t70, separation_of_powers_text__functionalist_reading, base_extractiveness, 70, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__functionalist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(sepa_su_t10, separation_of_powers_text__functionalist_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement(sepa_su_t20, separation_of_powers_text__functionalist_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(sepa_su_t30, separation_of_powers_text__functionalist_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(sepa_su_t40, separation_of_powers_text__functionalist_reading, suppression_requirement, 40, 0.23).
narrative_ontology:measurement(sepa_su_t50, separation_of_powers_text__functionalist_reading, suppression_requirement, 50, 0.22).
narrative_ontology:measurement(sepa_su_t60, separation_of_powers_text__functionalist_reading, suppression_requirement, 60, 0.22).
narrative_ontology:measurement(sepa_su_t70, separation_of_powers_text__functionalist_reading, suppression_requirement, 70, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__functionalist_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, separation_of_powers_text__unitary_executive_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, chevron_deference_doctrine).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, administrative_procedure_act_framework).

% DUAL FORMULATION NOTE:
% This constraint (functionalist_reading) is one reading of the kernel separation_of_powers_text. Sibling readings include formalist_reading (strict impermeable boundaries) and unitary_executive_reading (all executive power vests in President). These are separate constraint stories because they instantiate different ε values, different beneficiary/victim structures, and different type classifications, despite sharing the same kernel. The functionalist reading treats delegation as legitimate and agencies as beneficiaries (lower ε, rope); the formalist reading treats delegation as unconstitutional and agencies as extractive (higher ε, snare); the unitary reading treats independent agencies as violating presidential authority (high ε, snare from the presidential seat). All three instantiate the same constitutional text but with incompatible readings. Link them via network.affects_constraints to indicate they occupy the same contested domain and compete for institutional influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers_text__functionalist_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
