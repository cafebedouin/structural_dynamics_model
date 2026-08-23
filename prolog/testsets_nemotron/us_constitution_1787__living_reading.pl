% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__living_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__living_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: us_constitution_1787__living_reading
 *   human_readable: Living Constitution Reading — Evolving Meaning
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The living constitutional reading holds that the Constitution's meaning
 *   evolves with society's changing values and circumstances; the 1787 text
 *   is an aspirational framework whose open-textured provisions (due process,
 *   equal protection, cruel and unusual punishment, privileges or immunities)
 *   authorize each generation to recognize new rights and limits. This
 *   reading became dominant in the mid-20th century (Brown, Griswold, Roe,
 *   Obergefell) and structures modern constitutional litigation. It claims to
 *   solve the founding problem of a rigid, incomplete founding document, but
 *   its operation transfers substantial authority to courts and the
 *   interpretive elite. The constraint is claimed as tangled_rope: it
 *   performs genuine coordination (adaptation without Article V) but also
 *   extracts from originalist constituencies and democratic majorities whose
 *   preferred commitments are overridden.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__living_reading, 0.35).
domain_priors:suppression_score(us_constitution_1787__living_reading, 0.15).
domain_priors:theater_ratio(us_constitution_1787__living_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_1787__living_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__living_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__living_reading, "Living Constitution Reading — Evolving Meaning").
narrative_ontology:topic_domain(us_constitution_1787__living_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__living_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__living_reading, 'a99b7509-8dcd-4719-bbf5-7ac815de5bd8').
narrative_ontology:cs_kernel_codification('a99b7509-8dcd-4719-bbf5-7ac815de5bd8', fixed_text).
narrative_ontology:cs_authority_grounding('a99b7509-8dcd-4719-bbf5-7ac815de5bd8', lineage).
narrative_ontology:cs_interpretation_layer_present('a99b7509-8dcd-4719-bbf5-7ac815de5bd8').
narrative_ontology:cs_reading_relation('a99b7509-8dcd-4719-bbf5-7ac815de5bd8', us_constitution_1787__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('a99b7509-8dcd-4719-bbf5-7ac815de5bd8', us_constitution_1787__positivist_reading, influences).
narrative_ontology:cs_axiom('a99b7509-8dcd-4719-bbf5-7ac815de5bd8', foundational, constitutional_meaning_evolves_with_society).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_society, holdable).
narrative_ontology:cs_axiom_grounding('a99b7509-8dcd-4719-bbf5-7ac815de5bd8', constitutional_meaning_evolves_with_society, instrumental).
narrative_ontology:cs_axiom('a99b7509-8dcd-4719-bbf5-7ac815de5bd8', foundational, judicial_branch_authoritative_interpreter_of_evolving_meaning).
narrative_ontology:cs_axiom_status(judicial_branch_authoritative_interpreter_of_evolving_meaning, holdable).
narrative_ontology:cs_axiom_grounding('a99b7509-8dcd-4719-bbf5-7ac815de5bd8', judicial_branch_authoritative_interpreter_of_evolving_meaning, conventional).
narrative_ontology:cs_reference_frame('a99b7509-8dcd-4719-bbf5-7ac815de5bd8', ratification_era_understandings_as_baseline).
narrative_ontology:cs_drift_state('a99b7509-8dcd-4719-bbf5-7ac815de5bd8', contemporary_rights_jurisprudence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a99b7509-8dcd-4719-bbf5-7ac815de5bd8', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__living_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, constitutional_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, judicial_branch).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, legal_academy).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, originalist_constituency).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, democratic_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__living_reading, state_governments).
narrative_ontology:constraint_victim(us_constitution_1787__living_reading, state_governments).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, constitutional_adequacy_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, evolving_standards_of_decency).
narrative_ontology:constraint_vindicates(us_constitution_1787__living_reading, judicial_guardianship_role).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups asserting new or expanded constitutional rights (privacy, dignity, bodily autonomy, digital liberties) who depend on courts reading the Constitution as an evolving framework. Their claims succeed or fail based on whether judges accept that constitutional meaning can grow beyond ratification-era understandings. Exit means abandoning constitutional litigation for statutory or democratic channels — slower, less certain, and often unavailable for minority-protective claims.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, constitutional_claimants, beneficiary,
    organized, biographical, constrained, national).

% Courts — especially the Supreme Court — that authoritatively declare what the Constitution means today. The living reading expands judicial discretion and institutional prominence: courts become the primary site where constitutional meaning is negotiated across generations. They benefit from enlarged authority but also bear the burden of legitimacy; their exit is constrained by Article III tenure and the absence of any other authoritative interpreter.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, judicial_branch, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, judicial_branch, beneficiary).

% Law professors, scholars, and elite law schools that produce the doctrinal frameworks, historical narratives, and normative arguments that courts cite. The living reading creates sustained demand for scholarly interpretation, methodology debates, and 'evolving standards' research programs. Their exit is relatively mobile — scholars can shift to other fields or methodologies — but professional incentives and institutional prestige align with the living reading's continuation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, legal_academy, beneficiary,
    organized, biographical, mobile, national).

% Citizens, officials, and movements committed to the view that constitutional meaning was fixed at ratification. They experience the living reading as extraction: their preferred constitutional commitments (federalism limits, property protections, non-delegation, original public meaning) are overridden by judicial updates they never consented to. Their exit is identity-locked — abandoning originalism means surrendering a core constitutional identity and the movement infrastructure built around it. They cannot 'opt out' of Supreme Court precedents that bind them.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, originalist_constituency, payer,
    organized, generational, identity_locked, national).

% Legislative majorities and electorates whose enacted policies are invalidated by courts invoking evolving constitutional meaning. They bear the cost of having democratic outputs reversed by unelected judges applying standards not found in the ratified text. Exit is constrained: constitutional amendment is practically impossible (Article V supermajorities), jurisdiction-stripping is politically toxic, and court-packing destroys institutional legitimacy. They are structurally trapped under judicial updates they cannot reverse through ordinary politics.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, democratic_majorities, payer,
    moderate, biographical, constrained, national).

% State governments experience the living reading ambivalently. As payers: federal courts invalidate state laws on evolving rights grounds (abortion, voting rights, gun regulation, environmental policy), displacing state-level democratic choices. As beneficiaries: the same framework sometimes protects state autonomy from federal overreach (anti-commandeering, sovereign immunity). Their exit is constrained — they cannot leave the federal system, but they can sometimes forum-shop or resist implementation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, state_governments, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__living_reading, state_governments, beneficiary).

% Scholars who study constitutional dynamics empirically — measuring judicial behavior, public opinion, diffusion of rights claims, and institutional legitimacy. They neither collect rents nor pay them; they observe how the living reading operates across time and regimes. Their analytical seat is mobile across research programs but institutionally anchored in the discipline.
narrative_ontology:constraint_stakeholder(us_constitution_1787__living_reading, political_science_community, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable mechanism for constitutional adaptation without requiring Article V amendments — which are practically unavailable — allowing the constitutional order to incorporate new rights claims (privacy, dignity, equality extensions) and respond to technological and social change while maintaining the Constitution's symbolic authority and institutional continuity.
% TRANSFER_FUNCTION: Moves constitutional decision-making authority from the ratification-era public (fixed meaning) and democratic majorities (legislative outputs) to the judicial branch (especially the Supreme Court) and the interpretive community (legal academy, litigators) that supplies the evolving content. The transfer is not purely zero-sum: claimants gain enforceable rights; originalists lose binding force of original meaning; democratic majorities lose policy control on contested issues.
% ABSENT_VOICES: Future generations who will inherit the constitutional framework but cannot participate in its current evolution; non-litigating citizens whose constitutional understandings are not mediated by elite legal discourse; state and local officials who implement judicial mandates without voice in their creation; the founding generation whose ratification-era understandings are structurally overridden — they are absent by definition but their exclusion is the living reading's central claim.
% DISAPPEARANCE_RATIONALE: If the living reading vanished overnight and originalism became the only authoritative method, the constitutional constraint set would contract dramatically: recognized rights to abortion, same-sex marriage, contraception, digital privacy, and many equality protections would lose constitutional grounding. Democratic majorities would regain policy control on these issues. The judicial branch would lose its primary source of generational relevance. The legal academy's central constitutional project would collapse. The symbolic authority of the Constitution as a 'living charter' would be replaced by a fixed-text regime.
% FOUNDING_PROBLEM: The Constitution of 1787 contained no bill of rights, permitted slavery, excluded women from suffrage, and provided no mechanism for ordinary adaptation — Article V's supermajority thresholds made formal amendment nearly impossible for a continental republic. The founding problem was how a rigid, incomplete, and morally compromised founding document could remain legitimate and functional across centuries of unforeseen change.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Federalist Papers (Federalist 43 on amendment difficulty), the Reconstruction Amendments' framers (who expected Section 5 enforcement to evolve), and early 20th-century progressives (who experienced Article V paralysis). It is contested by originalists who argue the founding solution was democratic amendment, not judicial updating — attested by Ratification-era debates, early congressional practice, and the text of Article V itself. No neutral arbiter resolves this; the dispute is structural.
narrative_ontology:disappearance_verdict(us_constitution_1787__living_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__living_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__living_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(us_constitution_1787__living_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__living_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__living_reading_tests).
:- end_tests(us_constitution_1787__living_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate — the living reading does not simply take; it enables rights claims that many experience as liberation. But it extracts binding authority from originalists and policy control from democratic majorities, and that extraction has grown over time as the recognized rights catalog expands. Suppression (0.15) is low — no one is jailed for originalism; but the constraint's persistence depends on active judicial enforcement (court decisions, stare decisis, institutional resistance to originalist appointments). Theater (0.25) is modest but rising — methodological debates (originalism vs. living constitutionalism) increasingly perform the appearance of constraint while outcomes track ideological alignment. Accessibility collapse (0.45) reflects that alternatives (originalism, popular constitutionalism, democratic amendment) remain structurally available but are politically or institutionally marginalized. Resistance (0.4) is significant — originalism has built a powerful intellectual, institutional, and political counter-movement.
 *
 * PERSPECTIVAL GAP:
 *   From the claimant/judicial seat, the living reading is coordination: it solves the adaptation problem Article V cannot. From the originalist/democratic seat, it is extraction: it transfers authority to an unelected judiciary using a methodology with no fixed anchor. The engine computes this seat divergence from the declared roles, power atoms, and exit options. The claimed_type (tangled_rope) captures the structural reality that both coordination and extraction are simultaneously true — the constraint is not one or the other depending on perspective; it is both in its structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Constitutional claimants and the judicial branch are structural beneficiaries (d near 0.0–0.2): they gain authority, relevance, and enforceable rights from the living reading's operation. The legal academy benefits intellectually and professionally (d ~ 0.2). Originalist constituencies are structural targets (d ~ 0.8–0.9): they experience the living reading as illegitimate extraction of their constitutional commitments, with identity-locked exit. Democratic majorities are targets (d ~ 0.7) with constrained exit — they bear policy reversals they cannot easily reverse. State governments are ambivalent (d ~ 0.5): sometimes payers, sometimes beneficiaries. The political science community sits at analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Article V paralysis, incomplete rights catalog) remains live in the sense that formal amendment is still practically impossible and new rights claims (digital privacy, algorithmic due process, climate rights) keep emerging. But the living reading's mandated function — adaptation — has expanded beyond the founding problem into a self-justifying project of continuous rights recognition. The mandate has not atrophied (the problem persists) but it has metastasized: the reading now generates its own demand for new recognitions, and the interpretive elite benefits from this expansion. This is not classic mandatrophy (function dead, constraint persists) but mandate creep (function alive, scope unbounded). The omega on elite capture addresses this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_capture_of_evolving_norms,
    'Does the living reading''s open-ended methodology allow elite legal and cultural actors to project their own policy preferences onto ''evolving standards of decency,'' making the constraint a vehicle for elite capture rather than genuine intergenerational adaptation?',
    'Compare the trajectory of recognized rights against independent measures of public opinion, elite opinion, and cross-national diffusion patterns. If rights recognition consistently leads or diverges from broad public sentiment but tracks elite legal-academic-media consensus, the capture hypothesis gains support.',
    'If elite capture is substantial, the living reading''s coordination function is compromised — it coordinates elite preference rather than social evolution. Extraction from democratic majorities and originalist constituencies increases. The constraint shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_capture_of_evolving_norms, empirical, 'Whether ''evolving norms'' methodology is captured by interpretive elites').

omega_variable(
    coordination_extraction_separability,
    'Is the adaptation function (coordination) structurally separable from the rights-expansion function (extraction), or does the living reading inevitably couple them such that any mechanism for constitutional adaptation also empowers judicial rights creation?',
    'Examine historical alternatives: popular constitutionalism (Jefferson, Jackson), departmentalism, legislative constitutionalism, state-level rights expansion. If adaptation occurred without judicial rights creation in other eras or regimes, the functions are separable.',
    'If inseparable, the tangled_rope classification is structurally necessary — the coordination function cannot exist without the extraction. If separable, the living reading as practiced is a specific institutional choice that bundles them, and alternative coordination mechanisms exist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether constitutional adaptation requires judicial rights expansion').

omega_variable(
    living_originalist_foreclosure,
    'Does the living reading''s core premise (meaning evolves) logically foreclose the originalist reading''s core premise (meaning fixed) within a single coherent constitutional framework, or can a framework accommodate both as operating at different levels (e.g., fixed core + evolving penumbra)?',
    'Analyze whether any coherent constitutional theory simultaneously affirms both: that the Constitution''s meaning is authoritatively fixed at ratification AND that it authoritatively evolves with society. If no such theory exists without contradiction, foreclosure holds.',
    'If foreclosure holds, the two readings cannot coexist in one framework — they are rival regimes. The engine''s forecloses relation is validated. If they can coexist (e.g., fixed original meaning for structural provisions, evolving for rights provisions), the relation is coexists_with or influences.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_originalist_foreclosure, conceptual, 'Whether living and originalist readings are logically incompatible within one framework').

omega_variable(
    cs_framing_underdetermination,
    'Does the living reading''s commitment-system structure ground authority in the constitutional text-as-kernel (fixed_text), in the judicial interpretive tradition (lineage), or in the functional need for constitutional adequacy (extraction)? The cs_structure declaration chooses one, but the living reading''s rhetoric invokes all three.',
    'Trace which authority ground the living reading''s actual practice relies on when pressed: when courts justify new rights, do they cite text, precedent, or functional necessity? The dominant justificatory mode reveals the operative grounding.',
    'If authority_grounding is extraction (functional adequacy), the living reading admits its legitimacy depends on outcomes, not pedigree — this aligns with the tangled_rope claim. If lineage or fixed_text, the reading''s self-presentation is more continuous with the kernel than its operation suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Which authority ground the living reading actually operates from').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__living_reading, 1787, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1787, us_constitution_1787__living_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(us_c_tr_t1868, us_constitution_1787__living_reading, theater_ratio, 1868, 0.08).
narrative_ontology:measurement(us_c_tr_t1937, us_constitution_1787__living_reading, theater_ratio, 1937, 0.12).
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_1787__living_reading, theater_ratio, 1954, 0.15).
narrative_ontology:measurement(us_c_tr_t1973, us_constitution_1787__living_reading, theater_ratio, 1973, 0.2).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__living_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_1787__living_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1787, us_constitution_1787__living_reading, base_extractiveness, 1787, 0.1).
narrative_ontology:measurement(us_c_be_t1868, us_constitution_1787__living_reading, base_extractiveness, 1868, 0.15).
narrative_ontology:measurement(us_c_be_t1937, us_constitution_1787__living_reading, base_extractiveness, 1937, 0.25).
narrative_ontology:measurement(us_c_be_t1954, us_constitution_1787__living_reading, base_extractiveness, 1954, 0.28).
narrative_ontology:measurement(us_c_be_t1973, us_constitution_1787__living_reading, base_extractiveness, 1973, 0.32).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__living_reading, base_extractiveness, 2000, 0.34).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_1787__living_reading, base_extractiveness, 2025, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1787, us_constitution_1787__living_reading, suppression_requirement, 1787, 0.05).
narrative_ontology:measurement(us_c_su_t1868, us_constitution_1787__living_reading, suppression_requirement, 1868, 0.1).
narrative_ontology:measurement(us_c_su_t1937, us_constitution_1787__living_reading, suppression_requirement, 1937, 0.12).
narrative_ontology:measurement(us_c_su_t1954, us_constitution_1787__living_reading, suppression_requirement, 1954, 0.1).
narrative_ontology:measurement(us_c_su_t1973, us_constitution_1787__living_reading, suppression_requirement, 1973, 0.13).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__living_reading, suppression_requirement, 2000, 0.14).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_1787__living_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__living_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__living_reading, 0.08).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, us_constitution_1787__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, judicial_review_power).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, substantive_due_process_doctrine).
narrative_ontology:affects_constraint(us_constitution_1787__living_reading, unenumerated_rights_jurisprudence).

% DUAL FORMULATION NOTE:
% The us_constitution_1787 kernel decomposes into three constraint stories: living_reading (this file, tangled_rope, expanding constraint set), originalist_reading (mountain or tangled_rope depending on enforcement), positivist_reading (rope or scaffold). The living reading's extractiveness comes from judicial rights expansion; the originalist reading's extractiveness (if any) comes from blocking democratic adaptation; the positivist reading's extractiveness is minimal — it constrains courts to text and democratic amendment. They form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__living_reading, institutional, 0.15).
constraint_indexing:directionality_override(us_constitution_1787__living_reading, organized, 0.75).
constraint_indexing:directionality_override(us_constitution_1787__living_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
