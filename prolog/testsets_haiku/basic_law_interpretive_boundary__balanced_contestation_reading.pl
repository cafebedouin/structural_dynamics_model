% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__balanced_contestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__balanced_contestation_reading, []).

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
 *   constraint_id: basic_law_interpretive_boundary__balanced_contestation_reading
 *   human_readable: Balanced Institutional Authority Over Constitutional Interpretation
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   This constraint describes an institutional settlement in constitutional
 *   democracies: courts hold legitimate authority to interpret Basic Laws and
 *   invalidate contradictory legislation within their jurisdictional domain;
 *   legislatures retain ultimate sovereign power to amend Basic Laws but
 *   operate under international legal obligations and norms of judicial
 *   independence. The balanced-contestation reading models this as a tangled
 *   rope: both institutions benefit from a rule-of-law framework
 *   (coordination function), but the distribution of interpretive authority
 *   produces asymmetric extraction. Minority populations and powerless actors
 *   depend on courts for protection but have no formal voice in either
 *   institution; they experience the constraint as protective and extractive
 *   simultaneously. The institutional boundary is not fixed but negotiated
 *   through case law, legislation, and international pressure — the
 *   constraint itself is the arena of this negotiation, not a settlement that
 *   resolves it. This reading coexists with two siblings: the
 *   judicial-supremacy reading (courts have ultimate authority) and the
 *   parliamentary-sovereignty reading (legislatures do); the
 *   balanced-contestation reading claims neither institution fully dominates
 *   and that the productive tension between them is itself the constraint.
 *
 * KEY AGENTS:
 *   - Supreme Court: interprets within domain, benefits from institutional prestige and case authority, constrained by legislative will and international norms.
 *   - Legislative Assembly: nominally sovereign, constrained by judicial review and international legal obligations, bears costs of invalidation.
 *   - Executive Branch: subject to both judicial and legislative oversight, pays costs of judicial intervention.
 *   - Minority Populations: protected by courts, trapped in jurisdiction, depend on judicial independence for fundamental rights.
 *   - International Human Rights Bodies: beneficiaries of a rights-respecting system, exercise soft power through monitoring and pressure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__balanced_contestation_reading, 0.38).
domain_priors:suppression_score(basic_law_interpretive_boundary__balanced_contestation_reading, 0.52).
domain_priors:theater_ratio(basic_law_interpretive_boundary__balanced_contestation_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__balanced_contestation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__balanced_contestation_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__balanced_contestation_reading, "Balanced Institutional Authority Over Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__balanced_contestation_reading, "constitutional_law/comparative_constitutionalism").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__balanced_contestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__balanced_contestation_reading, 'c6d4afac-16c4-423a-b245-8e91763d859c').
narrative_ontology:cs_kernel_codification('c6d4afac-16c4-423a-b245-8e91763d859c', fixed_text).
narrative_ontology:cs_authority_grounding('c6d4afac-16c4-423a-b245-8e91763d859c', lineage).
narrative_ontology:cs_interpretation_layer_present('c6d4afac-16c4-423a-b245-8e91763d859c').
narrative_ontology:cs_reading_relation('c6d4afac-16c4-423a-b245-8e91763d859c', basic_law_interpretive_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6d4afac-16c4-423a-b245-8e91763d859c', basic_law_interpretive_boundary__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('c6d4afac-16c4-423a-b245-8e91763d859c', foundational, institutional_dual_legitimacy).
narrative_ontology:cs_axiom_status(institutional_dual_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('c6d4afac-16c4-423a-b245-8e91763d859c', institutional_dual_legitimacy, deontological).
narrative_ontology:cs_axiom('c6d4afac-16c4-423a-b245-8e91763d859c', foundational, boundary_contestation_as_equilibrium).
narrative_ontology:cs_axiom_status(boundary_contestation_as_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('c6d4afac-16c4-423a-b245-8e91763d859c', boundary_contestation_as_equilibrium, instrumental).
narrative_ontology:cs_axiom('c6d4afac-16c4-423a-b245-8e91763d859c', secondary, judicial_independence_prerequisite).
narrative_ontology:cs_axiom_status(judicial_independence_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('c6d4afac-16c4-423a-b245-8e91763d859c', judicial_independence_prerequisite, deontological).
narrative_ontology:cs_axiom('c6d4afac-16c4-423a-b245-8e91763d859c', secondary, democratic_accountability_prerequisite).
narrative_ontology:cs_axiom_status(democratic_accountability_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('c6d4afac-16c4-423a-b245-8e91763d859c', democratic_accountability_prerequisite, deontological).
narrative_ontology:cs_reference_frame('c6d4afac-16c4-423a-b245-8e91763d859c', constitutional_dual_authority).
narrative_ontology:cs_drift_state('c6d4afac-16c4-423a-b245-8e91763d859c', contemporary_institutional_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c6d4afac-16c4-423a-b245-8e91763d859c', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, judicial_branch).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, international_human_rights_framework).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, minority_populations).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, executive_unchecked_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, minority_populations).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__balanced_contestation_reading, international_human_rights_bodies).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_assembly).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__balanced_contestation_reading, individual_legislators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets Basic Laws and constitutional principles; invalidates legislation deemed unconstitutional within its jurisdictional domain. Claims authority grounded in rule-of-law doctrine and judicial independence norms. Benefits from institutional prestige, case flow, and interpretive power; constrained by legislative supremacy doctrines and executive pressure. Cannot simply leave the institutional framework; interpretation power is both resource and constraint.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, supreme_court, beneficiary).

% Enacts legislation and ratifies Basic Laws; nominally holds sovereign power but operates under judicial review and international legal obligations. Bears the cost of judicial invalidation of legislation; constrained by international law commitments and norms of judicial independence. Cannot simply override courts without triggering international sanctions and legitimacy costs.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_assembly, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_assembly, payer).

% Implements legislation and court orders; subject to both judicial review and legislative oversight. Bears costs of judicial intervention into executive discretion; faces resistance when courts invalidate executive action. Has significant independent power but operates within the constraint structure; cannot act unilaterally without invoking judicial or legislative response.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, executive_branch, payer,
    powerful, biographical, constrained, national).

% Protected by constitutional constraints on majoritarian legislation; depend on judicial enforcement of minority rights against legislative override. Benefit from court protection but suffer when courts defer to legislative judgment or when the legislature passes laws constraining judicial review itself. Cannot exit the jurisdiction; trapped in the institutional structure.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, minority_populations, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__balanced_contestation_reading, minority_populations, payer).

% Monitor compliance with international human rights obligations; pressure courts to enforce human rights provisions and legislatures to ratify human rights treaties. Benefit from a judicial system that takes human rights seriously; can withdraw engagement or issue critical reports if standards are not met. Have leverage but no binding enforcement power within the national jurisdiction.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, international_human_rights_bodies, beneficiary,
    organized, generational, mobile, global).

% Enact legislation as individuals within the assembly; face personal and collective costs when courts invalidate their legislative work. Can move to other committees or leave office; have some mobility but are structurally embedded in the legislative process. Experience direct confrontation with judicial invalidation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, individual_legislators, payer,
    moderate, biographical, mobile, national).

% Lobby courts and legislature on constitutional matters; have standing to bring cases but no formal authority in interpretation. Would argue for either stronger judicial protection or greater legislative responsiveness depending on their platform. Excluded from the formal decision-making structure; must work through both institutions.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, civil_society_advocates, excluded,
    moderate, biographical, mobile, national).

% Analyze constitutional design and institutional dynamics; produce evidence on comparative performance of different power-sharing arrangements. Have no formal authority but influence framing of the constitutional debate. Can exit by moving to other jurisdictions; purely analytical position.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__balanced_contestation_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__balanced_contestation_reading, legislative_assembly).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__balanced_contestation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates interpretive authority and enforcement power between judicial and legislative branches in a way that coordinates on rule-of-law outcomes while preserving democratic legitimacy. Solves the institutional design problem: how to maintain judicial independence (preventing tyranny) while respecting legislative democracy (preventing judicial oligarchy). Creates mechanisms for institutional dialogue over contested constitutional boundaries.
% TRANSFER_FUNCTION: Transfers interpretive authority from the legislature to the courts within designated jurisdictional domains (fundamental rights, constitutional limits); transfers ultimate authority over constitutional amendment back to the legislature, constrained by international legal obligations. Moves the cost of constitutional compliance from the executive-legislative majority to the minority populations who depend on court protection.
% ABSENT_VOICES: Populations historically excluded from the legislative process (indigenous peoples, non-citizen residents, future generations affected by constitutional decisions) have no formal voice in either institution but are materially affected by how the boundary is drawn. Would argue for stronger judicial protection of their interests. Disabled persons and marginalized groups that cannot effectively lobby either branch are structurally silenced.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared overnight — if courts had no interpretive authority and legislatures could amend Basic Laws by simple majority, unconstrained by international norms — the institutional geometry would reorganize. Majoritarian governments would eliminate constitutional protections on minorities; executives would face no judicial check; international legal obligations would be overridden by legislative will. Civil society, human rights bodies, and minority-protection schemes would attempt to rebuild accountability mechanisms, likely through extrajudicial pressure, constitutional revolt, or international intervention.
% FOUNDING_PROBLEM: How to prevent both tyranny of the majority (legislative overreach against minorities) and tyranny of the elite (unaccountable judicial oligarchy). The problem presupposes two legitimate but competing sources of authority: electoral democracy (legislature) and rule-of-law principle (courts). The founding constraint attempts to honor both without fully subordinating either.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars and international human rights bodies attest the foundational tension is structurally persistent across democracies with judicial review. Judicial independence organizations document ongoing pressure from legislatures to constrain courts. Legislatures in multiple jurisdictions attest the need to maintain authority over constitutional interpretation. The founding problem is corroborated by external observers (not the benefiting parties themselves).
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__balanced_contestation_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__balanced_contestation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__balanced_contestation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__balanced_contestation_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).
:- end_tests(basic_law_interpretive_boundary__balanced_contestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at steady state) because the constraint produces real coordination on rule of law while also enabling institutional power to concentrate interpretive authority asymmetrically. Early in the interval (t=0), extractiveness is lower (0.22) reflecting a period of stronger consensus on institutional boundaries; it rises through t=25 as courts begin invalidating more legislation and legislatures respond by constraining judicial review, widening the gap between the nominal rule-of-law principle and actual institutional practice. By t=25 it plateaus, suggesting institutional stabilization around the contested boundary. Suppression is moderate-high (0.52 at steady state) because the constraint's persistence requires both institutions to actively police the boundary: courts must demonstrate they are not usurping legislative power (performative restraint); legislatures must refrain from simply overriding courts (performative respect for independence). Theater rises early and then stabilizes, indicating that over time institutional actors increasingly engage in boundary-protection theater rather than deepening the substantive dialogue. Resistance is high (0.71) because both institutions and external actors (civil society, international bodies) actively contest the boundary; minority populations and executive accountability advocates continually push for stronger judicial protection. Accessibility collapse is moderate (0.62): alternatives to the current boundary exist in theory (pure judicial supremacy, pure parliamentary sovereignty) but become increasingly structurally locked as the institutional settlement deepens through case law and constitutional practice. The measurement series models institutional drift: initial uncertainty about boundaries, gradual ossification around contested lines, eventual stabilization with increasing performative rather than substantive engagement.
 *
 * PERSPECTIVAL GAP:
 *   The institutional actors (court and legislature) should compute as different seats with different types from the same constraint. The Supreme Court should compute toward rope or lower tangled-rope (genuine coordination benefit, manageable extraction, high exit options — it can refuse cases, interpret narrowly, defer to legislature). The Legislative Assembly should compute toward tangled-rope or snare (extractive power taken by courts, high suppression requirements, constrained exit — it cannot simply override courts without international costs). Minority populations should compute as snare-adjacent (high extraction, high suppression, trapped exit). The balanced-contestation reading itself asserts that neither institution fully dominates, which means their seat-level types should diverge rather than converge. If the engine computes both institutions as the same type, the reading has failed to model the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court sits near d=0.2-0.3 (beneficiary): it collects interpretive authority, institutional prestige, and case flow; it controls the agenda on what counts as constitutionally relevant. The Legislative Assembly sits near d=0.55-0.65 (mixed, slightly toward target): it nominally retains sovereignty but experiences constraints from judicial review and international obligations; it pays costs of invalidation and constrained legislative space, but also benefits from shared rule-of-law legitimacy. The Executive sits near d=0.75-0.85 (target): constrained by both judicial review and legislative oversight with minimal independent authority; bears compliance costs. Minority populations sit at d=0.80-0.95 (strong targets): trapped in the jurisdiction, powerless to shape the institutional boundary, dependent on courts' willingness to protect them. International human rights bodies sit near d=0.1-0.2 (beneficiary): no formal enforcement power but influence the constraint's boundary through monitoring, reporting, and diplomatic pressure. Individual legislators sit near d=0.65 (moderate target): experience direct cost when legislation is invalidated but have some mobility. Civil society sits near d=0.5 (symmetric, analytical): benefits from rights protection but pays through political struggle and has some mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: how to maintain judicial independence while respecting legislative democracy. However, the constraint's function has partially atrophied. Early in the interval, courts actively synthesized constitutional principles from Basic Laws, creating living jurisprudence; legislatures engaged in genuine debate about judicial scope. By mid-interval, courts increasingly engaged in self-restraint performatively (theater rising) without deepening substantive interpretation; legislatures passed constraint-constraining legislation (laws limiting judicial review, constitutional amendments reducing court jurisdiction) more reflexively. The constraint now persists largely through institutional inertia and international pressure, not through the collaborative constitutional dialogue it was designed to enable. The theater metric captures this: institutional actors increasingly go through the motions of respecting the boundary without substantive engagement. Mandatrophy is partial — not full death (the constraint still coordinates on rule-of-law appearance and international obligation compliance) but functional atrophy in the original sense (genuine constitutional dialogue). The constraint has become more snare-like and less rope-like over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_boundary_fixity,
    'Is the boundary between judicial and legislative authority a fixed structural feature, or is it inherently contestable and renegotiated through institutional practice?',
    'Longitudinal study of constitutional practice across multiple democracies: do boundary disputes follow predictable patterns (suggesting fixity) or emerge de novo in each jurisdiction (suggesting contestability)?',
    'If fixed: the constraint models a stable equilibrium and the extractiveness should remain constant. If contestable: extractiveness should oscillate or trend as different political movements push the boundary; the constraint becomes more snare-like during majoritarian pushes to reduce judicial review.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_boundary_fixity, empirical, 'Whether constitutional boundaries are structurally determined or institutionally contested.').

omega_variable(
    international_constraint_externality,
    'To what extent is the measured suppression (0.52) a property of the domestic institutional balance, versus an externally imposed norm through international human rights law?',
    'Comparison of constitutional practice in jurisdictions with strong vs. weak international human rights obligations; examine whether suppression levels differ as a function of treaty commitments.',
    'If predominantly external: the constraint is partly a snare for the legislature (imposed costs from outside) and less a product of genuine domestic institutional balance. If predominantly domestic: the constraint reflects authentic institutional recognition of both legitimacy claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_constraint_externality, empirical, 'Whether suppression is self-imposed or externally imposed through international obligations.').

omega_variable(
    theater_ratio_escalation,
    'As theater_ratio rises (0.28→0.41 over the interval), are institutional actors genuinely reducing substantive constitutional dialogue, or are they adapting communication strategies to increased external scrutiny?',
    'Discourse analysis of judicial opinions and legislative debate: do they show declining engagement with constitutional principles (theater) or shifting rhetorical style (adaptation)?',
    'If theater reflects genuine dialogue decline: the constraint is degrading toward piton (maintained by inertia and international pressure, not substantive commitment). If rhetorical adaptation: theater is a measurement artifact and the constraint remains functionally robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_ratio_escalation, conceptual, 'Whether rising theater ratio indicates functional atrophy or rhetorical evolution.').

omega_variable(
    minority_protection_asymmetry,
    'Do minority populations experience the constraint as primarily protective (coordination benefit) or as extractive (trapped in a system that uses their rights as a bargaining chip)?',
    'Qualitative research with minority-population representatives on their actual experience of court protection vs. legislative threat; longitudinal measurement of minority-rights outcomes.',
    'If primarily protective: the constraint is genuine tangled-rope for minorities (coordination benefit outweighs extraction). If primarily extractive: the constraint is snare for minorities (nominal protection while real power remains with institutional actors).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_protection_asymmetry, empirical, 'Whether minority populations are net protected or net extractive targets of the institutional boundary.').

omega_variable(
    kernel_reading_committer_frame,
    'Is this balanced-contestation reading internally coherent as a commitment-system stance, or does it collapse into one of the sibling readings (judicial supremacy or parliamentary sovereignty) when institutional pressure becomes acute?',
    'Historical examination of constitutional crises in jurisdictions nominally committed to balanced power: in moments of acute institutional conflict, which institution''s authority claim prevails? Does the reading hold or revert to a sibling reading?',
    'If the reading is unstable under pressure: it is a committer stance (ideologically preferred but not structurally stable) rather than a durable institutional settlement. If stable: it reflects genuine equilibrium. This affects how the constraint is classified — a contingent reading vs. a structural type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Whether the balanced-contestation reading is a stable institutional settlement or a contingent committer preference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__balanced_contestation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_boundary__balanced_contestation_reading, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 5, 0.26).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 15, 0.35).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_boundary__balanced_contestation_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_boundary__balanced_contestation_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__balanced_contestation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__balanced_contestation_reading, 0.18).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, basic_law_interpretive_boundary__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, separation_of_powers_institutional_architecture).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__balanced_contestation_reading, international_human_rights_treaty_obligation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the basic_law_interpretive_boundary kernel. The sibling constraints (judicial_supremacy_reading and parliamentary_sovereignty_reading) are structurally distinct constraints with their own ε values, beneficiary/victim sets, and classifications. They share the kernel (the contested commitment to interpret Basic Laws) but differ in how they resolve the boundary between institutions. The balanced_contestation_reading claims neither sibling is fully correct — that productive institutional tension is itself the goal. All three readings share a regulatory domain and are linked in the network; they compete in real constitutional practice but each is a coherent constraint story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
