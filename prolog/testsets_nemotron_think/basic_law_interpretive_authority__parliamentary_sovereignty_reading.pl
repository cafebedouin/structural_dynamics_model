% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Reading of Basic Law Interpretive Authority
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the parliamentary_sovereignty_reading
 *   of the basic_law_interpretive_authority kernel. The reading asserts that
 *   an elected legislature retains final interpretive authority over
 *   constitutional meaning through its democratic mandate and representative
 *   accountability. This creates a tangled rope: genuine coordination
 *   (democratic resolution of constitutional disputes, prevention of judicial
 *   deadlock) combined with asymmetric extraction (legislative majorities can
 *   override judicial protections for minorities and entrench their
 *   interpretive preferences). The constraint requires active enforcement —
 *   the legislature must actively exercise its interpretive authority through
 *   legislation, appointments, and constitutional amendments to maintain the
 *   arrangement. The sibling readings (judicial_supremacy_reading,
 *   popular_constitutionalism_reading) instantiate different constraints from
 *   the same kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.65).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty Reading of Basic Law Interpretive Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'd5dcea7f-6675-4288-bd62-100de6fa70de').
narrative_ontology:cs_kernel_codification('d5dcea7f-6675-4288-bd62-100de6fa70de', formalized).
narrative_ontology:cs_authority_grounding('d5dcea7f-6675-4288-bd62-100de6fa70de', lineage).
narrative_ontology:cs_interpretation_layer_present('d5dcea7f-6675-4288-bd62-100de6fa70de').
narrative_ontology:cs_reading_relation('d5dcea7f-6675-4288-bd62-100de6fa70de', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d5dcea7f-6675-4288-bd62-100de6fa70de', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('d5dcea7f-6675-4288-bd62-100de6fa70de', foundational, parliamentary_sovereignty_axiom).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_axiom, holdable).
narrative_ontology:cs_axiom_grounding('d5dcea7f-6675-4288-bd62-100de6fa70de', parliamentary_sovereignty_axiom, conventional).
narrative_ontology:cs_axiom('d5dcea7f-6675-4288-bd62-100de6fa70de', secondary, legislative_override_legitimacy).
narrative_ontology:cs_axiom_status(legislative_override_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('d5dcea7f-6675-4288-bd62-100de6fa70de', legislative_override_legitimacy, conventional).
narrative_ontology:cs_reference_frame('d5dcea7f-6675-4288-bd62-100de6fa70de', parliamentary_supremacy_framework).
narrative_ontology:cs_drift_state('d5dcea7f-6675-4288-bd62-100de6fa70de', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d5dcea7f-6675-4288-bd62-100de6fa70de', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, governing_majority).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, independent_judiciary).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_principles).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, political_opposition).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__parliamentary_sovereignty_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__parliamentary_sovereignty_reading, democratic_mandate_theory).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__parliamentary_sovereignty_reading, legislative_supremacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final interpretive authority over the basic law through democratic mandate. Exercises this authority by enacting legislation that overrides judicial interpretations, controlling judicial appointments, and setting the constitutional agenda. Collects institutional authority and policy control as benefits. Can entrench its interpretive preferences through supermajority requirements or constitutional amendments.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% The parliamentary majority that controls the legislature. Gains policy implementation certainty and protection from judicial invalidation of its agenda. Benefits from the ability to define constitutional meaning in line with its electoral mandate. Exit is mobile — can become opposition after elections but retains influence through party structures.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, governing_majority, beneficiary,
    powerful, biographical, mobile, national).

% Courts that exercise judicial review but lack final interpretive authority. Bear the cost of having their constitutional interpretations overridden by legislative action. Their independence is structurally constrained by legislative control over jurisdiction, appointments, and remedies. Exit is constrained — judges cannot easily leave the system but can resist through interpretive creativity or procedural delays.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, independent_judiciary, payer,
    institutional, generational, constrained, national).

% Minority communities whose rights depend on judicial enforcement against legislative majorities. Bear concentrated costs when legislative overrides remove rights protections. Exit is trapped — cannot leave the polity, lack political power to change legislative composition, and face identity-locked vulnerability to majority-defined constitutional meaning.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities, payer,
    powerless, generational, trapped, national).

% The abstract constitutional principles (rule of law, separation of powers, fundamental rights) that require stable interpretation beyond electoral cycles. Bear the cost of instrumentalization when constitutional meaning becomes a tool of temporary majorities. Non-agent entity listed for narrative completeness; does not feed directionality.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_principles, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_principles).

% Parties and actors outside the governing majority. Would object to legislative interpretive monopoly but are structurally excluded from exercising it until they win elections. Bear costs when in opposition (inability to check majority interpretations) but benefit when in government. Exit is constrained — must wait for electoral cycles to change position.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, political_opposition, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, political_opposition, payer).

% Academic commentators who analyze the constraint from outside the political process. Provide theoretical frameworks for evaluating parliamentary sovereignty vs. judicial supremacy. Neither collect nor pay; their exit is analytical (can change frameworks without material cost).
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% Supranational courts (e.g., ECHR, UN treaty bodies) that review domestic compliance with international obligations. Observe the constraint's operation as it affects rights protections. Can exert external pressure but lack domestic enforcement power. Exit is mobile — can engage or disengage from specific state reviews.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, international_courts, observer,
    institutional, generational, mobile, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves constitutional ambiguity and inter-branch conflict through democratic accountability: the elected legislature, accountable to voters, provides a single authoritative interpretation that prevents judicial deadlock and ensures constitutional meaning tracks popular will.
% TRANSFER_FUNCTION: Moves final interpretive authority from courts to the legislature; moves the cost of constitutional uncertainty from the political process to rights-holding minorities; moves the burden of constitutional stability from entrenched principles to electoral majorities.
% ABSENT_VOICES: Minority communities whose rights depend on judicial enforcement against legislative majorities; future generations bound by current interpretive choices but unable to vote; constitutional principles that lack a political constituency. These voices are structurally excluded because the constraint defines legitimacy through current electoral majorities.
% DISAPPEARANCE_RATIONALE: If parliamentary final interpretive authority vanished overnight, constitutional disputes would shift to courts as terminal arbiters; legislative majorities would lose the power to override judicial interpretations; rights protections would become more entrenched but less democratically responsive; the constitutional order would reorganize around judicial supremacy.
% FOUNDING_PROBLEM: The need for democratic legitimacy in constitutional interpretation: fear that unelected judges, unaccountable to the people, would override popular will and entrench their own preferences as constitutional law. The arrangement was built to ensure that constitutional meaning remains subject to democratic revision.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists in the parliamentary sovereignty tradition (Dicey, Waldron, Bellamy) attest the founding problem remains live — democratic legitimacy requires legislative supremacy. Rights theorists (Dworkin, Ely, Hirschl) and comparative constitutional scholars attest the problem is substantially solved or inverted — judicial review now protects democracy from majority tyranny. No consensus exists outside the benefiting parties (governing majorities).
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the legislature's ability to extract interpretive authority from courts and minorities — the commission-like transfer of constitutional meaning from independent adjudication to majoritarian politics. Suppression (0.58) captures the active exclusion of judicial finality and minority veto points. Theater ratio (0.28) acknowledges genuine democratic coordination value while noting performative invocations of 'mandate' to shield partisan entrenchment. Accessibility collapse (0.55) and resistance (0.45) reflect that judicial review persists but operates in a subordinate register — alternatives exist but are structurally disadvantaged. The measurement series shows gradual extraction accumulation and enforcement intensification over a century as legislative dominance consolidated.
 *
 * PERSPECTIVAL GAP:
 *   From the legislature's seat, the constraint is genuine coordination — it resolves constitutional disputes democratically. From the judiciary's seat, it is enforced subordination — their interpretations are advisory. From minorities' seat, it is extraction without recourse — their rights depend on majority forbearance. The engine computes these divergent seat types from the structural data; the authored claim (tangled_rope) reflects the authoring seat's assessment of the constraint's overall structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The elected_legislature and governing_majority are structural beneficiaries (d near 0.0-0.2) — they collect institutional authority and policy control. The independent_judiciary is a payer with constrained exit (d ~0.7) — it bears override costs but retains institutional position. Rights_minorities are payers with trapped exit (d ~0.9) — concentrated costs, no structural escape. Constitutional_principles (non-agent) pays analytical costs. Political_opposition is excluded/payer (d ~0.6 when out of power, ~0.1 when in power — dual role captured). Legal_scholars and international_courts are observers (d ~0.5, analytical exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (democratic legitimacy of constitutional interpretation) remains contested. The arrangement persists partly because governing majorities benefit from interpretive control (mandatrophy risk: the mandate has outlived its democratizing function and now serves entrenchment). However, the coordination function (preventing judicial deadlock, ensuring democratic responsiveness) remains live in many contexts. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges flags potential mandatrophy — the constraint reshapes the world if removed, but its original justification is disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_legitimacy_vs_rights_protection,
    'Does the democratic mandate genuinely justify legislative override of rights protections, or does it serve as a cover for majority tyranny?',
    'Longitudinal study of legislative override patterns: frequency, targets (minority rights vs. structural provisions), and correlation with electoral cycles. Comparative analysis of rights outcomes in parliamentary sovereignty vs. judicial supremacy systems.',
    'If overrides predominantly target minority rights during electoral peaks, the constraint operates as snare-like extraction. If overrides correct genuine judicial overreach on structural matters, the coordination function dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_rights_protection, empirical, 'Whether democratic mandate translates to legitimate coordination or majoritarian extraction.').

omega_variable(
    interpretive_authority_naturalness,
    'Is final legislative interpretive authority a natural feature of democratic systems, or a constructed institutional choice that could be otherwise?',
    'Historical analysis of constitutional founding moments: whether parliamentary sovereignty was deliberately chosen over judicial supremacy, or emerged from historical contingency. Comparative study of systems that transitioned between models.',
    'If constructed, the constraint is vulnerable to mandatrophy — its justification (democratic legitimacy) may not match its operation (majority entrenchment). If natural, it approaches mountain-like stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_authority_naturalness, conceptual, 'Whether the constraint''s form is necessary or contingent.').

omega_variable(
    kernel_reading_boundary,
    'Where exactly does this reading''s commitment to parliamentary sovereignty end and judicial review begin — is there a stable boundary or a sliding scale?',
    'Doctrinal analysis of ''manner and form'' requirements, entrenched clauses, and constitutional amendment procedures that limit legislative interpretive authority. Mapping the zone of contested authority.',
    'If the boundary is stable and principled, the constraint is a genuine tangled rope with coordination function. If the boundary slides with political convenience, extraction dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural location of the parliamentary-judicial authority boundary.').

omega_variable(
    sibling_reading_foreclosure,
    'Does this reading''s core premise (legislative final authority) logically foreclose judicial supremacy within a single constitutional framework, or can they coexist as competing but non-exclusive interpretive claims?',
    'Analysis of constitutional texts that attempt hybrid models (e.g., UK Human Rights Act, Canadian notwithstanding clause, Israeli basic laws). Testing whether a single framework can grant final authority to both institutions simultaneously.',
    'If foreclosure holds, the kernel admits no stable hybrid — institutional design must choose. If coexistence is possible, the readings occupy different institutional niches and the constraint family is more fluid.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between parliamentary sovereignty and judicial supremacy readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blia_parl_sov_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(blia_parl_sov_tr_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(blia_parl_sov_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(blia_parl_sov_tr_t60, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(blia_parl_sov_tr_t80, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(blia_parl_sov_tr_t100, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(blia_parl_sov_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(blia_parl_sov_be_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(blia_parl_sov_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(blia_parl_sov_be_t60, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(blia_parl_sov_be_t80, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 80, 0.64).
narrative_ontology:measurement(blia_parl_sov_be_t100, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(blia_parl_sov_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(blia_parl_sov_su_t20, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(blia_parl_sov_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(blia_parl_sov_su_t60, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 60, 0.54).
narrative_ontology:measurement(blia_parl_sov_su_t80, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 80, 0.56).
narrative_ontology:measurement(blia_parl_sov_su_t100, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings form a kernel family decomposing the 'basic law interpretive authority' concept. Parliamentary sovereignty reading: ε=0.65 (extraction via legislative override). Judicial supremacy reading: expected lower ε (coordination via rights protection) but higher suppression of democratic revision. Popular constitutionalism reading: expected diffuse extraction (no terminal authority) with contested coordination. The ε values differ because the structural arrangements differ — not because of measurement choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, organized, 0.35).
constraint_indexing:directionality_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
