% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Sovereignty Reading of Constitutional Text
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint story captures the legislative sovereignty reading of
 *   constitutional text — the view that parliament is supreme and courts play
 *   only an advisory role, with the legislature retaining final say through
 *   notwithstanding clauses (e.g., Canada's Section 33, UK's parliamentary
 *   sovereignty, Israel's override mechanisms) or simple legislative
 *   override. The reading presents itself as democratic coordination: the
 *   people's representatives, not unelected judges, determine constitutional
 *   meaning. But structurally, it extracts from minority rights holders whose
 *   protections become contingent on legislative majorities. The constraint
 *   has drifted toward higher extraction over time as rights consciousness
 *   expanded while legislative override mechanisms remained available. The
 *   claim/metric gap is deliberate: the reading CLAIMS rope (democratic
 *   coordination) while the authored metrics describe substantial extraction
 *   from powerless minorities — the engine measures this divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.68).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.55).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Sovereignty Reading of Constitutional Text").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, 'd1910699-1d71-471a-b016-5e2a4df9a692').
narrative_ontology:cs_kernel_codification('d1910699-1d71-471a-b016-5e2a4df9a692', formalized).
narrative_ontology:cs_authority_grounding('d1910699-1d71-471a-b016-5e2a4df9a692', lineage).
narrative_ontology:cs_interpretation_layer_present('d1910699-1d71-471a-b016-5e2a4df9a692').
narrative_ontology:cs_reading_relation('d1910699-1d71-471a-b016-5e2a4df9a692', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d1910699-1d71-471a-b016-5e2a4df9a692', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('d1910699-1d71-471a-b016-5e2a4df9a692', foundational, parliamentary_supremacy_over_constitutional_meaning).
narrative_ontology:cs_axiom_status(parliamentary_supremacy_over_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('d1910699-1d71-471a-b016-5e2a4df9a692', parliamentary_supremacy_over_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('d1910699-1d71-471a-b016-5e2a4df9a692', secondary, judicial_review_advisory_only).
narrative_ontology:cs_axiom_status(judicial_review_advisory_only, holdable).
narrative_ontology:cs_axiom_grounding('d1910699-1d71-471a-b016-5e2a4df9a692', judicial_review_advisory_only, conventional).
narrative_ontology:cs_reference_frame('d1910699-1d71-471a-b016-5e2a4df9a692', westminster_parliamentary_sovereignty).
narrative_ontology:cs_drift_state('d1910699-1d71-471a-b016-5e2a4df9a692', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d1910699-1d71-471a-b016-5e2a4df9a692', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, democratic_majority).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majoritarian_legislature).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_holders).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, vulnerable_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, constitutional_courts).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, democratic_legitimacy_of_majoritarian_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final interpretive authority over constitutional meaning through notwithstanding clauses or simple legislative override. Sets the legislative agenda and can disregard judicial advisory opinions. Collects the political benefit of unconstrained democratic lawmaking.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, majoritarian_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Sees its policy preferences enacted without judicial veto. The constraint validates majoritarian will as the legitimate source of constitutional meaning. Exit is available through electoral politics but the arrangement itself benefits their collective preferences.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, democratic_majority, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of rights protections being subject to legislative majorities. Their constitutional protections depend on legislative grace rather than entrenched judicial enforcement. Exit is identity-locked — they cannot exit their minority status or the polity that subjects them to majoritarian interpretation.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_rights_holders, payer,
    powerless, generational, identity_locked, national).

% Disproportionately affected when legislative majorities override rights protections. Lack political power to influence legislative outcomes and are structurally excluded from the interpretive authority that determines their protections.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, vulnerable_groups, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, vulnerable_groups, excluded).

% Retain advisory review function but lose final authority. Their interpretations can be set aside by legislative action. The constraint extracts their institutional authority and legitimacy as constitutional guardians. Exit is constrained — they remain the institutional forum for constitutional argument but without conclusive power.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_courts, payer,
    institutional, generational, constrained, national).

% Analyze the structural dynamics of legislative supremacy versus judicial review across comparative systems. Provide the analytical vocabulary for evaluating whether the arrangement coordinates democratic legitimacy or extracts from minorities.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, democratically accountable authority for constitutional meaning — the elected legislature — resolving interpretive disputes through majoritarian politics rather than judicial fiat. Solves the coordination problem of who speaks for the constitution by answering: the people's representatives.
% TRANSFER_FUNCTION: Moves final interpretive authority from courts to legislature, and moves the cost of rights violations from the majority (who would bear judicial constraints on their will) to minority rights holders (who lose entrenched protections). The legislature gains unconstrained lawmaking power; minorities lose veto points against rights-restrictive legislation.
% ABSENT_VOICES: Future generations who inherit a constitutional order with weakened entrenchment; non-citizens subject to the polity's laws without electoral voice; the international human rights regime that expects domestic constitutional entrenchment. These voices are structurally excluded from the majoritarian calculus that the reading validates.
% DISAPPEARANCE_RATIONALE: If legislative sovereignty vanished overnight and judicial supremacy became the only operative reading, minority rights would gain entrenched judicial protection, legislatures would lose override power, and the democratic accountability of constitutional meaning would shift from electoral to judicial forums. The constitutional order would rearrange around judicial finality.
% FOUNDING_PROBLEM: The founding problem was the perceived democratic deficit of unelected judges invalidating the will of elected representatives — the counter-majoritarian difficulty. Legislative sovereignty was built to solve the legitimacy crisis of judicial review overriding democratic outcomes.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the constraint's beneficiaries (majoritarian legislatures, democratic theorists of popular sovereignty) as still live — they argue the counter-majoritarian difficulty remains acute. It is attested as substantially resolved by judicial supremacy advocates and international human rights bodies (outside the beneficiary set) who argue that rights entrenchment has solved the legitimacy crisis by making rights protection a constitutional prerequisite of democratic legitimacy itself. No neutral arbiter corroborates either side; the contest is structural.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the arrangement transfers the cost of rights violations from the majority to minorities, and this transfer has grown as rights claims have multiplied. Suppression (0.55) is moderate — notwithstanding clauses are rarely invoked but their existence structures the entire constitutional conversation; the threat of override suppresses judicial boldness. Theater ratio (0.32) is low-moderate: the legislative process is genuine democratic coordination, but a growing share of its operation serves to legitimate rights-restrictive majoritarianism. Accessibility collapse (0.48) is moderate: alternatives (judicial supremacy, popular sovereignty) remain live in discourse and practice. Resistance (0.62) is high: courts, minorities, and international bodies actively contest the arrangement.
 *
 * PERSPECTIVAL GAP:
 *   From the legislature's seat, the constraint is genuine democratic coordination — it solves the counter-majoritarian difficulty. From minority seats, it is extraction legitimated by democratic rhetoric. From courts' seat, it is institutional diminishment masked as democratic deference. The engine computes these per-seat classifications from the structural data; the authored claim (tangled_rope) reflects the generating model's assessment that both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   The majoritarian legislature is the structural beneficiary (d near 0.0) — it collects unconstrained interpretive authority. The democratic majority benefits incidentally (d ~ 0.2). Courts pay the cost of lost final authority (d ~ 0.7). Minority rights holders and vulnerable groups are the primary targets (d near 1.0) — they bear the extraction with identity-locked or trapped exit. Constitutional scholars sit at the analytical pole (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (counter-majoritarian difficulty) is contested, not dead. The arrangement persists not because the problem vanished but because the beneficiaries (majoritarian legislatures) have the power to maintain it. The constraint shows mandatrophy signals: the original coordination function (democratic legitimacy) has been layered with extraction (minority rights as legislative grace), and the theater ratio rises as override threats substitute for actual rights protection. But the founding problem's contested status means it is not a pure piton — the coordination rationale remains live for its beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'How does this reading''s classification change if the constitutional_text kernel is read through judicial_supremacy_reading or popular_sovereignty_reading instead?',
    'Generate the sibling constraint stories and compare their ε, beneficiary/victim structures, and computed seat classifications. The kernel''s structural ambiguity is resolved only by comparing across readings.',
    'If judicial_supremacy_reading computes as mountain (low extraction, high accessibility collapse) while this reading computes as tangled_rope, the kernel itself is not a single constraint but a family of structurally distinct constraints. The classification of constitutional text is reading-indexed, not text-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'Committers-frame structural delta across kernel readings').

omega_variable(
    notwithstanding_frequency_vs_threat,
    'Is the extraction measured here driven by actual legislative overrides of rights, or by the structural threat of override that chills judicial protection?',
    'Empirical study of notwithstanding clause invocations versus judicial decision-making patterns in legislative sovereignty systems (Canada, UK, Israel, etc.). Compare rights outcomes in periods with and without active override threats.',
    'If extraction is primarily threat-based (chilling effect), the constraint''s suppression is higher than invocation counts suggest. If extraction requires actual override, the constraint is less extractive in practice than its structural authorization implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notwithstanding_frequency_vs_threat, empirical, 'Whether extraction operates through invocation or structural threat').

omega_variable(
    minority_extraction_structural_or_contingent,
    'Is the extraction from minorities a structural necessity of legislative sovereignty, or a contingent feature of current majoritarian coalitions?',
    'Comparative analysis: do legislative sovereignty systems with strong minority representation mechanisms (consociationalism, proportional representation, entrenched minority vetoes) show lower extraction? If yes, extraction is contingent on electoral rules; if no, it is structural to the reading.',
    'If structural, the tangled_rope classification is stable — the coordination function inherently extracts from minorities. If contingent, the constraint could be reformed toward rope without abandoning legislative sovereignty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(minority_extraction_structural_or_contingent, empirical, 'Whether minority extraction is inherent to legislative sovereignty or contingent on electoral design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 1789, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legislative_sovereignty_tr_t1789, constitutional_text__legislative_sovereignty_reading, theater_ratio, 1789, 0.12).
narrative_ontology:measurement(legislative_sovereignty_tr_t1850, constitutional_text__legislative_sovereignty_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(legislative_sovereignty_tr_t1900, constitutional_text__legislative_sovereignty_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(legislative_sovereignty_tr_t1950, constitutional_text__legislative_sovereignty_reading, theater_ratio, 1950, 0.22).
narrative_ontology:measurement(legislative_sovereignty_tr_t1982, constitutional_text__legislative_sovereignty_reading, theater_ratio, 1982, 0.28).
narrative_ontology:measurement(legislative_sovereignty_tr_t2000, constitutional_text__legislative_sovereignty_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(legislative_sovereignty_tr_t2025, constitutional_text__legislative_sovereignty_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(legislative_sovereignty_be_t1789, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 1789, 0.35).
narrative_ontology:measurement(legislative_sovereignty_be_t1850, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 1850, 0.42).
narrative_ontology:measurement(legislative_sovereignty_be_t1900, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 1900, 0.48).
narrative_ontology:measurement(legislative_sovereignty_be_t1950, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(legislative_sovereignty_be_t1982, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 1982, 0.62).
narrative_ontology:measurement(legislative_sovereignty_be_t2000, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(legislative_sovereignty_be_t2025, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legislative_sovereignty_su_t1789, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 1789, 0.25).
narrative_ontology:measurement(legislative_sovereignty_su_t1850, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 1850, 0.35).
narrative_ontology:measurement(legislative_sovereignty_su_t1900, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 1900, 0.42).
narrative_ontology:measurement(legislative_sovereignty_su_t1950, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 1950, 0.48).
narrative_ontology:measurement(legislative_sovereignty_su_t1982, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 1982, 0.52).
narrative_ontology:measurement(legislative_sovereignty_su_t2000, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement(legislative_sovereignty_su_t2025, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text__legislative_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The constitutional_text kernel decomposes into three structurally distinct constraint stories. This reading (legislative_sovereignty) instantiates legislative final authority with advisory courts; judicial_supremacy_reading instantiates judicial finality; popular_sovereignty_reading instantiates constituent power above both. Their ε values differ substantially: legislative_sovereignty ε=0.68 (tangled_rope), judicial_supremacy ε≈0.15 (rope/mountain), popular_sovereignty ε≈0.45 (scaffold/tangled_rope depending on amendment accessibility). The decomposition follows ε-invariance: each reading has a stable ε, beneficiary/victim structure, and type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
