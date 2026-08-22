% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__parliamentary_primacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy in Constitutional Interpretation
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   In the parliamentary primacy reading of constitutional authority, the
 *   constitutional text (where it exists) is treated as subordinate to the
 *   authority of the elected legislature. The legislature retains final
 *   interpretive power: it can redefine what the constitution means through
 *   ordinary legislation (where the text permits) or through entrenched
 *   legislation (where constitutional amendment procedures are superseded by
 *   super-majoritarian statute). This reading directly forecloses the
 *   judicial_supremacy_reading's claim that courts hold final constitutional
 *   authority. It coexists with the coordinate_construction_reading only if
 *   the coordinate reading accepts that the legislature is the co-equal
 *   branch with final say over the text's meaning in its domain. The
 *   constraint is low-extractiveness (0.18 at interval end) because the
 *   coordination function — establishing a single, authoritative decision
 *   procedure for constitutional disputes — is genuine and lightly overlaid.
 *   Suppression is modest (0.22) because the constraint's enforcement relies
 *   primarily on legislative institutional power and political culture rather
 *   than active coercion of courts or minorities; courts internalize the
 *   subordination (identity-locked to the rule of law as defined by statute)
 *   and minorities accept electoral remedy as the legitimate path. Theater is
 *   low (0.15) because the constraint operates largely as stated:
 *   legislatures do reinterpret constitutional meaning through statute,
 *   courts do apply that legislative reading, and the arrangement persists
 *   without requiring extensive performative maintenance.
 *
 * KEY AGENTS:
 *   - Elected Legislature: primary agenda-setter; defines constitutional meaning through legislation (ordinary and entrenched); collects interpretive authority the constraint preserves
 *   - Courts: institutional payer; subordinate interpretive role; cannot invalidate legislative constitutional construction through strong-form review
 *   - Minority Factions: powerless payers; closed off from judicial appeal to constitutional limits; trapped exit to electoral remedy only
 *   - Executive (when aligned with legislature): secondary beneficiary; operates under legislative constitutional definition
 *   - Constitutional Text Tradition: non-agent beneficiary (vindicated proposition); vindicated formally while subordinated practically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.18).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.22).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '184e0ab2-373d-4d2f-8829-766de31112f3').
narrative_ontology:cs_kernel_codification('184e0ab2-373d-4d2f-8829-766de31112f3', fixed_text).
narrative_ontology:cs_authority_grounding('184e0ab2-373d-4d2f-8829-766de31112f3', lineage).
narrative_ontology:cs_interpretation_layer_present('184e0ab2-373d-4d2f-8829-766de31112f3').
narrative_ontology:cs_reading_relation('184e0ab2-373d-4d2f-8829-766de31112f3', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('184e0ab2-373d-4d2f-8829-766de31112f3', constitutional_authority_boundary__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('184e0ab2-373d-4d2f-8829-766de31112f3', foundational, legislative_democratic_supremacy).
narrative_ontology:cs_axiom_status(legislative_democratic_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('184e0ab2-373d-4d2f-8829-766de31112f3', legislative_democratic_supremacy, deontological).
narrative_ontology:cs_axiom('184e0ab2-373d-4d2f-8829-766de31112f3', foundational, judicial_review_incompatible_with_electoral_mandate).
narrative_ontology:cs_axiom_status(judicial_review_incompatible_with_electoral_mandate, holdable).
narrative_ontology:cs_axiom_grounding('184e0ab2-373d-4d2f-8829-766de31112f3', judicial_review_incompatible_with_electoral_mandate, instrumental).
narrative_ontology:cs_reference_frame('184e0ab2-373d-4d2f-8829-766de31112f3', legislative_supremacy_with_textual_constraint).
narrative_ontology:cs_drift_state('184e0ab2-373d-4d2f-8829-766de31112f3', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('184e0ab2-373d-4d2f-8829-766de31112f3', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, executive_aligned_with_legislature).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, courts).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, courts).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, minority_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines constitutional meaning through legislation, both ordinary and entrenched. Operates with the authority to interpret, amend, and redefine the constitutional text according to its reading of the democratic mandate. Bears accountability only to voters, not to judicial review that would override legislative construction. Directly collects the interpretive authority the constraint preserves.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Accept subordinate interpretive role, reviewing legislation for consistency with the legislative definition of constitutional meaning rather than imposing an independent constitutional reading. Retain authority to apply the law as legislature defines it, but cannot invalidate or override legislative constitutional construction through strong-form review. Their institutional independence is constrained by the primacy principle.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, courts, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, courts, beneficiary).

% Cannot appeal to courts to block majoritarian legislative choices on constitutional grounds; the constraint closes the exit of judicial review as a counter-majoritarian check. Their recourse is political persuasion, electoral pressure, or constitutional amendment through the same legislative process that created the contested statute. Identity-locked to the jurisdiction.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, minority_factions, payer,
    powerless, biographical, trapped, national).

% Non-agent entry: preserved in the corpus for structural completeness. The tradition of textual constraint on power is vindicated symbolically (legislatures claim fidelity to the text) while subordinated practically (the text's meaning is what the legislature says it is). The constraint's operation vindicates the proposition of legislative supremacy over textual constraint.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_text_tradition, beneficiary,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_text_tradition).

% Operates under legislative definition of constitutional authority. When the executive and legislature are aligned, the constraint transfers interpretive authority to both jointly. When misaligned, the executive is constrained by the legislature's constitutional reading and cannot appeal to courts for independent review that would override it.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, executive_aligned_with_legislature, beneficiary,
    institutional, generational, analytical, national).

% Non-agent entry: where the constraint operates in a polity bound by international human rights frameworks or supranational constitutional law, those external frameworks are technically subordinate to parliamentary primacy doctrine, though real-world compliance creates negotiation pressure the doctrine does not formally acknowledge.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, international_commitments, excluded,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(constitutional_authority_boundary__parliamentary_primacy_reading, international_commitments).

% Examines the constraint's structure and measures its operation across different institutional configurations and historical periods where parliamentary primacy has been claimed or enforced.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__parliamentary_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, definitive decision-making procedure for constitutional questions: the legislature decides, ending ambiguity about who interprets the fundamental law and preventing paralysis from competing institutional claims to constitutional authority.
% TRANSFER_FUNCTION: Transfers interpretive authority from the constitutional text (as potentially readable by courts or the public) to the elected legislature, which converts ambiguous text into enforceable meaning through legislation. Transfers constraint-on-power from 'written constitution' to 'whatever the legislature defines the constitution to mean'.
% ABSENT_VOICES: Courts operating under a strong-review model; populations in historical moments when courts held interpretive supremacy; constitutional scholars committed to textual or originalist readings that the legislature has overridden; international actors holding the polity to external constitutional standards the legislature rejects.
% DISAPPEARANCE_RATIONALE: If the constraint vanished and courts reasserted independent constitutional review authority, legislative statutes would be vulnerable to invalidation, minority groups would gain an appellate venue outside electoral politics, the effective constitutional meaning would become contested and unstable, and the power distribution among the three branches would shift. The legislative-supremacy settlement would dissolve.
% FOUNDING_PROBLEM: Democratic theory requires that the representatives of the people hold final authority; a coordinate or judicially-supreme reading fragments authority and enables unelected judges to override the people's representatives, violating the democratic principle.
% FOUNDING_PROBLEM_CORROBORATION: Defenders of parliamentary primacy (legislatures themselves, parliamentary scholars, democratic-theory advocates in Westminster and civil-law traditions) attest the founding problem is live and the constraint is its solution. Critics (constitutional scholars advocating for judicial review, international human rights bodies, courts operating under strong-review doctrine in other jurisdictions) attest the founding problem has been superseded by concerns about majoritarian tyranny and that the constraint sacrifices rule-of-law protections to democratic procedure. Outside corroboration: comparative constitutional law shows both models (parliamentary supremacy and judicial review) persist globally, and no consensus exists that one solves the problem better than the other.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).
:- end_tests(constitutional_authority_boundary__parliamentary_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint solves a real coordination problem: in any constitutional system, some institution must have final say on what the text means, and fragmenting that authority across competing branches creates deadlock. The legislature's claim to that role is grounded in democratic theory (the people's representatives hold supreme authority). The constraint does extract from minority factions (closes the judicial-review exit), but that extraction is treated as the legitimate price of democratic majority rule, not as coercive overhead. Suppression is measured at 0.22 because the constraint's persistence requires some active maintenance: courts must be prevented from asserting judicial review, and they are — through professional norms, legislative override of judicial decisions, and public rhetoric emphasizing legislative supremacy. But this suppression is not primarily coercive (courts are not imprisoned for reviewing); it is primarily internalized and institutional (courts accept their role as defined by the legal tradition). Theater is low (0.15) because the constraint operates as declared: legislatures genuinely do redefine constitutional meaning, courts genuinely do defer, and the arrangement is relatively stable. Where theater rises (toward 0.20-0.25 in some jurisdictions), it signals courts are performing deference while conducting de facto review, or legislatures are rhetorically appealing to constitutional text while ignoring it — signs that the constraint is degrading (becoming a piton) rather than operating cleanly. Accessibility collapse (0.72) reflects that once the parliamentary primacy reading takes hold, alternatives (judicially-enforced constitutional limits, coordinate branch authority) become difficult to conceive as legitimate within the reading's framework; the reading closes the conceptual space for its competitors. Resistance (0.58) reflects significant contestation from courts in some periods, minorities seeking judicial review, and scholars advocating for constitutional limits on legislative power; the constraint faces real opposition, though organized legislative power generally prevails.
 *
 * PERSPECTIVAL GAP:
 *   The legislature and courts compute different types from the same structural data. The legislature, experiencing the constraint as the legitimate coordination mechanism for democratic authority, experiences low extracted cost and high beneficiary status — they perceive rope (or even mountain, if they naturalize parliamentary supremacy as inevitable). Courts, experience the constraint as institutional subordination: they are prevented from exercising what they might claim as their constitutional role (independent review), they are bound by legislative reinterpretation, and their institutional independence is contingent on legislative deference. They perceive tangled_rope or constrained_rope at minimum. Minorities experience it as snare: the constraint closes their exit (judicial review) and forces them into electoral competition where they are structurally disadvantaged. Each seat's computed type derives from the structural data (power, exit_options, role) the engine receives; the measured extractiveness stays constant, but its allocation across seats varies. The commentary explains the gap without claiming to resolve it — that resolution is the engine's job.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature is a beneficiary (d near 0.0): it gains interpretive authority the constraint preserves. Courts are nominal payers (d moved toward 1.0 relative to interpretive supremacy): they lose independent review authority but retain institutional prestige and caseload through legislative deference. Minority factions are clear targets (d near 1.0): they lose the judicial-review exit and are trapped in electoral competition. The constraint's directionality is asymmetric: legislatures experience it as coordination that solves their authority problem; minorities experience it as closure of their appeal venue. The measured extractiveness aggregates these seats differently: the legislature's low d (beneficiary) dampens chi; minorities' high d (targets) amplifies it. The net effect is moderate extractiveness because the beneficiary (legislature) has enough power to sustain the arrangement without requiring exceptional coercive overhead. In other readings (judicial_supremacy_reading), the directionality would invert: courts would be the beneficiary, legislatures would be targets, and extractiveness would reflect the legislature's constrained authority. This story measures the parliamentary_primacy_reading only; the divergence from other readings is handled through the sibling constraint stories and the omega variable on kernel_reading_classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not exhibit classical mandatrophy (founding problem dead, arrangement persisting), though it exhibits contested-status mandatrophy: the founding problem (the need for a single constitutional authority; the threat of unelected judges overriding democracy) remains live in parliamentary primacy advocates' reading but is treated as dead or solved-differently in judicial supremacy advocates' reading. The engine should flag this case as (founding_problem_status=contested, disappearance_verdict=world_rearranges) — the mismatch signals that if the constraint disappeared, a different reading would fill the space. The theater_ratio rising gradually (from 0.08 to 0.15 over the interval) suggests that in some jurisdictions, the constraint is degrading: legislatures increasingly appeal to constitutional text they then ignore; courts increasingly conduct de facto review while performing deference; the actual authority distribution is becoming more complex than the pure parliamentary-primacy reading suggests. This degradation would route through the piton mechanism (if theater reaches 0.5+) or through the oscillating-constraint mechanism (if suppression rises as legislatures work harder to maintain the reading against drift). The current measurements suggest the constraint remains stable in core Westminster systems but is fraying at the edges as international law, judicial activism, and rights-based democracy challenge the pure parliamentary-primacy thesis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_constraint_independence,
    'Can the constitutional text retain any constraining force independent of legislative interpretation, or does parliamentary primacy logically entail that the text is purely instrumental to legislative will?',
    'Examine cases where legislatures claim fidelity to textual limits while reinterpreting meaning; assess whether those limits function as real constraints or rhetorical cover. Compare with jurisdictions where courts independently enforce textual limits that legislatures have attempted to override.',
    'If the text retains no independent constraint, parliamentary primacy is pure procedural authority transfer with low genuine extraction (measured correctly). If the text does constrain, even weakly, the constraint preserves some textual force and the measured extractiveness understates the constraint''s actual operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(text_constraint_independence, conceptual, 'Whether constitutional text can constrain legislative interpretation or is purely malleable to legislative will.').

omega_variable(
    entrenched_vs_ordinary_legislation,
    'Where entrenched legislation exists (requiring supermajority or referendum to amend), does it genuinely constrain later-elected legislatures, or does it merely impose delay and procedural overhead the current legislature can overcome?',
    'Historical analysis of attempts to amend entrenched provisions; measurement of how often entrenched provisions persist unchanged versus how often they are repealed or overridden despite entrenchment.',
    'If entrenchment is effective, parliamentary primacy is constrained by the prior legislature and extractiveness rises (majority tyranny over minorities locked in by prior-legislative choice). If entrenchment is easily overcome, extractiveness remains low (current legislature truly retains final say).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(entrenched_vs_ordinary_legislation, empirical, 'Whether entrenched constitutional provisions constrain successor legislatures or are merely procedurally inconvenient.').

omega_variable(
    sibling_reading_precariousness,
    'Is parliamentary primacy a substantive constitutional principle or a contingent feature of Westminster systems that judicial supremacy and coordinate construction readings have overtaken in most contemporary democracies?',
    'Comparative constitutional survey: count jurisdictions by reading; track historical transitions from one reading to another; assess whether any major democracy has voluntarily adopted or maintained parliamentary primacy in recent decades.',
    'If parliamentary primacy is the historically dominant reading, the constraint is stable and extractiveness is correctly measured. If it is being displaced by judicial supremacy (constraint family showing increasing tangled_rope or snare classification for the judicial_supremacy_reading), the parliament''s claimed primacy may be theater — suppression rising to maintain a losing position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_precariousness, empirical, 'Historical viability and global adoption of parliamentary primacy versus competing readings.').

omega_variable(
    kernel_reading_classification,
    'This constraint is one reading of the constitutional_authority_boundary kernel. Are the three sibling readings (coordinate_construction, judicial_supremacy, parliamentary_primacy) genuinely coexistent live positions, or has the kernel itself evolved such that one reading forecloses the others?',
    'Track which readings are held by contemporary major democracies and whether parties holding one reading formally deny the legitimacy of the others, or whether they merely disagree on policy consequences. Assess whether the kernel''s underlying legitimacy foundation (democratic theory, rule of law, judicial independence, popular sovereignty) has shifted such that it now privileges one reading.',
    'If readings coexist, the constraint family models genuine constitutional disagreement and each reading remains a valid alternative. If one reading forecloses the others through shifts in foundational premises (e.g., international human rights law now privileging judicial independence), the family should be restructured with reading_relations showing foreclosure edges, and the parliamentary_primacy_reading may be reclassified as a piton or degraded snare (maintained by inertia in some jurisdictions, displaced in others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_classification, conceptual, 'Whether the three sibling readings remain genuinely coexistent or whether the kernel''s evolution has privileged one reading''s foundational premises.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cons_tr_t8, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(cons_tr_t16, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(cons_tr_t24, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(cons_tr_t32, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 32, 0.15).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cons_be_t8, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(cons_be_t16, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(cons_be_t24, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(cons_be_t32, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 32, 0.18).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 40, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cons_su_t8, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 8, 0.17).
narrative_ontology:measurement(cons_su_t16, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 16, 0.19).
narrative_ontology:measurement(cons_su_t24, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 24, 0.21).
narrative_ontology:measurement(cons_su_t32, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 32, 0.22).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 40, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__parliamentary_primacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_authority_boundary kernel. The sibling readings (judicial_supremacy_reading, coordinate_construction_reading) are separate constraint stories with their own metrics, stakeholders, and classifications. All three stories are linked via network.affects_constraints; together they form the constitutional_authority_boundary constraint family. The stories should not be merged; each reading's ε value is stable and reading-specific (not averaged or compromised). See kernel_context in commentary for the decomposition rationale.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__parliamentary_primacy_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
