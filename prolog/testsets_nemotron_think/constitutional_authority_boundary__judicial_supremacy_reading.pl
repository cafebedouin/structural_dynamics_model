% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy: Courts as Final Unchallengeable Arbiters of Constitutional Questions
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint story captures the judicial supremacy reading of the
 *   constitutional authority boundary kernel: the claim that the
 *   constitutional text establishes courts as final, unchallengeable arbiters
 *   of all constitutional questions, with authority to invalidate legislative
 *   and executive acts without remedy. This reading emerged gradually —
 *   Marbury v. Madison (1803) asserted review power; Cooper v. Aaron (1958)
 *   declared judicial supremacy; modern doctrine treats Court interpretations
 *   as binding on all branches. The constraint extracts interpretive monopoly
 *   rents for the judiciary while suppressing legislative and executive
 *   constitutional agency. The coordination function (finality, stability) is
 *   real but the extraction (counter-majoritarian veto, no remedy) is the
 *   dominant structural feature.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.72).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.78).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, snare).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy: Courts as Final Unchallengeable Arbiters of Constitutional Questions").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '0e5631b7-0fd7-4689-9912-5608cac24b8c').
narrative_ontology:cs_kernel_codification('0e5631b7-0fd7-4689-9912-5608cac24b8c', formalized).
narrative_ontology:cs_authority_grounding('0e5631b7-0fd7-4689-9912-5608cac24b8c', lineage).
narrative_ontology:cs_interpretation_layer_present('0e5631b7-0fd7-4689-9912-5608cac24b8c').
narrative_ontology:cs_reading_relation('0e5631b7-0fd7-4689-9912-5608cac24b8c', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('0e5631b7-0fd7-4689-9912-5608cac24b8c', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('0e5631b7-0fd7-4689-9912-5608cac24b8c', foundational, judicial_supremacy_finality).
narrative_ontology:cs_axiom_status(judicial_supremacy_finality, holdable).
narrative_ontology:cs_axiom_grounding('0e5631b7-0fd7-4689-9912-5608cac24b8c', judicial_supremacy_finality, conventional).
narrative_ontology:cs_axiom('0e5631b7-0fd7-4689-9912-5608cac24b8c', foundational, counter_majoritarian_legitimacy).
narrative_ontology:cs_axiom_status(counter_majoritarian_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0e5631b7-0fd7-4689-9912-5608cac24b8c', counter_majoritarian_legitimacy, deontological).
narrative_ontology:cs_reference_frame('0e5631b7-0fd7-4689-9912-5608cac24b8c', judicial_supremacy_framework).
narrative_ontology:cs_drift_state('0e5631b7-0fd7-4689-9912-5608cac24b8c', contemporary_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e5631b7-0fd7-4689-9912-5608cac24b8c', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, supreme_court).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, legal_academy).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, citizens).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, judicial_review_as_guardian).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final, unreviewable authority to interpret the constitution and invalidate legislative/executive acts. Collects interpretive monopoly rents: career prestige, institutional power, doctrinal control. No other branch can overturn its constitutional readings; amendment is the only formal exit, which the Court's own doctrine makes extremely difficult.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, supreme_court, agenda_setter,
    institutional, generational, arbitrage, national).

% Enacts laws that can be invalidated without remedy by judicial review. Policy space is constrained by anticipated judicial reaction; majoritarian preferences are routinely overridden. Exit options: constitutional amendment (supermajority, Court-influenced), court-packing (politically costly, norm-violating), or acquiescence.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Executive actions and orders subject to judicial invalidation without appeal. Administrative state operates under judicial supervision; enforcement discretion is constrained by judicial doctrine. Exit similar to legislature: amendment, appointment strategy (long-term), or compliance.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Subject to constitutional rulings they cannot directly challenge. No standing to override Court decisions; amendment requires supermajorities they cannot muster. Interest groups litigate but individuals have no exit. The constraint's legitimacy rests on their behalf but their voice is structurally absent.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, citizens, excluded,
    powerless, biographical, trapped, national).

% Builds careers interpreting and litigating within the Court's doctrinal framework. Gains professional rents from the monopoly on authoritative constitutional interpretation. Can exit to other fields or jurisdictions but professional identity is fused to the supremacy structure.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legal_academy, beneficiary,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, authoritative, and final resolution mechanism for constitutional disputes, preventing interpretive fragmentation and institutional deadlock among coordinate branches.
% TRANSFER_FUNCTION: Transfers final interpretive authority and policy veto power from electorally accountable branches (legislature, executive) to an unelected, life-tenured judiciary, concentrating constitutional meaning in judicial hands.
% ABSENT_VOICES: Voters and future generations who bear the consequences of constitutional rulings but have no direct role in selecting judges or overriding decisions; minority communities whose rights are adjudicated without their participation; state and local governments whose policy experimentation is preempted.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, constitutional interpretation would revert to coordinate construction (each branch interprets for itself) or parliamentary primacy (legislature has final say). The legislative and executive branches would reclaim policy space currently foreclosed by judicial veto; constitutional amendment would become the primary correction mechanism rather than judicial reversal.
% FOUNDING_PROBLEM: The founding generation feared legislative tyranny and constitutional instability; they needed a mechanism to enforce constitutional limits against temporary majorities and to provide authoritative settlement of constitutional disputes.
% FOUNDING_PROBLEM_CORROBORATION: Federalist 78 (Hamilton) attests the founding problem as live: courts as 'least dangerous branch' with 'neither force nor will but merely judgment.' Critics (Tushnet 'Taking the Constitution Away from the Courts,' Kramer 'The People Themselves') attest the problem is dead: judicial supremacy has become the very tyranny it was meant to prevent, and popular constitutionalism was the original understanding. The disagreement is structural, not merely ideological.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__judicial_supremacy_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.72) reflects the Court's power to veto democratic outcomes without accountability. High suppression (0.78) reflects the structural foreclosure of legislative overrides (no departmentalism, no popular constitutionalism). Theater ratio (0.35) captures that the Court performs genuine adjudication but an increasing share of its docket serves institutional self-preservation. Accessibility collapse (0.85) because amendment is nearly impossible and Court doctrine raises the bar further. Resistance (0.45) is moderate — periodic court-curbing attempts exist but fail structurally.
 *
 * PERSPECTIVAL GAP:
 *   From the Court's seat, the constraint is a rope: it coordinates constitutional meaning, prevents fragmentation, and the Court bears the burden of decision. From the legislature's seat, it is a snare: a veto wielded by an unaccountable body with no remedy. From citizens' seat, it is a mountain-presented-as-snare: they are told it is the Constitution itself (natural law) but experience it as extracted power. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court is the primary beneficiary (d ≈ 0.1): it collects interpretive rents, sets the agenda, and faces no structural check. Legislature and executive are primary payers (d ≈ 0.9): they bear the veto, cannot override, and their constitutional agency is suppressed. Citizens are trapped (d ≈ 0.95): no exit, no voice, subject to rulings. Legal academy benefits (d ≈ 0.3) but is mobile — professional rents without structural lock-in.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legislative tyranny, constitutional instability) is contested: some argue it persists (democratic backsliding risk), others that it has been superseded by the Court becoming the new tyrant. The constraint persists not because the founding problem is live but because the beneficiary (judiciary) controls the interpretation of whether the problem is live — a classic mandatrophy trap where the administrator of the solution defines the problem's existence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the judicial_supremacy_reading a distinct constraint from the coordinate_construction_reading and parliamentary_primacy_reading, or are they observables of the same underlying arrangement?',
    'Apply the ε-invariance test: if measuring the constraint via judicial supremacy yields ε ≈ 0.72 but measuring via coordinate construction yields ε ≈ 0.15, they are different constraints. The standing arrangement (the Constitution''s text and practice) is the fixed referent; the reading instantiates a different constraint with different ε.',
    'If they are one constraint, the framework must model observable-dependent classification. If they are three constraints, each gets its own story with its own ε, beneficiaries, and classification — linked by network.affects_constraints. The latter is required by DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s contested readings instantiate distinct constraints with invariant ε values.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the judicial supremacy arrangement genuinely solve a coordination problem (finality, stability) that would otherwise cause constitutional collapse, or is the coordination story a cover for judicial monopoly rents?',
    'Counterfactual analysis: in jurisdictions without judicial supremacy (UK parliamentary sovereignty, pre-1982 Canada), does constitutional interpretation fragment or stabilize through political means? If political stabilization works, the coordination function is not structurally necessary — the arrangement is a snare. If fragmentation occurs, it is a tangled_rope.',
    'If coordination is genuine, classification shifts from snare to tangled_rope (both coordination AND extraction). If coordination is cover, snare classification stands. This is the central classification ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the constraint''s coordination function is structurally necessary or a cover story for extraction.').

omega_variable(
    suppression_mechanism_judicial_supremacy,
    'Is the suppression of legislative overrides structural (formal constitutional barriers, supermajority requirements) or internalized (legislators self-censor, believe Court has moral authority)?',
    'Post-judicial-override trajectory: if a jurisdiction eliminates judicial review (e.g., via constitutional amendment), does legislative constitutional interpretation revive immediately (structural) or remain dormant due to internalized deference (internalized)? Historical cases: UK (no judicial supremacy, robust legislative interpretation), Canada pre/post-Charter.',
    'If internalized, effective suppression is higher than formal barriers suggest — the constraint persists even if formal enforcement lapses. This affects extraction amplification for the legislature seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_judicial_supremacy, empirical, 'Structural vs. internalized suppression in the judicial supremacy arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1789, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(cons_tr_t1857, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1857, 0.2).
narrative_ontology:measurement(cons_tr_t1905, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1905, 0.25).
narrative_ontology:measurement(cons_tr_t1937, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1937, 0.3).
narrative_ontology:measurement(cons_tr_t1954, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1954, 0.32).
narrative_ontology:measurement(cons_tr_t1973, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 1973, 0.34).
narrative_ontology:measurement(cons_tr_t2000, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(cons_tr_t2024, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(cons_be_t1789, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1789, 0.15).
narrative_ontology:measurement(cons_be_t1857, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1857, 0.35).
narrative_ontology:measurement(cons_be_t1905, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1905, 0.45).
narrative_ontology:measurement(cons_be_t1937, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1937, 0.55).
narrative_ontology:measurement(cons_be_t1954, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1954, 0.62).
narrative_ontology:measurement(cons_be_t1973, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 1973, 0.68).
narrative_ontology:measurement(cons_be_t2000, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 2000, 0.7).
narrative_ontology:measurement(cons_be_t2024, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1789, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1789, 0.2).
narrative_ontology:measurement(cons_su_t1857, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1857, 0.4).
narrative_ontology:measurement(cons_su_t1905, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1905, 0.55).
narrative_ontology:measurement(cons_su_t1937, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1937, 0.6).
narrative_ontology:measurement(cons_su_t1954, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1954, 0.7).
narrative_ontology:measurement(cons_su_t1973, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 1973, 0.75).
narrative_ontology:measurement(cons_su_t2000, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 2000, 0.77).
narrative_ontology:measurement(cons_su_t2024, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the constitutional_authority_boundary kernel. The coordinate_construction_reading and parliamentary_primacy_reading are sibling constraints with different ε values (low for coordinate construction, moderate for parliamentary primacy). This reading forecloses both: judicial supremacy's premise (one final arbiter) logically contradicts coordinate construction (multiple equal interpreters) and parliamentary primacy (legislative finality). The kernel's label 'constitutional authority boundary' conflates three structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__judicial_supremacy_reading, institutional, 0.15).
constraint_indexing:directionality_override(constitutional_authority_boundary__judicial_supremacy_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
