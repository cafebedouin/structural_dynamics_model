% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: President as Coordinated Executive Requiring Legislative Authorization (Fifth Republic Parliamentary Reading)
 *   domain: constitutional_law/political_systems/comparative_government
 *
 * SUMMARY:
 *   The Fifth Republic Constitution (1958) establishes a dual-executive
 *   system where the President requires legislative authorization for
 *   domestic policy implementation. Under the parliamentary constraint
 *   reading, the President functions as a coordinated executive whose policy
 *   agenda depends on Assembly confidence and majority support. The Prime
 *   Minister, responsible to the Assembly, directs government action. The
 *   President's role is to arbitrate and ensure continuity, not to drive
 *   policy independently. This reading treats the constitutional arrangement
 *   as a genuine coordination mechanism solving the problem of executive
 *   accountability to the legislature — the classic parliamentary
 *   'responsible government' principle adapted to a semi-presidential
 *   framework.
 *
 * KEY AGENTS:
 *   - President: Primary target (institutional/constrained) — bears constraint when Assembly withholds support
 *   - Legislative Majority: Primary beneficiary (organized/constrained) — controls policy agenda through confidence mechanism
 *   - Prime Minister: Secondary beneficiary/agenda_setter (institutional/constrained) — directs government action with Assembly backing
 *   - Opposition Parties: Excluded (moderate/trapped) — would object but lack procedural leverage
 *   - Constitutional Council: Observer (analytical/analytical) — adjudicates constitutional disputes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.15).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.25).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "President as Coordinated Executive Requiring Legislative Authorization (Fifth Republic Parliamentary Reading)").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems/comparative_government").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '0ca74079-2f5e-4691-8176-e9676443740d').
narrative_ontology:cs_kernel_codification('0ca74079-2f5e-4691-8176-e9676443740d', formalized).
narrative_ontology:cs_authority_grounding('0ca74079-2f5e-4691-8176-e9676443740d', lineage).
narrative_ontology:cs_interpretation_layer_present('0ca74079-2f5e-4691-8176-e9676443740d').
narrative_ontology:cs_reading_relation('0ca74079-2f5e-4691-8176-e9676443740d', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ca74079-2f5e-4691-8176-e9676443740d', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('0ca74079-2f5e-4691-8176-e9676443740d', foundational, president_requires_assembly_confidence_for_domestic_policy).
narrative_ontology:cs_axiom_status(president_requires_assembly_confidence_for_domestic_policy, holdable).
narrative_ontology:cs_axiom_grounding('0ca74079-2f5e-4691-8176-e9676443740d', president_requires_assembly_confidence_for_domestic_policy, conventional).
narrative_ontology:cs_axiom('0ca74079-2f5e-4691-8176-e9676443740d', foundational, prime_minister_directs_government_action).
narrative_ontology:cs_axiom_status(prime_minister_directs_government_action, holdable).
narrative_ontology:cs_axiom_grounding('0ca74079-2f5e-4691-8176-e9676443740d', prime_minister_directs_government_action, conventional).
narrative_ontology:cs_axiom('0ca74079-2f5e-4691-8176-e9676443740d', secondary, responsible_government_principle_adapted_to_semi_presidentialism).
narrative_ontology:cs_axiom_status(responsible_government_principle_adapted_to_semi_presidentialism, holdable).
narrative_ontology:cs_axiom_grounding('0ca74079-2f5e-4691-8176-e9676443740d', responsible_government_principle_adapted_to_semi_presidentialism, conventional).
narrative_ontology:cs_reference_frame('0ca74079-2f5e-4691-8176-e9676443740d', gaullist_parliamentary_arbitration).
narrative_ontology:cs_drift_state('0ca74079-2f5e-4691-8176-e9676443740d', contemporary_fifth_republic_practice, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('0ca74079-2f5e-4691-8176-e9676443740d', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, parliamentary_majority_party).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, responsible_government_principle).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, legislative_authorization_requirement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the presidency with a direct democratic mandate but requires Assembly confidence for domestic policy implementation. Cannot govern without legislative majority support. Has limited dissolution power (once per year, cannot dissolve during first year or under Article 16). Retains independent domains in foreign policy, defense, and certain appointments. When Assembly majority opposes, the President's domestic agenda is blocked — the constraint extracts policy capacity from this seat.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president, payer,
    institutional, biographical, constrained, national).

% Controls the Assembly majority and through it the confidence mechanism. Directs government policy via the Prime Minister. Can withdraw confidence and force government resignation. Benefits from the constraint by controlling the domestic policy agenda. Exit is constrained — majority status depends on electoral cycles and party discipline.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, beneficiary,
    organized, biographical, constrained, national).

% Directs government action and is responsible to the Assembly. Implements the legislative majority's policy agenda. Serves at the pleasure of the Assembly majority — can be replaced by a censure motion. Benefits from the constraint by holding executive implementation authority backed by legislative confidence. Exit is constrained by Assembly confidence.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister, agenda_setter).

% Hold Assembly seats but lack majority. Can propose censure motions (rarely successful), amend legislation, and use parliamentary procedure. Structurally excluded from controlling the government agenda. Would object to the presidential constraint if it protects an opposing President, but lack procedural leverage to change the constraint itself.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, opposition_parties, excluded,
    moderate, biographical, trapped, national).

% Adjudicates constitutional disputes including dissolution timing, Article 16 emergency powers, and legislative-executive boundary conflicts. Does not collect from or pay into the constraint. Provides the analytical seat that sees the full structure.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves executive instability by making the government responsible to a stable Assembly majority while preserving a directly elected President with arbitration and continuity functions. The Prime Minister directs government action with Assembly confidence; the President ensures state continuity and guards the constitutional order.
% TRANSFER_FUNCTION: Moves policy implementation authority from the President (who holds the democratic mandate but lacks legislative confidence) to the Prime Minister and legislative majority (who hold confidence but lack direct democratic mandate for the executive). The President's policy capacity is transferred to the Assembly-backed government.
% ABSENT_VOICES: Voters who elected the President on a policy platform but find that platform blocked by an Assembly majority from a different party — especially during cohabitation periods. They are not represented in the institutional mechanics of confidence and censure.
% DISAPPEARANCE_RATIONALE: If the parliamentary constraint vanished overnight, the President would govern domestically without Assembly confidence — reverting toward Fourth Republic instability or toward hyper-presidential dominance. The Assembly majority would lose its primary lever over government policy. The dual-executive balance would collapse into either presidential dominance or parliamentary instability.
% FOUNDING_PROBLEM: The Fourth Republic (1946-1958) suffered chronic executive instability: 24 governments in 12 years, inability to pursue coherent policy, parliamentary fragmentation preventing stable majorities. The Fifth Republic was designed to stabilize executive authority while preserving parliamentary accountability.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists (Duverger, Huber, Siaroff) corroborate that the founding problem (executive instability) was substantially resolved by the Fifth Republic's design. The parliamentary constraint's coordination function remains live — Assembly majorities still require a mechanism to direct government action. No serious scholar argues the constraint has become purely extractive; the debate is over its modal operation (parliamentary vs. presidential vs. cohabitation), not its foundational legitimacy.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.15) because the constraint primarily coordinates executive-legislative relations rather than extracting resources. The President's policy capacity is constrained, but this is the coordination function — not extraction for private benefit. Suppression is low-moderate (0.25): the constraint operates through constitutional procedures (confidence votes, legislative process) rather than coercion. Theater ratio is low (0.12): the coordination function is genuine and actively used. Accessibility collapse is moderate (0.45): alternative executive arrangements (cohabitation, presidential dominance) exist but are constitutionally structured. Resistance is moderate (0.35): presidents periodically test the boundaries (e.g., dissolution threats, Article 16 emergency powers) but the parliamentary constraint holds.
 *
 * PERSPECTIVAL GAP:
 *   From the legislative majority's seat, this is pure coordination (rope) — the mechanism that makes government accountable and effective. From the President's seat (especially under opposition Assembly), the same constraint registers as extraction — the President's democratic mandate is subordinated to Assembly confidence. The engine computes this divergence from structural data: the President has constrained exit (cannot dissolve Assembly at will, cannot govern without majority) while the legislative majority has mobile exit (can replace government, cannot easily replace President). The beneficiary/victim declarations map directly: legislative majority benefits (controls agenda), President bears costs (policy dependence).
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative majority and Prime Minister are declared beneficiaries — they collect policy control and executive implementation capacity. The President is declared victim when Assembly withholds confidence or blocks legislation — the President's policy agenda is extractively constrained by the Assembly majority. This is not symmetric: the President cannot govern domestically without majority consent, but the majority can govern (via Prime Minister) without presidential initiative. Exit options differentiate: President is constrained (fixed term, limited dissolution power, no alternative policy channel); legislative majority is constrained but with agenda control; Prime Minister is constrained (serves at Assembly pleasure). Directionality derivation: President d ≈ 0.7 (target), legislative majority d ≈ 0.2 (beneficiary), Prime Minister d ≈ 0.3 (beneficiary-leaning).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1958) was executive instability under the Fourth Republic — governments fell too frequently for coherent policy. The parliamentary constraint solved this by making the executive responsible to a stable Assembly majority while giving the President dissolution power and emergency authority. That founding problem (instability) is substantially resolved — Fifth Republic governments are stable. Yet the constraint persists with low extractiveness, suggesting it has not undergone mandatrophy into piton or snare. The coordination function remains live: the Assembly majority still requires a mechanism to direct government action. The constraint is not theatrical (low theater_ratio) and does not extract for private benefit. It remains a functioning rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_parliamentary_constraint,
    'Does this constraint instantiate a genuine parliamentary coordination mechanism, or is it a cover for legislative dominance over the executive?',
    'Compare executive legislative success rates under unified vs. divided government; track whether presidential initiatives fail structurally or only when opposed by the Assembly majority.',
    'If the constraint operates as genuine coordination, executive failure correlates with lack of Assembly support across all configurations. If it operates as legislative dominance, executive failure spikes only when the Assembly majority opposes the president''s party.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_parliamentary_constraint, empirical, 'Whether the parliamentary constraint reading describes coordination or extraction').

omega_variable(
    presidential_victim_status_ambiguity,
    'When the Assembly withholds confidence or blocks legislation, does the president experience genuine extractive victimhood or merely the normal friction of democratic coordination?',
    'Measure whether presidential policy capacity is structurally degraded (unable to pursue any agenda without majority consent) versus situationally blocked (specific proposals rejected on merits). Track whether the president retains independent domains of action (foreign policy, defense, appointments) that remain functional.',
    'If victimhood is structural, the constraint reclassifies toward snare for the presidential seat. If victimhood is situational democratic friction, the constraint remains rope/coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(presidential_victim_status_ambiguity, conceptual, 'Whether presidential constraint constitutes extraction or democratic coordination').

omega_variable(
    sibling_reading_cohabitation_delta,
    'How does the parliamentary constraint reading structurally relate to the cohabitation equilibrium reading — do they occupy different operational modes of the same kernel, or does one foreclose the other?',
    'Analyze whether cohabitation periods (president and Assembly majority from opposing parties) instantiate a distinct equilibrium that the parliamentary reading cannot capture, or whether cohabitation is merely the parliamentary constraint operating under partisan opposition.',
    'If cohabitation is a distinct equilibrium, the two readings coexist_with as live alternatives. If cohabitation is the parliamentary constraint under opposition, the parliamentary reading influences the cohabitation reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_cohabitation_delta, conceptual, 'Structural relationship between parliamentary constraint and cohabitation equilibrium readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 1958, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1958, 0.05).
narrative_ontology:measurement(fift_tr_t1962, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1962, 0.08).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1986, 0.15).
narrative_ontology:measurement(fift_tr_t1993, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1993, 0.12).
narrative_ontology:measurement(fift_tr_t2002, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2002, 0.1).
narrative_ontology:measurement(fift_tr_t2022, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2022, 0.12).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1958, 0.08).
narrative_ontology:measurement(fift_be_t1962, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1962, 0.12).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1986, 0.18).
narrative_ontology:measurement(fift_be_t1993, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1993, 0.15).
narrative_ontology:measurement(fift_be_t2002, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2002, 0.14).
narrative_ontology:measurement(fift_be_t2022, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2022, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1958, 0.15).
narrative_ontology:measurement(fift_su_t1962, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1962, 0.2).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1986, 0.3).
narrative_ontology:measurement(fift_su_t1993, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1993, 0.25).
narrative_ontology:measurement(fift_su_t2002, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2002, 0.22).
narrative_ontology:measurement(fift_su_t2022, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2022, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__parliamentary_constraint_reading, 0.1).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the fifth_republic_constitution kernel. The parliamentary_constraint_reading (this file) treats the President as a coordinated executive requiring legislative authorization — low extraction, strong democratic constraint. The hyper_presidential_reading treats the President as a direct sovereign with minimal legislative constraint — higher presidential extractiveness, weaker democratic constraint. The cohabitation_equilibrium_reading treats the dual executive as requiring negotiated authority allocation during divided government — a distinct equilibrium with its own extraction/coordination profile. The ε values differ structurally: this reading authors ε≈0.15 (coordination-dominant); hyper_presidential authors ε≈0.45+ (extraction-dominant for presidential seat); cohabitation authors ε≈0.25-0.35 (negotiated allocation with mutual constraint).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__parliamentary_constraint_reading, institutional, 0.7).
constraint_indexing:directionality_override(fifth_republic_constitution__parliamentary_constraint_reading, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
