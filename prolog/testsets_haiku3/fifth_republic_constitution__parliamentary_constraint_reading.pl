% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Executive-Legislative Coordination (Parliamentary Reading)
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   The Fifth Republic Constitution (1958) establishes the president as chief
 *   executive but requires legislative authorization for major policy. The
 *   president can initiate legislation, appoint government ministers, and
 *   dissolve the Assembly; but cannot implement policy against legislative
 *   opposition. This reading interprets the constraint as a democratic
 *   coordination mechanism: president and Assembly are bound together such
 *   that neither can unilaterally govern. Executive extractiveness is low
 *   (0.28) because the president faces institutional veto points when the
 *   Assembly withholds confidence or blocks legislation. Suppression is
 *   minimal (0.15) because the constraint operates through constitutional
 *   rule and electoral sanction, not through coercive force. The beneficiary
 *   is the legislative majority (and the principle of popular sovereignty
 *   they represent), which gains the power to block executive overreach. The
 *   executive enters the victim set when the Assembly exercises its veto
 *   authority—the president must either negotiate or accept legislative
 *   constraint on preferred policy. This reading sits between the
 *   parliamentary_constraint_reading (what is authored here) and the
 *   hyper_presidential_reading (which emphasizes executive autonomy when a
 *   cohesive majorit exists), and alongside the
 *   cohabitation_equilibrium_reading (which views power as split between
 *   president and prime minister during divided government).
 *
 * KEY AGENTS:
 *   - President: Chief executive, directly elected, constrained by legislative authorization requirement. Enters victim position when Assembly withholds confidence.
 *   - Legislative majority: Holds veto power over budget and legislation. Primary beneficiary of the coordination constraint.
 *   - Electoral public: Beneficiary through non-autocratic constitutional structure. Can alternate between left and right via elections.
 *   - Constitutional court: Observer and enforcer of constitutional boundaries.
 *   - Minority opposition: Excluded from power but structurally admitted to Assembly floor; can become majority via election.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.28).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.15).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Executive-Legislative Coordination (Parliamentary Reading)").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, '536af850-6225-42b6-be40-396d43dcac09').
narrative_ontology:cs_kernel_codification('536af850-6225-42b6-be40-396d43dcac09', fixed_text).
narrative_ontology:cs_authority_grounding('536af850-6225-42b6-be40-396d43dcac09', lineage).
narrative_ontology:cs_interpretation_layer_present('536af850-6225-42b6-be40-396d43dcac09').
narrative_ontology:cs_reading_relation('536af850-6225-42b6-be40-396d43dcac09', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('536af850-6225-42b6-be40-396d43dcac09', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('536af850-6225-42b6-be40-396d43dcac09', foundational, executive_legislative_coordination_required).
narrative_ontology:cs_axiom_status(executive_legislative_coordination_required, holdable).
narrative_ontology:cs_axiom_grounding('536af850-6225-42b6-be40-396d43dcac09', executive_legislative_coordination_required, deontological).
narrative_ontology:cs_axiom('536af850-6225-42b6-be40-396d43dcac09', foundational, legislative_veto_prevents_autocracy).
narrative_ontology:cs_axiom_status(legislative_veto_prevents_autocracy, holdable).
narrative_ontology:cs_axiom_grounding('536af850-6225-42b6-be40-396d43dcac09', legislative_veto_prevents_autocracy, deontological).
narrative_ontology:cs_reference_frame('536af850-6225-42b6-be40-396d43dcac09', presidential_authority_bound_by_legislative_coordination).
narrative_ontology:cs_drift_state('536af850-6225-42b6-be40-396d43dcac09', contemporary_executive_power_assertions, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('536af850-6225-42b6-be40-396d43dcac09', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, popular_sovereignty_principle).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, electoral_public).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elected head of state and chief executive. Can initiate legislation, appoint the government, and dissolve the Assembly. But cannot implement major policy without legislative authorization; if the Assembly withdraws confidence through a censure motion or blocks key legislation, the president must either negotiate, compromise, or accept replacement of the government. Faces institutional constraints when the Assembly opposes the executive agenda.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, president, payer).

% Parliamentary majority (Assembly members) controls the budget, can force government resignation through censure, and must authorize legislation. The coordination constraint binds the executive to legislative will; the majority benefits from veto power over executive policy, ensuring no executive can govern without assembly consent.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, legislative_majority, agenda_setter).

% Citizens who elect both president and Assembly. The constraint ensures neither branch can act unilaterally; alternation between left and right executive coalitions occurs via elections. Public benefits from the fact that no single person can rule without legislative check.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, electoral_public, beneficiary,
    organized, biographical, mobile, national).

% Interprets the constitutional distribution of powers; confirms Assembly elections and electoral procedures; can invalidate laws as unconstitutional. Operates outside the executive-legislative contest but enforces the constraint's boundaries.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_court, observer,
    institutional, generational, analytical, national).

% Parliamentary minority and extra-parliamentary critics who would object to executive governance without legislative authorization. They have a voice within the Assembly but structurally cannot control outcomes unless their coalition grows.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, minority_opposition, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the core post-World War II democratic problem: how to prevent executive monopoly on power while maintaining coherent governance. The constraint ensures major policy changes require legislative consensus, preventing a single person from imposing ideology on the nation. Coordinates presidential electoral authority (direct popular mandate) with legislative authority (representative mandate).
% TRANSFER_FUNCTION: Transfers veto power from the executive alone to the executive-legislative dyad. The president retains agenda-setting capacity but cannot implement without assembly authorization. The assembly gains negative power to block executive initiatives.
% ABSENT_VOICES: Authoritarian executives (those who would prefer unilateral rule) are structurally excluded from the conversation. Alternative constitutional designs—pure parliamentarism, pure presidentialism—would be advocated by academics and reform movements but are constitutionally foreclosed within this reading.
% DISAPPEARANCE_RATIONALE: If legislative authorization requirement vanished, the president would have unilateral power to legislate, set budgets, and govern for seven years without legislative check. Assembly vetoes would become advisory. The entire distribution of democratic authority would shift from shared power to executive dominance.
% FOUNDING_PROBLEM: Europe's interwar democracies collapsed when strong executives (or weak executives who enabled autocrats) faced no institutional constraint. The Fifth Republic was designed so that no president could accumulate unchecked power; the Assembly veto was the structural answer to fascism's institutional vulnerability.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and political scientists studying democratic resilience attest the founding problem (autocratic executive capture) remains live as an empirical concern. Contemporary legislative-executive cohabitation disputes (2002–2007, 2017–2022) and reform debates confirm the constraint's active defense against executive overreach. Opposition parties and civil society monitor for violations. No party with power has formally renounced the constitutional requirement for legislative authorization.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Base extractiveness (0.28 at end) reflects the fact that the executive faces real veto points—the Assembly can block legislation, refuse budgets, or censure the government. This is not zero extraction because exercising the veto imposes costs on the executive (delayed policy, forced negotiation) and the Assembly extracts side-payments through coalition-building. But it is substantially below a pure snare (which would show ~0.65+) because the veto is constitutional, not coercive, and the executive retains agenda-setting power and can dissolve the Assembly as counterbalance. Theater ratio rises slightly (0.12 to 0.22 across the interval) because enforcement increasingly operates through public debate about constitutional limits rather than through active suppression—constitutional interpretation becomes the primary enforcement mechanism, which is inherently more performative than pure institutional veto. Suppression requirement is minimal and stable (0.10 to 0.15) because the constraint's persistence depends on constitutional legitimacy and electoral cycles, not on coercive suppression of dissent. The measurements share a single time grid (0, 3, 6, 9, 12, 15, 18, 21, 25) so every metric is authored at every point; the slight rise in theater and suppression mid-interval followed by stabilization reflects periods of cohabitation disputes (2002–2007 electoral cycle) where procedural and constitutional interpretation became more salient than under unified governments.
 *
 * PERSPECTIVAL GAP:
 *   From the legislative majority's seat, the constraint is a coordination mechanism ensuring no executive monopoly—they are beneficiaries. From the president's seat during divided government, the constraint functions as a veto point imposing costs on preferred policy—they are partial targets when the Assembly opposes executive initiatives. From the electoral public's seat, the constraint is a form of power-sharing that ensures neither branch can dominate. The engine computes these divergent types from the structural data: the legislative majority will classify the constraint as rope (coordination), while a president facing Assembly opposition will classify the same constraint as tangled_rope (coordination + extraction). This perspectival gap is the core of the kernel contest: the reading declares what the constraint IS (rope, democratic coordination), but different institutional seats experience different types when power is actually distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative majority: d near 0.2 (beneficiary; controls outcomes; arbitrage exit via dissolution or coalition-switching). President: d near 0.4 under unified government (moderate beneficiary; can initiate but needs legislative cooperation), d near 0.6 under divided government (partial target; legislative veto constrains preferred policy). Electoral public: d near 0.3 (distributed beneficiary; benefits from non-autocratic structure but diffuse costs from legislative gridlock). The directionality overrides are not needed here because the beneficiary/victim structure is clear: no agent is fully extracted from, but power is distributed such that the legislative majority holds the tiebreaker. Under this reading, no one is a victim in the snare sense; all are participants in a democratic coordination mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing executive autocracy) remains live as an institutional concern, not dead. Every contemporary dispute about executive power (emergency decrees, constitutional interpretation, executive order authority) invokes the founding problem's persistence. The Assembly continues to exercise its legislative veto; no executive faction has renounced the authorization requirement as obsolete. Mandatrophy is not present because the constraint's function (preventing autocracy via legislative coordination) persists and is actively defended. The slight rise in theater ratio mid-interval reflects not mandatrophy but the shift toward constitutional interpretation as enforcement mechanism during cohabitation disputes—the function remains, but its visibility changes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_hyper_presidential,
    'Is the Fifth Republic''s constitutional design fundamentally a coordination mechanism binding executive to legislative will, or is it a facade masking presidential dominance when a cohesive executive majority exists?',
    'Observe behavior under different cohabitation scenarios: when president and Assembly majority diverge ideologically, can the president implement policy unilaterally or must compromise? Empirical test: does the executive initiate major legislation that fails Assembly passage, or does it pre-negotiate with legislative leadership?',
    'If the president repeatedly implements unilateral policy despite Assembly opposition, the reading collapses toward hyper_presidential. If the president always negotiates and backs down when Assembly withhold support, the reading is confirmed. The middle case (occasional override, frequent negotiation) indicates the reading is empirically contestable and structurally hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_hyper_presidential, empirical, 'Whether presidential power is actually constrained by legislative authorization requirement or merely appears so.').

omega_variable(
    alternative_institutional_framing,
    'Does the constitutional design instantiate a ''coordination rope'' (executives and legislatures solving a shared problem of preventing autocracy) or a ''power contest snare'' where the legislature uses authorization requirement to extract from the executive branch?',
    'Survey institutional stakeholders (presidents, prime ministers, Assembly leaders, constitutional scholars) from both sides of cohabitation disputes about whether they view the constraint as legitimate coordination or as partisan veto. Examine whether legislative blocking is motivated by policy disagreement or by executive imprisonment regardless of policy content.',
    'If the constraint is experienced as legitimate coordination by both branches, the reading holds. If the Assembly systematically blocks executive initiatives for partisan reasons while claiming constitutional principle, the constraint functions as snare. The lived experience of the institutional actors—whether they consent to the constraint or merely comply under duress—differentiates rope from snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_institutional_framing, conceptual, 'Whether the constraint''s legitimacy derives from shared democratic problem-solving or from power asymmetry between branches.').

omega_variable(
    sibling_reading_decomposition,
    'Is this reading (parliamentary constraint) empirically distinct from the cohabitation equilibrium reading, or do they describe the same constitutional arrangement from different partisan vantage points?',
    'Examine cohabitation periods (1986–1988, 1993–1995, 1997–2002) where president and Assembly majority were ideologically opposed. Under the parliamentary reading, the executive should be substantially constrained. Under the cohabitation reading, power should be stable at an equilibrium split between president and prime minister. The empirical test: did the president retain agenda-setting power despite opposition, or did the prime minister take over executive function?',
    'If cohabitation produces prime ministerial dominance, the readings are distinct: parliamentary describes the formal rule, cohabitation describes the practice divergence. If cohabitation still leaves president with meaningful veto, the readings collapse into one constraint viewed from different seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_decomposition, empirical, 'Whether parliamentary constraint reading and cohabitation reading are separate constraints or variants of one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t0, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(fift_tr_t0, observed).
narrative_ontology:measurement(fift_tr_t3, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 3, 0.15).
narrative_ontology:measurement_basis(fift_tr_t3, observed).
narrative_ontology:measurement(fift_tr_t6, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement_basis(fift_tr_t6, observed).
narrative_ontology:measurement(fift_tr_t9, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 9, 0.2).
narrative_ontology:measurement_basis(fift_tr_t9, observed).
narrative_ontology:measurement(fift_tr_t12, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement_basis(fift_tr_t12, observed).
narrative_ontology:measurement(fift_tr_t15, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(fift_tr_t15, observed).
narrative_ontology:measurement(fift_tr_t18, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement_basis(fift_tr_t18, observed).
narrative_ontology:measurement(fift_tr_t21, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 21, 0.22).
narrative_ontology:measurement_basis(fift_tr_t21, observed).
narrative_ontology:measurement(fift_tr_t25, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(fift_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(fift_be_t0, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(fift_be_t0, observed).
narrative_ontology:measurement(fift_be_t3, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 3, 0.22).
narrative_ontology:measurement_basis(fift_be_t3, observed).
narrative_ontology:measurement(fift_be_t6, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 6, 0.26).
narrative_ontology:measurement_basis(fift_be_t6, observed).
narrative_ontology:measurement(fift_be_t9, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 9, 0.28).
narrative_ontology:measurement_basis(fift_be_t9, observed).
narrative_ontology:measurement(fift_be_t12, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 12, 0.29).
narrative_ontology:measurement_basis(fift_be_t12, observed).
narrative_ontology:measurement(fift_be_t15, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 15, 0.3).
narrative_ontology:measurement_basis(fift_be_t15, observed).
narrative_ontology:measurement(fift_be_t18, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 18, 0.29).
narrative_ontology:measurement_basis(fift_be_t18, observed).
narrative_ontology:measurement(fift_be_t21, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 21, 0.28).
narrative_ontology:measurement_basis(fift_be_t21, observed).
narrative_ontology:measurement(fift_be_t25, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement_basis(fift_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t0, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(fift_su_t0, observed).
narrative_ontology:measurement(fift_su_t3, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 3, 0.11).
narrative_ontology:measurement_basis(fift_su_t3, observed).
narrative_ontology:measurement(fift_su_t6, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 6, 0.12).
narrative_ontology:measurement_basis(fift_su_t6, observed).
narrative_ontology:measurement(fift_su_t9, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 9, 0.13).
narrative_ontology:measurement_basis(fift_su_t9, observed).
narrative_ontology:measurement(fift_su_t12, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 12, 0.14).
narrative_ontology:measurement_basis(fift_su_t12, observed).
narrative_ontology:measurement(fift_su_t15, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement_basis(fift_su_t15, observed).
narrative_ontology:measurement(fift_su_t18, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 18, 0.15).
narrative_ontology:measurement_basis(fift_su_t18, observed).
narrative_ontology:measurement(fift_su_t21, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 21, 0.15).
narrative_ontology:measurement_basis(fift_su_t21, observed).
narrative_ontology:measurement(fift_su_t25, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement_basis(fift_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__parliamentary_constraint_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution__cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This reading is one of three constraint stories decomposing the Fifth Republic Constitution kernel (constraint_id: fifth_republic_constitution, reading_id: parliamentary_constraint_reading). The sibling readings are hyper_presidential_reading (executive autonomy emphasis) and cohabitation_equilibrium_reading (split executive during divided government). All three readings share the same constitutional text but differ in their ε-referent's interpreted boundaries: this reading's referent is the president's authority constrained by legislative coordination; the hyper_presidential reading's referent is the president's unilateral executive capacity; the cohabitation reading's referent is the power equilibrium between president and prime minister. Each reading is a complete, ε-invariant constraint story. The readings are linked via network.affects_constraints to enable decomposition analysis and cross-reading coupling detection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
