% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__coordinate_construction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: constitutional_authority_boundary__coordinate_construction_reading
 *   human_readable: Coordinate Construction: Distributed Interpretive Authority Among Co-Equal Branches
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint instantiates the coordinate construction
 *   (departmentalist) reading of the constitutional authority boundary
 *   kernel: the constitutional text is read as establishing three co-equal
 *   branches, each possessing interpretive authority over the constitution
 *   within its own sphere of operation, with no branch's reading binding as
 *   final upon the others outside the specific dispute adjudicated. This is
 *   distinct from the judicial supremacy reading (courts as final arbiter)
 *   and the parliamentary primacy reading (legislature as final arbiter) —
 *   those are separate constraints with their own ε and stakeholder
 *   structures, linked here via network.affects_constraints. Under this
 *   reading, moderate extraction arises not from a monopoly beneficiary
 *   capturing rents, but from the diffuse cost inter-branch conflict imposes
 *   on those needing prompt, durable resolution.
 *
 * KEY AGENTS:
 *   - sitting_judiciary: interprets within adjudicated cases, cannot compel other branches (institutional/analytical exit)
 *   - legislative_majority_coalitions: legislates against its own constitutional reading, can override or narrow judicial holdings (institutional/constrained)
 *   - incumbent_executive_officeholders: executes under its own reading, can decline enforcement (institutional/constrained)
 *   - litigants_facing_inter_branch_deadlock: bears the cost of unresolved disputes directly (moderate/trapped)
 *   - minority_political_factions and citizens_seeking_final_resolution: bear diffuse costs of instability (powerless/trapped)
 *   - constitutional_scholars: analytical observer of the historical pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__coordinate_construction_reading, 0.42).
domain_priors:suppression_score(constitutional_authority_boundary__coordinate_construction_reading, 0.38).
domain_priors:theater_ratio(constitutional_authority_boundary__coordinate_construction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Coordinate Construction: Distributed Interpretive Authority Among Co-Equal Branches").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, '5e233c08-452f-42f3-a1d3-352913c787cf').
narrative_ontology:cs_kernel_codification('5e233c08-452f-42f3-a1d3-352913c787cf', fixed_text).
narrative_ontology:cs_authority_grounding('5e233c08-452f-42f3-a1d3-352913c787cf', distributed).
narrative_ontology:cs_reading_relation('5e233c08-452f-42f3-a1d3-352913c787cf', constitutional_authority_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e233c08-452f-42f3-a1d3-352913c787cf', constitutional_authority_boundary__parliamentary_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('5e233c08-452f-42f3-a1d3-352913c787cf', foundational, no_branch_possesses_final_interpretive_monopoly).
narrative_ontology:cs_axiom_status(no_branch_possesses_final_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('5e233c08-452f-42f3-a1d3-352913c787cf', no_branch_possesses_final_interpretive_monopoly, conventional).
narrative_ontology:cs_axiom('5e233c08-452f-42f3-a1d3-352913c787cf', foundational, each_branch_interprets_authoritatively_within_its_own_sphere).
narrative_ontology:cs_axiom_status(each_branch_interprets_authoritatively_within_its_own_sphere, holdable).
narrative_ontology:cs_axiom_grounding('5e233c08-452f-42f3-a1d3-352913c787cf', each_branch_interprets_authoritatively_within_its_own_sphere, conventional).
narrative_ontology:cs_reference_frame('5e233c08-452f-42f3-a1d3-352913c787cf', departmentalist_founding_settlement).
narrative_ontology:cs_drift_state('5e233c08-452f-42f3-a1d3-352913c787cf', post_administrative_state_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5e233c08-452f-42f3-a1d3-352913c787cf', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, incumbent_executive_officeholders).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, legislative_majority_coalitions).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, sitting_judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, litigants_facing_inter_branch_deadlock).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, minority_political_factions).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, citizens_seeking_final_resolution).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, departmentalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitution when cases come before it and can decline to enforce a statute it finds unconstitutional within its own proceedings, but has no mechanism to compel the other branches to adopt its reading outside the judgment's direct parties. Benefits from the deference its interpretations receive in practice, but must rely on the executive to enforce judgments and cannot prevent legislative attempts to reframe or re-legislate around adverse rulings.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, sitting_judiciary, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, sitting_judiciary, agenda_setter).

% Enacts statutes based on its own reading of constitutional limits on its power, can pass legislation that tests or narrows judicial holdings, and controls funding and structural levers over the judiciary and executive. Benefits from retaining interpretive latitude but faces the cost of prolonged uncertainty when its acts are challenged and no branch can definitively settle the question quickly.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, legislative_majority_coalitions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, legislative_majority_coalitions, agenda_setter).

% Executes the law under its own understanding of constitutional constraints, can decline to enforce or slow-walk enforcement of judicial rulings it disputes (non-acquiescence), and interprets the scope of its own enumerated powers in the course of governing. Benefits from operational discretion but bears reputational and legitimacy costs when its non-acquiescence appears to defy settled process.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, incumbent_executive_officeholders, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, incumbent_executive_officeholders, agenda_setter).

% Brings a claim expecting a final, enforceable answer to a constitutional question, but discovers that a favorable judicial ruling may be narrowed by subsequent legislation, resisted by executive non-enforcement, or left unresolved across branches for years. Cannot exit the system — the dispute must be litigated within the very structure that produces the deadlock.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, litigants_facing_inter_branch_deadlock, payer,
    moderate, biographical, trapped, national).

% Seeks protection of rights or process through constitutional claims but finds that without a single final arbiter, an adverse ruling in one branch can be circumvented by coordinated action in another, especially when the same coalition controls both the legislature and executive. Has no institutional lever to force resolution and must rely on shifting political coalitions across election cycles.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, minority_political_factions, payer,
    powerless, biographical, trapped, national).

% Wants clear, stable rules for planning economic and personal life around constitutional questions (e.g., property, speech, contract) but experiences prolonged ambiguity when branches disagree, since no institution can issue the last word. Bears the diffuse cost of instability but has no direct standing to force closure.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, citizens_seeking_final_resolution, payer,
    powerless, biographical, trapped, national).

% Studies the practice of departmentalism and coordinate construction across historical episodes (e.g., Jackson's Bank veto message, Lincoln's suspension of habeas corpus, modern executive non-enforcement disputes) to assess whether the arrangement produces durable equilibrium or chronic instability. Takes no side in any particular dispute but documents the pattern for constitutional design purposes.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributes interpretive authority across three branches so that no single institution can seize permanent, unchallenged control over constitutional meaning; each branch's power to interpret within its sphere checks the others' ambitions and preserves inter-branch equilibrium.
% TRANSFER_FUNCTION: Moves the cost of unresolved constitutional disputes onto litigants, minority factions, and citizens who need finality, while allowing incumbent officeholders in all three branches to retain interpretive latitude and avoid being permanently bound by another branch's reading.
% ABSENT_VOICES: Litigants and minority factions who bear the practical cost of deadlock have no seat at the table when branches negotiate or posture against one another; their interest in prompt, durable resolution is structurally subordinate to each branch's interest in preserving its own interpretive prerogative.
% DISAPPEARANCE_RATIONALE: If distributed interpretive authority collapsed into a single final arbiter overnight (in either direction — judicial supremacy or parliamentary primacy), the practice of inter-branch negotiation, non-acquiescence, and legislative override would end; disputes would resolve in one forum rather than being contested across three, and the strategic behavior of legislators and executives currently shaped by anticipated judicial or political pushback would change substantially.
% FOUNDING_PROBLEM: The founding generation sought to prevent any single branch (particularly a monarch-like executive or an unchecked legislature) from becoming the sole authoritative interpreter of fundamental law, having experienced concentrated authority under colonial and early state constitutional arrangements they judged prone to abuse.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era and comparative constitutional scholars outside any branch's own interest attest that concern over concentrated interpretive power was a genuine founding-era concern, documented in Federalist No. 49 and No. 78 debates. However, some scholars argue the coordinate construction problem the framers actually addressed (preventing tyranny) is largely solved by modern electoral and civil-society checks, and that today's persistence of distributed authority instead serves each branch's institutional self-preservation — a status contested between constitutional historians and each branch's own institutional counsel.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__coordinate_construction_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).
:- end_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored in the moderate band (0.42 at interval end) reflecting the expected structural delta for this reading: no single institution captures a monopoly rent, but the coordination cost of resolving genuine three-way disagreement is real and lands disproportionately on parties needing finality rather than on any of the three branches themselves. Suppression is moderate (0.38) — each branch retains genuine exit through override, non-acquiescence, or narrow future rulings, but litigants trapped in active disputes have no comparable exit. Theater ratio is modest (0.28) and rises slowly, reflecting the increasing use of departmentalist rhetoric (each branch declaring its own reading 'settled' within its sphere) as a strategic move rather than a genuine resolution mechanism, without dominating the constraint's operation. Resistance is comparatively high (0.55) because this reading is actively contested by proponents of both sibling readings, who argue distributed authority produces instability rather than genuine coordination.
 *
 * PERSPECTIVAL GAP:
 *   From each branch's own seat, coordinate construction looks like healthy separation of powers — a rope. From the seat of a litigant awaiting final resolution or a minority faction watching a favorable ruling get circumvented by legislative override, the same structure looks like an extractive stalemate mechanism that protects incumbents in all three branches at the litigant's expense. The engine computes this divergence from the differing power/exit declarations; the claimed_type (tangled_rope) is authored to reflect the analytical seat's judgment that both readings are structurally present simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   All three branches sit near the beneficiary end of directionality: each retains interpretive latitude within its sphere and none is fully bound by another's reading, so none experiences the constraint as pure extraction from itself. Litigants, minority factions, and ordinary citizens sit toward the target end: they bear the transaction cost of inter-branch disagreement (delay, reversal, non-enforcement) without any comparable capacity to force resolution. This maps cleanly to the beneficiary/victim declarations — the three branches are declared beneficiaries because coordinate construction preserves each one's institutional prerogative; the payer groups are declared victims because they absorb the cost of that preserved latitude.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing any one branch from becoming the sole, unchecked interpreter of fundamental law — remains partially live: historical episodes of executive overreach or legislative encroachment recur, suggesting the underlying concern the framers addressed has not disappeared. But the founding_problem_status is authored as contested rather than clearly live or dead, because the specific mechanism (three-way distributed authority with no final arbiter) increasingly serves each branch's institutional self-preservation as much as it serves the original anti-tyranny purpose. The classification as tangled_rope rather than rope reflects this: there is a genuine coordination function (preventing concentrated interpretive power) but it operates alongside asymmetric extraction (litigants and minority factions pay for the branches' preserved latitude) and requires active enforcement (each branch must actively assert and defend its sphere against encroachment) — this prevents the naive move of calling coordinate construction pure coordination just because the framers' stated purpose was benign.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinate_construction_stability_vs_deadlock,
    'Does distributed interpretive authority among co-equal branches produce a stable, self-correcting equilibrium over the long run, or does it produce chronic, unresolved deadlock that primarily benefits incumbents in all three branches at the expense of those needing prompt resolution?',
    'Comparative historical analysis across jurisdictions and eras that have operated under coordinate construction (e.g., the pre-Marbury and early departmentalist periods in the US, contemporary systems with weak-form judicial review) measuring resolution time and outcome durability for constitutional disputes.',
    'If the evidence favors stable equilibrium, this reading''s coordination function dominates and the constraint drifts toward genuine rope; if the evidence favors chronic deadlock serving incumbent branches, the tangled_rope classification is validated and the extraction component is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_construction_stability_vs_deadlock, empirical, 'Whether distributed authority stabilizes or chronically deadlocks constitutional disputes.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the coordinate construction reading itself a description of what the constitutional text actually establishes, or is it one contestable interpretive choice among the three sibling readings, adopted here because it is the reading least favorable to any single monopoly beneficiary?',
    'Textual and historical analysis of the founding debates (Federalist Papers, ratification debates) cross-referenced against subsequent judicial and legislative practice to determine which reading the founding generation''s own statements most plausibly support, and how much interpretive latitude that record actually leaves open.',
    'If the historical record decisively supports one of the sibling readings over coordinate construction, this story''s classification as the operative reading is weakened even though it would remain independently ε-invariant and valid as one instantiated reading among the family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether coordinate construction is the textually correct reading or one contestable framing among three.').

omega_variable(
    incumbent_capture_of_departmentalism,
    'Do all three branches benefit symmetrically from distributed interpretive authority, or does one branch (typically the executive, via non-acquiescence and enforcement discretion) capture disproportionate practical benefit relative to the other two?',
    'Track record analysis of instances of executive non-enforcement, legislative override, and judicial invalidation across a comparable historical sample to determine which branch''s assertions of interpretive authority most often prevail in practice.',
    'If executive non-acquiescence proves systematically more effective than legislative override or judicial invalidation, the beneficiary structure is not symmetric and the constraint may function closer to an executive-favoring tangled rope than a genuinely tri-partite one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_capture_of_departmentalism, empirical, 'Whether the three branches benefit symmetrically or one branch captures disproportionate practical advantage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 0, 240).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(cons_tr_t80, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(cons_tr_t120, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 120, 0.22).
narrative_ontology:measurement(cons_tr_t160, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 160, 0.24).
narrative_ontology:measurement(cons_tr_t200, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 200, 0.26).
narrative_ontology:measurement(cons_tr_t240, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 240, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement(cons_be_t80, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 80, 0.34).
narrative_ontology:measurement(cons_be_t120, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 120, 0.37).
narrative_ontology:measurement(cons_be_t160, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 160, 0.4).
narrative_ontology:measurement(cons_be_t200, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 200, 0.41).
narrative_ontology:measurement(cons_be_t240, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 240, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement(cons_su_t80, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 80, 0.33).
narrative_ontology:measurement(cons_su_t120, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 120, 0.34).
narrative_ontology:measurement(cons_su_t160, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 160, 0.36).
narrative_ontology:measurement(cons_su_t200, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 200, 0.37).
narrative_ontology:measurement(cons_su_t240, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 240, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language label 'the constitutional authority boundary' per the epsilon-invariance principle. Each reading (coordinate_construction, judicial_supremacy, parliamentary_primacy) is authored as an independent constraint with its own epsilon, beneficiary/victim structure, and classification, because the three readings produce structurally distinct claims about who holds final interpretive authority — averaging or blending them would violate epsilon-invariance. All three link to each other via affects_constraints to preserve the family relationship: judicial_supremacy_reading typically produces a narrower, more concentrated beneficiary set (the judiciary) and a correspondingly different epsilon profile; parliamentary_primacy_reading concentrates beneficiary structure in the legislature. This coordinate_construction_reading sits structurally between them, distributing rather than concentrating interpretive authority, which is reflected in its moderate epsilon (0.35-0.50 band) and absence of a monopoly beneficiary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
