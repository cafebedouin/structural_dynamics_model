% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_authority_boundary__coordinate_construction_reading
 *   human_readable: Coordinate Construction: Distributed Interpretive Authority Across Co-Equal Branches
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   The constitutional text establishes three co-equal branches of government
 *   with a written, binding foundational law. The coordinate construction
 *   reading interprets this as distributing constitutional interpretive
 *   authority among the three branches, each authoritative within its sphere,
 *   with no single final arbiter. This differs structurally from judicial
 *   supremacy (courts interpret finally) and parliamentary primacy
 *   (legislatures override the text). Under coordinate construction, when
 *   branches disagree on constitutional meaning, the dispute is settled
 *   through political and structural mechanisms—legislative override,
 *   executive non-acquiescence, constitutional amendment—rather than
 *   deference to a single authoritative seat. The constraint is claimed as
 *   rope because it coordinates multiple independent power centers around a
 *   shared foundational text; it also carries extractive potential because
 *   the distribution creates ambiguity about which branch prevails when they
 *   conflict, and that ambiguity itself becomes an object of control.
 *
 * KEY AGENTS:
 *   - Judicial branch: interprets constitution in cases; not final across system
 *   - Legislative branch: interprets in enacting law; can override judicial readings
 *   - Executive branch: interprets in executing law; can non-acquiesce on constitutional grounds
 *   - Citizens: receive coordination benefit; bear uncertainty cost
 *   - State governments: bound by federal interpretation but retain reserved powers
 *   - Advocates of judicial supremacy: excluded from this reading's framework
 *   - Advocates of parliamentary primacy: excluded from this reading's framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__coordinate_construction_reading, 0.42).
domain_priors:suppression_score(constitutional_authority_boundary__coordinate_construction_reading, 0.28).
domain_priors:theater_ratio(constitutional_authority_boundary__coordinate_construction_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__coordinate_construction_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Coordinate Construction: Distributed Interpretive Authority Across Co-Equal Branches").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "political/constitutional").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, 'ab58d5ad-7cd2-4723-a545-c4834e55b3b4').
narrative_ontology:cs_kernel_codification('ab58d5ad-7cd2-4723-a545-c4834e55b3b4', fixed_text).
narrative_ontology:cs_authority_grounding('ab58d5ad-7cd2-4723-a545-c4834e55b3b4', lineage).
narrative_ontology:cs_interpretation_layer_present('ab58d5ad-7cd2-4723-a545-c4834e55b3b4').
narrative_ontology:cs_reading_relation('ab58d5ad-7cd2-4723-a545-c4834e55b3b4', constitutional_authority_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('ab58d5ad-7cd2-4723-a545-c4834e55b3b4', constitutional_authority_boundary__parliamentary_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('ab58d5ad-7cd2-4723-a545-c4834e55b3b4', foundational, no_single_final_interpreter).
narrative_ontology:cs_axiom_status(no_single_final_interpreter, holdable).
narrative_ontology:cs_axiom_grounding('ab58d5ad-7cd2-4723-a545-c4834e55b3b4', no_single_final_interpreter, deontological).
narrative_ontology:cs_axiom('ab58d5ad-7cd2-4723-a545-c4834e55b3b4', foundational, written_constitution_binding_all).
narrative_ontology:cs_axiom_status(written_constitution_binding_all, holdable).
narrative_ontology:cs_axiom_grounding('ab58d5ad-7cd2-4723-a545-c4834e55b3b4', written_constitution_binding_all, conventional).
narrative_ontology:cs_reference_frame('ab58d5ad-7cd2-4723-a545-c4834e55b3b4', co_equal_branches_distributed_authority).
narrative_ontology:cs_drift_state('ab58d5ad-7cd2-4723-a545-c4834e55b3b4', contemporary_judicial_supremacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab58d5ad-7cd2-4723-a545-c4834e55b3b4', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, institutional_balance_itself).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, executive_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, state_governments).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, citizens_as_political_actors).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, state_governments).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, checks_and_balances_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitution in cases brought before it; resolves disputes between branches and between state and citizen. Under the coordinate construction reading, judicial interpretation is authoritative within its cases but not final across the system—legislative and executive branches retain authority to interpret within their spheres and may act on readings that diverge from judicial pronouncements. Judges maintain institutional independence and collective prestige but must accept that their constitutional readings can be superseded through political process.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch, beneficiary).

% Enacts law and interprets constitutional limits on its own authority; can override or work around judicial constitutional readings through new legislation, constitutional amendment, or budgetary action. Under coordinate construction, legislative authority to interpret extends to constitutional questions about its own powers—it need not defer to judicial readings of congressional enumerated powers or structural limits. Bears the cost of political accountability for constitutional choices.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch, beneficiary).

% Executes law and interprets constitutional limits on executive power; retains authority to interpret the Take Care Clause, commander-in-chief powers, and treaty authority within its sphere. Under coordinate construction, executive may decline to enforce or implement judicial rulings on constitutional grounds if it believes the courts have erred; the remedy is political, not automatic acquiescence. Bears the cost of political and legal contestation when it non-acquiesces.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, executive_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, executive_branch, beneficiary).

% Apply constitutional law as interpreted by higher courts and their own institutional reading; coordinate construction creates ambiguity about whether they must follow Supreme Court precedent absolutely or whether they are entitled to question it if coordinate authority permits. Their actual role is constrained by precedent doctrine, but the coordinate reading leaves a theoretical opening.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, federal_courts_below_supreme, observer,
    institutional, generational, constrained, national).

% Bound by federal constitutional interpretation but retain reserved powers; coordinate construction creates space for state-level constitutional interpretation in federal questions affecting federalism. States bear the cost of legal uncertainty when federal branches disagree on constitutional meaning.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, state_governments, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, state_governments, beneficiary).

% Receive the coordination function of constitutional government: predictable rules, checks on concentrated power, and recourse to multiple branches and levels of government. Under coordinate construction, the distribution of interpretive authority means no single branch can impose constitutional meaning unilaterally, and political pressure can reshape constitutional understanding. Citizens also bear uncertainty about which interpretation will control in contested cases.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, citizens_as_political_actors, beneficiary,
    powerless, biographical, mobile, national).

% Would argue for a clearer hierarchy—that courts must have final authority to resolve constitutional disputes, and that executive or legislative non-acquiescence violates the rule of law. They are excluded from the coordinate construction's framings by its foundational commitment to distributed authority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, judicial_supremacy_advocates, excluded,
    organized, generational, constrained, national).

% Would argue for legislative primacy and the rejection of entrenched, unchallengeable constitutional law. They are excluded from the coordinate construction's commitment to a written, binding constitutional text that constrains all branches equally.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, parliamentary_supremacy_advocates, excluded,
    organized, generational, constrained, national).

% Sees the full structure: coordinate construction distributes interpretive authority as a deliberate structural choice, trades stability for flexibility, and creates persistent potential for inter-branch conflict where no branch accepts another's constitutional reading as binding.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a binding, written constitutional text interpreted by three institutionally independent branches, each with authority in its sphere, creating stability through the rule that constitutional meaning is not the monopoly of any single seat and dispute resolution occurs through political and structural mechanisms rather than final hierarchical adjudication.
% TRANSFER_FUNCTION: Distributes the power to declare constitutional meaning (and thus to invalidate or affirm actions) among judicial, legislative, and executive branches. The coordinate reading transfers authority FROM any single branch TO a multi-seat system, but does not move resources, money, or status from one branch to another—it moves interpretive authority itself.
% ABSENT_VOICES: Citizens subject to disputed constitutional interpretations have no direct seat in any branch; they experience the constraint through the outcomes of inter-branch disagreement. Subordinate courts below the Supreme Court technically exist but are subordinated in precedent doctrine, which may conflict with the coordinate reading's distributed-authority premise. International observers and states not party to the federal system have no voice but are affected by which reading controls U.S. behavior.
% DISAPPEARANCE_RATIONALE: If the coordinate construction reading—the premise that no single branch has final interpretive authority—vanished and one reading monopolized constitutional meaning, the entire structure of checks and balances would collapse. Either courts would become final and executive/legislative non-acquiescence would become unconstitutional (judicial supremacy); or legislatures would become final and written constitutional law would become subject to ordinary override (parliamentary primacy); or executives would arrogate final power. The distribution of authority is the load-bearing structure.
% FOUNDING_PROBLEM: The Framers faced a dual problem: (1) the need for a stable, binding foundational law that could constrain all actors and prevent tyranny, and (2) the impossibility of vesting final power to interpret that law in any single actor without recreating the tyranny the Constitution was meant to prevent. The coordinate construction reading asserts the Framers chose distributed authority as the solution: a written constitution binding on all, interpreted by all within their spheres, with no final arbiter.
% FOUNDING_PROBLEM_CORROBORATION: The Framers' intent on this question is genuinely contested. Federalist Papers passages can be read to support either distributed authority (Federalist 51 on checks and balances) or judicial supremacy (Federalist 78 on courts as interpreters of law). Scholars outside the benefiting branches cite Marbury v. Madison as evidence courts claimed final authority; others cite Lincoln's First Inaugural as evidence presidents reserved independent constitutional reading. No single testimony from OUTSIDE the three branches settles the founding question—the contest itself is the evidence.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__coordinate_construction_reading, 'none', 1).

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
 *   Extractiveness is moderate (0.42) because the coordinate construction reading itself is not inherently extractive—it distributes power rather than concentrating it. However, the ambiguity about which branch's reading controls creates a coordination problem that can be exploited: each branch can claim to be interpreting authentically while other branches resist, and the political resolution can favor whoever has the greatest political power at the moment. This is not the rope's core function but a side effect of the distributed-authority premise. Suppression is lower (0.28) because coordinate construction does not rely on suppressing alternatives—it explicitly permits them. The measurement series shows extractiveness and suppression rising modestly from 1789 (when the coordinate reading was most live and branches contested vigorously) to the mid-20th century (when judicial supremacy became increasingly dominant in practice), then plateauing as the coordinate construction persists in constitutional theory even as practice drifts toward judicial review as final. Theater is low throughout (0.08–0.22) because the constraint's coordination function is real—three independent branches do operate and do check each other—but the theatrical maintenance (claims of finality that are routinely circumvented) is moderate. The rise in theater from 1935 onward reflects the period in which judicial supremacy became the dominant reading in practice while coordinate construction persisted in constitutional theory and was occasionally invoked (Lincoln on executive power, legislative override of court decisions).
 *
 * PERSPECTIVAL GAP:
 *   From the judicial seat, coordinate construction is experienced as a constraint on judicial authority—courts must accept that their constitutional readings can be superseded. From the legislative seat, it is experienced as empowering—the legislature retains authority to interpret and override. From the executive seat, it is similarly empowering—the executive can assert independent constitutional reading. Analytical observers see the entire structure as distributed rather than hierarchical. The engine should compute different types from these different seats: the judicial seat may compute more extractive (the constraint limits its finality), the legislative and executive seats less extractive. The coordinate construction's neutrality in distributing authority makes inter-branch perspective divergence a key feature.
 *
 * DIRECTIONALITY LOGIC:
 *   The coordinate construction reading does not establish a concentrated beneficiary or victim structure. Instead, it distributes authority and thus distributes the relative advantage/disadvantage of the constraint across seats. The judicial branch benefits from independent authority (d toward beneficiary) but loses finality (d toward target). The legislative branch benefits from retained interpretive authority (d toward beneficiary) but accepts political accountability (d toward target). The executive branch similarly benefits and pays. Citizens benefit from the coordination function (three-branch check) but pay in uncertainty about which interpretation controls (d near symmetric, perhaps slightly toward beneficiary for the coordination good). No seat is trapped, and all have moderate exit options (legislative override, constitutional amendment, political pressure). The 'beneficiary' field lists 'institutional_balance_itself' as a non-actor because the primary beneficiary is the structural outcome—the maintenance of balanced authority—not any individual branch. This triggers FSM evaluation; an omega documents the ambiguity.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordinate construction reading faces a mandatrophy candidate: the founding problem was preventing tyranny through distributed authority, but judicial supremacy as an interpretive norm emerged in the 20th century and reduced the distribution to a de facto hierarchy. If courts have become final arbiters in practice, does the coordinate construction reading persist as a functional constraint or as theatrical maintenance (theater_ratio elevation)? The measurements show theater rising from 1935 onward, consistent with mandatrophy—the constraint persists in constitutional theory and is invoked in specific contexts (executive defiance, legislative override) but is not the dominant operational reading. However, the constraint does not fully atrophy: executive non-acquiescence has been rare but real (Lincoln, Truman, Nixon), and legislative override and constitutional amendment remain live options. The present measurement (2024) holds theater at 0.22, not approaching Piton levels (which would require theater > 0.5 and extractiveness declining). The reading is contested but not dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    foundational_commitment_stability,
    'Is the coordinate construction reading a foundational commitment of the constitutional order, or is it a normative preference that can be overridden by practice?',
    'Historical analysis of Framing intent (Federalist Papers, Convention records, Ratification debates) and doctrinal evolution from Marbury v. Madison onward; contemporary constitutional theory assessments of whether coordinate construction is binding on interpretation.',
    'If foundational, the rise of judicial supremacy in practice represents a drift or violation of the coordinate construction rather than a permitted reading—the constraint persists unchanged and judicial supremacy is unconstitutional. If a preference rather than commitment, judicial supremacy can be a permissible reading and the coordinate construction is one option among interpretive approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_commitment_stability, conceptual, 'Whether distributed authority is binding commitment or optional interpretive approach.').

omega_variable(
    false_summit_coordinate_construction,
    'Is the coordinate construction reading a genuine structural feature of constitutional design, or does it benefit the branches (by permitting each to claim authority) in ways that disguise concentrated power as distributed authority?',
    'Empirical analysis of actual outcomes when branches disagree: which reading prevails systematically? Does the distribution of interpretive authority correlate with distribution of political outcomes? Does any branch routinely dominate?',
    'If genuine distribution of outcomes, coordinate construction is a real rope. If judicial supremacy or another reading dominates outcomes despite coordinate construction theory, the reading is a false summit benefiting branches by providing legitimacy cover for concentrated power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_coordinate_construction, empirical, 'Whether coordinate construction genuinely distributes authority or provides legitimacy cover for concentrated power.').

omega_variable(
    inter_branch_conflict_extraction,
    'When branches disagree on constitutional interpretation, is the resulting conflict a coordination cost (inevitable friction in distributed decision-making) or an extraction mechanism (ambiguity exploited by the branch with greatest political power)?',
    'Analysis of inter-branch conflicts (Lincoln-Congress, executive-Court standoffs, legislative overrides): in each case, which branch''s reading prevailed? What was the cost to the losing branch and to citizens? Does the branch with greatest political power systematically win?',
    'If conflict is coordination cost, extractiveness is inherent to the constraint and properly measured at 0.42. If exploitation mechanism, the effective extractiveness is higher for weaker branches and lower for powerful ones—directionality should be individually overridden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inter_branch_conflict_extraction, empirical, 'Whether inter-branch conflict over interpretation is distributive (splitting losses) or extractive (concentrating gains).').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Which reading of the constitutional_authority_boundary kernel is the operative one in contemporary U.S. constitutional practice—coordinate construction, judicial supremacy, or parliamentary primacy? Or do all three coexist without one achieving dominance?',
    'Doctrinal analysis of Supreme Court opinions claiming finality versus presidential non-acquiescence versus legislative override—does any single reading claim monopoly? Do branches act as if bound by a single reading, or do they each assert independent authority?',
    'If one reading has achieved dominance, the coordinate construction is a recessive reading and should be classified as contested/declining. If all three coexist, the constraint is correctly characterized as coexisting with its siblings and as live, not Piton. If the reading oscillates (dominant in some periods, recessive in others), measurement points should capture the oscillation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, empirical, 'The actual operative reading of constitutional authority in contemporary and historical practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1789, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1789, 0.08).
narrative_ontology:measurement_basis(cons_tr_t1789, projected).
narrative_ontology:measurement(cons_tr_t1869, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1869, 0.12).
narrative_ontology:measurement_basis(cons_tr_t1869, projected).
narrative_ontology:measurement(cons_tr_t1935, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1935, 0.18).
narrative_ontology:measurement_basis(cons_tr_t1935, observed).
narrative_ontology:measurement(cons_tr_t1974, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1974, 0.22).
narrative_ontology:measurement_basis(cons_tr_t1974, observed).
narrative_ontology:measurement(cons_tr_t2000, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 2000, 0.21).
narrative_ontology:measurement_basis(cons_tr_t2000, observed).
narrative_ontology:measurement(cons_tr_t2024, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(cons_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t1789, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1789, 0.25).
narrative_ontology:measurement_basis(cons_be_t1789, projected).
narrative_ontology:measurement(cons_be_t1869, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1869, 0.35).
narrative_ontology:measurement_basis(cons_be_t1869, projected).
narrative_ontology:measurement(cons_be_t1935, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1935, 0.4).
narrative_ontology:measurement_basis(cons_be_t1935, observed).
narrative_ontology:measurement(cons_be_t1974, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1974, 0.45).
narrative_ontology:measurement_basis(cons_be_t1974, observed).
narrative_ontology:measurement(cons_be_t2000, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement_basis(cons_be_t2000, observed).
narrative_ontology:measurement(cons_be_t2024, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(cons_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1789, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1789, 0.15).
narrative_ontology:measurement_basis(cons_su_t1789, projected).
narrative_ontology:measurement(cons_su_t1869, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1869, 0.2).
narrative_ontology:measurement_basis(cons_su_t1869, projected).
narrative_ontology:measurement(cons_su_t1935, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1935, 0.28).
narrative_ontology:measurement_basis(cons_su_t1935, observed).
narrative_ontology:measurement(cons_su_t1974, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1974, 0.3).
narrative_ontology:measurement_basis(cons_su_t1974, observed).
narrative_ontology:measurement(cons_su_t2000, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 2000, 0.28).
narrative_ontology:measurement_basis(cons_su_t2000, observed).
narrative_ontology:measurement(cons_su_t2024, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 2024, 0.28).
narrative_ontology:measurement_basis(cons_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__coordinate_construction_reading, 0.18).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% The constitutional_authority_boundary kernel decomposes into three structurally distinct constraints, each a different reading of the same written text and institutional structure. The coordinate_construction_reading instantiates distributed authority (this file); judicial_supremacy_reading instantiates courts as final (sibling constraint); parliamentary_primacy_reading instantiates legislatures as final (sibling constraint). The three readings are ε-invariant and cannot be collapsed into a single constraint—measuring them together would obscure the structural differences. Each reading carries different beneficiary/victim structures, different extractiveness, and different suppression profiles. The readings coexist as live constitutional positions held by different parties and invoked in different historical periods. The network edges establish that the three constraints are part of one kernel family and compete for dominance in constitutional interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
