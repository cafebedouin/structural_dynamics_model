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
 *   This story instantiates the coordinate construction reading of the
 *   constitutional authority boundary kernel: the constitutional text is read
 *   as establishing three co-equal branches, each possessing interpretive
 *   authority within its own sphere, with no branch designated as final
 *   arbiter over the others. This is distinct from the judicial supremacy
 *   reading (which would make the courts the unchallengeable final word) and
 *   the parliamentary primacy reading (which would subordinate any judicial
 *   or executive constitutional reading to the legislature's ordinary
 *   lawmaking power). Under coordinate construction, the legislature can
 *   narrow doctrine through statute, the executive can decline enforcement of
 *   rulings it deems outside a court's sphere, and the court can decline to
 *   enforce statutes it deems unconstitutional — but none of the three can
 *   compel the others' compliance beyond the leverage each independently
 *   holds (appropriations, appointments, docket control, enforcement
 *   discretion). The moderate extractiveness (0.42) reflects the genuine
 *   coordination function this structure serves (preventing any single
 *   branch's interpretive claim from becoming unchallengeable) alongside real
 *   costs imposed on parties who need settled answers and instead get
 *   contingent, negotiated ones.
 *
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
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, 'b52d97a7-046c-41ca-8444-cb30e074d6df').
narrative_ontology:cs_kernel_codification('b52d97a7-046c-41ca-8444-cb30e074d6df', fixed_text).
narrative_ontology:cs_authority_grounding('b52d97a7-046c-41ca-8444-cb30e074d6df', distributed).
narrative_ontology:cs_reading_relation('b52d97a7-046c-41ca-8444-cb30e074d6df', constitutional_authority_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('b52d97a7-046c-41ca-8444-cb30e074d6df', constitutional_authority_boundary__parliamentary_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('b52d97a7-046c-41ca-8444-cb30e074d6df', foundational, no_branch_holds_final_interpretive_authority).
narrative_ontology:cs_axiom_status(no_branch_holds_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('b52d97a7-046c-41ca-8444-cb30e074d6df', no_branch_holds_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('b52d97a7-046c-41ca-8444-cb30e074d6df', foundational, each_branch_interprets_within_its_own_sphere).
narrative_ontology:cs_axiom_status(each_branch_interprets_within_its_own_sphere, holdable).
narrative_ontology:cs_axiom_grounding('b52d97a7-046c-41ca-8444-cb30e074d6df', each_branch_interprets_within_its_own_sphere, conventional).
narrative_ontology:cs_axiom('b52d97a7-046c-41ca-8444-cb30e074d6df', secondary, inter_branch_leverage_substitutes_for_hierarchical_review).
narrative_ontology:cs_axiom_status(inter_branch_leverage_substitutes_for_hierarchical_review, holdable).
narrative_ontology:cs_axiom_grounding('b52d97a7-046c-41ca-8444-cb30e074d6df', inter_branch_leverage_substitutes_for_hierarchical_review, instrumental).
narrative_ontology:cs_reference_frame('b52d97a7-046c-41ca-8444-cb30e074d6df', founding_era_departmentalist_settlement).
narrative_ontology:cs_drift_state('b52d97a7-046c-41ca-8444-cb30e074d6df', contemporary_administrative_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b52d97a7-046c-41ca-8444-cb30e074d6df', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, incumbent_branch_officials).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, legislative_majority_coalitions).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, executive_office_holders).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, litigants_seeking_final_resolution).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, minority_political_factions).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, citizens_relying_on_settled_law).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, supreme_or_high_court).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, national_legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, executive_office).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitution within cases and controversies before it, and treats its own reading as binding within the judicial sphere. It cannot compel the legislature to fund a remedy or compel the executive to enforce a ruling, but it controls the docket and the reasoning that other actors must engage with. When the other branches decline to follow a ruling, the court has no independent enforcement arm of its own.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, supreme_or_high_court, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, supreme_or_high_court, beneficiary).

% Passes statutes embodying its own reading of constitutional limits, can override or narrow judicial doctrine through ordinary legislation in many domains, and controls appropriations that fund enforcement of any branch's rulings. Its interpretive claim is asserted through the act of legislating rather than through explicit constitutional commentary, giving it plausible deniability when its reading conflicts with the court's.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, national_legislature, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, national_legislature, beneficiary).

% Interprets the constitution when deciding how to enforce laws, when to prosecute, and how to conduct foreign affairs and command the military; retains discretion to decline enforcement of judicial orders it deems non-binding on its constitutional sphere, and can act on its own reading absent an immediate coercive check. Its non-acquiescence is itself a form of constitutional interpretation exercised through inaction or selective enforcement.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, executive_office, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, executive_office, beneficiary).

% Bring disputes expecting a definitive constitutional answer but instead receive a ruling that may be contested, narrowed, or simply not enforced by a coordinate branch. They bear the cost of prolonged uncertainty, repeat litigation, and cases that remain formally 'won' but practically unresolved because no branch can compel the others to comply.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, litigants_seeking_final_resolution, payer,
    moderate, biographical, trapped, national).

% Rely on judicial protection of rights when they lack legislative or executive power, but find that protection contingent on the other branches' willingness to acquiesce. When a court ruling favors them but the legislature or executive resists, they have no branch left to appeal to — the distributed structure that protects them from majoritarian overreach in theory can strand them without a remedy in practice.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, minority_political_factions, payer,
    powerless, biographical, constrained, national).

% Order their affairs — contracts, marriages, business investments, immigration status — around what they believe the constitution settles. When branches diverge in their readings over time, or a later court/legislature/executive combination revisits a settled question, the ground can shift under decisions made in reliance on an earlier equilibrium among the branches.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, citizens_relying_on_settled_law, payer,
    powerless, biographical, trapped, national).

% Study inter-branch interpretive conflicts, departmentalism, and the historical record of when branches have deferred to or defied one another. They document the pattern without holding power to resolve it, and their scholarship is cited by all three branches selectively to support whichever reading suits the moment.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% Operate under the national constitutional settlement without a formal seat in the coordinate-branch interpretive dialogue; they can litigate and lobby but are not one of the three co-equal interpreting branches, even though the outcome of inter-branch disputes directly determines the scope of their own authority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, state_and_subnational_governments, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distributing interpretive authority across three branches prevents any single institution from monopolizing constitutional meaning, creating a system of mutual checking where each branch's reading of its own powers is tested against the others' willingness to comply, fund, or enforce.
% TRANSFER_FUNCTION: Moves the cost of interpretive uncertainty from the branches (none of which is individually accountable for producing a final, binding answer) onto litigants, minority factions, and citizens who need settled law to plan their affairs but instead receive contingent, branch-dependent resolutions.
% ABSENT_VOICES: State and subnational governments, and ordinary citizens whose rights or obligations hinge on the outcome, have no formal seat in the coordinate construction dialogue — they can petition, litigate, or vote, but the three branches conduct the actual interpretive contest among themselves.
% DISAPPEARANCE_RATIONALE: If distributed interpretive authority were replaced overnight by a single final arbiter (judicial or legislative), the entire pattern of inter-branch negotiation, strategic non-acquiescence, and departmentalist claim-staking would collapse into a single-track appeals process; legislatures and executives would lose their current capacity to act on their own constitutional readings pending a definitive external check, and the current equilibrium of mutual leverage would be replaced by a hierarchical one.
% FOUNDING_PROBLEM: The founding problem was preventing any one branch from accumulating unchecked power to define the limits of its own authority — a design response to the fear that a single final interpretive authority (whether a monarch, a legislature, or a court) could entrench itself against correction.
% FOUNDING_PROBLEM_CORROBORATION: Historical framers' debates and subsequent departmentalist scholarship (attesting the problem remains live: each branch continues to assert interpretive authority in disputes over war powers, impoundment, and judicial review scope) are cited by constitutional scholars outside all three branches. Sitting officials in each branch selectively invoke coordinate construction only when it favors their institutional position, which independent scholars flag as evidence the doctrine is sometimes deployed strategically rather than principledly — a corroboration coming from outside the benefiting branches themselves.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
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
 *   Extractiveness sits in the moderate band (0.30 rising to 0.42) because the coordination function is real — no branch can entrench an unchallengeable reading — but the structure also imposes genuine costs on litigants and citizens who bear the friction of unresolved inter-branch disagreement. Suppression is moderate (0.38) rather than low because each branch's assertion of interpretive authority within its sphere does constrain the others' ability to act on a contrary reading, even without formal subordination. Theater ratio is modest (0.28) — the doctrine does real work in structuring actual disputes (war powers, impoundment, judicial review scope) rather than being purely performative, though invocation of 'coordinate construction' by officials defending their own branch's prerogative carries some rhetorical cover function that grows slightly over the interval as the practice becomes more self-consciously cited.
 *
 * PERSPECTIVAL GAP:
 *   From each branch's own seat, the coordinate construction reading looks like principled coordination — a check against any one institution's overreach. From the seat of a litigant who has won a ruling that the executive declines to enforce, or a minority faction whose judicially-recognized right goes unfunded by the legislature, the same structure looks like extraction of certainty without recourse. The engine computing divergent per-seat classifications from these structural positions is the intended signal, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   The three branches themselves are the structural beneficiaries: each retains room to act on its own constitutional reading without external subordination, which is valuable to whichever branch currently favors its own position in a live dispute. Litigants, minority factions reliant on judicial protection, and citizens needing settled law are the structural payers — they bear the transaction costs of a system that, by design, produces contingent rather than final answers. State and subnational governments are excluded from the interpretive dialogue proper even though its outcome binds them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing entrenchment of unchecked interpretive power in any single branch — remains partially live: departmentalist disputes over war powers and impoundment continue to arise. But the founding_problem_status is contested because critics observe that branches now selectively invoke coordinate construction strategically (asserting it when it protects their own prerogative, ignoring it when a rival branch's independent reading would be inconvenient), which risks converting a genuine check-and-balance function into after-the-fact justification for whichever branch currently holds leverage in a dispute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinate_construction_vs_departmentalism_drift,
    'Is the coordinate construction reading a stable equilibrium reflecting genuine textual and historical design, or is it a contested doctrine (departmentalism) that different branches invoke opportunistically depending on which reading currently favors their institutional position?',
    'Historical pattern analysis: track whether branches invoke coordinate construction consistently across cases regardless of which reading benefits them, or only when the alternative (judicial supremacy or parliamentary primacy) would constrain their current action. Consistent invocation regardless of self-interest would support the stable-equilibrium reading; asymmetric invocation would support the opportunistic-cover reading.',
    'If opportunistic, the coordinate construction reading is closer to a tangled_rope with the branches jointly extracting flexibility from litigants and citizens who bear the cost of non-finality; if principled, it is closer to a genuine rope solving a real entrenchment-prevention problem with modest, evenly-distributed friction costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_construction_vs_departmentalism_drift, conceptual, 'Whether coordinate construction is a principled design or a strategically-invoked cover doctrine.').

omega_variable(
    kernel_reading_indeterminacy,
    'Does the constitutional text itself determine which of the three kernel readings (coordinate construction, judicial supremacy, parliamentary primacy) is correct, or is the text genuinely underdetermined such that the reading in force at any moment is a function of which branch currently has the practical leverage to make its reading stick?',
    'Comparative textual and historical analysis across constitutional systems with similar co-equal-branches language: if systems with materially identical text diverge in practice toward different readings depending on political configuration rather than textual features, this supports genuine indeterminacy over textual determinacy.',
    'If the text is genuinely indeterminate among the three readings, no reading can claim to be ''the'' correct interpretation of the founding design — each reading is better understood as a live political-institutional settlement contingent on the relative strength of the branches at a given moment, which would reframe founding_problem_status as inherently and permanently contested rather than resolvable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel text determines the reading or the reading is a function of contingent inter-branch power.').

omega_variable(
    remedy_gap_measurement,
    'How often, in practice, does a court ruling under coordinate construction go unenforced or under-enforced because the executive or legislature declines to acquiesce, and does this rate constitute evidence that the distributed-authority structure produces systematic remedy gaps for the parties it is meant to protect?',
    'Empirical tracking of compliance rates with judicial rulings across a sample of contested constitutional cases, cross-referenced with which branch held practical leverage (appropriations, enforcement discretion) in each instance.',
    'A high remedy-gap rate concentrated among minority factions and rights-claimants would shift the extraction analysis toward these groups bearing a disproportionate share of the coordination structure''s costs, potentially warranting reclassification toward a less benign profile than a symmetric tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_gap_measurement, empirical, 'Whether coordinate construction produces measurable, unevenly-distributed remedy gaps.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(cons_tr_t50, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(cons_be_t50, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 10, 0.32).
narrative_ontology:measurement(cons_su_t20, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 40, 0.37).
narrative_ontology:measurement(cons_su_t50, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the constitutional_authority_boundary kernel. constitutional_authority_boundary__judicial_supremacy_reading reads the same text as vesting final, unchallengeable interpretive authority in the courts (expected high accessibility_collapse once a ruling issues, concentrated judicial beneficiary). constitutional_authority_boundary__parliamentary_primacy_reading reads the text as subordinate to legislative sovereignty (expected concentrated beneficiary in legislative majorities, lower suppression on the executive/judicial seats but higher suppression on entrenched-rights claimants). Each sibling has its own ε, beneficiary/victim structure, and classification; they are linked here rather than merged because the ε-invariance principle requires one constraint per structurally distinct claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
