% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__popular_constitutionalism_reading, []).

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
 *   constraint_id: us_constitution_interpretive__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism: Constitutional Meaning Shaped by Democratic Contestation
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the popular constitutionalism reading
 *   of the US Constitution interpretive kernel. The reading holds that
 *   constitutional meaning is legitimately shaped by popular political
 *   movements, electoral majorities, and democratic contestation — not solely
 *   by judicial interpretation. It contests judicial supremacy (the claim
 *   that courts have final interpretive authority) and distributes
 *   interpretive authority across Congress, the President, social movements,
 *   and the electorate. The constraint has a genuine coordination function:
 *   it solves the counter-majoritarian difficulty by grounding constitutional
 *   legitimacy in ongoing popular sovereignty rather than in a fixed founding
 *   moment or in judicial reason. But it also extracts asymmetrically:
 *   minorities who depend on counter-majoritarian judicial protection (racial
 *   minorities, religious minorities, political dissenters, criminal
 *   defendants) bear the cost when majoritarian movements redefine
 *   constitutional protections. The enforcement machinery includes elections,
 *   legislative overrides, court-curbing measures, appointment politics, and
 *   the threat of constitutional amendment — all actively maintained. The
 *   claim/metric independence is observed: the reading claims to be
 *   democratic coordination (rope-like), but the authored metrics show
 *   substantial extractiveness and suppression toward minority seats.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, 0.58).
domain_priors:suppression_score(us_constitution_interpretive__popular_constitutionalism_reading, 0.52).
domain_priors:theater_ratio(us_constitution_interpretive__popular_constitutionalism_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(us_constitution_interpretive__popular_constitutionalism_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__popular_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__popular_constitutionalism_reading, "Popular Constitutionalism: Constitutional Meaning Shaped by Democratic Contestation").
narrative_ontology:topic_domain(us_constitution_interpretive__popular_constitutionalism_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__popular_constitutionalism_reading, 'f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64').
narrative_ontology:cs_kernel_codification('f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64', fixed_text).
narrative_ontology:cs_authority_grounding('f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64', practice).
narrative_ontology:cs_interpretation_layer_present('f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64').
narrative_ontology:cs_reading_relation('f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64', us_constitution_interpretive__living_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64', foundational, popular_sovereignty_continuous).
narrative_ontology:cs_axiom_status(popular_sovereignty_continuous, holdable).
narrative_ontology:cs_axiom_grounding('f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64', popular_sovereignty_continuous, deontological).
narrative_ontology:cs_axiom('f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64', foundational, judicial_supremacy_illegitimate).
narrative_ontology:cs_axiom_status(judicial_supremacy_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64', judicial_supremacy_illegitimate, deontological).
narrative_ontology:cs_reference_frame('f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64', continuous_popular_sovereignty).
narrative_ontology:cs_drift_state('f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64', contemporary_judicial_supremacy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f76a6fdb-e68a-48e0-a9f1-6f2c325d0c64', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__popular_constitutionalism_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, stable_settlement_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, minorities_dependent_on_counter_majoritarian_protection).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_interpretive__popular_constitutionalism_reading, courts_judiciary).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, popular_sovereignty_continuous).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, judicial_supremacy_illegitimate).
narrative_ontology:constraint_vindicates(us_constitution_interpretive__popular_constitutionalism_reading, democratic_legitimation_of_constitutional_meaning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Social movements (civil rights, labor, conservative legal, reproductive rights, etc.) mobilize to shape constitutional meaning through elections, litigation, protests, and cultural contestation. They gain interpretive influence when their constitutional visions prevail. Exit means abandoning the constitutional project or emigrating — constrained by identity, community, and material ties.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, popular_movements, beneficiary,
    organized, biographical, constrained, national).

% Congress and state legislatures enact statutes that construct constitutional meaning (e.g., Civil Rights Act, Voting Rights Act, RFRA, ACA). They benefit from interpretive latitude when courts defer. Exit is mobile — they can lose the next election — but institutional position gives them agenda-setting power within the constraint.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, legislative_majorities, beneficiary,
    institutional, biographical, mobile, national).

% Populist and anti-establishment actors who claim the Constitution belongs to 'the people' not 'elite judges.' They benefit symbolically (rhetorical ownership) and instrumentally (policy wins when courts are constrained). Exit is constrained — their identity is fused to the anti-elite claim.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, anti_elitist_claimants, beneficiary,
    organized, biographical, constrained, national).

% Judges, legal scholars, and institutionalists who defend judicial supremacy as essential to rule of law and minority protection. They bear the cost of interpretive authority displacement. Exit is identity-locked: their professional identity and institutional legitimacy are constituted by judicial finality; abandoning it dissolves their role.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, judicial_finality_advocates, payer,
    institutional, generational, identity_locked, national).

% Actors who require predictable constitutional rules for commerce, governance, and planning (businesses, state governments, international partners). They bear the cost of constitutional fluidity. Exit is constrained — they operate within the system but prefer stable rules.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, stable_settlement_advocates, payer,
    moderate, generational, constrained, national).

% Racial, religious, political, and other minorities whose constitutional protections (equal protection, free exercise, due process, voting rights) depend on courts willing to override majorities. They bear the extraction when popular movements redefine rights. Exit is trapped — minority status is not chosen and emigration is rarely feasible.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, minorities_dependent_on_counter_majoritarian_protection, payer,
    powerless, generational, trapped, national).

% Federal and state courts that interpret the Constitution. They set the agenda through judicial review but are constrained by popular constitutionalist pressure (appointments, jurisdiction stripping, non-compliance, legitimacy attacks). They are dual-positioned: they administer the constraint (issue rulings) but also pay the cost (authority erosion). Professional identity is fused to judicial role.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, courts_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_interpretive__popular_constitutionalism_reading, courts_judiciary, payer).

% Academic observers who analyze the contest from outside. They neither collect nor pay directly but their work shapes the interpretive discourse. Exit is analytical — they can change frameworks without material cost.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Democratic legitimation of constitutional meaning: solves the counter-majoritarian difficulty by grounding constitutional authority in ongoing popular sovereignty rather than in a fixed founding moment or in judicial reason alone. Coordinates interpretive authority across branches and movements so that constitutional change tracks democratic will.
% TRANSFER_FUNCTION: Moves final interpretive authority from courts to political branches and popular movements. Moves constitutional protections from counter-majoritarian guarantees (judicially enforced) to majoritarian politics (legislatively contingent). Transfers the cost of constitutional instability from majorities (who gain flexibility) to minorities (who lose shields).
% ABSENT_VOICES: Future generations (cannot participate in current contestation but inherit its constitutional settlements). Non-citizens affected by US constitutional law (detainees, migrants, foreign nationals). Dispersed minority interests that cannot organize effectively into movements (e.g., criminal defendants, the poor, the cognitively disabled). The excluded stakeholders are structurally absent from the democratic contestation that this reading celebrates.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism vanished overnight, judicial supremacy would be restored as the default interpretive regime. Minority protections would shift back toward counter-majoritarian judicial enforcement. Legislative majorities would lose interpretive latitude. The constitutional order would reorganize around courts as final expositors — a different distribution of authority, legitimacy, and protection.
% FOUNDING_PROBLEM: The counter-majoritarian difficulty: how to legitimate judicial review — the power of unelected judges to invalidate democratic enactments — in a system committed to popular sovereignty. The Founders created a Constitution with counter-majoritarian features (judicial review, Senate, Electoral College) but no settled theory of why unelected judges should have final say on constitutional meaning.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the beneficiary set corroborate: Bruce Ackerman (We the People) identifies founding moments of popular sovereignty; Larry Kramer (The People Themselves) documents early popular constitutionalism; Mark Tushnet (Taking the Constitution Away from the Courts) argues judicial supremacy is a 20th-century invention. Judicial supremacy advocates (e.g., Robert Bork, Antonin Scalia, contemporary originalists) contest the founding problem's framing, arguing the Constitution explicitly assigns judicial power to courts. The corroboration is split — the problem's status is genuinely contested across traditions.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_interpretive__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__popular_constitutionalism_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__popular_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects the transfer of interpretive authority from courts to majoritarian institutions, which extracts protection from minority groups. Suppression (0.52) reflects the active constraint on judicial finality and the difficulty minorities face in exiting majoritarian constitutional redefinitions. Theater ratio (0.32) is moderate: the democratic legitimation function is real but performative elements exist (movements claiming to speak for 'the people' while excluding dissenters). Accessibility collapse (0.38) is low because originalist and living constitution readings remain live alternatives. Resistance (0.65) is high: judicial supremacy advocates, originalists, and living constitutionalists actively contest this reading. The measurement series spans founding to present on a shared grid (1789, 1865, 1937, 1954, 1973, 2000, 2024) capturing Reconstruction, New Deal, Warren Court, and modern polarization eras.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (movements, majorities) experience this as genuine coordination — democratic self-governance solving the legitimacy problem. The minority victim seats experience it as extraction — their constitutional protections become contingent on majoritarian grace. The judicial seat experiences it as institutional displacement — its authority is contested and constrained. The engine computes per-seat classifications from these structural asymmetries; the authored claim (tangled_rope) acknowledges the dual nature but does not adjudicate the seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular movements and legislative majorities are structural beneficiaries (d near 0.0-0.2): they gain interpretive authority and policy latitude. Anti-elitist claimants benefit symbolically and instrumentally (d ~ 0.2). Courts and judicial finality advocates are targets (d ~ 0.7-0.8): their institutional authority is the extraction object. Minorities dependent on counter-majoritarian protection are full targets (d ~ 0.9): they lose the institutional shield that judicial supremacy provides. Stable settlement advocates are moderate targets (d ~ 0.6): they lose predictability but retain legislative avenues. The derivation chain from beneficiary/victim declarations + exit options produces these directionalities; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the counter-majoritarian difficulty — remains contested. Originalists argue it was never legitimately solved (judicial review is illegitimate). Living constitutionalists argue it is solved through reasoned adaptation. Popular constitutionalists argue it is solved through democratic contestation. The constraint persists not because the founding problem is dead, but because each reading offers a different solution and the contest among them is the constraint's operating mode. No mandate has atrophied; the contest is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Does popular constitutionalism genuinely coordinate democratic legitimacy, or does its majoritarian mechanism structurally extract from minorities who depend on counter-majoritarian protections?',
    'Comparative analysis of constitutional regimes with strong vs. weak judicial review: track minority rights outcomes under sustained popular constitutionalist pressure versus judicial supremacy regimes.',
    'If extraction dominates, the constraint reclassifies toward snare for minority seats; if coordination dominates, it remains tangled_rope with asymmetric costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the democratic coordination function outweighs the majoritarian extraction from minority protections.').

omega_variable(
    judicial_constraint_feasibility,
    'Can judicial review be substantively constrained by popular and legislative action without collapsing the institutional capacity to protect minority rights?',
    'Historical case studies of court-curbing episodes (e.g., 1930s Court-packing, 1860s Reconstruction, modern jurisdiction-stripping proposals) measuring subsequent minority rights trajectory.',
    'If constraint collapses protection capacity, the reading''s coordination claim is undermined; if capacity persists, the tangled_rope coordination function is vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_constraint_feasibility, conceptual, 'Whether the reading''s institutional design can constrain courts without disabling counter-majoritarian protection.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of judicial finality and minority protections structural (institutional design of majoritarian politics) or internalized (minorities accepting majoritarian outcomes as democratically legitimate)?',
    'Post-exit suppression trajectory: examine whether minority groups continue to experience constitutional exclusion after formal political participation channels are opened.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent beyond formal institutional channels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for minority seats under popular constitutionalism.').

omega_variable(
    kernel_reading_identity,
    'Is this reading of the US Constitution interpretive kernel structurally distinct from its siblings, or does it share an ε-invariant core that differs only in emphasis?',
    'Cross-reading ε comparison: author sibling constraint stories (originalist_reading, living_constitution_reading) with independent metrics and stakeholder structures; compare ε values and beneficiary/victim sets.',
    'If ε values converge across readings, the kernel may be a single constraint with observer-dependent classification; if they diverge, the decomposition is validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel decomposition into three readings satisfies ε-invariance or whether the readings share a single structural constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__popular_constitutionalism_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(popcon_tr_t1789, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1789, 0.15).
narrative_ontology:measurement(popcon_tr_t1865, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1865, 0.2).
narrative_ontology:measurement(popcon_tr_t1937, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1937, 0.25).
narrative_ontology:measurement(popcon_tr_t1954, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1954, 0.22).
narrative_ontology:measurement(popcon_tr_t1973, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 1973, 0.28).
narrative_ontology:measurement(popcon_tr_t2000, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(popcon_tr_t2024, us_constitution_interpretive__popular_constitutionalism_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(popcon_be_t1789, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1789, 0.25).
narrative_ontology:measurement(popcon_be_t1865, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1865, 0.35).
narrative_ontology:measurement(popcon_be_t1937, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1937, 0.45).
narrative_ontology:measurement(popcon_be_t1954, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1954, 0.38).
narrative_ontology:measurement(popcon_be_t1973, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 1973, 0.48).
narrative_ontology:measurement(popcon_be_t2000, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(popcon_be_t2024, us_constitution_interpretive__popular_constitutionalism_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(popcon_su_t1789, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1789, 0.2).
narrative_ontology:measurement(popcon_su_t1865, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1865, 0.4).
narrative_ontology:measurement(popcon_su_t1937, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1937, 0.45).
narrative_ontology:measurement(popcon_su_t1954, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1954, 0.38).
narrative_ontology:measurement(popcon_su_t1973, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 1973, 0.42).
narrative_ontology:measurement(popcon_su_t2000, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(popcon_su_t2024, us_constitution_interpretive__popular_constitutionalism_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_interpretive__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_interpretive__popular_constitutionalism_reading, 0.08).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, judicial_review_institutional_authority).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, legislative_supremacy_doctrine).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, minority_protection_counter_majoritarian_doctrines).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, constitutional_amendment_process).
narrative_ontology:affects_constraint(us_constitution_interpretive__popular_constitutionalism_reading, court_curbing_mechanisms).

% DUAL FORMULATION NOTE:
% This story is one of three in the us_constitution_interpretive constraint family. The kernel decomposes because the label 'constitutional interpretation' conflates three structurally distinct claims with different ε values, beneficiary sets, and enforcement mechanisms. Originalist_reading: ε ~ 0.2 (low extraction, high coordination for originalists), beneficiaries: originalist judges/scholars, victims: living constitutionalists. Living_constitution_reading: ε ~ 0.4 (moderate extraction, coordination through adaptation), beneficiaries: progressive judges/scholars, victims: originalists. Popular_constitutionalism_reading (this story): ε ~ 0.58, beneficiaries: popular movements/legislative majorities, victims: judicial finality/minorities. The upstream constraints (originalist, living constitution) are often cited as evidence for/against this reading, creating network influence edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_interpretive__popular_constitutionalism_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
