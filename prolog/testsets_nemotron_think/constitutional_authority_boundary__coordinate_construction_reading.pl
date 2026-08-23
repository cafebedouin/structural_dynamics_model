% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: constitutional_authority_boundary__coordinate_construction_reading
 *   human_readable: Coordinate Construction Constitutional Authority (Three Co-Equal Branches, Distributed Interpretation)
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   The coordinate construction reading of constitutional authority holds
 *   that the constitutional text establishes three co-equal branches, each
 *   possessing interpretive authority within its constitutional sphere, with
 *   no single branch designated as the final, unchallengeable arbiter of
 *   constitutional meaning. This reading instantiates a tangled rope: it
 *   provides genuine coordination (preventing tyranny through distributed
 *   interpretation and mutual checking) while generating asymmetric
 *   extraction (inter-branch conflict imposes compliance costs and gridlock
 *   on the citizenry, and the branches collectively benefit from
 *   institutional power that no single branch monopolizes). The constraint
 *   requires active enforcement — each branch must continually assert and
 *   defend its interpretive sphere against encroachment by the others. The
 *   claimed type (tangled_rope) and metrics are authored independently: the
 *   coordination function is real but so is the extraction from conflict and
 *   the citizenry's constrained exit.
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
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Coordinate Construction Constitutional Authority (Three Co-Equal Branches, Distributed Interpretation)").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, '6385353a-dd0a-41c7-aaf9-99bf71bbd242').
narrative_ontology:cs_kernel_codification('6385353a-dd0a-41c7-aaf9-99bf71bbd242', formalized).
narrative_ontology:cs_authority_grounding('6385353a-dd0a-41c7-aaf9-99bf71bbd242', lineage).
narrative_ontology:cs_interpretation_layer_present('6385353a-dd0a-41c7-aaf9-99bf71bbd242').
narrative_ontology:cs_reading_relation('6385353a-dd0a-41c7-aaf9-99bf71bbd242', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('6385353a-dd0a-41c7-aaf9-99bf71bbd242', constitutional_authority_boundary__parliamentary_primacy_reading, forecloses).
narrative_ontology:cs_axiom('6385353a-dd0a-41c7-aaf9-99bf71bbd242', foundational, interpretive_authority_distributed_among_coequal_branches).
narrative_ontology:cs_axiom_status(interpretive_authority_distributed_among_coequal_branches, holdable).
narrative_ontology:cs_axiom_grounding('6385353a-dd0a-41c7-aaf9-99bf71bbd242', interpretive_authority_distributed_among_coequal_branches, conventional).
narrative_ontology:cs_axiom('6385353a-dd0a-41c7-aaf9-99bf71bbd242', foundational, no_branch_holds_final_interpretive_monopoly).
narrative_ontology:cs_axiom_status(no_branch_holds_final_interpretive_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('6385353a-dd0a-41c7-aaf9-99bf71bbd242', no_branch_holds_final_interpretive_monopoly, conventional).
narrative_ontology:cs_reference_frame('6385353a-dd0a-41c7-aaf9-99bf71bbd242', founding_era_coordinate_construction).
narrative_ontology:cs_drift_state('6385353a-dd0a-41c7-aaf9-99bf71bbd242', contemporary_judicial_dominance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6385353a-dd0a-41c7-aaf9-99bf71bbd242', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, executive_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, citizenry).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, citizenry).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, state_governments).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, separation_of_powers).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, checks_and_balances).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, constitutional_supremacy).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, non_monopoly_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts legislation and interprets constitutional limits on its own lawmaking power; subject to judicial review and executive veto; cannot exit the constitutional framework without revolutionary change; bears costs of gridlock when other branches contest its interpretations
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch, payer).

% Executes laws and interprets constitutional authority in enforcement and foreign affairs; subject to legislative oversight and judicial review; bears costs of non-acquiescence conflicts and impeachment risk; cannot exit the constitutional framework
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, executive_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, executive_branch, payer).

% Adjudicates cases and interprets constitutional meaning in binding decisions; subject to appointment/confirmation politics, jurisdictional stripping, and non-acquiescence; bears legitimacy costs when perceived as overreaching; cannot exit the constitutional framework
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch, payer).

% Subject to all three branches' interpretive claims; benefits from rights protection and stable governance when system functions; bears costs of inter-branch conflict, gridlock, compliance burdens, and rights violations when interpretation fails; exit limited to emigration or constitutional amendment
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, citizenry, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, citizenry, beneficiary).

% Subject to federal constitutional interpretation by all three branches; can resist through interposition or amendment conventions but cannot unilaterally exit the federal constitutional framework; bear compliance costs and loss of policy autonomy
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, state_governments, payer,
    organized, generational, constrained, regional).

% Analyze and critique inter-branch interpretive dynamics; produce theories that influence all three branches; no direct stake in extraction but shape the intellectual environment in which the constraint operates
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the problem of concentrating interpretive authority in a single branch by distributing constitutional interpretation across three co-equal branches, each checking the others within its sphere through legislative override, executive non-acquiescence, and judicial review
% TRANSFER_FUNCTION: Moves interpretive authority from a hypothetical monopoly holder to a distributed system; moves compliance costs and gridlock risks to the citizenry and state governments; moves institutional power and interpretive prerogative to the three federal branches collectively
% ABSENT_VOICES: Future generations who will live under constitutional interpretations they had no voice in ratifying; historically marginalized groups excluded from the original constitutional bargain (enslaved persons, women, indigenous nations, propertyless men); territories and dependencies subject to constitutional authority without representation
% DISAPPEARANCE_RATIONALE: If distributed interpretive authority vanished overnight, constitutional interpretation would concentrate in one branch (most likely judicial given modern practice), fundamentally altering the separation of powers, the rights-protection calculus, and the mechanisms of constitutional change; the citizenry would lose the protection of inter-branch checking
% FOUNDING_PROBLEM: The founding problem was how to prevent tyranny by avoiding concentration of interpretive authority in any single branch while maintaining a functional government that could authoritatively interpret and apply the constitution across unforeseen circumstances
% FOUNDING_PROBLEM_CORROBORATION: The Federalist Papers (particularly Federalist 47-51), written by founders outside the benefiting branches as public persuasion, attest to the founding problem of tyranny prevention through separated powers; contemporary constitutional scholars across originalist, living constitutionalist, and popular constitutionalist traditions corroborate that the problem of interpretive monopoly remains live and contested
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_authority_boundary__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__coordinate_construction_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.42) reflects moderate but real extraction: the citizenry bears costs of inter-branch conflict, gridlock, and compliance with three competing interpretive claims, while the branches collectively capture institutional authority. Suppression (0.38) is moderate — the constitutional framework is enforced through institutional mechanisms (judicial review, veto, impeachment, appropriations) but alternatives exist (amendment, convention, elections) and exit, while constrained, is not impossible. Theater ratio (0.28) reflects some performative maintenance (ceremonial deference to coordinate construction while practice drifts toward judicial supremacy) but the coordination function remains substantially operative. Accessibility collapse (0.52) is moderate — alternative constitutional designs (parliamentary supremacy, judicial supremacy) are conceptually available but entrenched practice makes transition costly. Resistance (0.45) reflects ongoing scholarly, political, and inter-branch contestation over interpretive boundaries.
 *
 * PERSPECTIVAL GAP:
 *   From each branch's seat, the constraint appears as genuine coordination (it protects their sphere from the others); from the citizenry's seat, it appears as extraction (three competing interpreters impose triple compliance burden). The engine computes this divergence from the structural data: branches have agenda_setter+payer dual roles with institutional power and constrained exit; citizenry has payer+beneficiary with organized power and constrained exit. The claimed type (tangled_rope) captures the structural reality that both coordination and extraction are simultaneously present.
 *
 * DIRECTIONALITY LOGIC:
 *   The three branches are co-beneficiaries and co-payers: each gains interpretive authority within its sphere (beneficiary) but is constrained by the others' checking power (payer). The citizenry is a net payer — bearing compliance costs and gridlock risks — while also receiving rights-protection benefits (secondary beneficiary). State governments are payers subject to federal interpretive authority. The engine derives directionality from these structural positions: branches have institutional power but constrained exit (d ~ 0.4-0.5); citizenry has organized power through elections but constrained exit (d ~ 0.6); scholars are analytical observers (d ~ 0.0). No single branch is a monopoly beneficiary — the coordinate construction distributes both authority and constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordinate construction reading prevents mislabeling the separation of powers as pure coordination (rope) by acknowledging that inter-branch conflict generates real costs borne by non-consenting parties (citizenry, states). It prevents mislabeling as pure extraction (snare) by recognizing the genuine tyranny-prevention function that all three branches and the citizenry (when the system works) benefit from. The mandate (tyranny prevention through distributed interpretation) remains live — the founding problem has not been solved, only institutionalized — so mandatrophy is not resolved. The tension between live mandate and accumulating extraction (rising ε over time) is the central dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the coordinate construction reading a distinct constraint from the judicial supremacy and parliamentary primacy readings, or a different measurement of the same constraint?',
    'Apply the ε-invariance test: if measuring interpretive authority via judicial decisions vs. legislative enactments vs. executive actions yields different ε values for the same constitutional text, the label ''constitutional authority boundary'' covers multiple constraints. The coordinate construction reading''s ε (0.42) differs from the judicial supremacy reading''s ε (higher, due to monopoly beneficiary) and parliamentary primacy reading''s ε (different beneficiary structure).',
    'If distinct constraints, each gets its own classification and the kernel is a family linked by network.affects_constraints. If one constraint, the ε-invariance principle is violated and the classification is unstable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the coordinate construction reading is a structurally distinct constraint from its sibling readings per the ε-invariance principle').

omega_variable(
    sibling_reading_foreclosure,
    'Does the coordinate construction reading genuinely foreclose the judicial supremacy and parliamentary primacy readings within a single framework, or do they coexist as competing interpretations?',
    'Examine whether any historical constitutional regime has simultaneously maintained: (a) no single final arbiter, (b) courts as final unchallengeable arbiters, and (c) legislature as final authority. The logical structure of ''final arbiter'' is singular — two branches cannot both be final and unchallengeable.',
    'If foreclosure is genuine, the kernel''s readings are mutually exclusive regime types. If coexistence, the kernel permits pluralistic interpretive communities within one regime. This affects whether the engine treats them as competing constraints or complementary perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether coordinate construction logically forecloses judicial supremacy and parliamentary primacy in a single constitutional framework').

omega_variable(
    extraction_source_ambiguity,
    'Is the measured extractiveness (0.42) driven by structural features of coordinate construction (inevitable friction of three interpreters) or by drift toward judicial supremacy (one branch capturing interpretive monopoly)?',
    'Decompose the temporal series: if extractiveness rises correlate with judicial supremacy indicators (increased judicial review frequency, decreased legislative override, decreased executive non-acquiescence), the extraction is drift-driven. If extractiveness is stable and correlates with inter-branch conflict frequency regardless of which branch dominates, it is structural to coordinate construction.',
    'If structural, the coordinate construction reading is inherently a tangled rope. If drift-driven, the coordinate construction reading at founding was closer to a rope, and current extraction reflects contamination from judicial supremacy reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_source_ambiguity, empirical, 'Whether moderate extraction is inherent to distributed interpretation or reflects drift toward judicial monopoly').

omega_variable(
    citizenry_exit_constraint_nature,
    'Is the citizenry''s constrained exit from the constitutional framework structural (amendment difficulty, federalism) or internalized (legitimacy belief, identity fusion with constitutional order)?',
    'Post-amendment suppression trajectory: if citizen compliance persists after a successful amendment that reduces inter-branch conflict, exit constraint is partially internalized. Compare societies with easier amendment formulas — if their citizenry bears lower extraction, structural exit difficulty is confirmed.',
    'If internalized, effective suppression is higher than structural measure suggests; the citizenry carries the constraint''s suppression as legitimacy belief. If structural, exit options could be improved by institutional reform (easier amendment, stronger state autonomy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizenry_exit_constraint_nature, empirical, 'Whether citizenry''s constrained exit is structural or internalized, affecting effective suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cab_ccr_tr_t0, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(cab_ccr_tr_t0, observed).
narrative_ontology:measurement(cab_ccr_tr_t47, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 47, 0.15).
narrative_ontology:measurement_basis(cab_ccr_tr_t47, observed).
narrative_ontology:measurement(cab_ccr_tr_t94, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 94, 0.2).
narrative_ontology:measurement_basis(cab_ccr_tr_t94, observed).
narrative_ontology:measurement(cab_ccr_tr_t141, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 141, 0.23).
narrative_ontology:measurement_basis(cab_ccr_tr_t141, observed).
narrative_ontology:measurement(cab_ccr_tr_t188, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 188, 0.26).
narrative_ontology:measurement_basis(cab_ccr_tr_t188, observed).
narrative_ontology:measurement(cab_ccr_tr_t235, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 235, 0.28).
narrative_ontology:measurement_basis(cab_ccr_tr_t235, observed).

% Extraction over time
narrative_ontology:measurement(cab_ccr_be_t0, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(cab_ccr_be_t0, observed).
narrative_ontology:measurement(cab_ccr_be_t47, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 47, 0.3).
narrative_ontology:measurement_basis(cab_ccr_be_t47, observed).
narrative_ontology:measurement(cab_ccr_be_t94, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 94, 0.35).
narrative_ontology:measurement_basis(cab_ccr_be_t94, observed).
narrative_ontology:measurement(cab_ccr_be_t141, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 141, 0.38).
narrative_ontology:measurement_basis(cab_ccr_be_t141, observed).
narrative_ontology:measurement(cab_ccr_be_t188, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 188, 0.4).
narrative_ontology:measurement_basis(cab_ccr_be_t188, observed).
narrative_ontology:measurement(cab_ccr_be_t235, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 235, 0.42).
narrative_ontology:measurement_basis(cab_ccr_be_t235, observed).

% Suppression requirement over time
narrative_ontology:measurement(cab_ccr_su_t0, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(cab_ccr_su_t0, observed).
narrative_ontology:measurement(cab_ccr_su_t47, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 47, 0.25).
narrative_ontology:measurement_basis(cab_ccr_su_t47, observed).
narrative_ontology:measurement(cab_ccr_su_t94, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 94, 0.3).
narrative_ontology:measurement_basis(cab_ccr_su_t94, observed).
narrative_ontology:measurement(cab_ccr_su_t141, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 141, 0.33).
narrative_ontology:measurement_basis(cab_ccr_su_t141, observed).
narrative_ontology:measurement(cab_ccr_su_t188, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 188, 0.36).
narrative_ontology:measurement_basis(cab_ccr_su_t188, observed).
narrative_ontology:measurement(cab_ccr_su_t235, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 235, 0.38).
narrative_ontology:measurement_basis(cab_ccr_su_t235, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__coordinate_construction_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is the coordinate_construction_reading of the constitutional_authority_boundary kernel. The kernel decomposes into three structurally distinct readings with different ε values, beneficiary structures, and constraint types. The coordinate construction reading (this story) has distributed authority, no monopoly beneficiary, and moderate ε (0.42). The judicial_supremacy_reading has judicial monopoly beneficiary and higher ε. The parliamentary_primacy_reading has legislative monopoly beneficiary and different ε. All three are linked as a constraint family via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__coordinate_construction_reading, institutional, 0.45).
constraint_indexing:directionality_override(constitutional_authority_boundary__coordinate_construction_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
