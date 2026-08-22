% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
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
 *   human_readable: Coordinate Construction of Constitutional Authority (Distributed Interpretation)
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the coordinate construction reading of
 *   the constitutional authority boundary kernel. The reading holds that the
 *   constitutional text establishes three co-equal branches (legislative,
 *   executive, judicial) each possessing interpretive authority within its
 *   constitutional sphere, with no single branch designated as final arbiter
 *   of constitutional meaning. Inter-branch disputes are resolved through
 *   political negotiation, structural counter-mechanisms (legislative
 *   override, executive non-acquiescence, jurisdictional limits), and the
 *   pressure of institutional legitimacy — not by judicial supremacy. The
 *   arrangement coordinates governance across separated powers while
 *   extracting interpretive autonomy from each branch; it is neither pure
 *   coordination nor pure extraction but a sustained tension that generates
 *   moderate extraction (ε=0.42) from the friction of distributed authority.
 *
 * KEY AGENTS:
 *   - legislature: agenda_setter (primary lawmaking) / payer (constrained by other branches' interpretations)
 *   - executive: agenda_setter (enforcement discretion) / payer (constrained by judicial/legislative interpretation)
 *   - judiciary: agenda_setter (adjudicative interpretation) / payer (constrained by legislative override/executive non-acquiescence)
 *   - citizens: beneficiary (rights protection through pluralism) / payer (governance costs of inter-branch conflict)
 *   - legal_profession: beneficiary (interpretive monopoly rents) / observer (professional custodian)
 *   - states_subnational: excluded (interpretive voice marginalized in federal coordinate construction)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__coordinate_construction_reading, 0.42).
domain_priors:suppression_score(constitutional_authority_boundary__coordinate_construction_reading, 0.28).
domain_priors:theater_ratio(constitutional_authority_boundary__coordinate_construction_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__coordinate_construction_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Coordinate Construction of Constitutional Authority (Distributed Interpretation)").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "constitutional_law/political_philosophy/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, '6376de2c-170d-495a-9291-4e0fbf82b1ac').
narrative_ontology:cs_kernel_codification('6376de2c-170d-495a-9291-4e0fbf82b1ac', fixed_text).
narrative_ontology:cs_authority_grounding('6376de2c-170d-495a-9291-4e0fbf82b1ac', lineage).
narrative_ontology:cs_interpretation_layer_present('6376de2c-170d-495a-9291-4e0fbf82b1ac').
narrative_ontology:cs_reading_relation('6376de2c-170d-495a-9291-4e0fbf82b1ac', constitutional_authority_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('6376de2c-170d-495a-9291-4e0fbf82b1ac', constitutional_authority_boundary__parliamentary_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('6376de2c-170d-495a-9291-4e0fbf82b1ac', foundational, no_branch_monopolizes_constitutional_meaning).
narrative_ontology:cs_axiom_status(no_branch_monopolizes_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('6376de2c-170d-495a-9291-4e0fbf82b1ac', no_branch_monopolizes_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('6376de2c-170d-495a-9291-4e0fbf82b1ac', foundational, inter_branch_negotiation_legitimates_outcomes).
narrative_ontology:cs_axiom_status(inter_branch_negotiation_legitimates_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('6376de2c-170d-495a-9291-4e0fbf82b1ac', inter_branch_negotiation_legitimates_outcomes, conventional).
narrative_ontology:cs_axiom('6376de2c-170d-495a-9291-4e0fbf82b1ac', secondary, judicial_review_is_departmental_not_supreme).
narrative_ontology:cs_axiom_status(judicial_review_is_departmental_not_supreme, holdable).
narrative_ontology:cs_axiom_grounding('6376de2c-170d-495a-9291-4e0fbf82b1ac', judicial_review_is_departmental_not_supreme, conventional).
narrative_ontology:cs_reference_frame('6376de2c-170d-495a-9291-4e0fbf82b1ac', founding_era_coordinate_construction).
narrative_ontology:cs_drift_state('6376de2c-170d-495a-9291-4e0fbf82b1ac', contemporary_judicial_supremacy_dominance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6376de2c-170d-495a-9291-4e0fbf82b1ac', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, institutional_constitutionalism).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, majoritarian_legislative_agendas).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, executive_unilateral_action).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, citizens).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, legal_profession).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, executive).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__coordinate_construction_reading, citizens).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, constitutional_interpretation_is_distributed).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, no_single_branch_monopolizes_meaning).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, inter_branch_negotiation_legitimates_outcomes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts laws within its constitutional sphere; interprets the constitution when legislating (e.g., determining scope of commerce power, necessary and proper clause). Constrained by judicial invalidation and executive veto/non-enforcement. Exit via constitutional amendment (Article V) is theoretically possible but practically arduous — requires supermajorities across states. Collects democratic legitimacy rents but pays interpretive autonomy costs.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, legislature, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, legislature, payer).

% Enforces laws and interprets the constitution in execution (signing statements, OLC opinions, non-acquiescence to judicial rulings outside immediate parties). Constrained by judicial review, legislative oversight, appropriations power. Exit is constrained — term limits, impeachment, and electoral accountability bound unilateral action. Collects unitary executive theory rents; pays coordination friction costs.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, executive, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, executive, payer).

% Adjudicates cases and interprets the constitution authoritatively for the parties before it. Claims no general legislative or executive power. Constrained by jurisdiction limits, legislative override (jurisdiction stripping, court-packing threats), executive non-acquiescence, and dependence on other branches for enforcement. Life tenure provides insulation but not exit from the coordinate structure. Collects interpretive monopoly rents; pays legitimacy maintenance costs.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, judiciary, payer).

% Subject to all three branches' interpretive claims. Benefits from pluralistic rights protection (no single branch can unilaterally define rights away). Pays costs of governance paralysis, litigation expense, policy uncertainty, and democratic accountability dilution. Exit is constrained — emigration is costly; constitutional amendment is the only structural exit, requiring supermajorities. Organized through elections, advocacy, litigation, but never a unified interpretive voice.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, citizens, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, citizens, payer).

% Professional custodians of constitutional interpretation across all three branches. Extracts rents from interpretive complexity: litigation, counseling, academic production, judicial clerkships. Benefits from coordinate construction's sustained demand for authoritative interpretation. Mobile exit — can shift between private practice, government, academia, judiciary. Not a governor of the constraint but a structural beneficiary of its complexity.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Possess independent constitutional traditions and interpretive authority under their own constitutions. In the federal coordinate construction, their interpretive voice is marginalized — federal branches dominate constitutional meaning for the whole system. Constrained by supremacy clause, incorporation doctrine, federal judicial review. Exit is constrained: secession is foreclosed; state constitutional innovation operates in federal shadow. Would object to federal coordinate construction's monoculture if structurally included.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, states_subnational, excluded,
    powerful, generational, constrained, regional).

% Analyze, critique, and theorize the coordinate construction from outside the institutional structure. Provide the intellectual vocabulary for all three branches' self-justifications and for the sibling readings' critiques. No direct stake in governance outcomes; stakes are professional reputation and theoretical coherence. Analytical exit — can change frameworks without material cost.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of concentrated interpretive power by distributing constitutional authority across three branches, each checking the others' interpretive claims. Prevents tyranny of any single branch while maintaining a functional governance system that can act without permanent deadlock.
% TRANSFER_FUNCTION: Moves interpretive autonomy from each branch to the others (mutual constraint) and moves governance friction costs (delay, uncertainty, litigation) to citizens. Moves professional rents to the legal class. Moves democratic legitimacy rents to branches that can claim to represent the people's constitutional understanding.
% ABSENT_VOICES: The people as a direct constitutional constituency (popular constitutionalism) — they would demand more direct interpretive access (amendment, referendum, jury nullification) but are structurally excluded by the three-branch distribution. State and subnational constitutional traditions — they would claim co-equal interpretive authority but are marginalized by federal supremacy. Future generations — they bear the long-run institutional drift but have no voice in current interpretive contests.
% DISAPPEARANCE_RATIONALE: If coordinate construction vanished overnight, one branch would rapidly claim interpretive supremacy (likely the judiciary given current institutional momentum), legislative and executive interpretive practices would be subordinated, citizens would lose the pluralistic rights protection of distributed authority, and the legal profession would lose the multi-forum complexity that sustains its rents. The constitutional order would restructure around a new monopoly interpreter.
% FOUNDING_PROBLEM: How to create a government strong enough to govern but constrained enough not to become tyrannical, when any single interpretive authority could become the vehicle for tyranny. The coordinate construction answer: make interpretation a shared, contested practice among co-equal branches so that no branch's reading becomes law without the others' acquiescence or the pressure of sustained inter-branch negotiation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing tyranny through separated powers) is attested as live by: Federalist Papers (Madison/Hamilton/Jay) as original design intent; Anti-Federalist critiques (Brutus, Centinel) warning that coordinate construction would fail and judicial supremacy would emerge — a warning from outside the benefiting parties that the arrangement has partially vindicated; modern political scientists (e.g., Whittington, Fisher, Kramer) documenting the ongoing contest between coordinate construction and its siblings. No single party's self-assertion suffices — the corroboration comes from the persistence of the contest itself across 230+ years.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness (0.42) reflects the real but bounded costs of interpretive pluralism: litigation, delay, uncertainty, and the professional rents of the legal class. Suppression (0.28) is moderate — the constraint does not violently exclude alternatives but structurally privileges institutional actors over popular constitutionalism. Theater ratio (0.31) captures the performative dimension: branches often invoke coordinate construction rhetorically while pursuing de facto supremacy. Accessibility collapse (0.35) is partial — alternatives (judicial supremacy, parliamentary primacy, popular constitutionalism) remain conceptually available and politically live. Resistance (0.45) is significant: each sibling reading actively contests this one, and popular movements periodically challenge the entire institutional framework.
 *
 * PERSPECTIVAL GAP:
 *   From the legislature's seat, the constraint feels like extraction (judicial veto of democratic majorities). From the judiciary's seat, it feels like coordination (protecting minority rights from majoritarian excess). From the executive's seat, it oscillates: coordination when unified government, extraction when divided. From citizens' seat, it is opaque — they experience outcomes, not the interpretive structure. The engine computes these divergences from the stakeholder power/exit profiles; the claim does not resolve them.
 *
 * DIRECTIONALITY LOGIC:
 *   No single monopoly beneficiary exists — that is the defining feature of this reading. Each branch is both agenda_setter (in its sphere) and payer (constrained by the others). Citizens are structurally ambivalent: beneficiaries of rights pluralism, payers of governance friction. The legal profession extracts professional rents from interpretive complexity but does not govern the constraint. States/subnational units are excluded from the federal coordinate construction. Directionality derives from this structural symmetry: all three branches have moderate power, constrained exit (constitutional amendment is arduous), and national scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing tyranny through separated powers with no single interpreter — remains live (founding_problem_status: live). But the specific coordinate construction mechanism has drifted: judicial supremacy has captured significant territory, legislative override has atrophied, executive non-acquiescence is contested. The arrangement persists not because it solves the founding problem cleanly, but because no coalition exists to replace it — a classic mandatrophy signature where the structure outlives its clean functional justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Is this constraint a genuine structural reading of the constitutional authority boundary, or a self-serving construction by branches seeking to avoid accountability?',
    'Historical analysis of founding-era institutional design debates (Federalist/Anti-Federalist, constitutional convention records) to determine whether distributed interpretation was an intended structural feature or an emergent practice.',
    'If intended: the constraint is a Mountain of institutional design (low ε, genuine coordination). If emergent/self-serving: it is a Tangled Rope where branches coordinate to evade democratic accountability while extracting interpretive autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Whether coordinate construction is a designed feature or an evolved evasion').

omega_variable(
    inter_branch_conflict_extraction,
    'Do inter-branch conflicts over constitutional meaning generate net extraction from citizens (delays, uncertainty, litigation costs) that exceed the coordination value of distributed interpretation?',
    'Empirical measurement of governance paralysis events, litigation expenditure, and policy delay attributable to interpretive disputes vs. counterfactual of single-arbiter systems.',
    'If net extraction exceeds coordination value, the constraint reclassifies toward Snare (citizens pay for branches'' interpretive competition). If coordination value dominates, remains Rope/Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inter_branch_conflict_extraction, empirical, 'Whether interpretive pluralism imposes extractive costs on the governed').

omega_variable(
    excluded_popular_voice,
    'Does the coordinate construction reading structurally exclude popular constitutionalism — the people as an interpretive constituency — while presenting itself as democratic?',
    'Analyze whether the three-branch distribution functionally crowds out popular amendment, jury nullification, state constitutional innovation, and extra-institutional constitutional argument.',
    'If excluded: the constraint is a Snare on popular sovereignty masked as institutional coordination. If inclusive: the reading accommodates popular voice within the distributed structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_popular_voice, conceptual, 'Whether coordinate construction forecloses popular constitutionalism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 1789, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1789, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1789, 0.12).
narrative_ontology:measurement(cons_tr_t1803, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1803, 0.18).
narrative_ontology:measurement(cons_tr_t1865, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1865, 0.25).
narrative_ontology:measurement(cons_tr_t1937, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1937, 0.29).
narrative_ontology:measurement(cons_tr_t1973, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 1973, 0.27).
narrative_ontology:measurement(cons_tr_t2000, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 2000, 0.31).

% Extraction over time
narrative_ontology:measurement(cons_be_t1789, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1789, 0.22).
narrative_ontology:measurement(cons_be_t1803, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1803, 0.31).
narrative_ontology:measurement(cons_be_t1865, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1865, 0.38).
narrative_ontology:measurement(cons_be_t1937, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1937, 0.44).
narrative_ontology:measurement(cons_be_t1973, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 1973, 0.41).
narrative_ontology:measurement(cons_be_t2000, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 2000, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1789, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1789, 0.15).
narrative_ontology:measurement(cons_su_t1803, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1803, 0.22).
narrative_ontology:measurement(cons_su_t1865, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1865, 0.35).
narrative_ontology:measurement(cons_su_t1937, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1937, 0.28).
narrative_ontology:measurement(cons_su_t1973, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 1973, 0.26).
narrative_ontology:measurement(cons_su_t2000, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 2000, 0.28).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1789, tn=2000
narrative_ontology:measurement(cons_grid_01, constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse(class), 1789, 0.3).
narrative_ontology:measurement(cons_grid_02, constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse(class), 2000, 0.32).
narrative_ontology:measurement(cons_grid_03, constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse(individual), 1789, 0.4).
narrative_ontology:measurement(cons_grid_04, constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse(individual), 2000, 0.3).
narrative_ontology:measurement(cons_grid_05, constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse(organizational), 1789, 0.2).
narrative_ontology:measurement(cons_grid_06, constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse(organizational), 2000, 0.38).
narrative_ontology:measurement(cons_grid_07, constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse(structural), 1789, 0.25).
narrative_ontology:measurement(cons_grid_08, constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse(structural), 2000, 0.45).
narrative_ontology:measurement(cons_grid_09, constitutional_authority_boundary__coordinate_construction_reading, resistance(class), 1789, 0.4).
narrative_ontology:measurement(cons_grid_10, constitutional_authority_boundary__coordinate_construction_reading, resistance(class), 2000, 0.52).
narrative_ontology:measurement(cons_grid_11, constitutional_authority_boundary__coordinate_construction_reading, resistance(individual), 1789, 0.25).
narrative_ontology:measurement(cons_grid_12, constitutional_authority_boundary__coordinate_construction_reading, resistance(individual), 2000, 0.4).
narrative_ontology:measurement(cons_grid_13, constitutional_authority_boundary__coordinate_construction_reading, resistance(organizational), 1789, 0.35).
narrative_ontology:measurement(cons_grid_14, constitutional_authority_boundary__coordinate_construction_reading, resistance(organizational), 2000, 0.48).
narrative_ontology:measurement(cons_grid_15, constitutional_authority_boundary__coordinate_construction_reading, resistance(structural), 1789, 0.3).
narrative_ontology:measurement(cons_grid_16, constitutional_authority_boundary__coordinate_construction_reading, resistance(structural), 2000, 0.4).
narrative_ontology:measurement(cons_grid_17, constitutional_authority_boundary__coordinate_construction_reading, stakes_inflation(class), 1789, 0.2).
narrative_ontology:measurement(cons_grid_18, constitutional_authority_boundary__coordinate_construction_reading, stakes_inflation(class), 2000, 0.28).
narrative_ontology:measurement(cons_grid_19, constitutional_authority_boundary__coordinate_construction_reading, stakes_inflation(individual), 1789, 0.25).
narrative_ontology:measurement(cons_grid_20, constitutional_authority_boundary__coordinate_construction_reading, stakes_inflation(individual), 2000, 0.22).
narrative_ontology:measurement(cons_grid_21, constitutional_authority_boundary__coordinate_construction_reading, stakes_inflation(organizational), 1789, 0.15).
narrative_ontology:measurement(cons_grid_22, constitutional_authority_boundary__coordinate_construction_reading, stakes_inflation(organizational), 2000, 0.35).
narrative_ontology:measurement(cons_grid_23, constitutional_authority_boundary__coordinate_construction_reading, stakes_inflation(structural), 1789, 0.1).
narrative_ontology:measurement(cons_grid_24, constitutional_authority_boundary__coordinate_construction_reading, stakes_inflation(structural), 2000, 0.4).
narrative_ontology:measurement(cons_grid_25, constitutional_authority_boundary__coordinate_construction_reading, suppression(class), 1789, 0.22).
narrative_ontology:measurement(cons_grid_26, constitutional_authority_boundary__coordinate_construction_reading, suppression(class), 2000, 0.25).
narrative_ontology:measurement(cons_grid_27, constitutional_authority_boundary__coordinate_construction_reading, suppression(individual), 1789, 0.15).
narrative_ontology:measurement(cons_grid_28, constitutional_authority_boundary__coordinate_construction_reading, suppression(individual), 2000, 0.18).
narrative_ontology:measurement(cons_grid_29, constitutional_authority_boundary__coordinate_construction_reading, suppression(organizational), 1789, 0.18).
narrative_ontology:measurement(cons_grid_30, constitutional_authority_boundary__coordinate_construction_reading, suppression(organizational), 2000, 0.28).
narrative_ontology:measurement(cons_grid_31, constitutional_authority_boundary__coordinate_construction_reading, suppression(structural), 1789, 0.12).
narrative_ontology:measurement(cons_grid_32, constitutional_authority_boundary__coordinate_construction_reading, suppression(structural), 2000, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__coordinate_construction_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, judicial_review_legitimacy).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, legislative_override_practice).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, executive_non_acquiescence).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, popular_constitutionalism_access).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the constitutional_authority_boundary kernel into three readings with distinct ε values and stakeholder structures. coordinate_construction_reading (this story, ε≈0.42, Rope/Tangled Rope) coexists with judicial_supremacy_reading (ε≈0.65, Snare from legislative/executive seats) and parliamentary_primacy_reading (ε≈0.55, Tangled Rope from judicial seat). The upstream Mountain is the constitutional text itself (ε≈0.05) — the kernel's fixed text coordinates the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__coordinate_construction_reading, institutional, 0.48).
constraint_indexing:directionality_override(constitutional_authority_boundary__coordinate_construction_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
