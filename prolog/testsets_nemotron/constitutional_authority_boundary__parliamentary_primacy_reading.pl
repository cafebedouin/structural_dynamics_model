% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: constitutional_authority_boundary__parliamentary_primacy_reading
 *   human_readable: Parliamentary Primacy Reading of Constitutional Authority
 *   domain: constitutional_law/political_philosophy/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the parliamentary primacy reading of
 *   the contested kernel 'constitutional_authority_boundary'. The reading
 *   holds that constitutional text is subordinate to parliamentary
 *   sovereignty, with the elected legislature retaining final interpretive
 *   authority through ordinary or entrenched legislation. This reading is one
 *   of three sibling readings (judicial_supremacy_reading,
 *   coordinate_construction_reading, parliamentary_primacy_reading) that
 *   produce structurally distinct constraints from the same kernel label. The
 *   constraint is claimed as tangled_rope: it solves the genuine coordination
 *   problem of final interpretive authority (someone must decide what the
 *   constitution means) while extracting from judicial independence and
 *   minority rights protection — the legislature gains interpretive finality,
 *   courts and minority claimants lose binding constitutional constraint.
 *
 * KEY AGENTS:
 *   - elected_legislature: Primary beneficiary (institutional/arbitrage) — holds final interpretive authority
 *   - majority_parliamentary_party: Secondary beneficiary (powerful/constrained) — exercises legislative power to define constitutional meaning
 *   - constitutional_courts: Primary victim (institutional/constrained) — reduced to advisory or easily-overridden review
 *   - minority_rights_claimants: Secondary victim (powerless/trapped) — lose entrenched constitutional protection against legislative majorities
 *   - constitutional_scholars: Observer (analytical/analytical) — analyze the structural implications of each reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.22).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.35).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy Reading of Constitutional Authority").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, 'b8790e07-b3db-4e67-ab54-da4d4b8b2036').
narrative_ontology:cs_kernel_codification('b8790e07-b3db-4e67-ab54-da4d4b8b2036', distributed).
narrative_ontology:cs_authority_grounding('b8790e07-b3db-4e67-ab54-da4d4b8b2036', lineage).
narrative_ontology:cs_interpretation_layer_present('b8790e07-b3db-4e67-ab54-da4d4b8b2036').
narrative_ontology:cs_reading_relation('b8790e07-b3db-4e67-ab54-da4d4b8b2036', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('b8790e07-b3db-4e67-ab54-da4d4b8b2036', constitutional_authority_boundary__coordinate_construction_reading, coexists_with).
narrative_ontology:cs_axiom('b8790e07-b3db-4e67-ab54-da4d4b8b2036', foundational, legislative_final_interpretive_authority).
narrative_ontology:cs_axiom_status(legislative_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('b8790e07-b3db-4e67-ab54-da4d4b8b2036', legislative_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('b8790e07-b3db-4e67-ab54-da4d4b8b2036', foundational, constitutional_text_subordinate_to_parliamentary_sovereignty).
narrative_ontology:cs_axiom_status(constitutional_text_subordinate_to_parliamentary_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('b8790e07-b3db-4e67-ab54-da4d4b8b2036', constitutional_text_subordinate_to_parliamentary_sovereignty, conventional).
narrative_ontology:cs_reference_frame('b8790e07-b3db-4e67-ab54-da4d4b8b2036', parliamentary_sovereignty_framework).
narrative_ontology:cs_drift_state('b8790e07-b3db-4e67-ab54-da4d4b8b2036', contemporary_rights_based_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b8790e07-b3db-4e67-ab54-da4d4b8b2036', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, majority_parliamentary_party).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_courts).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, minority_rights_claimants).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, parliamentary_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_legitimacy_of_legislative_supremacy).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, legislative_final_authority_over_constitutional_meaning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final authority to define constitutional meaning through ordinary or entrenched legislation. This interpretive power is exercised through the normal legislative process — the legislature can override judicial interpretations, amend constitutional understanding, and define the scope of rights and powers. The institution has no external constraint on its interpretive authority and can change constitutional meaning at will (subject only to political consequences).
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, beneficiary,
    institutional, generational, arbitrage, national).

% As the controlling party in the legislature, it exercises the legislative interpretive power in practice. It can define constitutional meaning to advance its policy agenda, protect its legislative achievements from judicial invalidation, and structure the constitutional order to its advantage. Its power is constrained by electoral cycles, coalition dynamics, and the need to maintain democratic legitimacy — it cannot simply ignore political consequences.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, majority_parliamentary_party, beneficiary,
    powerful, biographical, constrained, national).

% Reduced to advisory review or easily-overridden review. Courts may issue opinions on constitutional questions, but the legislature retains final authority and can override judicial interpretations through subsequent legislation. The courts' institutional role is to advise and legitimate, not to bind. Their exit is constrained — they cannot leave the system, and their independence is structurally limited by legislative supremacy. They retain some influence through the persuasive force of their reasoning and the political cost to the legislature of openly defying judicial consensus.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_courts, payer,
    institutional, generational, constrained, national).

% Constitutional rights protections exist only at legislative sufferance. Minority claimants cannot rely on judicial enforcement of entrenched rights against legislative majorities — any protection can be overridden by ordinary or entrenched legislation. Their identity as rights-bearers is fused to the constitutional text that the parliamentary primacy reading subordinates; they cannot exit this identity without abandoning their claim to constitutional protection. They have no institutional exit and no political leverage to change the arrangement.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, minority_rights_claimants, payer,
    powerless, biographical, identity_locked, national).

% Analyze and critique the structural implications of parliamentary primacy versus its sibling readings. They do not bear the constraint's costs or collect its benefits directly, but their work shapes the intellectual environment in which the constitutional order operates. They can advocate for any of the three readings and influence how the kernel is understood across jurisdictions and generations.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the problem of final interpretive authority over constitutional meaning — someone must have the last word on what the constitution means, and this reading assigns that authority to the democratically elected legislature rather than unelected judges or a fragmented coordinate system.
% TRANSFER_FUNCTION: Moves final interpretive authority from judicial bodies to the elected legislature, and moves the power to define rights and structural limits from entrenched constitutional text to legislative majorities (or supermajorities via entrenched legislation). The transfer is authority and constraint-definition, not material resources.
% ABSENT_VOICES: Future generations who would inherit a constitutional order without entrenched protection against legislative majorities; minority groups not yet organized as rights-claimants but who would be subject to legislative definition of their status; constitutional courts in other jurisdictions that look to this system as a model — their structural interests are not represented in the current discourse.
% DISAPPEARANCE_RATIONALE: If parliamentary primacy vanished overnight, the constitutional order would immediately confront the question of who holds final interpretive authority. Either judicial supremacy would fill the vacuum (courts becoming final arbiters) or coordinate construction would emerge (branches negotiating interpretive settlements). The legislative majority would lose its interpretive trump card; minority rights claimants would gain judicial enforcement of entrenched protections. The constitutional order would fundamentally restructure.
% FOUNDING_PROBLEM: The democratic legitimacy crisis of unelected judges exercising final authority to invalidate legislation enacted by elected representatives — the 'counter-majoritarian difficulty.' Parliamentary primacy was constructed to resolve this by making the legislature the final interpreter of constitutional meaning, accountable to the electorate.
% FOUNDING_PROBLEM_CORROBORATION: Parliamentary sovereignty theorists (Dicey, contemporary UK and NZ scholars) attest the founding problem remains live — judicial review still threatens democratic legitimacy. Judicial supremacy advocates (Dworkin, Ely, constitutional courts in Germany, India, South Africa) attest the problem is misdiagnosed — the counter-majoritarian difficulty is a feature, not a bug, protecting minorities from majoritarian tyranny. Coordinate construction proponents (Tushnet, Waldron, Canadian 'dialogue' theorists) attest the problem is real but the solution is wrong — distributed interpretive authority better serves both democracy and rights. No single corroboration settles the dispute.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness is low (0.22) because the constraint's primary function is coordination — resolving the problem of who finally interprets the constitution. The extraction falls on judicial independence and minority rights, not on the general population. Suppression (0.35) is moderate because the constraint actively constrains judicial power (courts cannot bind the legislature) but does not suppress alternative constitutional visions — coordinate and judicial supremacy readings remain live in discourse. Theater ratio is low (0.15) because the legislative interpretive role is genuinely exercised, not merely performed. Accessibility collapse (0.65) is moderately high: once parliamentary primacy is accepted as the interpretive framework, alternative arrangements (judicial supremacy, coordinate construction) become difficult to instantiate without constitutional rupture. Resistance (0.45) reflects ongoing contestation from judicial supremacy advocates and minority rights defenders.
 *
 * PERSPECTIVAL GAP:
 *   The legislature and its majority party experience this constraint as coordination (rope-like): it solves the problem of interpretive finality and empowers democratic accountability. Constitutional courts experience it as extraction (snare-like): their binding authority is removed without their consent. Minority rights claimants experience it as extraction with no exit (trapped): their constitutional protections exist only at legislative sufferance. The engine computes this divergence from the structural data — beneficiary/victim declarations plus exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Elected legislature is the structural beneficiary: it gains final interpretive authority without external constraint (d near 0.0). Majority parliamentary party benefits as the agent exercising that authority (d near 0.1). Constitutional courts are primary victims: their binding review power is structurally eliminated, with constrained exit (d near 0.8). Minority rights claimants are secondary victims: they lose entrenched protection with no institutional exit (d near 0.9, identity_locked via rights-claimant identity). The coordination function (resolving interpretive finality) is real but asymmetrically distributed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — who has final authority to interpret the constitution — remains live (status: contested). The parliamentary primacy reading was constructed to solve the democratic legitimacy deficit of unelected judges overriding elected representatives. Whether that problem is solved or whether the solution has become extraction depends on whether legislative majorities exercise interpretive power responsibly or opportunistically. The mandate has not atrophied into piton because the coordination function (final interpretive authority) is actively exercised and contested, not merely performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the parliamentary primacy reading of constitutional authority a distinct constraint from the judicial supremacy and coordinate construction readings, or merely a different emphasis on the same constitutional arrangement?',
    'Test whether the three readings produce different epsilon values, different beneficiary/victim structures, and different enforcement requirements when evaluated against the same constitutional text. If they do, they are structurally distinct constraints.',
    'If distinct, each reading generates its own classification and drift trajectory; if not, the kernel boundary was misidentified and the readings are perspectival variants of one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether parliamentary primacy, judicial supremacy, and coordinate construction are one constraint with three observers or three constraints sharing a kernel label').

omega_variable(
    extraction_ambiguity_parliamentary_vs_constitutional,
    'Does the legislative power to define constitutional meaning constitute genuine coordination (solving the problem of final interpretive authority) or extraction (legislature capturing interpretive rents at the expense of judicial independence and minority protection)?',
    'Examine historical instances where parliamentary primacy was exercised to override judicial rights protections — did the legislature produce a more legitimate resolution, or did it extract advantage for the majority at minority expense? Cross-jurisdictional comparison of parliamentary sovereignty systems.',
    'If genuine coordination, the constraint is tangled_rope with low effective extraction; if extraction-dominant, the constraint approaches snare for judicial and minority seats despite the low base epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_ambiguity_parliamentary_vs_constitutional, conceptual, 'Whether legislative interpretive finality is coordination or extraction in disguise').

omega_variable(
    entrenched_legislation_boundary,
    'Does the availability of entrenched legislation (supermajority requirements, special procedures) meaningfully constrain legislative interpretive power, or is it a procedural formality that the legislative majority can bypass when motivated?',
    'Empirical study of constitutional amendment and override rates in parliamentary sovereignty systems — how often are entrenched provisions actually used versus ordinary legislation achieving the same interpretive effect?',
    'If entrenchment is a real constraint, the arrangement has a genuine coordination scaffold element; if it is routinely bypassed, the constraint''s extraction on judicial and minority seats is higher than the formal structure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenched_legislation_boundary, empirical, 'Whether entrenched legislation functions as a real constraint on legislative interpretive power or a procedural theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t25, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement(cons_tr_t50, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 50, 0.14).
narrative_ontology:measurement(cons_tr_t75, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 75, 0.15).
narrative_ontology:measurement(cons_tr_t100, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cons_be_t25, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 25, 0.2).
narrative_ontology:measurement(cons_be_t50, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(cons_be_t75, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 75, 0.23).
narrative_ontology:measurement(cons_be_t100, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 100, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cons_su_t25, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 25, 0.3).
narrative_ontology:measurement(cons_su_t50, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 50, 0.33).
narrative_ontology:measurement(cons_su_t75, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 75, 0.35).
narrative_ontology:measurement(cons_su_t100, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 100, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__parliamentary_primacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This reading and its two siblings form the constitutional_authority_boundary constraint family. All three instantiate different constraints from the same kernel label. The parliamentary primacy reading has the lowest base extractiveness (0.22) because its coordination function (legislative finality) is most directly aligned with democratic legitimacy, but it extracts from judicial independence and minority protection. The judicial supremacy reading likely has higher base extractiveness (judiciary extracting from legislative will) and the coordinate construction reading sits between them (distributed authority with higher transaction costs).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__parliamentary_primacy_reading, institutional, 0.05).
constraint_indexing:directionality_override(constitutional_authority_boundary__parliamentary_primacy_reading, powerful, 0.15).
constraint_indexing:directionality_override(constitutional_authority_boundary__parliamentary_primacy_reading, institutional, 0.8).
constraint_indexing:directionality_override(constitutional_authority_boundary__parliamentary_primacy_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
