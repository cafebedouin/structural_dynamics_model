% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__parliamentary_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Parliamentary Primacy Constitutional Authority Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint embodies one reading of a contested constitutional
 *   kernel: the question of WHO holds final authority to interpret the
 *   constitution. Under the parliamentary primacy reading, the elected
 *   legislature is sovereign over constitutional meaning. The written
 *   constitutional text, where it exists, is subordinate to legislative will.
 *   The legislature may define, redefine, or override constitutional meaning
 *   through ordinary legislation or (where constitutions are entrenched)
 *   through procedures that bind even future legislatures. The reading
 *   forecloses the judicial supremacy alternative (courts as final arbiters)
 *   and coexists with coordinate construction readings (distributed
 *   authority). The claim/metric gap is intentional: the reading is CLAIMED
 *   as tangled_rope (genuine democratic coordination problem + asymmetric
 *   extraction via reduced judicial review), while the authored metrics
 *   reflect low baseline extractiveness (0.18) because the reading genuinely
 *   solves a coordination problem in democratic governance.
 *
 * KEY AGENTS:
 *   - elected_legislature: primary beneficiary of interpretive authority; sets agenda for constitutional meaning
 *   - judiciary: constrained to subordinate role; payer in terms of reduced authority, but benefits from institutional independence
 *   - minority_rights_advocates: victims of reduced judicial protection; depend on legislative remedy
 *   - executive: benefits from legislative clarity; pays when hostile legislatures constrain executive power
 *   - constitutional_amendment_body: the ultimate authority under entrenchment; sets the backstop
 *   - judicial_supremacy_advocates: structurally excluded from this reading's authority framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__parliamentary_primacy_reading, 0.18).
domain_priors:suppression_score(constitutional_authority_boundary__parliamentary_primacy_reading, 0.42).
domain_priors:theater_ratio(constitutional_authority_boundary__parliamentary_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(constitutional_authority_boundary__parliamentary_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__parliamentary_primacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__parliamentary_primacy_reading, "Parliamentary Primacy Constitutional Authority Reading").
narrative_ontology:topic_domain(constitutional_authority_boundary__parliamentary_primacy_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__parliamentary_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__parliamentary_primacy_reading, '7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45').
narrative_ontology:cs_kernel_codification('7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45', formalized).
narrative_ontology:cs_authority_grounding('7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45', lineage).
narrative_ontology:cs_interpretation_layer_present('7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45').
narrative_ontology:cs_reading_relation('7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45', constitutional_authority_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45', constitutional_authority_boundary__coordinate_construction_reading, influences).
narrative_ontology:cs_axiom('7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45', foundational, parliamentary_sovereignty_supreme).
narrative_ontology:cs_axiom_status(parliamentary_sovereignty_supreme, holdable).
narrative_ontology:cs_axiom_grounding('7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45', parliamentary_sovereignty_supreme, conventional).
narrative_ontology:cs_axiom('7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45', foundational, judicial_review_subordinate_to_legislation).
narrative_ontology:cs_axiom_status(judicial_review_subordinate_to_legislation, holdable).
narrative_ontology:cs_axiom_grounding('7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45', judicial_review_subordinate_to_legislation, deontological).
narrative_ontology:cs_reference_frame('7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45', parliamentary_authority_primacy).
narrative_ontology:cs_drift_state('7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45', post_world_war_two_constitutional_courts, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7bf6fcbc-2495-431b-a4e1-31b7cfd4ac45', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_review_claimants).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, minority_rights_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__parliamentary_primacy_reading, executive).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary).
narrative_ontology:constraint_victim(constitutional_authority_boundary__parliamentary_primacy_reading, executive).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, democratic_mandate_supremacy).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__parliamentary_primacy_reading, legislative_accountability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets constitutional meaning through ordinary and entrenched legislation; retains final authority to override or redefine prior interpretations; directly accountable to voters in periodic elections. As the reading's primary seat, the legislature interprets the constitutional text as subordinate to its democratic mandate and views judicial review as advisory or easily overridden. Collects the benefit of unrestricted legislative authority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Constrained to a subordinate interpretive role; judicial review of legislative acts is either advisory or subject to legislative override via ordinary or entrenched legislation. Retains internal autonomy in non-constitutional matters and benefits from formal institutional independence, but bears the cost of reduced constitutional authority. The reading positions courts as administrators of law, not arbiters of constitutional limits.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, judiciary, beneficiary).

% Depend on judicial review to protect rights against majoritarian legislation. Under this reading, their recourse is legislative remedy (petition the same majority that enacted the harmful law) or constitutional amendment (collective action against entrenched legislation). They bear the cost of reduced judicial protection when legislatures act contrary to individual or minority interests. Exit options are limited to political organizing or migration.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, minority_rights_advocates, payer,
    organized, biographical, constrained, national).

% Benefits from legislative clarity and reduced judicial second-guessing of executive acts (judiciary cannot easily invalidate on constitutional grounds); at the same time, the executive's authority depends on legislative authorization and appropriation. Faces costs if hostile legislatures use their primacy to constrain executive power through statute.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, executive, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__parliamentary_primacy_reading, executive, payer).

% Analyze and debate the constitutional text's meaning. Under this reading, scholarship that contradicts legislative interpretation carries advisory force only; legislatures are not bound by academic consensus on constitutional doctrine. They function as analytical seats whose influence on constitutional meaning is filtered through the democratic process.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_scholars, observer,
    organized, generational, analytical, national).

% When the constitutional text is entrenched (requiring supermajority amendment), the body that can amend the constitution (super-legislature, constituent assembly, or electorate) sits above even the ordinary legislature. This reading locates the ultimate constitutional authority here: the legislature is sovereign over ordinary interpretation, but the amendment body is sovereign over entrenched text.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_amendment_body, agenda_setter,
    institutional, civilizational, analytical, national).

% Would argue for courts as final arbiters of constitutional meaning and would challenge legislative override of judicial decisions. They are structurally excluded from this reading's authority framework; their voice is present in ongoing constitutional dispute but not seated in the decision structure under parliamentary primacy. They would reverse the reading's core premise if given authority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__parliamentary_primacy_reading, judicial_supremacy_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__parliamentary_primacy_reading, elected_legislature).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__parliamentary_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, stable locus of constitutional authority: the elected legislature. Solves the coordination problem of 'who decides what the constitution means' by vesting that power in an accountable, periodic-election-responsive body rather than an unelected judiciary. Reduces constitutional deadlock by making the legislature the final forum for constitutional dispute.
% TRANSFER_FUNCTION: Transfers interpretive authority from courts to legislatures, and from written constitutional text to legislative will. The legislature collects the benefit of defining constitutional constraints on its own power (or declining to be constrained). Minority rights advocates and judicial review claimants bear the cost of reduced judicial protection against majoritarian legislation.
% ABSENT_VOICES: Judicial supremacy advocates are excluded from the authority structure; they would contest the premise that legislatures should be sovereign over constitutional meaning. Constitutional scholars whose expertise contradicts legislative interpretation are also excluded from binding authority, though they may lobby or publish. Future generations (who cannot amend the entrenched text without the specified supermajority) are structurally absent from decisions about constitutional entrenchment.
% DISAPPEARANCE_RATIONALE: If this reading's constraint disappeared (if legislatures suddenly lacked authority to define or override constitutional meaning, and courts became final arbiters instead), institutional arrangements would shift dramatically: the judiciary would become the primary locus of constitutional power, legislatures would operate under judicially-policed constraints, and the entire balance of institutional authority would invert. However, *within the reading itself*, the disappearance is contested because some parties (judiciary, minority advocates) might welcome the shift while the legislature would experience it as loss of authority.
% FOUNDING_PROBLEM: How can a representative democracy be governed by a constitution when that constitution is written text that may not perfectly reflect current democratic will? The founding problem posits that the solution is legislative sovereignty: the elected representatives of the people should be free to interpret and modify the constitutional framework through ordinary legislative processes, ensuring the constitution remains subject to democratic control rather than becoming a fixed constraint imposed by historical text.
% FOUNDING_PROBLEM_CORROBORATION: Parliaments in Westminster-tradition jurisdictions (UK, Canada, Australia, New Zealand before charter limitations) have long operated under this reading and attest to its necessity for responsive democracy. Legislatures in unitary sovereign states attest that judicial supremacy would paralyze democratic reform. However, constitutional court judges and civil rights organizations outside the benefiting parties attest that the founding problem has been substantially mitigated by constitutional rights guarantees and that parliamentary primacy creates the danger of tyranny of the majority. Post-WWII constitutionalism in Europe, and the proliferation of constitutional courts, reflects a competing diagnosis: the founding problem is not 'how to make the constitution democratic' but 'how to make the legislature respect fundamental rights'.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__parliamentary_primacy_reading, contested).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__parliamentary_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__parliamentary_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__parliamentary_primacy_reading, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.18) because the constraint solves a genuine problem: establishing a stable, accountable locus for constitutional interpretation. The legislature is popularly elected and periodically accountable; its authority reflects democratic will. Theater ratio is moderate (0.28) because the reading includes performative elements — legislatures invoke democratic mandate while simultaneously constraining judicial review, which reduces the candor with which the actual power transfer is discussed. Suppression is moderate (0.42) because the reading's persistence requires the judiciary to accept a subordinate role without forcibly claiming supreme authority, and requires minority-rights advocates to work within legislative channels rather than through judicial claims. Resistance is 0.58 because judicial institutions and constitutional scholars have mounted sustained intellectual and institutional resistance, and post-WWII constitutional developments have moved globally toward judicial review. The measurement series shows extractiveness and suppression stable over the interval, with slight oscillation in theater ratio as political cycles wax and wane.
 *
 * PERSPECTIVAL GAP:
 *   The divergence between beneficiary and victim seats is structural and permanent under this reading: the legislature cannot simultaneously be sovereign and subordinate; the judiciary cannot simultaneously be final arbiter and advisory. The divergence is not a measurement to reconcile but a classification to compute per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislature benefits from authority it would not have under competing readings (d near 0.0 for the legislature as beneficiary). The judiciary is constrained by a rule that, under alternative readings, would not bind their authority (d moderate-to-high for judiciary as payer, mitigated by institutional independence benefits). Minority-rights advocates are trapped: they cannot exit the jurisdiction without migration, their identity is often constitutionally bound (citizenship), and they depend on judicial protection that this reading denies them (d high for minority advocates as victims). The amendment body sits at the apex: ultimate authority, but analytically positioned since the constraint only binds ordinary legislatures, not the constituent power. Judicial supremacy advocates are excluded, not positioned; their alternative reading would reverse the entire authority structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (establishing stable locus for constitutional interpretation) remains live, but post-WWII developments have shifted the diagnosis. The reading was designed to solve 'tyranny of constitutional text' (how to prevent written constitutions from paralyzing democracies). Contemporary constitutional courts arose to solve a different problem: 'tyranny of the majority' (how to prevent elected majorities from eliminating fundamental rights). The rise of judicial review globally is mandatrophy evidence: the original founding problem has shifted from 'legislature vs. text' to 'individual rights vs. majority' and the reading's solution no longer addresses the live problem. However, in Westminster-tradition jurisdictions, the reading persists because the founding problem continues to be framed as 'democratic responsiveness' rather than 'rights protection'. The reading does not exhibit terminal mandatrophy (the founding problem is still invoked), but it does exhibit contested mandatrophy: different parties locate the founding problem differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    entrenchment_level_ambiguity,
    'When the reading refers to ''entrenched legislation,'' what level of supermajority and what override procedures lock in legislative dominance? Can a future legislature amend even the amendment mechanism itself?',
    'Analyze the specific constitutional provisions governing amendment and entrenchment in jurisdictions claiming parliamentary primacy (e.g., Canada Act 1982 with amending formula; UK Parliament Acts with supermajority requirements). Determine whether the reading permits infinite regress (a legislature amending its own entrenchment procedures).',
    'If legislative authority is not truly entrenched (any legislature can override prior entrenchment), the reading merges into pure legislative supremacy with no constitutional constraint. If entrenchment is rigid and unamendable by ordinary legislatures, the reading covertly defers to the super-legislature (amendment body) and the constraint becomes: legislatures are sovereign only within the authority granted by the amendment process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_level_ambiguity, conceptual, 'The recursion boundary for legislative authority under entrenchment.').

omega_variable(
    extraction_cover_story_ambiguity,
    'Is the claim that ''elected legislatures should be sovereign over constitutional meaning'' a genuine solution to a coordination problem, or a cover story for majority power to override minority protections?',
    'Compare historical patterns: Do legislatures that claim parliamentary primacy use that authority to expand rights (evidence for coordination solution) or to restrict minority rights (evidence for extraction cover story)? Examine whether the reading is invoked symmetrically (legislatures accept being bound by their own prior entrenchment) or selectively (legislatures override entrenchment when inconvenient).',
    'If the reading is a genuine coordination solution (legislatures accept binding entrenchment), the low extractiveness (0.18) is correct. If the reading functions as a cover story for majoritarian override of entrenched rights, the actual extractiveness should be measured at 0.35–0.50 and the reading reclassifies to snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_cover_story_ambiguity, empirical, 'Whether parliamentary primacy is a coordination mechanism or a rationalization for majoritarian extraction.').

omega_variable(
    judicial_resistance_internalization,
    'Does the judiciary''s acceptance of subordinate authority reflect internalized legitimacy of the parliamentary primacy reading, or does it reflect suppression (institutional pressure, career dependence, lack of alternative)?',
    'Examine judicial resistance trajectories: In jurisdictions moving from parliamentary primacy to judicial review (UK''s Human Rights Act, Canada''s Charter, post-WWII Europe), does the judiciary immediately reassert interpretive authority once the legal framework permits it (evidence of suppression and internalized constraint), or does it remain deferential (evidence of genuine legitimacy)?',
    'If resistance is suppressed and judges reassert authority when permitted, the measured suppression (0.42) understates the true pressure on the judiciary, and the reading''s actual constraining force is higher. If deference persists even after legal permission to review, the suppression is lower and represents genuine institutional subordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_resistance_internalization, empirical, 'Whether judicial deference to parliament is structural or suppressed.').

omega_variable(
    kernel_reading_foreclosure_test,
    'Does the parliamentary primacy reading logically foreclose the judicial supremacy reading within a single constitutional framework, or do they coexist as competing live positions?',
    'Test at the level of core premises: Parliamentary primacy asserts ''elected representatives should be sovereign over constitutional meaning''; judicial supremacy asserts ''courts should be final arbiters of all constitutional questions.'' Are these premises contradictory (one logically rules out the other) or are they different allocations of authority that could theoretically be held by different parties in an ongoing dispute?',
    'If foreclosure holds (the premises are truly contradictory), the reading_relations entry to judicial_supremacy should be ''forecloses'' not ''coexists_with''. If coexistence holds, the readings remain live options for different factions of a constitutional dispute.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Whether the parliamentary primacy and judicial supremacy readings are logically incompatible or merely different.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__parliamentary_primacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t5, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(cons_tr_t5, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t15, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(cons_tr_t15, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t25, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement_basis(cons_tr_t25, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__parliamentary_primacy_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(cons_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t5, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 5, 0.17).
narrative_ontology:measurement_basis(cons_be_t5, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t15, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 15, 0.19).
narrative_ontology:measurement_basis(cons_be_t15, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 20, 0.19).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t25, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 25, 0.18).
narrative_ontology:measurement_basis(cons_be_t25, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 30, 0.17).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__parliamentary_primacy_reading, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(cons_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t5, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement_basis(cons_su_t5, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t15, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement_basis(cons_su_t15, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t25, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 25, 0.43).
narrative_ontology:measurement_basis(cons_su_t25, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__parliamentary_primacy_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(cons_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__parliamentary_primacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__parliamentary_primacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__parliamentary_primacy_reading, constitutional_authority_boundary__coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel constitutional_authority_boundary. All three readings share the same referent (the question of constitutional interpretive authority) but author different ε values, beneficiary/victim structures, and classifications from their respective diagnostic perspectives. Sibling readings: judicial_supremacy_reading (courts as final arbiters; ε typically 0.35–0.50, snare-class), coordinate_construction_reading (distributed authority; ε typically 0.20–0.30, rope-class). The parliamentary_primacy_reading (this file) positions legislatures as beneficiaries of interpretive authority; ε 0.18 reflects that the reading genuinely solves a coordination problem in democratic governance while creating asymmetric costs for judicial and minority-rights seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__parliamentary_primacy_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
