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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: constitutional_authority_boundary__coordinate_construction_reading
 *   human_readable: Coordinate Construction: Distributed Constitutional Authority Across Three Branches
 *   domain: constitutional_law/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the coordinate construction reading of
 *   the constitutional authority boundary kernel. The reading asserts that
 *   the constitutional text establishes three co-equal branches with
 *   distributed interpretive authority—each branch interprets the
 *   constitution within its sphere (legislation, execution, adjudication)
 *   with no single final arbiter. This differs sharply from the judicial
 *   supremacy reading (courts as final interpreters) and the parliamentary
 *   primacy reading (legislature as final). The coordinate construction
 *   reading has been a live position in constitutional theory and practice,
 *   defended by scholars of departmentalism and institutional pluralism, and
 *   instantiated in periods when executive or legislative actors declined to
 *   defer to judicial constitutional interpretations. The constraint models
 *   the actual operation of this distributed authority system and its
 *   extraction dynamics: the arrangement requires ongoing negotiation and
 *   creates legal uncertainty, which can be read either as the price of
 *   preventing tyranny or as dysfunction that no reading should tolerate.
 *
 * KEY AGENTS:
 *   - legislative_branch: Agenda-setter and beneficiary; asserts interpretive authority over legislative scope and enumerated powers; pays coordination costs when executives or courts reject legislative constitutional readings.
 *   - executive_branch: Agenda-setter and beneficiary; asserts interpretive authority over executive power and foreign affairs; pays coordination costs when courts enjoin executive action or legislatures override executive constitutional claims.
 *   - judicial_branch: Agenda-setter and beneficiary; asserts interpretive authority in cases but does not claim final interpretive supremacy; constrained by non-acquiescence from other branches.
 *   - political_parties and electoral coalitions: Organized beneficiaries; able to reshape constitutional meaning through electoral control and legislation rather than through exclusive court control; pay the cost of ongoing constitutional dispute.
 *   - citizens and constitutional interpreters: Excluded from formal interpretation or powerless to enforce their reading; feed interpretive arguments to the branches but have no binding voice.
 *   - international legal order: Excluded from interpretation but affected by which branch's reading controls foreign policy and treaty meaning.
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
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Coordinate Construction: Distributed Constitutional Authority Across Three Branches").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "constitutional_law/institutional_design").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, '68012b9f-b092-482c-8244-0cfa494fb703').
narrative_ontology:cs_kernel_codification('68012b9f-b092-482c-8244-0cfa494fb703', formalized).
narrative_ontology:cs_authority_grounding('68012b9f-b092-482c-8244-0cfa494fb703', lineage).
narrative_ontology:cs_interpretation_layer_present('68012b9f-b092-482c-8244-0cfa494fb703').
narrative_ontology:cs_reading_relation('68012b9f-b092-482c-8244-0cfa494fb703', constitutional_authority_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('68012b9f-b092-482c-8244-0cfa494fb703', constitutional_authority_boundary__parliamentary_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('68012b9f-b092-482c-8244-0cfa494fb703', foundational, no_single_final_arbiter).
narrative_ontology:cs_axiom_status(no_single_final_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('68012b9f-b092-482c-8244-0cfa494fb703', no_single_final_arbiter, deontological).
narrative_ontology:cs_axiom('68012b9f-b092-482c-8244-0cfa494fb703', foundational, distributed_interpretive_duty_across_branches).
narrative_ontology:cs_axiom_status(distributed_interpretive_duty_across_branches, holdable).
narrative_ontology:cs_axiom_grounding('68012b9f-b092-482c-8244-0cfa494fb703', distributed_interpretive_duty_across_branches, conventional).
narrative_ontology:cs_reference_frame('68012b9f-b092-482c-8244-0cfa494fb703', constitutional_text_as_distributed_authority).
narrative_ontology:cs_drift_state('68012b9f-b092-482c-8244-0cfa494fb703', contemporary_administrative_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('68012b9f-b092-482c-8244-0cfa494fb703', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, constitutional_legitimacy_through_distributed_interpretation).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, inter_branch_equilibrium).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, executive_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, political_parties_and_electoral_coalitions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts legislation and retains authority to interpret the Constitution in the legislative sphere—regulatory scope, enumerated powers, spending authority, structural amendment process. Enforces this authority by refusing to acknowledge judicial invalidations that exceed constitutional bounds (as legislatively understood), and by redefining jurisdictional limits through statute. Benefits from not being subordinate to judicial supremacy but bears the cost of coordination failures with other branches.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch, beneficiary).

% Executes the law and interprets the Constitution within executive authority—treaty-making, commander-in-chief powers, appointment and removal authority, executive privilege. Asserts independent duty to uphold the Constitution and declines to defer to judicial interpretations it views as violating executive constitutional prerogatives (e.g., refusing to implement a court order it reads as ultra vires). Benefits from coordinate authority but faces conflict when courts assert jurisdiction over executive function.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, executive_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, executive_branch, beneficiary).

% Interprets the Constitution in cases and controversies brought within jurisdiction. Under coordinate construction, does not hold final authority over legislative or executive interpretation; rather, courts interpret the Constitution for purposes of adjudication, but coordinate-reading legislatures and executives may decline to accept judicial constitutional readings as binding on their own constitutional duties. Benefits from avoiding supremacy responsibility but faces structural constraint: cannot unilaterally enforce constitutional interpretation against non-acquiescence.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch, beneficiary).

% Benefit from the arrangement's stability: no single branch monopolizes constitutional meaning, so shifting electoral coalitions can reshape constitutional interpretation through lawmaking and electoral choice rather than through exclusive control of courts. They pay a coordination cost: constitutional disputes that might be settled by judicial supremacy remain open, creating uncertainty and requiring ongoing political contestation.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, political_parties_and_electoral_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Analyze the constitutional meaning from outside the branches, providing competing interpretive frameworks. Under coordinate construction, their work feeds all three branches rather than being subordinate to a single authoritative interpreter, which gives their function greater structural relevance but also means their analyses compete without institutional settlement.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_theorists_and_academic_interpreters, observer,
    moderate, generational, mobile, national).

% Have no formal seat at constitutional interpretation under any reading; they are the ultimate sovereign in some theories (e.g., popular sovereignty, amendment process), but cannot directly interpret the Constitution in law or policy. They would argue (if heard) that coordinate construction should include their voice through robust amendment, initiative, or recall mechanisms, but the standard three-branch model excludes their participation.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, citizens_as_constitutional_interpreters, excluded,
    powerless, biographical, trapped, national).

% Has no formal role in interpreting this nation's constitution but is affected by which branch's constitutional interpretation controls foreign policy and treaty interpretation. Coordinate construction can create ambiguity about whether treaties are constitutionally binding or whether executive or legislative interpretation controls in cases of conflict.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, international_legal_order, excluded,
    powerful, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__coordinate_construction_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__coordinate_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the founding constitutional problem: how to establish lawful government that binds itself (no source of authority stands above the constitution to enforce it), and how to prevent any single official from monopolizing constitutional meaning and using that monopoly to expand their power indefinitely. Coordinate construction distributes the interpretive duty across three branches, each interpreting within its sphere, so no single power-holder can unilaterally rewrite the constitution through interpretive claims.
% TRANSFER_FUNCTION: Moves constitutional interpretive authority FROM concentration in a single final arbiter TO distributed authority across three branches. The cost of this distribution is coordination overhead—disputes between branches over constitutional meaning are not automatically settled by judicial decree, leaving the settlement mechanism ambiguous and requiring ongoing political negotiation or constitutional amendment.
% ABSENT_VOICES: Citizens, as the ultimate source of constitutional authority (in popular sovereignty theories), are structurally excluded from direct interpretation; their only formal voice is the amendment process, which is deliberately high-friction. International parties affected by constitutional interpretations of foreign policy and treaty authority have no seat. Subjects of coordinate-construction disputes (those caught between conflicting branch interpretations) have no voice in the resolution mechanism itself—they experience the conflict as legal uncertainty.
% DISAPPEARANCE_RATIONALE: If the principle of distributed constitutional authority disappeared and one branch monopolized final interpretation, the entire structural logic of constitutional constraint would shift: the monopoly branch would become the constitutional sovereign, effectively able to rewrite the document through interpretive decree. The resulting government would be radically different—either judicially supreme (courts can invalidate any law) or legislatively supreme (parliament defines the constitution), both of which would reorganize political competition and the separation of powers.
% FOUNDING_PROBLEM: After independence/founding, the written constitutional text requires interpretation in practice, and the crucial question is: who decides what the constitution means when branches disagree? To give this power to one branch (e.g., courts) would make that branch effectively supreme and able to control the others through constitutional reinterpretation. But to give it to no one leaves constitutional disputes unsettled. Coordinate construction proposes that each branch interprets the constitution within its own sphere—legislatures interpret constitutional limits on legislative power, executives interpret constitutional executive power, and courts interpret it in cases—so no single branch has final authority but each is constrained by the others' coordinate interpretation.
% FOUNDING_PROBLEM_CORROBORATION: The coordinate construction reading is defended by constitutional scholars (e.g., Jack Balkin, Sanford Levinson, scholars of departmentalism) who cite the structural absence of any final arbiter in the constitutional text itself and the problem of unconstrained judicial supremacy. The sibling readings (judicial supremacy, parliamentary primacy) are defended by other scholars and by institutional actors within the respective branches, each claiming their reading better solves the founding problem. Corroboration comes from historical practice: periods in which courts did NOT invalidate legislation (most American history before 1803 and after the New Deal Court-packing threat); periods in which executives ignored judicial rulings (e.g., Jackson on the Cherokee); and periods in which legislatures overrode or defied constitutional judicial readings through statute and amendment. No single seat claims exclusive corroboration.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__coordinate_construction_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.42 at interval end, oscillating around 0.35–0.42) because the arrangement requires ongoing political negotiation to settle constitutional disputes—branches pay the cost of coordination failure, and the uncertainty creates space for strategic manipulation by the most powerful branch. Suppression is low (0.28) because coordinate construction does not depend on coercing acceptance of a single interpretation; rather, it relies on the possibility of inter-branch contestation, which is possible only if actors can voice disagreement without severe punishment. Theater ratio is similarly low (0.22), meaning the constraint's functional operation (actual dispute resolution through negotiation and amendment) is mostly what it claims to be, rather than disguised performance. Accessibility collapse is moderate-low (0.45): once the coordinate construction principle is understood, actors realize they can resist a single branch's monopoly, but they also realize the stability cost of distributed authority is high. Resistance is substantial (0.58): the coordinate construction reading faces real resistance from judicial supremacy advocates (who argue it leads to chaos) and parliamentary primacy advocates (who argue legislatures should be supreme). The measurement series models a system in modest flux: extractiveness and suppression requirement both rise gradually as modern administrative law and executive power expanded, but the trend stabilizes and does not accelerate to crisis levels—suggesting the coordinate construction principle continues to constrain even as institutional power dynamics shift.
 *
 * PERSPECTIVAL GAP:
 *   The three agenda-setting branches compute differently from coordinate construction. From the legislative seat, the arrangement is genuine coordination that preserves legislative authority; the suppression and extraction costs are the inevitable friction of inter-branch contestation. From the judicial seat, the arrangement is a constraint on the courts' ability to settle constitutional disputes unilaterally—a cost they bear because coordinate construction denies them supremacy, but also a protection against becoming the final political authority. From the executive seat, the arrangement permits independent constitutional interpretation and resistance to what executives view as judicial overreach, but also denies the executive final authority over its own constitutional scope. No seat experiences this as pure coordination, and no seat experiences it as pure extraction—each pays and collects differently. The engine computes this divergence from the structural data: three equally powerful institutional stakeholders with constrained exit (none can simply leave the constitutional order) and beneficiary/victim positions distributed across the arrangement rather than concentrated in one.
 *
 * DIRECTIONALITY LOGIC:
 *   All three branches are simultaneously beneficiaries (each collects interpretive authority) and victims (each is constrained by the others' coordinate interpretation). The directionality derivation chain yields d ≈ 0.5 for each branch—symmetric positioning—because none monopolizes the extraction and none is fully subordinate. Political parties and electoral coalitions cluster near the beneficiary end (d near 0.2) because they benefit from the possibility of reshaping constitutional meaning through electoral choice rather than losing it to a permanent court majority; they pay only the transaction cost of ongoing contestation. Citizens and excluded parties sit near the target end (d near 0.75) because the arrangement allocates constitutional interpretive authority away from them; they bear the cost of legal uncertainty and non-participation without collecting the benefit of being heard. The directionality overrides are unnecessary for this story: the structural data (beneficiary/victim declared for all institutional branches, constrained exit for all) already produces the right d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordinate construction reading resolves the mandatrophy question: Is this arrangement solving the founding problem it was built for? The founding problem was how to bind government to a constitutional text when no external arbiter can enforce the binding. Coordinate construction solves this by making each branch an internal check on the others' interpretive authority—if courts try to monopolize interpretation, legislatures can override through statute and amendment; if legislatures ignore constitutional limits, courts can refuse to enforce unconstitutional statutes and executives can refuse unconstitutional orders; if executives exceed constitutional scope, courts can enjoin and legislatures can restrict. The mandate is live, not dead—the founding problem persists in every generation because each generation must choose what the constitution means, and coordinate construction provides the institutional mechanism for that choice. Unlike judicial supremacy (which could become a dead mandate if courts never faced real resistance) or parliamentary primacy (which could become dead if the parliament became supreme de facto), coordinate construction's mandate stays live only as long as the branches actually coordinate and contest. If one branch achieves de facto supremacy, the mandate dies and the arrangement becomes an extraction mechanism (whichever branch is supreme extracts from the others). The measurement series does not show drift toward supremacy-by-one-branch; rather, it shows stabilization around the coordination equilibrium, so the mandate remains live at the interval's end.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_coordinate_authority,
    'When the three branches offer competing constitutional interpretations, what counts as ''coordinate'' rather than ''conflicting''? Is there a principled way to distinguish legitimate coordinate interpretation from simple inter-branch disagreement or institutional power struggle?',
    'Examination of historical periods of inter-branch constitutional contestation (e.g., Lincoln and Taney on executive war powers; FDR and the Court; current debates over executive privilege and legislative subpoena power) to discern whether disputants appeal to a shared coordinate-construction principle or whether they treat disagreement as a battle for supremacy.',
    'If the three branches do not appeal to a shared coordinate-construction principle but instead each claims supremacy and uses coercive power to prevail, the constraint collapses into a pure power struggle and reclassifies as a snare or tangled rope where institutional superiority determines outcome. If they do appeal to coordination and treat mutual resistance as legitimate, the coordinate-construction reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_coordinate_authority, conceptual, 'Whether coordinate authority is a real structural principle or a cover story for institutional power balance.').

omega_variable(
    sibling_reading_foreclosure_ambiguity,
    'Does the coordinate construction reading logically foreclose the judicial supremacy and parliamentary primacy readings, or do they coexist as live positions each branch could hold simultaneously?',
    'Logical analysis: Can a single institutional framework hold both ''coordinate authority'' and ''one branch is finally supreme''? Structural analysis: Do branches that claim coordinate authority also claim supremacy in crisis moments, suggesting the readings coexist pragmatically rather than logically?',
    'If the readings foreclose each other (logically incompatible), one constraint story should dominate and the others should be subsidiary or contextual. If they coexist (each is a live claim different seats hold), the network relationship should be coexists_with, and the three stories model a genuine constitutional contest rather than a settled hierarchy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_ambiguity, conceptual, 'Whether the coordinate construction reading is incompatible with judicial supremacy and parliamentary primacy, or whether all three readings remain live positions different parties hold simultaneously.').

omega_variable(
    extraction_beneficiary_ambiguity,
    'Who actually benefits from the coordinate construction arrangement? Is it genuinely symmetrical (all three branches benefit equally), or does one branch benefit more by appearing coordinate while gaining de facto supremacy?',
    'Measurement of institutional outcomes over time: which branch''s constitutional interpretations prevail in disputes? Which branch expands its power while invoking coordinate construction? Comparison of de facto vs. de jure authority.',
    'If one branch consistently prevails and coordinates construction is invoked by the subordinate branches as a way to legitimate their non-compliance, the beneficiary is asymmetric and the constraint reclassifies as a snare (with the subordinate branches as victims). If all three branches prevail in different domains and genuinely negotiate settlements, the symmetry holds and the rope/tangled rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_ambiguity, empirical, 'Whether coordinate construction produces symmetric inter-branch benefit or masks asymmetric supremacy.').

omega_variable(
    stability_through_suppression,
    'How much of the low suppression measurement (0.28) reflects genuine lack of coercive enforcement, and how much reflects the suppression cost of being discovered violating the coordinate-construction principle? Is the measured suppression honest, or is it a hidden bargain where branches suppress open claims to supremacy to maintain the coordinate appearance?',
    'Analysis of historical moments when branches explicitly claimed supremacy (Jackson on Indian removal, Lincoln on suspension of habeas corpus, FDR on court-packing, Nixon on executive privilege) and were resisted; investigation of whether branches acknowledge and constrain these supremacy claims or whether they reassert them later.',
    'If suppression is low because the principle is genuinely observed, the constraint works as designed. If it is low because branches suppress their supremacy claims to avoid backlash, the true suppression level is higher and the constraint is more extraction-like than the metric suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stability_through_suppression, empirical, 'Whether measured low suppression reflects institutional restraint or hidden coercion and strategic silence.').

omega_variable(
    excluded_citizens_reclassification,
    'From the standpoint of excluded voices (citizens, international legal order), is coordinate construction a coordinating mechanism or an extractive cartel? Do the three branches coordinate to exclude external voices, or does coordinate construction inherently exclude non-institutional actors?',
    'Comparison with constitutional orders that include citizen participation (e.g., direct democracy, amendment by popular initiative, citizen juries) to determine whether exclusion is structural to coordinate construction or incidental to this implementation.',
    'If exclusion is structural, coordinate construction is a snare for excluded voices even if it is a rope for the institutional branches—requiring a bifurcated classification. If exclusion is incidental, the constraint could be redesigned to include external voices without losing coordinate authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_citizens_reclassification, conceptual, 'Whether the exclusion of citizens from constitutional interpretation is intrinsic to coordinate construction or contingent to its implementation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cons_tr_t5, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(cons_tr_t10, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(cons_tr_t15, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(cons_tr_t25, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement(cons_tr_t40, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cons_be_t5, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(cons_be_t10, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(cons_be_t15, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(cons_be_t25, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 25, 0.41).
narrative_ontology:measurement(cons_be_t40, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(cons_su_t5, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement(cons_su_t10, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 10, 0.23).
narrative_ontology:measurement(cons_su_t15, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 15, 0.26).
narrative_ontology:measurement(cons_su_t25, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 25, 0.28).
narrative_ontology:measurement(cons_su_t40, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__coordinate_construction_reading, 0.18).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% The constitutional_authority_boundary kernel decomposes into three structurally distinct constraints, each a reading of the same contested text. This story (coordinate_construction_reading) models distributed authority with no final arbiter. The judicial_supremacy_reading models courts as final interpreters. The parliamentary_primacy_reading models legislatures as final. The three readings have different ε values, different beneficiary/victim structures, and different classification trajectories—they are not the same constraint viewed from different angles. The ε-invariance principle (DP-001) requires decomposition: measuring the constraint as 'coordinate' vs. 'supremacist' would yield different ε values from the same text, which signals two constraints, not one. Each reading is authored independently as a clean ε-invariant constraint and linked via network.affects_constraints to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
