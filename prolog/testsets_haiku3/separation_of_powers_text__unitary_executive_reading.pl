% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Reading: Presidential Control of All Executive Authority
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the UNITARY EXECUTIVE READING of the
 *   contested separation-of-powers kernel. The kernel itself (the
 *   constitutional text and structure governing executive power allocation)
 *   is fixed and shared across all readings. This reading asserts that
 *   Article II vesting clause language ('The executive Power shall be vested
 *   in a President') means all executive power flows through and under the
 *   President, making independent agencies with statutory removal protections
 *   unconstitutional infringements on Presidential authority. The reading
 *   benefits the Presidency and originalist legal scholars; it imposes costs
 *   on independent agencies forced to subordinate autonomous functions to
 *   Presidential direction, and on Congress, whose delegated authority is
 *   diminished. The constraint is a READING: other interpretations
 *   (functionalist and formalist) are different constraints drawn from the
 *   same kernel, authored in separate files, linked via
 *   network.affects_constraints. This file carries ONLY the unitary executive
 *   reading as a structurally complete, ε-invariant constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.68).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.71).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Reading: Presidential Control of All Executive Authority").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional/political").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, '88f76da2-a2b7-42c3-9780-a5f45e6786f6').
narrative_ontology:cs_kernel_codification('88f76da2-a2b7-42c3-9780-a5f45e6786f6', formalized).
narrative_ontology:cs_authority_grounding('88f76da2-a2b7-42c3-9780-a5f45e6786f6', lineage).
narrative_ontology:cs_interpretation_layer_present('88f76da2-a2b7-42c3-9780-a5f45e6786f6').
narrative_ontology:cs_reading_relation('88f76da2-a2b7-42c3-9780-a5f45e6786f6', separation_of_powers_text__formalist_reading, forecloses).
narrative_ontology:cs_reading_relation('88f76da2-a2b7-42c3-9780-a5f45e6786f6', separation_of_powers_text__functionalist_reading, coexists_with).
narrative_ontology:cs_axiom('88f76da2-a2b7-42c3-9780-a5f45e6786f6', foundational, presidential_supremacy_in_article_ii).
narrative_ontology:cs_axiom_status(presidential_supremacy_in_article_ii, holdable).
narrative_ontology:cs_axiom_grounding('88f76da2-a2b7-42c3-9780-a5f45e6786f6', presidential_supremacy_in_article_ii, empirically_contingent).
narrative_ontology:cs_axiom('88f76da2-a2b7-42c3-9780-a5f45e6786f6', foundational, removal_power_absolute).
narrative_ontology:cs_axiom_status(removal_power_absolute, holdable).
narrative_ontology:cs_axiom_grounding('88f76da2-a2b7-42c3-9780-a5f45e6786f6', removal_power_absolute, empirically_contingent).
narrative_ontology:cs_reference_frame('88f76da2-a2b7-42c3-9780-a5f45e6786f6', framers_executive_supremacy).
narrative_ontology:cs_drift_state('88f76da2-a2b7-42c3-9780-a5f45e6786f6', contemporary_administrative_state, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('88f76da2-a2b7-42c3-9780-a5f45e6786f6', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, presidency).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, agency_personnel).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, beneficiaries_of_agency_independence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, originalist_legal_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, the President claims absolute removal power over all executive-branch actors, including those in independent agencies with statutory removal protections. The reading consolidates executive authority into the President's hands by treating all Article II power as flowing from and subordinate to the chief executive. The President interprets the Constitution and enforces Presidential directives through appointments, removals, and operational control.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, presidency, agenda_setter,
    institutional, generational, analytical, national).

% Federal agencies statutorily insulated from at-will Presidential removal (Federal Reserve, FTC, NLRB, SEC, etc.) lose the independence Congress granted them under this reading. Their autonomy to set policy and enforce rules is subordinated to Presidential directives. Removal protections become nominal if the President asserts unitary executive authority and forces departures through political or institutional pressure.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agencies, payer,
    institutional, generational, trapped, national).

% Career civil servants and politically appointed agency heads operate under ambiguous authority: statutory duties direct them to act independently; Presidential assertions of unitary control threaten their positions if they refuse Presidential direction. The tension creates compliance pressure: follow the law as written or follow the President's reading of the Constitution. Career staff cannot easily exit the civil service.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, agency_personnel, payer,
    moderate, biographical, constrained, national).

% Congress ostensibly created independent agencies through statute to insulate technical expertise from political cycles. Under unitary executive reading, Congress's statutory authority to structure the executive branch is subordinate to the President's Article II interpretation. Congress benefits indirectly when the reading is NOT enforced (its delegation authority is vindicated), but under this reading Congress is the losing party in the institutional contest for authority allocation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, congress, observer).

% Courts sit as arbiters in this contest. The unitary executive reading asks courts to adopt a narrow view of constitutional interpretation (executive power as monolithic) while functionalist and formalist readings ask for broader authority to Congress and agencies. Courts that reject the reading validate agency independence; courts that accept it consolidate Presidential power. The judiciary's interpretation determines which reading governs.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Workers protected by labor agencies, consumers protected by regulatory agencies, citizens relying on Federal Reserve monetary independence, etc. — are nominally beneficiaries of agencies' independent authority. Under unitary executive reading, those agencies lose their insulation and become subject to Presidential directive, which may not prioritize the protection statutory authority mandates. This class is excluded from formal contest (they do not participate in constitutional interpretation) but would oppose the reading if empowered.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, beneficiaries_of_agency_independence, excluded,
    powerless, biographical, mobile, national).

% Legal scholars and judges committed to originalist constitutional interpretation find the unitary executive reading appeals to textual support (Article II vesting clause: 'The executive Power shall be vested in a President'). They benefit from this reading's vindication in case law and statutory interpretation; it provides intellectual scaffolding for their broader constitutional project. They can exit to other interpretive schools.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, originalist_legal_community, beneficiary,
    organized, generational, arbitrage, national).

% Legal scholars and judges committed to functionalist analysis (focus on systemic consequences rather than original text) oppose this reading as destabilizing administrative order and concentrating power dangerously. They argue constitutional separation of powers is permissive, not prescriptive, and should adapt to modern governance needs. They are a contending seat in the interpretive contest.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, functionalist_legal_community, observer,
    organized, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__unitary_executive_reading, presidency).
narrative_ontology:fixing_cost_class(separation_of_powers_text__unitary_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates executive-branch authority under a single accountable chief executive: the President stands as the unified representative of executive power, responsible to voters for all executive action. This reading claims to simplify accountability and prevent bureaucratic drift from presidential policy.
% TRANSFER_FUNCTION: Moves authority from independent agencies and Congress to the Presidency. Agencies lose autonomy over rulemaking and enforcement decisions; Congress loses its ability to insulate executive functions from Presidential control through statutory removal protections. That authority transfers to the President's discretionary power.
% ABSENT_VOICES: Labor unions, consumer advocates, workers protected by labor agencies, and monetary policy beneficiaries (those relying on Federal Reserve independence) would contest this reading but are structurally excluded from constitutional interpretation — their objections appear only through agency advocates and functionalist legal scholars, not as autonomous seats at the interpretive table.
% DISAPPEARANCE_RATIONALE: If this reading were definitively rejected and functionalist/formalist readings prevailed, independent agencies would retain statutory protections against at-will removal, and Congress's delegation authority would be revitalized. Decades of administrative law doctrine establishing agency independence would move from contested to settled; Presidential power over agencies would face judicial constraint. The institutional balance would shift decisively toward administrative autonomy and away from Presidential supremacy.
% FOUNDING_PROBLEM: How should executive power be allocated between the President, Congress, and technical agencies? The Constitution's text (Article II vesting clause) and structure (separation of powers) leave this ambiguous. The unitary executive reading resolves the ambiguity by privileging the President's textual claim to all executive power and reading agency independence as an unconstitutional infringement on that power.
% FOUNDING_PROBLEM_CORROBORATION: The reading is attested by originalist scholars (Calabresi, Lawson, Justice Scalia, Justice Thomas) and several Republican administrations in litigation positions. It is contended by functionalist scholars (Sunstein, Lessig), major law schools' administrative law casebooks, the Federal Reserve and independent agencies themselves, and Democratic administrations in litigation. Congress's century-long practice of creating independent agencies attests Congress's reading of its own constitutional authority; courts have repeatedly upheld agency independence (Humphrey's Executor, Wiener, Seila Law dissents). No unitary reading has achieved Supreme Court majority.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins moderate (0.48 at t=0) because the reading's claim is textual and legitimate within originalist interpretation — it rests on real constitutional language, not arbitrary assertion. By t=30 it reaches 0.68 because: (1) the reading requires active legal and political enforcement to override statutory agency protections, and (2) the beneficiaries (the Presidency, originalist scholars) are identifiable and concentrated, while the costs (agency autonomy loss, Congress's delegation authority diminishment) are distributed but real. Suppression mirrors extractiveness closely: the reading must suppress competing interpretations in courts and suppress agency resistance to Presidential directives. Theater is moderate and stable (0.28–0.42) because much of the reading's operation is formal constitutional argument conducted in courts and law reviews (genuine function), but an increasing share is political theater: administrations asserting the reading through removal threats and the press, symbolically invoking unitary executive language to justify policy outcomes that have no clear connection to constitutional structure. Resistance remains high (0.74) because agencies, Congress, functionalist scholars, and civil society organizations actively contest the reading; courts have not adopted it in toto.
 *
 * PERSPECTIVAL GAP:
 *   From the Presidential seat, the reading is a vindication of Article II's plain language and a restoration of proper constitutional balance against agency overreach; from the agency seat, the reading is a threat to congressionally granted autonomy and expert governance; from Congress's seat, it is an infringement on legislative authority to structure the executive; from the judiciary's seat (the interpretive arbiter), it is a coherent originalist account competing against functionalist and formalist accounts, none of which have definitively prevailed. The engine should compute radically different type-classifications for these seats: the Presidency will compute as a rope-beneficiary (genuine coordination of executive accountability plus extraction of agency autonomy); agencies will compute as tangled-rope-victims (the coordination function benefits Presidential accountability, but extraction of their autonomy is the primary effect); Congress will compute closer to snare (it loses authority with no compensating function). This divergence is the measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The Presidency sits at d ≈ 0.0–0.2 (full beneficiary): the reading consolidates authority and power. Independent agencies and agency personnel sit at d ≈ 0.8–1.0 (full targets): the reading strips their autonomous authority and subjects them to Presidential will. Congress sits at d ≈ 0.6–0.7 (asymmetric loss): Congress gains nothing from this reading and loses its authority to insulate executive functions through statute, but Congress is a powerful institutional seat with options (legislation, court appointments, public pressure). Agency beneficiaries (workers, consumers, citizens relying on Fed independence) sit at d ≈ 0.9 (targets through excluded mechanism): they lose the protective authority they nominally depend on, but they are excluded from formal interpretation, so their directionality is indirect. Functionalist and originalist legal communities sit near d ≈ 0.2–0.5 (beneficiaries for the originalsts, targets for functionalists) depending on whose interpretive frame prevails.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (How should executive power be allocated between President, Congress, and agencies?) remains live and contested. The reading has not achieved institutional settlement — courts have not embraced unitary executive theory in binding precedent; Congress has not repealed agency independence statutes in response; agencies continue to assert independence through litigation. Thus the constraint is NOT mandatrophic (the founding problem is not dead, and enforcement has not become entirely theatrical). However, mandatrophy pressure exists: if courts continue to reject unitary executive claims, the reading will face delegitimacy (founding problem declared dead by binding judicial authority); alternatively, if executive branches continue to assert the reading and remove agency leadership despite statutory protections, the constraint could shift to a piton-theater mode where the reading persists as political theater despite judicial rejection. The measurement trajectory (extractiveness plateauing after t=30, theater climbing slowly) suggests the reading's enforcement is hitting resistance ceiling — further Presidential assertion triggers court intervention, which limits enforcement depth.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_originalism_vs_structural_function,
    'Does Article II''s vesting of ''the executive Power'' in the President mean all executive authority must flow through the President''s discretion, or does structural analysis of separation of powers permit Congress to carve out independent agencies as long as the President retains supervisory authority?',
    'Supreme Court clarification through a test case on Presidential removal of an independent agency head — courts would have to choose between reading the vesting clause as textually absolute or structurally permissive.',
    'If the court rules the vesting clause is textually absolute, the unitary reading gains binding authority and independent agencies face constitutional vulnerability; if the court rules separation of powers is structurally permissive, the unitary reading is foreclosed and functionalist reading prevails.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_originalism_vs_structural_function, empirical, 'Whether Article II''s text or functional separation-of-powers structure controls constitutional interpretation of executive power.').

omega_variable(
    originalism_vs_living_constitution_meta,
    'Is originalist interpretation (fixing constitutional meaning at the Framers'' understanding) the legitimate mode for judicial review, or is it one interpretive method among several equally valid approaches (originalism, living constitutionalism, functionalism)?',
    'This is a meta-question about constitutional interpretation itself. It cannot be resolved empirically; resolution requires the federal judiciary to make an explicit choice about which interpretive methodology governs. The composition of the Supreme Court determines which methodology has institutional authority.',
    'If originalism becomes the Court''s dominant mode, unitary executive reading gains legitimacy; if the Court embraces pluralism or living constitutionalism, functionalist and formalist readings gain equal standing and the unitary reading faces interpretive competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_vs_living_constitution_meta, preference, 'The epistemological status of originalist interpretation — whether it is the correct method or one valid method among several.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of competing readings (functionalist and formalist) achieved through internal legal logic (originalists argue their reading is more faithful to the text) or through institutional power (originalist judges control courts and can impose their reading on lower courts)?',
    'Examine the reasoning of court decisions: do originalist opinions win through logical persuasion of previously skeptical judges, or do they win through voting coalitions of originalist justices outvoting functionalists? If the former, suppression is internalized (the logic is persuasive); if the latter, suppression is structural (it depends on Court composition).',
    'If suppression is internalized (legal logic), the unitary reading''s persistence is justified by genuine intellectual force and does not depend on institutional power — lower extractiveness and more stable classification. If suppression is structural (institutional power), the reading''s persistence depends on maintaining originalist control of courts — higher extractiveness, higher theater, vulnerability to composition change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, conceptual, 'Whether suppression of competing readings is achieved through legal logic or institutional power politics.').

omega_variable(
    kernel_identity_and_reading_stability,
    'Is the separation-of-powers kernel stable enough to support multiple competing readings, or is one reading''s acceptance logically incompatible with another''s?',
    'Historical and logical analysis: have the three readings (unitary, functionalist, formalist) coexisted in serious legal and political discourse, or is their coexistence temporary pending resolution?',
    'If the kernel is stable (as asserted by the coexists_with reading relations), the constraint remains live and contested indefinitely. If the readings are inherently unstable (one will eventually foreclose the others), the constraint is transitional and a terminal attractor exists (either unitary, functionalist, or formalist interpretation becomes law, eliminating the others).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_identity_and_reading_stability, conceptual, 'Whether the separation-of-powers kernel permits stable coexistence of three readings or requires eventual resolution into one dominant interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__unitary_executive_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sepa_tr_t5, separation_of_powers_text__unitary_executive_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(sepa_tr_t10, separation_of_powers_text__unitary_executive_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(sepa_tr_t15, separation_of_powers_text__unitary_executive_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(sepa_tr_t20, separation_of_powers_text__unitary_executive_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(sepa_tr_t25, separation_of_powers_text__unitary_executive_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(sepa_tr_t30, separation_of_powers_text__unitary_executive_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(sepa_tr_t40, separation_of_powers_text__unitary_executive_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sepa_be_t5, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(sepa_be_t10, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sepa_be_t15, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(sepa_be_t20, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(sepa_be_t25, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(sepa_be_t30, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(sepa_be_t40, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sepa_su_t5, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(sepa_su_t10, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(sepa_su_t15, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(sepa_su_t20, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement(sepa_su_t25, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement(sepa_su_t30, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(sepa_su_t40, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__unitary_executive_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__functionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of a three-reading kernel family centered on the separation-of-powers constitutional text. The unitary_executive_reading instantiates the constraint for the President's textual claim to plenary executive power. The formalist_reading instantiates the constraint for Congress's authority to enforce strict boundaries against agency independence. The functionalist_reading instantiates the constraint for permissive structural adaptation of separated powers. All three readings operate on the same underlying kernel (Article II vesting clause, institutional structure) but derive different structural relationships, beneficiary/victim sets, and classifications from that kernel. Each reading's ε is independent and fixed to its own understanding of the kernel's operation; the readings' mutual influence is modeled through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
