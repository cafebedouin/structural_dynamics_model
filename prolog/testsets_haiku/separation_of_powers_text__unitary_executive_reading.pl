% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Doctrine and Independent Agency Constraint
 *   domain: constitutional_law/administrative_law/political_theory
 *
 * SUMMARY:
 *   The unitary executive reading of the separation of powers asserts that
 *   all executive power vests in the president, making independent agencies
 *   (FTC, NLRB, Federal Reserve, SEC) constitutional aberrations that violate
 *   the presidential vesting clause by operating outside presidential removal
 *   authority. Under this reading, Congressional statutes that restrict
 *   presidential removal of agency heads are unconstitutional, and agencies'
 *   claims to independence are invalid. This is one of three structurally
 *   distinct readings of the separation-of-powers kernel: the formalist
 *   reading grounds the constitutional problem in Congressional delegation of
 *   legislative power to agencies (not in presidential control); the
 *   functionalist reading permits overlapping authority and statutory removal
 *   restrictions as consistent with flexible separation of powers. The
 *   unitary reading differs from both: it locates constitutional authority in
 *   presidential personalism and makes any agency independence a violation.
 *   The constraint's claimed type (tangled rope) reflects the real
 *   coordination benefit of unified executive structure alongside the
 *   asymmetric extraction of agency independence. The measurement series
 *   track the doctrine's growing judicial enforcement and increasing
 *   suppression of agency statutory authority over the interval.
 *
 * KEY AGENTS:
 *   - executive_branch: claims all executive power vests in the president (institutional power, analytical exit)
 *   - independent_agencies: defend Congressional delegations and statutory removal restrictions (institutional power, identity-locked exit—institutional identity constituted by independence)
 *   - congress: delegates authority to agencies and restricts removal, but bears constitutional constraint on that delegation (institutional power, constrained exit)
 *   - judiciary: adjudicates removal power, delegation authority, and presidential control (observer seat, constitutional arbiter)
 *   - citizens: excluded from voice, rely on insulated agencies for labor, environmental, consumer enforcement (powerless, trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.68).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.72).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Doctrine and Independent Agency Constraint").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/administrative_law/political_theory").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, '7e6fc706-653f-40c1-a17f-7b3b21d1c19c').
narrative_ontology:cs_kernel_codification('7e6fc706-653f-40c1-a17f-7b3b21d1c19c', fixed_text).
narrative_ontology:cs_authority_grounding('7e6fc706-653f-40c1-a17f-7b3b21d1c19c', lineage).
narrative_ontology:cs_interpretation_layer_present('7e6fc706-653f-40c1-a17f-7b3b21d1c19c').
narrative_ontology:cs_reading_relation('7e6fc706-653f-40c1-a17f-7b3b21d1c19c', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e6fc706-653f-40c1-a17f-7b3b21d1c19c', separation_of_powers_text__functionalist_reading, forecloses).
narrative_ontology:cs_axiom('7e6fc706-653f-40c1-a17f-7b3b21d1c19c', foundational, executive_power_presidential_vesting).
narrative_ontology:cs_axiom_status(executive_power_presidential_vesting, holdable).
narrative_ontology:cs_axiom_grounding('7e6fc706-653f-40c1-a17f-7b3b21d1c19c', executive_power_presidential_vesting, empirically_contingent).
narrative_ontology:cs_axiom('7e6fc706-653f-40c1-a17f-7b3b21d1c19c', foundational, removal_authority_plenary).
narrative_ontology:cs_axiom_status(removal_authority_plenary, holdable).
narrative_ontology:cs_axiom_grounding('7e6fc706-653f-40c1-a17f-7b3b21d1c19c', removal_authority_plenary, deontological).
narrative_ontology:cs_reference_frame('7e6fc706-653f-40c1-a17f-7b3b21d1c19c', presidential_personalism_vesting).
narrative_ontology:cs_drift_state('7e6fc706-653f-40c1-a17f-7b3b21d1c19c', contemporary_administrative_state_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('7e6fc706-653f-40c1-a17f-7b3b21d1c19c', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_branch).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congress_delegated_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, congress).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The presidency and executive departments assert that all executive power vests in the president, that agency independence violates the Constitution, and that the president retains plenary removal authority over all executive subordinates. The executive branch sets the agenda by invoking the unitary doctrine in litigation, by claiming removal authority over agency heads, and by asserting that Congressional delegation is unconstitutional. This reading benefits the executive branch by consolidating power and eliminating constraints on presidential control.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_branch, agenda_setter,
    institutional, generational, analytical, national).

% The FTC, NLRB, Federal Reserve, SEC, and similar bodies are structured by Congress to operate independently from presidential removal authority. Under the unitary reading, their statutory independence is unconstitutional. Their institutional identity is constituted around independence (the Federal Reserve's monetary policy autonomy, the NLRB's labor-law neutrality, the FTC's consumer protection insulation from political pressure). Accepting presidential subordination means institutional dissolution or fundamental identity transformation. They cannot exit the executive branch without Congressional action the executive branch opposes.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agencies, payer,
    institutional, generational, identity_locked, national).

% Congress delegates rulemaking and enforcement authority to independent agencies and restricts presidential removal via statutory cause requirements and fixed terms of office. Congress thus benefits from having agencies execute policy while insulated from short-term political pressure. But under the unitary reading, Congress loses the constitutional authority to insulate agencies—the vesting clause bars Congress from restricting removal. Congress bears the cost of constitutional confinement: it retains nominal delegation authority but loses the ability to shield agencies from presidential control.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(separation_of_powers_text__unitary_executive_reading, congress, payer).

% Courts adjudicate removal authority, delegation validity, and the scope of presidential power. Under the unitary reading, the judiciary's role is to police Congressional incursions on executive power and to enforce presidential removal authority—but the judiciary cannot find Congressional delegation or removal restrictions constitutional. The judiciary observes from the constitutional arbiter seat but the reading constrains what the judiciary can lawfully hold.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Citizens depend on independent agencies to enforce labor law, environmental protection, consumer safety, and monetary stability insulated from short-term political swings. If the unitary reading eliminates agency independence, citizens lose insulated enforcement without having had voice in the constitutional debate. They are excluded from the dispute itself.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, citizens_reliant_on_agency_enforcement, excluded,
    powerless, biographical, trapped, national).

% Scholars and judges holding the formalist reading locate the constitutional problem not in presidential control but in Congressional delegation of legislative authority to agencies. They argue agencies violate separation of powers because they exercise law-making power Congress cannot delegate—not because they escape presidential control. They are excluded from the unitary doctrine dispute, which treats agency independence as the constitutional defect rather than agency legislative authority.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, formalist_constitutional_scholars, excluded,
    analytical, generational, analytical, national).

% Scholars and judges holding the functionalist reading argue the Constitution permits flexible separation of powers, overlapping authority, and Congressional delegation via intelligible principle. They see the unitary doctrine as an unnecessary hardening of the constitutional text and a misreading of the founders' design (which permitted distributed executive authority). They are excluded from the current dispute because the unitary reading frames agency independence itself as the constitutional defect.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, functionalist_constitutional_scholars, excluded,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__unitary_executive_reading, executive_branch).
narrative_ontology:fixing_cost_class(separation_of_powers_text__unitary_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of executive power through unified presidential authority. The unitary doctrine claims that a single executive chain of command (running from the president through all subordinates to the president) improves coordination of policy, eliminates conflicting directives, and ensures executive authority operates as a coherent whole rather than as fragmented agency fiefdoms.
% TRANSFER_FUNCTION: Transfers authority from independent agencies and from Congress's delegated power to the president. Independent agencies lose statutory insulation and must operate under presidential removal authority. Congress loses the constitutional ability to delegate to insulated bodies—the vesting clause, under this reading, bars any Congressional delegation that escapes presidential control.
% ABSENT_VOICES: Citizens who rely on insulated agencies for labor, environmental, and consumer enforcement; formalist scholars who see the constitutional problem as legislative delegation, not presidential control; functionalist scholars who see the Constitution as permitting flexible separation and Congressional delegation. None of these parties are seated in the unitary executive debate—it is conducted between the executive branch asserting the doctrine and independent agencies defending statutory autonomy.
% DISAPPEARANCE_RATIONALE: If the unitary executive constraint disappeared—if courts rejected the doctrine and affirmed Congressional authority to create insulated agencies with removal restrictions—the federal government would structurally rearrange. The FTC, NLRB, Federal Reserve, SEC, and other independent bodies would operate under statutory insulation from presidential removal. Monetary policy would remain insulated from short-term political pressure. Labor law enforcement would recover institutional independence from executive direction. Consumer protection would no longer be subject to presidential reorganization. The organization of the administrative state and the distribution of executive power would fundamentally change.
% FOUNDING_PROBLEM: The vesting clause creates ambiguity: does 'The executive Power shall be vested in a President' mean the president personally holds all executive authority, or does it establish an executive branch (headed by the president) within which authority may be distributed? The unitary reading answers: the president personally holds all executive power; any agency independence is unconstitutional.
% FOUNDING_PROBLEM_CORROBORATION: The executive branch asserts the founding problem is live and the unitary reading resolves it (see Justice Thomas's concurrence in Seila Law v. CFPB, 2020; DOJ Office of Legal Counsel memoranda; executive-branch litigation positions). Independent agencies and Congress contest the framing itself—they argue the founding problem does not exist and the vesting clause permits executive-branch distribution under Congressional authority. Functionalist constitutional scholars (outside the beneficiary set) argue the Constitution permits flexible separation and intelligent delegation; formalist scholars (also outside the beneficiary set) argue the problem is legislative delegation, not presidential control. No corroboration from the agencies themselves (which would lose authority); minimal corroboration from scholars outside the executive-branch beneficiary set. The corroboration gap is structural: the reading concentrates executive power, so only beneficiaries of that concentration affirm it.
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).

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
 *   Extractiveness rises from 0.48 to 0.68 over the interval as the executive branch's assertion of unitary doctrine gains judicial traction (Seila Law v. CFPB, 2020; Myers v. United States precedent intensification). The measurement data trace enforcement intensification: as courts accept the reading, agency statutory independence erodes, forcing agencies into presidential subordination. Suppression requirement climbs because the reading's persistence depends on actively suppressing Congressional statutes that restrict removal—the constraint is not naturally emergent but maintained by constitutional interpretation and presidential assertion backed by judicial review. Theater ratio rises moderately (0.25 to 0.41) because the coordination-of-unified-executive narrative remains legitimate while enforcement increasingly targets agency independence rather than genuine problems of coordination. The coercion grid shows the doctrine operates most strongly at the structural (constitutional framework) and organizational (agency-level) levels; class-level and individual-level resistance persists because citizens depend on agency enforcement insulated from short-term political pressure. Suppression is highest at structural and organizational levels (where Constitutional power vests) and lower at individual level (where citizens' actual reliance on agency enforcement creates resistance to the doctrine).
 *
 * PERSPECTIVAL GAP:
 *   From the executive seat: the unitary doctrine is a reading of what the Constitution's text actually says—presidential vesting is clear and agencies are aberrations to be corrected. From the agency seat: the doctrine is a power grab disguised as originalism; it erases 80+ years of administrative law and statutory delegation without changing a word of the Constitution. From Congress's seat: the doctrine constricts what Congress can constitutionally authorize; Congress loses the ability to insulate agencies even when it has good reasons (depoliticizing monetary policy, protecting consumer protection from partisan swings). From the judiciary's seat: the doctrine requires policing Congressional delegation and presidential removal authority but narrows judicial power to strike down executive actions (because all executive actions are presidential under this reading). From the citizen seat: the doctrine is invisible at the constitutional level but felt acutely in regulatory outcomes (labor enforcement, environmental protection, banking regulation). Each seat experiences the constraint differently because the reading's structural implications are genuinely asymmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch is the beneficiary: it consolidates power by claiming all executive authority must flow through the president and by asserting removal authority over agencies that tried to claim independence (d → 0.0, full beneficiary). Independent agencies are the victims: their statutory insulation erodes, their rulemaking authority becomes contingent on presidential approval, their institutional identity (constituted around independence) is threatened (d → 1.0, full target). Congress sits near symmetric but slightly toward victim: it nominally retains legislative power but loses ability to delegate to insulated agencies—it benefits from having agencies execute policy but loses constitutional authority to insulate those agencies from presidential removal (d ≈ 0.55, slightly target-biased). Citizens are structurally trapped but not formally seated: they benefit from independent agency enforcement but have no voice in the constitutional debate and no exit if the doctrine removes that enforcement mechanism (exit = identity-locked, because leaving the United States is the only exit from the constitutional order). The high suppression reflects that the doctrine's persistence requires actively subordinating agencies to presidential will through constitutional interpretation and Presidential assertion of removal authority—without that active enforcement, Congressional statutes and agency precedent would sustain independence.
 *
 * MANDATROPHY ANALYSIS:
 *   The unitary executive reading avoids mandatrophy by maintaining a plausible (though contested) justification: coordination of executive action through unified presidential authority. That justification is live—presidents do invoke it, and judges do accept it in some contexts. But the reading's operation shows signs of extraction beyond coordination: the suppression required to maintain it (actively limiting Congressional delegation, subordinating independent agency statutes to presidential will) exceeds what a pure coordination reading would require. The theater ratio (0.41 at interval end) suggests meaningful performative defense: the coordination narrative is maintained in rhetoric while enforcement increasingly targets agency independence specifically. The classification as tangled_rope (not pure snare) reflects this: there is real coordination benefit (unified executive command does solve some problems), but the extraction of agency independence is asymmetric and requires active enforcement. If judicial assertion of absolute removal authority continues without restraint, the constraint risks drifting toward snare territory, where the coordination benefit becomes vestigial and only extraction remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vesting_clause_ambiguity,
    'Does the vesting clause mean the president personally holds all executive power, or does it establish an executive branch (within which the president is chief) that may contain multiple bodies?',
    'Historical analysis of constitutional intent (founders'' debates, Federalist Papers, state constitutional precedents); parallel construction analysis (compare Legislative Power vests in Congress, Judicial Power vests in Supreme Court—do those mean bodies or individuals?); subsequent constitutional practice and ratification of amendments.',
    'If the clause means presidential personalism, the unitary reading is texturally sound and independent agencies are constitutional defects. If it means executive-branch structure, the clause permits (and may require) distributed executive authority subject to Congressional delegation within intelligible-principle limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vesting_clause_ambiguity, empirical, 'Whether vesting clause establishes presidential personalism or executive-branch structure.').

omega_variable(
    founding_function_obsolescence,
    'Was the founding problem the unitary reading solves—fragmented executive authority making presidential accountability impossible—actually live in 1787, or is it a retroactive problem identified by the reading itself?',
    'Historical records of executive operations under early presidents; contemporary commentary on whether fragmentation of authority was perceived as a constitutional problem; design of 1789 executive structure (cabinet, department heads, removal authority) and whether it treated agencies as presidential subordinates or independent.',
    'If the problem was live and the reading solves a genuine 1787 difficulty, unitary doctrine carries stronger foundational warrant. If the problem is retroactively constructed, the founding_problem_status = contested is justified and the doctrine is better read as solving modern political problems (concentrating executive authority in face of rising administrative state) rather than founding-era ones.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_function_obsolescence, empirical, 'Whether unitary doctrine solves a live founding-era problem or a retroactively constructed one.').

omega_variable(
    removal_authority_and_statutory_law_conflict,
    'Can a statute restrict presidential removal of an agency head if that statute is itself constitutional? That is, if Congress has enumerated power to create an agency and structure it, does that enumeration include authority to restrict the president''s removal power?',
    'Constitutional text analysis (Necessary and Proper Clause, enumerated legislative powers, vesting clause in relation to each other); case law trajectory (from Humphrey''s Executor v. FTC permitting removal restrictions, to modern unitary doctrine assertions that removal is plenary); structural constitutional theory (does legislative creation-and-structuring power include removal-restriction authority?).',
    'If removal-restriction statutes are themselves constitutionally authorized, then the unitary reading loses authority—the Constitution permits Congress to restrict removal while creating agencies. If removal is plenary presidential authority overriding any statute, then the unitary reading is sound but invalidates 80+ years of administrative law and statutory structure without textual amendment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(removal_authority_and_statutory_law_conflict, conceptual, 'Whether statutory removal restrictions are constitutionally valid when passed under enumerated Congressional authority.').

omega_variable(
    reading_vs_formalist_foreclosure,
    'Does the unitary reading foreclose the formalist reading within a single constitutional framework, or do they coexist as different party''s readings of the same text?',
    'Logical analysis of core premises: unitary doctrine says agencies violate the Constitution by escaping presidential control (even if Congress authorized the escape); formalist doctrine says agencies violate the Constitution by exercising legislative power Congress cannot delegate (regardless of presidential control). Do these premises rule each other out, or can they coexist?',
    'If they foreclose each other, the readings are in zero-sum competition and only one can be constitutionally sound. If they coexist, both can be live and different factions can hold different readings depending on their priority (whether the constitutional problem is presidential control or legislative delegation). The coexistence relation shapes the constraint-family network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_formalist_foreclosure, conceptual, 'Whether unitary and formalist readings of separation of powers are logically compatible or mutually exclusive.').

omega_variable(
    removal_authority_identity_lock_mechanism,
    'Is the independent agencies'' identity-locked exit truly irreversible, or can agency heads psychologically and institutionally accept presidential subordination without institutional dissolution?',
    'Post-removal-authority assertions: if courts enforce presidential removal authority and agencies accept subordination without formal dissolution, assess whether agencies continue to function as distinct entities or become mere presidential instruments. Track whether agency professional cultures, mission statements, and operational independence persist after removal authority is established.',
    'If identity-lock is absolute, then agency resistance to unitary doctrine will be persistent and structural (institutional self-preservation). If identity can accommodate presidential subordination, then agencies may adapt and resistance may decay over time as organizational identity shifts. This affects whether the constraint''s extraction persists as perceived subjective cost or becomes normalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(removal_authority_identity_lock_mechanism, empirical, 'Whether agency identity-lock to independence is psychologically irreversible or adaptable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t0, separation_of_powers_text__unitary_executive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(sepa_tr_t0, observed).
narrative_ontology:measurement(sepa_tr_t8, separation_of_powers_text__unitary_executive_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(sepa_tr_t8, observed).
narrative_ontology:measurement(sepa_tr_t16, separation_of_powers_text__unitary_executive_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement_basis(sepa_tr_t16, observed).
narrative_ontology:measurement(sepa_tr_t24, separation_of_powers_text__unitary_executive_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(sepa_tr_t24, observed).
narrative_ontology:measurement(sepa_tr_t32, separation_of_powers_text__unitary_executive_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement_basis(sepa_tr_t32, observed).
narrative_ontology:measurement(sepa_tr_t40, separation_of_powers_text__unitary_executive_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(sepa_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(sepa_be_t0, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(sepa_be_t0, observed).
narrative_ontology:measurement(sepa_be_t8, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 8, 0.55).
narrative_ontology:measurement_basis(sepa_be_t8, observed).
narrative_ontology:measurement(sepa_be_t16, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement_basis(sepa_be_t16, observed).
narrative_ontology:measurement(sepa_be_t24, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(sepa_be_t24, observed).
narrative_ontology:measurement(sepa_be_t32, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(sepa_be_t32, observed).
narrative_ontology:measurement(sepa_be_t40, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(sepa_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t0, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(sepa_su_t0, observed).
narrative_ontology:measurement(sepa_su_t8, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(sepa_su_t8, observed).
narrative_ontology:measurement(sepa_su_t16, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(sepa_su_t16, observed).
narrative_ontology:measurement(sepa_su_t24, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(sepa_su_t24, observed).
narrative_ontology:measurement(sepa_su_t32, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement_basis(sepa_su_t32, observed).
narrative_ontology:measurement(sepa_su_t40, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(sepa_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(sepa_grid_01, separation_of_powers_text__unitary_executive_reading, accessibility_collapse(class), 0, 0.35).
narrative_ontology:measurement(sepa_grid_02, separation_of_powers_text__unitary_executive_reading, accessibility_collapse(class), 40, 0.42).
narrative_ontology:measurement(sepa_grid_03, separation_of_powers_text__unitary_executive_reading, accessibility_collapse(individual), 0, 0.2).
narrative_ontology:measurement(sepa_grid_04, separation_of_powers_text__unitary_executive_reading, accessibility_collapse(individual), 40, 0.28).
narrative_ontology:measurement(sepa_grid_05, separation_of_powers_text__unitary_executive_reading, accessibility_collapse(organizational), 0, 0.55).
narrative_ontology:measurement(sepa_grid_06, separation_of_powers_text__unitary_executive_reading, accessibility_collapse(organizational), 40, 0.62).
narrative_ontology:measurement(sepa_grid_07, separation_of_powers_text__unitary_executive_reading, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(sepa_grid_08, separation_of_powers_text__unitary_executive_reading, accessibility_collapse(structural), 40, 0.65).
narrative_ontology:measurement(sepa_grid_09, separation_of_powers_text__unitary_executive_reading, resistance(class), 0, 0.5).
narrative_ontology:measurement(sepa_grid_10, separation_of_powers_text__unitary_executive_reading, resistance(class), 40, 0.45).
narrative_ontology:measurement(sepa_grid_11, separation_of_powers_text__unitary_executive_reading, resistance(individual), 0, 0.35).
narrative_ontology:measurement(sepa_grid_12, separation_of_powers_text__unitary_executive_reading, resistance(individual), 40, 0.32).
narrative_ontology:measurement(sepa_grid_13, separation_of_powers_text__unitary_executive_reading, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(sepa_grid_14, separation_of_powers_text__unitary_executive_reading, resistance(organizational), 40, 0.52).
narrative_ontology:measurement(sepa_grid_15, separation_of_powers_text__unitary_executive_reading, resistance(structural), 0, 0.55).
narrative_ontology:measurement(sepa_grid_16, separation_of_powers_text__unitary_executive_reading, resistance(structural), 40, 0.48).
narrative_ontology:measurement(sepa_grid_17, separation_of_powers_text__unitary_executive_reading, stakes_inflation(class), 0, 0.3).
narrative_ontology:measurement(sepa_grid_18, separation_of_powers_text__unitary_executive_reading, stakes_inflation(class), 40, 0.42).
narrative_ontology:measurement(sepa_grid_19, separation_of_powers_text__unitary_executive_reading, stakes_inflation(individual), 0, 0.18).
narrative_ontology:measurement(sepa_grid_20, separation_of_powers_text__unitary_executive_reading, stakes_inflation(individual), 40, 0.3).
narrative_ontology:measurement(sepa_grid_21, separation_of_powers_text__unitary_executive_reading, stakes_inflation(organizational), 0, 0.48).
narrative_ontology:measurement(sepa_grid_22, separation_of_powers_text__unitary_executive_reading, stakes_inflation(organizational), 40, 0.6).
narrative_ontology:measurement(sepa_grid_23, separation_of_powers_text__unitary_executive_reading, stakes_inflation(structural), 0, 0.52).
narrative_ontology:measurement(sepa_grid_24, separation_of_powers_text__unitary_executive_reading, stakes_inflation(structural), 40, 0.62).
narrative_ontology:measurement(sepa_grid_25, separation_of_powers_text__unitary_executive_reading, suppression(class), 0, 0.42).
narrative_ontology:measurement(sepa_grid_26, separation_of_powers_text__unitary_executive_reading, suppression(class), 40, 0.55).
narrative_ontology:measurement(sepa_grid_27, separation_of_powers_text__unitary_executive_reading, suppression(individual), 0, 0.25).
narrative_ontology:measurement(sepa_grid_28, separation_of_powers_text__unitary_executive_reading, suppression(individual), 40, 0.38).
narrative_ontology:measurement(sepa_grid_29, separation_of_powers_text__unitary_executive_reading, suppression(organizational), 0, 0.58).
narrative_ontology:measurement(sepa_grid_30, separation_of_powers_text__unitary_executive_reading, suppression(organizational), 40, 0.72).
narrative_ontology:measurement(sepa_grid_31, separation_of_powers_text__unitary_executive_reading, suppression(structural), 0, 0.62).
narrative_ontology:measurement(sepa_grid_32, separation_of_powers_text__unitary_executive_reading, suppression(structural), 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(separation_of_powers_text__unitary_executive_reading, 0.12).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, administrative_procedure_act__notice_and_comment).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, independent_agency_removal_restrictions__statutory_basis).

% DUAL FORMULATION NOTE:
% This is one reading of the separation-of-powers kernel. The formalist reading (in separate file separation_of_powers_text__formalist_reading) locates the constitutional problem in Congressional delegation of legislative authority, not in presidential control of agencies—ε values differ substantially. The functionalist reading (in separate file separation_of_powers_text__functionalist_reading) permits flexible separation of powers and Congressional delegation within intelligible-principle limits—also different ε. These three readings are distinct constraints with distinct classifications; they are linked as a constraint family via network.affects_constraints. The unitary reading INFLUENCES both sibling readings by shifting the legitimacy conditions under which they operate: if unitary doctrine prevails judicially, functionalist delegation becomes harder to justify and formalist doctrine gains comparative legitimacy (both oppose unitary personalism, though for different reasons).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(separation_of_powers_text__unitary_executive_reading, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
