% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Separation of Powers Text: Unitary Executive Reading
 *   domain: constitutional_law/administrative_law/executive_power
 *
 * SUMMARY:
 *   The unitary executive reading of Article II vests all executive power in
 *   the President and treats independent agencies as constitutional anomalies
 *   that violate the principle that the President must control all executive
 *   action. This is one reading of the separation of powers text — contested,
 *   institutionally enforced, but not uncontested. The reading emphasizes
 *   textual commitment to a single locus of executive authority and derives
 *   removal power as an intrinsic executive prerogative. Under this reading,
 *   any agency with statutory removal protections violates the Constitution
 *   because it fragments executive power and shields officials from
 *   Presidential accountability. The constraint exhibits tangled rope
 *   structure: it coordinates executive branch hierarchy (genuine
 *   coordination function) while simultaneously extracting power from
 *   independent agencies, Congress, and the judiciary. The theater ratio has
 *   increased over the interval (0.35 → 0.55) as the doctrine's textual
 *   claims encounter mounting doctrinal complexity (Seila Law recognitions of
 *   limited exceptions, Collins v. Yellen's functional analysis). The
 *   suppression has hardened (0.55 → 0.72) as judicial commitment to removal
 *   power has intensified, making it increasingly difficult for Congress to
 *   create agency structures with removal protections. This is a kernel
 *   reading: the same constitutional text (Article II) is read by formalists
 *   to support unitary executive interpretation, by functionalists to permit
 *   independent agencies if they remain functionally accountable, and by
 *   originalists to require specific historical intent analysis. The unitary
 *   executive reading is one of these readings — structurally coherent but
 *   contested.
 *
 * KEY AGENTS:
 *   - The Presidency: Institutional beneficiary (institutional/arbitrage) — gains consolidated executive authority and absolute removal power; experiences the constraint as enabling coordination
 *   - Executive Branch Bureaucracy: Institutional beneficiary (institutional/arbitrage) — unified hierarchical control enables efficient policy coordination across agencies
 *   - Independent Agency Officials (FTC, NLRB, SEC, Fed): Primary victims (powerless/trapped) — face absolute removal power, eliminating structural independence; experience maximum suppression and extraction
 *   - Congress: Secondary victim (organized/constrained) — loses statutory authority to design agency structures with removal protections; experiences extraction of legislative power
 *   - The Judiciary: Tertiary victim (powerful/mobile) — review scope constrained by Presidential control of agency policy; pushed toward deference
 *   - Democratic Accountability Coalition: Organized resistance (organized/constrained) — labor unions, consumer advocates, progressive legal scholars; view unitary executive as temporary doctrine with sunset via new jurisprudence
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional preference as constitutional necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.58).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.72).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Separation of Powers Text: Unitary Executive Reading").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/administrative_law/executive_power").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, '95dc49f0-5e6c-403b-afd5-e374cde8112a').
narrative_ontology:cs_kernel_codification('95dc49f0-5e6c-403b-afd5-e374cde8112a', fixed_text).
narrative_ontology:cs_authority_grounding('95dc49f0-5e6c-403b-afd5-e374cde8112a', lineage).
narrative_ontology:cs_interpretation_layer_present('95dc49f0-5e6c-403b-afd5-e374cde8112a').
narrative_ontology:cs_reading_relation('95dc49f0-5e6c-403b-afd5-e374cde8112a', separation_of_powers_text__formalist_reading, forecloses).
narrative_ontology:cs_reading_relation('95dc49f0-5e6c-403b-afd5-e374cde8112a', separation_of_powers_text__functionalist_reading, coexists_with).
narrative_ontology:cs_axiom('95dc49f0-5e6c-403b-afd5-e374cde8112a', foundational, all_executive_power_individual_vesting).
narrative_ontology:cs_axiom_status(all_executive_power_individual_vesting, holdable).
narrative_ontology:cs_axiom_grounding('95dc49f0-5e6c-403b-afd5-e374cde8112a', all_executive_power_individual_vesting, deontological).
narrative_ontology:cs_axiom('95dc49f0-5e6c-403b-afd5-e374cde8112a', foundational, removal_power_intrinsic_executive_prerogative).
narrative_ontology:cs_axiom_status(removal_power_intrinsic_executive_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('95dc49f0-5e6c-403b-afd5-e374cde8112a', removal_power_intrinsic_executive_prerogative, deontological).
narrative_ontology:cs_reference_frame('95dc49f0-5e6c-403b-afd5-e374cde8112a', constitutional_text_as_unified_executive_authority).
narrative_ontology:cs_drift_state('95dc49f0-5e6c-403b-afd5-e374cde8112a', contemporary_post_seila_law, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('95dc49f0-5e6c-403b-afd5-e374cde8112a', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_branch_power).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, presidential_removal_authority).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agency_autonomy).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congressional_oversight_capacity).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, judicial_review_scope).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT AGENCY OFFICIALS (SNARE) — Trapped by absolute removal power; no exit from presidential control. The unitary executive reading eliminates the structural independence that agencies claim. Officials face maximal extraction: they must obey or be fired, with no intermediate protections. Congressional statutes creating removal protections are void under this reading. Trapped/powerless → maximum experienced extraction.
constraint_indexing:constraint_classification(separation_of_powers_text__unitary_executive_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESS (SNARE, ORGANIZED) — Cannot exit the constraint without constitutional amendment. The unitary executive reading voids statutory removal protections and limits Congressional ability to insulate agencies. Congress experiences extraction of its power to structure executive administration. High suppression: Congressional authority over agency design is preempted by constitutional text as read. However, Congress retains organized power through appropriations and new legislation — constrained exit, not trapped exit. Still snare because suppression is severe and extraction mechanism is structural.
constraint_indexing:constraint_classification(separation_of_powers_text__unitary_executive_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PRESIDENCY (TANGLED ROPE) — Coordinates executive branch action (genuine coordination function) AND extracts power from agencies and Congressional oversight. Experiences the constraint as beneficial rope — unitary authority enables coherent policy execution. But the reading ALSO involves extraction from other branches: Congressional removal protections are overridden, judicial review of agency decisions is constrained by presidential control. Arbitrage exit + beneficiary status → low effective extraction from the presidency's perspective, but the constraint IS extractive for others. The presidency experiences coordination; the constraint structure involves asymmetric extraction.
constraint_indexing:constraint_classification(separation_of_powers_text__unitary_executive_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE JUDICIARY (SNARE) — Can escape through constitutional interpretation (mobile exit via overruling prior precedent), but does not. The unitary executive reading constrains judicial review of agency action by making the President the ultimate arbiter of agency policy. Courts are pushed toward deference when agency decisions are clearly presidential directives. Powerful/mobile but experiencing suppression through preemption of review scope. The constraint extracts judicial authority and replaces it with Presidential will.
constraint_indexing:constraint_classification(separation_of_powers_text__unitary_executive_reading, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE EXECUTIVE BRANCH BUREAUCRACY (ROPE) — Coordinates vast administrative apparatus through hierarchical command. Unitary executive principle enables efficient policy coordination across agencies. From the bureaucracy's perspective, this is pure coordination — directing action, allocating resources, enforcing policy. The beneficiary from this reading is clear. Low extraction experienced because the constraint solves a genuine coordination problem: how to make thousands of administrative actors move in concert. Institutional/arbitrage → negative effective extraction.
constraint_indexing:constraint_classification(separation_of_powers_text__unitary_executive_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DEMOCRATIC ACCOUNTABILITY COALITION (SCAFFOLD) — Sees the unitary executive as a temporary constitutional theory with a sunset: newer Appointments Clause jurisprudence (Seila Law, Collins v. Yellen) has begun carving exceptions, and Congressional pushback through new statutes creates friction. The coalition is constrained but organized; it views the unitary executive not as permanent constitutional law but as a doctrine in retreat. Theater ratio is moderate because the doctrine claims textual grounding but faces mounting structural pressure from competing constitutional principles. Scaffold logic: temporary constraint with exit path via doctrine shift + legislative response.
constraint_indexing:constraint_classification(separation_of_powers_text__unitary_executive_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — The unitary executive principle appears as a direct reading of Article II text: 'The executive Power shall be vested in a President.' This perspective sees the constraint as following necessarily from the constitutional text itself — an immutable consequence of the Framers' structural choice to vest all executive power in one person. The text permits no independent agency authority; therefore, independence violates constitutional law. This is the mountain reading: the constraint is natural law derived from authoritative text. However, this classification is a FALSE SUMMIT: the text is contested (functionalists and formalists read the same text differently), beneficiaries exist (the Presidency, executive branch efficiency), and the constraint's enforcement depends on judicial acceptance of this particular interpretation. The mountain framing naturalizes a reading, not a law.
constraint_indexing:constraint_classification(separation_of_powers_text__unitary_executive_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(separation_of_powers_text__unitary_executive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(separation_of_powers_text__unitary_executive_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The unitary executive reading extracts power from agencies (removing removal protections), Congress (voiding statutes structuring agency independence), and the judiciary (constraining review scope). But the extraction is not maximal (0.70+) because: (1) The reading claims textual grounding, not pure power assertion; (2) it solves a genuine coordination problem (hierarchical executive control); (3) agencies retain functional authority within Presidential control. The measurement trajectory (0.42 → 0.58) reflects increasing judicial acceptance of the reading post-Seila Law. Suppression (0.72): High. The constraint's suppressive force operates through: (1) Removal power: officials cannot resist Presidential direction; (2) statutory preemption: Congress cannot protect agency independence; (3) doctrinal narrowing: judicial review is constrained. The rising suppression (0.55 → 0.72) over the measurement interval reflects hardening doctrinal commitment to removal power through Supreme Court precedent. Theater ratio (0.48): Moderate. The doctrine claims direct textual grounding ('executive Power vested in a President'), which reduces performative content compared to doctrines requiring elaborate inference. However, the reading encounters mounting interpretive complexity (Seila Law carved exceptions for solo directors; Collins applied functional analysis). The rising theater (0.35 → 0.55) reflects accumulating doctrinal exceptions and functional tests that dilute the textual clarity the reading initially claimed.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a severe perspectival gap rooted in directionality. The Presidency and executive bureaucracy see coordination (Rope from institutional/arbitrage perspectives) — the constraint enables unified policy execution. Independent agency officials see extraction and trap (Snare from powerless/trapped perspectives) — removal power leaves no escape. Congress sees power extraction (Snare from organized/constrained) — its authority to structure agencies is overridden. The judiciary sees scope constraint (Snare from powerful/mobile) — removal power narrows review scope. The analytical observer risks seeing immutable constitutional law (Mountain) — Article II vesting clause appears to necessitate the reading. But the false summit detection fires because identifiable beneficiaries exist (the Presidency, executive efficiency) and the reading is contested (functionalists and formalists reject it). The perspectival gap reveals that what the Presidency experiences as coordination feels like extraction to everyone else.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality differs sharply across beneficiaries and victims. The Presidency experiences d ≈ 0.15 (clear beneficiary with arbitrage exit — the President can always invoke unitary executive authority). Congressional and agency perspectives experience d ≈ 0.85 (clear victims with constrained exit — Congress cannot escape without new statutes that respect removal power; agencies cannot resist Presidential removal). The judicial perspective experiences d ≈ 0.70 (powerful but mobile, yet constrained by doctrinal commitment to defer to Presidential authority). These directionality differences are not overridden; they reflect actual structural relationships. The presidential perspective sees f(d ≈ 0.15) ≈ -0.01 (negative effective extraction — the constraint subsidizes the Presidency). Agency perspectives see f(d ≈ 0.85) ≈ 1.15 (high effective extraction — the constraint targets them). The scope modifier σ(national) = 1.0 does not amplify or dampen; the constraint operates at national scale. The chi formula resolves: χ_president ≈ 0.58 × (-0.01) × 1.0 ≈ -0.006 (President experiences this as coordination/rope), χ_agency ≈ 0.58 × 1.15 × 1.0 ≈ 0.67 (agencies experience this as extraction/snare).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that it is simultaneously a coordination mechanism (for the Presidency and executive bureaucracy) and an extraction mechanism (for agencies, Congress, and the judiciary). The constraint is tangled rope precisely because it performs BOTH functions: it coordinates executive action and extracts power asymmetrically. The mandatrophy question — 'Is this coordination or extraction?' — has a context-dependent answer: from the Presidency's vantage point, it is coordination; from the agency's vantage point, it is extraction. The framework does not falsely collapse this into a single type. Instead, it models the presheaf of perspectives and identifies the directionality gap as the source of the classification variance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_versus_structure_ambiguity,
    'Does Article II''s vesting of ''the executive Power'' in the President logically entail that independent agencies are unconstitutional, or does the Necessary and Proper Clause permit Congressional creation of agencies subject to Presidential removal?',
    'Textual analysis of competing constitutional principles (vesting vs. Necessary and Proper); historical intent documentation from Framers'' debates and Federalist Papers; precedent analysis (Myers, Humphrey''s Executor, Free Enterprise Fund, Seila Law)',
    'If vesting clause is dispositive: unitary executive reading is correct, and independent agencies violate the Constitution. If Necessary and Proper permits exceptions: unitary executive reading is one reading among competing ones, and the constraint''s classification shifts from Mountain to Tangled Rope across perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(text_versus_structure_ambiguity, conceptual, 'Whether Article II vesting clause logically forecloses Congressional agency design choices').

omega_variable(
    removal_power_boundary,
    'What counts as ''executive Power''? Is the power to remove officers an intrinsic executive power, or is it a power that Congress can limit by statute?',
    'Supreme Court precedent evolution: Myers (1926) held removal is inherent executive power; Humphrey''s Executor (1935) carved exception for ''quasi-legislative'' and ''quasi-judicial'' agencies; Free Enterprise Fund (2010) permitted multi-layer removal protection; Seila Law (2020) struck down solo-director agency with removal protection. Determine whether a stable doctrine has emerged or whether the boundary remains contested.',
    'If removal is inherent: unitary executive reading is Supreme Court doctrine, and independent agencies are unconstitutional under current law. If removal can be limited by statute: the constraint is a reading in active doctrinal contest, not settled constitutional law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(removal_power_boundary, empirical, 'Supreme Court doctrine on Presidential removal power').

omega_variable(
    functional_versus_formalist_reading,
    'Does the constitutional test for executive power structure depend on formal hierarchical control (unitary executive = formalism) or on functional accountability to the President (functionalism permits more independence if President retains effective policy authority)?',
    'Doctrinal analysis of Supreme Court opinions adopting either framework; empirical assessment of whether agencies without formal removal protections actually follow Presidential directives via appointments, budgets, and regulatory review',
    'If formalism prevails: unitary executive reading is enforced. If functionalism prevails: independent agencies can exist if they are de facto controlled by the President. The constraint''s extractiveness and suppression values depend on which framework the judiciary adopts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_versus_formalist_reading, conceptual, 'Formalist vs. functionalist doctrine in executive power jurisprudence').

omega_variable(
    beneficiary_constituency_ambiguity,
    'Who benefits from the unitary executive reading? Is it the President as individual, the Executive Office of the President, the executive branch bureaucracy, or a particular political coalition?',
    'Historical analysis of who invoked unitary executive doctrine and when; correlation of doctrine advocacy with partisan interests; empirical assessment of agency behavior under Presidents with different unitary executive commitments',
    'If the beneficiary is the Presidency institution: the constraint has stable beneficiary structure. If the beneficiary is a political coalition: the constraint is partisan and extraction is contingent on political alignment. If the beneficiary is bureaucratic efficiency: the constraint may dissolve if alternative administrative structures prove more efficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_constituency_ambiguity, empirical, 'Identification of actual beneficiaries from unitary executive doctrine').

omega_variable(
    foundational_premise_naturalization,
    'Is the unitary executive reading a valid constitutional interpretation, or does it involve naturalizing a particular institutional design choice as if it were a logical consequence of the text?',
    'Comparative constitutional analysis: how do other nations structure executive power? Historical analysis: did the Framers intend the unitary executive reading, or did they permit independent agencies? Functional analysis: do alternative interpretations (formalist, functionalist) provide equally coherent readings of the same text?',
    'If the reading is a valid constitutional interpretation: the constraint is Mountain (natural law from the text). If the reading naturalizes a design choice: the constraint is a false summit, and the mountain classification masks a tangled rope or snare reading. This omega documents whether the analytical observer''s mountain is genuine or naturalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foundational_premise_naturalization, conceptual, 'Whether unitary executive reading is constitutional law or naturalized institutional preference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(separ_unitary_tr_t0, separation_of_powers_text__unitary_executive_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(separ_unitary_tr_t25, separation_of_powers_text__unitary_executive_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(separ_unitary_tr_t50, separation_of_powers_text__unitary_executive_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(separ_unitary_be_t0, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(separ_unitary_be_t25, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(separ_unitary_be_t50, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(separ_unitary_su_t0, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(separ_unitary_su_t25, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(separ_unitary_su_t50, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__formalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text__functionalist_reading).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, independent_agency_structural_legitimacy).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, congressional_capacity_for_agency_design).

% DUAL FORMULATION NOTE:
% The unitary_executive_reading is one reading of the separation_of_powers_text kernel. The formalist_reading and functionalist_reading are sibling readings stored as separate constraint stories with different ε values reflecting different empirical contentions about textual meaning and constitutional necessity. The unitary_executive_reading (ε=0.58) claims that independent agencies violate the text; the functionalist_reading (expected ε≈0.35) claims that functionally responsive agencies respect the text; the formalist_reading (expected ε≈0.45) claims that Congress can structure agencies consistently with Presidential accountability. Each reading has its own perspectives and measurements reflecting how agents experience that specific interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
