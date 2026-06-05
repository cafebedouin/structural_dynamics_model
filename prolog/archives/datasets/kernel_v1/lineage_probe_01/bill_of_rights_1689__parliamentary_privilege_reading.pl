% ============================================================================
% CONSTRAINT STORY: bill_of_rights_1689__parliamentary_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bill_of_rights_1689__parliamentary_privilege_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: bill_of_rights_1689__parliamentary_privilege_reading
 *   human_readable: Parliamentary Privilege under the Bill of Rights 1689: Article 9 Reading
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested Bill of Rights
 *   1689 kernel: the parliamentary privilege reading. The kernel itself — the
 *   Bill of Rights 1689 — carries multiple structural interpretations held by
 *   different political and jurisprudential communities. This reading
 *   emphasizes Article 9 (proceedings in Parliament may not be impeached in
 *   any court) as the living constitutional core, establishing legislative
 *   immunity from judicial interference as the foundation of parliamentary
 *   free speech. The reading is coherent, historically grounded, and
 *   continues to structure English law. It is NOT the only reading — the
 *   anti_catholic_settlement reading emphasizes the Bill's confessional
 *   exclusions and succession clauses; the proto_rights_charter reading
 *   emphasizes its role as ancestor to modern rights charters. These are
 *   separate constraints (separate JSON files), linked via the network. This
 *   file generates the parliamentary privilege reading alone. The constraint
 *   exhibits tangled_rope structure: genuine coordination function
 *   (protecting parliamentary deliberation from legal suppression) combined
 *   with asymmetric extraction (injured parties have no judicial remedy,
 *   false information can propagate with immunity). The suppression of
 *   judicial reach into Parliament (0.68) is enforced through constitutional
 *   custom and explicit doctrinal prohibition. The extractiveness (0.52)
 *   reflects that while the coordination benefit is real, the cost to
 *   remedial access and information reliability is also real and
 *   uncompensated.
 *
 * KEY AGENTS:
 *   - Members of Parliament: Primary beneficiary (institutional/arbitrage) — benefit from absolute immunity; experience the constraint as protection for candid speech
 *   - Injured Litigants: Primary victim (powerless/trapped) — defamed or slandered by false parliamentary statements; barred from judicial remedy by the privilege
 *   - The Judiciary: Secondary institutional actor (institutional/constrained) — required to enforce dismissal of defamation suits; structurally excluded from adjudicating tort claims within the privileged domain
 *   - Parliamentary Deliberation (abstract): Coordination target (institutional/analytical) — the public good being protected; benefits from candor, harmed by fear of suit
 *   - Judicial Oversight Capacity: Secondary victim (institutional/constrained) — the judiciary's authority over tort law and defamation is structurally foreclosed in a key domain
 *   - Civil Society / Press: Organized beneficiary (organized/constrained) — benefits from parliamentary immunity protecting scrutiny; also constrained by immunity that allows false statements to propagate unchecked
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bill_of_rights_1689__parliamentary_privilege_reading, 0.52).
domain_priors:suppression_score(bill_of_rights_1689__parliamentary_privilege_reading, 0.68).
domain_priors:theater_ratio(bill_of_rights_1689__parliamentary_privilege_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bill_of_rights_1689__parliamentary_privilege_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(bill_of_rights_1689__parliamentary_privilege_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(bill_of_rights_1689__parliamentary_privilege_reading, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bill_of_rights_1689__parliamentary_privilege_reading, tangled_rope).
narrative_ontology:human_readable(bill_of_rights_1689__parliamentary_privilege_reading, "Parliamentary Privilege under the Bill of Rights 1689: Article 9 Reading").
narrative_ontology:topic_domain(bill_of_rights_1689__parliamentary_privilege_reading, "legal/doctrinal/constitutional").

domain_priors:requires_active_enforcement(bill_of_rights_1689__parliamentary_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bill_of_rights_1689__parliamentary_privilege_reading, 'e9478127-5718-422f-b86c-f2b60c3b3b5d').
narrative_ontology:cs_kernel_codification('e9478127-5718-422f-b86c-f2b60c3b3b5d', formalized).
narrative_ontology:cs_authority_grounding('e9478127-5718-422f-b86c-f2b60c3b3b5d', lineage).
narrative_ontology:cs_interpretation_layer_present('e9478127-5718-422f-b86c-f2b60c3b3b5d').
narrative_ontology:cs_reading_relation('e9478127-5718-422f-b86c-f2b60c3b3b5d', bill_of_rights_1689__anti_catholic_settlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9478127-5718-422f-b86c-f2b60c3b3b5d', bill_of_rights_1689__proto_rights_charter_reading, influences).
narrative_ontology:cs_axiom('e9478127-5718-422f-b86c-f2b60c3b3b5d', foundational, parliamentary_immunity_structural_necessity).
narrative_ontology:cs_axiom_status(parliamentary_immunity_structural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('e9478127-5718-422f-b86c-f2b60c3b3b5d', parliamentary_immunity_structural_necessity, instrumental).
narrative_ontology:cs_axiom('e9478127-5718-422f-b86c-f2b60c3b3b5d', foundational, judicial_sovereignty_bounded_by_privilege).
narrative_ontology:cs_axiom_status(judicial_sovereignty_bounded_by_privilege, holdable).
narrative_ontology:cs_axiom_grounding('e9478127-5718-422f-b86c-f2b60c3b3b5d', judicial_sovereignty_bounded_by_privilege, deontological).
narrative_ontology:cs_reference_frame('e9478127-5718-422f-b86c-f2b60c3b3b5d', parliamentary_immunity_framework).
narrative_ontology:cs_drift_state('e9478127-5718-422f-b86c-f2b60c3b3b5d', contemporary_expanded_parliamentary_reach, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e9478127-5718-422f-b86c-f2b60c3b3b5d', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(bill_of_rights_1689__parliamentary_privilege_reading, bill_of_rights_1689).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bill_of_rights_1689__parliamentary_privilege_reading, members_of_parliament).
narrative_ontology:constraint_beneficiary(bill_of_rights_1689__parliamentary_privilege_reading, parliamentary_deliberation).
narrative_ontology:constraint_victim(bill_of_rights_1689__parliamentary_privilege_reading, litigants_injured_by_privileged_speech).
narrative_ontology:constraint_victim(bill_of_rights_1689__parliamentary_privilege_reading, judicial_oversight_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INJURED LITIGANT (SNARE) — A citizen defamed, slandered, or injured by false statements made in parliamentary proceedings has no remedy in law. The court must refuse to hear the case; the MP's utterance is absolutely privileged. The injured party is structurally trapped: the barrier to exit (judicial remedy) is total and enforced by law itself. Maximum experienced extraction — the suppression is not economic cost but legal prohibition of redress.
constraint_indexing:constraint_classification(bill_of_rights_1689__parliamentary_privilege_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: JUDICIARY (TANGLED ROPE) — Courts have genuine coordination function: maintaining parliamentary privilege requires active enforcement of dismissal procedures and damage limitation. Judges coordinate this enforcement. But the constraint also extracts from judicial authority: courts are structurally prevented from exercising core functions (tort remedies, defamation adjudication) within a defined domain. Exit costs are high (overturning constitutional settlement) but not impossible. Moderate extraction with real coordination burden.
constraint_indexing:constraint_classification(bill_of_rights_1689__parliamentary_privilege_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEMBERS OF PARLIAMENT (ROPE) — The constraint exists precisely to serve this agent's coordination function: enable candid deliberation without fear of civil suit. MPs benefit from the privilege without bearing its costs. The constraint solves the genuine coordination problem of how to ensure free speech in parliamentary proceedings. Net beneficiary — extraction flows toward them, not away. The privilege is experienced as protection, not suppression.
constraint_indexing:constraint_classification(bill_of_rights_1689__parliamentary_privilege_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIARY INSTITUTION (TANGLED ROPE, GENERATIONAL) — Over generations, the judiciary experiences the constraint as both coordination mechanism and extraction. Coordination: the privilege is a constitutional boundary that allows courts to develop other doctrines (judicial independence, common law rights) without parliamentary encroachment — a reciprocal limit. Extraction: the judiciary's authority over defamation, tort, and civil remedy is structurally foreclosed in a key domain. This is the institutional reading of judicial constraint — the boundary is enforced through deference norms and constitutional custom, requiring active institutional maintenance.
constraint_indexing:constraint_classification(bill_of_rights_1689__parliamentary_privilege_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CIVIL SOCIETY / PRESS (ROPE) — Media organizations and civil society benefit from the principle of parliamentary privilege: it protects scrutiny of government within debate without fear of suit. But organized media also experience suppression: if a newspaper quotes an MP's false claim verbatim from Hansard, the paper cannot be sued for defamation. The benefit is coordination (parliamentary scrutiny enabled), but the cost is real — false information can propagate through the privilege. Organized actors have some agency to seek legislative change; thus constrained rather than trapped.
constraint_indexing:constraint_classification(bill_of_rights_1689__parliamentary_privilege_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The constraint is a genuine hybrid: the coordination function (protecting deliberation from legal threat) is real and produces measurable benefit (candid debate). But extraction is also real and produces measurable harm (injured parties have no remedy, false information persists). The claim that this is 'natural law' or 'necessary immunity' naturalizes a contingent institutional choice with genuine trade-offs. The engine will detect this as a false summit if Article 9 is classified as a mountain; the analytical perspective confirms the tangled_rope classification across the full span of deliberation vs. remedial access.
constraint_indexing:constraint_classification(bill_of_rights_1689__parliamentary_privilege_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bill_of_rights_1689__parliamentary_privilege_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bill_of_rights_1689__parliamentary_privilege_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bill_of_rights_1689__parliamentary_privilege_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bill_of_rights_1689__parliamentary_privilege_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bill_of_rights_1689__parliamentary_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The privilege provides genuine coordination benefit (MPs can speak without fear of suit), but the cost is substantial and irreversible — injured parties have literally no remedy at law. The metric reflects that suppression is not accidental but intentional: the law explicitly forbids courts from hearing these cases. The upward drift from 0.38 (1689) to 0.52 (2026) reflects that as communication technology expanded (newspapers, broadcast, social media), the scope of potential injury expanded, making the immunity's cost higher in modern context. Suppression (0.68): High. The mechanism is enforcement of a constitutional boundary — courts are actively required to dismiss suits, and parliamentary privilege is invoked as an absolute bar. The suppression is not structural accident but doctrinal mandate. Minimal drift (0.62 → 0.68) reflects that enforcement mechanisms have remained stable, though the domain of privileged utterance has expanded with parliamentary reach into media and commentary. Theater ratio (0.38): Moderate. The constraint has functional content: it does protect candid deliberation, and this function is real. But increasing theater over time (0.28 → 0.38) reflects that modern parliamentary statements are strategically framed for media distribution, not internal deliberation — the utterance is made for the privilege's coverage, not despite it. Theater remains below piton threshold (0.70) because the coordination function is still primary.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence. The beneficiary (MPs) sees rope: the privilege exists to protect their deliberation, and they experience it as enabling coordination. The injured litigant sees snare: absolute, enforced by law, no exit. The judiciary sees tangled rope across both biographical and generational timescales: at biographical level (immediate dismissal of suits), the constraint is an extraction mechanism preventing judicial function. At generational level, the judiciary understands the privilege as reciprocal — it defines a constitutional boundary that allows courts to develop other doctrines (judicial independence, separation of powers) without parliamentary encroachment. The civil society organization sees rope: the privilege enables parliamentary scrutiny of government without legal threat. The analytical observer sees tangled rope: genuine coordination function (candid debate) and genuine extraction (foreclosed remedies) coexist without resolution. The perspectival gap reveals the constraint's true structure: it is not a natural law of parliamentary function (mountain) but a negotiated boundary with real winners (MPs, parliamentary scrutiny) and real losers (injured litigants, judicial authority).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position. MPs as beneficiaries with arbitrage options (can lobby to modify the privilege if desired, operate at institutional power) have low d values — the constraint runs toward them, not away. Injured litigants as victims with trapped exit (no legal remedy exists, court is prohibited from hearing suit) have high d values — maximum experienced extraction. The judiciary at institutional power with constrained exit (can change the law through court reasoning, but constitutional settlement constrains them) has moderate d values with asymmetric structure depending on timescale: immediate biographical context shows higher d (extraction-like experience of foreclosed authority), generational context shows lower d (understanding of reciprocal boundary). The analytical observer sees the full structure — the d value reflects that all three beneficiary/victim/neutral positions are present in the constraint's mechanics. No override needed; the structural data produces accurate d values through the standard derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED VIA KERNEL DECOMPOSITION. The constraint's claimed_type is tangled_rope (0.52 extractiveness, 0.68 suppression, requires_active_enforcement: true, beneficiaries + victims declared). The mandate emerges from the false-summit risk: if Article 9 is presented as a natural law of parliamentary structure ('immunity is inherent to legislative function'), the constraint might classify as mountain at some perspectives. But the structural data — beneficiaries (MPs), victims (injured litigants), measurable extractiveness (0.52), enforced suppression (0.68) — contradicts mountain classification. The tangled_rope reading resolves the mandatrophy by showing that parliamentary privilege is a contingent institutional choice with real coordination benefits AND real extractive costs, not a natural law. The kernel frame further resolves mandatrophy: the reading is ONE interpretation of the Bill of Rights among three. The Bill itself is the kernel; the readings are how different communities instantiate its meaning. This reading's tangled_rope status is the correct classification for the parliamentary privilege interpretation. A different reading (proto_rights_charter) would produce a different constraint with different type and metrics. The three readings together span the Bill's full structural spectrum. The engine confirms mandatrophy resolution when all three readings are compiled and network links established.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    absolute_vs_qualified_privilege_boundary,
    'Does Article 9''s absolute privilege represent the optimal balance between parliamentary deliberation freedom and remedies for injured parties, or is a qualified privilege doctrine (allowing suits for malice, knowing falsehood, or recklessness) structurally coherent within the 1689 framework?',
    'Doctrinal analysis of whether qualified privilege could maintain parliamentary immunity for good-faith deliberation while allowing suits for knowingly false statements; comparative analysis of qualified privilege regimes (e.g., US state defamation law, EU member states) and their effect on parliamentary candor',
    'If qualified privilege is coherent: Article 9 represents a contingent policy choice, not a natural necessity. The extractiveness value is policy-dependent, not structural. Reclassify toward higher snare component if qualified privilege shows superior outcomes in empirical cases. If absolute privilege is uniquely necessary: the suppression is legitimate cost of coordination; extractiveness reflects real structural trade-off, tangled_rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolute_vs_qualified_privilege_boundary, conceptual, 'Whether absolute privilege is structurally necessary or represents a contingent policy choice').

omega_variable(
    modern_remedial_alternatives,
    'Do modern remedies outside defamation law (regulatory prosecution, recall mechanisms, parliamentary censure, parliamentary privilege waiver procedures) create effective exits for injured parties that were unavailable in 1689, thereby reducing the actual extractiveness of the privilege in contemporary practice?',
    'Empirical audit of cases involving alleged parliamentary false statements: track outcomes via regulatory prosecution, investigative journalism, parliamentary internal mechanisms, and legislative amendment rather than tort law; measure frequency and effectiveness of privilege-waiver petitions',
    'If alternatives are effective: modern extractiveness is lower than the metric reflects (0.52 may overstate contemporary suppression). Reclassify toward rope. If alternatives are theatrical or rarely used: extractiveness is accurate, and the privilege remains a snare for modern litigants despite formal alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_remedial_alternatives, empirical, 'Whether modern remedies outside defamation law reduce actual suppression of injured parties').

omega_variable(
    kernel_reading_contest_identity,
    'Is this reading of Article 9 as legislative-protection-focused fundamentally incompatible with the anti_catholic_settlement_reading (which emphasizes the Bill of Rights as confessional exclusion), or do they coexist as competing but valid interpretations within the 1689 framework?',
    'Constitutional hermeneutics: examine whether the parliamentary privilege protection mechanism (Article 9) and the confessional settlement mechanism (succession and religious tests) operate on different constitutional axes and could both be intended by the same framers, or whether the readings logically foreclose each other. Analyze primary source debates during drafting.',
    'If readings foreclose each other: one reading''s axioms contradict the other''s foundational premise at the framework level. Update cs_structure.reading_relations to ''forecloses'' rather than ''coexists_with''. If readings coexist: different aspects of the same instrument, held by different parties in the political settlement. Confirms ''coexists_with'' relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_identity, conceptual, 'Whether parliamentary privilege reading and confessional settlement reading logically foreclose each other').

omega_variable(
    american_reception_and_reinterpretation,
    'Did the American founders'' incorporation of parliamentary privilege principles into the Speech and Debate Clause (U.S. Constitution, Article I, Section 6) represent faithful transmission of the 1689 reading, or a substantive reinterpretation that changed the constraint''s extractiveness profile?',
    'Comparative doctrinal analysis: examine how American courts apply Speech and Debate Clause (broader immunity, clearer boundaries, stronger remedies for witnesses) vs. how UK courts apply Article 9 (broader historical scope, less clarity on modern application). Measure empirical outcomes: frequency of successful suits, remedies available, parliamentary candor changes across jurisdictions.',
    'If faithful transmission: Article 9''s reading is portable and stable. If reinterpreted: American version has lower extractiveness (tighter boundaries) or lower suppression (better remedies), indicating that the 1689 reading is contingent to its institutional context, not universal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(american_reception_and_reinterpretation, empirical, 'Whether American interpretation represents faithful transmission or substantive reinterpretation').

omega_variable(
    false_summit_natural_law_claim,
    'Is Article 9 presented as inherent to legislative function or as a contingent institutional choice? If the former, does the constraint''s documented extractiveness (0.52) and identified beneficiaries (MPs, parliamentary deliberation) contradict the natural-law framing?',
    'Textual analysis of how Article 9 is justified in constitutional law: appeals to ''necessity,'' ''inherent parliamentary immunity,'' ''structural constitutional law'' vs. appeals to ''policy choice,'' ''historical settlement,'' ''negotiated balance.'' Compare with how other constitutional privileges (attorney-client, spousal) are justified. If Article 9 claims natural-law status but exhibits clear beneficiary/victim asymmetry, engine''s false-summit detection fires.',
    'If natural-law claim is made: the constraint is a false summit. Beneficiary presence (MPs) and victim presence (injured litigants) should trigger FSM reclassification to tangled_rope. If the 1689 framers presented it as policy choice: no false summit; the tangled_rope classification is the framers'' own reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether Article 9 is presented as natural law to parliamentary function or as contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bill_of_rights_1689__parliamentary_privilege_reading, 0, 337).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bor1689_parlpriv_theater_1689, bill_of_rights_1689__parliamentary_privilege_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(bor1689_parlpriv_theater_1789, bill_of_rights_1689__parliamentary_privilege_reading, theater_ratio, 100, 0.32).
narrative_ontology:measurement(bor1689_parlpriv_theater_1889, bill_of_rights_1689__parliamentary_privilege_reading, theater_ratio, 200, 0.35).
narrative_ontology:measurement(bor1689_parlpriv_theater_2026, bill_of_rights_1689__parliamentary_privilege_reading, theater_ratio, 337, 0.38).

% Extraction over time
narrative_ontology:measurement(bor1689_parlpriv_extractiveness_1689, bill_of_rights_1689__parliamentary_privilege_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bor1689_parlpriv_extractiveness_1789, bill_of_rights_1689__parliamentary_privilege_reading, base_extractiveness, 100, 0.48).
narrative_ontology:measurement(bor1689_parlpriv_extractiveness_1889, bill_of_rights_1689__parliamentary_privilege_reading, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(bor1689_parlpriv_extractiveness_2026, bill_of_rights_1689__parliamentary_privilege_reading, base_extractiveness, 337, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bor1689_parlpriv_suppression_1689, bill_of_rights_1689__parliamentary_privilege_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(bor1689_parlpriv_suppression_1789, bill_of_rights_1689__parliamentary_privilege_reading, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(bor1689_parlpriv_suppression_1889, bill_of_rights_1689__parliamentary_privilege_reading, suppression_requirement, 200, 0.68).
narrative_ontology:measurement(bor1689_parlpriv_suppression_2026, bill_of_rights_1689__parliamentary_privilege_reading, suppression_requirement, 337, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bill_of_rights_1689__parliamentary_privilege_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(bill_of_rights_1689__parliamentary_privilege_reading, bill_of_rights_1689__anti_catholic_settlement_reading).
narrative_ontology:affects_constraint(bill_of_rights_1689__parliamentary_privilege_reading, bill_of_rights_1689__proto_rights_charter_reading).

% DUAL FORMULATION NOTE:
% The Bill of Rights 1689 kernel permits three structurally distinct constraint readings: (1) parliamentary privilege reading (this file) — emphasis on Article 9 immunity, extractiveness 0.52, tangled_rope; (2) anti_catholic_settlement_reading — emphasis on confessional exclusions, likely higher suppression, snare or tangled_rope; (3) proto_rights_charter_reading — emphasis on precedent to modern rights, likely lower extractiveness as coordination mechanism, rope or tangled_rope. Each reading has its own ε value, its own beneficiary/victim structure, and its own CS axioms. The three readings coexist across different jurisprudential communities and different historical moments (1689 confessional moment vs. 1791 rights moment vs. contemporary parliamentary practice moment). The epsilon-invariance principle holds: if reading A's ε differs from reading B's ε, they are different constraints, not the same constraint viewed from different angles. The kernel decomposition allows the engine to model how a single written text (the Bill) carries multiple structurally distinct constraints depending on which reading one instantiates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bill_of_rights_1689__parliamentary_privilege_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
