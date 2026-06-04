% ============================================================================
% CONSTRAINT STORY: accountability_machinery__ostracism_institution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_accountability_machinery__ostracism_institution, []).

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
 *   constraint_id: accountability_machinery__ostracism_institution
 *   human_readable: Ostracism Institution: Pre-emptive Exile as Accountability
 *   domain: legal/doctrinal/athenian_democracy
 *
 * SUMMARY:
 *   Ostracism in Athenian democracy represents a stark institutional choice:
 *   the power to exile any citizen for ten years by majority vote, with no
 *   charge, no trial, no defense. The victim faces not accusation but
 *   popularity — prominence itself becomes the offense. This constraint
 *   instantiates one reading of the broader accountability machinery kernel:
 *   the question of how democracy constrains over-mighty individuals. Unlike
 *   euthynai audits (which examine everyone's accounts through quasi-judicial
 *   process) or graphe paranomon suits (which hold proposers liable for
 *   persuading illegal decisions), ostracism operates pre-emptively and
 *   structurally, targeting standing itself rather than conduct. The
 *   mechanism exhibits maximal suppression (0.82) and high extractiveness
 *   (0.68) because it functions without the procedural legitimacy granted to
 *   other accountability forms. The theater ratio is low (0.35) — unlike
 *   performative review rituals, ostracism is brutally functional: a vote, a
 *   decision, exile. The constraint benefits the democratic collective (fear
 *   of tyranny is channeled into institutional form) while victimizing the
 *   prominent individual and the principle of due process. This reading is
 *   one point in the accountability machinery's multi-dimensional design
 *   space — and it directly conflicts with the charge-based logic of the
 *   sibling readings.
 *
 * KEY AGENTS:
 *   - Exiled Prominent Citizen: Primary victim (powerless/trapped) — faces exit from the state with no recourse; bears maximum extraction and suppression
 *   - Due Process Principle: Secondary victim (powerless/trapped) — suspended entirely; no charge, no hearing, no defense; victimized as an abstract commitment
 *   - Democratic Assembly: Primary beneficiary (organized/arbitrage) — solves the tyranny coordination problem; experiences the constraint as legitimate authority; can revoke at decade's end
 *   - Prominent Non-Exiled Citizen: Secondary target (powerful/constrained) — suppressed pre-emptively; high career risk from the existence of the mechanism itself
 *   - Athenian State Machinery: Institutional actor (institutional/constrained) — benefits from anti-tyranny function but pays extraction cost in military instability and deliberation corruption
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional choice as an inherent political necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(accountability_machinery__ostracism_institution, 0.68).
domain_priors:suppression_score(accountability_machinery__ostracism_institution, 0.82).
domain_priors:theater_ratio(accountability_machinery__ostracism_institution, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(accountability_machinery__ostracism_institution, extractiveness, 0.68).
narrative_ontology:constraint_metric(accountability_machinery__ostracism_institution, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(accountability_machinery__ostracism_institution, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(accountability_machinery__ostracism_institution, snare).
narrative_ontology:human_readable(accountability_machinery__ostracism_institution, "Ostracism Institution: Pre-emptive Exile as Accountability").
narrative_ontology:topic_domain(accountability_machinery__ostracism_institution, "legal/doctrinal/athenian_democracy").

domain_priors:requires_active_enforcement(accountability_machinery__ostracism_institution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(accountability_machinery__ostracism_institution, '4a0bd24d-bec3-4aab-8e45-27f3a5caf522').
narrative_ontology:cs_kernel_codification('4a0bd24d-bec3-4aab-8e45-27f3a5caf522', formalized).
narrative_ontology:cs_authority_grounding('4a0bd24d-bec3-4aab-8e45-27f3a5caf522', extraction).
narrative_ontology:cs_interpretation_layer_present('4a0bd24d-bec3-4aab-8e45-27f3a5caf522').
narrative_ontology:cs_reading_relation('4a0bd24d-bec3-4aab-8e45-27f3a5caf522', accountability_machinery__euthynai_audit, coexists_with).
narrative_ontology:cs_reading_relation('4a0bd24d-bec3-4aab-8e45-27f3a5caf522', accountability_machinery__graphe_paranomon, coexists_with).
narrative_ontology:cs_axiom('4a0bd24d-bec3-4aab-8e45-27f3a5caf522', foundational, tyranny_prevention_primacy).
narrative_ontology:cs_axiom_status(tyranny_prevention_primacy, holdable).
narrative_ontology:cs_axiom_grounding('4a0bd24d-bec3-4aab-8e45-27f3a5caf522', tyranny_prevention_primacy, empirically_contingent).
narrative_ontology:cs_axiom('4a0bd24d-bec3-4aab-8e45-27f3a5caf522', foundational, structural_suppression_legitimacy).
narrative_ontology:cs_axiom_status(structural_suppression_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4a0bd24d-bec3-4aab-8e45-27f3a5caf522', structural_suppression_legitimacy, deontological).
narrative_ontology:cs_reference_frame('4a0bd24d-bec3-4aab-8e45-27f3a5caf522', tyranny_prevention_through_preemptive_exile).
narrative_ontology:cs_drift_state('4a0bd24d-bec3-4aab-8e45-27f3a5caf522', decline_of_ostracism_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4a0bd24d-bec3-4aab-8e45-27f3a5caf522', '').
narrative_ontology:cs_kernel_id(accountability_machinery__ostracism_institution, accountability_machinery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(accountability_machinery__ostracism_institution, democratic_assembly_collective).
narrative_ontology:constraint_victim(accountability_machinery__ostracism_institution, prominent_individuals).
narrative_ontology:constraint_victim(accountability_machinery__ostracism_institution, due_process_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXILED PROMINENT CITIZEN (SNARE) — No charge, no defense, no appeal. The vote itself is the mechanism; prominence is the crime. Trapped by citizenship itself — the only exit is permanent loss of political membership. Maximum suppression and extraction: the constraint targets this agent with surgical precision and offers no recourse.
constraint_indexing:constraint_classification(accountability_machinery__ostracism_institution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE DUE PROCESS PRINCIPLE (SNARE) — Ostracism operates outside every known accountability mechanism: no charge, no hearing, no defense. The principle is victimized — its rules are suspended entirely. Trapped because the democracy has the power to suspend it, and there is no meta-rule to prevent this suspension. Structural extraction of legitimacy from the rule itself.
constraint_indexing:constraint_classification(accountability_machinery__ostracism_institution, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE DEMOCRATIC ASSEMBLY (ROPE) — Sees ostracism as a coordination solution to the problem of tyranny: the assembly collectively neutralizes the risk of over-mighty individuals capturing the state. The extraction experienced by the assembly is minimal (the vote costs little). The beneficiary is the shared fear of tyranny and the collective benefit of distributed power. Exit option for the assembly is arbitrage: they can revoke ostracism at the decade's end or dissolve the mechanism entirely. Net beneficiary — experiences the constraint as solving a genuine collective action problem.
constraint_indexing:constraint_classification(accountability_machinery__ostracism_institution, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: PROMINENT NON-EXILED CITIZEN (SNARE) — Powerful but constrained by the knowledge that prominence itself invites exile. The mechanism suppresses ambitious orators and generals who might otherwise accumulate power. The constraint targets them pre-emptively, not through criminal charge but through existential threat. Suppression is high; exit (becoming unprominent) carries severe career cost. This agent experiences extraction in the form of constant political risk premium.
constraint_indexing:constraint_classification(accountability_machinery__ostracism_institution, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ATHENIAN STATE MACHINERY (TANGLED ROPE) — The state benefits from ostracism as an anti-tyranny mechanism (genuine coordination problem solved: tyranny is prevented). But ostracism also extracts from the state itself — it destabilizes military leadership during wars, exiles military expertise needed for defense, and corrupts deliberation with fear. Mixed benefit and cost: the machinery depends on ostracism for legitimacy but pays a structural price for running it.
constraint_indexing:constraint_classification(accountability_machinery__ostracism_institution, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the tension between collective safety and individual rights is inherent to any political system: some mechanism must constrain over-mighty individuals, yet any such mechanism risks tyranny itself. Ostracism appears as a natural law of political structures — the built-in contradiction that no design fully resolves. However, this naturalizes a contingent Athenian choice; the engine's false summit detector flags this perspective as misidentifying an institution for an inevitability.
constraint_indexing:constraint_classification(accountability_machinery__ostracism_institution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(accountability_machinery__ostracism_institution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(accountability_machinery__ostracism_institution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(accountability_machinery__ostracism_institution, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(accountability_machinery__ostracism_institution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(accountability_machinery__ostracism_institution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The mechanism extracts from prominent individuals by targeting them pre-emptively. The extraction is asymmetric and structural — prominence itself, not conduct, is the basis. The value reflects that the extraction is real and systematic, but not absolute (exiled individuals retain property, can return after ten years, may retain influence networks). Suppression (0.82): Very high. The mechanism operates without charge, without hearing, without defense — the procedural safeguards are stripped. Suppression is enforced through fear: the knowledge that any citizen can be exiled creates a chilling effect on ambitious action. Theater ratio (0.35): Low. Unlike peer review rituals or formal trials, ostracism is functional and direct: the vote IS the mechanism. There is minimal performative content — the outcome is what it claims. This low theater distinguishes ostracism from piton or scaffold constraints that depend on theatrical maintenance. The rising trajectory in measurements reflects institutional hardening over time: as ostracism becomes normalized, extractiveness increases (prominent individuals grow more cautious), suppression requirement increases (the mechanism must target more aggressively to prevent tyranny), and theater increases slightly (the practice becomes more ritualized and less shocking).
 *
 * PERSPECTIVAL GAP:
 *   The exiled citizen and the democratic assembly occupy opposite structural positions regarding the same institutional mechanism. The assembly sees a solution to tyranny (rope-like coordination); the exile sees preemptive extraction without recourse (snare). The due process principle sees its suspension; the assembly sees its temporary rational override. The prominent non-exiled citizen experiences suppression through existential threat (snare); the assembly experiences authority through collective agreement (rope). The analytical observer risks mistaking this design choice for a natural law of political structure (mountain), but the structural data reveals it as a contingent institutional arrangement with identifiable beneficiaries and victims — a false summit. The perspectival gap is maximal: no two observers classify the constraint identically.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) is derived from their structural position and relationship to the extraction flow. The exiled citizen is a powerless, trapped victim — maximum d (0.95+), high f(d), experiencing the constraint at maximum magnitude. The prominent non-exiled citizen is powerful but constrained by the threat — intermediate d reflecting that they are a structural target but not a realized victim. The democratic assembly is an organized beneficiary with arbitrage exit — low d (0.10-0.20), experiencing negative effective extraction (the mechanism benefits them). The analytical observer occupies the measurement position itself — d derived from the classical fallback for analytical power (0.73), experiencing the constraint as a perspectival site. The sibling readings (euthynai, graphe paranomon) occupy different structural positions in the accountability machinery, with different d values and different mechanisms for controlling over-mighty individuals.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: Ostracism resolves the mandatrophy by demonstrating that accountability mechanisms face an inherent trade-off between preventing tyranny and preserving due process — but this trade-off is NOT a natural law. It is a design choice. Ostracism chooses tyranny prevention over due process. Euthynai chooses distributed accountability over concentrated power. Graphe paranomon chooses orator liability over assembly immunity. Each reading of the accountability machinery makes different assumptions about which risks are greatest and which principles are expendable. The snare classification is correct for ostracism because it targets standing structurally, operates without procedural safeguard, and extracts from the prominent individual. But the snare is a choice, not an inevitability. Other readings of the accountability machinery would classify differently and build different institutional forms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ostracism_as_coordination_vs_preemptive_extraction,
    'Is ostracism a genuine solution to the tyranny coordination problem, or a preemptive extraction mechanism against prominence itself?',
    'Historical pattern analysis: do exiled individuals correlate with actual threats to democracy, or with mere political prominence? Counterfactual: did Athens require ostracism to prevent tyranny, or were other accountability mechanisms (euthynai, graphe paranomon) sufficient?',
    'If coordination: snare classification downgrades to tangled_rope; suppression interpretation shifts from preemptive to preventive. If preemptive extraction: snare classification confirmed; the mechanism targets standing regardless of actual threat. This determines whether ostracism is an accountability tool or a weapon against over-mighty individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ostracism_as_coordination_vs_preemptive_extraction, empirical, 'Whether ostracism targets actual tyranny threats or mere prominence').

omega_variable(
    natural_law_vs_constructed_institution,
    'Is the tension between tyranny prevention and due process an immutable feature of political structure (natural law), or a particular institutional choice made in response to Athenian historical contingencies?',
    'Comparative political design: do other democracies and states require ostracism-equivalent mechanisms to prevent tyranny? What alternatives existed in Athens? Did Athens fail to prevent tyranny when ostracism was weakened or abandoned?',
    'If immutable natural law: the mountain classification is correct, and any accountability system faces this trade-off. If contingent choice: ostracism is a constructed constraint naturalizing one solution to a solvable problem — the mountain is a false summit. This is the kernel contest itself: whether the accountability machinery is a law of nature or a design question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_institution, conceptual, 'Whether the tyranny/due-process trade-off is inherent or contingent').

omega_variable(
    ostracism_relationship_to_sibling_mechanisms,
    'Does ostracism coexist with euthynai and graphe paranomon, or does ostracism''s pre-emptive mechanism foreclose the charge-based accountability of the other two?',
    'Institutional history: Did Athens use all three simultaneously, or did ostracism replace the others? When ostracism was weakened, did euthynai and graphe paranomon expand? Did the three mechanisms conflict or complement?',
    'If coexist: ostracism is one reading of accountability machinery; euthynai and graphe paranomon are alternative readings. If ostracism forecloses: then the reading is exclusive — the pre-emptive logic rules out charge-based accountability as the primary mechanism. This determines the reading_relations in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ostracism_relationship_to_sibling_mechanisms, empirical, 'How ostracism interacts with euthynai and graphe paranomon mechanisms').

omega_variable(
    extractiveness_of_standing_calibration,
    'Is the extractiveness value (0.68) correct, or does the cost of prominence — the perpetual risk of exile — calibrate extractiveness higher (0.75+)?',
    'Career analysis: what proportion of prominent Athenians were exiled? Did prominence itself correlate with ostracism probability? Did the mechanism suppress ambitious individuals from seeking prominence?',
    'If extractiveness should be higher: classification remains snare but magnitude of extraction is underestimated. If correct: the snare classification is properly calibrated. This affects the amplitude of the extraction experienced by powerful non-exiled citizens and the constraint''s overall severity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extractiveness_of_standing_calibration, empirical, 'Calibration of extractiveness value for standing risk').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(accountability_machinery__ostracism_institution, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acco_tr_t0, accountability_machinery__ostracism_institution, theater_ratio, 0, 0.25).
narrative_ontology:measurement(acco_tr_t25, accountability_machinery__ostracism_institution, theater_ratio, 25, 0.35).
narrative_ontology:measurement(acco_tr_t50, accountability_machinery__ostracism_institution, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(acco_be_t0, accountability_machinery__ostracism_institution, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(acco_be_t25, accountability_machinery__ostracism_institution, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(acco_be_t50, accountability_machinery__ostracism_institution, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(acco_su_t0, accountability_machinery__ostracism_institution, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(acco_su_t25, accountability_machinery__ostracism_institution, suppression_requirement, 25, 0.82).
narrative_ontology:measurement(acco_su_t50, accountability_machinery__ostracism_institution, suppression_requirement, 50, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(accountability_machinery__ostracism_institution, enforcement_mechanism).
narrative_ontology:affects_constraint(accountability_machinery__ostracism_institution, accountability_machinery__euthynai_audit).
narrative_ontology:affects_constraint(accountability_machinery__ostracism_institution, accountability_machinery__graphe_paranomon).

% DUAL FORMULATION NOTE:
% Ostracism is one reading of the accountability_machinery kernel. The other readings (euthynai_audit, graphe_paranomon) instantiate alternative institutional forms for the same problem domain: constraining over-mighty individuals while maintaining legitimacy. The three readings do not decompose per ε-invariance because they are not measuring the same observable differently — they are intentionally different answers to the same kernel question. Each gets its own constraint story with its own ε, reflecting the structural distinctness of pre-emptive suppression (ostracism), distributed audit (euthynai), and orator liability (graphe paranomon).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(accountability_machinery__ostracism_institution, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
