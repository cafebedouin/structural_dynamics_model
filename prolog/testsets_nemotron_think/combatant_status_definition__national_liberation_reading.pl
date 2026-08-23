% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: AP I Article 1(4) Combatant Status for National Liberation Movements
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   Additional Protocol I Article 1(4) extends combatant status to non-state
 *   armed groups fighting colonial domination, alien occupation, and racist
 *   regimes, provided they are organized under responsible command and comply
 *   with IHL. This reading — the national liberation reading — treats the
 *   provision as a substantive expansion of combatant status, creating real
 *   POW protections for qualifying movements and binding obligations on the
 *   regimes they fight. The constraint is claimed as tangled_rope: it
 *   coordinates by clarifying status in asymmetric conflicts (genuine
 *   coordination function) but extracts asymmetrically from occupying powers
 *   who must grant immunity to fighters they deem illegitimate. The metrics
 *   reflect stable extraction from 1990s onward — the obligation persists but
 *   state practice increasingly evades it through non-recognition of
 *   applicability, making the coordination function more theatrical.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.62).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.48).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "AP I Article 1(4) Combatant Status for National Liberation Movements").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, 'aba3531c-6509-449a-86f1-bf3a7d09a3c8').
narrative_ontology:cs_kernel_codification('aba3531c-6509-449a-86f1-bf3a7d09a3c8', formalized).
narrative_ontology:cs_authority_grounding('aba3531c-6509-449a-86f1-bf3a7d09a3c8', lineage).
narrative_ontology:cs_interpretation_layer_present('aba3531c-6509-449a-86f1-bf3a7d09a3c8').
narrative_ontology:cs_reading_relation('aba3531c-6509-449a-86f1-bf3a7d09a3c8', combatant_status_definition__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('aba3531c-6509-449a-86f1-bf3a7d09a3c8', combatant_status_definition__functional_protection_reading, coexists_with).
narrative_ontology:cs_axiom('aba3531c-6509-449a-86f1-bf3a7d09a3c8', foundational, combatant_status_extends_to_liberation_fighters).
narrative_ontology:cs_axiom_status(combatant_status_extends_to_liberation_fighters, holdable).
narrative_ontology:cs_axiom_grounding('aba3531c-6509-449a-86f1-bf3a7d09a3c8', combatant_status_extends_to_liberation_fighters, conventional).
narrative_ontology:cs_axiom('aba3531c-6509-449a-86f1-bf3a7d09a3c8', secondary, article_1_4_reflects_customary_international_law).
narrative_ontology:cs_axiom_status(article_1_4_reflects_customary_international_law, holdable).
narrative_ontology:cs_axiom_grounding('aba3531c-6509-449a-86f1-bf3a7d09a3c8', article_1_4_reflects_customary_international_law, empirically_contingent).
narrative_ontology:cs_reference_frame('aba3531c-6509-449a-86f1-bf3a7d09a3c8', api_1977_treaty_text).
narrative_ontology:cs_drift_state('aba3531c-6509-449a-86f1-bf3a7d09a3c8', contemporary_post_cold_war, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aba3531c-6509-449a-86f1-bf3a7d09a3c8', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, liberation_movements).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, organized_armed_groups_under_responsible_command).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_powers).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, colonial_administrations).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, racist_regimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-state armed groups fighting colonial domination, alien occupation, or racist regimes. They gain conditional combatant status and POW protections if they meet the Article 1(4) criteria — organized structure, responsible command, and carrying arms openly. Their identity is fused with the liberation struggle; exit means abandoning the political project that constitutes them. They bear the burden of proving organizational compliance to claim the protections.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, liberation_movements, beneficiary,
    organized, biographical, identity_locked, regional).

% States exercising colonial, occupation, or apartheid authority. They are obligated to grant combatant immunity and POW status to qualifying enemy fighters whom they domestically classify as terrorists or criminals. The constraint extracts compliance costs: legal obligation to treat captives as POWs, forego domestic criminal prosecution for lawful acts of war, and accept international monitoring. Exit from the obligation requires ending the occupation/regime or withdrawing from AP I — both politically prohibitive.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, occupying_powers, payer,
    institutional, generational, constrained, global).

% The 174 states party to Additional Protocol I. They created the rule through diplomatic conference and maintain it through treaty compliance mechanisms. They can invoke dispute settlement, support ICRC monitoring, and shape customary law interpretation. Some (Western states) joined with understandings limiting Article 1(4); others (Global South) championed it as anti-colonial achievement.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, state_parties_to_api, agenda_setter,
    institutional, generational, arbitrage, global).

% ICJ, ICC, ICTY, ICTR, and regional human rights courts. They interpret Article 1(4) in adjudicating status disputes — e.g., whether a specific group meets 'organized under responsible command.' Their jurisprudence shapes the operational boundary but they do not collect or pay the constraint's extraction.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, international_courts_tribunals, observer,
    institutional, generational, analytical, global).

% Populations in territories where liberation movements operate. They experience both the violence of the conflict and the protection effects of the rule — when combatant status is recognized, IHL rules on distinction and proportionality apply more rigorously. They have no voice in treaty interpretation and cannot exit the conflict zone.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, civilian_populations_under_occupation, excluded,
    powerless, immediate, trapped, local).

% ICRC, NGOs, UN agencies. They monitor compliance, visit detainees, and advocate for status determinations. They benefit operationally when the rule functions (clearer access, legal basis for POW visits) but do not structurally collect the constraint's extraction.
narrative_ontology:constraint_stakeholder(combatant_status_definition__national_liberation_reading, humanitarian_organizations, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework to distinguish lawful combatants from unlawful fighters in wars of national liberation, replacing the prior gap where liberation fighters were treated as common criminals under domestic law. Solves the coordination problem of status determination in asymmetric conflicts against colonial/occupation/racist regimes.
% TRANSFER_FUNCTION: Moves the obligation to grant full POW protections (combatant immunity, humane treatment, fair trial guarantees, protection from prosecution for lawful acts of war) from occupying/colonial/racist regimes to organized liberation movements meeting the Article 1(4) criteria. The transfer is conditional on the movement's structural compliance.
% ABSENT_VOICES: Non-state armed groups fighting in non-colonial contexts (e.g., internal insurgencies not meeting Article 1(4) threshold) — they would argue for equal status but are structurally excluded by the colonial/occupation/racist regime qualifier. Also absent: populations living under liberation movement governance who may experience authoritarian practices; their consent to the movement's 'representative' claim is never tested.
% DISAPPEARANCE_RATIONALE: If Article 1(4) vanished overnight, liberation fighters would revert to 'unprivileged belligerent' status — subject to domestic criminal prosecution for mere participation in hostilities. Occupying powers would lose treaty obligation to grant POW protections, though customary law might retain some floor. The legal architecture distinguishing liberation wars from internal insurgencies would collapse, reshaping state practice in ongoing occupations (Palestine, Western Sahara, etc.).
% FOUNDING_PROBLEM: The 1949 Geneva Conventions limited combatant status to state armed forces and assimilated groups, leaving fighters against colonial and racist regimes without POW protections — they were prosecuted as terrorists or bandits. The 1974-1977 diplomatic conference created Article 1(4) to close this gap, recognizing wars of national liberation as international armed conflicts.
% FOUNDING_PROBLEM_CORROBORATION: ICRC Commentary on AP I (1987) and preparatory works confirm the anti-colonial purpose. UNGA resolutions 3070 (XXVIII) and 3103 (XXVIII) affirm legitimacy of liberation struggles. However, major Western states (US, UK, France) filed understandings limiting Article 1(4) applicability, and post-Cold War state practice shows declining recognition of new 'liberation wars' — the founding problem (colonial domination) is declared resolved by some, ongoing by others.
narrative_ontology:disappearance_verdict(combatant_status_definition__national_liberation_reading, world_rearranges).
narrative_ontology:founding_problem_status(combatant_status_definition__national_liberation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(combatant_status_definition__national_liberation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(combatant_status_definition__national_liberation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(combatant_status_definition__national_liberation_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) centers on the occupying power's obligation: they must confer POW status on adversaries they politically reject, a significant compliance cost. Suppression (0.48) is moderate — the rule is treaty law with monitoring mechanisms, but enforcement relies on state consent and ICRC access; occupying powers routinely deny applicability. Theater ratio (0.28) rose post-1990 as decolonization waned: the provision remains on the books but its operational domain shrinks, making invocation increasingly performative. Resistance (0.71) is high — target regimes actively contest status determinations in every conflict. Accessibility collapse (0.52) is moderate: the criteria (organization, command, open carry) are legally defined but factually contested in each case.
 *
 * PERSPECTIVAL GAP:
 *   From the liberation movement seat, the constraint is a rope — it coordinates their recognition and protects their fighters. From the occupying power seat, it is a snare — pure extraction of immunity they never consented to. From the state party seat, it is a scaffold — a transitional provision for decolonization whose sunset has ambiguously arrived. The engine computes these divergences from the structural data; the claim does not resolve them.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberation movements are conditional beneficiaries (d ~0.3): they gain protections but must meet structural criteria that require organizational discipline — the constraint shapes them as much as it benefits them. Occupying powers are full payers (d ~0.85): they bear the obligation without reciprocal benefit, and their exit is politically blocked. State parties to AP I are agenda-setters with arbitrage-grade exit (they can withdraw from the treaty). Courts and humanitarian orgs are observers (d=0.5). Civilian populations are excluded and trapped (d=1.0 for harm, but they don't pay the constraint's extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial domination) is contested as live or dead. If dead, the constraint persists as a piton — vestigial coordination maintained by institutional inertia and symbolic politics. If live (ongoing occupations), it remains a tangled rope with active extraction. The mandatrophy diagnosis turns on whether the colonial/occupation/racist regime category still describes real-world situations that the rule was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural classification change when the contested kernel (combatant_status_definition) is instantiated by sibling readings?',
    'Compare effective extraction (χ) and computed type across all three readings using identical stakeholder structures but different beneficiary/victim assignments per reading. The kernel''s referent is the standing legal arrangement; each reading authors ε for that arrangement from its own lights.',
    'If state_centric_reading computes as mountain (no extraction, natural law of sovereignty) while national_liberation_reading computes as tangled_rope, the kernel itself is not a single constraint but a family. The divergence measures the contestation intensity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame mapping: this reading is one instantiation of the combatant_status_definition kernel; sibling readings produce different ε and type for the same referent.').

omega_variable(
    article_1_4_applicability_post_decolonization,
    'Does Article 1(4) apply to contemporary occupations (e.g., Palestine, Western Sahara) or only to classic decolonization wars of the 1960s-1970s?',
    'ICJ advisory opinions, state practice in recognizing conflicts as Article 1(4) situations, and ICRC operational guidance. A finding of continuing applicability sustains the coordination function; a finding of obsolescence converts the constraint to piton.',
    'If applicability is limited to historical decolonization, the constraint''s extraction on current occupying powers is illegitimate (no live coordination problem) — reclassifies toward piton. If applicability continues, extraction remains justified as coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_1_4_applicability_post_decolonization, empirical, 'Temporal scope of the colonial/occupation/racist regime trigger — determines whether the constraint has a live coordination function.').

omega_variable(
    organized_responsible_command_threshold,
    'What level of organizational structure and command control satisfies Article 1(4)''s criteria for non-state groups?',
    'ICRC interpretive guidance, ICTY/ICTR jurisprudence on ''organized armed groups,'' and state practice in status determinations. A high threshold excludes most real-world movements; a low threshold extends combatant status broadly.',
    'High threshold → fewer beneficiaries, lower extraction on occupying powers, constraint approaches rope. Low threshold → more beneficiaries, higher extraction, constraint deepens as tangled_rope. The threshold is the operational valve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organized_responsible_command_threshold, conceptual, 'Interpretive boundary of the beneficiary class — directly modulates ε for both liberation movements and occupying powers.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (treaty denial, non-ratification, understandings) or internalized (liberation movements self-limiting to meet criteria, occupying powers internalizing denial as legal reality)?',
    'Post-exit suppression trajectory: if a liberation movement achieves statehood and the suppression dynamic persists in new forms (e.g., successor state denying protections to new insurgents), reclassify as partially internalized.',
    'If internalized, effective suppression exceeds the structural measure — the constraint''s categories reproduce themselves beyond the treaty text.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression in the liberation/occupation dynamic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 1977, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comb_tr_t1977, combatant_status_definition__national_liberation_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(comb_tr_t1987, combatant_status_definition__national_liberation_reading, theater_ratio, 1987, 0.22).
narrative_ontology:measurement(comb_tr_t1997, combatant_status_definition__national_liberation_reading, theater_ratio, 1997, 0.28).
narrative_ontology:measurement(comb_tr_t2007, combatant_status_definition__national_liberation_reading, theater_ratio, 2007, 0.28).
narrative_ontology:measurement(comb_tr_t2017, combatant_status_definition__national_liberation_reading, theater_ratio, 2017, 0.28).
narrative_ontology:measurement(comb_tr_t2024, combatant_status_definition__national_liberation_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(comb_be_t1977, combatant_status_definition__national_liberation_reading, base_extractiveness, 1977, 0.55).
narrative_ontology:measurement(comb_be_t1987, combatant_status_definition__national_liberation_reading, base_extractiveness, 1987, 0.58).
narrative_ontology:measurement(comb_be_t1997, combatant_status_definition__national_liberation_reading, base_extractiveness, 1997, 0.61).
narrative_ontology:measurement(comb_be_t2007, combatant_status_definition__national_liberation_reading, base_extractiveness, 2007, 0.62).
narrative_ontology:measurement(comb_be_t2017, combatant_status_definition__national_liberation_reading, base_extractiveness, 2017, 0.62).
narrative_ontology:measurement(comb_be_t2024, combatant_status_definition__national_liberation_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comb_su_t1977, combatant_status_definition__national_liberation_reading, suppression_requirement, 1977, 0.45).
narrative_ontology:measurement(comb_su_t1987, combatant_status_definition__national_liberation_reading, suppression_requirement, 1987, 0.48).
narrative_ontology:measurement(comb_su_t1997, combatant_status_definition__national_liberation_reading, suppression_requirement, 1997, 0.48).
narrative_ontology:measurement(comb_su_t2007, combatant_status_definition__national_liberation_reading, suppression_requirement, 2007, 0.48).
narrative_ontology:measurement(comb_su_t2017, combatant_status_definition__national_liberation_reading, suppression_requirement, 2017, 0.48).
narrative_ontology:measurement(comb_su_t2024, combatant_status_definition__national_liberation_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, pow_protections_geneva_iii).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, common_article_3_threshold).

% DUAL FORMULATION NOTE:
% This reading and state_centric_reading are logically incompatible within a single legal framework (forecloses relation). Both coexist with functional_protection_reading which operates at the Common Article 3 floor. The three readings form a constraint family linked by the combatant_status_definition kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__national_liberation_reading, institutional, 0.85).
constraint_indexing:directionality_override(combatant_status_definition__national_liberation_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
