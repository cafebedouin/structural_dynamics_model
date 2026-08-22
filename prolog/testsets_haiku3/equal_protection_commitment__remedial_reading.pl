% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection Remedial Reading: Caste-Dismantling via Race-Conscious State Action
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The remedial reading of equal protection holds that the clause forbids
 *   the perpetuation of subordination and permits (indeed, requires)
 *   race-conscious state action to dismantle it. This is one reading of a
 *   contested constitutional kernel—the Equal Protection Clause itself, which
 *   has been read by different judges and scholars as demanding
 *   color-blindness, permitting diversity-seeking, or mandating remediation
 *   for historical caste. The remedial reading centers the historical wrong
 *   (systematic legal subordination) and treats race-conscious remediation as
 *   the structural correction. The beneficiary/victim structure inverts
 *   depending on observer position: from the remedial actor's seat,
 *   historically subordinated groups are beneficiaries and the state is the
 *   agenda-setter; from the historically privileged group's seat, they are
 *   the victims of redistribution. The claim/metric gap is intentional: the
 *   constraint is CLAIMED as tangled_rope (genuine coordination of
 *   remediation + asymmetric extraction of preferential access), while the
 *   metrics (extractiveness 0.38–0.52, suppression 0.55–0.70) reflect the
 *   contested nature and the real resistance from those who experience it as
 *   unjust redistribution.
 *
 * KEY AGENTS:
 *   - historically_subordinated_racial_groups: primary beneficiary (access to remedial programs); constrained exit (cannot opt out of remediation or past exclusion)
 *   - state_remedial_actors: agenda_setter (design and implement programs); institutional power (legislative, executive, judicial); arbitrage exit (can choose remedial strategy or abandon it)
 *   - historically_privileged_groups_denied_preferential_access: payer (denied admission/contracts); powerful but constrained to legal challenge
 *   - colorblind_advocates: excluded from this reading's framework; structural opposition; access denied by the reading's core axiom
 *   - supreme_court: observer/arbiter; final authority on reading legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.52).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.68).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection Remedial Reading: Caste-Dismantling via Race-Conscious State Action").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional/political").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, '12ed1db4-3c47-4e31-901a-a3fbdd910f51').
narrative_ontology:cs_kernel_codification('12ed1db4-3c47-4e31-901a-a3fbdd910f51', fixed_text).
narrative_ontology:cs_authority_grounding('12ed1db4-3c47-4e31-901a-a3fbdd910f51', extraction).
narrative_ontology:cs_interpretation_layer_present('12ed1db4-3c47-4e31-901a-a3fbdd910f51').
narrative_ontology:cs_reading_relation('12ed1db4-3c47-4e31-901a-a3fbdd910f51', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('12ed1db4-3c47-4e31-901a-a3fbdd910f51', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('12ed1db4-3c47-4e31-901a-a3fbdd910f51', foundational, race_consciousness_required_for_remediation).
narrative_ontology:cs_axiom_status(race_consciousness_required_for_remediation, holdable).
narrative_ontology:cs_axiom_grounding('12ed1db4-3c47-4e31-901a-a3fbdd910f51', race_consciousness_required_for_remediation, deontological).
narrative_ontology:cs_axiom('12ed1db4-3c47-4e31-901a-a3fbdd910f51', foundational, subordination_dismantling_is_equal_protection_duty).
narrative_ontology:cs_axiom_status(subordination_dismantling_is_equal_protection_duty, holdable).
narrative_ontology:cs_axiom_grounding('12ed1db4-3c47-4e31-901a-a3fbdd910f51', subordination_dismantling_is_equal_protection_duty, empirically_contingent).
narrative_ontology:cs_reference_frame('12ed1db4-3c47-4e31-901a-a3fbdd910f51', subordination_dismantling_mandate).
narrative_ontology:cs_drift_state('12ed1db4-3c47-4e31-901a-a3fbdd910f51', contemporary_2020s, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('12ed1db4-3c47-4e31-901a-a3fbdd910f51', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_remedial_actors).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_groups_denied_preferential_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups whose access to education, employment, public accommodations, and wealth accumulation was systematically restricted by law and practice. Under this reading, state-sponsored remedial programs (affirmative action in admissions, contracting preferences, targeted investment) are understood as correcting that subordination. They bear the burden of proving historical exclusion and present effects; they benefit when state actors design remedial programs on their behalf.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Government bodies—legislatures, administrators, courts—that design and implement remedial programs. Under this reading, they have the authority and duty to use race-conscious measures to dismantle the effects of prior caste-like subordination. They collect legitimacy from the remedial mandate and from successful program design; they bear the cost of legal defense and political contestation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_remedial_actors, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals from groups that did not face systematic legal exclusion (primarily white Americans in the U.S. context) who are denied admission to universities, public contracts, or other benefits in order to prioritize remedial candidates. Under this reading they are the 'victims' of redistribution, though the reading contests the framings of victimhood: they experience non-preference rather than exclusion, denial of advantage rather than denial of access. Their exit is constrained to legal challenge.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_groups_denied_preferential_access, payer,
    powerful, biographical, constrained, national).

% Legal scholars, judges, and policy advocates who hold that equal protection demands race-neutral law and that remedial race-consciousness reproduces the constitutional wrong it claims to fix. They are excluded from the remedial-reading framework—their core premise (that color-blindness is the constitutional mandate) is directly contradicted by this reading's axiom that remediation requires race-consciousness. They would object loudly if present but are kept out by the reading's foundational claim.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, colorblind_advocates, excluded,
    institutional, generational, trapped, national).

% Proponents of diversity as a compelling state interest—that race consciousness serves educational and social goals beyond remediation. Under this reading, they coexist rather than foreclose: both remedial and diversity readings permit race-conscious measures, but ground the permission differently. The diversity reading does not require findings of prior caste-like subordination; the remedial reading does and makes that the primary justification.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, diversity_advocates, excluded,
    institutional, generational, trapped, national).

% The ultimate authority over constitutional interpretation in this domain. The Court's composition and doctrine have oscillated between readings—from Plessy's acceptance of caste (1896) through Brown's rejection (1954), through Bakke's diversity language (1978), to recent decisions narrowing race-consciousness (Students First v. Harvard, 2023). The Court does not instantiate the remedial reading so much as rule on its legitimacy.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__remedial_reading, state_remedial_actors).
narrative_ontology:fixing_cost_class(equal_protection_commitment__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared understanding of what equal protection MEANS in a society marked by deep-rooted, legally-created racial hierarchies: it means not merely formal equality going forward, but active undoing of the structural subordination that formal law created and preserved. Solves the coordination problem of whether remediation is a constitutional duty or an optional policy choice.
% TRANSFER_FUNCTION: Transfers educational opportunity, contracting advantage, employment preference, and wealth-building access FROM historically privileged groups TO historically subordinated groups, justified as restitution for prior legal exclusion and correction of ongoing effects. The transfer is understood as compensation for systematic subordination, not as charity or diversity-seeking.
% ABSENT_VOICES: Colorblind-reading advocates are structurally excluded from the remedial framework—the reading's core axiom (that dismantling caste requires race-consciousness) directly contradicts their premise. They are not merely absent but objecting; the framework treats their objection as a fundamental misreading of the equal-protection clause. Also absent: the views of groups experiencing remedial programs (historically subordinated communities) as applied—whether the remedial programs actually dismantle subordination or merely redistribute within unchanged hierarchies.
% DISAPPEARANCE_RATIONALE: If the remedial reading and its supporting race-conscious programs disappeared overnight, access to elite education, public contracting, employment, and wealth-building would snap back toward pre-remediation distributions. The structure of institutional legitimacy for remedial action would collapse. Historically subordinated groups would revert to the formal-equality position with no resource for claiming the effects of prior exclusion. The reading is not decorative—it justifies real transfers.
% FOUNDING_PROBLEM: Centuries of explicit legal subordination: slavery, Jim Crow, redlining, exclusionary covenants, and de jure segregation created a caste-like system in which entire racial groups were barred from property ownership, education, employment, and political participation. Even after de jure abolition (Civil Rights Act 1964, Fair Housing Act 1968), the structural subordination persisted: wealth gaps, educational disparities, residential segregation, and unequal access to opportunity were the accumulated effects of prior law.
% FOUNDING_PROBLEM_CORROBORATION: Historians, economists, and empirical researchers outside the benefiting coalition document the founding problem extensively: Douglas Massey (segregation), Thomas Piketty (wealth inequality), Richard Rothstein (the color of law), and legal historians like Derrick Bell establish that subordination was systemic and legal. However, the question of whether remedial programs SOLVE the founding problem—whether they dismantle the caste system or merely redistribute within it—remains live. Conservative critics argue the founding problem is solved by law (post-1964 formal equality) and remediation reproduces rather than fixes it.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because the remedial reading serves a genuine coordination function (how to dismantle subordination?) while also imposing costs on those denied preferential access. The structure is tangled: the same race-consciousness that corrects historical exclusion redistributes current opportunity. Suppression is substantial (0.68) because the reading's persistence depends on active enforcement against the colorblind reading—judicial decisions must block colorblind challenges, legislation must defend remedial programs, and institutional culture must maintain that race-consciousness is constitutionally required, not merely permitted. Theater ratio rises from 0.25 to 0.41 because over the measured interval (roughly 1970–2020), the remedial framing increasingly becomes performative: institutions adopt race-conscious language and programs while actual remediation of subordination—wealth gaps, educational outcomes, political power—remains incomplete. The rise in theater_ratio reflects growing gap between the stated remedial goal and the constraint's actual operation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state remedial actors) experiences this constraint as enabling their duty to dismantle subordination—they compute it as a genuine coordination mechanism supported by constitutional authority. The payer (historically privileged groups) experiences it as extractive redistribution—they compute it as suppression of their rights. Historically subordinated groups experience it as partial correction—they benefit from the programs but often find the remediation inadequate to the depth of prior subordination. The colorblind reading's advocates experience the constraint as a constitutional wrong—they are excluded by the reading's core axiom and have no seat at the table. The engine computes different types from each seat because the structural relationships differ: the state actor sits in an agenda-setting beneficiary position (d near 0.0); the historically privileged sit in a target position (d near 1.0); historically subordinated groups sit ambiguously (benefits from remediation but constrained exit, potential d around 0.4–0.6 depending on exit elasticity).
 *
 * DIRECTIONALITY LOGIC:
 *   The remedial reading inverts the beneficiary/victim structure compared to the colorblind reading. Under colorblind logic, race-consciousness harms those denied race-preferential access; under remedial logic, race-consciousness corrects harms from prior subordination. This reading declares historically subordinated groups as beneficiaries (they receive remedial advantage) and historically privileged groups as payers (they bear the cost of denied preference). The state is the agenda-setter (it designs and enforces remedial programs). Directionality for historically subordinated groups is ambiguous (they benefit but are constrained by the reading's dependence on state action—d potentially 0.35–0.45); for historically privileged groups it is near target (d 0.75–0.85, they bear concentrated costs from denial of preference); for the state it is ambiguous beneficiary (d 0.2–0.3, it collects legitimacy from remediation but must expend enforcement energy against colorblind challenges).
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading avoids mislabeling by anchoring the classification in structural asymmetry: the state actively enforces race-consciousness against resistance (suppression = 0.68), the beneficiary/victim structure is clearly declared (historically subordinated as beneficiaries, historically privileged as payers), and the coordination function is real but partial (dismantle caste-like subordination—a genuine coordination problem—while also redistributing educational and contracting opportunity). The constraint is not pure snare (the remedial mission is genuine, not mere cover for extraction) and not pure rope (the redistribution of opportunity is asymmetric, benefiting one group at cost to another, and depends on active suppression of the colorblind reading). Tangled_rope fits: the remedial reading solves a coordination problem (how to undo systematic legal subordination) while imposing asymmetric costs (denial of preferential access) on those who did not participate in creating the subordination. This is mandatorily entangled—you cannot solve the coordination problem without imposing some cost on currently-advantaged groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_adequacy_vs_extraction,
    'Does race-conscious remedial action actually dismantle subordination, or does it merely redistribute opportunity within unchanged hierarchies of wealth and power?',
    'Longitudinal empirical analysis: do remedial programs produce convergence in wealth, educational outcomes, political power, and health across racial groups, or do they merely shuffle who holds advantage while systemic gaps persist? Does the state''s investment in remediation match the depth of historical subordination?',
    'If remedial programs substantively dismantle subordination, the constraint is a genuine tangled_rope with real coordination function and justified asymmetric cost. If they merely redistribute visible opportunity while structural subordination persists, the constraint may be better classified as snare-with-window-dressing (extraction using remedial framing as cover). This is the core empirical question that determines whether the reading''s claim to remediation is honest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedial_adequacy_vs_extraction, empirical, 'Whether remedial programs functionally dismantle subordination or redistribute within unchanged hierarchies').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the remedial reading''s core axiom (race-consciousness is required for equal protection) logically foreclose the colorblind reading, or do the two coexist as contested positions in the same constitutional framework?',
    'Conceptual/constitutional analysis: Can a single framework hold both axioms (race-consciousness forbidden AND race-consciousness required) simultaneously, or does accepting one require rejecting the other? Is this a logical contradiction or a value disagreement within a shared framework?',
    'If the axioms logically foreclose each other (a single coherent constitutional reading cannot hold both), the relation is forecloses and the two readings fight for exclusive authority. If they coexist because different parties hold different readings within the same constitutional conversation, the relation is coexists_with and both remain live. This determines the structural pressure between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the remedial and colorblind readings are logically incompatible or can coexist within the same constitutional discourse').

omega_variable(
    state_actor_beneficiary_status,
    'Are state remedial actors genuine beneficiaries of the constraint (collecting legitimacy and institutional power), or are they constrained agents executing a constitutional duty without capturing value?',
    'Institutional analysis: Do state actors that implement remedial programs accumulate political capital, institutional prestige, or budgetary resources? Or are they merely instruments of a subordinated group''s agency, collecting no independent benefit? Does abandonment of remedial programs cause institutional loss for the state actors?',
    'If state actors are genuine beneficiaries, the constraint''s beneficiary set (historically_subordinated_racial_groups + state_remedial_actors) is correctly declared and the directionality derivation holds. If state actors are merely executing instruments, they should be reclassified from beneficiary to observer, and the constraint''s extraction profile may shift. This affects whether the agenda_setter can be simultaneously a beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_actor_beneficiary_status, empirical, 'Whether state remedial actors capture independent benefit from the constraint or merely execute a duty').

omega_variable(
    reading_instantiation_vs_kernel_neutrality,
    'Is this JSON instantiating ONE coherent reading (remedial_reading) with a stable ε and beneficiary/victim structure, or is the supposed ''remedial reading'' actually a composite of multiple incoherent positions masquerading as one?',
    'Consistency check: Do the authored beneficiary/victim declarations, the founding_problem, the suppression_requirement, and the commentary cohere into a single ε-invariant constraint? Or do they oscillate between treating remediation as coordination-solving and treating it as zero-sum redistribution—thus conflating two different constraints?',
    'If the reading is internally coherent, the JSON is a valid kernel reading story. If it conflates two readings (pure remediation-seeking, which might be pure rope; zero-sum redistribution, which might be snare), the story should be split into two separate constraints per the ε-invariance principle. This is a meta-omega about the story''s own coherence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_instantiation_vs_kernel_neutrality, conceptual, 'Whether the remedial reading is a coherent constraint or conflates multiple incoherent positions').

omega_variable(
    historical_vs_forward_looking_remedy,
    'Does the remedial reading justify race-consciousness ONLY as backward-looking compensation for prior subordination, or also as forward-looking correction of ongoing subordination?',
    'Textual analysis of remedial-reading jurisprudence and philosophy: do remedial justifications cite historical wrongs (slavery, Jim Crow) or ongoing disparities (present wealth gaps, residential segregation)? Can remediation continue indefinitely (until disparities close) or does it sunset when historical wrong is compensated?',
    'If remediation is purely backward-looking, it should sunset when historical wrong is compensated—indefinite race-consciousness might not be justified. If it is forward-looking (ongoing subordination requires ongoing remediation), the constraint has no natural termination condition and suppression_requirement may rise over time as the tension between permanence and compensation grows. This affects temporal dynamics and the founding_problem_status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_vs_forward_looking_remedy, conceptual, 'Whether remedial race-consciousness is justified by historical wrong (backward-looking) or ongoing subordination (forward-looking)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__remedial_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(equa_tr_t8, equal_protection_commitment__remedial_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(equa_tr_t16, equal_protection_commitment__remedial_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(equa_tr_t24, equal_protection_commitment__remedial_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(equa_tr_t32, equal_protection_commitment__remedial_reading, theater_ratio, 32, 0.43).
narrative_ontology:measurement(equa_tr_t40, equal_protection_commitment__remedial_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(equa_tr_t50, equal_protection_commitment__remedial_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__remedial_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(equa_be_t8, equal_protection_commitment__remedial_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(equa_be_t16, equal_protection_commitment__remedial_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(equa_be_t24, equal_protection_commitment__remedial_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement(equa_be_t32, equal_protection_commitment__remedial_reading, base_extractiveness, 32, 0.51).
narrative_ontology:measurement(equa_be_t40, equal_protection_commitment__remedial_reading, base_extractiveness, 40, 0.49).
narrative_ontology:measurement(equa_be_t50, equal_protection_commitment__remedial_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__remedial_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(equa_su_t8, equal_protection_commitment__remedial_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(equa_su_t16, equal_protection_commitment__remedial_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(equa_su_t24, equal_protection_commitment__remedial_reading, suppression_requirement, 24, 0.68).
narrative_ontology:measurement(equa_su_t32, equal_protection_commitment__remedial_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(equa_su_t40, equal_protection_commitment__remedial_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement(equa_su_t50, equal_protection_commitment__remedial_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(equal_protection_commitment__remedial_reading, 0.12).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the equal_protection_commitment kernel. The kernel (the text and doctrine of the 14th Amendment's Equal Protection Clause) is stable; different readings instantiate different constraints with different ε values and beneficiary/victim structures. The remedial_reading (this story) emphasizes subordination-dismantling and race-consciousness as required correction. The colorblind_reading (separate story) emphasizes formal equality and race-neutrality as the constitutional mandate. The diversity_reading (separate story) permits race-consciousness for diversity-seeking without grounding it in remediation of prior subordination. All three affect each other because they compete for judicial and legislative authority over the same constitutional text. The remedial reading influences the colorblind reading by establishing that race-consciousness can be constitutionally required, shifting the burden on colorblind advocates to show that such requirements are illegitimate. The remedial reading coexists with the diversity reading because both permit race-consciousness but for different reasons. The remedial reading and colorblind reading may foreclose each other because their core axioms (race-consciousness required vs. forbidden) appear to contradict.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
