% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__phonics_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__phonics_reading
 *   human_readable: Phonics-First Instructional Mandate (Decoding-Before-Text Sequencing)
 *   domain: educational psychology/literacy pedagogy/cognitive science
 *
 * SUMMARY:
 *   A legislative and administrative movement requires early-grade reading
 *   instruction to begin with explicit, systematic teaching of
 *   phoneme-grapheme correspondences, sequenced before and alongside
 *   connected-text work, with decoding treated as the gateway skill that
 *   comprehension instruction presupposes. The arrangement operates through
 *   state reading laws, approved curriculum and training lists, licensure
 *   requirements, administrator fidelity checks, and scripted program
 *   materials. Its ε referent is the standing mandate arrangement itself,
 *   assessed by this reading's own lights: the instructional core is endorsed
 *   as effective skill-building that reduces decoding failure, while the
 *   enforcement apparatus — fidelity policing, exclusive procurement,
 *   displacement of teacher judgment — carries the arrangement's measurable
 *   costs. The claim and the metrics are independent authored facts: the type
 *   claim is made from the structural data (genuine coordination function,
 *   asymmetric costs, active enforcement); the metrics describe observed
 *   operation and were not tuned toward any predicted engine verdict.
 *
 * KEY AGENTS:
 *   - students_with_weak_phonological_awareness: primary beneficiary (powerless/trapped) — receives explicit decoding instruction they would not reliably get otherwise
 *   - classroom_teachers: primary payer (moderate/constrained) — deliver scripted sequences under fidelity monitoring; professional judgment subordinated to program pacing
 *   - state_legislatures_and_education_agencies: agenda setter (institutional/mobile) — write, fund, and enforce the mandate
 *   - phonics_curriculum_vendors: secondary beneficiary (organized/arbitrage) — collect procurement revenue scaled to mandate breadth and strictness
 *   - parents_of_struggling_readers: beneficiary-advocates (organized/constrained) — campaigned for the mandate; obtain early identification and explicit instruction for their children
 *   - advanced_early_readers: minor payers (powerless/trapped) — sit through pacing set by the program rather than their own reading
 *   - teacher_educators_in_literacy: excluded (institutional/constrained) — their text-immersion pedagogy displaced by licensure rules without a seat in mandate drafting
 *   - reading_science_researchers: analytical observers (institutional/analytical) — produced the evidence base; divided over its translation into procurement and policing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, 0.55).
domain_priors:suppression_score(literacy_acquisition_kernel__phonics_reading, 0.65).
domain_priors:theater_ratio(literacy_acquisition_kernel__phonics_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__phonics_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__phonics_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__phonics_reading, "Phonics-First Instructional Mandate (Decoding-Before-Text Sequencing)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__phonics_reading, "educational psychology/literacy pedagogy/cognitive science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__phonics_reading, '5efd0230-2952-4189-9113-aef250c2df24').
narrative_ontology:cs_kernel_codification('5efd0230-2952-4189-9113-aef250c2df24', distributed).
narrative_ontology:cs_authority_grounding('5efd0230-2952-4189-9113-aef250c2df24', expertise).
narrative_ontology:cs_interpretation_layer_present('5efd0230-2952-4189-9113-aef250c2df24').
narrative_ontology:cs_reading_relation('5efd0230-2952-4189-9113-aef250c2df24', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('5efd0230-2952-4189-9113-aef250c2df24', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('5efd0230-2952-4189-9113-aef250c2df24', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('5efd0230-2952-4189-9113-aef250c2df24', foundational, decoding_precedes_connected_text_comprehension).
narrative_ontology:cs_axiom_status(decoding_precedes_connected_text_comprehension, holdable).
narrative_ontology:cs_axiom_grounding('5efd0230-2952-4189-9113-aef250c2df24', decoding_precedes_connected_text_comprehension, empirically_contingent).
narrative_ontology:cs_axiom('5efd0230-2952-4189-9113-aef250c2df24', foundational, explicit_phoneme_grapheme_instruction_necessary_for_most_children).
narrative_ontology:cs_axiom_status(explicit_phoneme_grapheme_instruction_necessary_for_most_children, holdable).
narrative_ontology:cs_axiom_grounding('5efd0230-2952-4189-9113-aef250c2df24', explicit_phoneme_grapheme_instruction_necessary_for_most_children, empirically_contingent).
narrative_ontology:cs_axiom('5efd0230-2952-4189-9113-aef250c2df24', secondary, instructional_fidelity_justifies_scripted_delivery).
narrative_ontology:cs_axiom_status(instructional_fidelity_justifies_scripted_delivery, holdable).
narrative_ontology:cs_axiom_grounding('5efd0230-2952-4189-9113-aef250c2df24', instructional_fidelity_justifies_scripted_delivery, instrumental).
narrative_ontology:cs_reference_frame('5efd0230-2952-4189-9113-aef250c2df24', systematic_decoding_first_sequenced_instruction).
narrative_ontology:cs_drift_state('5efd0230-2952-4189-9113-aef250c2df24', contemporary_science_of_reading_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('5efd0230-2952-4189-9113-aef250c2df24', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__phonics_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, parents_of_struggling_readers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, phonics_curriculum_vendors).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, classroom_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__phonics_reading, advanced_early_readers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__phonics_reading, advanced_early_readers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, simple_view_of_reading).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, ehri_phases_of_word_reading).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__phonics_reading, national_reading_panel_phonics_findings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Children who do not intuitively connect speech sounds to letters. Under the mandate they receive explicit, sequenced instruction in those connections from the first years of school, with progress checked against common benchmarks. They choose neither the curriculum nor the school; what reaches them is whatever the district adopted.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, students_with_weak_phonological_awareness, beneficiary,
    powerless, biographical, trapped, national).

% Children who already decode fluently when formal instruction begins. They sit through sequenced phonics lessons calibrated to the class midpoint; the program's pacing, not their own reading, sets their instructional diet. Differentiation and acceleration are left to individual teachers operating inside fidelity expectations.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, advanced_early_readers, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__phonics_reading, advanced_early_readers, beneficiary).

% Deliver mandated phonics lessons in a fixed order, often from a script, with administrators checking fidelity to the sequence. Their judgments about when a child needs something other than the next lesson are subordinate to program pacing. Leaving the mandate while staying in the classroom is not possible; leaving the profession carries salary, pension, and identity costs.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, classroom_teachers, payer,
    moderate, biographical, constrained, national).

% Pass reading laws, maintain approved curriculum and training lists, fund professional development, and direct agencies to monitor implementation. They respond to reading scores, advocacy campaigns, and model legislation; they can tighten or amend the mandate each session.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, state_legislatures_and_education_agencies, agenda_setter,
    institutional, generational, mobile, national).

% Publish scripted programs, decodable texts, and training services. Inclusion on state-approved lists converts the mandate into guaranteed demand; revenue scales with the number of jurisdictions adopting and the strictness of fidelity requirements.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, phonics_curriculum_vendors, beneficiary,
    organized, biographical, arbitrage, national).

% Organized advocacy chapters that campaigned for the mandate after children failed under earlier approaches. They obtain explicit decoding instruction and early screening identification for their children when schools must provide them; their alternatives — private tutoring, homeschooling — are costly.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, parents_of_struggling_readers, beneficiary,
    organized, biographical, constrained, national).

% University faculty whose courses taught text-immersion and cueing-based approaches. Legislation and licensure rules now displace their pedagogy in the preparation pipeline; they had little seat in the legislative drafting that redefined what new teachers must be taught.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, teacher_educators_in_literacy, excluded,
    institutional, generational, constrained, national).

% Produce the evidence base the mandate cites — word-reading phase models, comprehension frameworks, meta-analyses of instructional effects. Many welcome the policy attention; some object that the science is being translated into exclusive procurement and fidelity policing beyond what the evidence supports. They analyze; they do not run classrooms.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__phonics_reading, reading_science_researchers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__phonics_reading, phonics_curriculum_vendors).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__phonics_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem that a substantial share of children do not infer letter-sound relationships incidentally and fail to decode without explicit instruction: a common, sequenced phonics scope and series ensures every child receives systematic decoding instruction, makes struggling readers identifiable early against a shared benchmark, and gives schools a curriculum that can be taught, monitored, and evaluated at scale.
% TRANSFER_FUNCTION: Moves instructional authority from classroom teachers and teacher-preparation traditions to legislatively mandated program sequences; moves daily instructional time toward decoding practice ahead of or in place of connected-text work; moves public funds to approved curricula, decodable texts, teacher training, and fidelity monitoring, where vendor contracts scale with mandate breadth.
% ABSENT_VOICES: Teacher-educators trained in text-immersion and cueing-based traditions would contest the sequencing claim and the fidelity-enforcement apparatus; children have no seat in curriculum adoption; experienced teachers who adapted instruction to individual readers were rarely consulted in the legislative rooms where mandates were drafted.
% DISAPPEARANCE_RATIONALE: If the phonics-first mandate vanished overnight, state-approved curriculum lists, teacher-training mandates, vendor markets built on adoption contracts, fidelity-monitoring routines, and the daily lesson structure of early-grade classrooms would all reorganize; early reading instruction would revert to jurisdiction-by-jurisdiction contest among the rival instructional doctrines.
% FOUNDING_PROBLEM: Widespread reading failure: large fractions of children — concentrated among those without home literacy advantages and among students with dyslexia — were failing to acquire decoding under incidental, text-immersion approaches, producing downstream academic failure that was expensive to remediate late.
% FOUNDING_PROBLEM_CORROBORATION: NAEP and PIRLS score trends attest persistent reading failure from outside the benefiting parties; dyslexia advocacy organizations and district remediation caseloads corroborate; researchers across all the rival instructional traditions agree that children fail to acquire decoding — they contest the fix, not the existence of the problem.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__phonics_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__phonics_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55: the mandate's costs concentrate on teacher professional judgment (scripted delivery, fidelity monitoring, approved-list procurement) while the student-facing skill-building function genuinely reduces decoding failure — the expected structural delta is high extraction on the teacher seat and low extraction on the student seat, and the blended ε reflects that asymmetry. Suppression (0.65) is structural and unscaled: state statutes, approved-curriculum lists, licensure rules, and administrator fidelity walkthroughs leave classroom teachers no lawful alternative delivery while they hold the job. Theater ratio (0.25) is low-moderate: the instruction itself is functionally real, but a growing share of activity is compliance documentation and checkbox professional development. Accessibility collapse (0.50): alternatives are legally foreclosed in mandate states yet remain live in others and in the rival doctrines' institutional bases — the contest is unresolved, so alternatives are suppressed, not collapsed. Resistance (0.60): teacher pushback, defense of the rival pedagogies, and ongoing academic contest are real and organized. All three tracked metrics share one time grid (T=0≈2000 National Reading Panel era; T=4≈2004 Reading First; T=8≈2008 Reading First backlash and inspector-general controversies; T=13≈2013 first state reading/dyslexia laws; T=18≈2018 science-of-reading media wave; T=22≈2022 cueing bans and approved lists; T=26≈2026 mandate wave at roughly forty states), with every metric authored at every point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the classroom-teacher seat the arrangement operates as subordinated judgment: the same script the legislature calls fidelity is, from inside the classroom, the removal of the decision the profession was trained to make. From the weak-phonological-awareness student seat it operates as subsidy: instruction that materially lowers the odds of reading failure. From the vendor seat it is demand guarantee. From the legislature seat it is legible, monitorable delivery of a public good. Two non-agenda seats diverge on exit: vendors hold arbitrage (they sell across jurisdictions and re-tool products to each approved list), while teachers hold only costly constrained exit (leave the profession), so the derivation should place them at opposite ends despite both being outside the drafting rooms.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: students with weak phonological awareness (primary), parents of struggling readers, phonics curriculum vendors. Victims declared: classroom teachers (professional judgment), advanced early readers (pacing costs). The derivation should place weak-phonological-awareness students near the beneficiary end (they pay nothing and receive the instruction), vendors near the beneficiary end amplified by arbitrage exit, teachers near the target end amplified by constrained exit and daily enforcement contact, and advanced readers slightly above symmetric (small pacing cost, real residual benefit from consolidated review). The agenda setter's position is structural rather than rent-collecting: legislatures bear no extraction and collect compliance, not revenue. Vindicated propositions (the simple view of reading, Ehri's phase model, the National Reading Panel phonics findings) are listed as vindicated propositions, not beneficiaries — doctrines collect no rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass decoding failure under text-immersion instruction — is live and corroborated by assessment trends from outside the benefiting parties, so no mandatrophy is declared. The classification discipline cuts both ways: reading the mandate as pure extraction would erase the genuine skill-building function that measurably reduces decoding failure for the students the arrangement was built for; reading it as pure coordination would erase the teacher-judgment costs and vendor rents that its enforcement machinery actively sustains. The fidelity-versus-flexibility omega carries the live question of whether the teacher-facing cost is the price of the function or gratuitous overhead — the point at which this arrangement would drift toward or away from pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which reading of the literacy_acquisition_kernel should govern early reading instruction? This story is authored as the phonics_reading; what would each sibling reading change structurally if adopted in its place?',
    'Longitudinal, population-scale literacy outcomes under each instructional regime, with pre-registered outcome definitions agreed across the rival reading communities.',
    'Adopting whole_language_reading would dissolve the mandate and its teacher-facing enforcement entirely; adopting balanced_literacy_reading would keep systematic phonics but remove strict sequencing and fidelity enforcement, reducing extraction from teacher judgment; adopting structured_literacy_reading would broaden the mandate across five instructional strands and redistribute extraction toward teacher-training capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer structure: this constraint is one reading of the literacy kernel; sibling readings are separate constraints with their own ε.').

omega_variable(
    fidelity_vs_flexibility_causal_weight,
    'How much of the student outcome benefit attributed to the mandate depends on script-fidelity enforcement, versus surviving under trained teachers delivering the same sequence flexibly?',
    'Randomized or well-matched comparisons of scripted-fidelity delivery against trained-but-adaptive delivery of the same scope and sequence.',
    'If flexibility preserves outcomes, the cost imposed on teacher professional judgment is largely gratuitous overhead and the arrangement sits nearer pure coordination; if fidelity is causal, the teacher-facing cost is the price of the function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_vs_flexibility_causal_weight, empirical, 'Whether scripting is causally necessary or an enforcement preference.').

omega_variable(
    science_mandate_decomposition,
    'Does the measured extraction belong to the instructional claim (explicit, sequenced decoding instruction) or to the governance apparatus (approved lists, procurement, fidelity policing, cueing bans)?',
    'Decompose and evaluate separately: jurisdictions adopting the instructional sequence without exclusive procurement or fidelity policing, versus full-mandate jurisdictions.',
    'If the instructional core performs with low extraction while the apparatus carries the extraction, the constraint family splits into a low-ε instructional standard and a high-ε enforcement-and-procurement arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(science_mandate_decomposition, conceptual, 'ε-invariance guard: the label ''phonics mandate'' bundles a science claim with a procurement-and-policing arrangement.').

omega_variable(
    early_drill_motivation_cost,
    'Does sustained early decoding drill measurably reduce reading motivation or engagement for some students — the whole-language reading''s central objection — and if so, for whom and at what scale?',
    'Motivation and voluntary-reading measures collected alongside decoding outcomes in mandate jurisdictions, disaggregated by entering skill.',
    'If the cost is substantial for identifiable student groups, the student-side beneficiary picture is partial and effective extraction on those students is understated by the current authoring.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(early_drill_motivation_cost, empirical, 'Whether the reading''s weakest flank — the motivation cost of early drill — is real and material.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__phonics_reading, 0, 26).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__phonics_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(lite_tr_t0, observed).
narrative_ontology:measurement(lite_tr_t4, literacy_acquisition_kernel__phonics_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement_basis(lite_tr_t4, observed).
narrative_ontology:measurement(lite_tr_t8, literacy_acquisition_kernel__phonics_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(lite_tr_t8, observed).
narrative_ontology:measurement(lite_tr_t13, literacy_acquisition_kernel__phonics_reading, theater_ratio, 13, 0.2).
narrative_ontology:measurement_basis(lite_tr_t13, observed).
narrative_ontology:measurement(lite_tr_t18, literacy_acquisition_kernel__phonics_reading, theater_ratio, 18, 0.23).
narrative_ontology:measurement_basis(lite_tr_t18, observed).
narrative_ontology:measurement(lite_tr_t22, literacy_acquisition_kernel__phonics_reading, theater_ratio, 22, 0.26).
narrative_ontology:measurement_basis(lite_tr_t22, observed).
narrative_ontology:measurement(lite_tr_t26, literacy_acquisition_kernel__phonics_reading, theater_ratio, 26, 0.25).
narrative_ontology:measurement_basis(lite_tr_t26, observed).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(lite_be_t0, observed).
narrative_ontology:measurement(lite_be_t4, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement_basis(lite_be_t4, observed).
narrative_ontology:measurement(lite_be_t8, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement_basis(lite_be_t8, observed).
narrative_ontology:measurement(lite_be_t13, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 13, 0.44).
narrative_ontology:measurement_basis(lite_be_t13, observed).
narrative_ontology:measurement(lite_be_t18, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement_basis(lite_be_t18, observed).
narrative_ontology:measurement(lite_be_t22, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 22, 0.53).
narrative_ontology:measurement_basis(lite_be_t22, observed).
narrative_ontology:measurement(lite_be_t26, literacy_acquisition_kernel__phonics_reading, base_extractiveness, 26, 0.55).
narrative_ontology:measurement_basis(lite_be_t26, observed).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(lite_su_t0, observed).
narrative_ontology:measurement(lite_su_t4, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement_basis(lite_su_t4, observed).
narrative_ontology:measurement(lite_su_t8, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement_basis(lite_su_t8, observed).
narrative_ontology:measurement(lite_su_t13, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 13, 0.48).
narrative_ontology:measurement_basis(lite_su_t13, observed).
narrative_ontology:measurement(lite_su_t18, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement_basis(lite_su_t18, observed).
narrative_ontology:measurement(lite_su_t22, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 22, 0.65).
narrative_ontology:measurement_basis(lite_su_t22, observed).
narrative_ontology:measurement(lite_su_t26, literacy_acquisition_kernel__phonics_reading, suppression_requirement, 26, 0.65).
narrative_ontology:measurement_basis(lite_su_t26, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__phonics_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__phonics_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'how reading is acquired' decomposes into four structurally distinct instructional mandates, one per reading of the literacy_acquisition_kernel. This story authors the phonics_reading only: its ε reflects the phonics-first sequencing mandate as this reading assesses it — genuine decoding instruction with substantial costs borne by teacher professional judgment and vendor rents riding on enforcement. The whole-language sibling would carry near-zero instructional mandate (its constraint dissolves the apparatus); the balanced-literacy sibling keeps phonics but drops strict sequencing and fidelity enforcement; the structured-literacy sibling broadens the mandate across five instructional strands. The siblings are linked here as one constraint family; ε is not averaged across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
