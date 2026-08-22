% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__balanced_literacy_integration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__balanced_literacy_integration, []).

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
 *   constraint_id: reading_acquisition_legitimacy__balanced_literacy_integration
 *   human_readable: Balanced Literacy Integration in Reading Instruction
 *   domain: education/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   Reading instruction is contested between three major schools:
 *   phonics-first advocates argue that explicit, systematic decoding
 *   instruction is the foundational prerequisite and that authentic
 *   literature must wait; whole-language advocates argue that reading is
 *   inherently about meaning-making and that decoding emerges naturally from
 *   engagement with real texts; and integrationist (balanced literacy)
 *   advocates argue that legitimate instruction must honor both
 *   pathways—explicit phonics for decoding skill, authentic literature for
 *   meaning-making and engagement—and that teachers should be able to toggle
 *   between direct instruction and facilitated exploration based on learner
 *   needs. This constraint story instantiates the balanced-literacy reading.
 *   The kernel is stable (reading requires both processes); the contest is
 *   over which process is primary and how to weight instructional time. The
 *   claim/metric gap is intentional: the constraint is CLAIMED as
 *   tangled_rope (genuine coordination of heterogeneous learner needs +
 *   legitimate flexibility) while the authored metrics show substantial
 *   suppression (constraint requires active enforcement against phonics-first
 *   and whole-language mandates) and measurable theater (the rhetoric of
 *   integration sometimes masks unchanged underlying single-method practice).
 *   The measurement series documents a rising trajectory in extractiveness
 *   and theater, plateauing around t=18, suggesting that as the
 *   balanced-literacy mandate hardens into policy (without corresponding
 *   teacher training and assessment alignment), it increasingly operates as a
 *   layer of administrative control rather than a license for genuine
 *   method-switching.
 *
 * KEY AGENTS:
 *   - Educational researchers (integrationist school): set the agenda for balanced-literacy research and professional development; control research synthesis and policy influence pathways; high institutional power and exit options.
 *   - Teachers practicing balanced literacy: the primary implementers; face conflicting accountability pressures from assessment systems designed for single methods; benefit from instructional flexibility but pay the cost of method coordination.
 *   - Struggling readers in both prior regimes: bore the cost of being forced into one method exclusively; benefit from access to the balanced approach but trapped in dependent learner status.
 *   - Phonics-first and whole-language advocates: excluded from the balanced-literacy agenda but maintain parallel research and advocacy infrastructure; would argue the constraint falsely equates their positions.
 *   - School administrators: enforce district mandates; in balanced-literacy districts, must coordinate mixed implementation; constrained by competing assessment and accountability signals.
 *   - Standardized assessment systems: de facto shape legitimacy of instructional approaches through what they measure; observational seat with significant indirect power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, 0.58).
domain_priors:suppression_score(reading_acquisition_legitimacy__balanced_literacy_integration, 0.62).
domain_priors:theater_ratio(reading_acquisition_legitimacy__balanced_literacy_integration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__balanced_literacy_integration, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__balanced_literacy_integration, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__balanced_literacy_integration, "Balanced Literacy Integration in Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__balanced_literacy_integration, "education/cognitive_science/literacy_pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__balanced_literacy_integration).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__balanced_literacy_integration, '14a386ee-125f-4992-bfd5-67010a443bc9').
narrative_ontology:cs_kernel_codification('14a386ee-125f-4992-bfd5-67010a443bc9', formalized).
narrative_ontology:cs_authority_grounding('14a386ee-125f-4992-bfd5-67010a443bc9', expertise).
narrative_ontology:cs_interpretation_layer_present('14a386ee-125f-4992-bfd5-67010a443bc9').
narrative_ontology:cs_reading_relation('14a386ee-125f-4992-bfd5-67010a443bc9', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('14a386ee-125f-4992-bfd5-67010a443bc9', reading_acquisition_legitimacy__whole_language_meaning_primacy, coexists_with).
narrative_ontology:cs_reading_relation('14a386ee-125f-4992-bfd5-67010a443bc9', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('14a386ee-125f-4992-bfd5-67010a443bc9', foundational, reading_requires_both_decoding_and_meaning_making).
narrative_ontology:cs_axiom_status(reading_requires_both_decoding_and_meaning_making, holdable).
narrative_ontology:cs_axiom_grounding('14a386ee-125f-4992-bfd5-67010a443bc9', reading_requires_both_decoding_and_meaning_making, empirically_contingent).
narrative_ontology:cs_axiom('14a386ee-125f-4992-bfd5-67010a443bc9', foundational, instructional_method_should_match_learner_needs_not_universal_sequence).
narrative_ontology:cs_axiom_status(instructional_method_should_match_learner_needs_not_universal_sequence, holdable).
narrative_ontology:cs_axiom_grounding('14a386ee-125f-4992-bfd5-67010a443bc9', instructional_method_should_match_learner_needs_not_universal_sequence, empirically_contingent).
narrative_ontology:cs_reference_frame('14a386ee-125f-4992-bfd5-67010a443bc9', integrated_dual_process_reading_model).
narrative_ontology:cs_drift_state('14a386ee-125f-4992-bfd5-67010a443bc9', contemporary_assessment_accountability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('14a386ee-125f-4992-bfd5-67010a443bc9', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, teachers_practicing_balanced_literacy).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, educational_researchers_integrationist_school).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers_in_decodable_first_systems).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers_in_whole_language_systems).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers_managing_method_conflicts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_reader_families).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, teachers_practicing_balanced_literacy).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_reader_families).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, reading_involves_both_decoding_and_comprehension).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__balanced_literacy_integration, learner_heterogeneity_requires_instructional_flexibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopt a mixed approach: use explicit phonics instruction for decoding while also exposing students to authentic literature for meaning-making. They benefit from access to research-aligned flexibility and are freed from dogmatic adherence to a single method. They simultaneously pay through increased planning complexity, resource fragmentation, and accountability pressure from divergent evaluation systems that expect phonics-only or whole-language-only outcomes.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, teachers_practicing_balanced_literacy, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, teachers_practicing_balanced_literacy, payer).

% In systems that prioritize phonics-first instruction without adequate meaning-making exposure, these readers bore the cost of narrow decodable-text environments that did not attend to their comprehension needs and engagement with real literature. The balanced approach claims to remediate this but exists in tension with the phonics-first mandate.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers_in_decodable_first_systems, payer,
    powerless, biographical, trapped, national).

% In systems that prioritize authentic literature and meaning-making without explicit phonics instruction, these readers bore the cost of inadequate explicit decoding instruction, limiting their ability to access unfamiliar words independently. The balanced approach claims to remediate this but exists in tension with the whole-language mandate.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_readers_in_whole_language_systems, payer,
    powerless, biographical, trapped, national).

% Teachers in schools transitioning to or implementing balanced literacy face conflicting accountability pressures: phonics-focused assessment systems, whole-language district policies, divergent parent expectations, and insufficient professional development for method switching. They bear the cost of navigating irreconcilable mandates.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, classroom_teachers_managing_method_conflicts, payer,
    moderate, biographical, constrained, national).

% Frame and legitimize the balanced literacy model through research synthesis, grant funding, and policy influence. They set the agenda for which instructional practices are certified as evidence-based, control access to professional development pathways, and benefit from institutional authority and research funding tied to this framing. They have high exit options through academic mobility.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, educational_researchers_integrationist_school, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, educational_researchers_integrationist_school, beneficiary).

% Researchers and policy advocates who argue systematic phonics is the primary legitimate basis for reading instruction. They are excluded from setting the agenda in balanced-literacy frameworks and would argue for phonics-first primacy and decoding as the foundational constraint. They maintain parallel research and advocacy infrastructure.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, phonics_first_advocates, excluded,
    institutional, generational, arbitrage, global).

% Researchers and educators who argue authentic literature and natural meaning-making are the legitimate basis for reading instruction. They are excluded from setting the agenda in balanced-literacy frameworks and would argue meaning-making is primary. They maintain parallel instructional communities and publishing infrastructure.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, whole_language_advocates, excluded,
    institutional, generational, arbitrage, global).

% Implement and enforce whichever literacy approach district policy mandates. In balanced-literacy districts they must coordinate mixed-method implementation, manage teacher training, and reconcile divergent assessment systems. In non-balanced districts they enforce alternative mandates and may actively suppress balanced approaches.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, school_administrators_and_district_officials, agenda_setter,
    institutional, biographical, constrained, regional).

% Children and families experiencing reading difficulty. They benefit from access to well-implemented balanced instruction that meets their specific decoding and comprehension needs. They pay through ongoing remediation burden, potential retention/stigma, and limited agency in instructional design choices.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_reader_families, payer,
    powerless, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__balanced_literacy_integration, struggling_reader_families, beneficiary).

% Design and administer reading assessments that may emphasize decoding skills, comprehension, or a mixed model depending on the assessment's own theory. Their design choices create de facto legitimacy pressures on instructional approaches and produce measurement signals that feed accountability systems.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__balanced_literacy_integration, standardized_assessment_system_operators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__balanced_literacy_integration, educational_researchers_integrationist_school).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__balanced_literacy_integration, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the resolution of a foundational pedagogical dispute: whether reading instruction should prioritize explicit decoding, authentic meaning-making, or both. The balanced approach claims to solve the coordination problem by providing an integrated framework that honors both cognitive pathways (decoding as a foundational skill; meaning-making as the functional goal). It coordinates teacher flexibility and student heterogeneity: different learners benefit from different entry points (some need explicit phonics scaffolding; others need authentic literature engagement to sustain motivation), and teachers need permission to toggle between direct instruction and facilitation.
% TRANSFER_FUNCTION: Moves instructional authority and resource allocation away from single-method advocates (phonics-first or whole-language-only) toward integrationist researchers and professional developers. Redistributes classroom time and materials from purely phonics-focused or purely literature-based sequences toward mixed structures. Shifts assessment emphasis away from either decoding-only or comprehension-only metrics toward a both/and model.
% ABSENT_VOICES: Phonics-first researchers and whole-language advocates are structurally excluded from the balanced-literacy agenda-setting layer; they would argue the constraint falsely equates their logically distinct positions and invokes false-balance to block their preferred approaches. Neurodivergent readers (dyslexia, oral language differences, processing speed differences) whose specific learning profiles may not fit the averaged both/and assumption are not prominently at the table. Families from reading-disadvantaged communities experiencing active disinvestment in their schools are not decision-makers at the agenda level.
% DISAPPEARANCE_RATIONALE: If the balanced-literacy constraint and its research legitimacy disappeared, school systems would polarize back toward phonics-first or whole-language-only mandates, with no middle-ground option available to teachers. Struggling readers would lose access to the mixed instructional approaches that this constraint makes available. Instructional time would reallocate wholesale to one method or the other, and teachers would lose flexibility to meet learner heterogeneity.
% FOUNDING_PROBLEM: Reading instruction was divided into competing dogmas—phonics advocates arguing decoding is primary, whole-language advocates arguing meaning-making is primary—and individual learners and teachers were forced to choose one paradigm exclusively. Struggling readers, particularly those who needed both explicit decoding instruction AND engaged, authentic reading experiences, fell through the gap. Teachers observed both phonics-only and whole-language-only approaches leaving learners behind and lacked research permission to integrate both.
% FOUNDING_PROBLEM_CORROBORATION: Educational researchers in the integrationist school (Pearson, Allington, Graves, others in balance-of-evidence syntheses) attest the foundational problem is still live: teachers still face method mandates that constrain their flexibility, and reading outcomes remain stratified by socioeconomic status and neurodevelopmental profile. Phonics-first advocates attest the problem was methodological confusion and that phonics-priority instruction solves it. Whole-language advocates attest phonics-first mandates solve a false problem. Meta-analyses (National Reading Panel 2000 and successors) side with integrated approaches, but the mandate-level polarization persists in schools. Families of struggling readers and classroom teachers consistently report being forced to choose methods rather than mix them.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__balanced_literacy_integration, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__balanced_literacy_integration, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__balanced_literacy_integration, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__balanced_literacy_integration, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__balanced_literacy_integration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__balanced_literacy_integration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.42) in the early phase when balanced literacy is an optional research-informed choice available to educators with professional autonomy. It rises to 0.58 by mid-interval as balanced-literacy framing becomes a district or state mandate with compliance expectations. The plateau at 0.58 in the final period reflects that extractiveness is fundamentally bounded by the nature of the constraint: it is not a simple rent-extraction (like monopoly commission) but rather an authority/flexibility tradeoff. Suppression is consistently higher than extractiveness because the constraint's persistence depends on actively suppressing or marginalizing phonics-first and whole-language-only advocates who maintain institutional presence and would overturn the balanced mandate if unopposed. Theater (meaning performative compliance) starts at 0.35 and rises to 0.48 because many districts adopt balanced-literacy language while maintaining single-method practice underneath: teachers are told to 'integrate' but given phonics-only or literature-only assessments, insufficient professional development in method-switching, or resource configurations that make genuine integration infeasible. The plateau in theater around t=18-25 reflects stabilization at a state of widespread false-balance: the constraint's language persists; the practice remains largely single-method. One shared measurement grid: every metric is authored at every examined time point (0, 3, 6, 12, 18, 25) so temporal analysis has complete data.
 *
 * PERSPECTIVAL GAP:
 *   From the integrationist researcher seat, balanced literacy is genuine coordination solving a real pedagogical problem: learners do need both decoding skill and meaning-making engagement. From the phonics-first advocate seat, balanced literacy is false-balance obscuring the truth that phonics is primary and that other methods distract from it. From the whole-language advocate seat, balanced literacy is phonics-washing: adding phonics to whole-language practice makes it less authentic and student-centered. From the struggling-reader seat, balanced literacy is potential benefit undermined by implementation fidelity: the constraint is real only if the teacher actually has flexibility and skill to switch methods; if the teacher is phonics-only or literature-only dressed in balanced language, the reader experiences the same constraint as before. The engine computes this perspectival divergence from the structural data: a struggling reader has low power and trapped exit (cannot choose schools easily), so they compute a high d (target); an integrationist researcher has institutional power and arbitrage exit, so they compute a low d (beneficiary). The agenda-setter seat (researchers setting the integrationist frame) and payer seat (struggling readers bearing the cost of method conflicts) should diverge sharply in their type classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   Integrationist researchers are structural beneficiaries: they set the agenda, control research synthesis and professional development resources, and benefit from institutional authority grounded in their framing. They have high power (institutional) and high exit options (arbitrage: they can publish, teach, consult across contexts). Their d is low (~0.2–0.3), biasing toward beneficiary end. Teachers practicing balanced literacy are near-symmetric or slightly toward target: they benefit from instructional flexibility but pay through increased cognitive load, resource fragmentation, and accountability pressure from assessment systems designed for single methods. They have moderate power and constrained exit (cannot easily leave teaching or change districts). Their d is near 0.5, possibly slightly higher (0.55–0.65) because the suppression cost of maintaining method-switching competence against implicit single-method mandates is real. Struggling readers are full targets: they have no power, trapped exit (cannot choose their school), and their outcomes depend entirely on whether the constraint is implemented with fidelity. Their d is high (~0.85–0.95). Phonics-first and whole-language advocates are excluded, not coordinated: they have institutional power and arbitrage exit (they can publish, teach, consult in their own schools), but they are not parties to the balanced-literacy arrangement; they are structural rivals whose exclusion requires active enforcement. Teachers in non-balanced districts bear a cost: they experience the balanced-literacy mandate as external pressure (state standards, research advocacy) that may contradict their district's actual mandate (phonics-first or whole-language-only). Their d is inverted (near 0.0 or negative) because the constraint extracts from them through a subsidy to researchers rather than through direct resource capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (method polarization forcing single-paradigm instruction on learners who need both) was live when the balanced-literacy reading emerged (1990s–2000s). By 2020+, the status is contested: integrationist researchers attest the problem is still live (method mandates still constrain teachers); phonics-first advocates attest the problem was solved by phonics-priority research (the Science of Reading movement); whole-language advocates attest phonics-first mandates created a worse problem. The core mandatrophy risk is: if the balanced-literacy constraint hardens into procedural compliance (balanced-literacy language + single-method practice underneath) without genuine teacher flexibility and assessment alignment, then the original founding problem (learners forced into one method) is NOT solved—it is merely obscured. The theater_ratio rising to 0.48 by t=25 suggests exactly this: widespread balanced-literacy rhetoric with underlying single-method practice. The measurement divergence (extractiveness plateaus while theater plateaus) indicates the constraint has reached a stable compromise state: it legitimizes teacher flexibility in principle while enforcement systems actually maintain single-method practice in practice. This is the piton-adjacent state: the constraint persists because integrationist researchers benefit from its authority, administrators benefit from the discourse covering method conflicts, and neither group has enough unified opposition (phonics-first and whole-language advocates are split) to overturn it; but the constraint no longer solves its founding problem for struggling readers. No party benefits enough to actively defend full implementation (teachers don't have resources for genuine flexibility; researchers have moved on to other projects); no party is hurt enough to fix it (administrators avoid the cost of assessment system redesign; struggling readers have no voice at the policy table). The constraint is thus a zombie: alive in policy language, dead in functional solving of the problem it was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    integration_implementation_fidelity,
    'When balanced literacy is mandated, do schools actually implement genuine method flexibility and teacher-directed switching, or do they adopt balanced-literacy language while maintaining underlying single-method practice?',
    'Classroom observation studies coding teacher instructional moves and method switching over time; analysis of resource allocation (decodable vs. authentic texts); assessment of teacher professional development depth and retention; comparison of student outcomes across learner profiles in nominally balanced vs. actually single-method classrooms.',
    'If implementation fidelity is high (genuine method-switching), the constraint solves its founding problem and remains a tangled_rope coordinating legitimate heterogeneity. If fidelity is low (theater ratio ≥0.6), the constraint is a piton: it has the language of coordination without the function, persists due to researcher institutional interests, and struggling readers experience no meaningful benefit. The rising theater_ratio in our measurements (0.35→0.48) suggests fidelity may be eroding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integration_implementation_fidelity, empirical, 'Whether balanced literacy is implemented as genuine method flexibility or false-balance rhetoric.').

omega_variable(
    learner_heterogeneity_grain_size,
    'Does the ''learner heterogeneity'' that balanced literacy coordinates map onto meaningful subpopulations with genuinely different optimal instructional pathways, or is the claim of heterogeneity a post-hoc cover for method indeterminacy?',
    'Randomized controlled trials comparing method-matched instruction (phonics-first for decoding-deficient readers, literature-first for meaning-making-deficient readers) to mixed balanced instruction; analysis of effect-size heterogeneity by learner profile (decoding strength, language exposure, motivation, neurodevelopment); interaction studies examining whether learner characteristics predict optimal method sequence.',
    'If meaningful heterogeneity exists and methods can be matched, balanced literacy''s integrative flexibility is structurally justified and reduces extraction (it genuinely serves diverse needs). If heterogeneity is a gloss over method indeterminacy (all learners eventually need both, and sequence doesn''t matter much), then the claim of flexibility is false and the constraint is pure agenda-setting for the integrationist school.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(learner_heterogeneity_grain_size, empirical, 'Whether learner heterogeneity justifies method flexibility or whether balanced literacy obscures that all learners need both pathways regardless of entry sequence.').

omega_variable(
    agenda_setter_rent_extraction_boundary,
    'Do integrationist researchers benefit materially from balanced-literacy mandates through funding, publishing, training contracts, or institutional authority in ways that constitute extraction beyond legitimate research influence?',
    'Analysis of funding flows to balanced-literacy research vs. phonics-first research; documentation of professional development contract distribution; examination of journal editorial boards and funding-agency leadership composition; interviews with implementers about whether researcher involvement in their adoption was based on evidence or economic interest.',
    'If material benefit is substantial and decoupled from evidence-driven superiority of balanced literacy, the agenda-setting role is extractive (high ε, beneficiary seat is agenda-setter not because coordination is real but because the constraint transfers resources to them). If material benefit is moderate and correlated with research quality, extraction is lower and the coordination function is more credible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agenda_setter_rent_extraction_boundary, empirical, 'Whether integrationist researchers'' institutional benefits constitute extraction beyond normal research influence.').

omega_variable(
    committer_frame_sibling_foreclosure,
    'Is the balanced-literacy reading logically foreclosed by either the phonics-first or whole-language reading, or do all three readings remain genuinely live options for different parties?',
    'Logical analysis: phonics-first claims decoding is primary (but does not claim meaning-making is unnecessary); whole-language claims meaning-making is primary (but does not claim decoding is unnecessary). The balanced reading claims both are necessary and method should integrate. No reading''s core premise directly contradicts another''s core premise such that they cannot coexist. This is a conceptual omega: the readings coexist as live positions held by different research communities and districts.',
    'If readings foreclose each other, the constraint is an arena of genuine zero-sum conflict and its persistence requires stronger suppression than if readings coexist. Our expectation: readings coexist_with each other, not foreclose. This routes suppression into maintenance of the boundary against rivals, not into logical elimination of rivals.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_sibling_foreclosure, conceptual, 'Whether balanced literacy logically forecloses competing readings or coexists with them as live alternatives.').

omega_variable(
    struggling_reader_benefit_attribution,
    'When balanced-literacy classrooms produce better outcomes for struggling readers, is the benefit attributable to the integrated method itself, to increased teacher attention and flexibility, to better assessment and identification, or to selection effects (only well-resourced schools adopt balanced literacy)?',
    'Comparison of balanced-literacy implementation in high-resourced vs. under-resourced schools; comparison of outcomes in schools that adopted balanced literacy vs. maintained single methods but increased teacher professional development and resources equally; mediation analysis isolating the contribution of method integration vs. resource/attention effects.',
    'If benefit is purely from integration, balanced literacy solves the founding problem for struggling readers and is a genuine tangled_rope. If benefit is from increased resources/attention/assessment that could be achieved through any method with equal investment, then balanced literacy is a vehicle for distributing attention and resources, not an inherent coordinating structure—extraction is lower because the benefit is universalizable, not locked to the balanced method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(struggling_reader_benefit_attribution, empirical, 'Whether balanced literacy''s benefit for struggling readers comes from method integration or from associated increases in resources and teacher attention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__balanced_literacy_integration, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ralbli_tr_t0, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(ralbli_tr_t0, observed).
narrative_ontology:measurement(ralbli_tr_t3, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 3, 0.4).
narrative_ontology:measurement_basis(ralbli_tr_t3, observed).
narrative_ontology:measurement(ralbli_tr_t6, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 6, 0.44).
narrative_ontology:measurement_basis(ralbli_tr_t6, observed).
narrative_ontology:measurement(ralbli_tr_t12, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 12, 0.48).
narrative_ontology:measurement_basis(ralbli_tr_t12, observed).
narrative_ontology:measurement(ralbli_tr_t18, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 18, 0.49).
narrative_ontology:measurement_basis(ralbli_tr_t18, observed).
narrative_ontology:measurement(ralbli_tr_t25, reading_acquisition_legitimacy__balanced_literacy_integration, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(ralbli_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(ralbli_be_t0, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(ralbli_be_t0, observed).
narrative_ontology:measurement(ralbli_be_t3, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 3, 0.48).
narrative_ontology:measurement_basis(ralbli_be_t3, observed).
narrative_ontology:measurement(ralbli_be_t6, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 6, 0.53).
narrative_ontology:measurement_basis(ralbli_be_t6, observed).
narrative_ontology:measurement(ralbli_be_t12, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 12, 0.58).
narrative_ontology:measurement_basis(ralbli_be_t12, observed).
narrative_ontology:measurement(ralbli_be_t18, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 18, 0.61).
narrative_ontology:measurement_basis(ralbli_be_t18, observed).
narrative_ontology:measurement(ralbli_be_t25, reading_acquisition_legitimacy__balanced_literacy_integration, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(ralbli_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(ralbli_su_t0, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(ralbli_su_t0, observed).
narrative_ontology:measurement(ralbli_su_t3, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 3, 0.54).
narrative_ontology:measurement_basis(ralbli_su_t3, observed).
narrative_ontology:measurement(ralbli_su_t6, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 6, 0.58).
narrative_ontology:measurement_basis(ralbli_su_t6, observed).
narrative_ontology:measurement(ralbli_su_t12, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 12, 0.63).
narrative_ontology:measurement_basis(ralbli_su_t12, observed).
narrative_ontology:measurement(ralbli_su_t18, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 18, 0.65).
narrative_ontology:measurement_basis(ralbli_su_t18, observed).
narrative_ontology:measurement(ralbli_su_t25, reading_acquisition_legitimacy__balanced_literacy_integration, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(ralbli_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__balanced_literacy_integration, attachment_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__balanced_literacy_integration, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__balanced_literacy_integration, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint story instantiates the balanced-literacy reading of the contested kernel reading_acquisition_legitimacy. Sibling readings (phonics_decoding_primacy, whole_language_meaning_primacy, structured_literacy_remediation) are separate constraints with different ε values, beneficiary/victim structures, and classification profiles. The balanced reading claims ~0.58 extractiveness (moderate: genuine coordination of learner heterogeneity, but substantial suppression required to maintain against phonics-first and whole-language advocates). The phonics-first reading would author much lower extractiveness (~0.25) from its perspective (genuine coordination of decoding-bottleneck hypothesis, minimal suppression needed because decoding priority seems scientifically obvious). The whole-language reading would author lower extractiveness (~0.20) from its perspective (authentic meaning-making is natural; extraction comes only from phonics-first mandates blocking it). The structured-literacy reading would author moderate-to-high extractiveness (~0.55–0.70) from its perspective (genuine coordination of vulnerable-learner-first design principle, but high suppression required because it challenges the assumption that average-learner design scales). The network link reflects: balanced literacy INFLUENCES phonics-first and whole-language mandates by legitimizing a middle position that constrains how purely single-method systems can operate without appearing pedagogically crude. Balanced literacy COEXISTS_WITH all siblings as live positions held by different school systems and research communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__balanced_literacy_integration, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
