% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__phonics_decoding_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Systematic Phonics as the Legitimate Basis of Early Reading Instruction
 *   domain: education_policy/cognitive_science/literacy_pedagogy
 *
 * SUMMARY:
 *   This constraint instantiates the phonics-decoding-primacy reading of the
 *   contested reading-acquisition-legitimacy kernel: the claim that reading
 *   is fundamentally decoding, and that legitimate early instruction must
 *   make the alphabetic principle explicit through systematic, sequenced
 *   phonics rather than allowing decoding skill to emerge incidentally from
 *   literature exposure. It is authored as a Rope with meaningfully
 *   independent metrics: a real coordination function exists (converting
 *   scattershot, teacher-dependent decoding instruction into a reliable,
 *   evidence-grounded sequence that serves struggling decoders who do not
 *   infer the code on their own), but the mandate has also become a vehicle
 *   through which curriculum publishers, structured-literacy credentialing
 *   bodies, and policymakers displace an incumbent professional and
 *   publishing ecosystem — meaning-first teachers and publishers pay real
 *   costs in retraining, credential jeopardy, and market exclusion. The
 *   rising suppression_requirement series reflects the reading's growing
 *   entrenchment in statute and accountability testing over the interval,
 *   which increasingly forecloses classroom-level pedagogical discretion even
 *   where the underlying decoding-primacy claim is narrower than the mandate
 *   built on it.
 *
 * KEY AGENTS:
 *   - state_literacy_policymakers: agenda_setter (institutional/analytical) — writes and enforces the mandate
 *   - struggling_decoders: primary beneficiary (powerless/trapped) — the evidentiary center of the claim
 *   - phonics_curriculum_publishers: beneficiary (organized/arbitrage) — captures procurement demand
 *   - whole_language_trained_teachers: payer (moderate/constrained) — bears retraining and devaluation costs
 *   - cognitive_reading_scientists: observer (analytical/analytical) — supplies the narrower empirical basis the broader mandate is built on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.28).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.42).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.28).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Systematic Phonics as the Legitimate Basis of Early Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education_policy/cognitive_science/literacy_pedagogy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '7e83eb49-f350-4641-b311-3f475fe6dd65').
narrative_ontology:cs_kernel_codification('7e83eb49-f350-4641-b311-3f475fe6dd65', distributed).
narrative_ontology:cs_authority_grounding('7e83eb49-f350-4641-b311-3f475fe6dd65', expertise).
narrative_ontology:cs_interpretation_layer_present('7e83eb49-f350-4641-b311-3f475fe6dd65').
narrative_ontology:cs_reading_relation('7e83eb49-f350-4641-b311-3f475fe6dd65', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('7e83eb49-f350-4641-b311-3f475fe6dd65', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('7e83eb49-f350-4641-b311-3f475fe6dd65', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('7e83eb49-f350-4641-b311-3f475fe6dd65', foundational, decoding_is_constitutive_of_reading).
narrative_ontology:cs_axiom_status(decoding_is_constitutive_of_reading, holdable).
narrative_ontology:cs_axiom_grounding('7e83eb49-f350-4641-b311-3f475fe6dd65', decoding_is_constitutive_of_reading, empirically_contingent).
narrative_ontology:cs_axiom('7e83eb49-f350-4641-b311-3f475fe6dd65', foundational, explicit_instruction_required_for_alphabetic_principle_acquisition).
narrative_ontology:cs_axiom_status(explicit_instruction_required_for_alphabetic_principle_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('7e83eb49-f350-4641-b311-3f475fe6dd65', explicit_instruction_required_for_alphabetic_principle_acquisition, empirically_contingent).
narrative_ontology:cs_reference_frame('7e83eb49-f350-4641-b311-3f475fe6dd65', alphabetic_code_as_primary_reading_mechanism).
narrative_ontology:cs_drift_state('7e83eb49-f350-4641-b311-3f475fe6dd65', post_reading_wars_legislative_consolidation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('7e83eb49-f350-4641-b311-3f475fe6dd65', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_decoders).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_trainers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_trained_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, advanced_early_readers_under_rigid_pacing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write statutes and adopt curriculum frameworks mandating systematic phonics instruction, citing cognitive-science evidence on decoding as foundational. They set assessment requirements (early decoding screeners) and approve or disapprove instructional materials, converting a pedagogical claim into an enforceable procurement and accountability regime.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, state_literacy_policymakers, agenda_setter,
    institutional, generational, analytical, national).

% Children who do not spontaneously infer the alphabetic code from exposure to text. Explicit, sequenced phonics instruction gives them a reliable path to decoding that implicit or meaning-first approaches did not provide. They have no say in which pedagogy their classroom uses; their outcomes are the primary evidentiary basis cited for the reading's legitimacy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, struggling_decoders, beneficiary,
    powerless, biographical, trapped, local).

% Sell decodable text series, scope-and-sequence programs, and teacher training aligned to systematic phonics mandates. State adoption of the phonics-primacy reading converts their product line into a required purchase across entire school systems; they can exit any single state market but not the underlying demand structure they helped create through advocacy.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers, beneficiary,
    organized, biographical, arbitrage, national).

% Certify teachers and administrators in systematic-phonics-aligned methods, running the professional-development pipeline that phonics mandates require. Their credentialing authority and consulting revenue depend on the reading remaining institutionally dominant.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_trainers, beneficiary,
    organized, biographical, mobile, national).

% Built careers and classroom practice around meaning-first, literature-immersion approaches, often over decades. Mandated retraining, materials replacement, and evaluation against decoding-fidelity metrics devalue their accumulated expertise and, in some jurisdictions, threaten certification or employment if they do not comply. Exit means retraining at personal cost or leaving the profession.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_trained_teachers, payer,
    moderate, biographical, constrained, regional).

% Children who infer the code quickly and would benefit from rich, complex, meaning-focused text earlier. Rigid, cumulative phonics-sequence pacing that will not let them advance until scope-and-sequence checkpoints are cleared can produce boredom and disengagement; their classroom placement is set by the same mandate that benefits struggling decoders.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, advanced_early_readers_under_rigid_pacing, payer,
    powerless, biographical, trapped, local).

% Study the empirical record on decoding acquisition (simple view of reading, alphabetic-principle research, dyslexia intervention studies) and testify in curriculum and policy disputes. Their evidence is invoked by the reading's advocates but their findings are narrower in scope than the sweeping instructional mandates built on top of them.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, cognitive_reading_scientists, observer,
    analytical, generational, analytical, global).

% Publishers whose catalogs are built around leveled, authentic-literature-based reading programs lose procurement access when states mandate phonics-aligned decodable texts. They are not represented in the legislative hearings that adopt these mandates and would argue the empirical basis is narrower than the policy sweep it justifies.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__phonics_decoding_primacy, publishing_industry_incumbents_meaning_first, excluded,
    organized, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__phonics_decoding_primacy, diffuse).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__phonics_decoding_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of inconsistent, teacher-dependent guesswork about how children learn to decode print by codifying a single, explicit, sequenced instructional method grounded in alphabetic-principle research, so that struggling decoders are not left to infer the code from unstructured exposure.
% TRANSFER_FUNCTION: Moves instructional authority, curriculum procurement budgets, and professional legitimacy from meaning-first practitioners and publishers toward phonics-aligned publishers, structured-literacy trainers, and the policymakers who mandate them; moves classroom time and pacing away from open-ended text exploration toward sequenced decoding drills.
% ABSENT_VOICES: Meaning-first publishers and long-tenured whole-language teachers are rarely represented in the legislative and state-board hearings that adopt phonics mandates; their objection — that the evidence base for decoding-primacy is narrower than the instructional sweep imposed on it — is made in professional journals and union statements but not in the policymaking room.
% DISAPPEARANCE_RATIONALE: If phonics-primacy legitimacy vanished as the governing framework, struggling decoders' advocates argue instruction would revert to unsystematic exposure and reading failure rates would rise; meaning-first advocates argue classrooms would simply return to richer literature engagement without a measurable decline in decoding outcomes for most children. Which world obtains depends on empirical questions (transfer of explicit code knowledge vs. incidental learning) that remain actively contested between the reading's proponents and its rivals.
% FOUNDING_PROBLEM: Whole-language and meaning-immersion approaches dominant through the late twentieth century left a substantial minority of children, especially those with weaker phonological awareness, unable to reliably decode print despite years of literature exposure; the phonics-primacy reading was built to give those children an explicit, teachable path to the alphabetic code.
% FOUNDING_PROBLEM_CORROBORATION: Independent reading scientists outside the phonics-publishing and structured-literacy-training industries (university-based cognitive psychologists studying the simple view of reading and dyslexia intervention) corroborate that systematic phonics instruction measurably improves decoding for at-risk readers. However, the same independent researchers and meaning-first practitioners dispute whether the founding problem justifies the full institutional mandate now built on it — some argue the narrow, well-supported claim (explicit phonics helps struggling decoders) has been extended into a broader, less-corroborated claim (phonics-first sequencing should govern all early reading instruction), which serves the training and publishing industries more than it serves the residual research question.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__phonics_decoding_primacy, contested).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__phonics_decoding_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).
:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.28 at interval end) because the coordination function is genuine and well-evidenced for its core claim (explicit phonics measurably helps struggling decoders), but a real transfer exists from meaning-first practitioners and publishers to phonics-aligned incumbents that is not fully justified by the narrower research claim. Suppression is authored higher and rising (0.20 to 0.42) because state mandates increasingly convert a pedagogical preference into statute-backed procurement and evaluation requirements, foreclosing meaning-first practice even in classrooms where it might serve some learners well. Theater ratio stays low throughout (0.05 to 0.15) because the decoding-focused instructional activity is substantively functional, not performative — phonics drills genuinely target the alphabetic principle they claim to teach.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and beneficiary-publisher seats, this reads as a Rope: a real coordination problem (unsystematic decoding instruction) solved cleanly by codifying explicit method. From the whole-language-trained-teacher and meaning-first-publisher seats, the same structure reads closer to Tangled Rope or Snare: coordination language covering a transfer of curricular authority and market share that exceeds what the narrower evidentiary claim supports. The engine computes these divergent seat-level readings from the structural power/exit data; the claimed_type of rope here reflects only the coordination-function judgment, not a reconciliation with the payer-seat experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling decoders sit near the beneficiary end: the constraint subsidizes exactly the population whose reading failure motivated it. Phonics publishers and structured-literacy trainers are institutional beneficiaries whose revenue depends on the mandate's persistence — genuine coordination value plus captured rents. Whole-language-trained teachers and meaning-first publishers sit near the target end: constrained or trapped exit, real professional and financial cost, displaced by statute rather than by evidence specific to their students. Advanced early readers under rigid pacing are a secondary payer group: the same sequencing that helps strugglers can under-serve them, though their exit options remain trapped by classroom assignment rather than by policy design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (children failing to decode without explicit instruction) remains partly live for a subset of learners, which prevents blanket dismissal of the reading as pure mandatrophy. But the founding_problem_status is authored contested because independent researchers distinguish the narrow, well-corroborated claim (explicit phonics helps at-risk decoders) from the broader institutional mandate (phonics-first sequencing should govern ALL early instruction), and it is the broader mandate — not the narrow claim — that primarily benefits publishers and trainers. Classification as Rope rather than Tangled Rope reflects a judgment that the coordination function still dominates the transfer function at the level this specific reading targets; the sibling structured_literacy_remediation reading (not authored here) makes the vulnerable-learner-first case even more narrowly and would likely show a cleaner coordination profile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrow_claim_vs_broad_mandate_scope,
    'Does the empirical evidence for explicit phonics benefiting struggling decoders justify the full scope of mandated phonics-first sequencing for ALL early readers, or only for an identifiable at-risk subgroup?',
    'Meta-analytic comparison of effect sizes for systematic phonics instruction stratified by baseline phonological-awareness risk; if effect sizes for typically-developing and advanced early readers are near-zero or negative under rigid pacing, the broader mandate exceeds its evidentiary basis.',
    'If the narrower reading is correct, this constraint''s coordination function is much smaller than its enforcement scope, and classification would shift toward tangled_rope (real coordination for a subgroup, extraction imposed on the rest via blanket mandate). If the broader reading holds, rope classification is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_claim_vs_broad_mandate_scope, empirical, 'Whether decoding-primacy evidence supports universal mandate scope or only at-risk-targeted intervention.').

omega_variable(
    kernel_framing_alternative_diagnostic_vs_ideological,
    'Is the reading_acquisition_legitimacy kernel best understood as a diagnostic/empirical dispute (what does cognitive science show about how children learn to decode) or as an ideological/professional dispute (which pedagogical tradition and its institutions retain curricular authority)?',
    'Track whether policy shifts track new empirical findings (diagnostic framing) or track political/legislative cycles and advocacy campaign funding independent of new evidence (ideological framing).',
    'Under the diagnostic framing, this reading''s classification should track the evidence base tightly and would justify a rope-leaning read wherever evidence is strong. Under the ideological framing, the same instructional mandate is better modeled as tangled_rope regardless of evidence strength, because the transfer of authority and resources is the operative mechanism rather than the science.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_diagnostic_vs_ideological, conceptual, 'Alternative framings of the kernel contest as empirical dispute versus institutional/ideological contest, which would change this reading''s classification.').

omega_variable(
    teacher_deskilling_reversibility,
    'Is the professional devaluation experienced by whole-language-trained teachers reversible through retraining, or does it represent a permanent loss of a distinct pedagogical expertise that will not be reconstituted once meaning-first training pipelines are defunded?',
    'Longitudinal tracking of teacher-preparation program curricula and whether meaning-first instructional expertise persists institutionally after a generation of phonics-mandate-driven credentialing.',
    'If reversible, the payer cost to whole-language-trained teachers is transitional (scaffold-like); if irreversible, it represents a permanent extraction of professional capital that strengthens the case for tangled_rope classification of the broader institutional mandate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(teacher_deskilling_reversibility, empirical, 'Whether displaced meaning-first teaching expertise is a temporary transition cost or a permanent institutional loss.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0, 0.05).
narrative_ontology:measurement(read_tr_t4, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 4, 0.07).
narrative_ontology:measurement(read_tr_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 8, 0.09).
narrative_ontology:measurement(read_tr_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 12, 0.11).
narrative_ontology:measurement(read_tr_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 16, 0.13).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 20, 0.14).
narrative_ontology:measurement(read_tr_t24, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 24, 0.15).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(read_be_t4, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 4, 0.16).
narrative_ontology:measurement(read_be_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 8, 0.2).
narrative_ontology:measurement(read_be_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(read_be_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 16, 0.25).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(read_be_t24, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 24, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(read_su_t4, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 4, 0.26).
narrative_ontology:measurement(read_su_t8, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(read_su_t12, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(read_su_t16, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 16, 0.38).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(read_su_t24, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, information_standard).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.05).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language concept 'legitimate reading instruction' along the reading_acquisition_legitimacy kernel. phonics_decoding_primacy (this story) claims decoding is the whole of reading and mandates explicit sequencing for all learners. whole_language_meaning_primacy claims the opposite: decoding should emerge from meaning-immersion. balanced_literacy_integration claims both are needed and instantiates a lower-suppression, lower-extraction hybrid. structured_literacy_remediation narrows this reading's claim specifically to the most vulnerable learners and is expected to show the cleanest coordination profile (highest rope-purity) of the four, since it declines to extend the mandate beyond the population the evidence most strongly supports. Each carries its own ε; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
