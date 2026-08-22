% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__whole_language_meaning_primacy, []).

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
 *   constraint_id: reading_acquisition_legitimacy__whole_language_meaning_primacy
 *   human_readable: Whole-Language Meaning-Primacy Regime for Reading Instruction Legitimacy
 *   domain: education policy/cognitive science/literacy pedagogy
 *
 * SUMMARY:
 *   From the 1980s through the 2010s the meaning-first account of reading
 *   acquisition held institutional authority across Anglophone education:
 *   reading was framed as meaning-making analogous to oral language,
 *   legitimate instruction meant immersion in authentic literature, and
 *   explicit systematic code instruction was treated as unnecessary at best
 *   and harmful at worst. Education faculties carried the doctrine, statewide
 *   adoptions spread it (California 1987 as emblem), and leveled-text
 *   publishing supplied it commercially. Its costs concentrated on children
 *   who cannot induce the alphabetic code from exposure alone,
 *   disproportionately dyslexic students and children without preschool print
 *   exposure, and surfaced years later as remediation, special-education
 *   referral, and stagnant national assessment results. This file
 *   instantiates ONE reading of the reading_acquisition_legitimacy kernel
 *   (whole_language_meaning_primacy) as a clean, epsilon-invariant
 *   constraint: the referent of extractiveness is the meaning-first
 *   instructional regime as it actually operated, not the phonics-first
 *   alternative its rivals would install. Sibling readings are separate
 *   constraints with their own victim sets and epsilon values, linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   teacher_education_faculties: Agenda setter
 *   (institutional/identity_locked) — trains and certifies teachers in
 *   meaning-first methods - literacy_curriculum_publishers: Primary
 *   beneficiary (powerful/arbitrage) — sells the leveled texts,
 *   running-record kits, and guided-reading programs the approach requires -
 *   classroom_teachers: Payer and incidental beneficiary
 *   (organized/constrained) — implements the approach, absorbs blame,
 *   receives identity and autonomy - struggling_readers: Primary target
 *   (powerless/trapped) — children who do not infer the code incidentally and
 *   are helped only after failure - affluent_parents_of_struggling_readers:
 *   Partially mobile target (powerful/arbitrage) — buys private remediation,
 *   masking system-level failure - low_income_families: Target with no exit
 *   (powerless/trapped) — bears delayed costs without voice in adoption
 *   decisions - reading_science_researchers: Excluded expert voice
 *   (institutional/analytical) — produced the counterevidence, long outside
 *   the methods canon - state_legislatures: Observer turning actor
 *   (institutional/analytical) — commissions inquiries and passes
 *   evidence-based-instruction mandates
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.7).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.6).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.7).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole-Language Meaning-Primacy Regime for Reading Instruction Legitimacy").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education policy/cognitive science/literacy pedagogy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, '8129e5da-0a3a-4c2e-8bd9-9036707f34e8').
narrative_ontology:cs_kernel_codification('8129e5da-0a3a-4c2e-8bd9-9036707f34e8', distributed).
narrative_ontology:cs_authority_grounding('8129e5da-0a3a-4c2e-8bd9-9036707f34e8', lineage).
narrative_ontology:cs_interpretation_layer_present('8129e5da-0a3a-4c2e-8bd9-9036707f34e8').
narrative_ontology:cs_reading_relation('8129e5da-0a3a-4c2e-8bd9-9036707f34e8', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('8129e5da-0a3a-4c2e-8bd9-9036707f34e8', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('8129e5da-0a3a-4c2e-8bd9-9036707f34e8', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('8129e5da-0a3a-4c2e-8bd9-9036707f34e8', foundational, reading_is_meaning_construction).
narrative_ontology:cs_axiom_status(reading_is_meaning_construction, holdable).
narrative_ontology:cs_axiom_grounding('8129e5da-0a3a-4c2e-8bd9-9036707f34e8', reading_is_meaning_construction, empirically_contingent).
narrative_ontology:cs_axiom('8129e5da-0a3a-4c2e-8bd9-9036707f34e8', foundational, decoding_emerges_from_meaningful_print_engagement).
narrative_ontology:cs_axiom_status(decoding_emerges_from_meaningful_print_engagement, holdable).
narrative_ontology:cs_axiom_grounding('8129e5da-0a3a-4c2e-8bd9-9036707f34e8', decoding_emerges_from_meaningful_print_engagement, empirically_contingent).
narrative_ontology:cs_reference_frame('8129e5da-0a3a-4c2e-8bd9-9036707f34e8', natural_acquisition_meaning_primacy).
narrative_ontology:cs_drift_state('8129e5da-0a3a-4c2e-8bd9-9036707f34e8', contemporary_science_of_reading_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8129e5da-0a3a-4c2e-8bd9-9036707f34e8', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, teacher_education_faculties).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, proficient_child_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, low_income_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, affluent_parents_of_struggling_readers).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, psycholinguistic_guessing_game_hypothesis).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, oral_language_analogy_for_literacy_acquisition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and teach the methods courses that decide what counts as legitimate reading instruction. Their syllabi center meaning-making, authentic texts, and the teacher-as-facilitator stance; candidates who emphasize explicit code instruction risk failing methods assessments. Decades of scholarship, careers, and departmental culture are invested in the framework, and revising it would mean conceding that a generation of graduates was misprepared.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, teacher_education_faculties, agenda_setter,
    institutional, generational, identity_locked, continental).

% Sell the materials the approach requires: leveled book collections, running-record kits, guided-reading sets, and the professional-development contracts attached to them. Revenue scales with district adoption of the framework. Catalogs can be retooled quickly if districts shift to other materials, so the firms' commitment is commercial rather than doctrinal.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_curriculum_publishers, beneficiary,
    powerful, biographical, arbitrage, global).

% Children who crack the alphabetic code with minimal explicit help, often from print-rich homes. They experience the classroom as a stream of good books, read-alouds, and writing projects with little repetitive drill. They would likely have learned to read under any mainstream method, so what they receive is mostly a pleasant surface at little cost.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, proficient_child_readers, beneficiary,
    powerless, immediate, trapped, local).

% Implement daily blocks of shared reading, guided-reading groups, and running records. When scores stagnate they absorb public blame despite teaching as trained. They spend personal money on classroom libraries and receive in return professional autonomy, a coherent identity as facilitators of meaning, and relief from scripted lessons.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, classroom_teachers, beneficiary).

% Children, disproportionately those with dyslexia or limited preschool print exposure, who do not induce letter-sound patterns from exposure alone. Taught to guess from pictures, first letters, and context, they often plateau around third grade when pictures disappear and multisyllabic words arrive. Help arrives late, typically after repeated failure, and the deficit compounds across every subject that assumes fluent reading.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Notice their child falling behind, pay for private assessments and structured tutoring outside school, and advocate fiercely at the margins of the system. Their purchases rescue individual children and simultaneously hide the pattern from district data, weakening the political case for changing classroom practice.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, affluent_parents_of_struggling_readers, payer,
    powerful, biographical, arbitrage, local).

% Depend entirely on the school for their children's reading. They rarely sit on curriculum adoption committees, cannot purchase remediation, and discover the gap when it surfaces as special-education referral, grade retention, or dropout. The costs land a grade level or more after the instructional choices that caused them.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, low_income_families, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, low_income_families, excluded).

% Publish eye-tracking, longitudinal, and cross-linguistic evidence that skilled readers rely on letter-sound mapping rather than context guessing, and that explicit instruction helps precisely the children this approach serves worst. Their findings circulate in psychology journals but entered education methods courses slowly and often through hostile paraphrase.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_science_researchers, excluded,
    institutional, generational, analytical, global).

% Fund schools and respond to flat national assessment trends. Over the past decade many have commissioned inquiries, heard testimony from affected families, and passed statutes requiring evidence-based reading instruction and early screening, moving from spectator toward rule-writer.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, state_legislatures, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_curriculum_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__whole_language_meaning_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes early-literacy classrooms around shared authentic books, read-alouds, and meaning-centered talk; gives teachers a common professional identity and a workable answer to what to do all day; keeps young children engaged with print before formal decoding; and gives publishers a coherent product architecture.
% TRANSFER_FUNCTION: Moves instructional hours and the opportunity for explicit code instruction away from novice readers, concentrating the loss on those who cannot infer the code unaided, and moves material sales, professional-development fees, professional authority, and graduate enrollments toward publishers and education faculties.
% ABSENT_VOICES: Reading scientists and parents of dyslexic children were structurally outside the rooms where methods-course canons, adoption lists, and editorial priorities were set; their objections arrived as external noise rather than agenda items.
% DISAPPEARANCE_RATIONALE: Methods courses would rewrite syllabi, adoption lists would swap leveled texts for decodable and knowledge-building series, publishers would pivot catalogs, and large numbers of serving teachers would need retraining; screening and intervention caseloads would shrink over time as fewer children fell behind.
% FOUNDING_PROBLEM: Mid-century basal readers offered fragmented, unnatural sentences drilled in isolation; children learned to call words without wanting to read. Miscue research and the oral-language analogy proposed that reading, like speaking, is acquired through meaningful use, and that instruction should center whole stories rather than isolated skills.
% FOUNDING_PROBLEM_CORROBORATION: Historians of literacy education corroborate the founding context (the basal-drill era the meaning-first movement reacted against). Contemporary reading scientists and several state inquiry reports corroborate that the founding dilemma has been dissolved by curricula pairing systematic code instruction with rich authentic literature, so engagement no longer trades off against explicit teaching. This attestation comes from outside the faculties and publishers who carry the arrangement; no beneficiary-party source independently maintains that the founding problem still binds.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__whole_language_meaning_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.70 at interval end) because the arrangement's costs are concentrated and severe for the minority who cannot infer the code, while its benefits are broad but shallow. Suppression (0.60) reflects gatekeeping in methods courses, adoption committees, and editorial cultures rather than legal coercion; it is authored as a raw structural property and is not scaled by power or scope. Theater ratio (0.48) reflects an assessment apparatus — running records, text leveling, cueing prompts — that expanded to fill the space rigorous outcome evaluation vacated. Accessibility collapse is moderate (0.45): systematic phonics never vanished (special education, homeschooling, some districts) but was delegitimized inside the mainstream credentialing channel. Resistance is high (0.72): the reading wars, parent organizing, the National Reading Panel, and a wave of state statutes. The three temporal series share one nine-point grid (index years since 1980): extractiveness climbs as cohorts accumulate unpaid costs, theater climbs as assessment substitutes for outcome accountability, and suppression rises to a mid-1990s peak of orthodoxy then decays modestly as the National Reading Panel and the science-of-reading legislative wave erode enforcement capacity while institutional inertia holds the arrangement in place. Coordination type is declared identity_coordination because the arrangement's binding function is professional boundary maintenance; the identity framing is genuine (a real professional community and practice framework exists) but is flagged for the known gaming risk — identity leeway must not excuse coupling that concentrates costs on powerless agents at wide scope, which is exactly the observed pattern.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is professional liberation: children reading real books, teachers freed from scripts. From the trapped payer seats the same structure is a withheld necessity — instruction their children needed and did not get. From the excluded researcher seat it is a falsified theory sustained by institutional inertia. The engine computes these divergences from power, exit, and role data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Education faculties and publishers sit near the beneficiary pole: the framework subsidizes their authority and revenue and costs them little; publishers' arbitrage exit places them nearest the subsidy end. Proficient child readers sit near symmetric — genuine literary benefit, negligible cost. Classroom teachers occupy the middle: they pay workload and blame but collect identity and autonomy. Struggling readers and low-income families sit at the target pole with trapped exit, so effective extraction concentrates on them; affluent parents share the target position but their arbitrage exit damps it. Continental-to-global scope scales verification difficulty upward: harms surface years later and districts away from the instructional choice that caused them, which amplifies effective extraction on the trapped seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (basal-drill tedium suppressing the desire to read) is dead: modern curricula dissolve the dilemma by pairing systematic code instruction with rich authentic literature. The arrangement persists through faculty identity fusion, adoption-cycle revenue, and the masking effect of private tutoring — the classic signature of a mandate outliving its function. Expect the R5 mismatch signal (status=dead x verdict=world_rearranges) to fire. The tangled_rope claim prevents both classification errors: calling this a pure snare would erase the genuine coordination it performs (engagement, print exposure, a workable professional framework that served the majority adequately), and calling it a rope would erase the identifiable victims who pay through the same structure that coordinates everyone else. The receipt surface records the asymmetry: gains accrue demonstrably to the publishing seat, and fixing is prohibitive for whoever could fix it, since repair requires retraining a workforce and confronting the agenda-setter's identity rather than replacing a single administrator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates one reading (whole_language_meaning_primacy) of the reading_acquisition_legitimacy kernel; how would the constraint''s victim set and epsilon shift under the sibling readings?',
    'Generate the sibling stories (phonics_decoding_primacy, balanced_literacy_integration, structured_literacy_remediation) and compare victim declarations and epsilon over the same referent period.',
    'Under phonics_decoding_primacy this arrangement reads as the deviation to be corrected; under balanced_literacy_integration part of the measured harm is attributed to imperfect implementation rather than the principle; victim sets and epsilon move accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame indexicality: one kernel, four readings, one instantiated here.').

omega_variable(
    natural_acquisition_status,
    'Is written-language acquisition a natural, exposure-driven process like speech, or a biologically secondary skill that most learners must be explicitly taught?',
    'Cross-linguistic and longitudinal studies of children receiving no explicit code instruction; comparative prevalence of reading difficulty across instructional regimes.',
    'If acquisition is natural for nearly all children, the arrangement''s core wager holds and measured extraction collapses toward coordination cost; if it is secondary, withholding explicit instruction is a foreseeable injury to a large minority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_acquisition_status, empirical, 'Whether the pedagogy''s founding wager about the nature of reading acquisition is true.').

omega_variable(
    cueing_mechanism_validity,
    'Do skilled readers identify words primarily through letter-sound mapping (orthographic mapping) or through the three-cueing strategy (meaning, structure, visual) the pedagogy trains?',
    'Eye-tracking and lexical-decision studies comparing skilled and novice readers; replication of cueing-use findings.',
    'If cueing is not how skilled reading works, the pedagogy trains a strategy that must later be unlearned, and the arrangement''s theoretical warrant fails outright.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cueing_mechanism_validity, empirical, 'Validity of the three-cueing theory of skilled word recognition.').

omega_variable(
    suppression_mechanism_split,
    'Is the marginalization of explicit-code instruction structural (hiring, methods-course gating, adoption committees) or internalized (teachers who believe code instruction harms children)?',
    'Post-mandate trajectory: in states now requiring evidence-based instruction, track whether classroom practice converges once structural gates open, or whether resistance persists; persistent resistance indicates internalized components.',
    'If largely internalized, statutory fixes alone underperform and the arrangement outlives its enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized share of the measured suppression.').

omega_variable(
    constructivist_identity_lock,
    'How much of the arrangement''s persistence depends on education faculties'' professional identity fusion with constructivist pedagogy, and what changes if that frame breaks?',
    'Compare faculties that publicly revised methods after state inquiries with those that did not: enrollment, graduate placement, and curriculum-revision rates.',
    'If identity lock is load-bearing, enforcement decay will not produce voluntary reform; external mandates remain the only repair path, and the arrangement behaves as inertially maintained rather than actively defended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructivist_identity_lock, conceptual, 'Identity-fusion dependence of the arrangement''s persistence.').

omega_variable(
    private_tutoring_masking,
    'What share of the arrangement''s apparent workability in affluent districts is purchased privately (tutoring, assessment, advocacy) rather than produced by classroom instruction?',
    'District-level comparison of reading outcomes against household income and private-tutoring penetration; natural experiments where tutoring markets thin.',
    'High masking means measured success understates extraction and explains why harm concentrates where political voice is weakest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_tutoring_masking, empirical, 'Out-of-system compensation hiding the arrangement''s failure signature.').

omega_variable(
    balanced_absorption_ambiguity,
    'Is balanced_literacy_integration a genuine synthesis of the kernel''s readings, or the interpretive layer absorbing the National Reading Panel critique into the meaning-first framework without surfacing revision?',
    'Content analysis of balanced-literacy materials and methods syllabi: does cueing instruction persist under new labels, and is code instruction systematic or incidental?',
    'If absorption, the sibling reading inherits this arrangement''s extraction profile and the family''s epsilon differences are smaller than they appear; if genuine synthesis, the family decomposes as advertised.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balanced_absorption_ambiguity, conceptual, 'Whether the sibling reading is synthesis or rebrand via the interpretive layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(read_tr_t0, observed).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(read_tr_t5, observed).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(read_tr_t10, observed).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 15, 0.32).
narrative_ontology:measurement_basis(read_tr_t15, observed).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(read_tr_t20, observed).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(read_tr_t25, observed).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 30, 0.44).
narrative_ontology:measurement_basis(read_tr_t30, observed).
narrative_ontology:measurement(read_tr_t35, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 35, 0.46).
narrative_ontology:measurement_basis(read_tr_t35, observed).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(read_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(read_be_t0, observed).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(read_be_t5, observed).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(read_be_t10, observed).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(read_be_t15, observed).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(read_be_t20, observed).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(read_be_t25, observed).
narrative_ontology:measurement(read_be_t30, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 30, 0.69).
narrative_ontology:measurement_basis(read_be_t30, observed).
narrative_ontology:measurement(read_be_t35, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 35, 0.7).
narrative_ontology:measurement_basis(read_be_t35, observed).
narrative_ontology:measurement(read_be_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 40, 0.7).
narrative_ontology:measurement_basis(read_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(read_su_t0, observed).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 5, 0.45).
narrative_ontology:measurement_basis(read_su_t5, observed).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(read_su_t10, observed).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(read_su_t15, observed).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 20, 0.63).
narrative_ontology:measurement_basis(read_su_t20, observed).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 25, 0.65).
narrative_ontology:measurement_basis(read_su_t25, observed).
narrative_ontology:measurement(read_su_t30, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(read_su_t30, observed).
narrative_ontology:measurement(read_su_t35, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 35, 0.62).
narrative_ontology:measurement_basis(read_su_t35, observed).
narrative_ontology:measurement(read_su_t40, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(read_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, identity_coordination).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% The colloquial label 'reading acquisition legitimacy' covers structurally distinct claims about how literacy is acquired and what instruction is therefore legitimate. Per the epsilon-invariance principle the kernel decomposes into four readings, each with its own epsilon, beneficiaries, and victims; this file instantiates whole_language_meaning_primacy. Historically the meaning-first reading sat upstream of balanced_literacy_integration (its institutional dominance shaped the synthesis' content) and stands opposed to phonics_decoding_primacy at the level of core premises; structured_literacy_remediation competes for the same instructional space from a vulnerability-first design principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
