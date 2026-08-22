% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__balanced_literacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__balanced_literacy_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__balanced_literacy_reading
 *   human_readable: Balanced Literacy Reading Instruction Doctrine
 *   domain: educational/psychological/pedagogical
 *
 * SUMMARY:
 *   The balanced literacy reading of the literacy acquisition kernel claims
 *   that reading acquisition requires both systematic phonics and meaningful
 *   text engagement, treating them as complementary. This reading emerged in
 *   the 1990s as a proposed synthesis of the 'reading wars' between
 *   phonics-first and whole-language approaches. It has become the dominant
 *   instructional orthodoxy in many Anglophone education systems, enforced
 *   through teacher certification, state standards, and publisher offerings.
 *   The reading is contested: critics argue it is a genuine third way, while
 *   opponents (particularly structured literacy and phonics advocates) argue
 *   it is a rebranding of whole language that admits minimal phonics to
 *   deflect criticism. The structural extraction is moderate and operates
 *   through 'method churn'âthe perpetual cycle of new curricula,
 *   assessments, and professional development that generates revenue for
 *   education schools and publishers while imposing implementation costs on
 *   teachers and potentially diluting effective instruction for struggling
 *   readers.
 *
 * KEY AGENTS:
 *   - balanced_literacy_researchers: agenda-setter (institutional/constrained) â frames the pedagogical consensus
 *   - education_schools: beneficiary (institutional/constrained) â certifies teachers in the orthodoxy
 *   - literacy_publishers: beneficiary (powerful/mobile) â captures method-churn revenue
 *   - classroom_teachers: payer (moderate/constrained) â implements hybrid methods under district mandate
 *   - struggling_readers: payer (powerless/trapped) â bears risk of insufficient systematic phonics
 *   - structured_literacy_advocates: excluded (organized/constrained) â marginalized reading-science voice
 *   - cognitive_scientists: observer (analytical/analytical) â independent empirical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, 0.55).
domain_priors:suppression_score(literacy_acquisition_kernel__balanced_literacy_reading, 0.42).
domain_priors:theater_ratio(literacy_acquisition_kernel__balanced_literacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__balanced_literacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__balanced_literacy_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__balanced_literacy_reading, "Balanced Literacy Reading Instruction Doctrine").
narrative_ontology:topic_domain(literacy_acquisition_kernel__balanced_literacy_reading, "educational/psychological/pedagogical").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__balanced_literacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__balanced_literacy_reading, '3d892c85-b11d-4a94-aba3-0b7218736b67').
narrative_ontology:cs_kernel_codification('3d892c85-b11d-4a94-aba3-0b7218736b67', distributed).
narrative_ontology:cs_authority_grounding('3d892c85-b11d-4a94-aba3-0b7218736b67', expertise).
narrative_ontology:cs_interpretation_layer_present('3d892c85-b11d-4a94-aba3-0b7218736b67').
narrative_ontology:cs_reading_relation('3d892c85-b11d-4a94-aba3-0b7218736b67', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d892c85-b11d-4a94-aba3-0b7218736b67', literacy_acquisition_kernel__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('3d892c85-b11d-4a94-aba3-0b7218736b67', literacy_acquisition_kernel__structured_literacy_reading, influences).
narrative_ontology:cs_axiom('3d892c85-b11d-4a94-aba3-0b7218736b67', foundational, explicit_phonics_and_authentic_text_both_necessary).
narrative_ontology:cs_axiom_status(explicit_phonics_and_authentic_text_both_necessary, holdable).
narrative_ontology:cs_axiom_grounding('3d892c85-b11d-4a94-aba3-0b7218736b67', explicit_phonics_and_authentic_text_both_necessary, empirically_contingent).
narrative_ontology:cs_axiom('3d892c85-b11d-4a94-aba3-0b7218736b67', foundational, instructional_balance_resolves_reading_wars).
narrative_ontology:cs_axiom_status(instructional_balance_resolves_reading_wars, holdable).
narrative_ontology:cs_axiom_grounding('3d892c85-b11d-4a94-aba3-0b7218736b67', instructional_balance_resolves_reading_wars, instrumental).
narrative_ontology:cs_reference_frame('3d892c85-b11d-4a94-aba3-0b7218736b67', balanced_pedagogical_framework).
narrative_ontology:cs_drift_state('3d892c85-b11d-4a94-aba3-0b7218736b67', contemporary_structured_literacy_resurgence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3d892c85-b11d-4a94-aba3-0b7218736b67', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__balanced_literacy_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, education_schools).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__balanced_literacy_reading, literacy_publishers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop the balanced literacy framework, conduct research supporting integration of phonics and whole language, sit on curriculum committees, and train teachers through university programs and professional development. Their professional reputation and grant funding are tied to the viability of the balanced approach.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, balanced_literacy_researchers, agenda_setter,
    institutional, generational, constrained, national).

% Universities and colleges of education that certify teachers in balanced literacy methods. Their enrollment, prestige, and relevance depend on training teachers in the prevailing instructional orthodoxy, which shifts as methods are rebranded.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, education_schools, beneficiary,
    institutional, generational, constrained, national).

% Publish and sell balanced literacy curricula, leveled reader sets, assessment systems, and professional development packages. Revenue cycles are tied to district adoptions and method churn.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, literacy_publishers, beneficiary,
    powerful, biographical, mobile, national).

% Required to implement district-mandated balanced literacy programs, attend recurring professional development on the latest iteration, and attempt to reconcile phonics drills with authentic reading workshops in daily classroom practice.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Depend on school-provided reading instruction for foundational decoding skills. When balanced literacy is implemented with insufficient systematic phonics, they fail to master word reading and are routed into intervention systems rather than receiving preventative explicit instruction.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, struggling_readers, payer,
    powerless, biographical, trapped, local).

% Promote explicit, systematic, cumulative instruction based on reading science, especially for students with dyslexia. Often excluded from state standard-setting committees, ed school curricula, and mainstream literacy conferences dominated by balanced literacy frameworks.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, structured_literacy_advocates, excluded,
    organized, generational, constrained, national).

% Conduct empirical research on reading acquisition, eye movements, phonological processing, and instructional efficacy. Their findings frequently support explicit phonics, but they do not control classroom policy or teacher training pathways.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__balanced_literacy_reading, cognitive_scientists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__balanced_literacy_reading, literacy_publishers).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__balanced_literacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the 'reading wars' stalemate by positing that systematic phonics instruction and meaningful text engagement are complementary rather than contradictory, offering a unified instructional framework around which districts, publishers, and teacher training programs can coordinate.
% TRANSFER_FUNCTION: Moves district and state education budgets into curriculum publishers and education schools via cyclical textbook adoptions, leveled reader purchases, and mandatory professional development; moves teacher cognitive labor into reconciling contradictory instructional demands; moves political capital from phonics-first and whole-language purists into a centrist coalition.
% ABSENT_VOICES: Students with dyslexia and their families are rarely represented in curriculum adoption decisions; cognitive scientists emphasizing explicit systematic instruction are often excluded from ed school governance and state standard-setting; classroom teachers who prefer structured literacy are discouraged from dissenting within district-mandated frameworks.
% DISAPPEARANCE_RATIONALE: If the balanced literacy framework vanished, some districts would migrate toward structured literacy mandates (especially where dyslexia advocacy has gained political traction), others might drift back toward whole-language practices, and publisher catalogs would pivot; the field would likely fragment rather than converge.
% FOUNDING_PROBLEM: The 'reading wars' conflict between phonics and whole language produced curricular incoherence, teacher confusion, and political paralysis in school districts during the 1980s and 1990s; a synthesis was sought to end the conflict and unify instructional practice.
% FOUNDING_PROBLEM_CORROBORATION: Balanced literacy researchers and education schools attest the reading wars required a synthesis; phonics advocates and cognitive scientists outside the balanced literacy camp attest the problem was not a lack of balance but the dominance of whole-language assumptions, and that balanced literacy functions as a rebrand rather than a resolution.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__balanced_literacy_reading, contested).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__balanced_literacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__balanced_literacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__balanced_literacy_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__balanced_literacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__balanced_literacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.55) is moderate: the constraint extracts through method churn and professional development cycles rather than direct rent, but the decoupling of publisher revenue from instructional efficacy is substantial. Suppression (0.42) is moderate: enforcement occurs through credentialing and textbook adoption, but the rise of structured literacy legislation and the science-of-reading movement has weakened the exclusivity. Theater ratio (0.48) is moderate-to-high: a significant share of balanced literacy activity is performative compliance with district mandates rather than evidence-based instructional change. Accessibility collapse (0.50) is moderate: structured literacy alternatives have become more visible and accessible, though they remain marginal in many ed school programs. Resistance (0.60) is substantial: phonics advocates, dyslexia parents, and reading scientists have mounted an effective political and empirical challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the education school and publisher seats, the constraint appears as a necessary coordination mechanism that ended destructive curricular warfare and provides teachers with an integrated toolkit. From the classroom teacher seat, it appears as an incoherent mandate requiring impossible daily synthesis of contradictory methods. From the struggling reader seat, it appears as a lottery depending on whether the local 'balance' tilts toward sufficient phonics. The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Balanced literacy researchers and education schools are structural beneficiaries (low d): their institutional identity and revenue depend on the doctrine's persistence. Literacy publishers are beneficiaries (low-to-moderate d): they extract directly from adoption cycles but can pivot. Classroom teachers are targets (moderate-to-high d): they bear the implementation labor and are identity-locked into district employment. Struggling readers are strong targets (high d): they are trapped by compulsory schooling and cannot exit to alternative instruction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâthe reading wars stalemateâmay be dead or misdiagnosed. If the problem was whole-language dominance rather than a lack of synthesis, then balanced literacy is not a resolution but a continuation. The R5 genealogy interview captures this: the founding problem status is contested, and corroboration from outside the benefiting parties is weak. This prevents mislabeling the doctrine as pure coordination (rope) or pure extraction (snare); the tangled rope classification fits because the coordination claim is structurally real (it did unify a field) while the extraction is simultaneously real (method churn revenue).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_synthesis_vs_rebrand,
    'Is balanced literacy a genuine epistemic synthesis of phonics and whole language, or a rebranded whole-language framework that admits token phonics to deflect political pressure?',
    'Systematic curriculum archaeology comparing phonics scope-and-sequence depth in 1990s whole-language, 2000s balanced-literacy, and 2020s materials; meta-analysis of student decoding outcomes in balanced-literacy versus structured-literacy districts, controlling for SES.',
    'If rebrand, the coordination function is cover and the constraint tilts toward snare with higher extractiveness and clearer victims (struggling readers denied systematic phonics). If genuine synthesis, the coordination function is real and the type remains tangled rope or potentially rope if extraction is incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_synthesis_vs_rebrand, empirical, 'Whether balanced literacy is a true synthesis or a whole-language rebrand').

omega_variable(
    method_churn_dependency,
    'To what extent do education schools and literacy publishers structurally depend on perpetual pedagogical churn for revenue and relevance?',
    'Financial analysis of publisher revenue cyclicality against curriculum adoption cycles; ed school enrollment data correlated with methodological rebranding events.',
    'If dependency is high, extraction is structurally embedded in the institutional ecosystem and the constraint is likely to persist beyond its instructional efficacy. If low, revenue is incidental and the constraint is primarily coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(method_churn_dependency, empirical, 'Institutional dependency on method churn revenue').

omega_variable(
    victim_identity_uncertainty,
    'Who bears the primary cost of balanced literacy implementation: classroom teachers (implementation burden), struggling readers (instructional dilution), or school districts (financial cost)?',
    'Longitudinal outcome studies tracking decoding proficiency in districts before and after balanced literacy adoption; teacher retention and burnout studies; district budget analysis.',
    'Determines which stakeholder seat carries the highest effective extraction and thus the computed seat classification. If struggling readers are the primary victims, the moral case for reclassification strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_identity_uncertainty, conceptual, 'Ambiguity about primary cost-bearing seat').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__balanced_literacy_reading, 0, 34).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blr_tr_t0, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(blr_tr_t5, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(blr_tr_t10, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(blr_tr_t15, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(blr_tr_t20, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(blr_tr_t25, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(blr_tr_t30, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 30, 0.5).
narrative_ontology:measurement(blr_tr_t34, literacy_acquisition_kernel__balanced_literacy_reading, theater_ratio, 34, 0.48).

% Extraction over time
narrative_ontology:measurement(blr_be_t0, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(blr_be_t5, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(blr_be_t10, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(blr_be_t15, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(blr_be_t20, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(blr_be_t25, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(blr_be_t30, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(blr_be_t34, literacy_acquisition_kernel__balanced_literacy_reading, base_extractiveness, 34, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(blr_su_t0, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(blr_su_t5, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(blr_su_t10, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(blr_su_t15, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(blr_su_t20, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(blr_su_t25, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(blr_su_t30, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement(blr_su_t34, literacy_acquisition_kernel__balanced_literacy_reading, suppression_requirement, 34, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__balanced_literacy_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, whole_language_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__balanced_literacy_reading, structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the literacy_acquisition_kernel, which conflates multiple structurally distinct claims about how reading is acquired. The kernel is decomposed into four readings: phonics_reading, whole_language_reading, structured_literacy_reading, and balanced_literacy_reading. Each reading has distinct epsilon, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
