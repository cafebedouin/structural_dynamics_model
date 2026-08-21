% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_acquisition_kernel__whole_language_reading, []).

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
 *   constraint_id: literacy_acquisition_kernel__whole_language_reading
 *   human_readable: Whole Language Reading Pedagogy
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   This constraint describes the 'whole language' pedagogical approach to
 *   literacy, which posits that reading emerges naturally from meaningful
 *   engagement with connected text, and that explicit phonics instruction is
 *   unnecessary or even harmful. It is one reading of the broader
 *   'literacy_acquisition_kernel' and stands in direct opposition to
 *   phonics-first approaches. The claimed type is 'tangled_rope' because it
 *   offers a coordination function (holistic reading experience) but
 *   simultaneously extracts significantly from students who require explicit
 *   decoding instruction, while suppressing alternative pedagogical methods.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.7).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.75).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, tangled_rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Pedagogy").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, 'ebe88a61-83d0-4632-ad0c-678375243884').
narrative_ontology:cs_kernel_codification('ebe88a61-83d0-4632-ad0c-678375243884', implicit).
narrative_ontology:cs_authority_grounding('ebe88a61-83d0-4632-ad0c-678375243884', practice).
narrative_ontology:cs_interpretation_layer_present('ebe88a61-83d0-4632-ad0c-678375243884').
narrative_ontology:cs_reading_relation('ebe88a61-83d0-4632-ad0c-678375243884', literacy_acquisition_kernel__phonics_reading, forecloses).
narrative_ontology:cs_reading_relation('ebe88a61-83d0-4632-ad0c-678375243884', literacy_acquisition_kernel__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('ebe88a61-83d0-4632-ad0c-678375243884', literacy_acquisition_kernel__structured_literacy_reading, forecloses).
narrative_ontology:cs_axiom('ebe88a61-83d0-4632-ad0c-678375243884', foundational, reading_is_natural_process).
narrative_ontology:cs_axiom_status(reading_is_natural_process, holdable).
narrative_ontology:cs_axiom_grounding('ebe88a61-83d0-4632-ad0c-678375243884', reading_is_natural_process, empirically_contingent).
narrative_ontology:cs_axiom('ebe88a61-83d0-4632-ad0c-678375243884', foundational, meaning_is_primary_goal).
narrative_ontology:cs_axiom_status(meaning_is_primary_goal, holdable).
narrative_ontology:cs_axiom_grounding('ebe88a61-83d0-4632-ad0c-678375243884', meaning_is_primary_goal, conventional).
narrative_ontology:cs_reference_frame('ebe88a61-83d0-4632-ad0c-678375243884', holistic_meaning_making_paradigm).
narrative_ontology:cs_drift_state('ebe88a61-83d0-4632-ad0c-678375243884', contemporary_science_of_reading_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('ebe88a61-83d0-4632-ad0c-678375243884', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, teachers_adopting_whole_language).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, whole_language_advocates).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_lacking_home_literacy_support).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, constructivist_learning_theory).
narrative_ontology:constraint_vindicates(literacy_acquisition_kernel__whole_language_reading, reading_as_meaning_making).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote and defend the whole language philosophy, influencing curriculum development, teacher training, and policy. They gain professional standing and ideological validation from its adoption. Exit means abandoning a core professional identity and theoretical framework.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, whole_language_advocates, agenda_setter,
    institutional, generational, identity_locked, global).

% Embrace the whole language approach, valuing its emphasis on student engagement and professional autonomy in curriculum design. They benefit from a pedagogical framework that aligns with their beliefs about natural learning, but face pressure if student outcomes are poor.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, teachers_adopting_whole_language, beneficiary,
    organized, biographical, identity_locked, local).

% Are disproportionately harmed by the lack of explicit decoding instruction, as they do not acquire phonics skills naturally through exposure. They struggle to read, fall behind peers, and experience reduced educational opportunities. Their 'exit' is often academic failure or dropping out.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_lacking_home_literacy_support, payer,
    powerless, immediate, trapped, local).

% Bear the emotional and financial costs of their children's reading difficulties. They often seek tutoring or alternative educational paths when the whole language approach fails their children, but face resistance from schools and limited options.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_of_struggling_readers, payer,
    moderate, biographical, constrained, local).

% Are systematically marginalized or dismissed within whole language-dominant educational systems. Their research and pedagogical recommendations for explicit decoding instruction are often labeled as 'harmful' or 'unnecessary', limiting their influence on curriculum and policy.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, phonics_advocates, excluded,
    powerful, generational, constrained, global).

% Conduct research on reading acquisition, often finding strong evidence for the necessity of explicit phonics instruction. They observe the pedagogical debates and outcomes, providing evidence that frequently contradicts whole language tenets, but their findings are often resisted by advocates.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, cognitive_scientists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__whole_language_reading, whole_language_advocates).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To foster a love of reading and make literacy acquisition a natural, joyful, and meaning-centered process, integrating reading with other language arts.
% TRANSFER_FUNCTION: Transfers pedagogical autonomy and professional identity validation to teachers and advocates, while transferring the burden of decoding skill acquisition to students' implicit learning and home environments, particularly extracting from those without rich literacy backgrounds.
% ABSENT_VOICES: Cognitive scientists emphasizing the importance of explicit decoding, and parents of struggling readers who are often told to 'wait and see' or that their child will 'catch up' rather than receiving targeted intervention.
% DISAPPEARANCE_RATIONALE: If whole language pedagogy vanished overnight, educational institutions would rapidly shift to explicit phonics or structured literacy approaches, curricula would be rewritten, teacher training programs would be overhauled, and the 'reading wars' debate would fundamentally change, reorganizing the entire field of literacy education.
% FOUNDING_PROBLEM: Overly mechanistic, decontextualized phonics instruction that alienated students, failed to develop comprehension, and reduced reading to a series of isolated skills rather than a holistic meaning-making process.
% FOUNDING_PROBLEM_CORROBORATION: Whole language advocates maintain that the problem of decontextualized instruction and lack of motivation is still live. However, cognitive scientists, structured literacy advocates, and many parents of struggling readers attest that the original problem has been overcorrected, leading to new and more widespread problems for many students, particularly those from disadvantaged backgrounds. Independent research on reading acquisition from outside the whole language paradigm supports the shifted-function reading.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_acquisition_kernel__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(literacy_acquisition_kernel__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because the approach systematically fails a significant portion of the student population, particularly those from print-poor environments or with learning differences, leading to long-term educational and economic disadvantages. Suppression is also high (0.75) as whole language advocates actively dismiss and marginalize explicit phonics instruction, often framing it as developmentally inappropriate or detrimental to motivation. Theater ratio is moderate (0.4) as the emphasis on 'natural' acquisition can sometimes mask a lack of effective instructional strategies for struggling readers. Resistance is high (0.7) due to ongoing 'reading wars' and increasing pressure from parents and cognitive scientists for evidence-based literacy instruction. Accessibility collapse is high (0.8) because within whole language frameworks, alternatives like explicit phonics are often deemed harmful, effectively collapsing their perceived viability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of whole language advocates and many teachers, this approach is a 'rope' that fosters a love of reading and respects child development. However, from the perspective of students who struggle to read and their parents, it operates as a 'snare' that denies them essential skills and traps them in a cycle of academic failure. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Whole language advocates and teachers adopting the approach are beneficiaries, gaining professional validation and pedagogical autonomy. Students lacking home literacy support and their parents are victims, bearing the costs of inadequate instruction. Phonics advocates are excluded, as their methods are actively suppressed. Cognitive scientists act as observers, providing analytical insights that often challenge the constraint's premises.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to make reading a joyful and meaningful experience, avoiding the perceived pitfalls of rote phonics. However, for a significant portion of learners, this mandate has atrophied, and the constraint now functions to preserve a pedagogical ideology and professional identity, even at the cost of student outcomes. The high extractiveness and suppression, coupled with contested founding problem status, indicate a potential mandatrophy where the coordination function has been overshadowed by extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_for_all_learners,
    'Is the whole language approach equally effective for all learners, or does its efficacy depend on pre-existing home literacy support and cognitive profiles?',
    'Longitudinal studies comparing reading outcomes across diverse student populations under whole language vs. explicit instruction, controlling for socioeconomic status and cognitive abilities.',
    'If efficacy is highly variable and dependent on external factors, the constraint''s extractiveness for vulnerable populations is confirmed as structural, strengthening its ''snare'' characteristics for those seats. If efficacy is universal, the extractiveness metrics would need re-evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_for_all_learners, empirical, 'Whether whole language is universally effective or selectively extracts from certain student groups.').

omega_variable(
    motivation_vs_decoding_tradeoff,
    'Does explicit decoding instruction genuinely harm reading motivation, or is the perceived harm a misattribution, and can both motivation and decoding be fostered simultaneously?',
    'Experimental studies comparing student motivation and reading outcomes in classrooms using integrated approaches (explicit phonics within meaningful contexts) versus pure whole language or pure phonics.',
    'If motivation and decoding can coexist, the whole language claim of ''harm'' becomes a false dilemma, weakening its justification for suppressing alternatives and potentially reclassifying it as a more purely extractive ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(motivation_vs_decoding_tradeoff, empirical, 'The validity of the claim that explicit phonics harms reading motivation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of explicit phonics instruction primarily structural (e.g., curriculum mandates, teacher training programs) or internalized (e.g., teachers'' deeply held beliefs about ''natural'' learning that resist alternative methods)?',
    'Post-policy-change observation: if explicit phonics instruction remains resisted or poorly implemented even after policy mandates it, reclassify as partially internalized suppression. If implementation follows policy, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as resistance to change persists even after external barriers are removed, making reform more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative reading pedagogies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t1970, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(lite_tr_t1980, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(lite_tr_t1990, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(lite_tr_t2000, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(lite_tr_t2010, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(lite_tr_t2020, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(lite_be_t1970, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(lite_be_t1980, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(lite_be_t1990, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(lite_be_t2000, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(lite_be_t2010, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(lite_be_t2020, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 2020, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t1970, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(lite_su_t1980, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(lite_su_t1990, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(lite_su_t2000, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(lite_su_t2010, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(lite_su_t2020, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'literacy_acquisition_kernel', focusing on the whole language approach. Its structural properties and metrics are distinct from other readings of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
