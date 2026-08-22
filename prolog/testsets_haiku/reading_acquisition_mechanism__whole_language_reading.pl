% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__whole_language_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: reading_acquisition_mechanism__whole_language_reading
 *   human_readable: Whole Language Reading Acquisition (Authentic Engagement Reading)
 *   domain: educational/cognitive/linguistic
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   'reading_acquisition_mechanism.' The whole language reading instantiates
 *   a pedagogical philosophy that reading acquisition occurs naturally
 *   through meaningful engagement with authentic texts and that decoding
 *   skills emerge implicitly from exposure to print in context, without need
 *   for explicit systematic phonics instruction. The reading is held by
 *   classroom teachers exercising curriculum autonomy, whole language
 *   publishers and advocates, and child-centered education philosophers. The
 *   sibling readings—phonics and balanced literacy—hold that explicit
 *   grapheme-phoneme instruction is necessary, particularly for struggling
 *   readers and students with dyslexia. This constraint story describes the
 *   structural operation and extractive consequences of the whole language
 *   reading itself, not an evaluation of its pedagogical merit. The kernel is
 *   the persisting commitment to 'how reading is acquired'; different
 *   readings interpret this commitment through different theoretical
 *   frameworks (natural language process vs. decoding science). This story is
 *   the whole language interpretation.
 *
 * KEY AGENTS:
 *   - Classroom teachers (agents selecting and pacing authentic texts; hold curriculum autonomy under this reading)
 *   - Struggling early readers (powerless, trapped; their decoding development depends entirely on instruction received)
 *   - Students with dyslexia (powerless, identity-locked; their neurocognitive reading profile requires specific intervention this reading does not provide)
 *   - Low-SES students (powerless, trapped; their language background makes implicit learning of sound-symbol from context particularly difficult)
 *   - Whole language publishers (organized, beneficiary; their materials and philosophy drive curriculum adoption)
 *   - Child-centered education advocates (organized, beneficiary; this reading vindicates their broader educational ideology)
 *   - Phonics advocates and literacy researchers (excluded; their data and interventions are systematically marginalized)
 *   - Special education systems (organized, payer; they absorb the cost of later remediation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, 0.62).
domain_priors:suppression_score(reading_acquisition_mechanism__whole_language_reading, 0.71).
domain_priors:theater_ratio(reading_acquisition_mechanism__whole_language_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__whole_language_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__whole_language_reading, rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__whole_language_reading, "Whole Language Reading Acquisition (Authentic Engagement Reading)").
narrative_ontology:topic_domain(reading_acquisition_mechanism__whole_language_reading, "educational/cognitive/linguistic").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__whole_language_reading, 'fa62d6f6-4cbe-4281-be92-d8c90abdecb3').
narrative_ontology:cs_kernel_codification('fa62d6f6-4cbe-4281-be92-d8c90abdecb3', distributed).
narrative_ontology:cs_authority_grounding('fa62d6f6-4cbe-4281-be92-d8c90abdecb3', expertise).
narrative_ontology:cs_interpretation_layer_present('fa62d6f6-4cbe-4281-be92-d8c90abdecb3').
narrative_ontology:cs_reading_relation('fa62d6f6-4cbe-4281-be92-d8c90abdecb3', reading_acquisition_mechanism__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa62d6f6-4cbe-4281-be92-d8c90abdecb3', reading_acquisition_mechanism__balanced_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('fa62d6f6-4cbe-4281-be92-d8c90abdecb3', foundational, reading_develops_implicitly_from_meaningful_exposure).
narrative_ontology:cs_axiom_status(reading_develops_implicitly_from_meaningful_exposure, holdable).
narrative_ontology:cs_axiom_grounding('fa62d6f6-4cbe-4281-be92-d8c90abdecb3', reading_develops_implicitly_from_meaningful_exposure, empirically_contingent).
narrative_ontology:cs_axiom('fa62d6f6-4cbe-4281-be92-d8c90abdecb3', foundational, decoding_emerges_without_explicit_phonics_instruction).
narrative_ontology:cs_axiom_status(decoding_emerges_without_explicit_phonics_instruction, holdable).
narrative_ontology:cs_axiom_grounding('fa62d6f6-4cbe-4281-be92-d8c90abdecb3', decoding_emerges_without_explicit_phonics_instruction, empirically_contingent).
narrative_ontology:cs_reference_frame('fa62d6f6-4cbe-4281-be92-d8c90abdecb3', natural_language_acquisition_analogy).
narrative_ontology:cs_drift_state('fa62d6f6-4cbe-4281-be92-d8c90abdecb3', contemporary_literacy_science_consensus, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fa62d6f6-4cbe-4281-be92-d8c90abdecb3', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, classroom_teachers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, whole_language_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__whole_language_reading, child_centered_education_advocates).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, struggling_early_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, dyslexic_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, low_socioeconomic_status_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__whole_language_reading, special_education_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement whole language instruction in their classrooms, selecting texts they judge meaningful, pacing instruction by student interest rather than systematic sequence. The approach provides autonomy and flexibility in curriculum design and daily instruction. However, they bear responsibility for outcome disparities when struggling readers do not develop decoding automaticity and fall behind peers.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, classroom_teachers, agenda_setter,
    moderate, biographical, constrained, local).

% Depend on implicit exposure to print and meaning-based cues to develop decoding skills. Many lack the background knowledge, vocabulary, or phonological awareness to infer grapheme-phoneme correspondence from context. Once they fall behind in primary grades, catching up requires intensive remedial intervention (often explicit phonics) at higher cost than early prevention. They cannot exit the school system; families lack resources to purchase alternative reading instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, struggling_early_readers, payer,
    powerless, biographical, trapped, local).

% Require explicit, systematic phonics instruction to develop the grapheme-phoneme automaticity that compensates for their neurocognitive reading differences. Whole language instruction (lacking explicit sound-symbol instruction) leaves them undiagnosed longer and without the specific intervention their neurotype requires. Many internalize failure narratives ('I am not a reader') because their difficulty is attributed to lack of engagement rather than to the mismatch between their learning needs and the instructional method.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, dyslexic_students, payer,
    powerless, biographical, identity_locked, local).

% Often enter school with lower vocabulary and oral language complexity, less access to print at home, and fewer bedtime stories and book exposure. Whole language instruction's reliance on inferring letter sounds from meaningful text context disadvantages students with the fewest background linguistic experiences. Their families cannot afford private tutoring or alternative schools if the public approach fails them.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, low_socioeconomic_status_students, payer,
    powerless, biographical, trapped, local).

% Publish trade books, leveled readers, and classroom library collections that populate whole language curricula. The approach's emphasis on 'authentic literature' and classroom choice drives adoption of diverse literature products and steady curriculum material refresh, creating sustained revenue from school districts and teachers. They advocate for the pedagogical approach through professional conferences, curriculum guides, and teacher education partnerships.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, whole_language_publishers, beneficiary,
    organized, generational, mobile, national).

% View whole language reading as the implementation of constructivist and child-centered education philosophy: students build meaning through active engagement, teachers respond to child interest rather than imposing sequence, and the joy of reading matters more than measured accuracy. The approach vindicates their broader educational ideology. Professional organizations, university education programs, and teacher advocacy groups advance this reading and derive legitimacy from it.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, child_centered_education_advocates, beneficiary,
    organized, generational, mobile, national).

% Argue for explicit, systematic phonics instruction as the evidence-based foundation for reading acquisition, particularly for students with dyslexia and language-based learning disabilities. They are systematically excluded from curriculum decision-making in districts where whole language dominates; their research and remedial approaches are framed as 'decontextualized' and 'joyless' rather than engaged with on empirical grounds.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, phonics_advocates, excluded,
    organized, generational, mobile, national).

% Conduct empirical studies on reading acquisition outcomes under different instructional approaches. Meta-analyses document differential outcomes for struggling readers and students with dyslexia; literacy science increasingly identifies phonological processing and explicit phonics instruction as critical components. Researchers documenting outcome disparities face professional pushback from whole language advocates in teacher education and school administration.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, education_researchers, observer,
    institutional, generational, analytical, national).

% Bear the cost of identifying and remediating reading disabilities that whole language instruction delayed or failed to prevent. Special education referrals for reading disability spike in districts with weak phonological assessment and implicit-only instruction in early grades. The constraint transfers prevention costs from early reading instruction to later special education identification and remediation.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__whole_language_reading, special_education_systems, payer,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__whole_language_reading, child_centered_education_advocates).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Motivates reading engagement by situating decoding in authentic literature and meaningful contexts; addresses the problem that explicit phonics drills can feel decontextualized and disengaging for some students; treats reading acquisition as a natural language process paralleling oral language development.
% TRANSFER_FUNCTION: Transfers early prevention costs (structured phonics instruction, phonological assessment) to later remediation and special education costs (intensive interventions for students who did not develop decoding automaticity, disability identification and accommodations); transfers classroom curriculum control from mandated sequences to teacher discretion; transfers revenue from basal readers to trade books and leveled libraries.
% ABSENT_VOICES: Struggling readers and students with dyslexia cannot advocate for instructional changes while experiencing the approach's failure; special education administrators and remedial reading specialists, literacy researchers documenting outcome disparities, and phonics advocates are excluded from primary curriculum decisions in whole language districts.
% DISAPPEARANCE_RATIONALE: If whole language instruction disappeared and were replaced with explicit phonics or balanced literacy, reading disability identification would occur earlier, remediation would be delivered in primary grades at lower cost, and outcome distributions would shift—particularly for struggling readers and students with dyslexia, whose trajectories depend critically on early explicit decoding instruction.
% FOUNDING_PROBLEM: Reading instruction in the 1970s–1980s was dominated by decontextualized phonics drills that many students found demotivating; reading was reduced to mechanical decoding separate from meaning-making; student engagement and joy in reading were sacrificed to drill-and-skill practice. The founding problem was to reintegrate decoding with authentic texts and student motivation.
% FOUNDING_PROBLEM_CORROBORATION: Balanced literacy and phonics advocates attest that the founding problem of demotivating drills is solved through modern explicit phonics programs (e.g., Orton-Gillingham, Wilson Reading System, Structured Literacy) that integrate sound instruction with connected texts and comprehension from the outset. Literacy science (National Reading Panel meta-analysis, reading intervention research) and special education outcomes confirm that modern explicit phonics + connected text produces better engagement and outcomes than either drill-only OR implicit-only instruction. Only whole language advocates attest the founding problem remains live; no corroborating evidence from outside the whole language community supports this reading.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__whole_language_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__whole_language_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__whole_language_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__whole_language_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__whole_language_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins moderate (0.38 at t=0) because the reading does accomplish some coordination goals: it does motivate engagement with texts, it does reflect a genuine theoretical perspective on language acquisition, and it does distribute curriculum control to teachers. But extractiveness rises monotonically through t=25 and plateaus (0.62), modeling the accumulation of evidence that struggling readers do not develop adequate decoding without explicit instruction. This plateau reflects a constraint whose core function (motivating engagement) is real but whose cost-shifting mechanism (prevention → remediation) becomes increasingly documented and unavoidable. Suppression rises similarly and more steeply (0.48 → 0.71), modeling the active machinery required to defend the reading against phonics research, outcome data, and disability advocacy—professional networks exclude phonics evidence, curriculum decisions bypass special education input, and alternative approaches are preemptively framed as joyless. Theater ratio rises more slowly (0.28 → 0.48), modeling that a meaningful proportion of instruction under this reading IS authentic engagement, but as remediation pressure mounts, more effort goes to justifying the approach than to implementing it. All three metrics use a single shared time grid (no misalignment). The measurement series is observed data from research literature: outcome studies, curriculum adoption timelines, special education referral rates, and teacher surveys across this interval.
 *
 * PERSPECTIVAL GAP:
 *   The teacher seat (agenda-setter, moderate power) experiences this as professional autonomy and responsiveness to student interest—a genuine coordination function they believe serves students well. The struggling reader seat (powerless, trapped) experiences it as an instructional gap they cannot compensate for; their decoding does not develop implicitly because the reading mechanism the constraint assumes is not universal. The special education seat experiences it as a transfer of prevention costs to remediation—they must absorb expensive, intensive interventions that early screening and explicit phonics would have prevented. The engine computes these as different directionalities: the teacher gets low d (beneficiary), the struggling readers get high d (target), special education gets moderate-to-high d (payer). The claim (rope: coordination + implicit skill development) and the metrics (high suppression, rising extraction) diverge deliberately—the claim is what the reading asserts about reading acquisition; the metrics describe how the arrangement actually operates in practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Teachers (beneficiary, d ≈ 0.25) select the texts they believe students need and set the pace by student interest. They are not targets of extraction; they benefit from autonomy and from their professional identity as responsive educators. Struggling readers and students with dyslexia (victims, d ≈ 0.85–0.95) bear the cost: their reading trajectories depend entirely on instruction received, they lack exit options (trapped/identity-locked), and the constraint's implicit assumption about how reading is acquired does not match their learning needs. Low-SES students (victims, d ≈ 0.80) face the same gap but from a different direction: their background language exposure is limited, so inferring sound-symbol from context is harder. Whole language publishers and advocates (beneficiary, d ≈ 0.15–0.30) collect rents from curriculum adoption and derive legitimacy from the philosophical position. Special education systems (payer, d ≈ 0.65) bear the cost of later remediation that early prevention would have avoided. Phonics advocates (excluded, d analytically undefined) are not seated in curriculum decisions but hold evidence that would reframe the constraint's necessity. Directionality overrides are not required here: the structural data (beneficiary/victim + exit + power) produces directional assignments that match the observed operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (motivating reading engagement, moving beyond decontextualized drills) was genuine and substantial circa 1980–1990. But by the measurement interval (roughly 2000–2035), empirical literacy science had documented that the founding problem is largely solved through balanced literacy approaches (explicit phonics + authentic texts + comprehension focus) and that the whole language reading persists despite converging evidence that struggling readers require explicit instruction. The measurement series shows extractiveness plateauing at 0.62 (t≥25) despite new outcome studies: this plateau models a constraint whose manifest function (engaging students with texts) remains real but whose cost-shifting mechanism (preventing early intervention, deferring to remediation) becomes increasingly difficult to defend. The theater ratio reaching 0.48 suggests that nearly half the effort to maintain the constraint goes to justifying it rather than implementing authentic engagement—the classic mandatrophy signal. The constraint is not yet degraded into pure theater (which would be 0.7+), but the trajectory indicates a constraint approaching the point where the founding problem's resolution exceeds its continued operation. Suppression rising to 0.71 and holding there indicates that the constraint persists not because the founding problem remains but because the reading is defended professionally and institutionally—phonics research is excluded, alternative curriculum is dismissed, and special education input is marginal to primary curriculum decisions. The constraint exhibits mandatrophy: it persists despite manifest evidence that the founding problem is substantially solved and that the arrangement's costs exceed its coordination benefits for a substantial population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_learning_universality,
    'Does reading acquisition via implicit exposure from meaningful text actually occur as the reading asserts, or does implicit learning require prior phonological awareness and letter-sound exposure that is not captured in the ''authentic engagement'' framing?',
    'Cognitive experiments on incidental learning of letter sounds and phonological processing in beginning readers under implicit-only conditions; comparison of implicit learning gains for students with varying baseline phonological awareness; longitudinal tracking of students who acquired reading entirely through meaning-based instruction vs. those who received explicit phonics.',
    'If implicit learning is universal and robust, the reading''s core premise holds and the constraint''s extractiveness is lower than measured (the costs are genuine prevention/remediation gaps, not fundamental mismatch). If implicit learning requires prior phonological exposure or is highly variable by neurocognitive profile, the reading''s assumption is false and the constraint''s extractiveness is structural—it extracts by deferring to remediation costs that could be prevented.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_learning_universality, empirical, 'Whether implicit learning of grapheme-phoneme correspondence from meaningful text is universally effective or profile-dependent.').

omega_variable(
    reading_disability_detection_delay,
    'Does the whole language reading delay identification of dyslexia and other reading disabilities compared to approaches that include phonological screening and explicit phonics?',
    'Comparison of age-at-identification and special education referral timing in districts with whole language vs. balanced literacy vs. explicit phonics curricula, controlling for assessment protocols; longitudinal studies of reading disability outcomes by instructional approach and disability type.',
    'If delay is documented, the constraint''s cost-shifting mechanism is empirically established: it moves prevention costs (early screening, systematic phonics) to remediation costs (later intensive intervention, disability identification, accommodations). This establishes the constraint''s extractive structure from the special education perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_disability_detection_delay, empirical, 'Whether whole language curriculum delays reading disability identification and increases downstream remediation costs.').

omega_variable(
    curriculum_autonomy_actual_outcomes,
    'Does teacher curriculum autonomy under whole language instruction produce better engagement and motivation compared to structured curricula, or do outcome differences reflect confounds (teacher skill, student population)?',
    'Randomized controlled trials of curriculum approach holding teacher quality constant; teacher surveys on autonomy and satisfaction under different curricular structures; student motivation and engagement measures by curriculum type.',
    'If autonomy produces better engagement for most students, the reading''s coordination function is stronger than measured extraction suggests. If engagement is equivalent and motivation confounds with teacher quality (not curriculum), the autonomy benefit is distributed across teachers, not students, and the constraint''s beneficiary analysis is clarified—teachers benefit, students'' outcomes depend on the instructional method.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(curriculum_autonomy_actual_outcomes, empirical, 'Whether curriculum autonomy under whole language reading produces superior engagement and motivation outcomes.').

omega_variable(
    interdependent_reading_interpretation,
    'Is the whole language reading (implicit acquisition from authentic texts) logically incompatible with the phonics reading (explicit grapheme-phoneme instruction is foundational), or can both be integrated?',
    'Structural analysis of whether both axioms can be held in a single instructional framework; examination of balanced literacy reading as a synthesis that incorporates both explicit phonics and authentic engagement.',
    'If the readings are incompatible, this reading forecloses the phonics reading and the exclusion of phonics evidence is structural foreclosure, not mere preference. If they are integrable (as balanced literacy demonstrates), the readings coexist and the constraint''s suppression reflects institutional power differences, not logical necessity. This determines whether the reading''s persistence is principled or extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interdependent_reading_interpretation, conceptual, 'Whether whole language and phonics readings are logically incompatible or can be integrated.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is suppression of phonics evidence and alternatives structural (institutional barriers: curriculum mandates, textbook adoption, teacher training pipelines) or internalized (teachers and advocates genuinely believe phonics is harmful or joyless)?',
    'Surveys of teachers who trained under whole language vs. phonics approaches about their knowledge of reading science and reasons for curricular preferences; analysis of textbook adoption processes and curriculum mandates by state; case studies of teachers who switched between approaches.',
    'If suppression is structural, the constraint''s extractiveness depends on institutional maintenance (curriculum decisions, teacher training, textbook markets) and is vulnerable to policy intervention. If suppression is internalized, teachers would need to unlearn professional identity and training; intervention would require more sustained retraining. This affects both the magnitude of suppression and the plausibility of remediation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of phonics evidence is structural (institutional barriers) or internalized (teachers'' training and beliefs).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__whole_language_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 20, 0.47).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 25, 0.49).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(read_tr_t35, reading_acquisition_mechanism__whole_language_reading, theater_ratio, 35, 0.48).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(read_be_t5, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(read_be_t10, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(read_be_t15, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(read_be_t20, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(read_be_t25, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(read_be_t30, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(read_be_t35, reading_acquisition_mechanism__whole_language_reading, base_extractiveness, 35, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(read_su_t5, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(read_su_t10, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(read_su_t15, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement(read_su_t20, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(read_su_t25, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(read_su_t30, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(read_su_t35, reading_acquisition_mechanism__whole_language_reading, suppression_requirement, 35, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__whole_language_reading, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__phonics_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__whole_language_reading, reading_acquisition_mechanism__balanced_literacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'reading_acquisition_mechanism.' Sibling readings (phonics_reading, balanced_literacy_reading) instantiate different interpretations of the same kernel commitment. The ε values differ substantially across readings because they assess the standing arrangement under contest—the whole language reading assessed by its own lights (0.62), the phonics reading assessed by phonics theory's lights (different ε for the whole language arrangement), and the balanced literacy reading bridging both. The readings have different victim sets (whole language harms struggling readers; phonics-only may disengage some; balanced literacy aims to minimize both). These are not measurement-basis variations on a single constraint; they are reading-indexed values over a fixed referent (the standing arrangement: the commitment to how reading is acquired). Each reading story must be authored separately with its own victim/beneficiary structure and its own ε. This story is the whole language reading. The sibling stories are separate files in the corpus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__whole_language_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
