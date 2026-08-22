% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__whole_language_meaning_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Whole Language Reading Pedagogy: Meaning-First Instruction
 *   domain: education/cognitive_science/literacy
 *
 * SUMMARY:
 *   This constraint models the whole-language reading pedagogy as one reading
 *   of the contested kernel 'reading acquisition legitimacy.' The reading
 *   asserts that reading is fundamentally meaning-making, that authentic
 *   literature should be primary from day one, and that decoding skills
 *   emerge naturally through immersion in context-rich text. The pedagogy
 *   claimed to reject mechanical phonics instruction in favor of
 *   developmentally appropriate, child-centered methods. The constraint
 *   describes how this reading became institutionalized: trained teachers,
 *   professional networks, curriculum materials, and educational research
 *   created a framework that marginalized alternative readings
 *   (phonics-first, balanced, structured literacy) and positioned
 *   whole-language instruction as the legitimate approach. Struggling
 *   decoders—particularly those without rich home literacy environments—bear
 *   the costs: they receive running records and guided reading instead of
 *   explicit phonological instruction, fall behind their peers, and
 *   internalize reading failure as personal inability. The measurement series
 *   document extraction and suppression accumulating over 44 years
 *   (1980–2024), with plateauing around 2018 as evidence-based literacy
 *   science produced contradictory findings that the constraint had to
 *   suppress.
 *
 * KEY AGENTS:
 *   - progressive_educators: institutional authority for whole-language pedagogy (powerful/organized, constrained exit)
 *   - whole_language_researchers: career advancement tied to the reading's legitimacy (institutional, mobile exit)
 *   - high_literacy_environments: affluent schools/homes where implicit decoding support compensates for lack of explicit instruction (powerful, arbitrage exit)
 *   - struggling_decoders: children with phonological deficits who need explicit instruction but receive meaning-based observation instead (powerless, trapped exit)
 *   - phonological_deficit_learners: children who internalize reading failure as personal inadequacy under whole-language framing (powerless, identity-locked exit)
 *   - low_resource_schools: serving economically disadvantaged students without incidental language exposure; told whole-language respects development while gaps widen (moderate, constrained exit)
 *   - phonics_advocates: excluded from the discourse; cite evidence for explicit instruction, marginalized as traditional (organized, constrained exit)
 *   - literacy_science_community: observing increasingly contradictory findings (institutional, analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.68).
domain_priors:suppression_score(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.72).
domain_priors:theater_ratio(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__whole_language_meaning_primacy, rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__whole_language_meaning_primacy, "Whole Language Reading Pedagogy: Meaning-First Instruction").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__whole_language_meaning_primacy, "education/cognitive_science/literacy").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__whole_language_meaning_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__whole_language_meaning_primacy, '996c6436-2deb-443c-b60b-0993a6a187a4').
narrative_ontology:cs_kernel_codification('996c6436-2deb-443c-b60b-0993a6a187a4', distributed).
narrative_ontology:cs_authority_grounding('996c6436-2deb-443c-b60b-0993a6a187a4', extraction).
narrative_ontology:cs_interpretation_layer_present('996c6436-2deb-443c-b60b-0993a6a187a4').
narrative_ontology:cs_reading_relation('996c6436-2deb-443c-b60b-0993a6a187a4', reading_acquisition_legitimacy__phonics_decoding_primacy, coexists_with).
narrative_ontology:cs_reading_relation('996c6436-2deb-443c-b60b-0993a6a187a4', reading_acquisition_legitimacy__balanced_literacy_integration, coexists_with).
narrative_ontology:cs_reading_relation('996c6436-2deb-443c-b60b-0993a6a187a4', reading_acquisition_legitimacy__structured_literacy_remediation, influences).
narrative_ontology:cs_axiom('996c6436-2deb-443c-b60b-0993a6a187a4', foundational, reading_as_meaning_construction).
narrative_ontology:cs_axiom_status(reading_as_meaning_construction, holdable).
narrative_ontology:cs_axiom_grounding('996c6436-2deb-443c-b60b-0993a6a187a4', reading_as_meaning_construction, deontological).
narrative_ontology:cs_axiom('996c6436-2deb-443c-b60b-0993a6a187a4', foundational, implicit_skill_emergence_from_context).
narrative_ontology:cs_axiom_status(implicit_skill_emergence_from_context, overridden).
narrative_ontology:cs_axiom_grounding('996c6436-2deb-443c-b60b-0993a6a187a4', implicit_skill_emergence_from_context, empirically_contingent).
narrative_ontology:cs_reference_frame('996c6436-2deb-443c-b60b-0993a6a187a4', authentic_literature_immersion).
narrative_ontology:cs_drift_state('996c6436-2deb-443c-b60b-0993a6a187a4', contemporary_neuroscience_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('996c6436-2deb-443c-b60b-0993a6a187a4', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, progressive_educators).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_researchers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__whole_language_meaning_primacy, high_literacy_environments).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_decoders).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonological_deficit_learners).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__whole_language_meaning_primacy, low_resource_schools).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_is_meaning_construction).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, authentic_texts_superior_to_decodables).
narrative_ontology:constraint_vindicates(reading_acquisition_legitimacy__whole_language_meaning_primacy, decoding_skills_emerge_naturally_from_context).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and implement whole-language pedagogy in classrooms. Frame the approach as child-centered, developmentally appropriate, and respecting children's natural language acquisition. Author curriculum guidance, direct professional development, and set evaluation standards that reward authentic-text use and minimize structured phonics. They benefit from the approach's alignment with constructivist theory and from institutional recognition as progressive educators.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, progressive_educators, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__whole_language_meaning_primacy, progressive_educators, beneficiary).

% Conduct research studies and publish findings that support meaning-first reading instruction. Their career advancement, grant funding, and institutional prestige depend on the legitimacy of whole-language theory. Their research is cited in policy and curriculum design as evidence for the approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, whole_language_researchers, beneficiary,
    institutional, biographical, mobile, national).

% Private schools, affluent districts, and homes with rich print saturation where children encounter language-dense environments, parental read-alouds, and peer modeling of literacy. These settings provide incidental phonological exposure and contextual language support that allow decoding to emerge even without explicit instruction. Their success is cited as proof that whole-language methods work universally.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, high_literacy_environments, beneficiary,
    powerful, generational, arbitrage, global).

% Children with dyslexia, phonological processing deficits, or limited phonemic awareness who need explicit instruction in sound-symbol mapping to decode words. In whole-language classrooms, they receive 'guided reading' and 'running records' (teacher observes miscues, provides meaning-based cues) but often do not receive systematic phonics instruction. Many fall further behind as peers advance, internalizing reading failure.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, struggling_decoders, payer,
    powerless, biographical, trapped, local).

% Children with specific language impairment, auditory processing issues, or non-dominant English exposure whose phonological development is slower than peers. They are positioned as 'not yet ready' for explicit instruction and are placed in lower reading groups where they receive simplified texts and less challenging word study. They internalize the message that reading difficulty is a personal deficit, not a pedagogical mismatch.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonological_deficit_learners, payer,
    powerless, biographical, identity_locked, local).

% School districts serving economically disadvantaged students, where many children come from homes with limited access to books, fewer parental read-alouds, and less print saturation. These schools are told that whole-language instruction respects children's natural development and that explicit phonics is unnecessary or even harmful. Yet their students lack the incidental language exposure that allows meaning-based reading to work. The approach widens achievement gaps.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, low_resource_schools, payer,
    moderate, generational, constrained, regional).

% Researchers, educators, and speech-language pathologists who advocate for explicit phonics instruction, particularly for struggling readers. They cite brain imaging and intervention research showing that phonological awareness and decoding instruction improve outcomes. They are marginalized in whole-language dominated discourse as 'traditional,' 'bottom-up,' or 'narrow in scope.'
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, phonics_advocates, excluded,
    organized, biographical, constrained, national).

% Cognitive scientists, neuroscientists, and reading researchers who study how the brain learns to read. They conduct controlled experiments, meta-analyses, and longitudinal studies on reading instruction methods. Their role is to report findings on efficacy; they may be cited to support whole-language theory, but their data increasingly contradict it.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__whole_language_meaning_primacy, literacy_science_community, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__whole_language_meaning_primacy, progressive_educators).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__whole_language_meaning_primacy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates pedagogical legitimacy around a coherent theory of reading as meaning-making: teachers share a common framework (authentic texts, student choice, attention to comprehension over decoding precision), peer learning through literature circles, and a common language for discussing children's reading development. Provides a unified approach that feels child-centered and developmentally respectful across diverse classrooms.
% TRANSFER_FUNCTION: Moves pedagogical authority and institutional legitimacy away from systematic phonics instruction toward whole-language interpretation. Teachers trained in whole-language methods receive professional standing and curriculum decision-making power. Struggling decoders receive running-record observation and guided reading instead of explicit phonics—transferring the cost of reading failure from the system to the child, who internalizes it as personal inability.
% ABSENT_VOICES: Speech-language pathologists who work with children with phonological deficits; neuroscientists studying reading acquisition; struggling readers themselves, particularly those from low-literacy homes who cannot compensate with contextual knowledge; parents of children with dyslexia seeking evidence-based remediation; literacy science researchers outside the whole-language tradition.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared, classroom instruction would bifurcate rapidly: schools with resources would integrate explicit phonics with literature (hybrid approaches already in use); low-resource schools would shift toward structured literacy approaches with evidence-based phonological support. Reading achievement gaps would narrow; struggling decoders would receive diagnostic-prescriptive instruction instead of meaning-based observation. The pedagogical authority of progressive educators would decline relative to evidence-based literacy science. Professional networks, curriculum publishers, and teacher-training programs would reorganize around balanced or structured approaches.
% FOUNDING_PROBLEM: Early reading instruction was heavily phonics-centric and mechanical, often using decodable primers divorced from meaningful text. Children who memorized letter-sounds without understanding reading's purpose, and children in restrictive, skill-drill regimens with little authentic literature exposure, experienced reading as joyless and disconnected from language meaning.
% FOUNDING_PROBLEM_CORROBORATION: Progressive educators attest the founding problem remains: phonics-first instruction can be mechanical and joyless. Literacy science researchers and special educators (outside the whole-language tradition) attest the problem is substantially solved by balanced approaches (phonics + literature concurrently), and that the remedy (whole-language meaning-emphasis) has created a worse problem: struggling decoders without explicit skill instruction, falling behind and internalizing reading failure. Longitudinal data from low-resource schools (audits of reading achievement gaps), meta-analyses of reading intervention research (National Reading Panel, 2000; NICHD meta-analyses), and neuroscience studies of phonological processing (brain imaging during reading tasks) support the contested reading. The National Assessment of Educational Progress (NAEP) data showing stagnant reading achievement since the 1990s when whole-language dominance was strongest, and subsequent gains when balanced approaches were adopted, support the literacy-science assessment that the founding problem is dead.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__whole_language_meaning_primacy, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__whole_language_meaning_primacy, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__whole_language_meaning_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68 at interval end) and rising over the interval (0.35 at 1980, peaking at 0.68 by 2018, then plateauing). The extraction is the transfer of pedagogical authority away from phonics and toward whole-language interpretation, and the downstream harm: struggling decoders lose access to explicit instruction under the cover of a 'meaning-centered' pedagogy that only works for children with rich literacy environments. Suppression is higher still (0.72 at interval end) because maintaining the reading requires active suppression of contradictory evidence—meta-analyses showing phonics efficacy are reframed as 'skills in isolation,' brain imaging of phonological processing is dismissed as 'mechanistic,' and intervention research showing explicit instruction helps struggling readers is contextualized as 'limited in scope.' Theater ratio rises from 0.18 to 0.41: the constraint's functional core (authentic literature exposure) is real, but a growing share of activity is theater—running records that observe miscues without addressing their cause, reading groups organized by whole-language philosophy rather than diagnostic need, professional development sessions on 'meeting children where they are' that avoid explicit phonological instruction. Accessibility collapse is moderate-high (0.62 at 2024): once the reading is embedded in teacher training, curriculum, assessment, and school culture, alternatives appear pedagogically backward or developmentally harmful, so even teachers who doubt the approach stay within it. Resistance falls over the interval (0.71 to 0.35 at structural level, 0.51 to 0.35 at individual level), showing that initial skepticism from phonics advocates and special educators weakened as the reading's institutional power accumulated—the phonics advocates were organized but lacked the professional networks and publishing infrastructure that progressive educators built. The coercion grid shows differential suppression across levels: organizational suppression is highest (0.78 at 2024) because curriculum adoption, teacher licensure, and administrative oversight enforce the reading; individual resistance is lowest (0.35) because teachers and parents internalize the reading as developmentally correct and feel personally committed to child-centered approaches. Class-level suppression is high (0.68) because teachers in low-resource schools are told the constraint respects all learners while it systematically underserves those without home literacy support.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting progressive educators compute the constraint as genuine coordination (authentic literature exposure, teacher autonomy, child-centered philosophy), while the struggling-decoder seats compute it as pure extraction masked by developmental rhetoric. The high_literacy_environments benefit from the reading because their children receive incidental phonological exposure at home, making decoding 'emerge naturally' as the theory predicts; their success is cited as universal proof. Low_resource_schools and struggling_decoders experience the same constraint as pedagogical malpractice because there is no home compensation. The engine computes per-seat types from power + exit + extraction structure; the structural data (beneficiaries/victims) should make the divergence transparent.
 *
 * DIRECTIONALITY LOGIC:
 *   Progressive educators are low-d beneficiaries (they set the agenda, defend the reading, benefit from its institutional legitimacy; d near 0.1–0.2). Whole-language researchers are beneficiaries (their careers and grant funding depend on the reading's credibility; d near 0.15–0.25). High-literacy environments are beneficiaries in disguise—they benefit from the reading's success without bearing its costs because incidental support compensates (d near 0.2–0.3, lower than educators because they do not actively maintain it). Struggling decoders and phonological-deficit learners are high-d targets (trapped or identity-locked exit, bearing the cost of lack of explicit instruction, no voice in pedagogy; d near 0.8–0.95). Low-resource schools are targets (moderate power, constrained exit, bearing the cost of methods that only work with home support; d near 0.65–0.75). Phonics advocates are excluded (they have something to say but no seat in curriculum decisions; not a d actor, but their exclusion is structural to the constraint's persistence). Overrides: none needed if beneficiary/victim declarations and exit options are clean; the derived d should track the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mechanical, joyless phonics-first instruction) is real and was addressed by whole-language pedagogy's emphasis on authentic literature and meaning-making. However, the founding problem is substantially solved by balanced approaches (explicit phonics integrated with literature, used in many schools by 2024), yet the whole-language constraint persists unchanged—it is no longer solving the original problem but persisting as institutional inertia and professional identity protection. The mandatrophy is partial (not total) because the constraint's coordination function (unified pedagogical framework, authentic literature, teacher autonomy) is still alive, but the coordination is now coupled to extraction (suppression of phonics evidence, harm to struggling decoders). The constraint should compute as tangled_rope or piton depending on how theater_ratio and resistance interact; if theater_ratio is high and resistance is low, it is approaching piton (defended by institutional inertia more than by evidence). The measurement series show theater_ratio plateauing after 2018, possibly because evidence-based literacy science has made theatrical maintenance exhausting—the founding problem is dead enough that the constraint should sunset, but institutional actors lack incentive to unwind it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implicit_vs_explicit_skill_emergence,
    'Do decoding skills genuinely emerge naturally from immersion in authentic text for all learners, or only for those with adequate phonological awareness and home literacy support?',
    'Randomized controlled trials comparing whole-language instruction to explicit phonics instruction for children with and without phonological deficits, stratified by home literacy environment. Longitudinal tracking of decoding trajectory by baseline phonological awareness and SES.',
    'If decoding emerges naturally only for children with pre-existing phonological support, the constraint''s claim to legitimate instruction for all learners is false, and the whole-language reading should be reclassified as pure extraction masked by developmental rhetoric. If decoding emerges naturally across all groups, the constraint''s coordination function is real and extraction claims are overstated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_vs_explicit_skill_emergence, empirical, 'Whether implicit skill emergence is universal or conditional on learner characteristics.').

omega_variable(
    authentic_literature_vs_decoding_mastery_trade_off,
    'Is the use of authentic literature (complex texts with varied orthography and vocabulary) incompatible with explicit decoding instruction, or are the two practices separable?',
    'Natural experiments from schools implementing balanced literacy (explicit phonics + authentic literature concurrently). Meta-analyses of reading instruction comparing phonics-only, whole-language-only, and balanced approaches on both decoding and comprehension outcomes.',
    'If authentic literature and explicit phonics are separable, the constraint''s framing (choice between them) is false, and the reading''s extraction mechanism (suppression of phonics as incompatible with meaning-making) is exposed. If they are incompatible, the reading''s coordination claim is stronger, but empirical data show balanced approaches outperform whole-language on decoding without sacrificing comprehension.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authentic_literature_vs_decoding_mastery_trade_off, empirical, 'Whether authentic literature and explicit phonics are pedagogically separable.').

omega_variable(
    naturalistic_emergence_for_struggling_learners,
    'For children with dyslexia, specific language impairment, or phonological processing deficits, do decoding skills emerge naturally from authentic-text immersion without explicit phonological instruction?',
    'Longitudinal cohort studies of children with identified phonological deficits under whole-language vs. explicit-phonics instruction. Post-intervention assessments of word decoding, phonological awareness, and reading fluency. Comparison of remedial intervention outcomes when explicit phonics is introduced late vs. early.',
    'If decoding does not emerge naturally for struggling learners, the constraint systematically harms them and should be classified as a snare. If decoding eventually emerges with late explicit instruction, the harm is documented but the legitimacy claim depends on opportunity cost. If decoding never reliably emerges without explicit instruction, the constraint is pure extraction of these children''s literacy development.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalistic_emergence_for_struggling_learners, empirical, 'Whether naturalistic skill emergence works for learners with phonological deficits.').

omega_variable(
    reading_science_paradigm_incommensurability,
    'Is the whole-language meaning-primacy reading using a different epistemology of reading science (constructivist, phenomenological) that is incommensurable with cognitive neuroscience findings, or are they measuring the same thing and contradicting?',
    'Philosophical analysis of the axioms each reading commits to. Clarification from prominent whole-language theorists of whether brain imaging and behavioral data on phonological processing are relevant to their claims or orthogonal (a conceptual boundary question, not empirical).',
    'If incommensurable, the readings are not directly competing claims but frameworks; the constraint''s extraction mechanism is institutional power consolidation more than falsifiable theory suppression. If commensurable, the constraint is suppressing evidence and should be classified accordingly. The resolution affects whether mandatrophy is a dead founding problem (solved by balanced approaches within the same epistemology) or a paradigm clash (not solvable within whole-language epistemic boundaries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_science_paradigm_incommensurability, conceptual, 'Whether whole-language and cognitive neuroscience readings of reading acquisition are measuring the same phenomenon.').

omega_variable(
    identity_lock_mechanism_in_struggling_readers,
    'When children experience reading failure under whole-language instruction, do they internalize it as personal inability (identity fusion with ''not a reader'') that persists even after receiving explicit phonics instruction?',
    'Longitudinal follow-up of children who experienced reading failure under whole-language then received explicit phonics remediation. Assessment of self-concept as readers, persistence in reading tasks, and engagement with reading after intervention. Comparison to children who received explicit phonics early.',
    'If identity lock is established (children continue to avoid reading despite competence gains), the constraint''s harm extends beyond instructional-method failure to internalized self-concept damage. This supports the ''identity_locked'' exit classification for struggling-decoder stakeholders and increases the classification''s extraction severity (identity lock makes exit from the internalized ''not a reader'' identity harder than exit from the pedagogical situation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_struggling_readers, empirical, 'Whether reading failure under whole-language creates identity-level damage that persists after remediation.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Do the foundational axioms of whole-language meaning-primacy logically foreclose the phonics-decoding-primacy reading, or can both readings be held as live normative positions by different parties?',
    'Logical analysis of the core axioms. Can a teacher/policy-maker believe both ''reading is fundamentally meaning-making'' AND ''decoding should be explicitly taught first''? Do these axioms contradict in any single framework, or do they occupy different normative space?',
    'If the axioms contradict (foreclose), the relation is ''forecloses.'' If they can both be held, the relation is ''coexists_with.'' This affects the classification of the reading-relations in cs_structure and the expected institutional outcome: foreclosure suggests one reading will eventually dominate; coexistence suggests persistent institutional fragmentation and ongoing contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Whether whole-language meaning-primacy and phonics-decoding-primacy axioms logically contradict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__whole_language_meaning_primacy, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1980, 0.18).
narrative_ontology:measurement_basis(read_tr_t1980, projected).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 1990, 0.24).
narrative_ontology:measurement_basis(read_tr_t1990, observed).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2000, 0.31).
narrative_ontology:measurement_basis(read_tr_t2000, observed).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2010, 0.38).
narrative_ontology:measurement_basis(read_tr_t2010, observed).
narrative_ontology:measurement(read_tr_t2018, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2018, 0.41).
narrative_ontology:measurement_basis(read_tr_t2018, observed).
narrative_ontology:measurement(read_tr_t2024, reading_acquisition_legitimacy__whole_language_meaning_primacy, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(read_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement_basis(read_be_t1980, observed).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement_basis(read_be_t1990, observed).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement_basis(read_be_t2000, observed).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement_basis(read_be_t2010, observed).
narrative_ontology:measurement(read_be_t2018, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement_basis(read_be_t2018, observed).
narrative_ontology:measurement(read_be_t2024, reading_acquisition_legitimacy__whole_language_meaning_primacy, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(read_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1980, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1980, 0.42).
narrative_ontology:measurement_basis(read_su_t1980, projected).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 1990, 0.54).
narrative_ontology:measurement_basis(read_su_t1990, observed).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2000, 0.64).
narrative_ontology:measurement_basis(read_su_t2000, observed).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement_basis(read_su_t2010, observed).
narrative_ontology:measurement(read_su_t2018, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2018, 0.72).
narrative_ontology:measurement_basis(read_su_t2018, observed).
narrative_ontology:measurement(read_su_t2024, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(read_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1980, tn=2024
narrative_ontology:measurement(read_grid_01, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(class), 1980, 0.28).
narrative_ontology:measurement(read_grid_02, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(class), 2024, 0.58).
narrative_ontology:measurement(read_grid_03, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(individual), 1980, 0.22).
narrative_ontology:measurement(read_grid_04, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(individual), 2024, 0.52).
narrative_ontology:measurement(read_grid_05, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(organizational), 1980, 0.42).
narrative_ontology:measurement(read_grid_06, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(organizational), 2024, 0.71).
narrative_ontology:measurement(read_grid_07, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(structural), 1980, 0.35).
narrative_ontology:measurement(read_grid_08, reading_acquisition_legitimacy__whole_language_meaning_primacy, accessibility_collapse(structural), 2024, 0.62).
narrative_ontology:measurement(read_grid_09, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(class), 1980, 0.71).
narrative_ontology:measurement(read_grid_10, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(class), 2024, 0.58).
narrative_ontology:measurement(read_grid_11, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(individual), 1980, 0.51).
narrative_ontology:measurement(read_grid_12, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(individual), 2024, 0.35).
narrative_ontology:measurement(read_grid_13, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(organizational), 1980, 0.62).
narrative_ontology:measurement(read_grid_14, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(organizational), 2024, 0.41).
narrative_ontology:measurement(read_grid_15, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(structural), 1980, 0.68).
narrative_ontology:measurement(read_grid_16, reading_acquisition_legitimacy__whole_language_meaning_primacy, resistance(structural), 2024, 0.52).
narrative_ontology:measurement(read_grid_17, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(class), 1980, 0.25).
narrative_ontology:measurement(read_grid_18, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(class), 2024, 0.52).
narrative_ontology:measurement(read_grid_19, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(individual), 1980, 0.19).
narrative_ontology:measurement(read_grid_20, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(individual), 2024, 0.48).
narrative_ontology:measurement(read_grid_21, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(organizational), 1980, 0.38).
narrative_ontology:measurement(read_grid_22, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(organizational), 2024, 0.66).
narrative_ontology:measurement(read_grid_23, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(structural), 1980, 0.31).
narrative_ontology:measurement(read_grid_24, reading_acquisition_legitimacy__whole_language_meaning_primacy, stakes_inflation(structural), 2024, 0.58).
narrative_ontology:measurement(read_grid_25, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(class), 1980, 0.32).
narrative_ontology:measurement(read_grid_26, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(class), 2024, 0.68).
narrative_ontology:measurement(read_grid_27, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(individual), 1980, 0.28).
narrative_ontology:measurement(read_grid_28, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(individual), 2024, 0.64).
narrative_ontology:measurement(read_grid_29, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(organizational), 1980, 0.45).
narrative_ontology:measurement(read_grid_30, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(organizational), 2024, 0.78).
narrative_ontology:measurement(read_grid_31, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(structural), 1980, 0.38).
narrative_ontology:measurement(read_grid_32, reading_acquisition_legitimacy__whole_language_meaning_primacy, suppression(structural), 2024, 0.69).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__whole_language_meaning_primacy, attachment_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__whole_language_meaning_primacy, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__whole_language_meaning_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'reading_acquisition_legitimacy.' The sibling readings (phonics_decoding_primacy, balanced_literacy_integration, structured_literacy_remediation) are separate constraint stories, each with its own ε, stakeholder structure, and classification. All four readings share a contested kernel: what counts as legitimate reading instruction. The whole-language reading (this story) influences the others by defining what is normatively acceptable in the domain; the phonics reading forecloses the whole-language reading by asserting a contradictory core axiom (reading is decoding, not meaning-making); the balanced reading coexists with whole-language by offering a synthesis that both readings cite as evidence. The four stories linked here form a constraint family decomposed by the ε-invariance principle: the same domain label 'reading acquisition legitimacy' covers multiple structurally distinct claims with different extractiveness, different victim sets, and different legitimate authority structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
