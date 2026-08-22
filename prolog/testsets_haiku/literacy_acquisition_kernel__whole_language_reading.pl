% ============================================================================
% CONSTRAINT STORY: literacy_acquisition_kernel__whole_language_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Whole Language Reading Acquisition (Kernel Reading: Emergent Literacy via Meaningful Text Engagement)
 *   domain: educational/cognitive/social
 *
 * SUMMARY:
 *   The whole language reading framework asserts that reading acquisition is
 *   a natural process emerging from meaningful engagement with connected text
 *   and context; explicit phoneme-grapheme instruction is presented as
 *   mechanistic and demotivating. This reading privileges teacher
 *   professional judgment, rejects scripted curricula, and forgrounds
 *   comprehension and motivation over decoding accuracy. At the structural
 *   level, the framework extracts substantially from students lacking
 *   print-rich home environments and from those with neurobiological
 *   differences in phonological processing (dyslexia spectrum), while
 *   benefiting teachers' professional autonomy, progressive education
 *   advocates' ideological authority, and education schools that train
 *   according to whole language principles. This is a KERNEL READING of the
 *   contested 'literacy acquisition kernel' — one of four live readings
 *   (whole language, phonics, balanced literacy, structured literacy). The
 *   story generates this reading ONLY, claims it as rope (coordination +
 *   teacher autonomy), but the authored metrics describe substantial
 *   extraction from powerless students and active enforcement against
 *   competing instructional models. The claim/metric gap reflects the kernel
 *   contest: different readings author different ε values for the same core
 *   mechanism.
 *
 * KEY AGENTS:
 *   - Classroom teachers: moderate power, constrained exit, beneficiary of autonomy and professional judgment; core to the agenda-setting apparatus.
 *   - Students from low-print homes: powerless, trapped, payers — depend on instruction for literacy background; extractive asymmetry is highest here.
 *   - Students with dyslexia spectrum: powerless, identity-locked, payers — neurobiological mismatch with inferential-context reading; early failure shapes identity.
 *   - Progressive education advocates: organized power, mobile exit, beneficiaries — institutional authority in ed schools, policy bodies, professional organizations.
 *   - Dyslexia/reading science researchers: powerful but excluded — evidence base challenges core premises but lacks institutional access to policy or curricula.
 *   - School districts: institutional power, beneficiaries — align with progressive frameworks, benefit from teacher retention and ed school compatibility.
 *   - Parents with home literacy capital: powerful, arbitrage exit, beneficiaries — children succeed under whole language because of home background; can supplement with tutors.
 *   - Trade book publishers: powerful, beneficiaries — whole language prioritizes trade books over phonics workbooks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_acquisition_kernel__whole_language_reading, 0.68).
domain_priors:suppression_score(literacy_acquisition_kernel__whole_language_reading, 0.72).
domain_priors:theater_ratio(literacy_acquisition_kernel__whole_language_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(literacy_acquisition_kernel__whole_language_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_acquisition_kernel__whole_language_reading, rope).
narrative_ontology:human_readable(literacy_acquisition_kernel__whole_language_reading, "Whole Language Reading Acquisition (Kernel Reading: Emergent Literacy via Meaningful Text Engagement)").
narrative_ontology:topic_domain(literacy_acquisition_kernel__whole_language_reading, "educational/cognitive/social").

domain_priors:requires_active_enforcement(literacy_acquisition_kernel__whole_language_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(literacy_acquisition_kernel__whole_language_reading, '91326910-b874-48ee-b027-827ec49347c1').
narrative_ontology:cs_kernel_codification('91326910-b874-48ee-b027-827ec49347c1', distributed).
narrative_ontology:cs_authority_grounding('91326910-b874-48ee-b027-827ec49347c1', distributed).
narrative_ontology:cs_reading_relation('91326910-b874-48ee-b027-827ec49347c1', literacy_acquisition_kernel__phonics_reading, coexists_with).
narrative_ontology:cs_reading_relation('91326910-b874-48ee-b027-827ec49347c1', literacy_acquisition_kernel__balanced_literacy_reading, influences).
narrative_ontology:cs_reading_relation('91326910-b874-48ee-b027-827ec49347c1', literacy_acquisition_kernel__structured_literacy_reading, coexists_with).
narrative_ontology:cs_axiom('91326910-b874-48ee-b027-827ec49347c1', foundational, reading_emerges_from_meaningful_engagement).
narrative_ontology:cs_axiom_status(reading_emerges_from_meaningful_engagement, holdable).
narrative_ontology:cs_axiom_grounding('91326910-b874-48ee-b027-827ec49347c1', reading_emerges_from_meaningful_engagement, empirically_contingent).
narrative_ontology:cs_axiom('91326910-b874-48ee-b027-827ec49347c1', foundational, explicit_phonics_is_unnecessary_and_harmful).
narrative_ontology:cs_axiom_status(explicit_phonics_is_unnecessary_and_harmful, holdable).
narrative_ontology:cs_axiom_grounding('91326910-b874-48ee-b027-827ec49347c1', explicit_phonics_is_unnecessary_and_harmful, empirically_contingent).
narrative_ontology:cs_reference_frame('91326910-b874-48ee-b027-827ec49347c1', natural_literacy_development_framework).
narrative_ontology:cs_drift_state('91326910-b874-48ee-b027-827ec49347c1', contemporary_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('91326910-b874-48ee-b027-827ec49347c1', '').
narrative_ontology:cs_kernel_id(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, classroom_teachers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, progressive_education_advocates).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_from_low_print_homes).
narrative_ontology:constraint_victim(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia_spectrum).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, school_districts_policy_makers).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, parents_high_print_homes).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, publisher_children_trade_books).
narrative_ontology:constraint_beneficiary(literacy_acquisition_kernel__whole_language_reading, ed_schools_teacher_preparation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Implement whole language reading instruction in classrooms. Under this reading, teachers are positioned as trusted professionals whose judgment about student engagement and text selection is the core instructional asset. They choose high-quality children's literature, model fluent reading, discuss story meaning, and guide students to infer unfamiliar words from context and illustrations. The constraint grants them professional autonomy — no scripted curriculum, no lockstep pacing. However, teacher preparation programs aligned with whole language typically do not require coursework in phonological awareness, phoneme-grapheme instruction, or dyslexia-related reading differences. When students fall behind, teachers lack the instructional tools to diagnose phonological-processing deficits or provide explicit decoding instruction. District mandates to 'teach whole language' can restrict their ability to supplement with phonics, especially for struggling students, even when they recognize the need.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, classroom_teachers, agenda_setter,
    moderate, biographical, constrained, national).

% Depend almost entirely on classroom instruction for their literacy development. Whole language pedagogy assumes students arrive at school with substantial background knowledge: exposure to books, familiarity with story structures and conventions, letter-sound awareness developed through family reading routines. For students without this home literacy capital, the pedagogical assumption is false. They are expected to infer phonetic patterns from context and repeated exposure to connected text — a process that requires phonological awareness and working memory resources they may not have consolidated. When they fall behind, the constraint attributes the gap to insufficient motivation or engagement ('they just need more meaningful text and a reason to read') rather than to instructional mismatch (they need explicit phonological awareness and decoding instruction). They cannot exit: school attendance is compulsory, and alternative instructional routes (explicit phonics, structured literacy) are often withheld, delayed until formal diagnosis, or presented as stigmatizing ('remedial' or 'special education').
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_from_low_print_homes, payer,
    powerless, biographical, trapped, national).

% Have neurobiological differences (often in phonological processing) that make inferring phoneme-grapheme mappings from context unreliable. Under whole language instruction focused on meaning and fluency, their reading trajectories diverge early and sharply from peers. They struggle to decode even as comprehension instruction proceeds, creating a mismatch between what they understand (meaning) and what they can independently read (decoding). This gap generates frustration, shame, and disengagement. They infer, often in elementary school, that reading is 'not for them' — a conclusion reinforced by peer comparison, teacher expectations ('he's a strong comprehender but struggles with decoding'), and the self-concept that they are fundamentally bad at reading. Access to explicit phonological awareness and phonics instruction is contingent on formal diagnosis of dyslexia (often delayed until 3rd–4th grade) and placement in special education (which carries stigma and segregation). By the time explicit instruction is available, identity-level beliefs ('I'm not a reader,' 'reading is hard') are already consolidated. Their exit is identity-locked: the belief is self-fulfilling.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, students_with_dyslexia_spectrum, payer,
    powerless, biographical, identity_locked, national).

% Advance whole language reading as part of a broader progressive education philosophy: child-centered learning, intrinsic motivation, respect for individual developmental pace, holistic development. The reading affirms their professional and ideological commitments. They benefit from the authority the reading grants them within education schools, state departments of education, district policy bodies, and professional organizations like the International Reading Association (which had substantial whole language influence). They direct teacher preparation curricula, textbook adoption committees, and professional development — all of which institutionalize whole language principles. Their institutional power and professional authority are sustained by the constraint's operation. They are invested in its continuation.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, progressive_education_advocates, beneficiary,
    organized, generational, mobile, national).

% Have accumulated evidence (cognitive neuroscience, longitudinal reading studies, randomized controlled trials of reading interventions, brain imaging studies) that explicit phonological awareness instruction and systematic phonics produce better reading outcomes for most students, and particularly for those with dyslexia and low-print-home backgrounds. Their research challenges the core claims of whole language: that phonics skills develop naturally from text exposure, that explicit decoding instruction is unnecessary, that meaning-focused reading is sufficient for all learners. They are largely excluded from education school curricula, policy conversations, and teacher preparation standards. When they publish research, it is dismissed as 'narrow,' 'reductionist,' 'deficit-focused,' or 'not real research' by whole language advocates. When they present at conferences, they face institutional dismissal and narrative marginalizing rather than scientific engagement. This exclusion is not incidental; it is structural to the constraint's maintenance.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, education_researchers_dyslexia_camp, excluded,
    powerful, generational, constrained, global).

% Adopt whole language as the district reading policy, often with genuine ideological commitment to progressive education. They benefit from improved teacher retention and morale (the autonomy and professional respect teachers experience under whole language is real), strong alignment with ed schools that train their hires (making teacher recruitment and credentialing compatible), and alignment with professional education organizations whose conferences and memberships they attend. They also experience political alignment with constituencies and school boards that value progressive pedagogy and worry about standardized test pressure. However, they defer systemic adoption of phonics-heavy or structured literacy curricula even as evidence accumulates, often by creating separate 'intervention tracks' (remedial, special education) rather than revising core classroom instruction. This allows them to claim evidence-responsiveness while maintaining the ideological commitment.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, school_districts_policy_makers, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, school_districts_policy_makers, beneficiary).

% Have high print literacy in their homes: they read aloud to children frequently, discuss stories, model fluent reading, have extensive book collections, and engage in language-rich conversations. Their children arrive at school with substantial background knowledge, phonological awareness, and letter-sound familiarity consolidated through family routines. Whole language instruction works well for these children because it builds on existing literacy capital. They benefit without bearing costs, because the pedagogical assumption (students come to school with literacy background) is built on their advantage. They can also supplement with additional reading, hire tutors, or provide explicit phonics instruction at home if their child needs it. Their children's success under whole language validates the approach and reinforces the framework's institutional adoption.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, parents_high_print_homes, beneficiary,
    powerful, biographical, arbitrage, national).

% Benefit directly from whole language adoption because the approach prioritizes trade book libraries (trade-bound children's literature) over phonics workbooks and controlled-vocabulary decodable readers. District and school budgets allocated to classroom libraries and trade book purchases expand under whole language policy. Publishers of traditional phonics curricula and controlled-vocabulary decodables face reduced market demand. Publishers of trade books for children's classrooms have strong incentive to support whole language adoption and to oppose structured literacy and phonics curricula.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, publisher_children_trade_books, beneficiary,
    powerful, generational, mobile, global).

% Design teacher preparation curricula and professional development around whole language principles. Faculty in reading education and literacy instruction were trained in and committed to whole language frameworks. Curricula typically require courses on children's literature, reading comprehension strategies, and engagement practices, but not on phonological awareness, phoneme-grapheme instruction, or dyslexia-related processing differences. The framework aligns with ed schools' broader pedagogical philosophy (child-centered, holistic, anti-standardization). Whole language adoption across K–12 schools creates demand for ed-school-trained teachers whose methods align with district policy. Shifting to structured literacy or phonics-heavy approaches would require curriculum revision, faculty retraining, and ideological repositioning — a substantial institutional cost. Ed schools have strong incentive to maintain whole language dominance.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, ed_schools_teacher_preparation, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(literacy_acquisition_kernel__whole_language_reading, ed_schools_teacher_preparation, beneficiary).

% Examines the constraint from outside the institutional stakeholder network. Observes the structural asymmetry: the whole language reading benefits teachers, progressive advocates, ed schools, and parents with high home literacy, while extracting substantially from students lacking print-rich home environments and from those with dyslexia-spectrum neurobiological differences. Notes the rising theater ratio (engagement claims substituting for outcome data), stable suppression (active exclusion of alternatives), and mandatrophy signature (founding problem solved; persistence is institutional-incentive driven). Sees the kernel contest as unresolved, with four viable readings of how reading is acquired, each with different ε and beneficiary/victim structures. Records that resolution depends on empirical evidence (longitudinal outcome studies, meta-analyses) and institutional willingness to revise frameworks when evidence requires it.
narrative_ontology:constraint_stakeholder(literacy_acquisition_kernel__whole_language_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(literacy_acquisition_kernel__whole_language_reading, progressive_education_advocates).
narrative_ontology:fixing_cost_class(literacy_acquisition_kernel__whole_language_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates classroom reading instruction around high-quality children's literature and teacher professional judgment about engagement and meaning-making. Solves the problem of how to motivate reading and foster comprehension without mechanistic drill-and-skill fragmentation.
% TRANSFER_FUNCTION: Transfers instructional authority and autonomy from externally-mandated phonics sequences to classroom teachers' professional judgment and from systematic decoding instruction to inferential and contextual reading strategies. Also transfers the burden of background literacy knowledge from the pedagogical system onto individual families' home literacy practices.
% ABSENT_VOICES: Researchers specializing in dyslexia, phonological processing, and literacy intervention are largely excluded from policy and curriculum conversations. Students from low-print homes, once they fall behind, have limited voice in choosing instructional alternatives. Parents advocating for explicit phonics are often framed as anti-progressive or deficit-focused rather than engaged as stakeholders.
% DISAPPEARANCE_RATIONALE: If whole language reading instruction disappeared, schools would shift to more explicit phonics emphasis, teacher training programs would require coursework in phonological awareness and systematic decoding, classroom instruction would focus more on word-level skills before connected text, and students from low-print homes would see acceleration in early reading development (though with trade-offs in motivation and engagement). Trade book publishers would face reduced demand; progressive education advocates would lose a flagship framework.
% FOUNDING_PROBLEM: Conventional basal reader programs and phonics workbooks were dull, fragmented, and demotivating; children were learning to decode mechanically without understanding or enjoying reading; comprehension was treated as secondary to decoding accuracy; teacher autonomy was constrained by scripted, assembly-line curricula.
% FOUNDING_PROBLEM_CORROBORATION: Progressive education advocates attest the founding problem is still live: phonics-heavy curricula remain available and risk reducing motivation. Dyslexia researchers and reading scientists attest the problem is partially solved (modern phonics curricula have improved substantially) and that the constraint's persistence is now driven by ideology rather than by evidence of motivational superiority. Longitudinal studies and intervention trials from outside the progressive education establishment document equivalent or superior comprehension outcomes from structured literacy approaches, including high engagement for many students.
narrative_ontology:disappearance_verdict(literacy_acquisition_kernel__whole_language_reading, world_rearranges).
narrative_ontology:founding_problem_status(literacy_acquisition_kernel__whole_language_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(literacy_acquisition_kernel__whole_language_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(literacy_acquisition_kernel__whole_language_reading, 'none', 1).
narrative_ontology:epsilon_provenance(literacy_acquisition_kernel__whole_language_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.48 to 0.68 over the interval (1975–2015) as evidence accumulates showing the framework underserves low-print and dyslexia-spectrum students, but adoption and enforcement intensify rather than relax. Theater ratio rises from 0.42 to 0.58 as the 'engagement' and 'motivation' claims come to substitute for actual reading outcome data (Goodhart drift: the motivational proxy replaces measurable fluency/accuracy). Suppression requirement stays high and stable (0.65–0.72) because the constraint's persistence depends on actively excluding or stigmatizing alternative instructional models (phonics programs are framed as 'bad pedagogy' rather than engaged on evidence). Accessibility_collapse is low (0.45) because alternatives exist and are well-articulated by a powerful research community; the constraint persists despite alternatives, not because they have collapsed. Resistance is high (0.71) because dyslexia researchers, structured literacy advocates, and evidence-focused educators mount persistent, evidence-based objections to the reading's core claims. The measurement grid is shared across all three metrics at all six time points (the temporal profile is singular, not per-metric).
 *
 * PERSPECTIVAL GAP:
 *   From the teacher's seat, the constraint is genuine coordination and autonomy — it trusts their judgment and frees them from mechanical syllable-by-syllable instruction. From the student-lacking-home-literacy seat, it is extraction — the pedagogy assumes background knowledge the student does not have, attributes failure to motivation rather than to instructional mismatch, and withholds the explicit instruction that would accelerate decoding. From the dyslexia-spectrum student's seat, it is extraction plus identity damage — early failure under a 'meaning-focused' approach creates a self-concept as a non-reader before the neurobiological mismatch is diagnosed. From the progressive advocate seat, it is ideological alignment and institutional authority. From the excluded researcher seat, it is suppression — evidence-based objections are dismissed as reductionist. The engine computes per-seat classifications from this structural data; the divergence is exactly what the framework exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (teachers, progressive advocates) are identified by what the constraint grants them: professional judgment, ideological authority, institutional control of teacher preparation. Their d values are low (beneficiary end) because they collect benefits without bearing the main costs — the costs are borne by students. Victims (low-print-home students, dyslexia-spectrum students) are identified by the asymmetric burden: they depend entirely on classroom instruction, have constrained or identity-locked exits, and bear the cost of the pedagogical mismatch. Their d values are high (target end) because they are trapped by the constraint's operation and extract from their accumulated literacy deficit. Teachers sit near symmetric (d~0.5) because while they benefit from autonomy, they also bear the cost of inadequate training in phonological processing, putting them in a position where they cannot effectively help struggling students. The constraint's persistence depends on active enforcement: alternatives (phonics, structured literacy) are available and effective but face institutional barriers to adoption.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (basal readers were dull, phonics was mechanical, teachers lacked autonomy) was real and live. By 2000–2010, the founding problem had been substantially solved: modern phonics curricula were more engaging, teacher autonomy had expanded across approaches, and evidence on reading acquisition had matured. Yet the constraint persists and intensifies. The mismatch between founding problem (solved) and persistence (enforced) is the mandatrophy signature. The theater ratio rise (0.42 to 0.58) shows increasing substitution of 'engagement' narrative for outcome data. Suppression requirement stability (0.65–0.72) shows the constraint depends on active exclusion of alternatives, not on evidence. The classically extractive pattern emerges: a framework that solved a real coordination problem (teacher autonomy, motivational engagement) has atrophied into rent-seeking (progressive institutional authority, ideological alignment) while continuing to extract from the most vulnerable students.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_resolution,
    'Which reading of the literacy acquisition kernel is correct: do students acquire reading primarily through emergent engagement with meaningful text (whole language), or do they require explicit instruction in phoneme-grapheme correspondence (phonics, structured literacy)?',
    'Longitudinal studies comparing reading outcomes (fluency, accuracy, comprehension, motivation) across students from different socioeconomic backgrounds and with different neurocognitive profiles, taught under different instructional approaches, with outcome measures independent of the instructional philosophy. Meta-analyses of intervention studies comparing whole language, phonics, balanced, and structured approaches.',
    'If whole language outcomes are superior or equivalent for all populations: the constraint''s extractiveness is lower (coordination genuinely works for everyone), and the framework''s beneficiary structure is broader. If structured literacy outcomes are superior for low-print and dyslexia-spectrum populations: the constraint''s extractiveness is substantially higher (the framework extracts from vulnerable populations), and mandatrophy diagnosis is confirmed (a framework solving a coordination problem has atrophied into institutional rent-seeking).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, empirical, 'Which reading of the literacy kernel is supported by evidence-based outcome data.').

omega_variable(
    home_literacy_assumption_dependency,
    'To what extent does whole language reading instruction depend on students arriving at school with substantial home literacy background (exposure to books, letter-sound knowledge, story structure familiarity)?',
    'Comparison of whole language outcomes for students with high vs. low print-home exposure, controlling for IQ and other predictors. Analysis of phonological awareness levels in student cohorts at school entry, correlating with home literacy measures. Study of which instructional additions accelerate reading for low-print-home students within whole language frameworks.',
    'If whole language outcomes depend critically on home literacy background: the constraint extracts from students lacking that background, and the pedagogical choice redistributes educational benefit toward already-advantaged students. If home literacy background is neither necessary nor sufficient: whole language coordination is more genuine across populations, and extractiveness is lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(home_literacy_assumption_dependency, empirical, 'Whether the whole language reading assumes and depends on home literacy capital that not all students possess.').

omega_variable(
    suppression_internalization_trajectory,
    'After students are exposed to whole language reading instruction and fall behind, does the suppression (lack of belief in one''s own reading ability) persist after exposure to explicit phonics instruction?',
    'Follow-up studies of students who receive whole language instruction, fall behind, are then exposed to explicit phonics-based intervention, and are tracked for reading identity, motivation, and self-efficacy beliefs. Compare recovery trajectories between students with early whole language failure and those with early phonics instruction + intervention.',
    'If suppression persists after intervention (students retain identity-level belief ''I''m not a reader'' even after decoding ability improves): the constraint''s suppression is partly internalized and carries forward as identity lock-in, making effective extraction higher than the structural measures alone suggest. If identity recovers with intervention: suppression is primarily structural and reversible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Whether suppression of reading identity is structural or internalized after early instructional mismatch.').

omega_variable(
    phonological_processing_neurobiology,
    'Is phonological processing (the ability to manipulate and represent the sound structure of language) a prerequisite for learning to read, or can it be bypassed through meaningful context and sight-word learning?',
    'Neurocognitive studies of reading acquisition in students with specific phonological processing deficits, measuring reading outcomes with and without explicit phonological awareness training. Imaging studies during reading tasks showing which neural systems are engaged under different instructional conditions.',
    'If phonological processing is a prerequisite: whole language''s assumption that phonics skills develop ''naturally'' from text exposure is empirically false for a portion of the student population, and the constraint systematically extracts from those students by denying them the instruction they need. If phonological processing can be bypassed: whole language''s core claim is vindicated, and extractiveness is lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(phonological_processing_neurobiology, empirical, 'Whether explicit phonological awareness and phonological processing are prerequisites for reading acquisition or can be bypassed.').

omega_variable(
    institutional_incentive_structure,
    'To what extent does the persistence of whole language instruction despite contradictory evidence reflect genuine commitment to the pedagogical claims versus institutional incentives (teacher autonomy reducing burnout, ed school ideological alignment, trade publisher market share)?',
    'Analysis of policy statements and curriculum decisions from districts that adopted whole language: do they cite research evidence or appeal to teacher autonomy/professional judgment? Interview studies of teachers and administrators about why they maintain whole language despite students falling behind. Institutional histories of ed schools showing which research gets cited in curricula.',
    'If persistence is primarily institutional-incentive driven: the constraint is substantially more extractive than the pedagogical justifications suggest, and suppression (of alternatives) is the operative mechanism. If genuine commitment to the pedagogy dominates: extractiveness is lower and the constraint is less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_incentive_structure, conceptual, 'Whether whole language persistence reflects pedagogical conviction or institutional incentive structures benefiting particular constituencies.').

omega_variable(
    alternative_reading_coexistence_possibility,
    'Can multiple readings of the literacy acquisition kernel coexist in the same institution/classroom, or does adoption of whole language exclude the possibility of structured literacy or systematic phonics approaches?',
    'Case studies of districts and schools that attempted to maintain multiple instructional approaches simultaneously (whole language for motivated learners, phonics for intervention); documentation of whether the approaches were genuinely integrated or whether one dominated. Analysis of teacher preparation programs that required training in multiple methods.',
    'If readings can coexist: the constraint''s structure shifts from foreclose to influences-or-coexists, and extraction is lower (alternatives remain available even if not preferred). If whole language adoption excludes alternatives: the relation is stronger toward foreclosure, and suppression is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_coexistence_possibility, conceptual, 'Whether whole language reading is compatible with simultaneous adoption of systematic phonics or structured literacy within the same framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_acquisition_kernel__whole_language_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lite_tr_t0, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(lite_tr_t5, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 5, 0.46).
narrative_ontology:measurement(lite_tr_t10, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 10, 0.51).
narrative_ontology:measurement(lite_tr_t15, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 15, 0.54).
narrative_ontology:measurement(lite_tr_t25, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 25, 0.57).
narrative_ontology:measurement(lite_tr_t40, literacy_acquisition_kernel__whole_language_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(lite_be_t0, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(lite_be_t5, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(lite_be_t10, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(lite_be_t15, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(lite_be_t25, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement(lite_be_t40, literacy_acquisition_kernel__whole_language_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(lite_su_t0, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(lite_su_t5, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(lite_su_t10, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(lite_su_t15, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(lite_su_t25, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(lite_su_t40, literacy_acquisition_kernel__whole_language_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_acquisition_kernel__whole_language_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(literacy_acquisition_kernel__whole_language_reading, 0.12).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__phonics_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__balanced_literacy_reading).
narrative_ontology:affects_constraint(literacy_acquisition_kernel__whole_language_reading, literacy_acquisition_kernel__structured_literacy_reading).

% DUAL FORMULATION NOTE:
% The 'literacy acquisition kernel' is a single persisting commitment (what claims about reading acquisition ground instructional policy) that different parties read structurally differently. This story instantiates the WHOLE_LANGUAGE_READING interpretation. Sibling readings (phonics_reading, balanced_literacy_reading, structured_literacy_reading) are separate constraint stories that contest the same kernel. Each reading has its own beneficiaries, victims, and ε value because each makes different claims about which students are the baseline and which instruction is necessary. This story documents the whole language reading's structure: it benefits teachers and progressive advocates, extracts from students without print-rich homes and those with dyslexia-spectrum differences, and persists despite substantial counter-evidence (rising theater ratio, stable suppression, mandatrophy signature). Sibling readings will document their own extraction patterns and structural asymmetries. The kernel contest is not resolved within any single reading; rather, it is a fact about the educational system that multiple readings coexist, each controlling institutional resources and policy in different regions/periods.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_acquisition_kernel__whole_language_reading, powerful, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
