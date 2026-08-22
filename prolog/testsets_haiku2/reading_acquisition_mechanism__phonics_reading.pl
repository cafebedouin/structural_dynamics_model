% ============================================================================
% CONSTRAINT STORY: reading_acquisition_mechanism__phonics_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_mechanism__phonics_reading, []).

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
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Systematic Phonics as Foundational Reading Instruction
 *   domain: educational_psychology/literacy_pedagogy/cognitive_science
 *
 * SUMMARY:
 *   The phonics reading instantiates the scientific-method side of a
 *   contested kernel: reading acquisition mechanisms. The constraint claims
 *   that explicit, systematic instruction in grapheme-phoneme correspondence
 *   is a foundational necessity for reliable early reading acquisition,
 *   particularly for struggling readers. This reading emerges from cognitive
 *   science research on automaticity, phonological awareness, and the
 *   dual-route model of reading. The sibling readings—whole language and
 *   balanced literacy—stake competing claims on the same kernel: reading
 *   acquisition occurs through implicit meaning-driven exposure (whole
 *   language) or through integration of phonics and literature in balanced
 *   practice (balanced literacy). The phonics reading's structural signature
 *   is high front-loaded instructional cost (systematic scope-and-sequence
 *   curricula, teacher training) with low long-term remediation cost for
 *   struggling readers; narrowed teacher discretion; and asymmetric benefit
 *   distribution (largest gains for students with implicit phonemic-awareness
 *   deficits). The constraint is CLAIMED as tangled_rope (real coordination
 *   function for struggling readers + extraction from teacher autonomy and
 *   alternative pedagogy advocates). Metrics track a 56-year interval showing
 *   rising institutional entrenchment (extractiveness climbing from 0.18 in
 *   1970 to 0.68 in 2026) while theater ratio declines (real phonics
 *   instruction becomes increasingly functional, less performative)—the
 *   inverse pattern of a constraint with growing empirical grounding.
 *
 * KEY AGENTS:
 *   - Struggling and dyslexic readers: powerless, trapped — the primary beneficiary seat; phonics provides measurably faster decoding gains
 *   - Reading science researchers: institutional, arbitrage — secondary agenda-setter; institutional legitimacy for phonics research programs and grant funding
 *   - Structured literacy publishers: powerful, mobile — beneficiary; market capture through phonics-aligned curriculum adoption
 *   - Whole language advocates: moderate power, constrained exit — payers; experience methodological authority loss and pressure to abandon or conceal their approach
 *   - Teacher professional autonomy: moderate power, identity-locked — payer seat; discretion narrowed, professional identity threatened, exit costly because it requires internalization of phonics as legitimate
 *   - Policy makers and legislators: institutional, analytical — primary agenda-setters; translate research claims into accountability structures that enforce phonics-aligned instruction
 *   - Elementary administrators: organized, constrained — secondary agenda-setters; implement and monitor phonics compliance; mediate between teacher resistance and policy mandate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.68).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.52).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Systematic Phonics as Foundational Reading Instruction").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational_psychology/literacy_pedagogy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, '3f8bfb74-b7a8-4916-8911-d0b2b6666956').
narrative_ontology:cs_kernel_codification('3f8bfb74-b7a8-4916-8911-d0b2b6666956', fixed_text).
narrative_ontology:cs_authority_grounding('3f8bfb74-b7a8-4916-8911-d0b2b6666956', expertise).
narrative_ontology:cs_interpretation_layer_present('3f8bfb74-b7a8-4916-8911-d0b2b6666956').
narrative_ontology:cs_reading_relation('3f8bfb74-b7a8-4916-8911-d0b2b6666956', reading_acquisition_mechanism__whole_language_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f8bfb74-b7a8-4916-8911-d0b2b6666956', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('3f8bfb74-b7a8-4916-8911-d0b2b6666956', foundational, grapheme_phoneme_automaticity_prerequisite).
narrative_ontology:cs_axiom_status(grapheme_phoneme_automaticity_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('3f8bfb74-b7a8-4916-8911-d0b2b6666956', grapheme_phoneme_automaticity_prerequisite, empirically_contingent).
narrative_ontology:cs_axiom('3f8bfb74-b7a8-4916-8911-d0b2b6666956', foundational, phonemic_awareness_foundational_necessity).
narrative_ontology:cs_axiom_status(phonemic_awareness_foundational_necessity, holdable).
narrative_ontology:cs_axiom_grounding('3f8bfb74-b7a8-4916-8911-d0b2b6666956', phonemic_awareness_foundational_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('3f8bfb74-b7a8-4916-8911-d0b2b6666956', cognitive_reading_science_mechanism).
narrative_ontology:cs_drift_state('3f8bfb74-b7a8-4916-8911-d0b2b6666956', contemporary_policy_implementation_2026, gap(codification_collapse, minor, true)).
narrative_ontology:cs_created_at('3f8bfb74-b7a8-4916-8911-d0b2b6666956', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_and_dyslexic_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, reading_science_researchers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, structured_literacy_publishers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, whole_language_advocates).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teacher_professional_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, parents_of_early_readers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, publishers_of_tradebook_curriculum).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, parents_of_early_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Students who do not acquire phonemic awareness implicitly from exposure to print benefit measurably from explicit, systematic grapheme-phoneme instruction. For this population, phonics is not extraction—it is the foundational mechanism that makes reading acquisition possible. They remain trapped in the schooling system until literacy is acquired; their only meaningful choice is the method of instruction.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_and_dyslexic_readers, beneficiary,
    powerless, biographical, trapped, national).

% Researchers in cognitive science, neuroscience, and educational psychology who study reading acquisition mechanisms benefit from the phonics reading's institutional legitimacy. Grant funding, publication venues, professional status, and policy influence flow toward research programs that align with phonics mechanism claims (automaticity, phonemic awareness prerequisites, dual-route models). They set the research agenda through professional organizations, peer review, and policy advisory roles. They have exit optionality: they could pursue alternative research programs, but funding and status flow toward phonics-aligned work.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, reading_science_researchers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, reading_science_researchers, agenda_setter).

% Curriculum and assessment publishers specializing in structured literacy, phonics scope-and-sequence, and evidence-based programs (e.g., Orton-Gillingham derivatives, Fundations, phonically-controlled text) capture significant market share when schools and districts mandate phonics-aligned instruction. They directly benefit from policy mandates and funding allocations that require phonics-aligned materials. They have mobile exit: they could publish alternative curriculum, but phonics publishing is more profitable.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, structured_literacy_publishers, beneficiary,
    powerful, biographical, mobile, global).

% Teachers, education professors, and practitioners trained in whole language and child-centered reading pedagogy experience the phonics reading as suppression of their methodological approach. Their pedagogical authority is undermined as whole language is reframed as 'empirically refuted' and 'anti-science.' They bear the cost of professional credential devaluation, curriculum replacement, and required adoption of phonics frameworks. Exit is constrained: adopting phonics language and methods means abandoning the core claim that decoding emerges implicitly from meaningful engagement.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, whole_language_advocates, payer,
    moderate, biographical, constrained, national).

% Teachers' discretion in reading method selection is narrowed by policy mandates, accountability systems, and curriculum adoptions that require explicit, systematic phonics instruction. Teachers who were trained in alternative methods, who experienced success with balanced or literature-first approaches, or who value professional autonomy in pedagogy experience this constraint as erosion of judgment. Exit is identity-locked: professional identity fuses with autonomy, so compliance requires internalizing the phonics framework as legitimate rather than externally mandated. Remaining committed to alternative methods requires professional identity reconstruction that many teachers resist.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, teacher_professional_autonomy, payer,
    moderate, biographical, identity_locked, national).

% Publishers of literature-based reading programs, authentic-text curricula, and meaning-driven literacy frameworks lose market adoption when schools mandate phonics-aligned programs. They are not fully excluded but are repositioned as supplementary (for fluency, comprehension, engagement AFTER foundational phonics is established) rather than foundational. Their product lines carry institutional stigma as lacking 'rigor' or 'science.' Exit is constrained: they could repackage materials as 'phonics-aligned,' but that negates the pedagogical philosophy underlying their approach.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, publishers_of_tradebook_curriculum, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, publishers_of_tradebook_curriculum, excluded).

% School and district administrators implement and monitor phonics-aligned reading instruction. They mediate between teacher resistance (especially from experienced teachers trained in alternative methods), parent demand (varying: some parents demand phonics, others demand engagement and meaning-making), and policy mandates (state standards, federal accountability). The constraint narrows their curricular discretion and creates compliance obligations (classroom observation, fidelity monitoring, professional development). Exit is constrained by state policy and accountability frameworks.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, elementary_administrators, agenda_setter,
    organized, biographical, constrained, regional).

% Lawmakers and policy bodies set accountability frameworks, standards, and funding conditions that effectively mandate phonics-aligned instruction. They translate reading science claims into policy enforcement structures. They decide whether phonics is a requirement or a recommendation, how it is assessed, and what curricula are approved for adoption. Their analytical position allows exit optionality: they could revert to teacher discretion or shift to balanced-literacy mandates, but political pressure from advocacy groups supporting phonics (parents with dyslexic children, reading science researchers, structured literacy publishers) constrains that exit.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, policy_makers_and_legislators, agenda_setter,
    institutional, generational, analytical, national).

% Parents, especially those whose children struggle with reading or who have been diagnosed with dyslexia, benefit from explicit phonics instruction and accelerated early-literacy gains. They experience the phonics reading as validating their concerns and as providing evidence-based intervention. Parents who value meaning-making, engagement, and authentic literature in early reading experience the phonics constraint as narrowing that opportunity. Exit is constrained: public school placement makes private-school alternatives expensive; their choice set is bounded by district policy.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, parents_of_early_readers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, parents_of_early_readers, payer).

% Cross-national and comparative educational research examines reading outcomes, instructional practices, and policy implementation across different systems (phonics-first, integrated, implicit-method dominant). They observe the constraint's operation, long-term effects, and differential impacts without partisanship. They produce evidence on transfer effects, reading motivation trajectories, and population-level literacy outcomes. They can identify cases where alternative methods succeed or where phonics-dominant systems produce unexpected failures.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, observer_comparative_reading_research, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__phonics_reading, structured_literacy_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a standardized, evidence-aligned foundational mechanism for reading instruction that ensures struggling readers receive systematic, sequenced, explicit phoneme-grapheme instruction rather than experiencing repeated failure under implicit-method expectation. Coordinates instructional sequence (phonemic awareness → phonics → fluency → comprehension) around cognitive load and prerequisite skill acquisition. Reduces variance in early reading outcomes by systematizing instruction rather than leaving it to teacher intuition and student implicit-learning capacity.
% TRANSFER_FUNCTION: Moves professional discretion from teachers to curriculum designers and reading science researchers: teacher agency in method selection is constrained by the requirement that phonics instruction be explicit, systematic, and scope-and-sequence aligned. Also moves market adoption to structured-literacy publishers (Orton-Gillingham derivatives, Fundations, SoR-aligned programs) and away from meaning-first and literature-based curricula. Moves classroom time from meaning-engagement to decoding-skill sequences during foundational grades.
% ABSENT_VOICES: Whole language advocates, meaning-first pedagogy practitioners, and older teachers with decades of successful experience using implicit and literature-centered methods are structurally constrained to silence or reframe their approaches as 'phonics-adjacent' or 'not actually whole language.' They would argue for child-centered, meaning-driven reading acquisition and the integration of phonics within authentic text engagement. Their voice is repositioned as empirically unfounded and harmful to struggling readers, which suppresses their testimony about student engagement, reading motivation, and long-term literacy love.
% DISAPPEARANCE_RATIONALE: If the phonics-reading constraint disappeared overnight, schools would return to mixed methods and teacher-discretion models. Curriculum adoption would rebalance toward literature-based and authentic-text programs. Teachers would recover professional discretion and authority over method selection. Reading instruction would not stop, but the standardized scope-and-sequence structure, the front-loaded phonics cost, and the asymmetric benefit distribution (struggling readers get fastest gains, but engagement-focused readers experience delayed meaning-making) would change. Some populations of students might experience slower initial decoding under reverted methods; others might engage more meaningfully with texts earlier and develop stronger reading motivation.
% FOUNDING_PROBLEM: Early reading instruction in the latter 20th century lacked systematic, evidence-based structure. Students experienced as reading-ready were taught through implicit exposure and meaning-first approaches. Students who didn't develop phonemic awareness implicitly were labeled 'learning disabled' rather than recognized as needing explicit instruction. Remediation was reactive, expensive, and often stigmatizing. Classroom reading methods varied wildly based on teacher training, intuition, and philosophy rather than aligned to cognitive mechanisms of reading acquisition.
% FOUNDING_PROBLEM_CORROBORATION: Reading science researchers (Seidenberg, Dehaene, Kilpatrick, the National Reading Panel, UK Rose Inquiry) attest that the founding problem is live and empirically documented: struggling readers benefit measurably from explicit phonics, and implicit methods fail too many students. Whole language and balanced-literacy advocates contest whether the problem is as severe as claimed, whether phonics-first is the necessary or best solution, and whether the phonics reading's framing ignores successful implicit-method cases and diminishes engagement and meaning-making in early literacy. Comparative education researchers outside the phonics-advocacy camp document that alternative countries with strong reading outcomes use mixed methods, and that the phonics-dominant US model produces its own extraction by narrowing teacher discretion and crowding out engagement with diverse literature in early grades. No single authority outside the competing advocacy groups can adjudicate the dispute; the founding problem is genuinely contested.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_mechanism__phonics_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_mechanism__phonics_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_mechanism__phonics_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the constraint extracts professional discretion from teachers and market share from alternative-method publishers, despite its genuine coordination benefit for struggling readers. Suppression is moderate-high (0.52) because the constraint's persistence depends on actively marginalizing whole language as 'empirically refuted'—a suppression of alternative pedagogy, not of external barrier. Theater is low and declining (0.28) because the bulk of activity in 2026 is genuine phonics instruction with demonstrated outcomes, not performative maintenance; the constraint has moved from nascent claim (high theater in 1970 when evidence was still sparse) to institutionalized practice with empirical ground. Accessibility collapse is moderate-high (0.71): once the phonics reading is understood as 'the scientific approach,' alternatives are framed as irrational; however, whole language and balanced literacy remain coherent positions held by real practitioners, so complete collapse is not achieved. Resistance is high (0.64): whole language advocates, progressive educators, and teachers who experienced success with alternative methods mount real resistance through practitioner networks, parent advocacy, and counter-research; the constraint must be actively defended through policy enforcement, not passively sustained. The measurement series tracks the historical shift from scattered phonics practice (1970s) through cognitive science accumulation (1990s–2000s) to policy institutionalization (2010s–present). The extractiveness climb reflects rising policy mandate; the theater ratio decline reflects authentic phonics instruction becoming the functional core rather than a contested claim.
 *
 * PERSPECTIVAL GAP:
 *   From the struggling reader's seat, phonics is a genuine coordination benefit—it solves a cognitive load problem. From the researcher's and publisher's seats, phonics is coordination + captured market value. From the teacher's seat, phonics is a constraining mandate that erodes discretion. From the whole language advocate's seat, phonics is extraction of pedagogical authority and suppression of meaning-driven literacy development. The engine should compute these divergences from the stakeholder power + exit + role data: struggling readers are beneficiaries with trappping exit; teachers are payers with identity-locked exit; researchers/publishers are beneficiaries with escape or arbitrage exit. The structural asymmetry is irreducible: the same instruction that benefits one seat (struggling reader) constrains another (teacher autonomy).
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers occupy d near 0.15 (full beneficiary: trapped exit, powerless position, direct benefit). Researchers/publishers occupy d near 0.25 (beneficiary with institutional power and escape optionality). Whole language advocates occupy d near 0.75 (payer: constrained exit, moderate power, methodological authority loss). Teachers occupy d near 0.80 (payer: identity-locked exit, moderate power, discretion constraint hits identity core). Policy makers are analytical (d undefined). The constraint extracts from teachers and alternative advocates while genuinely benefiting struggling readers—the tangled rope signature. Suppression targets alternative pedagogy; it is structural (policy mandate) and internalized (teacher identity fusion with traditional autonomy).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (lack of structure in reading instruction, high failure rates for implicit-method non-responders) is live, documented, and addressed by the phonics solution. No mandatrophy signal emerges from founding_problem_status=live + disappearance_verdict=world_rearranges. However, omega variables flag the tension: empirical evidence supports phonics for decoding automaticity AND whole language for engagement and meaning-making—the readings partition the problem space rather than one proving the other false. The constraint avoids the false-summit trap (it is not claiming to be a natural law, and it does not emerge naturally; it requires active enforcement). Mandatrophy risk would emerge if the phonics regime persisted despite the founding problem becoming dead (e.g., if dyslexia were cured or reading motivation became non-problem while phonics remained mandated for symbolic reasons). Current status: founding problem is contested but plausibly live; extraction is substantial but justified by the coordination function; suppression is real but not at snare-level coerciveness. Classification: tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_integration_problem,
    'Can explicit phonics instruction and authentic literature engagement coexist optimally, or is there a true tradeoff in classroom time and cognitive load?',
    'Longitudinal randomized trials comparing phonics-only, literature-only, and integrated phonics-within-authentic-texts conditions, with measurement of decoding automaticity, reading comprehension, reading motivation, and sustained engagement over 5+ years.',
    'If integration is feasible without cost, the constraint''s extraction (crowding out authentic texts) becomes indefensible; the tangled_rope might degrade to snare or shift to balanced_literacy_reading. If tradeoffs are genuine, phonics-first remains justified for struggling readers and the extraction is coordination cost, not pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_integration_problem, empirical, 'Whether phonics and authentic literature engagement can coexist optimally or involve irreducible tradeoff.').

omega_variable(
    teacher_autonomy_suppression_mechanism,
    'Is the suppression of teacher discretion in reading methods structural (policy enforcement, accountability mandates) or internalized (professional identity fusion with whole-language training, belief in child-centered methods)?',
    'Post-policy-reversal trajectories: if a district removes phonics mandates and mandated curricula, do teachers revert to pre-policy methods or continue phonics adoption? Internalized suppression would persist; structural-only suppression would dissolve.',
    'If internalized: teacher autonomy recovery would require identity reconstruction, not just policy change; the suppression is more effective and harder to reverse. If structural: policy reversal could restore autonomy quickly. Internalized suppression would classify the constraint as more snare-like (identity capture); purely structural would keep it tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(teacher_autonomy_suppression_mechanism, empirical, 'Distinction between structural vs. internalized teacher suppression.').

omega_variable(
    whole_language_forecast_divergence,
    'Whole language and balanced literacy advocates dispute whether the ''failure'' of implicit reading development in struggling readers reflects individual cognitive differences or insufficiently rich exposure and engagement. Which reading of the data is more defensible?',
    'Comparative analysis of reading outcomes in high-literacy-exposure, low-phonics-instruction environments (e.g., bilingual immersion, language-rich homes without explicit phonics) vs. phonics-first instruction. If struggling readers thrive under high-exposure, low-phonics, the ''cognitive difference'' reading weakens; if they struggle, the phonics reading gains ground.',
    'This omega determines whether phonics is universally necessary or an intervention for specific populations with specific input constraints. A narrow necessity (only for low-exposure populations) would reduce the extraction from whole language and support balanced_literacy_reading coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whole_language_forecast_divergence, empirical, 'Whether implicit decoding failure reflects cognitive individual differences or insufficient literacy exposure.').

omega_variable(
    reading_science_consensus_stability,
    'The phonics reading grounds its legitimacy in ''reading science consensus'' (cognitive mechanisms of decoding, phonemic awareness as prerequisite, dual-route models). How stable is this consensus, and what would falsify it?',
    'Systematic review of dissent within cognitive science (e.g., connectionist models that model implicit decoding without explicit phonemic awareness; longitudinal studies of meaning-first reading success; neuroscience of reading acquisition showing alternative routes to automaticity).',
    'If consensus is fragile or grounded in cherry-picked evidence, the constraint''s authority legitimacy weakens; extraction becomes harder to defend. If consensus is robust, the phonics reading''s institutional entrenchment is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_science_consensus_stability, empirical, 'Stability and vulnerability of reading science consensus grounding phonics necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 1970, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1970, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1970, 0.42).
narrative_ontology:measurement(read_tr_t1990, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1990, 0.38).
narrative_ontology:measurement(read_tr_t2000, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(read_tr_t2010, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(read_tr_t2018, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2018, 0.29).
narrative_ontology:measurement(read_tr_t2026, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2026, 0.28).

% Extraction over time
narrative_ontology:measurement(read_be_t1970, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1970, 0.18).
narrative_ontology:measurement(read_be_t1990, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(read_be_t2000, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(read_be_t2010, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(read_be_t2018, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2018, 0.64).
narrative_ontology:measurement(read_be_t2026, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1970, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(read_su_t1990, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1990, 0.32).
narrative_ontology:measurement(read_su_t2000, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2000, 0.41).
narrative_ontology:measurement(read_su_t2010, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement(read_su_t2018, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2018, 0.51).
narrative_ontology:measurement(read_su_t2026, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2026, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__phonics_reading, 0.14).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__balanced_literacy_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, teacher_autonomy_in_curriculum_selection).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, curriculum_publishing_market_structure).

% DUAL FORMULATION NOTE:
% The reading_acquisition_mechanism kernel has three constraint readings: phonics_reading (this story), whole_language_reading, and balanced_literacy_reading. Each reading instantiates a different ε-invariant constraint with different beneficiary/victim structures and different enforcement mechanisms. The phonics reading is claimed tangled_rope (genuine coordination for struggling readers + extraction from teacher autonomy). The whole_language_reading would claim rope or snare depending on its reading of meaning-driven acquisition necessity. The balanced_literacy_reading would claim rope (coordination of decoding and engagement). All three compete for legitimacy within the same kernel (reading acquisition mechanisms); they are not independent constraints but siblings within a commitment-system contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__phonics_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
