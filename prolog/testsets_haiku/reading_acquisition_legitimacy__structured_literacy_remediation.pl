% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__structured_literacy_remediation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reading_acquisition_legitimacy__structured_literacy_remediation, []).

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
 *   constraint_id: reading_acquisition_legitimacy__structured_literacy_remediation
 *   human_readable: Structured Literacy Remediation as Legitimate Reading Instruction Design
 *   domain: education_policy/cognitive_science
 *
 * SUMMARY:
 *   The constraint names one reading of how reading instruction legitimacy
 *   should be defined. This reading — structured literacy remediation — holds
 *   that reading instruction must be explicitly designed for the most
 *   vulnerable learners first, using cumulative, diagnostic, multisensory
 *   approaches grounded in alphabetic principle and phonological processing
 *   science. The constraint makes this reading the standard against which
 *   other approaches are measured and deemed adequate or inadequate. The
 *   reading instantiates a tangled rope: a genuine coordination function
 *   (preventing early literacy failure through evidence-grounded systematic
 *   instruction) exists alongside asymmetric extraction (professional
 *   authority transfers to structured-literacy specialists, whole-language
 *   practitioners face delegitimization, assessment infrastructure expands
 *   and captures institutional budget). The claim and metrics are authored
 *   independently: this reading is CLAIMED as tangled rope (it coordinates
 *   literacy prevention AND extracts professional rents), and the authored
 *   extractiveness and suppression metrics describe the actual operation
 *   observed as the constraint matured (rising extractiveness early,
 *   plateauing as adoption stabilized; persistent suppression of alternative
 *   pedagogies).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, 0.58).
domain_priors:suppression_score(reading_acquisition_legitimacy__structured_literacy_remediation, 0.72).
domain_priors:theater_ratio(reading_acquisition_legitimacy__structured_literacy_remediation, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__structured_literacy_remediation, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__structured_literacy_remediation, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__structured_literacy_remediation, "Structured Literacy Remediation as Legitimate Reading Instruction Design").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__structured_literacy_remediation, "education_policy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__structured_literacy_remediation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__structured_literacy_remediation, '312fbdba-845f-43ac-81e6-3c0375491255').
narrative_ontology:cs_kernel_codification('312fbdba-845f-43ac-81e6-3c0375491255', distributed).
narrative_ontology:cs_authority_grounding('312fbdba-845f-43ac-81e6-3c0375491255', extraction).
narrative_ontology:cs_interpretation_layer_present('312fbdba-845f-43ac-81e6-3c0375491255').
narrative_ontology:cs_reading_relation('312fbdba-845f-43ac-81e6-3c0375491255', reading_acquisition_legitimacy__phonics_decoding_primacy, forecloses).
narrative_ontology:cs_reading_relation('312fbdba-845f-43ac-81e6-3c0375491255', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('312fbdba-845f-43ac-81e6-3c0375491255', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_axiom('312fbdba-845f-43ac-81e6-3c0375491255', foundational, vulnerable_learners_require_explicit_systematic_instruction).
narrative_ontology:cs_axiom_status(vulnerable_learners_require_explicit_systematic_instruction, holdable).
narrative_ontology:cs_axiom_grounding('312fbdba-845f-43ac-81e6-3c0375491255', vulnerable_learners_require_explicit_systematic_instruction, empirically_contingent).
narrative_ontology:cs_axiom('312fbdba-845f-43ac-81e6-3c0375491255', foundational, cumulative_alphabetic_principle_foundational_to_decoding).
narrative_ontology:cs_axiom_status(cumulative_alphabetic_principle_foundational_to_decoding, holdable).
narrative_ontology:cs_axiom_grounding('312fbdba-845f-43ac-81e6-3c0375491255', cumulative_alphabetic_principle_foundational_to_decoding, empirically_contingent).
narrative_ontology:cs_axiom('312fbdba-845f-43ac-81e6-3c0375491255', secondary, continuous_diagnostic_assessment_prevents_reading_failure).
narrative_ontology:cs_axiom_status(continuous_diagnostic_assessment_prevents_reading_failure, holdable).
narrative_ontology:cs_axiom_grounding('312fbdba-845f-43ac-81e6-3c0375491255', continuous_diagnostic_assessment_prevents_reading_failure, empirically_contingent).
narrative_ontology:cs_reference_frame('312fbdba-845f-43ac-81e6-3c0375491255', evidence_based_reading_science_prioritizing_vulnerable_learners).
narrative_ontology:cs_drift_state('312fbdba-845f-43ac-81e6-3c0375491255', contemporary_policy_adoption_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('312fbdba-845f-43ac-81e6-3c0375491255', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_practitioners).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, diagnostic_assessment_infrastructure).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, intervention_specialists).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_programs).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, teachers_trained_in_alternatives).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, schools_invested_in_non_diagnostic_models).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, vulnerable_early_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__structured_literacy_remediation, elementary_classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_practitioners).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_advocates).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_invested_in_alternatives).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__structured_literacy_remediation, elementary_classroom_teachers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Literacy specialists and remedial-reading instructors trained in structured literacy frameworks (Orton-Gillingham, Fountas & Pinnell, Science of Reading alignment). They argue that explicit, cumulative, multisensory instruction with continuous diagnostic assessment is the empirically grounded standard. Their professional authority and certification depend on this framework remaining the legitimate standard; adoption of the constraint expands their scope and validates their expertise.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_practitioners, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_practitioners, agenda_setter).

% Children with dyslexia, language processing deficits, or late-starting literacy exposure. Structured literacy's explicit, diagnostic approach catches their gaps early and provides targeted intervention before failure accumulates. Their exit option is the school environment they are assigned to; they cannot choose an instructional model.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, vulnerable_early_readers, beneficiary,
    powerless, biographical, trapped, local).

% Reading specialists and teachers trained in whole-language, meaning-centered, literature-immersion models. They argue that decoding emerges naturally from authentic reading exposure and that explicit phonics is stilted and demotivating. Adoption of the structured literacy standard delegitimizes their professional expertise, threatens their job security in districts adopting the constraint, and requires retraining or professional displacement.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_practitioners, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, whole_language_practitioners, excluded).

% Teachers and instructional leaders committed to balancing explicit phonics with literature exposure, arguing that reading requires both decoding and meaning-making. They see the structured literacy constraint as overweighting decoding mechanics and undervaluing comprehension and engagement. Adoption forces them to either conform or defend their approach in hostile policy environments.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, balanced_literacy_advocates, payer,
    organized, biographical, constrained, national).

% Standardized testing vendors, assessment platforms, and data-management systems that sell diagnostic reading batteries, progress-monitoring tools, and outcomes tracking. Structured literacy's continuous diagnostic mandate creates an institutional demand for their products and services; the constraint entrenches their role in the instructional system.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, diagnostic_assessment_infrastructure, beneficiary,
    institutional, generational, mobile, national).

% Districts that have invested in whole-language or balanced-literacy curricula, training, and materials. Adoption of the structured literacy constraint requires capital-intensive curriculum replacement, teacher retraining, and acknowledgment that prior investments were misdirected. The constraint imposes substantial switching costs and institutional humbling.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, school_districts_invested_in_alternatives, payer,
    institutional, biographical, constrained, regional).

% Teachers in grades K–3 who implement reading instruction daily. Those aligned with structured literacy gain clarity and professional authority; those trained in other approaches face retraining requirements, performance pressure from new standards, and potential judgment that prior practice was inadequate. Their compliance is required by administrative mandate and curriculum adoption.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, elementary_classroom_teachers, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_legitimacy__structured_literacy_remediation, elementary_classroom_teachers, beneficiary).

% Cognitive neuroscientists, educational researchers, and evidence synthesis bodies (National Reading Panel, National Academies reports) that study reading acquisition and effectiveness of instructional approaches. They examine which models best predict reading success, particularly for struggling learners. Their findings anchor the empirical claims the constraint rides on.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, reading_science_researchers, observer,
    institutional, generational, analytical, global).

% State education departments, federal reading policy offices, and legislative bodies that adopt reading standards, mandate curriculum, and fund instructional interventions. They declare which pedagogical approaches count as legitimate, which teacher certifications are valid, and which instruction models receive funding priority. Their enforcement power makes the constraint stick.
narrative_ontology:constraint_stakeholder(reading_acquisition_legitimacy__structured_literacy_remediation, policy_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_legitimacy__structured_literacy_remediation, structured_literacy_practitioners).
narrative_ontology:fixing_cost_class(reading_acquisition_legitimacy__structured_literacy_remediation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the early literacy crisis by establishing a shared, evidence-grounded framework that prevents reading failure before it becomes entrenched: explicit phonemic awareness, systematic alphabetic principle instruction, cumulative skill progression, and continuous diagnostic data allow all students (especially vulnerable learners) to decode reliably and build reading stamina before meaning-making demands accelerate.
% TRANSFER_FUNCTION: Transfers instructional legitimacy and professional authority from whole-language and balanced-literacy practitioners to structured-literacy specialists; transfers operational control over reading curricula from individual teacher discretion to diagnostic-protocol-driven systems; transfers budget allocation from literature-purchase programs to assessment infrastructure and specialist-teacher positions.
% ABSENT_VOICES: Bilingual and multilingual learners whose reading development spans two or more language systems; teachers in under-resourced schools who lack capacity for continuous diagnostic assessment infrastructure; families whose home literacy practices do not align with phonics-first sequencing; children whose reading struggles stem from oral-language processing or attention disorders rather than decoding deficits (the constraint may misdiagnose and over-treat). These voices would argue the constraint assumes monolingual, neurotypical, well-resourced learner populations and risks iatrogenic harm to populations its core theory misidentifies.
% DISAPPEARANCE_RATIONALE: If the structured literacy constraint disappeared overnight, reading instruction would revert to competing frameworks: whole-language immersion, balanced literacy, or teacher-discretion models. Reading outcomes for struggling learners would likely decline in the short term (evidence-base suggests structured literacy prevents some reading failure); early intervention capacity would atrophy; professional status and hiring in remedial reading would shift; assessment vendors would lose a major market; teacher certification standards would ease; alternative pedagogies would regain institutional legitimacy. The constraint's removal would reorganize instructional legitimacy, resource flows, professional hierarchies, and diagnostic infrastructure throughout K–3 literacy systems.
% FOUNDING_PROBLEM: Early reading failure, particularly among children with language-processing vulnerabilities, dyslexia, and disadvantaged backgrounds: traditional whole-language and balanced-literacy approaches failed to catch these students early, allowed them to fall behind, and rarely caught them up with intensive, explicit intervention. The founding problem is the cascade of reading failure that becomes self-reinforcing by grade 3 and leaves 35–40% of children reading below grade level by high school, with disproportionate impact on children from low-income and multilingual households.
% FOUNDING_PROBLEM_CORROBORATION: National Assessment of Educational Progress (NAEP) data documenting persistent reading achievement gaps; longitudinal studies of reading failure trajectories (National Reading Panel, Kilpatrick); cognitive neuroscience research on dyslexia and alphabetic processing (Shaywitz, Dehaene); analyses of whole-language instructional outcomes in low-income districts. Corroboration comes from independent researchers and federal measurement systems outside the benefiting literacy-specialist community, though the benefiting community also attests the problem. However, the sibling readings dispute whether structured literacy is the answer: whole-language advocates argue whole-language prevents reading failure through engagement; balanced literacy advocates argue both decoding and meaning prevent failure.
narrative_ontology:disappearance_verdict(reading_acquisition_legitimacy__structured_literacy_remediation, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_legitimacy__structured_literacy_remediation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__structured_literacy_remediation, 'none', 1).
narrative_ontology:epsilon_provenance(reading_acquisition_legitimacy__structured_literacy_remediation, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__structured_literacy_remediation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__structured_literacy_remediation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.38 (early constraint adoption) to 0.58 (mature state) because the benefits of literacy prevention are real but diffuse, while the costs to displaced practitioners and schools are concentrated and visible. Suppression is sustained at 0.72 throughout the interval: the constraint's persistence requires active enforcement against competing pedagogies — whole-language and balanced-literacy models do not voluntarily cede ground; state-level policy pressure (curriculum mandates, assessment requirements, teacher-certification changes, funding prioritization) is the mechanism maintaining the constraint. Theater ratio rises initially (0.18→0.26) as compliance expands and diagnostic-assessment rituals become performative (schools adopt the language and surface structures without deep implementation), then plateaus (0.26→0.31) as adoption becomes routine and theater becomes a stable minority of activity. Accessibility collapse is 0.68: alternative pedagogies remain intellectually available (books, articles, professional organizations defending whole-language and balanced literacy exist), but institutional pathways to practicing them have narrowed substantially. Resistance is 0.54: moderate but real; whole-language and balanced-literacy communities continue arguing for their approaches in professional journals, state policy debates, and individual schools, even as the constraint expands.
 *
 * PERSPECTIVAL GAP:
 *   From the structured-literacy agenda-setter seat: this is evidence-grounded coordination that corrects a failed system and rescues struggling readers from preventable failure. From the whole-language practitioner seat: this is professional displacement disguised as science, silencing pedagogies that honor student agency and meaning-making, and imposing a mechanistic decoding-first model that stifles comprehension and engagement. From the policy-authority seat: this is legitimate standard-setting for a public good (literacy). From the district administrator seat investing in alternatives: this is capital-destroying policy churn. From the parent of a child with dyslexia: this is rescue via explicit diagnosis and targeted help. From the parent of a child thriving in a whole-language program: this is unnecessary medicalization and overtreatment. Each seat experiences structurally different extraction/coordination ratios because their institutional position, exit options, and stake in the constraint's persistence diverge.
 *
 * DIRECTIONALITY LOGIC:
 *   Structured-literacy practitioners (agenda-setter + beneficiary) sit near the d=0.0 end: their professional authority, scope, and institutional standing rise as the constraint expands; they set the standards and enforce them. Vulnerable learners (beneficiary but powerless and trapped) sit near d=0.5 with asymmetry: they benefit from the explicit, preventative approach, but they cannot exit the constraint or choose alternatives — their benefit is real but non-negotiable. Whole-language and balanced-literacy practitioners (payers, constrained) sit near d=1.0: their professional credentials are questioned, their curriculum choices are overridden, their job security in adoption districts is threatened. Assessment infrastructure vendors (beneficiary, institutional, mobile exit) sit near d=0.3: they benefit substantially from the diagnostic mandate but retain market arbitrage — they can supply to any instructional model; their exit option is real. School districts invested in alternatives (payer, institutional, constrained) sit near d=0.9: high switching costs, institutional humbling, budget displacement. Classroom teachers (mixed payer/beneficiary) split: those aligned with structured literacy gain professional clarity and authority (d≈0.2), those retraining under mandate face pressure and compliance costs (d≈0.7). The constraint's persistence depends on policy enforcement that keeps alternative pathways sufficiently closed for payers while maintaining genuine benefit for vulnerable learners and practitioners.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not present. The founding problem (early reading failure, especially for vulnerable learners) remains LIVE and the constraint addresses it with a coordination function that measurably improves outcomes for the target population (struggling readers who need explicit, systematic intervention). The constraint's persistence is not inertial — policy systems actively enforce it, schools continuously adopt it, and the underlying empirical claim (structured literacy prevents reading failure) remains valid and contested (not dead; competing readings remain live). However, the measurement trajectory shows warning signs: theater ratio is rising and suppression is elevated and stable, indicating that compliance pressure (institutional enforcement) and performative adoption are growing. If theater continues rising toward 0.50+ while suppression remains high, the constraint risks drifting toward piton status — maintained by enforcement momentum rather than by solved coordination problem. The early-stage observation is tangled rope with healthy coordination and extractive rent-seeking; long-term monitoring should track whether the coordination function (preventing early reading failure) continues delivering measurable outcomes or becomes decoupled from the enforcement infrastructure (diagnostic rituals continue, but reading outcomes plateau despite intensifying compliance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_coexistence,
    'Are the four readings of the reading_acquisition_legitimacy kernel genuinely coexisting competing readings, or does one (structured literacy) logically foreclose the others?',
    'Examine whether holding structured literacy''s core premise (reading requires explicit alphabetic-principle instruction for vulnerable learners; decoding is foundational) logically entails rejecting whole-language''s core premise (decoding emerges naturally from immersion in authentic literature) or whether both premises can coexist as domain-specific applications (e.g., structured literacy for struggling readers; whole-language for advanced/engaged readers). If the premises do not directly contradict, the readings coexist; if one entails the falsity of the other, foreclosure applies.',
    'If coexistence is the true relationship, the constraint is one live reading among several, and misclassifying it as foreclosing would overstate its legitimacy claim. If foreclosure is the true relationship, the constraint''s persistence depends on suppressing a logically incoherent alternative, not merely a pedagogically different one — a stronger closure claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether structured literacy and whole-language readings are logically incompatible or merely pedagogically competing.').

omega_variable(
    empirical_closure_vs_policy_closure,
    'Is the suppression of alternative pedagogies driven by empirical evidence that structured literacy outperforms alternatives for most populations, or by policy enforcement that privileges structured literacy independent of comparative effectiveness evidence?',
    'Meta-analysis comparing reading outcomes under structured-literacy-designed instruction, whole-language instruction, and balanced literacy in comparable student populations (controlling for implementation quality, teacher training, student demographics). If structured literacy shows measurable superiority across populations, the suppression is empirically grounded; if outcomes are comparable or population-specific, the suppression is policy-driven enforcement.',
    'Empirically grounded suppression of inferior approaches is legitimate constraint-setting; policy-driven suppression of comparable alternatives is extractive constraint that happens to benefit practitioners whose pedagogy was chosen by policy fiat rather than evidence. This distinction determines whether the constraint is tangled rope (coordination + extraction) or snare (extraction with coordination as cover).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_closure_vs_policy_closure, empirical, 'Whether the constraint''s enforcement reflects evidence-based superiority or policy momentum.').

omega_variable(
    identity_lock_in_teacher_retraining,
    'How much of the suppression of whole-language and balanced-literacy practitioners is structural (policy barriers, job loss, credential devaluation) versus internalized (teachers have fused their professional identity with a pedagogical approach and cannot psychologically exit the framework even after its institutional closure)?',
    'Post-adoption trajectory analysis: if whole-language-trained teachers successfully retrain and adopt structured literacy without reported identity crisis, internalization is low and suppression is structural. If retraining is reported as identity-threatening, role-destabilizing, or cognitively dissonant, internalization is significant; the practitioners carry suppression with them across retraining.',
    'Structural suppression (job loss, credential barriers, policy pressure) is reversible with policy change; internalized suppression (identity fusion with a delegitimized pedagogy) persists after policy reversal. If internalization is high, the constraint''s effective suppression is higher than the structural enforcement suggests, and the lasting cost to displaced practitioners extends beyond institutional switching costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_teacher_retraining, empirical, 'Structural vs. internalized identity lock-in for displaced whole-language practitioners.').

omega_variable(
    vulnerable_learner_overtreatment_risk,
    'For children whose reading struggles are not decoding-based (e.g., oral language processing deficits, attention disorders, limited exposure), does the structured-literacy constraint risk misdiagnosis and overtreatment as decoding-disordered, delaying or preventing appropriate alternative interventions?',
    'Differential diagnosis studies examining how many struggling readers identified as needing structured-literacy intervention actually have decoding deficits (true diagnosis) versus language processing, attention, or exposure problems (alternative diagnosis requiring different intervention). If misdiagnosis rates are non-trivial, the constraint''s application to all vulnerable learners extracts a hidden cost from subpopulations who would benefit from different approaches.',
    'If diagnostic accuracy is high, the constraint''s targeting to vulnerable learners is legitimate coordination. If misdiagnosis is common, the constraint''s claim to solve the founding problem is partially false for some populations — it solves early reading failure for some vulnerable learners while creating iatrogenic delay for others. This undermines the constraint''s empirical foundation and introduces asymmetric extraction (correct-diagnosis populations benefit; misdiagnosed populations bear costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_learner_overtreatment_risk, empirical, 'Risk of misdiagnosis and overtreatment under the structured-literacy framework for non-decoding reading struggles.').

omega_variable(
    assessment_infrastructure_capture,
    'Does the structured-literacy constraint''s mandate for continuous diagnostic assessment create institutional lock-in where assessment vendors become indispensable to compliance, and the infrastructure''s commercial interests shape the constraint''s evolution independent of literacy outcomes?',
    'Historical analysis of assessment-vendor influence on state reading standards, diagnostic-tool requirements, and funding allocations. If assessment vendors actively lobby for expanded diagnostic mandates, sell tools that create data-dependency, and capture policy conversations, assessment infrastructure is a captured institution feeding the constraint. If the constraint''s evolution reflects literacy-outcome improvement, assessment is a legitimate tool.',
    'If captured, the constraint extracts from districts and schools in the form of assessment-software licensing, forcing resource allocation away from direct instruction toward infrastructure. The beneficiary structure shifts: assessment vendors become primary beneficiaries; vulnerable learners'' actual reading benefit depends on whether assessment mandates correlate with instructional improvement (contestable). This would move the constraint toward snare territory (extraction with coordination as cover).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(assessment_infrastructure_capture, empirical, 'Degree of assessment-infrastructure capture in shaping the structured-literacy constraint''s institutional evolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__structured_literacy_remediation, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 0, 0.18).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 5, 0.2).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 10, 0.23).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 15, 0.26).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 20, 0.28).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 25, 0.3).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 30, 0.31).
narrative_ontology:measurement(read_tr_t35, reading_acquisition_legitimacy__structured_literacy_remediation, theater_ratio, 35, 0.31).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(read_be_t30, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(read_be_t35, reading_acquisition_legitimacy__structured_literacy_remediation, base_extractiveness, 35, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 5, 0.67).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(read_su_t30, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(read_su_t35, reading_acquisition_legitimacy__structured_literacy_remediation, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__structured_literacy_remediation, identity_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__structured_literacy_remediation, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__phonics_decoding_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, teacher_certification_reading_standards).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__structured_literacy_remediation, state_reading_curricula_mandates).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the reading_acquisition_legitimacy kernel. The kernel comprises four structurally distinct readings that instantiate different constraints with different ε values, beneficiary/victim structures, and enforcement mechanisms: (1) structured_literacy_remediation (THIS story) — highest structure, explicit cumulative instruction, continuous diagnosis; (2) phonics_decoding_primacy — decoding focus, systematic phonics, moderate explicit instruction; (3) whole_language_meaning_primacy — immersion in authentic literature, decoding emerges naturally, minimal explicit phonics; (4) balanced_literacy_integration — both decoding and meaning-making, balanced approaches, teacher discretion. Each reading emerges from different assumptions about what reading IS, what constitutes legitimate instruction, and which learners are primary. Structured literacy places vulnerable learners first and makes preventative intervention-grade instruction the baseline; phonics emphasizes systematic decoding for all; whole language emphasizes engagement and meaning for all; balanced literacy refuses to prioritize between decoding and meaning. The four constraints form a family linked by network.affects_constraints in all directions — each reading's institutional success constrains or amplifies the others. Structured literacy's rise (2010s forward) correlates with decline of whole-language and balanced-literacy policy adoption and professional legitimacy, not because empirical evidence became conclusive (it remained contested), but because policy authorities adopted the reading and state adoption cascaded. This is an institutional dominance story, not an epistemic settlement story — the kernel remains contested even as one reading's policy instantiation dominates schooling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_legitimacy__structured_literacy_remediation, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
