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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reading_acquisition_mechanism__phonics_reading
 *   human_readable: Phonics-First Reading Acquisition Pedagogy
 *   domain: educational/cognitive/institutional
 *
 * SUMMARY:
 *   The phonics-first reading of reading-acquisition mechanisms asserts that
 *   explicit, systematic instruction in grapheme-phoneme correspondence is a
 *   cognitive prerequisite for reading skill and should be the foundational
 *   instructional component for all readers, especially those with decoding
 *   deficits. This reading emerged from cognitive science research on
 *   phonemic awareness and fluency mechanisms, gained institutional power
 *   through policy mandates (No Child Left Behind, state-level Reading
 *   Foundational Acts), and now shapes curricula, teacher training, and
 *   assessment in majority of US schools. It is contested by whole-language
 *   advocates (who assert decoding emerges implicitly from authentic-text
 *   engagement) and balanced-literacy researchers (who assert that both
 *   explicit phonics and literature engagement are necessary in integration).
 *   The constraint's extractive character (0.68) reflects that the reading
 *   has acquired institutional authority through appeals to cognitive science
 *   while suppressing competing pedagogies that may be equally efficacious
 *   for other reader populations. The measurement series documents the rise
 *   of phonics-first from 1985 (minority alternative) through 2025 (dominant
 *   institutional standard), with steady increases in extractiveness,
 *   suppression requirement, and theater ratio—indicating that as the
 *   reading's institutional power consolidated, the justificatory load
 *   shifted from coordinating actual reading-acquisition mechanisms toward
 *   defending the allocation of instructional authority.
 *
 * KEY AGENTS:
 *   - phonics_researchers_cognitive_scientists: Institutional authority-setters; control the evidentiary standard and policy influence (power=institutional, exit=analytical)
 *   - struggling_readers: Structural beneficiaries; gain from explicit phoneme-awareness instruction targeted to their documented deficit (power=powerless, exit=trapped, identity_locked by dyslexia screening)
 *   - economically_disadvantaged_populations: Structural beneficiaries; phonics-systematic structure reduces dependence on home-literacy resources; least dependent on pedagogical diversity (power=powerless, exit=trapped)
 *   - classroom_teachers: Payer; bear the compliance and autonomy costs of systematic curricula and fidelity monitoring; often identity-locked into whole-language training and philosophy (power=moderate, exit=constrained-to-identity_locked)
 *   - whole_language_practitioners: Payer and excluded; professional identity tied to constructivist pedagogy, face institutional devaluation; organized but systematically disadvantaged in policy settings (power=organized, exit=identity_locked, time_horizon=generational)
 *   - balanced_literacy_advocates: Payer and excluded; operate between the two poles with empirical support but weaker policy purchase than phonics-first (power=organized, exit=constrained, time_horizon=generational)
 *   - policy_makers: Agenda-setter and beneficiary; gain political credit for reading-outcome improvements in struggling-reader populations, centralize instructional authority, reduce accountability variance through standardized curricula (power=institutional, exit=analytical)
 *   - educational_publishers: Beneficiary; capture market share in phonics-aligned curricula, assessment, and professional development (power=powerful, exit=mobile)
 *   - reading_fluency_deficit_students: Beneficiary but also trapped; gain measurable progress in their documented deficit; identity permanently marked by early screening and special instruction (power=powerless, exit=identity_locked, time_horizon=biographical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_mechanism__phonics_reading, 0.68).
domain_priors:suppression_score(reading_acquisition_mechanism__phonics_reading, 0.72).
domain_priors:theater_ratio(reading_acquisition_mechanism__phonics_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(reading_acquisition_mechanism__phonics_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_mechanism__phonics_reading, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_mechanism__phonics_reading, "Phonics-First Reading Acquisition Pedagogy").
narrative_ontology:topic_domain(reading_acquisition_mechanism__phonics_reading, "educational/cognitive/institutional").

domain_priors:requires_active_enforcement(reading_acquisition_mechanism__phonics_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_mechanism__phonics_reading, '9511af2e-499c-45bf-aadb-130ea1c949bb').
narrative_ontology:cs_kernel_codification('9511af2e-499c-45bf-aadb-130ea1c949bb', distributed).
narrative_ontology:cs_authority_grounding('9511af2e-499c-45bf-aadb-130ea1c949bb', expertise).
narrative_ontology:cs_interpretation_layer_present('9511af2e-499c-45bf-aadb-130ea1c949bb').
narrative_ontology:cs_reading_relation('9511af2e-499c-45bf-aadb-130ea1c949bb', reading_acquisition_mechanism__whole_language_reading, forecloses).
narrative_ontology:cs_reading_relation('9511af2e-499c-45bf-aadb-130ea1c949bb', reading_acquisition_mechanism__balanced_literacy_reading, influences).
narrative_ontology:cs_axiom('9511af2e-499c-45bf-aadb-130ea1c949bb', foundational, grapheme_phoneme_automaticity_cognitive_prerequisite).
narrative_ontology:cs_axiom_status(grapheme_phoneme_automaticity_cognitive_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('9511af2e-499c-45bf-aadb-130ea1c949bb', grapheme_phoneme_automaticity_cognitive_prerequisite, empirically_contingent).
narrative_ontology:cs_axiom('9511af2e-499c-45bf-aadb-130ea1c949bb', foundational, explicit_systematic_instruction_efficacy_hypothesis).
narrative_ontology:cs_axiom_status(explicit_systematic_instruction_efficacy_hypothesis, holdable).
narrative_ontology:cs_axiom_grounding('9511af2e-499c-45bf-aadb-130ea1c949bb', explicit_systematic_instruction_efficacy_hypothesis, empirically_contingent).
narrative_ontology:cs_reference_frame('9511af2e-499c-45bf-aadb-130ea1c949bb', phonemic_awareness_prerequisite_cognitive_model).
narrative_ontology:cs_drift_state('9511af2e-499c-45bf-aadb-130ea1c949bb', contemporary_balanced_literacy_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9511af2e-499c-45bf-aadb-130ea1c949bb', '').
narrative_ontology:cs_kernel_id(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, struggling_readers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, dyslexia_identified_students).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, economically_disadvantaged_populations).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, whole_language_practitioners).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, authentic_literature_advocates).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, teacher_autonomy_defenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, educational_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, policy_makers_state_departments).
narrative_ontology:constraint_beneficiary(reading_acquisition_mechanism__phonics_reading, reading_fluency_deficit_students).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, classroom_teachers).
narrative_ontology:constraint_victim(reading_acquisition_mechanism__phonics_reading, balanced_literacy_advocates).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, grapheme_phoneme_automaticity_cognitive_prerequisite).
narrative_ontology:constraint_vindicates(reading_acquisition_mechanism__phonics_reading, explicit_systematic_instruction_efficacy_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conduct systematic empirical research on reading acquisition mechanisms, publish findings showing phonemic awareness and grapheme-phoneme mapping as causal prerequisites for fluency. Testify to policy bodies, design intervention curricula, and set the evidentiary standard that other pedagogies must meet. Control the publication gates in peer-reviewed outlets where literacy science is legitimized.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, phonics_researchers_cognitive_scientists, agenda_setter,
    institutional, generational, analytical, global).

% Experience persistent decoding difficulty under whole-language exposure; systematic phonics instruction targets their specific deficit directly and produces measurable decoding gains. They have no choice in pedagogy—schools select the curriculum—but benefit substantially when phonics is the method used. Often identified through dyslexia screening that presupposes the phonics-need model.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, struggling_readers, beneficiary,
    powerless, biographical, trapped, local).

% Have fewer literacy-rich home environments and less access to supplemental tutoring; phonics-structured curricula reduce reliance on implicit exposure and family resource gaps, making reading attainment less dependent on socioeconomic status. Whole-language approaches amplify existing opportunity gaps because decoding-by-exposure presupposes exposure.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, economically_disadvantaged_populations, beneficiary,
    powerless, biographical, trapped, regional).

% Must implement systematic phonics scope-and-sequence curricula with fidelity, often scripted materials, regular progress monitoring, and data collection. Trade autonomy and responsiveness to individual student interests for structured, evidence-aligned instruction. Non-compliance triggers intervention and evaluation pressure; compliance requires ongoing professional development and curriculum adoption costs.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, classroom_teachers, payer,
    moderate, biographical, constrained, local).

% Are professional educators and researchers who developed whole-language pedagogy based on constructivist learning theory and the principle that meaning-making drives decoding. Face institutional pressure to abandon their approach, see their published research devalued in policy settings, lose grant funding and publishing venues as phonics-first frameworks dominate, and experience professional identity crisis as their core teaching philosophy is characterized as harmful to struggling readers.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, whole_language_practitioners, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, whole_language_practitioners, excluded).

% Developed integrated pedagogies combining explicit phonics with authentic literature experience, arguing that both decoding skill AND engagement with meaningful text are necessary for reading development. Face pressure from phonics-first policy to adopt more reductionist systematic sequences, see their empirical claims (that reading requires both elements) treated as methodologically weaker than phonics-specific trials, and experience their professional standing diminished despite their own strong empirical support base.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, balanced_literacy_advocates, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, balanced_literacy_advocates, excluded).

% Develop and sell phonics-aligned curricula (decodable readers, scope-and-sequence materials, progress-monitoring systems) that become mandated in districts adopting phonics-first frameworks. Capture significant market share and expand into assessment and professional development products. Competition for whole-language and balanced literacy materials shrinks.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, educational_publishers, beneficiary,
    powerful, biographical, mobile, national).

% Set mandates requiring systematic phonics instruction, often through legislation influenced by research reports and advocacy from cognitive scientists and parent groups of struggling readers. Reduce accountability burden by adopting an evidence-aligned curriculum framework; benefit politically from demonstrable reading-gains metrics in disadvantaged populations. Centralize instructional authority through approved materials lists and fidelity monitoring.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, policy_makers_state_departments, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(reading_acquisition_mechanism__phonics_reading, policy_makers_state_departments, beneficiary).

% Are identified early through screening that operationalizes phonemic awareness and decoding as the literacy-readiness gate; phonics-first curricula provide direct, measurable progress in their deficit area and prevent the accumulation of failure and identity damage that ensues when decoding is left implicit. Their identity as 'struggling readers' becomes structural to their school experience; phonics intervention becomes part of their institutionalized pathway.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, reading_fluency_deficit_students, beneficiary,
    powerless, biographical, identity_locked, local).

% Conduct meta-analyses, systematic reviews, and comparative trials attempting to adjudicate phonics-first versus integrated versus whole-language reading instruction. Operate under the constraint that phonics-first has captured the methodological and evidentiary standard; balancing evidence must meet stricter criteria; their role is to document the empirical record, not to set it.
narrative_ontology:constraint_stakeholder(reading_acquisition_mechanism__phonics_reading, observers_comparative_research_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reading_acquisition_mechanism__phonics_reading, educational_publishers).
narrative_ontology:fixing_cost_class(reading_acquisition_mechanism__phonics_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes reading instruction around a shared, empirically derived model of reading acquisition: phonemic awareness and grapheme-phoneme mapping as foundational capacities that, once automated, free cognitive resources for comprehension. Solves the coordination problem of what to teach first and in what sequence, making literacy instruction systematic and measurable rather than idiosyncratic.
% TRANSFER_FUNCTION: Transfers instructional authority and curriculum design from individual teachers (and heterogeneous whole-language traditions) to cognitive science-grounded curricula and materials; redistributes reading-acquisition gains toward struggling readers and economically disadvantaged populations (who benefit most from systematic structure) while concentrating pedagogical control in policy and publishing; transfers professional identity costs onto whole-language practitioners.
% ABSENT_VOICES: Whole-language practitioners and balanced-literacy advocates are structurally organized and visible but systematically disadvantaged in policy settings dominated by cognitive-science framing. Students who thrive under literature-rich, meaning-first instruction and have strong home literacy exposure are not organized as a constituency; their interests (intrinsic engagement with authentic texts) are present but subordinate to the organizing principle of systematicity.
% DISAPPEARANCE_RATIONALE: If the phonics-first mandate disappeared, school districts would return to pedagogically diverse reading instruction, whole-language and balanced-literacy curricula would resume market share, teachers would recover autonomy over scope-and-sequence decisions, and—empirically contested—reading outcomes would shift (phonics advocates predict decline in struggling-reader outcomes; balanced-literacy advocates predict stable or improved outcomes when literature engagement is restored). The institutional structure of reading instruction would reorganize around teacher and district choice rather than mandated systematicity.
% FOUNDING_PROBLEM: Reading instruction in the late 20th century lacked a shared empirical model of decoding mechanisms; whole-language pedagogy dominated despite growing evidence that implicit phonics exposure inadequately served struggling readers and dyslexic learners, leaving them without access to the explicit skill building their cognitive deficits required.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive science researchers (outside the pedagogy community) document systematic phonics efficacy in randomized trials with struggling-reader populations. Dyslexia researchers and parent advocacy groups attest the founding problem is ongoing—students still fail under whole-language approaches. Whole-language practitioners and balanced-literacy researchers dispute the framing: they attest the founding problem is misdiagnosed (reading difficulty stems from insufficient exposure and engagement, not phoneme-awareness deficits) and that phonics-first creates new problems (reduced literature engagement, decontextualized skill practice, narrowed curriculum). Balanced-literacy empirical literature provides independent corroboration that integrated approaches achieve comparable or superior outcomes.
narrative_ontology:disappearance_verdict(reading_acquisition_mechanism__phonics_reading, world_rearranges).
narrative_ontology:founding_problem_status(reading_acquisition_mechanism__phonics_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_mechanism__phonics_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_mechanism__phonics_reading, 'none', 1).

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
 *   Extractiveness rises from 0.32 (1985, pre-dominance, whole-language hegemonic) to 0.68 (2025, phonics-first mandates consolidated). The rise reflects institutionalization through policy leverage, not empirical discovery—the core cognitive science findings (phonemic awareness and grapheme-phoneme mapping matter) were stable by 1995, but extractiveness rose as mandates consolidated and alternative pedagogies were suppressed. Suppression requirement (0.72) is high because phonics-first persistence depends on active administrative enforcement (mandated materials, fidelity monitoring, teacher re-training, alternative pedagogies eliminated from curricula adoption). Without enforcement, whole-language and balanced-literacy alternatives would resurface—teachers trained in those traditions continue to view them as legitimate, and they have constituency (literature advocates, constructivist educators). Theater ratio (0.41) indicates growing performative character: as the reading became institutional standard, rhetoric shifted from 'phonics works best for struggling readers' to 'phonics is the scientifically-proven method all teachers must use.' The justification carries more heat relative to the actual empirical warrant; the measurement shows the constraint's function increasingly defended through institutional authority rather than demonstrated superiority in context-differentiated trials. Accessibility collapse (0.79) means alternatives (whole-language, balanced-literacy) have become nearly invisible in mainstream policy discourse; a teacher trained in phonics-first will find the alternatives labeled as 'discredited' or 'not research-based,' even though independent meta-analyses support balanced literacy. This is not Mountain-level collapse (natural law inevitability) but Tangled Rope collapse—alternatives are institutionally suppressed, not structurally impossible.
 *
 * PERSPECTIVAL GAP:
 *   Agenda-setter seats (researchers, policy-makers) and beneficiary seats (struggling readers) perceive the constraint as genuine coordination: phonics-first solves a real problem (decoding deficits in struggling readers) and reflects accurate science. Payer seats (teachers, whole-language advocates) perceive it as forced institutional authority: the constraint narrows their pedagogical choices, devalues their professional training, and is maintained through enforcement despite empirical evidence supporting integrated approaches. The divergence is structural: from the agenda-setter's position, the evidence warrant is strong and the constraint is efficient; from the teacher's position, the evidence warrant is selectively reported (phonics trials versus balanced-literacy trials are held to different evidentiary standards) and the constraint is maintained by institutional power, not scientific consensus. The engine computes these divergent type classifications from the structural data: the agenda-setter seat computes Rope or Mountain (coordination with minimal enforcement); the teacher seat computes Tangled Rope or Snare (suppression masquerading as coordination). The claim/metric divergence is intentional: this story claims Tangled Rope (both coordination function for struggling readers AND asymmetric enforcement costing teachers), and the metrics support that—extraction is high, suppression is higher, enforcement is active.
 *
 * DIRECTIONALITY LOGIC:
 *   Struggling readers and economically disadvantaged populations are full beneficiaries: they gain measurable decoding improvement from structured phonics; their directionality (d) is near 0.0–0.25 (beneficiary end). Teachers face mixed position: they implement higher-quality instruction for struggling readers (modest benefit) but lose autonomy and bear compliance costs (asymmetric burden)—they sit near d = 0.55–0.65. Whole-language practitioners and balanced-literacy advocates are payers: they lose professional standing, publishing venues, grant funding, and institutional legitimacy; d approaches 0.75–0.85 (target end). Policy-makers and publishers are positioned as beneficiaries—they gain centralized control and market capture—d near 0.1–0.3. The constraint's asymmetry: beneficial for the powerless (struggling readers trapped in reading-deficit identity), costly for the organized middle-power professionals (teachers, pedagogy advocates). This is classic Tangled Rope: real coordination function (struggling readers genuinely benefit) + asymmetric extraction (teachers pay via autonomy loss and alternative-pedagogy suppression). No directionality overrides are required; the derivation from beneficiary/victim + power + exit produces accurate d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—reading instruction lacking systematic empirical grounding and failing struggling readers—is live but contested. Cognitive scientists attest it persists; whole-language practitioners attest it is misdiagnosed (the problem is insufficient literature exposure, not implicit phonics inadequacy). This is not mandatrophy-as-function-atrophy because the constraint's function (improving struggling-reader fluency) is still operative. However, there is incipient mandatrophy in a second dimension: teacher autonomy suppression was justified as necessary for ensuring fidelity to evidence-based methods, but as phonics-first became institutional standard, the rationale shifted to 'phonics is the proven method' without continuing the empirical-comparison justification. The constraint risks becoming Piton if: (1) empirical comparisons show balanced-literacy produces equivalent or superior long-term outcomes (function atrophies, but suppression persists), or (2) policy becomes ritual compliance without evidence review. Currently, the constraint remains functional Tangled Rope because genuine benefits accrue to struggling readers and the empirical justification is actively maintained—but the measurement series (theater_ratio rising from 0.15 to 0.41) suggests creeping theatricality. The omega documenting the whole-language vs. balanced-literacy contest is the mandatrophy watch: if either sibling reading's empirical support grows to parity, the phonics-first reading's exclusive institutional authority will have become performance without justification, and the constraint will compute as Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonemic_awareness_causal_primacy,
    'Is phonemic awareness and grapheme-phoneme mapping a causal prerequisite for reading acquisition, or a correlate that is necessary only for certain reader populations under certain instructional histories?',
    'Longitudinal studies controlling for home literacy exposure, reading engagement, and instructional method; randomized trials with matched cohorts receiving different instructional sequencing but equivalent explicit-instruction intensity; analysis of reading-outcome variance attributable to phoneme-awareness prerequisites versus other factors.',
    'If causal-prerequisite claim is supported, phonics-first framing is justified for all readers; if correlate/population-specific claim is supported, instructional sequencing should be differentiated by reader profile, and the warrant for universal phonics-first mandate weakens. This shapes whether the reading is Mountain, Rope, or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonemic_awareness_causal_primacy, empirical, 'Whether phonemic awareness is a universal prerequisite or population/context-dependent.').

omega_variable(
    whole_language_foreclosure_relation,
    'The phonics-reading core axiom (grapheme_phoneme_automaticity_cognitive_prerequisite) logically forecloses the whole_language_reading axiom (implicit_decoding_emergence_from_engagement)—if phoneme mapping is a prerequisite, then implicit-emergence methods cannot provide it. Does the empirical record support the foreclosure, or do both readings describe subpopulations correctly (some readers require explicit phonics, others learn from implicit exposure)?',
    'Large-scale randomized trials isolating instructional method (explicit phonics vs. implicit engagement) while controlling for reader starting profiles, home literacy, and intensity; analysis of whether failure modes under each method are reader-population-specific (suggesting coexistence) or universal (supporting foreclosure). The ''Reading Wars'' primary empirical question.',
    'If both methods succeed for different reader populations, the relation shifts from ''forecloses'' to ''coexists_with''—both readings remain valid for different populations, and the constraint is not justified in suppressing whole-language for all readers. If only phonics succeeds universally, the foreclosure holds, and phonics-first mandate is justified. This is the kernel-level empirical crux.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(whole_language_foreclosure_relation, empirical, 'Whether phonics-first forecloses whole-language (exclusive mechanism) or coexists with it (population-differentiated).').

omega_variable(
    balanced_literacy_influence_strength,
    'The balanced_literacy_reading influences phonics_reading by asserting that integrated explicit-phonics + authentic-literature pedagogy produces superior or equivalent outcomes on multiple dimensions (fluency, comprehension, engagement, durability) compared to phonics-first-then-literature sequencing. If this influence-claim is empirically supported, does it weaken phonics-reading''s warrant for primacy, or does it merely show that sequencing can be flexible within phonics-first dominance?',
    'Meta-analysis of comparative trials (phonics-first vs. integrated balanced-literacy) measuring fluency, comprehension, engagement, long-term persistence, reader identity/self-efficacy, and learning-gain variance by reader profile. Does balanced literacy produce equivalent or superior outcomes on domains beyond decoding fluency?',
    'If balanced literacy produces superior outcomes on engagement, comprehension, and long-term persistence (with equivalent fluency gains), the phonics-reading''s claim that primacy is necessary is weakened—the constraint becomes more extractive (imposing pedagogical control without demonstrable benefit on all dimensions). The influence becomes transformative rather than marginal. If phonics-first outperforms on all dimensions, the influence-claim is empirically weaker and the suppression of balanced-literacy becomes harder to justify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balanced_literacy_influence_strength, empirical, 'Whether balanced-literacy influence over phonics-first is strong (superior outcomes) or marginal (equivalent outcomes).').

omega_variable(
    kernel_reading_mechanism_dispute,
    'This constraint is one reading of the contested kernel ''reading_acquisition_mechanism'': does reading skill fundamentally require explicit phoneme-phoneme instruction (phonics_reading), or can it emerge from meaning-centered authentic-text engagement (whole_language_reading), or does it require both in integration (balanced_literacy_reading)?',
    'The dispute is settled via large-scale randomized trials with long-term follow-up, comparative analysis of reader outcomes (fluency, comprehension, engagement, durability) across populations and grade levels under each approach, and analysis of failure modes (who fails under each method and why). The committer frame acknowledges this is one reading of the kernel; the engine computes the constraint''s type from the structural data independent of the reading contest.',
    'This reading''s classification depends on whether the phonics-first mandate reflects genuine coordination necessity (Rope/Mountain) or is leveraging cognitive science authority to extract pedagogical control (Tangled Rope/Snare). If the whole-language and balanced-literacy readings produce comparable or superior outcomes for most reader populations, this reading''s coordination justification weakens and its extractive character becomes dominant. If phonics-first produces superior outcomes for struggling readers while harming engagement-dependent readers, the constraint is genuinely Tangled Rope—real coordination for some, extraction for others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_mechanism_dispute, conceptual, 'This constraint''s reading of the contested kernel; sibling readings coexist in different institutional and professional frames.').

omega_variable(
    teacher_autonomy_suppression_mechanism,
    'Is the measured suppression (0.72) primarily structural (mandates and materials eliminate implementation options) or internalized (teachers have adopted the phonics-first mental model and no longer perceive alternatives as legitimate)?',
    'Post-mandate interviews with teachers in districts that shifted from whole-language to phonics-first and then decentralized mandates; analysis of whether teachers return to whole-language/balanced-literacy approaches when mandates are lifted, or retain phonics-first framing; study of teacher-reported perception of alternative pedagogies before and after mandate periods.',
    'If suppression is primarily structural, removing the mandate should restore teacher autonomy and alternative pedagogies; if primarily internalized, the mandate has durably shifted the profession''s cognitive models and removal has weak effect. This determines whether the constraint can be undone or whether it has produced lasting institutional sunk-cost lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(teacher_autonomy_suppression_mechanism, empirical, 'Structural versus internalized suppression of alternative pedagogies.').

omega_variable(
    dyslexia_identification_gate,
    'Does the phonics-first framing benefit struggling readers because decoding deficits are the root cause of reading difficulty, or does it benefit them because dyslexia screening operationalizes phonemic awareness as the diagnostic gate, structurally concentrating resources on learners identified by that gate?',
    'Comparative analysis of reading-outcome distributions (gain per student) under whole-language, balanced-literacy, and phonics-first across non-dyslexic struggling readers, dyslexia-identified readers, and strong readers. Analysis of whether resource concentration is proportional to phoneme-awareness deficit or to institutional identification via dyslexia screening.',
    'If the first mechanism dominates, phonics-first is serving the actual causal deficit; if the second, the benefit is partly an artifact of resource-allocation gatekeeping. This affects whether the constraint''s beneficiary structure is genuine (meeting actual needs) or partly constructed (defining needs to fit the solution).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dyslexia_identification_gate, empirical, 'Whether phonics-first benefits struggling readers via causal-deficit addressing or via diagnostic-gate concentration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_mechanism__phonics_reading, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t1985, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement_basis(read_tr_t1985, observed).
narrative_ontology:measurement(read_tr_t1995, reading_acquisition_mechanism__phonics_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement_basis(read_tr_t1995, observed).
narrative_ontology:measurement(read_tr_t2005, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement_basis(read_tr_t2005, observed).
narrative_ontology:measurement(read_tr_t2015, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2015, 0.39).
narrative_ontology:measurement_basis(read_tr_t2015, observed).
narrative_ontology:measurement(read_tr_t2025, reading_acquisition_mechanism__phonics_reading, theater_ratio, 2025, 0.41).
narrative_ontology:measurement_basis(read_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(read_be_t1985, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1985, 0.32).
narrative_ontology:measurement_basis(read_be_t1985, observed).
narrative_ontology:measurement(read_be_t1995, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement_basis(read_be_t1995, observed).
narrative_ontology:measurement(read_be_t2005, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(read_be_t2005, observed).
narrative_ontology:measurement(read_be_t2015, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement_basis(read_be_t2015, observed).
narrative_ontology:measurement(read_be_t2025, reading_acquisition_mechanism__phonics_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(read_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t1985, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement_basis(read_su_t1985, observed).
narrative_ontology:measurement(read_su_t1995, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 1995, 0.54).
narrative_ontology:measurement_basis(read_su_t1995, observed).
narrative_ontology:measurement(read_su_t2005, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement_basis(read_su_t2005, observed).
narrative_ontology:measurement(read_su_t2015, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement_basis(read_su_t2015, observed).
narrative_ontology:measurement(read_su_t2025, reading_acquisition_mechanism__phonics_reading, suppression_requirement, 2025, 0.72).
narrative_ontology:measurement_basis(read_su_t2025, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1985, tn=2025
narrative_ontology:measurement(read_grid_01, reading_acquisition_mechanism__phonics_reading, accessibility_collapse(class), 1985, 0.41).
narrative_ontology:measurement(read_grid_02, reading_acquisition_mechanism__phonics_reading, accessibility_collapse(class), 2025, 0.78).
narrative_ontology:measurement(read_grid_03, reading_acquisition_mechanism__phonics_reading, accessibility_collapse(individual), 1985, 0.45).
narrative_ontology:measurement(read_grid_04, reading_acquisition_mechanism__phonics_reading, accessibility_collapse(individual), 2025, 0.68).
narrative_ontology:measurement(read_grid_05, reading_acquisition_mechanism__phonics_reading, accessibility_collapse(organizational), 1985, 0.52).
narrative_ontology:measurement(read_grid_06, reading_acquisition_mechanism__phonics_reading, accessibility_collapse(organizational), 2025, 0.82).
narrative_ontology:measurement(read_grid_07, reading_acquisition_mechanism__phonics_reading, accessibility_collapse(structural), 1985, 0.55).
narrative_ontology:measurement(read_grid_08, reading_acquisition_mechanism__phonics_reading, accessibility_collapse(structural), 2025, 0.85).
narrative_ontology:measurement(read_grid_09, reading_acquisition_mechanism__phonics_reading, resistance(class), 1985, 0.65).
narrative_ontology:measurement(read_grid_10, reading_acquisition_mechanism__phonics_reading, resistance(class), 2025, 0.55).
narrative_ontology:measurement(read_grid_11, reading_acquisition_mechanism__phonics_reading, resistance(individual), 1985, 0.68).
narrative_ontology:measurement(read_grid_12, reading_acquisition_mechanism__phonics_reading, resistance(individual), 2025, 0.48).
narrative_ontology:measurement(read_grid_13, reading_acquisition_mechanism__phonics_reading, resistance(organizational), 1985, 0.72).
narrative_ontology:measurement(read_grid_14, reading_acquisition_mechanism__phonics_reading, resistance(organizational), 2025, 0.52).
narrative_ontology:measurement(read_grid_15, reading_acquisition_mechanism__phonics_reading, resistance(structural), 1985, 0.62).
narrative_ontology:measurement(read_grid_16, reading_acquisition_mechanism__phonics_reading, resistance(structural), 2025, 0.58).
narrative_ontology:measurement(read_grid_17, reading_acquisition_mechanism__phonics_reading, stakes_inflation(class), 1985, 0.35).
narrative_ontology:measurement(read_grid_18, reading_acquisition_mechanism__phonics_reading, stakes_inflation(class), 2025, 0.68).
narrative_ontology:measurement(read_grid_19, reading_acquisition_mechanism__phonics_reading, stakes_inflation(individual), 1985, 0.38).
narrative_ontology:measurement(read_grid_20, reading_acquisition_mechanism__phonics_reading, stakes_inflation(individual), 2025, 0.71).
narrative_ontology:measurement(read_grid_21, reading_acquisition_mechanism__phonics_reading, stakes_inflation(organizational), 1985, 0.42).
narrative_ontology:measurement(read_grid_22, reading_acquisition_mechanism__phonics_reading, stakes_inflation(organizational), 2025, 0.74).
narrative_ontology:measurement(read_grid_23, reading_acquisition_mechanism__phonics_reading, stakes_inflation(structural), 1985, 0.45).
narrative_ontology:measurement(read_grid_24, reading_acquisition_mechanism__phonics_reading, stakes_inflation(structural), 2025, 0.72).
narrative_ontology:measurement(read_grid_25, reading_acquisition_mechanism__phonics_reading, suppression(class), 1985, 0.38).
narrative_ontology:measurement(read_grid_26, reading_acquisition_mechanism__phonics_reading, suppression(class), 2025, 0.72).
narrative_ontology:measurement(read_grid_27, reading_acquisition_mechanism__phonics_reading, suppression(individual), 1985, 0.32).
narrative_ontology:measurement(read_grid_28, reading_acquisition_mechanism__phonics_reading, suppression(individual), 2025, 0.65).
narrative_ontology:measurement(read_grid_29, reading_acquisition_mechanism__phonics_reading, suppression(organizational), 1985, 0.41).
narrative_ontology:measurement(read_grid_30, reading_acquisition_mechanism__phonics_reading, suppression(organizational), 2025, 0.75).
narrative_ontology:measurement(read_grid_31, reading_acquisition_mechanism__phonics_reading, suppression(structural), 1985, 0.42).
narrative_ontology:measurement(read_grid_32, reading_acquisition_mechanism__phonics_reading, suppression(structural), 2025, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_mechanism__phonics_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(reading_acquisition_mechanism__phonics_reading, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__whole_language_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, reading_acquisition_mechanism__balanced_literacy_reading).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, teacher_professional_identity_lock).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, dyslexia_identification_institutional_gatekeeping).
narrative_ontology:affects_constraint(reading_acquisition_mechanism__phonics_reading, curriculum_adoption_market_concentration).

% DUAL FORMULATION NOTE:
% Reading acquisition mechanisms form a three-reading kernel family. Phonics_reading (this constraint) forecloses whole_language_reading (core axioms incompatible: phonemic-awareness prerequisites vs. implicit-emergence mechanisms). Phonics_reading influences balanced_literacy_reading (both incorporate explicit phonics, but balanced-literacy's resource integration may produce superior outcomes on non-fluency dimensions, challenging phonics-first primacy claim). All three readings are in active institutional and professional contest; none has achieved settled empirical or policy dominance, despite phonics-reading's current institutional power consolidation. The constraint networks to dyslexia-identification gatekeeping because the phonics-reading's benefits concentrate on readers screened and identified via phoneme-awareness deficits, raising the omega question: is the benefit from addressing true deficits or from resource-allocation gatekeeping via screening. The constraint networks to teacher-professional-identity-lock because whole-language and balanced-literacy practitioners experience identity damage and career penalty when phonics-first mandates devalue their training and pedagogy—the suppression mechanism operates via professional identity fusion, making exit costly even when mandates are relaxed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reading_acquisition_mechanism__phonics_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
