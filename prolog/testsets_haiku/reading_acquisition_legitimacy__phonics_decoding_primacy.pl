% ============================================================================
% CONSTRAINT STORY: reading_acquisition_legitimacy__phonics_decoding_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: reading_acquisition_legitimacy__phonics_decoding_primacy
 *   human_readable: Phonics Decoding Primacy Reading Instruction Framework
 *   domain: education/literacy/cognitive_science
 *
 * SUMMARY:
 *   This constraint instantiates the phonics-decoding-primacy reading of the
 *   contested kernel 'reading_acquisition_legitimacy.' The reading asserts
 *   that reading is fundamentally decoding (converting written symbols to
 *   sounds), that legitimate reading instruction must make the alphabetic
 *   principle explicit through systematic phonics, and that teacher-directed,
 *   structured sequencing with decodable texts and early diagnostic
 *   assessment of decoding gaps is the proper institutional foundation. This
 *   reading competes with whole-language meaning-primacy (reading as
 *   meaning-making; literature immersion; natural decoding emergence),
 *   balanced-literacy integration (both decoding and meaning-making; mix of
 *   phonics and authentic texts), and structured-literacy-remediation
 *   (phonics designed for struggling learners first). The measurement series
 *   documents the interval from early adoption (t=0, early 2000s,
 *   post-National Reading Panel influence) through plateau (t=40, mid-2020s,
 *   where implementation is widespread but resistance and competing framings
 *   persist). The claim/metric gap is intentional: the constraint is CLAIMED
 *   as tangled_rope (genuine coordination function in making decoding
 *   explicit + asymmetric extraction from alternative pedagogies), while the
 *   metrics document rising extractiveness and theater as the coordination
 *   function plateaus and enforcement overhead becomes more defensive. The
 *   engine measures this divergence; the story does not reconcile them.
 *
 * KEY AGENTS:
 *   - Structured-literacy researchers: institutional authority, sets research agenda and peer-review standards, benefits from funding and policy influence flowing from phonics-primacy adoption.
 *   - Phonics-curriculum publishers: powerful market beneficiary, profits from widespread adoption mandates and district purchasing aligned with the reading.
 *   - Traditional elementary educators: moderate power, benefit from explicit curriculum structures and validation, but also bear cost when mandates conflict with pedagogical judgment.
 *   - Whole-language advocates: organized resistance, pay cost of funding redirection, publication gatekeeping, professional marginalization; research framed as unscientific.
 *   - Balanced-literacy teachers: moderate power, constrained exit, caught between personal pedagogical integration and district mandates enforcing phonics-first sequencing.
 *   - Early struggling readers: powerless but centered in the constraint's stated beneficiary framing; benefit from early decoding diagnostics but may pay cost if phonics-only neglects comprehension.
 *   - Education policymakers: agenda-setters, enforce constraint through purchasing, standards, assessments, teacher evaluation aligned with phonics-primacy.
 *   - Literacy-assessments industry: powerful beneficiary, decoding-centered assessment tools become operational definition of reading progress.
 *   - Comparative reading science observers: analytical seat, documents that phonics-primacy reflects English orthographic properties and policy contexts, not universal reading science.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.58).
domain_priors:suppression_score(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.62).
domain_priors:theater_ratio(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(reading_acquisition_legitimacy__phonics_decoding_primacy, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reading_acquisition_legitimacy__phonics_decoding_primacy, tangled_rope).
narrative_ontology:human_readable(reading_acquisition_legitimacy__phonics_decoding_primacy, "Phonics Decoding Primacy Reading Instruction Framework").
narrative_ontology:topic_domain(reading_acquisition_legitimacy__phonics_decoding_primacy, "education/literacy/cognitive_science").

domain_priors:requires_active_enforcement(reading_acquisition_legitimacy__phonics_decoding_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reading_acquisition_legitimacy__phonics_decoding_primacy, '289d6a34-77db-4b03-b323-99d4e2a37b6c').
narrative_ontology:cs_kernel_codification('289d6a34-77db-4b03-b323-99d4e2a37b6c', formalized).
narrative_ontology:cs_authority_grounding('289d6a34-77db-4b03-b323-99d4e2a37b6c', extraction).
narrative_ontology:cs_interpretation_layer_present('289d6a34-77db-4b03-b323-99d4e2a37b6c').
narrative_ontology:cs_reading_relation('289d6a34-77db-4b03-b323-99d4e2a37b6c', reading_acquisition_legitimacy__whole_language_meaning_primacy, forecloses).
narrative_ontology:cs_reading_relation('289d6a34-77db-4b03-b323-99d4e2a37b6c', reading_acquisition_legitimacy__balanced_literacy_integration, influences).
narrative_ontology:cs_reading_relation('289d6a34-77db-4b03-b323-99d4e2a37b6c', reading_acquisition_legitimacy__structured_literacy_remediation, coexists_with).
narrative_ontology:cs_axiom('289d6a34-77db-4b03-b323-99d4e2a37b6c', foundational, reading_is_decoding_ontological).
narrative_ontology:cs_axiom_status(reading_is_decoding_ontological, holdable).
narrative_ontology:cs_axiom_grounding('289d6a34-77db-4b03-b323-99d4e2a37b6c', reading_is_decoding_ontological, empirically_contingent).
narrative_ontology:cs_axiom('289d6a34-77db-4b03-b323-99d4e2a37b6c', foundational, alphabetic_principle_requires_explicit_systematic_instruction).
narrative_ontology:cs_axiom_status(alphabetic_principle_requires_explicit_systematic_instruction, holdable).
narrative_ontology:cs_axiom_grounding('289d6a34-77db-4b03-b323-99d4e2a37b6c', alphabetic_principle_requires_explicit_systematic_instruction, empirically_contingent).
narrative_ontology:cs_axiom('289d6a34-77db-4b03-b323-99d4e2a37b6c', secondary, early_decoding_diagnostics_prevent_reading_failure).
narrative_ontology:cs_axiom_status(early_decoding_diagnostics_prevent_reading_failure, holdable).
narrative_ontology:cs_axiom_grounding('289d6a34-77db-4b03-b323-99d4e2a37b6c', early_decoding_diagnostics_prevent_reading_failure, empirically_contingent).
narrative_ontology:cs_reference_frame('289d6a34-77db-4b03-b323-99d4e2a37b6c', systematic_alphabetic_decoding_foundation).
narrative_ontology:cs_drift_state('289d6a34-77db-4b03-b323-99d4e2a37b6c', contemporary_comprehension_emphasis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('289d6a34-77db-4b03-b323-99d4e2a37b6c', '').
narrative_ontology:cs_kernel_id(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, structured_literacy_researchers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, phonics_curriculum_publishers).
narrative_ontology:constraint_beneficiary(reading_acquisition_legitimacy__phonics_decoding_primacy, traditional_elementary_educators).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, whole_language_advocates).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, meaning_centered_instruction_practitioners).
narrative_ontology:constraint_victim(reading_acquisition_legitimacy__phonics_decoding_primacy, balanced_literacy_teachers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reading_acquisition_legitimacy__phonics_decoding_primacy, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(reading_acquisition_legitimacy__phonics_decoding_primacy, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reading_acquisition_legitimacy__phonics_decoding_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reading_acquisition_legitimacy__phonics_decoding_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 at t=0 to 0.58 by t=25, then plateaus. This trajectory reflects the lifecycle of a coordinating constraint that accumulates extractive dynamics: early phase (t=0-10) emphasizes genuine coordination (standardizing decoding instruction, creating diagnostic common language); middle phase (t=10-25) sees extraction rise as policy enforcement intensifies, alternative pedagogies are increasingly suppressed, and benefits concentrate in publishing and research sectors; late phase (t=25-40) shows plateau as suppression requirement stabilizes and resistance ossifies (whole-language and balanced-literacy communities persist despite marginalization). Theater ratio rises from 0.15 to 0.41 over the same interval, indicating increasing performative activity: research continues to frame phonics as 'the science,' but mounting evidence of effectiveness variation across student populations and contexts requires defensive rhetorical work to maintain the primacy framing. Suppression is high and rising (0.45 → 0.64) because the constraint's persistence depends on actively excluding alternative pedagogies from policy space, funding, and teacher preparation — the enforcement is not passive preference but active institutional gatekeeping. Accessibility-collapse is moderate-high (0.68): once a teacher or district understands the phonics-primacy frame, alternatives become harder to pursue (curriculum mandates, assessment alignment, teacher training requirements), but alternatives remain conceptually available and organizationally present in some contexts. Resistance is moderate (0.54): meaningful pushback from meaning-centered practitioners and balanced-literacy advocates, plus growing recognition from comparative reading science that phonics-primacy is context-dependent, not universal. The claim/metric independence is deliberate: this story claims tangled_rope (coordination + extraction + enforcement) while the metrics show rising theater and suppression, which the engine will flag for review. That divergence is exactly what the corpus measures.
 *
 * PERSPECTIVAL GAP:
 *   The structured-literacy researcher and phonics-publisher seats should compute as moderate beneficiaries (low d, benefiting from the constraint, arbitrage-grade exit options). The whole-language and balanced-literacy seats should compute as high-extraction targets (high d, victims of funding/policy exclusion, constrained exit). The traditional teacher seat should compute as dual (beneficiary of clarity, payer of mandate conflict, constrained exit moderates directionality). The policy-maker seat should compute as agenda-setter (controls enforcement, analytical exit). The early-struggling-reader seat is the critical divergence: the constraint claims to prioritize them (beneficiary framing), but extractiveness from alternative pedagogies may cost them comprehension development and meaning-making engagement — a complex d that sits between beneficiary and target. The engine's per-seat derivation from beneficiary/victim + exit will expose this structural asymmetry; the story's unified claim that this is 'coordination for struggling readers' masks the seat-level divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are declared as structured-literacy researchers (institutional power, arbitrage exit, benefits from research authority), phonics-curriculum publishers (powerful, arbitrage exit, profits from adoption), and traditional educators (moderate, constrained, benefit from clarity but also pay mandate cost — marked as secondary_role payer). Victims are declared as whole-language advocates (organized, constrained, pay funding and publication exclusion), balanced-literacy teachers (moderate, constrained, caught between pedagogy and mandate), and by extension meaning-centered instruction practitioners (excluded, but would articulate harm if included). Early struggling readers are marked as dual beneficiary/payer: they benefit from early decoding diagnostics but pay if phonics-only neglects comprehension. The directionality for the structured-literacy researchers should derive as near 0.0 (full beneficiary: controls authority, arbitrage exit, collects research prestige and funding). The whole-language advocates should derive as near 1.0 (full target: constrained exit, explicit victim declaration, funding/publication exclusion). Teachers in the middle should diverge by context: traditional educators holding phonics-aligned practice should compute lower d (beneficiary of clarity), while balanced-literacy practitioners should compute higher d (constrained by mandate conflict). The engine's derivation from these structured facts will compute the per-seat type divergence without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (decoding chaos, lack of early intervention) was genuine and partially addressed by phonics-primacy frameworks. By t=25-30, that founding problem is substantially solved: reading instruction is more systematic, early struggling readers are identified earlier via decoding assessments, and outcomes have improved broadly. However, the constraint persists at high extractiveness (0.58) because the institutional architecture (research funding, curriculum adoption, teacher evaluation, publishing) has become dependent on maintaining phonics-primacy authority. The theater_ratio rising to 0.41 by t=40 indicates increasing performative maintenance: research continues framing phonics as 'the science,' but mounting evidence of effectiveness variation and cross-linguistic differences requires defensive rhetorical work to suppress competing framings. This is mandatrophy: the constraint's founding function (standardizing decoding instruction) has been substantially accomplished, but it persists as extractive institutional inertia because no party with veto power benefits enough from fixing it (researchers profit from continued grants, publishers from curriculum sales, policymakers from alignment with established standards), and no party hurt enough by it can change it (whole-language advocates are marginalized, teachers are constrained by mandates, struggling readers are beneficiaries of early diagnostics even if comprehension lags). The constraint is neither dead (phonics is genuinely part of reading) nor fully live (its claimed primacy is increasingly contested by evidence). It is zombie: formerly justified, now persisting through institutional inertia and active suppression of alternatives that would require restructuring authority relationships.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    decoding_primacy_sufficiency,
    'Is explicit, systematic phonics decoding instruction sufficient for reading comprehension development, or is comprehension a separate skill requiring additional instructional components (vocabulary, syntax, meaning-making engagement)?',
    'Longitudinal studies comparing phonics-only instruction with phonics-plus-comprehension-strategy instruction, stratified by reader type (struggling decoders, decoding-skilled-but-comprehension-weak, typically developing). Effectiveness measured on both decoding fluency AND comprehension outcomes.',
    'If decoding alone is sufficient, phonics-primacy is justified and comprehension emerges naturally from fluent decoding. If comprehension requires separate instruction, phonics-primacy is partially extractive — it privileges decoding assessment while neglecting comprehension development, harming readers whose comprehension lags fluency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decoding_primacy_sufficiency, empirical, 'Whether phonics decoding is sufficient for reading or whether comprehension requires separate instruction.').

omega_variable(
    orthographic_universality,
    'Does phonics-primacy pedagogy apply universally across orthographies (English, transparent orthographies like Italian/Spanish, logographic systems like Chinese), or is it specifically adapted to English''s irregular phonology and complex morphology?',
    'Comparative reading science across languages and orthographic systems, examining what instructional reading frameworks are effective in each context and why (phonemic awareness demands, grapheme-phoneme consistency, morphological transparency).',
    'If universally necessary, phonics-primacy is a genuine foundation of reading science. If context-dependent, phonics-primacy reflects English-specific demands and Anglo-American policy contexts, not universal reading science — the constraint''s extraction involves claiming universal validity for a context-specific reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(orthographic_universality, empirical, 'Whether phonics-primacy applies universally or is English/orthography-specific.').

omega_variable(
    research_authority_capture,
    'Has the ''Science of Reading'' research infrastructure (funding allocation, peer review, publication gating, expertise credentialing) been captured by phonics-primacy advocates such that contradicting evidence is systematically excluded, reframed, or delegitimized?',
    'Meta-analysis of publication patterns, funding allocation by research tradition, peer review outcomes, and systematic documentation of research excluded from ''the science'' despite methodological soundness. Compare citation patterns and career trajectories for phonics advocates vs. alternative-pedagogy researchers.',
    'If research authority has been captured, the constraint functions as snare (pure extraction justified by false claims of scientific consensus). If research processes remain open to alternatives, phonics-primacy is genuine coordination grounded in evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(research_authority_capture, empirical, 'Whether research authority structures are open or captured by phonics-primacy advocates.').

omega_variable(
    early_intervention_cost_accounting,
    'Do the benefits of early phonics-decoding diagnostics and intervention (identifying struggling decoders at age 5-6) exceed the costs of phonics-only instruction that may suppress comprehension development and meaning-engagement for students whose challenge is not decoding but language development or motivation?',
    'Prospective study tracking early-identified struggling decoders through outcomes at age 10 (reading comprehension, engagement, transfer to other languages): compare groups receiving phonics-intensive intervention vs. phonics-plus-comprehension-development intervention. Measure both narrow (decoding fluency) and broad (comprehension, engagement, self-concept) outcomes.',
    'If decoding intervention benefits exceed comprehension-neglect costs, early phonics diagnostics are net beneficial. If costs are substantial, the constraint extracts from the early-struggling population it claims to serve — false-summit candidate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(early_intervention_cost_accounting, empirical, 'Whether early phonics intervention''s benefits exceed costs of comprehension-neglect.').

omega_variable(
    reading_is_decoding_ontology,
    'Is the foundational axiom ''reading is decoding'' a description of what reading fundamentally IS (an ontological claim), or a policy choice about how to teach reading (a normative claim about instruction sequencing)?',
    'Conceptual analysis: if ''reading is decoding'' is ontological (reading cannot exist without decoding), then it is true universally and non-negotiable. If it is normative (we will teach decoding first), it is a policy choice that could be sequenced differently. Examine whether the constraint defends the claim as description or as prescriptive policy, and whether slippage between these frames is used to exclude alternative pedagogies.',
    'If the claim slides between ontology and policy, the constraint is using false naturalization (false summit) to justify exclusion of alternatives. If the claim is explicit policy, alternatives can be presented as competing policy choices rather than scientifically refuted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_is_decoding_ontology, conceptual, 'Whether ''reading is decoding'' is an ontological or normative claim.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.62 at interval end) structural (policy mandates, funding gatekeeping, publication exclusion) or internalized (meaning-centered teachers have absorbed the phonics-primacy frame and believe it is scientifically mandatory), or both?',
    'Qualitative interviews with balanced-literacy and meaning-centered teachers in phonics-mandate districts: ask whether they perceive suppression as external (forced compliance against their judgment) or internal (they have shifted their beliefs toward phonics-primacy). Post-exit trajectory: if teachers who exit phonics-mandate contexts return to meaning-centered practices, suppression was partly structural; if they maintain phonics-first sequencing, suppression is partly internalized.',
    'If suppression is structural, policy change (removing mandates, reopening funding to alternatives) could reduce extractiveness. If internalized, the constraint is embedded in teacher professional identity and cognition; exit requires not just policy change but epistemic re-education.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in teacher cognition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reading_acquisition_legitimacy__phonics_decoding_primacy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(read_tr_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 0, 0.15).
narrative_ontology:measurement(read_tr_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 5, 0.22).
narrative_ontology:measurement(read_tr_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 10, 0.28).
narrative_ontology:measurement(read_tr_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 15, 0.35).
narrative_ontology:measurement(read_tr_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 20, 0.39).
narrative_ontology:measurement(read_tr_t25, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 25, 0.41).
narrative_ontology:measurement(read_tr_t30, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 30, 0.42).
narrative_ontology:measurement(read_tr_t35, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 35, 0.41).
narrative_ontology:measurement(read_tr_t40, reading_acquisition_legitimacy__phonics_decoding_primacy, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(read_be_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(read_be_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(read_be_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(read_be_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(read_be_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(read_be_t25, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(read_be_t30, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(read_be_t35, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 35, 0.58).
narrative_ontology:measurement(read_be_t40, reading_acquisition_legitimacy__phonics_decoding_primacy, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(read_su_t0, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(read_su_t5, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 5, 0.51).
narrative_ontology:measurement(read_su_t10, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(read_su_t15, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(read_su_t20, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(read_su_t25, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 25, 0.64).
narrative_ontology:measurement(read_su_t30, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(read_su_t35, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 35, 0.62).
narrative_ontology:measurement(read_su_t40, reading_acquisition_legitimacy__phonics_decoding_primacy, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reading_acquisition_legitimacy__phonics_decoding_primacy, information_standard).
narrative_ontology:boltzmann_floor_override(reading_acquisition_legitimacy__phonics_decoding_primacy, 0.12).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__whole_language_meaning_primacy).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__balanced_literacy_integration).
narrative_ontology:affects_constraint(reading_acquisition_legitimacy__phonics_decoding_primacy, reading_acquisition_legitimacy__structured_literacy_remediation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the kernel 'reading_acquisition_legitimacy.' The kernel is the contested institutional commitment: 'What is reading, and what makes reading instruction legitimate?' Decomposition is required because different readings instantiate different constraints with different ε values, beneficiary/victim structures, and enforcement mechanisms. Phonics-decoding-primacy claims reading IS decoding; extractiveness derives from suppression of alternatives (whole-language meaning-primacy, balanced-literacy integration) and concentration of institutional authority. Whole-language meaning-primacy claims reading IS meaning-making; extractiveness would derive differently (suppression of decoding focus, literature-access control). Balanced-literacy-integration claims both are necessary; extractiveness derives from reconciling competing demands. Structured-literacy-remediation claims vulnerable-learner-first design; extractiveness would derive from whether that design is used inclusively or becomes a tracking mechanism. Each reading is a separate constraint with its own ε, its own beneficiary/victim declarations, and its own type — linked by network.affects_constraints to show the family structure and policy competition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
