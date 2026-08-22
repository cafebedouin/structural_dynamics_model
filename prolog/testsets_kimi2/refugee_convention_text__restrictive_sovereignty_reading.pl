% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Restrictive Sovereignty Reading of the 1951 Refugee Convention
 *   domain: international law / migration governance / human rights
 *
 * SUMMARY:
 *   This constraint instantiates the restrictive_sovereignty_reading of the
 *   refugee_convention_text kernel. It reads the 1951 Convention as a
 *   state-consent instrument establishing only a minimum protection floor,
 *   permitting maximum sovereign discretion over admission, procedural
 *   design, and categorical interpretation. The reading narrows 'well-founded
 *   fear' to individualized, state-targeted persecution; limits 'particular
 *   social group' to immutable characteristics with state awareness; and
 *   treats offshore processing and safe-third-country arrangements as
 *   permissible exercises of discretion. Sibling readings include
 *   expansive_humanitarian_reading (broad protection mandate encompassing
 *   generalized violence and gender-based claims) and
 *   procedural_integrity_reading (non-negotiable fair process priority). The
 *   authored metrics treat the constraint as substantially extractive and
 *   actively enforced while acknowledging a residual coordination function
 *   (minimum floor preventing total closure); the claimed type is
 *   tangled_rope, leaving the engine to measure the exact divergence.
 *
 * KEY AGENTS:
 *   - sovereign_states (agenda_setter/beneficiary): States party to the convention who interpret it narrowly to retain border control, externalize processing, and limit categorical eligibility.
 *   - asylum_seekers (payer): Individuals who must prove individualized state persecution and are excluded if they flee generalized violence or lack state nexus.
 *   - unhcr (observer): The UN refugee agency, mandated to supervise compliance but lacking enforcement power against sovereign discretion.
 *   - human_rights_ngos (excluded): Advocacy groups arguing for expansive interpretation, structurally excluded from interstate asylum policymaking.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.72).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.78).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Restrictive Sovereignty Reading of the 1951 Refugee Convention").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international law / migration governance / human rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, 'c1342e80-07d7-4b09-9de7-20a3c05f8720').
narrative_ontology:cs_kernel_codification('c1342e80-07d7-4b09-9de7-20a3c05f8720', fixed_text).
narrative_ontology:cs_authority_grounding('c1342e80-07d7-4b09-9de7-20a3c05f8720', lineage).
narrative_ontology:cs_interpretation_layer_present('c1342e80-07d7-4b09-9de7-20a3c05f8720').
narrative_ontology:cs_reading_relation('c1342e80-07d7-4b09-9de7-20a3c05f8720', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('c1342e80-07d7-4b09-9de7-20a3c05f8720', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('c1342e80-07d7-4b09-9de7-20a3c05f8720', foundational, state_sovereignty_primary_over_protection).
narrative_ontology:cs_axiom_status(state_sovereignty_primary_over_protection, holdable).
narrative_ontology:cs_axiom_grounding('c1342e80-07d7-4b09-9de7-20a3c05f8720', state_sovereignty_primary_over_protection, conventional).
narrative_ontology:cs_axiom('c1342e80-07d7-4b09-9de7-20a3c05f8720', foundational, immutable_characteristics_psg_limit).
narrative_ontology:cs_axiom_status(immutable_characteristics_psg_limit, holdable).
narrative_ontology:cs_axiom_grounding('c1342e80-07d7-4b09-9de7-20a3c05f8720', immutable_characteristics_psg_limit, conventional).
narrative_ontology:cs_reference_frame('c1342e80-07d7-4b09-9de7-20a3c05f8720', state_consent_minimum_floor).
narrative_ontology:cs_drift_state('c1342e80-07d7-4b09-9de7-20a3c05f8720', contemporary_human_rights_expansion, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c1342e80-07d7-4b09-9de7-20a3c05f8720', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, state_consent_doctrine).
narrative_ontology:constraint_vindicates(refugee_convention_text__restrictive_sovereignty_reading, westphalian_sovereignty_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate, interpret, and enforce asylum policy under the convention. Assert maximum discretion to control borders, externalize processing through safe-third-country agreements and offshore detention, and narrowly define protected categories. Collect sovereignty dividends from the legal cover the convention provides for selective admission.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Seek protection from persecution but must prove individualized, state-targeted fear to a high evidentiary standard. Excluded if fleeing generalized violence, gang persecution without state nexus, or gender-based harm in the absence of state enforcement. Subjected to interdiction, offshore detention, and expedited removal that limits access to substantive determination.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Mandated to supervise international protection and issue guidance on convention interpretation. Lacks enforcement power against sovereign discretion; its expansive guidelines are frequently ignored by states advancing restrictive readings.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, unhcr, observer,
    institutional, generational, analytical, global).

% Advocate for broad protection and contest narrow interpretive limits through litigation and reporting. Structurally excluded from interstate bargaining and from admissibility screening processes that filter out the categories of claimants they seek to represent.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, human_rights_ngos, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state behavior by establishing a negotiated minimum floor for refugee protection, apportioning responsibility among states and preventing total unilateral border closure while permitting differential implementation.
% TRANSFER_FUNCTION: Transfers the burden of proof and the risk of refoulement from states to asylum seekers, moving protection away from those fleeing generalized violence, non-state persecution, and gender-based harm.
% ABSENT_VOICES: Asylum seekers lacking documentary evidence of individualized state persecution; women fleeing gender-based violence; LGBTQ+ persons in jurisdictions without direct state criminalization; populations fleeing climate disaster or gang violence. They are filtered out by admissibility screening and interdiction before reaching substantive determination.
% DISAPPEARANCE_RATIONALE: If the restrictive reading vanished, states would lose the legal framework that legitimizes narrow discretion and externalization. Some jurisdictions would expand protection to fill the gap; others would abandon the convention for pure sovereignty. Regional burden-sharing and non-refoulement norms would destabilize rapidly.
% FOUNDING_PROBLEM: Post-Second World War displacement required an agreed international mechanism to identify protected persons and apportion responsibility among states, preventing total border closure and destabilizing refugee accumulation.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship and the travaux prÃ©paratoires corroborate the original displacement problem. No source outside the benefiting parties attests that the current restrictive readingâoffshore processing, safe-third-country exclusion, and immutable-characteristics limitsâstill serves that original humanitarian purpose. UNHCR and human rights organizations attest the arrangement has drifted toward sovereignty protection rather than refugee protection.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the reading systematically excludes broad categories of vulnerable peopleâgeneralized violence, non-state persecution, gender-based claimsâthrough definitional narrowing and procedural barriers. Suppression (0.78) is higher because the constraint's persistence depends on active border enforcement, interdiction, offshore detention, and admissibility screening that physically prevents access. Theater ratio (0.48) reflects growing performative compliance: states maintain the language of international protection while hollowing out access and substituting proxy goals (deterrence) for the stated function. Accessibility collapse (0.70) indicates that once the restrictive framework is institutionalized, alternatives (expansive interpretation, territorial access) are structurally invisible in state policymaking. Resistance (0.50) captures moderate but partially captured opposition from human rights bodies and some domestic courts.
 *
 * PERSPECTIVAL GAP:
 *   The sovereign_states seat experiences the constraint as a necessary coordination mechanism that preserves international order by preventing uncontrolled migration and apportioning limited responsibility. The asylum_seekers seat experiences it as an active barrier that extracts safety from the vulnerable by demanding proof they often cannot furnish and by excluding the contexts from which they flee. The engine will compute these seats differently because the structural dataâbeneficiary versus victim, arbitrage-grade exit versus trapped exitâdiverge sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   sovereign_states are declared beneficiaries with arbitrage-grade exit (they can denounce the convention, renegotiate, or reinterpret); the engine will derive a low directionality, dampening effective extraction for this seat. asylum_seekers are declared victims with trapped exit (cannot return home due to persecution, cannot enter due to border controls); the engine will derive a high directionality, amplifying effective extraction. unhcr and human_rights_ngos sit at institutional and organized power with analytical and constrained exit, near symmetric but with limited influence.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents pure mandatrophy because it retains a genuine coordination function: the convention does prevent total border closure and establishes some procedural norms that would not exist in a pure sovereignty default. However, the restrictive reading layers extraction onto that coordination by interpreting the same text to maximize discretion, narrow categories, and externalize obligations. The Tangled Rope classification captures this hybridity, preventing mislabeling as either pure coordination (Rope) or pure extraction (Snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generalized_violence_textual_warrant,
    'Does the 1951 Convention text categorically exclude generalized violence and non-state persecution without state nexus, or is this exclusion a later judicial construct serving sovereign discretion?',
    'Historical-legal analysis of the travaux prÃ©paratoires, early state practice (1949â1954), and comparative textual interpretation across authentic language versions.',
    'If the exclusion is a construct, the restrictive reading extracts protection from a broad victim set without textual warrant, raising extractiveness and undermining the coordination legitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generalized_violence_textual_warrant, conceptual, 'Textual basis for excluding generalized violence').

omega_variable(
    kernel_reading_ambiguity,
    'Is the convention text genuinely ambiguous between the restrictive sovereignty, expansive humanitarian, and procedural integrity readings, or does one reading have exclusive textual warrant?',
    'Corpus-wide analysis of the three constraint stories'' axioms and reference frames; convergent textual evidence would reduce ambiguity, while divergent coherent framings confirm it.',
    'If the text is irreducibly ambiguous, the divergence between readings is political rather than interpretive, and extraction is driven by power rather than fidelity to law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel text supports multiple coherent readings').

omega_variable(
    offshore_processing_outcome,
    'Does offshore processing under the restrictive reading produce protection outcomes comparable to onshore determination, or does it functionally nullify non-refoulement?',
    'Empirical comparison of recognition rates, procedural fairness metrics, and refoulement incidence between matched onshore and offshore cohorts.',
    'If outcomes collapse offshore, the coordination function (burden-sharing) is cover for extraction (evasion of obligations); if comparable, the arrangement may retain genuine coordination content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(offshore_processing_outcome, empirical, 'Protection outcomes under offshore processing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 0, 73).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rcrsr_tr_t0, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(rcrsr_tr_t15, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(rcrsr_tr_t30, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(rcrsr_tr_t45, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement(rcrsr_tr_t60, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 60, 0.45).
narrative_ontology:measurement(rcrsr_tr_t73, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 73, 0.48).

% Extraction over time
narrative_ontology:measurement(rcrsr_be_t0, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rcrsr_be_t15, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(rcrsr_be_t30, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(rcrsr_be_t45, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(rcrsr_be_t60, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(rcrsr_be_t73, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 73, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(rcrsr_su_t0, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(rcrsr_su_t15, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(rcrsr_su_t30, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(rcrsr_su_t45, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(rcrsr_su_t60, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement(rcrsr_su_t73, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 73, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
