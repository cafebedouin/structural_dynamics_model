% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__native_generative, []).

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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew Native Generative Speaker Requirement
 *   domain: sociolinguistics/language_revitalization
 *
 * SUMMARY:
 *   Hebrew revitalization from the late 19th century onward represents a
 *   remarkable linguistic transformation: a liturgically-preserved language
 *   was reconstructed as a daily native vernacular, primarily in
 *   Palestine/Israel. This story constrains the reading of that
 *   revitalization to a specific interpretation: Hebrew 'lives' only through
 *   native speaker intuition and daily generative use. This reading
 *   privileges Ashkenazi Zionist intellectual frameworks and Israeli
 *   institutional authority over pre-Zionist liturgical traditions and
 *   diaspora Hebrew practices. Under this constraint, Sephardi, Mizrahi,
 *   Yemenite, and other communities whose Hebrew was liturgically and
 *   culturally alive are reclassified as holding 'dead' Hebrew. The
 *   constraint enforces a particular claim about what authenticity, life, and
 *   ownership in language mean—and whose knowledge counts.
 *
 * KEY AGENTS:
 *   - Ashkenazi Zionist intellectuals (Eliezer Ben-Yehuda, successors): set the agenda, define what 'real' Hebrew is, benefit from canonical authority
 *   - Israeli state apparatus: enforces native-speaker standard through education, law, employment, media control
 *   - Native Hebrew-speaking children: are the constraint's mechanism—born into standardized forms, internalize them as intuition
 *   - Liturgical Hebrew communities (Sephardi, Mizrahi, Yemenite, Eastern European): victims whose knowledge is delegitimized, excluded from authority
 *   - Diaspora Jews without native speakers: bear the cost of reclassification as inauthentic
 *   - Hebrew Language Academy: institutional enforcer of standardized forms
 *   - Comparative linguists: observers who can articulate alternative framings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.68).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.71).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew Native Generative Speaker Requirement").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '35de79c2-e079-49a4-89ff-6e15629c94e6').
narrative_ontology:cs_kernel_codification('35de79c2-e079-49a4-89ff-6e15629c94e6', formalized).
narrative_ontology:cs_authority_grounding('35de79c2-e079-49a4-89ff-6e15629c94e6', extraction).
narrative_ontology:cs_interpretation_layer_present('35de79c2-e079-49a4-89ff-6e15629c94e6').
narrative_ontology:cs_reading_relation('35de79c2-e079-49a4-89ff-6e15629c94e6', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('35de79c2-e079-49a4-89ff-6e15629c94e6', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('35de79c2-e079-49a4-89ff-6e15629c94e6', foundational, native_generative_use_necessary_for_language_life).
narrative_ontology:cs_axiom_status(native_generative_use_necessary_for_language_life, holdable).
narrative_ontology:cs_axiom_grounding('35de79c2-e079-49a4-89ff-6e15629c94e6', native_generative_use_necessary_for_language_life, empirically_contingent).
narrative_ontology:cs_axiom('35de79c2-e079-49a4-89ff-6e15629c94e6', foundational, hebrew_authority_vested_in_native_speaker_intuition).
narrative_ontology:cs_axiom_status(hebrew_authority_vested_in_native_speaker_intuition, holdable).
narrative_ontology:cs_axiom_grounding('35de79c2-e079-49a4-89ff-6e15629c94e6', hebrew_authority_vested_in_native_speaker_intuition, deontological).
narrative_ontology:cs_reference_frame('35de79c2-e079-49a4-89ff-6e15629c94e6', ashkenazi_zionist_hebrew_authority).
narrative_ontology:cs_drift_state('35de79c2-e079-49a4-89ff-6e15629c94e6', contemporary_diaspora_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('35de79c2-e079-49a4-89ff-6e15629c94e6', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, ashkenazi_zionist_intellectuals).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_state_apparatus).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_hebrew_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_jews_without_native_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, non_hebrew_speaking_jewish_populations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__native_generative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 to 0.68 over the interval because the constraint's real cost—the delegitimization and exclusion of non-native Hebrew forms—becomes fully sedimented into institutions, education, and identity. Early on (t=0), the revival was still contested; many communities maintained parallel Hebrew practices. By t=120, the native-generative standard has become hegemonic—it is internalized by native speakers as natural, enforced in schools and media, codified in the Academy, embedded in Israeli state identity. Suppression rises sharply because suppression of alternative framings requires increasing institutional work: media control, curriculum standardization, redefinition of liturgical Hebrew as 'dead,' stigmatization of diaspora Hebrew as 'corrupted' or 'artificial.' Theater rises more slowly because the constraint initially solves a real coordination problem (unifying diverse Hebrew traditions into a working vernacular) but over time becomes increasingly performative—the constraint's persistence depends on insisting that native-speaker intuition is the only legitimate form, even as this claim becomes ideologically motivated rather than functionally necessary. The measurement series tracks the constraint's embedding: early flexibility, gradual ossification, eventual asymptotic settling around institutional enforcement.
 *
 * PERSPECTIVAL GAP:
 *   A critical asymmetry: from inside the constraint (native speakers, Israeli state), the native-generative reading appears as describing what Hebrew naturally is. From outside or from subordinated seats (diaspora, liturgical communities), it appears as a specific ideological choice that erases alternatives. The constraint's power lies partly in making one reading feel natural and inevitable rather than contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Ashkenazi Zionist intellectuals and the Israeli state are the structural beneficiaries: they set the agenda, define authority, control institutions, and benefit from the constraint's validation of their vision. Their directionality d is near 0.0 (they are funded by the system). Liturgical communities, diaspora Jews, and non-native speakers are the targets: they pay through delegitimization, exclusion, pressure to acquire native fluency, and loss of alternative pathways. Their d is near 1.0 (the constraint extracts from them). Native Hebrew-speaking children appear as beneficiaries (the system works for them—they inherit intuitive competence), but they are also partially locked in: their identity is constituted through the constraint, making exit identity-shattering. The Hebrew Language Academy and Israeli institutions are agenda-setters: they administer the enforcement. Comparative linguists are observers: they can describe what is happening but have no seat in the constraint's maintenance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—Hebrew as a fragmented diaspora language without daily use—was live in the late 19th century. By the late 20th century, it was substantially solved: Hebrew is the daily language of Israeli millions and the language of state. However, the constraint persists by redefining the problem in terms of 'purity' and 'authenticity': not just 'Hebrew is spoken' but 'Hebrew lives only through native intuition.' This allows the constraint to persist even after the coordination problem it solved is largely resolved. The theater ratio rises because maintaining the claim that 'only native generative use counts' requires increasing institutional performance—curriculum design, Academy pronouncements, media dominance, stigmatization of alternatives—even though these are no longer necessary to keep Hebrew alive as a language. The constraint has shifted from solving a real problem to defending a particular vision of Jewishness and Israeli identity. Mandatrophy is partial and contested: the Israeli state and benefiting parties assert the founding problem remains live (threats to Hebrew, diaspora assimilation), while critics assert it is dead or that it has been solved in ways that do not require the native-generative criterion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_speaker_criterion_contingency,
    'Is the native-speaker criterion a necessary condition for Hebrew''s linguistic vitality and perpetuation, or is it a particular ideological choice about what counts as ''authentic'' and ''living'' language?',
    'Comparative study of language revitalization cases where non-native, liturgical, and multilingual forms sustained linguistic vitality (Basque, Irish, Quechua); documentation of Hebrew functioning as a literary and scholarly language for 2000+ years without native speakers; empirical testing of whether native-speaker proficiency is actually required for the linguistic tasks Hebrew now performs (state administration, culture, daily communication) or whether competent non-native speakers can perform them adequately.',
    'If the native-speaker criterion is revealed as contingent rather than necessary, the constraint shifts from describing a fact about language life to enforcing an ideological choice. This would support alternative framings (liturgical, bridge, diaspora polyglot) as equally valid. The classification would shift from tangled rope (with genuine coordination component) toward snare (pure extraction under the guise of naturalness). If the criterion is actually necessary, the constraint''s classification is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_criterion_contingency, conceptual, 'Whether native generative use is a necessary condition for linguistic vitality or an ideological position.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of alternative Hebrew framings (liturgical, diaspora, non-native) primarily structural—enforced by institutions, law, and economic incentives—or is it also internalized in the identity and aspirations of Hebrew speakers themselves, such that the suppression would persist even if external enforcement were removed?',
    'Post-institutional removal testing: if Israel formally legitimized all Hebrew forms (liturgical, diaspora, non-native) and removed achievement hierarchies based on native fluency, would diaspora and liturgical communities reactivate those forms, or would they still pursue native-like proficiency because they have internalized the native-generative standard as what ''real'' Hebrew means? Historical cases of language subordination that were formally de-institutionalized but remained subjectively suppressed (post-colonial settings where colonized languages remained stigmatized despite official equality).',
    'If suppression is primarily structural, removing institutional enforcement would enable alternatives to flourish; classification would shift as the effective extraction decreased. If suppression is substantially internalized, alternatives would remain marginal even after institutional barriers fell, indicating the constraint has deeper psychic and identity dimensions than the structural mechanics suggest. Internalized suppression suggests the constraint operates partly through what the schema calls ''identity_locked'' exit options—targets cannot imagine themselves speaking Hebrew in non-native ways.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural versus internalized dimensions of suppression in the native-speaker standard.').

omega_variable(
    coordination_extraction_separability,
    'Is the genuine coordination function of Hebrew revitalization (unifying diverse Jewish populations and creating a shared linguistic identity) structurally inseparable from the extraction of delegitimizing alternative Hebrew forms? Or could Hebrew have been revitalized as a daily language while simultaneously preserving the legitimacy of liturgical and diaspora Hebrew traditions?',
    'Historical counterfactual: if early Hebrew revivalists (Ben-Yehuda era) had framed native generative Hebrew as ''new Hebrew'' or ''modern Hebrew'' (coordinate with but distinct from liturgical Hebrew) rather than as the sole authentic form, would the revitalization have succeeded in creating a functional daily vernacular? Ethnographic study of multilingual societies (Singapore, India) where multiple language varieties (colonial, indigenous, regional, native) coexist with institutional success.',
    'If the coordination and extraction are separable, the constraint is not structurally tangled—it is snare wearing rope clothing. The real coordination could be achieved by other means (Israeli state supporting multiple Hebrew varieties while establishing modern Hebrew as official). If inseparable, the tangled classification is sustained: you cannot have a unified national linguistic identity without delegitimizing the alternatives. This affects both type and victim remedies: separability suggests deextraction is possible without losing the coordination benefits; inseparability suggests the benefits and harms are fundamentally coupled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable or inherently coupled in Hebrew revitalization.').

omega_variable(
    false_summit_natural_language_claim,
    'Does the constraint describe a discovered fact about how languages live (a natural law: languages require native speakers to persist) or does it describe a constructed institutional choice about what forms of Hebrew are legitimate?',
    'Linguistic anthropology: documentation of natural languages that have persisted and evolved without native-speaker populations (Sanskrit in scholarly communities, Classical Arabic in liturgical and intellectual contexts, Latin in medieval Europe). Documentation of how the native-speaker criterion emerged historically in linguistics (Saussure, Chomsky) as a methodological choice tied to particular research questions, not as a discovered universal. Examination of whether the claim ''languages need native speakers'' conflates language persistence with language change—languages may persist through non-native transmission but undergo phonological, morphological, or semantic drift.',
    'If ''native speakers are necessary'' is revealed as contingent or false, the constraint is a false summit: it presents itself as natural law while benefiting identifiable parties (Ashkenazi intellectuals, Israeli state) whose authority and vision it validates. This would trigger FSM reclassification toward snare. If the claim is true, the constraint describes a genuine natural limit on language revitalization and the classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_language_claim, empirical, 'Whether the native-speaker requirement is a natural law or a contingent institutional choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hebr_tr_t15, hebrew_continuity__native_generative, theater_ratio, 15, 0.12).
narrative_ontology:measurement(hebr_tr_t30, hebrew_continuity__native_generative, theater_ratio, 30, 0.18).
narrative_ontology:measurement(hebr_tr_t45, hebrew_continuity__native_generative, theater_ratio, 45, 0.26).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__native_generative, theater_ratio, 60, 0.34).
narrative_ontology:measurement(hebr_tr_t75, hebrew_continuity__native_generative, theater_ratio, 75, 0.39).
narrative_ontology:measurement(hebr_tr_t90, hebrew_continuity__native_generative, theater_ratio, 90, 0.41).
narrative_ontology:measurement(hebr_tr_t120, hebrew_continuity__native_generative, theater_ratio, 120, 0.42).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hebr_be_t15, hebrew_continuity__native_generative, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(hebr_be_t30, hebrew_continuity__native_generative, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(hebr_be_t45, hebrew_continuity__native_generative, base_extractiveness, 45, 0.54).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__native_generative, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(hebr_be_t75, hebrew_continuity__native_generative, base_extractiveness, 75, 0.66).
narrative_ontology:measurement(hebr_be_t90, hebrew_continuity__native_generative, base_extractiveness, 90, 0.67).
narrative_ontology:measurement(hebr_be_t120, hebrew_continuity__native_generative, base_extractiveness, 120, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(hebr_su_t15, hebrew_continuity__native_generative, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(hebr_su_t30, hebrew_continuity__native_generative, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(hebr_su_t45, hebrew_continuity__native_generative, suppression_requirement, 45, 0.58).
narrative_ontology:measurement(hebr_su_t60, hebrew_continuity__native_generative, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(hebr_su_t75, hebrew_continuity__native_generative, suppression_requirement, 75, 0.69).
narrative_ontology:measurement(hebr_su_t90, hebrew_continuity__native_generative, suppression_requirement, 90, 0.7).
narrative_ontology:measurement(hebr_su_t120, hebrew_continuity__native_generative, suppression_requirement, 120, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__native_generative, 0.12).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, jewish_identity_zionist_framework).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, israeli_linguistic_nationalism).

% DUAL FORMULATION NOTE:
% The hebrew_continuity kernel has three structurally distinct readings: native_generative (this story), liturgical_preservation, and bridge_pidginized. Each reading assigns different ε values and victim/beneficiary structures to the same kernel because they define what 'living Hebrew' means differently. This story chains upstream to liturgical_preservation (which it forecloses in single-framework contexts) and exerts pressure on bridge_pidginized (which it influences by delegitimizing as inauthentic). All three stories must be present in the corpus to capture the full contested structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__native_generative, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
