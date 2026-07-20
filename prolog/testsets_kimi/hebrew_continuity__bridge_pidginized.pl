% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew Bridge Pidgin Continuity Reading
 *   domain: sociolinguistic/commitment_system
 *
 * SUMMARY:
 *   This constraint instantiates the bridge_pidginized reading of the
 *   hebrew_continuity kernel: the claim that Hebrew persists as a living
 *   language through its instrumental utility as a contact/pidgin register
 *   for Jewish diaspora interaction, rather than through liturgical
 *   preservation or native generative competence. The reading is contested by
 *   liturgical authorities (who dismiss it as profanation) and native-speaker
 *   purists (who dismiss it as deficient interlanguage). Structurally, the
 *   arrangement coordinates diaspora Jewish communication and identity
 *   performance while channeling resources into state and educational
 *   institutions that validate and maintain the bridge register. Sparse
 *   native speakers outside Israel, high-register written production, and
 *   marketplace pidgin use characterize the actual language ecology. Both
 *   sibling readings reject this as 'not really Hebrew,' creating a
 *   legitimacy contest that the constraint actively manages through
 *   institutional enforcement.
 *
 * KEY AGENTS:
 *   - State language institutions (agenda_setter): Set policy, certify, fund, and enforce the bridge reading as legitimate continuity.
 *   - Diaspora educators (beneficiary): Deliver Hebrew instruction; institutional survival depends on the constraint.
 *   - Diaspora learners (payer): Invest time/money; achieve contact register; face competence gap and identity lock-in.
 *   - Native speaker community (payer): Bear accommodation burden; experience symbolic dilution without reciprocity.
 *   - Liturgical guardians (excluded): Reject secular bridge Hebrew; marginalized in policy.
 *   - Linguistic observers (observer): Document the contest without institutional stake.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.65).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.5).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.65).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew Bridge Pidgin Continuity Reading").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistic/commitment_system").

domain_priors:requires_active_enforcement(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, 'd10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f').
narrative_ontology:cs_kernel_codification('d10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f', fixed_text).
narrative_ontology:cs_authority_grounding('d10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f', lineage).
narrative_ontology:cs_interpretation_layer_present('d10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f').
narrative_ontology:cs_reading_relation('d10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f', hebrew_continuity__liturgical_preservation, influences).
narrative_ontology:cs_reading_relation('d10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f', hebrew_continuity__native_generative, influences).
narrative_ontology:cs_axiom('d10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f', foundational, instrumental_utility_suffices_for_continuity).
narrative_ontology:cs_axiom_status(instrumental_utility_suffices_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('d10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f', instrumental_utility_suffices_for_continuity, conventional).
narrative_ontology:cs_axiom('d10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f', foundational, non_native_register_counts_as_hebrew).
narrative_ontology:cs_axiom_status(non_native_register_counts_as_hebrew, holdable).
narrative_ontology:cs_axiom_grounding('d10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f', non_native_register_counts_as_hebrew, conventional).
narrative_ontology:cs_reference_frame('d10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f', diaspora_instrumental_continuity).
narrative_ontology:cs_drift_state('d10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f', contemporary_english_hegemony, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d10e27fa-fdf4-4ee6-a4fc-3b39ea5b325f', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, state_language_institutions).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_educators).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, diaspora_learners).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, native_speaker_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets curricular standards, certifies teachers, and funds ulpanim and diaspora Hebrew programs. Validates the bridge register as legitimate continuity of historical Hebrew, tying linguistic policy to state-building and diaspora engagement goals.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, state_language_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Operate schools and adult education programs that deliver Hebrew instruction to diaspora populations. Their institutional budgets, enrollments, and professional identity depend on the constraint that Hebrew remains a necessary diaspora contact language.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_educators, beneficiary,
    organized, biographical, constrained, regional).

% Invest years of study and significant tuition in acquiring Hebrew for identity, travel, and communal participation. They typically achieve a classroom or contact register that differs markedly from native Israeli Hebrew, leaving them in a permanent competence gap relative to the native norm.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_learners, payer,
    moderate, biographical, identity_locked, regional).

% Native Israeli Hebrew speakers who interact with diaspora learners in educational, tourist, or marketplace contexts. They bear the communicative burden of accommodating simplified or fossilized learner interlanguage, while their own native norms are not reciprocally accommodated in diaspora settings.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, native_speaker_community, payer,
    organized, biographical, mobile, national).

% Religious authorities and traditionalist communities who maintain Hebrew as a sacred tongue for prayer and study. They reject the bridge reading as a profanation of holy speech and are structurally excluded from secular Zionist language policy and funding frameworks.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, liturgical_guardians, excluded,
    organized, civilizational, constrained, global).

% Sociolinguists and linguistic anthropologists who document the contact-language phenomenon and the contest between continuity readings. They observe the gap between institutional claims and actual language use without institutional stake in any particular reading.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, linguistic_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared symbolic and communicative medium for geographically dispersed Jewish communities that lack another common native language, enabling interaction, migration pathways, and collective identity performance across diaspora contexts.
% TRANSFER_FUNCTION: Moves financial and temporal resources from diaspora learners and state budgets to language institutions and educators; transfers communicative labor from diaspora communities to native speakers who accommodate non-native usage; moves legitimacy from liturgical and native-speaker exclusivity claims to an instrumental contact register.
% ABSENT_VOICES: Liturgical authorities who reject any secular or simplified Hebrew use as profane; generative linguists and native-speaker purists who deny that a contact pidgin constitutes language vitality. They are present in religious and academic discourse but excluded from policy and funding allocations.
% DISAPPEARANCE_RATIONALE: If Hebrew were no longer maintained as a diaspora contact language, diaspora Jewish international interaction would shift overwhelmingly to English, institutional Hebrew education outside Israel would contract to liturgical study, and Israeli state-diaspora cultural ties would lose a primary symbolic and practical medium.
% FOUNDING_PROBLEM: Jewish national dispersion created a need for a shared modern vernacular to facilitate collective identity, migration, and cultural-economic coordination across communities that historically lacked a common daily language other than local vernaculars or liturgical Hebrew.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and Israeli state institutions attest the problem remains live, citing ongoing diaspora engagement needs. Linguistic anthropologists and critical sociolinguists outside the benefiting parties note that English increasingly performs the contact function, corroborating a shift in the problem's urgency rather than its persistence.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__bridge_pidginized, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because the bridge arrangement extracts significant time and tuition from diaspora learners for a contact register with limited communicative return, and extracts symbolic capital from native speakers whose norms are displaced in international contexts. Suppression (0.50) reflects the active marginalization of liturgical and native-generative definitions in state-funded and Zionist institutional contexts, though these alternatives persist in non-state spheres. Theater ratio (0.52) captures the performative dimension: much diaspora Hebrew activity (holiday celebrations, ulpan ceremonies, social media displays) enacts continuity rather than enabling genuine spontaneous communication. Accessibility collapse (0.40) is moderate because English is the practical alternative for diaspora contact, but institutional and identity barriers block exit for committed learners. Resistance (0.45) comes from liturgical authorities, native-speaker purists, and some critical sociolinguists who challenge the legitimacy of the bridge reading. Temporal measurements show extraction and theater rising as institutionalization outpaces actual communicative utility.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state language institutions) experiences the constraint as successful national coordination preserving diaspora ties. The payer seats experience it differently: diaspora learners experience costly identity maintenance with low communicative payoff, while native speakers experience uncompensated accommodative labor and linguistic misrecognition. The excluded liturgical seat experiences the constraint as a desacralization. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   State language institutions and diaspora educators are structural beneficiaries (low directionality): they collect budget, legitimacy, and employment from the constraint's maintenance. Diaspora learners are identity-locked targets (high directionality): their exit is blocked by fused identity investment, and they bear the costs of the competence gap. Native speakers are mobile but structurally disadvantaged in cross-context interaction (moderate-high directionality): they can exit individual encounters but cannot escape the macro-level register shift. Liturgical guardians are excluded entirely, their directionality irrelevant to the constraint's operation because they are not party to it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâJewish dispersion requiring a shared modern vernacularâmay be obsolescent as English increasingly performs the diaspora contact function. However, the constraint is not a pure piton because the coordination function (diaspora identity maintenance, aliyah pipeline) has not fully atrophied; it is contested and thinning. The Tangled Rope classification prevents mislabeling the remaining coordination as pure extraction, while capturing the layered extraction that has accumulated around the institutional maintenance of the bridge reading. R5 genealogy flags a potential mandatrophy: founding_problem_status is contested, disappearance_verdict is world_rearranges, and theater is rising, suggesting the arrangement persists partly beyond its functional peak.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrumental_vs_identity_motivation,
    'What proportion of diaspora Hebrew maintenance is driven by genuine instrumental communication need versus identity performance and institutional inertia?',
    'Corpus analysis of actual diaspora Hebrew use domains (business, tourism, religious, social) compared with attitudinal surveys on motivation, plus economic analysis of Hebrew education returns.',
    'If instrumental need is low, the extraction profile is higher â learners and funders pay for symbolic rather than practical value, moving the constraint toward snare-like territory. If instrumental need is high, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrumental_vs_identity_motivation, empirical, 'Whether diaspora Hebrew use is instrumental or performative.').

omega_variable(
    native_speaker_cost_bearing,
    'Do native Hebrew speakers bear significant communicative or symbolic costs from the diaspora contact register, or is their accommodation minimal and reciprocal?',
    'Interactional sociolinguistic studies of native-non-native Hebrew discourse; attitudinal research among Israeli speakers on diaspora Hebrew legitimacy.',
    'If costs are negligible, the victim classification of native speakers may be over-assigned, reducing the asymmetric extraction component. If costs are substantial, the tangled rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(native_speaker_cost_bearing, empirical, 'Whether native speakers are genuine victims of the bridge register.').

omega_variable(
    kernel_reading_legitimacy,
    'Can a contact or pidginized register legitimately claim continuity with a language whose other readings demand either liturgical purity or native generative competence?',
    'Comparative historical linguistics on language death and revitalization cases; philosophical analysis of language identity criteria.',
    'If the bridge reading is conceptually incoherent, the constraint rests on a false premise and functions as a snare of institutional extraction. If coherent, the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy, conceptual, 'Conceptual legitimacy of the bridge reading against sibling criteria.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_bridge_tr_t0, hebrew_continuity__bridge_pidginized, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hebrew_bridge_tr_t10, hebrew_continuity__bridge_pidginized, theater_ratio, 10, 0.28).
narrative_ontology:measurement(hebrew_bridge_tr_t20, hebrew_continuity__bridge_pidginized, theater_ratio, 20, 0.36).
narrative_ontology:measurement(hebrew_bridge_tr_t30, hebrew_continuity__bridge_pidginized, theater_ratio, 30, 0.43).
narrative_ontology:measurement(hebrew_bridge_tr_t40, hebrew_continuity__bridge_pidginized, theater_ratio, 40, 0.48).
narrative_ontology:measurement(hebrew_bridge_tr_t50, hebrew_continuity__bridge_pidginized, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(hebrew_bridge_be_t0, hebrew_continuity__bridge_pidginized, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hebrew_bridge_be_t10, hebrew_continuity__bridge_pidginized, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hebrew_bridge_be_t20, hebrew_continuity__bridge_pidginized, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(hebrew_bridge_be_t30, hebrew_continuity__bridge_pidginized, base_extractiveness, 30, 0.59).
narrative_ontology:measurement(hebrew_bridge_be_t40, hebrew_continuity__bridge_pidginized, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(hebrew_bridge_be_t50, hebrew_continuity__bridge_pidginized, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_bridge_su_t0, hebrew_continuity__bridge_pidginized, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(hebrew_bridge_su_t10, hebrew_continuity__bridge_pidginized, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(hebrew_bridge_su_t20, hebrew_continuity__bridge_pidginized, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(hebrew_bridge_su_t30, hebrew_continuity__bridge_pidginized, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(hebrew_bridge_su_t40, hebrew_continuity__bridge_pidginized, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(hebrew_bridge_su_t50, hebrew_continuity__bridge_pidginized, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hebrew_continuity kernel, which decomposes into three structurally distinct claims about how Hebrew persists: liturgical transmission (preservation), native generativity (revival), and diaspora contact utility (bridge/pidgin). Each reading has a different epsilon, beneficiary structure, and victim set. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
