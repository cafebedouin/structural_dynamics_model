% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__secular_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__secular_nationalist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: turkish_graphemic_substrate__secular_nationalist_reading
 *   human_readable: Secular Nationalist Turkish Script Reform (Latin Substrate Legitimacy)
 *   domain: political_linguistics/state_formation/cultural_engineering
 *
 * SUMMARY:
 *   In 1928 the Turkish Republic abandoned the Arabic script in favor of a
 *   Latin-based alphabet, constitutionally entrenching the claim that Turkish
 *   linguistic identity was distinct from its Ottoman-Islamic past and
 *   aligned with European modernity. This constraint story captures the
 *   secular nationalist reading of that reform: a state-imposed graphemic
 *   rupture enforced by criminalizing Arabic-script public use, standardizing
 *   education, and devaluing Ottoman cultural capital. The reading treats the
 *   Latin substrate as the sole legitimate written form and the Ottoman past
 *   as a foreign, superseded formation. It is one of three structurally
 *   distinct readings of the same kernel; the other two (Ottoman continuity
 *   and gradual transition) are modeled as separate constraints.
 *
 * KEY AGENTS:
 *   - republican_state_apparatus (agenda_setter/beneficiary): institutional power, arbitrage exit â sets and enforces the script monopoly
 *   - urban_secular_intelligentsia (beneficiary): moderate power, mobile exit â gains cultural authority under the new script
 *   - ottoman_literate_elite (payer): moderate power, identity_locked exit â bears immediate cultural capital destruction
 *   - religious_scholars (payer): organized power, constrained exit â loses scriptural mediation role
 *   - anatolian_rural_population (payer): powerless, trapped exit â bears compliance costs without receiving coordination benefits
 *   - arabic_script_advocates (excluded): moderate power, trapped exit â structurally absent from policy formation
 *   - comparative_linguists (observer): analytical power, analytical exit â external analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, 0.78).
domain_priors:suppression_score(turkish_graphemic_substrate__secular_nationalist_reading, 0.65).
domain_priors:theater_ratio(turkish_graphemic_substrate__secular_nationalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__secular_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__secular_nationalist_reading, "Secular Nationalist Turkish Script Reform (Latin Substrate Legitimacy)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__secular_nationalist_reading, "political_linguistics/state_formation/cultural_engineering").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__secular_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__secular_nationalist_reading, '2da618d8-394c-4159-8538-c74aed40a529').
narrative_ontology:cs_kernel_codification('2da618d8-394c-4159-8538-c74aed40a529', formalized).
narrative_ontology:cs_authority_grounding('2da618d8-394c-4159-8538-c74aed40a529', lineage).
narrative_ontology:cs_interpretation_layer_present('2da618d8-394c-4159-8538-c74aed40a529').
narrative_ontology:cs_reading_relation('2da618d8-394c-4159-8538-c74aed40a529', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('2da618d8-394c-4159-8538-c74aed40a529', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('2da618d8-394c-4159-8538-c74aed40a529', foundational, national_modernity_requires_graphemic_rupture).
narrative_ontology:cs_axiom_status(national_modernity_requires_graphemic_rupture, holdable).
narrative_ontology:cs_axiom_grounding('2da618d8-394c-4159-8538-c74aed40a529', national_modernity_requires_graphemic_rupture, instrumental).
narrative_ontology:cs_axiom('2da618d8-394c-4159-8538-c74aed40a529', foundational, european_alignment_as_linguistic_teleology).
narrative_ontology:cs_axiom_status(european_alignment_as_linguistic_teleology, holdable).
narrative_ontology:cs_axiom_grounding('2da618d8-394c-4159-8538-c74aed40a529', european_alignment_as_linguistic_teleology, conventional).
narrative_ontology:cs_reference_frame('2da618d8-394c-4159-8538-c74aed40a529', secular_nationalist_linguistic_order).
narrative_ontology:cs_drift_state('2da618d8-394c-4159-8538-c74aed40a529', contemporary_akp_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2da618d8-394c-4159-8538-c74aed40a529', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, urban_secular_intelligentsia).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, republican_state_apparatus).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_literate_elite).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, anatolian_rural_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the 1928 Alphabet Law and subsequent language reforms through the Turkish Language Association and Ministry of National Education. Enforces exclusive Latin-script use in all state, legal, educational, and public contexts; prohibits Arabic-script publishing and signage. Captures territorial homogenization, ideological alignment, and the symbolic rupture that anchors republican legitimacy.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, republican_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__secular_nationalist_reading, republican_state_apparatus, beneficiary).

% Gains cultural authority, professional positions, and symbolic capital as the native literate class of the new republic. Their mastery of the Latin script and the purified Turkish vocabulary becomes the gatekeeping mechanism for state employment, journalism, and modern literature. They produce and reproduce the nationalist narrative of linguistic renewal.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, urban_secular_intelligentsia, beneficiary,
    moderate, biographical, mobile, national).

% Held cultural capital and institutional roles through Ottoman Turkish literacy in Arabic script. The reform instantaneously devalues their expertise; they must relearn literacy from scratch or exit public life. Their personal libraries and family archives become illegible to younger generations, locking them into a deprecated identity with high replacement costs.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_literate_elite, payer,
    moderate, biographical, identity_locked, national).

% Depend on Arabic-script textual traditions for Quranic exegesis, hadith scholarship, and theological jurisprudence. The reform severs the direct scriptural literacy of the broader congregation and forces religious instruction into a Latin-script state curriculum supervised by the Diyanet. Their institutional authority is partially captured and redirected by the state.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars, payer,
    organized, generational, constrained, national).

% Predominantly non-literate or marginally literate in any script; the reform replaces one distant state language with another without delivering accessible schooling in the short term. They lose the customary mediation of religious and legal texts by local literate figures, while gaining no practical literacy benefit for a generation. The compliance cost is borne through cultural disorientation and delayed state integration.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, anatolian_rural_population, payer,
    powerless, biographical, trapped, regional).

% Argue for the preservation of Arabic script or a dual-script transition on historical, religious, or practical grounds. Systematically excluded from the 1928 Language Commission and subsequent policy councils; their publications are censored and their public advocacy criminalized under the script ban. Their voice is structurally absent from the official deliberative process.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, arabic_script_advocates, excluded,
    moderate, biographical, trapped, national).

% Analyze the reform as a case of coercive language planning and state-driven identity engineering. They observe the divergence between the official claim of scientific phonetic optimization and the actual outcome of generational rupture, noting that the reform's extractive costs were borne asymmetrically by non-urban and religious populations.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Homogenizes a multi-ethnic, multi-script imperial territory into a unified nation-state with a single standardized written language; eliminates the diglossia between high Ottoman and vernacular Turkish; enables centralized mass education and administrative control.
% TRANSFER_FUNCTION: Moves cultural capital, institutional access, and symbolic legitimacy from Ottoman-Arabic literate elites and religious scholars to the republican state and its secular urban intelligentsia, by devaluing the old script and criminalizing its public use.
% ABSENT_VOICES: Ottomanist intellectuals, Arabic-script religious scholars, and advocates of a gradual dual-script transition were structurally excluded from the 1928 Language Commission and subsequent state language councils; their exclusion was enforced by the state's monopoly on deliberative and punitive authority.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, the official text of the Republic would revert to a multi-script field, the intergenerational barrier to Ottoman textual heritage would begin to close, state education would lose its primary homogenizing instrument, and the ideological tie between Turkishness and European modernity would loosen; the republican symbolic order would require renegotiation.
% FOUNDING_PROBLEM: The Ottoman Empire's diglossiaâhigh Ottoman written in Arabic script diverging from vernacular Turkishâand its multi-ethnic, multi-religious composition were interpreted as structural barriers to forging a unified, governable Turkish nation-state capable of reaching European levels of development.
% FOUNDING_PROBLEM_CORROBORATION: Kemalist historiography and the Turkish Language Corporation attest the problem was urgent and existential. Ottomanist historians and linguists outside the Republican tradition argue the diglossia was manageable and the reform was primarily ideological; European linguists of the interwar period noted the abruptness but were not consulted in the decision. No independent corroboration from non-benefiting parties confirms that the problem required this specific rupture rather than a managed transition.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__secular_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__secular_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__secular_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores high on extractiveness (0.78) because it forcibly devalued existing literacy, imposed relearning costs, and severed intergenerational textual continuity. Suppression is high (0.65) because the monopoly persists through active legal prohibition of Arabic script, not mere convention. Theater ratio (0.40) reflects that while the original nation-building function was genuine, a substantial share of current enforcement is ideological maintenance rather than ongoing coordination. Accessibility collapse (0.88) is extreme: once the reform was implemented, Ottoman texts became effectively inaccessible without specialized study. Resistance (0.55) is moderate: Kurdish linguistic movements and Islamist political formations have mounted recurring challenges, but the state has absorbed or suppressed them. The temporal series show an inverted-U in theater (peaking at mid-century normalization) and a recent re-intensification of extraction as neo-Ottoman identity politics makes the intergenerational rupture politically salient again.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (republican state) experiences the constraint as necessary nation-building coordination that prevented Balkan-style fragmentation. The payer seats (Ottoman-literate elite, religious scholars, rural population) experience the same structure as targeted cultural dispossession. The urban secular intelligentsia sits near the beneficiary end, though some members experience ambivalence as the 'purified' language cuts them off from their own grandparents' writings. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate between these experiences but notes the coexistence of genuine coordination and asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The republican state apparatus and urban secular intelligentsia are declared beneficiaries: they collect territorial homogenization, institutional control, and cultural authority from the Latin-script monopoly. The Ottoman-literate elite, religious scholars, and Anatolian rural population are declared victims (payers): they bear the costs of relearning, devalued cultural capital, and severed textual traditions. Arabic-script advocates are excluded, not victims of direct extraction but prevented from altering the constraint. The beneficiary/payer split maps cleanly onto the directionality derivation: the state and intelligentsia receive low d (subsidized by the constraint), while the old elite and rural population receive high d (targets of extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâimperial diglossia and nation-state consolidationâwas at least partially genuine, which prevents classification as a pure snare. However, the solution chosen (abrupt, legally enforced script rupture) imposed asymmetric costs on non-benefiting populations and structurally excluded less disruptive alternatives. The Tangled Rope classification captures this dual character: the constraint coordinates a real collective-action problem (standardizing a national language) while simultaneously extracting from identifiable parties. A Snare reading would be incorrect because the coordination function is not merely cover; a Rope reading would be incorrect because the extraction is not incidental or symmetrically distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the secular_nationalist_reading of kernel turkish_graphemic_substrate. Does the engine treat it as one of multiple structurally distinct claims sharing a colloquial label?',
    'Corpus-level decomposition audit: verify that sibling constraints ottoman_continuity_reading and gradual_transition_reading are authored with different epsilon values and different beneficiary/victim structures.',
    'If siblings collapse to the same constraint, the decomposition fails and the epsilon-invariance principle is violated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Verification of kernel reading decomposition integrity').

omega_variable(
    script_efficiency_empirical_status,
    'Does the Latin script actually improve literacy acquisition and phonetic transparency for Turkish net of transition costs, or is its superiority a post-hoc rationalization for a politically motivated rupture?',
    'Controlled historical comparison with Azerbaijan''s Soviet-era script changes, or cross-script literacy acquisition studies among Turkish-speaking populations using Arabic vs. Latin orthographies.',
    'A null or negative efficiency finding would shift the coordination function toward the theatrical/extractive end, suggesting the reform''s stated rationale was cover for cultural engineering.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(script_efficiency_empirical_status, empirical, 'Whether phonetic efficiency claims are empirically grounded').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the persistence of the Latin-script monopoly maintained primarily by active legal enforcement (structural suppression) or by generational normalization that makes Arabic-script literacy socially unthinkable (internalized suppression)?',
    'Measure Arabic-script literacy rates and public attitudes toward Ottoman heritage across age cohorts; observe whether relaxation of legal penalties leads to spontaneous script revival or requires active state subsidy.',
    'If suppression is largely internalized, the constraint''s effective extraction is higher than the legal measure suggestsâthe population carries the constraint even after formal enforcement eases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__secular_nationalist_reading, 0, 95).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tgss_snr_tr_t0, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tgss_snr_tr_t10, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(tgss_snr_tr_t25, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(tgss_snr_tr_t50, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement(tgss_snr_tr_t75, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 75, 0.45).
narrative_ontology:measurement(tgss_snr_tr_t95, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 95, 0.4).

% Extraction over time
narrative_ontology:measurement(tgss_snr_be_t0, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(tgss_snr_be_t10, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 10, 0.85).
narrative_ontology:measurement(tgss_snr_be_t25, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(tgss_snr_be_t50, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(tgss_snr_be_t75, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 75, 0.65).
narrative_ontology:measurement(tgss_snr_be_t95, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 95, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(tgss_snr_su_t0, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(tgss_snr_su_t10, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 10, 0.9).
narrative_ontology:measurement(tgss_snr_su_t25, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(tgss_snr_su_t50, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement(tgss_snr_su_t75, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 75, 0.55).
narrative_ontology:measurement(tgss_snr_su_t95, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 95, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__secular_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, gradual_transition_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Turkish graphemic substrate' conflates three structurally distinct constraints. This reading models the state-imposed Latin-script monopoly and its extractive operation; the sibling readings model the Arabic-script continuity claim and the dual-script counterfactual. Their epsilon values, beneficiary structures, and failure modes differ. They form a constraint family linked by mutual affectation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
