% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Native Generative Hebrew as Sole Criterion of Language Life
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures the 'native_generative' reading of the
 *   contested kernel 'hebrew_continuity'. The reading asserts that Hebrew
 *   lives ONLY through native speaker intuition and daily generative use —
 *   requiring native child speakers, lexical expansion for modern domains,
 *   and phonological standardization. This reading was institutionalized
 *   through the Hebrew Language Academy, the Israeli education system, and
 *   the revivalist project from the 1880s onward. It coordinates a unified
 *   national language but extracts from liturgical-only communities (Haredi,
 *   traditional diaspora) by deeming their Hebrew 'dead' and excluding them
 *   from legitimate continuity. The constraint is actively enforced through
 *   education policy, state funding criteria, and academic linguistics. The
 *   claimed type is tangled_rope: genuine coordination (modern Hebrew as a
 *   living national language) combined with asymmetric extraction (liturgical
 *   communities marginalized).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.68).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.62).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Native Generative Hebrew as Sole Criterion of Language Life").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '44539bc6-7fe4-4317-a152-5f7f256d83cd').
narrative_ontology:cs_kernel_codification('44539bc6-7fe4-4317-a152-5f7f256d83cd', formalized).
narrative_ontology:cs_authority_grounding('44539bc6-7fe4-4317-a152-5f7f256d83cd', practice).
narrative_ontology:cs_interpretation_layer_present('44539bc6-7fe4-4317-a152-5f7f256d83cd').
narrative_ontology:cs_reading_relation('44539bc6-7fe4-4317-a152-5f7f256d83cd', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('44539bc6-7fe4-4317-a152-5f7f256d83cd', hebrew_continuity__bridge_pidginized, forecloses).
narrative_ontology:cs_axiom('44539bc6-7fe4-4317-a152-5f7f256d83cd', foundational, native_generative_use_necessary_for_living_language).
narrative_ontology:cs_axiom_status(native_generative_use_necessary_for_living_language, holdable).
narrative_ontology:cs_axiom_grounding('44539bc6-7fe4-4317-a152-5f7f256d83cd', native_generative_use_necessary_for_living_language, empirically_contingent).
narrative_ontology:cs_axiom('44539bc6-7fe4-4317-a152-5f7f256d83cd', secondary, liturgical_preservation_insufficient_for_continuity).
narrative_ontology:cs_axiom_status(liturgical_preservation_insufficient_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('44539bc6-7fe4-4317-a152-5f7f256d83cd', liturgical_preservation_insufficient_for_continuity, empirically_contingent).
narrative_ontology:cs_reference_frame('44539bc6-7fe4-4317-a152-5f7f256d83cd', native_revival_frame).
narrative_ontology:cs_drift_state('44539bc6-7fe4-4317-a152-5f7f256d83cd', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('44539bc6-7fe4-4317-a152-5f7f256d83cd', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, native_hebrew_speakers).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_education_system).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_hebrew_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_traditionalist_groups).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, native_speaker_criterion_for_language_vitality).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, lexical_expansion_as_continuity_marker).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the official standard for modern Hebrew: coins neologisms, prescribes grammar, authorizes curricula. Its authority derives from the native-generative reading — it is the guardian of the 'living language'. Collects institutional prestige and state funding. Can exit by redefining its mandate, but its identity is fused with the native-generative project.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, hebrew_language_academy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, hebrew_language_academy, beneficiary).

% Israeli Jews who acquired Hebrew as L1. They benefit from a fully functional national language in all domains. They bear maintenance costs (academy prescriptions, purism pressures) but cannot exit the language without leaving the polity. Their generative intuition is the constraint's validation mechanism.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, native_hebrew_speakers, beneficiary,
    organized, biographical, constrained, national).

% Implements the native-generative standard through compulsory Hebrew education. Receives state funding tied to modern Hebrew outcomes. Marginalizes liturgical Hebrew tracks (separate religious streams exist but are deprecated as 'not the living language'). Could reform but institutional inertia and nationalist identity lock it in.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_education_system, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, israeli_education_system, beneficiary).

% Haredi and traditional diaspora communities for whom Hebrew is primarily the language of prayer, study, and textual tradition. They are told their Hebrew is 'dead' because it lacks native child speakers in generative use. They bear status denial, exclusion from national language resources, and pressure to adopt modern Hebrew norms. Exit would require abandoning their religious-linguistic identity — the constraint fuses their Hebrew with 'non-life'.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, liturgical_hebrew_communities, payer,
    organized, generational, identity_locked, global).

% Non-Haredi traditional communities (e.g., Modern Orthodox, Conservative/Masorti) that maintain liturgical Hebrew alongside vernaculars. They are caught between the native-generative standard (which they partially adopt in Israel) and their own liturgical continuity. They pay through curricular dual-track costs and delegitimization of their Hebrew practices.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_traditionalist_groups, payer,
    moderate, generational, constrained, global).

% Scholars of language revitalization, contact linguistics, and commitment systems. They analyze the constraint from outside, documenting the seat divergence. Their work feeds back into the constraint when cited by the Academy or liturgical advocates.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, sociolinguistic_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__native_generative, hebrew_language_academy).
narrative_ontology:fixing_cost_class(hebrew_continuity__native_generative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a single, standardized, natively spoken Hebrew that functions across all modern domains (science, law, technology, daily life) — solving the fragmentation of diaspora Hebrew varieties and the gap between liturgical and spoken registers.
% TRANSFER_FUNCTION: Moves institutional authority, state funding, educational resources, and legitimacy from liturgical/traditional Hebrew communities to the modern Hebrew revival apparatus (Academy, schools, media). The transfer is justified as 'making Hebrew live' but operates by defining the victims' Hebrew as dead.
% ABSENT_VOICES: Pre-revival Hebrew speakers (historical), Sephardic/Mizrahi communities whose liturgical pronunciations were overridden by the Ashkenazi-based standard, Palestinian Arabic speakers whose contact Hebrew (bridge_pidginized) was erased. They are absent because the constraint's timeline begins with the revival's victory.
% DISAPPEARANCE_RATIONALE: If the native-generative standard vanished overnight, the Academy would lose its mandate, education would fragment into competing Hebrew norms, liturgical communities would claim equal continuity status, and the unified national language would dissolve into a pluricentric or diglossic system. The Israeli polity's linguistic coherence depends on this constraint.
% FOUNDING_PROBLEM: The Jewish people lacked a shared spoken language for national self-determination. Diaspora Hebrew was confined to liturgy and study; spoken varieties were Yiddish, Ladino, Judeo-Arabic, etc. A living national language was needed for sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Revivalists (Ben-Yehuda, Academy founders) attest the problem was live and solved. Liturgical communities attest the problem was misdiagnosed: Hebrew never died, it lived in ritual and text. Historians of nationalism (Hobsbawm, Anderson) corroborate that a spoken vernacular was a standard nation-building requirement — but note that other nations achieved this without declaring their liturgical languages dead.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the structural marginalization of liturgical Hebrew: state resources flow to modern Hebrew institutions, liturgical communities are denied recognition as 'living Hebrew' bearers, and their linguistic practices are treated as fossilized. Suppression (0.62) is significant but not total: liturgical communities persist and maintain their Hebrew, but they do so under a regime that officially defines their variety as non-living. Theater ratio (0.28) is moderate: the Academy's prescriptive work has real communicative function, but a growing share of its activity polices boundaries against 'non-native' influences (including liturgical syntax). Accessibility collapse (0.55) and resistance (0.48) reflect that alternatives (liturgical, pidginized) remain viable for their communities but are excluded from the official continuity narrative.
 *
 * PERSPECTIVAL GAP:
 *   From the Academy's seat, the constraint is a rope: it coordinates a miraculous revival. From liturgical communities' seat, it is a snare: their Hebrew is declared dead by fiat. The engine will compute this divergence from the structural data. The native_generative reading's exclusive 'only' claim creates the fork: it cannot accommodate liturgical continuity as 'living' without abandoning its core premise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: native_hebrew_speakers (gain a living national language, economic mobility), hebrew_language_academy (institutional authority, prestige), israeli_education_system (coherent curriculum). Victims: liturgical_hebrew_communities (bear status denial, curricular exclusion, delegitimization), diaspora_traditionalist_groups (lose continuity claim). The agenda_setter seats (Academy, state) derive directionality near 0.0 (subsidized by the constraint). Native speakers sit near 0.3 (beneficiaries with maintenance costs). Liturgical communities sit near 0.85 (targets with constrained exit — they cannot become native generative communities without abandoning their religious framework).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating a spoken national language for a stateless people) was live in 1880-1948. By 1948 it was substantially solved. The constraint persists with increasing extraction (theater rising, suppression stable) because the institutional apparatus (Academy, education) now extracts legitimacy from policing the boundary. The mandatrophy is unresolved: the arrangement continues as if the founding problem were still live, but the extraction now serves institutional self-preservation more than the original coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Does the native_generative reading foreclose the liturgical_preservation and bridge_pidginized readings within a single commitment framework, or do they coexist as competing legitimacy claims?',
    'Analyze whether any institutional or communal framework simultaneously treats liturgical Hebrew as ''living'' while maintaining native generative use as the exclusive criterion. If such frameworks exist (e.g., some Modern Orthodox communities), the foreclosure claim fails.',
    'If foreclosure holds, the constraint is a stronger tangled_rope with active suppression of alternative readings. If coexistence holds, the extraction is less structural and more discursive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the exclusive native-speaker criterion logically eliminates rival readings of Hebrew continuity.').

omega_variable(
    extraction_mechanism_ambiguity,
    'Is the extraction from liturgical communities primarily symbolic (status denial) or material (resource allocation, educational access)?',
    'Trace funding flows for Hebrew language education, religious school curricula, and state recognition of religious courts. If liturgical communities lose tangible resources due to the native-generative standard, extraction is material.',
    'Material extraction raises effective χ for victim seats and strengthens snare/tangled_rope classification. Purely symbolic extraction may still be coercive but operates through different mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_ambiguity, empirical, 'Nature of the cost imposed on liturgical-only Hebrew communities by the native-generative standard.').

omega_variable(
    coordination_function_genuineness,
    'Does the native-generative standard solve a genuine coordination problem (unified national communication, technological adaptation) or is the coordination story cover for nationalist extraction?',
    'Compare Hebrew''s lexical expansion and standardization outcomes with other revived languages (e.g., Māori, Welsh) where native generative use coexists with liturgical/traditional registers. If Hebrew''s standardization uniquely suppresses alternatives, the coordination claim is suspect.',
    'If coordination is genuine, the constraint remains tangled_rope. If cover, it drifts toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_genuineness, conceptual, 'Whether the coordination function of native-generative Hebrew is structurally necessary or a legitimating narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 145).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_native_generative_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebrew_native_generative_tr_t30, hebrew_continuity__native_generative, theater_ratio, 30, 0.15).
narrative_ontology:measurement(hebrew_native_generative_tr_t60, hebrew_continuity__native_generative, theater_ratio, 60, 0.2).
narrative_ontology:measurement(hebrew_native_generative_tr_t90, hebrew_continuity__native_generative, theater_ratio, 90, 0.25).
narrative_ontology:measurement(hebrew_native_generative_tr_t120, hebrew_continuity__native_generative, theater_ratio, 120, 0.28).
narrative_ontology:measurement(hebrew_native_generative_tr_t145, hebrew_continuity__native_generative, theater_ratio, 145, 0.28).

% Extraction over time
narrative_ontology:measurement(hebrew_native_generative_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hebrew_native_generative_be_t30, hebrew_continuity__native_generative, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(hebrew_native_generative_be_t60, hebrew_continuity__native_generative, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(hebrew_native_generative_be_t90, hebrew_continuity__native_generative, base_extractiveness, 90, 0.62).
narrative_ontology:measurement(hebrew_native_generative_be_t120, hebrew_continuity__native_generative, base_extractiveness, 120, 0.68).
narrative_ontology:measurement(hebrew_native_generative_be_t145, hebrew_continuity__native_generative, base_extractiveness, 145, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_native_generative_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hebrew_native_generative_su_t30, hebrew_continuity__native_generative, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(hebrew_native_generative_su_t60, hebrew_continuity__native_generative, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(hebrew_native_generative_su_t90, hebrew_continuity__native_generative, suppression_requirement, 90, 0.62).
narrative_ontology:measurement(hebrew_native_generative_su_t120, hebrew_continuity__native_generative, suppression_requirement, 120, 0.62).
narrative_ontology:measurement(hebrew_native_generative_su_t145, hebrew_continuity__native_generative, suppression_requirement, 145, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__native_generative, 0.08).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the hebrew_continuity kernel. The native_generative reading has the highest empirical confirmation (native speaker community exists) but also the highest extraction (liturgical communities deemed dead). The liturgical_preservation reading has near-zero extraction but contested coordination (no native speakers). The bridge_pidginized reading sits between. Together they form a constraint family where the native_generative reading's institutional dominance creates downstream pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__native_generative, institutional, 0.1).
constraint_indexing:directionality_override(hebrew_continuity__native_generative, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
