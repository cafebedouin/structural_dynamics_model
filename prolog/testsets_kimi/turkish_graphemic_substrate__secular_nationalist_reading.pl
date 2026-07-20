% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__secular_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: turkish_graphemic_substrate__secular_nationalist_reading
 *   human_readable: Turkish Latin Script Reform â Secular Nationalist Reading
 *   domain: political/linguistic/state_formation
 *
 * SUMMARY:
 *   In 1928, the Turkish Republic replaced the Ottoman Turkish alphabet
 *   (Arabic script) with a Latin-based Turkish alphabet, accompanied by state
 *   language purification and a narrative that Turkish identity is distinct
 *   from its Ottoman-Islamic past. This constraint operates through
 *   compulsory education, press laws, state ceremony, and military
 *   intervention to enforce the Latin script as the sole legitimate graphemic
 *   substrate and to align the nation with European modernity. It functions
 *   as a tangled rope: it genuinely coordinates a modern nation-state, mass
 *   literacy, and a unified public sphere, while simultaneously extracting
 *   cultural capital from Ottoman literati, suppressing religious and
 *   non-Turkish identities, and homogenizing national territory under state
 *   power. This JSON instantiates ONLY the secular_nationalist_reading of the
 *   turkish_graphemic_substrate kernel.
 *
 * KEY AGENTS:
 *   - secular_nationalist_state: Primary agenda setter (institutional/mobile) â enforces the script reform and identity narrative.
 *   - urban_secular_elites: Primary beneficiary (powerful/mobile) â accumulate cultural capital under the new order.
 *   - ottoman_literate_class: Primary target (moderate/identity_locked) â bear the destruction of Arabic-script cultural capital.
 *   - religious_conservatives: Secondary target (organized/identity_locked) â bear the severance from Islamic textual tradition.
 *   - kurdish_minority_communities: Territorial homogenization target (powerless/trapped) â bear suppression of minority language and script traditions.
 *   - linguistic_historians: Analytical observer (analytical/analytical) â documents the engineered nature of the shift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, 0.7).
domain_priors:suppression_score(turkish_graphemic_substrate__secular_nationalist_reading, 0.75).
domain_priors:theater_ratio(turkish_graphemic_substrate__secular_nationalist_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__secular_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__secular_nationalist_reading, "Turkish Latin Script Reform â Secular Nationalist Reading").
narrative_ontology:topic_domain(turkish_graphemic_substrate__secular_nationalist_reading, "political/linguistic/state_formation").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__secular_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__secular_nationalist_reading, '3fc5febe-39da-43db-b1e0-bec31f61c1a2').
narrative_ontology:cs_kernel_codification('3fc5febe-39da-43db-b1e0-bec31f61c1a2', fixed_text).
narrative_ontology:cs_authority_grounding('3fc5febe-39da-43db-b1e0-bec31f61c1a2', lineage).
narrative_ontology:cs_interpretation_layer_present('3fc5febe-39da-43db-b1e0-bec31f61c1a2').
narrative_ontology:cs_reading_relation('3fc5febe-39da-43db-b1e0-bec31f61c1a2', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('3fc5febe-39da-43db-b1e0-bec31f61c1a2', turkish_graphemic_substrate__gradual_transition_reading, influences).
narrative_ontology:cs_axiom('3fc5febe-39da-43db-b1e0-bec31f61c1a2', foundational, latin_script_as_modernity_substrate).
narrative_ontology:cs_axiom_status(latin_script_as_modernity_substrate, holdable).
narrative_ontology:cs_axiom_grounding('3fc5febe-39da-43db-b1e0-bec31f61c1a2', latin_script_as_modernity_substrate, instrumental).
narrative_ontology:cs_axiom('3fc5febe-39da-43db-b1e0-bec31f61c1a2', foundational, state_mandated_identity_rupture).
narrative_ontology:cs_axiom_status(state_mandated_identity_rupture, holdable).
narrative_ontology:cs_axiom_grounding('3fc5febe-39da-43db-b1e0-bec31f61c1a2', state_mandated_identity_rupture, conventional).
narrative_ontology:cs_reference_frame('3fc5febe-39da-43db-b1e0-bec31f61c1a2', kemalist_foundational_moment).
narrative_ontology:cs_drift_state('3fc5febe-39da-43db-b1e0-bec31f61c1a2', contemporary_multiparty_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3fc5febe-39da-43db-b1e0-bec31f61c1a2', '').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, secular_nationalist_state).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, urban_secular_elites).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, republican_educators).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, religious_conservatives).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_minority_communities).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, secular_nationalist_state_identity).
narrative_ontology:constraint_vindicates(turkish_graphemic_substrate__secular_nationalist_reading, european_modernity_alignment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces the 1928 alphabet reform, language purification laws, and education curriculum that mandates Latin-script Turkish as the sole legitimate writing system. Consolidates territorial control and European-alignment legitimacy through homogenization of public culture and suppression of Ottoman-Arabic script in official and educational contexts.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, secular_nationalist_state, agenda_setter,
    institutional, generational, mobile, national).

% Accumulate cultural and social capital as the primary bearers of the new republican modernity. Their education, professional networks, and social status are aligned with the Latin-script order and European-facing identity, giving them privileged access to state institutions and cosmopolitan circuits.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, urban_secular_elites, beneficiary,
    powerful, biographical, mobile, national).

% Employed by the state to implement the script reform and language purification in schools. Their professional identity, income, and social role depend on transmitting the new script and the secular nationalist narrative; dissent risks exclusion from the profession.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, republican_educators, beneficiary,
    organized, biographical, constrained, national).

% Held cultural authority through Arabic-script literacy, poetry, jurisprudence, and bureaucratic expertise. The 1928 reform annihilated the value of this capital overnight, reducing them to a silenced generation whose skills were delegitimized and excluded from public life.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_literate_class, payer,
    moderate, biographical, identity_locked, national).

% Experience the Latin-script mandate as severance from Islamic textual tradition, prayer manuals, and Ottoman religious scholarship. Their communal memory and liturgical language remain tied to Arabic script, forcing a choice between underground retention and public invisibility.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, religious_conservatives, payer,
    organized, generational, identity_locked, national).

% Subject to territorial homogenization that suppresses Kurdish linguistic and script traditions under the umbrella of a single Turkish national language in Latin script. Geographic concentration offers no exit because the state extends the same curriculum and legal script monopoly into all provinces.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, kurdish_minority_communities, payer,
    powerless, generational, trapped, regional).

% Study the reform as a case of state-directed language engineering, comparing it to other 20th-century graphemic shifts. They document the cost-benefit distribution across classes and the persistence of Ottoman memory despite state suppression.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a unified national language and literate citizenry for a post-imperial republic, replacing a multi-ethnic, multi-script imperial order with a standardized, state-controlled vernacular and writing system aligned with Western statehood norms.
% TRANSFER_FUNCTION: Moves cultural legitimacy and state recognition from Ottoman-Arabic script and multilingual heritage to a state-monopolized Turkish Latin script; transfers cultural capital from Ottoman literati to secular republican cadres; transfers identity compliance from minorities to the homogenizing state.
% ABSENT_VOICES: Arabic-script literate populations outside the early Republic's urban centers, non-Turkish linguistic communities whose languages were excluded from the new script standard, and Ottomanist intellectuals who would argue for continuity and syncretism are structurally absent from the state archive and public education system.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, the education system would need to accommodate Arabic-script resurgence, state identity narratives would require rewriting, the cultural capital of republican elites would depreciate, and the homogenization of national territory would lose a primary graphemic lever â the political-linguistic field would reorganize around competing Ottoman, Turkish, and Kurdish scripts.
% FOUNDING_PROBLEM: The collapse of the Ottoman Empire left a multi-ethnic, multi-script society with low mass literacy and no unified national language capable of supporting a modern nation-state; the new republic needed to consolidate Anatolian territory and align with Western civilization to ensure survival and recognition.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians of Turkey and critical sociologists outside the secular state apparatus attest that the existential crisis of imperial collapse was resolved by mid-century. Kurdish political representatives and Islamic conservative movements corroborate that the homogenization imperative persists, but they frame it as ongoing oppression rather than an unresolved founding problem.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__secular_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__secular_nationalist_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__secular_nationalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__secular_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 0.7, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.70) is high because the constraint annihilates the value of pre-reform literacy, marginalizes competing identity claims, and channels cultural capital to republican cadres. Suppression (0.75) is high because the state bans Arabic script in public, suppresses dissenting education, and intervenes militarily to defend Kemalist linguistic norms. Theater ratio (0.50) is moderate-to-high: early literacy gains were real, but by mid-century a growing share of state language activity is ritualized performance (language festivals, statuary, oath ceremonies) that substitutes for substantive pluralism. Accessibility collapse (0.75) is high because within one generation the alternative script became practically inaccessible to youth. Resistance (0.60) reflects persistent underground retention of Ottoman/Islamic memory, Kurdish linguistic persistence, and periodic electoral pushback. Temporal series share a single grid (0â50) to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   From the state and urban elites, the constraint is experienced as necessary modernization and national salvation; from Ottoman literati, religious conservatives, and Kurdish communities, the same structure is experienced as coerced erasure and homogenization. The divergence is not a perceptual error but a structural asymmetry in who bears costs and who captures benefits.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular nationalist state and urban elites sit near the beneficiary end: the constraint subsidizes their cultural authority and European alignment. Republican educators are constrained beneficiaries â they depend on the constraint for employment but lack agenda-setting power. Ottoman literati and religious conservatives sit near the full-target end: their identity is fused to the suppressed script, locking exit and amplifying effective extraction. Kurdish minority communities are trapped at the target end by territorial scope and powerlessness. The engine computes per-seat divergence from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâimperial collapse and the urgency of territorial consolidationâwas substantially resolved by the mid-20th century, yet the constraint persisted and in some dimensions intensified. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges signals mandatrophy: the script reform outlived its original survival mandate and became a permanent instrument of identity enforcement and minority homogenization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'If the ottoman_continuity_reading or gradual_transition_reading were adopted, which structural elementsâbeneficiary sets, victim sets, or enforcement intensityâwould change, and by how much?',
    'Comparative analysis of the compiled sibling constraints in the kernel family; empirical study of jurisdictions where script transitions were gradual or where Arabic-script literacy was preserved.',
    'If a sibling reading would produce materially different directionalities or lower suppression, the current reading''s high extraction is confirmed as reading-specific rather than kernel-intrinsic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Structural delta between this reading and its kernel siblings').

omega_variable(
    coordination_extraction_separability,
    'Can the literacy-coordination and nation-building function of the reform be separated from the identity-erasure and homogenization extraction, or are they structurally fused in this reading?',
    'Counterfactual analysis: would a script reform that retained Arabic-script education alongside Latin introduction have achieved comparable literacy and state coordination without the same victim structure?',
    'If separable, a substantial portion of measured extraction is contingent on the secular nationalist reading''s specific axioms rather than inherent to graphemic modernization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction are structurally separable in this reading').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (state laws, school curricula, military intervention) or internalized (generational shame about Ottoman heritage, self-censorship of Arabic-script knowledge)?',
    'Post-exit suppression trajectory: if suppression of Arabic-script use persists among diaspora communities after structural state enforcement is removed, reclassify as partially internalized.',
    'If internalized, effective suppression exceeds the structural measureâtargets carry the constraint with them across borders and generations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    homogenization_scope_boundary,
    'Does the constraint''s extraction fall primarily on Ottoman-Islamic identity or on non-Turkish minorities, and can these victim categories be analytically separated?',
    'Disaggregated historical analysis of enforcement intensity and educational resource allocation across regions and communities.',
    'If the two victim categories are inseparable in practice, the constraint''s scope is wider than its public script narrative suggests, affecting classification symmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homogenization_scope_boundary, empirical, 'Boundary between Ottoman-Islamic and minority-language victim sets').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__secular_nationalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t0, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(turk_tr_t10, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(turk_tr_t20, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(turk_tr_t30, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(turk_tr_t40, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(turk_tr_t50, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(turk_be_t0, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(turk_be_t10, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(turk_be_t20, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(turk_be_t30, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(turk_be_t40, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(turk_be_t50, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 50, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t0, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(turk_su_t10, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(turk_su_t20, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(turk_su_t30, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(turk_su_t40, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(turk_su_t50, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__secular_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_continuity_reading).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, gradual_transition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the turkish_graphemic_substrate kernel, decomposed per the Îµ-invariance principle because the kernel's epsilon and directionalities vary by reading. The secular_nationalist_reading carries high extractiveness and active enforcement; sibling readings instantiate different structural profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
