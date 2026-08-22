% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__literary_continuity_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Literary Productivity as Living-Language Criterion (Haskalah Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested 'living language'
 *   kernel: the literary-continuity reading, under which Hebrew's status as a
 *   living language is established by the productive output of Haskalah
 *   periodicals and modern secular Hebrew literature, independent of whether
 *   anyone acquires it as a mother tongue. This is the maskilim's own
 *   criterion, and it is authored here as a low-extraction, largely
 *   non-coercive coordination mechanism among a literate intelligentsia: it
 *   lets Hebrew-writing intellectuals build a cumulative, citable literary
 *   tradition and claim cultural modernity for the language. The cost it
 *   imposes is definitional rather than material — it is not a coercive
 *   apparatus, but it does structurally exclude the linguistic lives of the
 *   Yiddish-speaking masses and liturgically-oriented traditional communities
 *   from counting as evidence of vitality. Per Rule 1, this story does not
 *   describe or average over the sibling readings
 *   (liturgical_preservation_reading, native_generation_reading) — those are
 *   separate constraints with their own ε and stakeholder structures, linked
 *   here only via cs_structure.reading_relations and omegas.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.28).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.22).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Literary Productivity as Living-Language Criterion (Haskalah Reading)").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistics/religious_studies/nationalism_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, 'ec6312dc-83b3-4ee4-8dbe-e0c11693630b').
narrative_ontology:cs_kernel_codification('ec6312dc-83b3-4ee4-8dbe-e0c11693630b', distributed).
narrative_ontology:cs_authority_grounding('ec6312dc-83b3-4ee4-8dbe-e0c11693630b', practice).
narrative_ontology:cs_interpretation_layer_present('ec6312dc-83b3-4ee4-8dbe-e0c11693630b').
narrative_ontology:cs_reading_relation('ec6312dc-83b3-4ee4-8dbe-e0c11693630b', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec6312dc-83b3-4ee4-8dbe-e0c11693630b', living_language_status__native_generation_reading, influences).
narrative_ontology:cs_axiom('ec6312dc-83b3-4ee4-8dbe-e0c11693630b', foundational, productive_authorship_constitutes_vitality).
narrative_ontology:cs_axiom_status(productive_authorship_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('ec6312dc-83b3-4ee4-8dbe-e0c11693630b', productive_authorship_constitutes_vitality, conventional).
narrative_ontology:cs_axiom('ec6312dc-83b3-4ee4-8dbe-e0c11693630b', foundational, vitality_independent_of_native_acquisition).
narrative_ontology:cs_axiom_status(vitality_independent_of_native_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('ec6312dc-83b3-4ee4-8dbe-e0c11693630b', vitality_independent_of_native_acquisition, instrumental).
narrative_ontology:cs_reference_frame('ec6312dc-83b3-4ee4-8dbe-e0c11693630b', maskilic_literary_productivity_standard).
narrative_ontology:cs_drift_state('ec6312dc-83b3-4ee4-8dbe-e0c11693630b', post_zionist_revival_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ec6312dc-83b3-4ee4-8dbe-e0c11693630b', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim_literary_intellectuals).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, haskalah_periodical_editors).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, secular_hebrew_writers).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_yiddish_speaking_masses).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, non_literary_traditional_communities).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, hebrew_as_modern_literary_vehicle).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, vitality_independent_of_native_acquisition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write, edit, and circulate Hebrew periodicals and belletristic prose among a literate Jewish intelligentsia in Central and Eastern Europe. They set the criterion for what counts as linguistic vitality — productive authorship of new work — because that is precisely the capacity they possess and exercise. Their cultural authority as arbiters of Hebrew's modernity does not require that anyone speak the language at home.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim_literary_intellectuals, agenda_setter,
    organized, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, maskilim_literary_intellectuals, beneficiary).

% Run journals like Ha-Me'assef that publish essays, poetry, and criticism in revived literary Hebrew. Their institutional standing and readership depend on Hebrew being recognized as a living vehicle for contemporary thought, not merely a liturgical relic. They can move between languages and publishing venues if Hebrew's status claim fails to hold.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, haskalah_periodical_editors, beneficiary,
    moderate, biographical, mobile, regional).

% Compose original poetry, fiction, and scientific popularization in Hebrew for an educated readership. The literary-continuity definition confers on them the status of participants in a living tradition rather than antiquarians reviving a dead tongue — a status that attaches to their authorship, independent of whether children grow up speaking Hebrew.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, secular_hebrew_writers, beneficiary,
    moderate, generational, mobile, continental).

% Speak Yiddish as a genuine mother tongue and cannot read the Hebrew periodicals that are held up as proof of Hebrew's vitality. Under this criterion their actual daily linguistic life counts for nothing in the vitality determination; the language they live in is treated as beneath consideration while a language almost none of them can read is declared alive on their supposed behalf.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_yiddish_speaking_masses, payer,
    powerless, biographical, trapped, regional).

% Use Hebrew liturgically — prayer, study of sacred texts, occasional correspondence — without producing new secular literary or intellectual work in it. The literary-continuity criterion structurally excludes their mode of engagement from counting as evidence of vitality, even though it is the mode through which the overwhelming majority of Hebrew-using Jews actually relate to the language.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, non_literary_traditional_communities, payer,
    powerless, generational, constrained, regional).

% Argue that Yiddish, not Hebrew, is the true living vernacular of the Jewish masses and that the maskilim's Hebrew-literary criterion is an elite imposition serving a small intelligentsia's cultural prestige. They are not consulted in how vitality gets defined by the literary-continuity reading, though they have a competing claim about which language is actually alive.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, yiddishist_cultural_movement, excluded,
    organized, generational, constrained, regional).

% Later draw on Haskalah literary Hebrew as raw material for revernacularization, citing the periodicals' existence as evidence that Hebrew never fully died. They observe and later inherit the literary-continuity claim without having been party to its original elite coordination.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, future_zionist_language_planners, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed literate intelligentsia around a shared standard of what counts as evidence that Hebrew remains a living, modern intellectual medium — enabling them to publish, cite, and build on one another's work as participants in a continuous tradition rather than as antiquarians.
% TRANSFER_FUNCTION: Moves cultural authority and the status of 'living tradition' to those who can write and read new literary Hebrew, away from those whose only relationship to the language is liturgical or absent entirely; no material resources are transferred, but recognition, legitimacy, and the right to speak for 'the language' are.
% ABSENT_VOICES: Yiddish-speaking masses and non-literary traditional communities have no seat in defining vitality by this criterion; the Yiddishist cultural movement actively contests the criterion but is excluded from the maskilic literary sphere's self-definition of what counts as evidence.
% DISAPPEARANCE_RATIONALE: If the literary-continuity criterion vanished, the maskilim's claim to cultural authority over 'living Hebrew' would lose its evidentiary basis and would need to be re-argued on other grounds (e.g., liturgical continuity or eventual native acquisition) — a real rearrangement for the intelligentsia's status claims. For the Yiddish-speaking masses and liturgical communities, whose daily linguistic life does not depend on this definitional victory either way, little would change, which is why the verdict is contested rather than uniform across seats.
% FOUNDING_PROBLEM: Enlightenment-era Jewish intellectuals needed to establish that Hebrew could serve as a vehicle for modern secular thought — science, philosophy, poetry, journalism — to counter both the view that Hebrew was a dead liturgical language and the pull toward full linguistic assimilation into European vernaculars.
% FOUNDING_PROBLEM_CORROBORATION: Later Zionist language revivalists (outside the original Haskalah circle) corroborate that the literary corpus mattered as a resource for revernacularization, treating the founding problem as substantially resolved once modern spoken Hebrew emerged. Yiddishist historians and linguists, also outside the maskilic beneficiary group, dispute that literary productivity was ever the right test of vitality, holding the founding problem was misconceived from the start rather than solved.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, contested).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).
:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at interval end) because the mechanism is elite self-coordination around a publication and citation practice, not an apparatus that extracts material resources or livelihoods from the excluded groups — the cost to non-literary speakers is exclusion from a status category, not economic or physical harm. Suppression is correspondingly low (0.22): no one is coerced into accepting the literary-continuity definition, and rival definitions (liturgical, native-vernacular) circulate freely in the same period. Theater ratio stays low throughout (0.08 to 0.15) because the periodicals and literary output are real, substantive intellectual production, not performative activity substituting for a decayed function. Accessibility collapse (0.4) is moderate: illiterate speakers cannot straightforwardly become literary Hebrew authors, but literacy and Haskalah circles were not hermetically sealed, so the alternative of eventual access was not fully foreclosed. Resistance (0.45) reflects the real, ongoing Yiddishist and traditionalist contestation of the criterion throughout the period.
 *
 * DIRECTIONALITY LOGIC:
 *   Maskilim, periodical editors, and secular Hebrew writers are declared beneficiaries: the criterion is authored by and for them, and it confers cultural authority on the capacity they actually possess (literary production) without requiring the harder-to-achieve condition of mass vernacular adoption. Illiterate Yiddish speakers and non-literary traditional communities are declared victims not because material value is extracted from them, but because the vitality definition structurally excludes their actual linguistic practice from counting as evidence — a status cost, not an economic one, which is why the derived directionality sits at moderate rather than extreme magnitude for this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing Hebrew's capacity for modern secular intellectual expression against the twin threats of linguistic fossilization and assimilation — is genuinely contested rather than dead: later Zionist revivalists treat it as resolved via the eventual emergence of spoken Modern Hebrew, while Yiddishist critics hold that literary productivity was never the right test and that the underlying problem (mass Jewish vernacular vitality) was never addressed by this criterion at all. This divergence is exactly why the story is authored as 'contested' rather than resolved in either direction — collapsing it to 'dead' would ignore the corroboration route through Zionist revivalism, and collapsing it to 'live' would ignore that the criterion never claimed to address vernacular transmission in the first place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_literary_vs_liturgical_vs_native,
    'Is ''living language'' correctly indexed to literary productivity, to liturgical continuity, or to native generational transmission — and does the choice of criterion itself functionally serve the interests of whichever group already possesses the relevant capacity?',
    'No empirical resolution exists because this is a definitional/conceptual dispute about what ''living'' means for a language; it can only be tracked by which criterion different communities of practice (literary, liturgical, linguistic-sociological) actually adopt and for what purposes, and by whether the criterion-selector benefits from the selection.',
    'Adopting literary_continuity_reading (this story) makes the maskilim and secular Hebrew writers the vindicated party and Hebrew ''alive'' well before any native speakers exist; adopting native_generation_reading would make the same 19th-century Hebrew a ''dead'' or ''not-yet-revived'' language despite the periodicals; adopting liturgical_preservation_reading would make Hebrew alive continuously since antiquity regardless of the Haskalah''s innovation at all. The three readings are not resolvable by more data about Hebrew — they are separate constraints, each internally coherent, corresponding to this kernel''s contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_literary_vs_liturgical_vs_native, conceptual, 'Location of disagreement across the living_language_status kernel''s three sibling readings.').

omega_variable(
    beneficiary_self_selection_of_vitality_criterion,
    'Is it a coincidence that the maskilim, who possess literary production capacity but not mass native-speaker reach, happen to define vitality in exactly the terms they satisfy — or is the criterion itself downstream of their interest in claiming cultural authority?',
    'Compare against counterfactual communities with the reverse capacity profile (e.g., mass native speakers with minimal contemporary literary output) and ask whether the same intellectuals would apply the literary criterion symmetrically to deny such a community''s language ''living'' status.',
    'If the criterion is asymmetrically applied — praised as sufficient when the maskilim satisfy it, but not treated as necessary when other communities lack it — this strengthens the case that literary_continuity_reading functions partly as retrospective justification for existing elite cultural authority, which would push the story''s classification toward tangled_rope rather than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_self_selection_of_vitality_criterion, conceptual, 'Whether the literary criterion is self-serving to the group that authored it.').

omega_variable(
    eventual_vernacularization_retroactive_vindication,
    'Does the later success of Modern Hebrew as a native spoken language retroactively vindicate the Haskalah literary corpus as having kept the language ''alive,'' or was the corpus''s role in that later revival causally incidental rather than constitutive?',
    'Historical-linguistic analysis of how much Haskalah-era literary vocabulary, syntax, and register actually fed into early Zionist spoken Hebrew versus how much the revival drew on other sources (Sephardi liturgical pronunciation, ad hoc neologism, children''s acquisition dynamics in Palestine).',
    'High causal contribution would support treating this reading''s founding problem as substantially resolved (live corroboration from Zionist revivalists is strong); low causal contribution would suggest the corroboration is more retrospective myth-making than genuine genealogical continuity, weakening founding_problem_status toward ''dead.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eventual_vernacularization_retroactive_vindication, empirical, 'Causal contribution of Haskalah literary Hebrew to the later native-speaker revival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__literary_continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(livi_tr_t12, living_language_status__literary_continuity_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(livi_tr_t24, living_language_status__literary_continuity_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement(livi_tr_t36, living_language_status__literary_continuity_reading, theater_ratio, 36, 0.13).
narrative_ontology:measurement(livi_tr_t48, living_language_status__literary_continuity_reading, theater_ratio, 48, 0.14).
narrative_ontology:measurement(livi_tr_t60, living_language_status__literary_continuity_reading, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__literary_continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(livi_be_t12, living_language_status__literary_continuity_reading, base_extractiveness, 12, 0.21).
narrative_ontology:measurement(livi_be_t24, living_language_status__literary_continuity_reading, base_extractiveness, 24, 0.24).
narrative_ontology:measurement(livi_be_t36, living_language_status__literary_continuity_reading, base_extractiveness, 36, 0.26).
narrative_ontology:measurement(livi_be_t48, living_language_status__literary_continuity_reading, base_extractiveness, 48, 0.27).
narrative_ontology:measurement(livi_be_t60, living_language_status__literary_continuity_reading, base_extractiveness, 60, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(living_language_status__literary_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__literary_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__literary_continuity_reading, 0.1).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__native_generation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the living_language_status kernel, decomposed per the ε-invariance principle because 'is Hebrew a living language' resolves to structurally distinct claims depending on the observable used (literary output vs. liturgical use vs. generational transmission). literary_continuity_reading (this file) authors low ε (~0.28), rope-leaning, elite-coordination structure. liturgical_preservation_reading is expected to author very low ε with a different beneficiary set (religious authorities, continuity of textual tradition) and near-mountain framing (preservation requires little active enforcement). native_generation_reading is expected to author the highest ε among the three, since its stricter criterion positions most historical Hebrew-use communities (including the maskilim themselves, pre-revival) as failing the vitality test, generating a different victim structure oriented around denying status to non-native-speaking traditions. All three share the same underlying kernel (what makes a language 'living') but are NOT the same constraint — each gets its own file per Rule 1.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
