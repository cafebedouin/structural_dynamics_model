% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: 1928 Turkish Latin Alphabet Reform as Kemalist Civilizational Rupture
 *   domain: linguistics/political_authority/state_building
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested script_as_identity
 *   kernel: the Kemalist rupture reading, which holds that adopting Latin
 *   script served a deliberate civilizational severing of Turkish national
 *   identity from its Ottoman-Islamic past, and that this severing was the
 *   point, not an incidental side effect of phonetic reform. On this reading,
 *   the coordination function (a more phonetically regular writing system,
 *   standardized mass literacy) is real but is bundled with, and subordinated
 *   to, an extraction and status-transfer function: the transfer of
 *   interpretive authority from Ottoman scribal and religious-scholarly
 *   classes to the secular state, and the erasure of transitional costs for
 *   those already oriented toward European print culture. The 'zero
 *   transition cost' structural delta named for this reading refers to the
 *   state's framing, not to the actual costs borne by displaced literate and
 *   scholarly classes — the state bore no cost because it externalized the
 *   entire transition cost onto exactly those groups. This is a tangled_rope:
 *   genuine coordination (regularized orthography, expanded literacy)
 *   coexists with asymmetric extraction (status transfer, authority transfer)
 *   sustained by active enforcement (criminalization of continued
 *   Arabic-script use in official contexts, dissolution of religious courts
 *   in the same period).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.62).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.81).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "1928 Turkish Latin Alphabet Reform as Kemalist Civilizational Rupture").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "linguistics/political_authority/state_building").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, 'a829e31b-c061-46ed-abcb-8e254fd17d77').
narrative_ontology:cs_kernel_codification('a829e31b-c061-46ed-abcb-8e254fd17d77', formalized).
narrative_ontology:cs_authority_grounding('a829e31b-c061-46ed-abcb-8e254fd17d77', extraction).
narrative_ontology:cs_interpretation_layer_present('a829e31b-c061-46ed-abcb-8e254fd17d77').
narrative_ontology:cs_reading_relation('a829e31b-c061-46ed-abcb-8e254fd17d77', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('a829e31b-c061-46ed-abcb-8e254fd17d77', script_as_identity__phonetic_instrumentalism_reading, influences).
narrative_ontology:cs_axiom('a829e31b-c061-46ed-abcb-8e254fd17d77', foundational, rupture_from_ottoman_islamic_past_is_liberatory).
narrative_ontology:cs_axiom_status(rupture_from_ottoman_islamic_past_is_liberatory, holdable).
narrative_ontology:cs_axiom_grounding('a829e31b-c061-46ed-abcb-8e254fd17d77', rupture_from_ottoman_islamic_past_is_liberatory, instrumental).
narrative_ontology:cs_axiom('a829e31b-c061-46ed-abcb-8e254fd17d77', secondary, state_monopoly_on_literacy_apparatus_is_legitimate_modernization_tool).
narrative_ontology:cs_axiom_status(state_monopoly_on_literacy_apparatus_is_legitimate_modernization_tool, holdable).
narrative_ontology:cs_axiom_grounding('a829e31b-c061-46ed-abcb-8e254fd17d77', state_monopoly_on_literacy_apparatus_is_legitimate_modernization_tool, conventional).
narrative_ontology:cs_reference_frame('a829e31b-c061-46ed-abcb-8e254fd17d77', ottoman_islamic_scribal_continuity).
narrative_ontology:cs_drift_state('a829e31b-c061-46ed-abcb-8e254fd17d77', post_1928_decree_enforcement, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('a829e31b-c061-46ed-abcb-8e254fd17d77', '').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, kemalist_state_apparatus).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, secular_urban_elite).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, new_literacy_bureaucracy).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_literate_class).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, religious_scholars_ulema).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, older_generation_functional_illiterates).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, arabic_script_publishers).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, secular_modernization_doctrine).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, civilizational_westward_orientation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decrees the 1928 alphabet law, establishes the Millet Mektepleri (Nation's Schools) to compel adult re-literacy, and criminalizes continued Arabic-script publication in official and commercial contexts. Frames the change as completing a civilizational reorientation; captures the resulting monopoly over literacy certification, education curricula, and the archive of what counts as legible public knowledge going forward.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, kemalist_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Already oriented toward European institutions, professions, and print culture; the reform ratifies a status this group already held informally and converts it into formal advantage in state employment, publishing, and education, since fluency in the new orthography maps onto existing social position.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, secular_urban_elite, beneficiary,
    powerful, generational, mobile, national).

% Scribes, clerks, and professionals whose entire accumulated literacy — decades of skill in Ottoman Arabic-script prose — is rendered non-transferable overnight. Cannot appeal to their credential in the new system; must re-train from a position of functional illiteracy alongside people they previously outranked.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_literate_class, payer,
    moderate, biographical, trapped, national).

% Their authority rested on interpretive access to Arabic-script Quranic, legal, and juridical texts. The script change is bundled with disestablishment of religious courts and closure of the caliphate's institutional supports; loses standing as the arbiter of textual meaning to the secular state's new pedagogical apparatus. Formally without a channel to object once religious institutions are dissolved in the same period.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, religious_scholars_ulema, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, religious_scholars_ulema, excluded).

% Rural and older populations who had partial or oral relationships to Arabic-script literacy through religious education now face a state literacy campaign in an entirely new orthography, administered on the state's timetable, with no possibility of falling back on prior partial competence.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, older_generation_functional_illiterates, payer,
    powerless, biographical, trapped, regional).

% Printing houses, newspapers, and booksellers built around Arabic-script production must retool presses, retrain compositors, and rebuild a customer base of newly literate readers, or exit the trade; the transition window is short and state-mandated.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, arabic_script_publishers, payer,
    moderate, biographical, constrained, national).

% Teachers, textbook producers, and the newly created Turkish Language Society gain a mandate, funding, and institutional permanence from administering the transition; their professional existence is constituted by the reform's continuation and elaboration.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, new_literacy_bureaucracy, beneficiary,
    organized, generational, arbitrage, national).

% Communities with their own scriptural or liturgical traditions bound to Arabic or Ottoman orthography (some Kurdish, Arab, and other minority populations) are not consulted on the rupture and absorb the same compulsory transition without the compensating status gains that accrue to the secular urban elite.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, diaspora_and_minority_script_communities, excluded,
    powerless, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a national population around a single, phonetically regularized writing system administered and certified by the state, replacing a script whose orthographic conventions were poorly matched to Turkish vowel phonology and whose literacy was unevenly distributed and religiously gatekept.
% TRANSFER_FUNCTION: Moves interpretive and credentialing authority from Ottoman scribal and religious-scholarly classes to the secular state and its new pedagogical bureaucracy; moves social status from those already oriented toward European print culture to those it displaces from Arabic-script literacy.
% ABSENT_VOICES: The ulema and Ottoman literate class had no formal channel to contest the reform once the caliphate and religious courts were dissolved in the same legislative period; minority script communities were not separately consulted at all — their objections, where they existed, surface mainly in post-hoc historical and diasporic memoir literature outside the state record.
% DISAPPEARANCE_RATIONALE: Were the rupture undone — had Arabic script retained official and educational status — the entire subsequent architecture of Turkish state literacy, the Turkish Language Society's institutional mission, and the informal status hierarchy privileging secular-oriented elites over religious-scholarly authority would not have the same shape; generations of national identity formation were built directly on the presumption of the break.
% FOUNDING_PROBLEM: Ottoman Arabic script was held, in the reformers' framing, to poorly represent Turkish vowel harmony, to gatekeep literacy behind years of specialized training, and to bind Turkish civic identity to an Ottoman-Islamic imperial and religious order the new republic sought to leave behind.
% FOUNDING_PROBLEM_CORROBORATION: The Kemalist state and its historiographic successors attest the founding problem (illiteracy, script-imperial entanglement) was real and is resolved by the reform's success. Independent linguists outside the state's framing corroborate the phonetic-mismatch claim as technically accurate but separable from the rupture's civilizational framing; historians of the late Ottoman period and descendants of the displaced literate and scholarly classes attest that literacy itself was rising under Arabic script reform proposals already circulating before 1928, and that the rupture's severity was a political choice, not a linguistic necessity — this corroboration comes from outside the reform's beneficiary set.
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__kemalist_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__kemalist_rupture_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects substantial but not totalizing transfer: mass literacy genuinely expanded and vowel-harmony phonetic mismatch was a real technical problem the reform solved, but a large share of the reform's political value was the severing itself, borne disproportionately by scribal and religious-scholarly classes who had no path to convert prior skill into new standing. Suppression (0.81, declining from 0.88 to 0.68 over the interval as compliance normalized) captures the compulsory character of the Nation's Schools campaign and the criminalization of continued Arabic-script commercial and official use — suppression is a raw structural property, not scaled by scope, and it is authored highest at the outset when active enforcement machinery was newly built and declines as the new generation ages into the system and enforcement need falls. Theater ratio rises modestly (0.12 to 0.28) as the reform's coordination function (teaching literacy) increasingly runs alongside performative civilizational rhetoric (annual observances, monument culture) once the practical literacy transition is substantially complete.
 *
 * DIRECTIONALITY LOGIC:
 *   The kemalist_state_apparatus and secular_urban_elite sit near the beneficiary end: the state captures literacy-certification and archival authority; the elite converts pre-existing informal cultural capital into formal advantage. The ottoman_literate_class, religious_scholars_ulema, older_generation_functional_illiterates, and arabic_script_publishers sit near the target end: each bears a transition cost the reform's framing declares not to exist. The ulema's exit options are marked constrained rather than trapped because some scholarly authority persisted informally in private and rural contexts even after formal disestablishment — but their formal institutional channel for objection was closed in the same legislative period, which the excluded secondary role captures.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (phonetic mismatch, uneven literacy, script-imperial entanglement) is contested as to whether it remains live: literacy is now near-universal in the Latin script, so the practical problem is resolved, but the civilizational-rupture framing this reading foregrounds persists as an ongoing ideological commitment (secularist historiography, state commemoration) independent of any live literacy problem. This is exactly the founding_problem_status=contested case the R5 interview is built to surface: corroboration from outside the reform's beneficiary set (independent linguists, historians of the late Ottoman period, descendants of displaced scholarly classes) supports treating the literacy problem as solved while the rupture-as-identity-project persists on inertia and doctrine, which is a live candidate for mandatrophy even though this story does not declare mandatrophy_resolved outright, since the coordination function (literacy) still receives partial ongoing justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_intentionality_vs_side_effect,
    'Was the severing of Ottoman-Islamic textual continuity the deliberate political objective of the 1928 reform, or an incidental consequence of pursuing genuine phonetic and literacy improvements that the state later narrated as civilizational rupture for legitimation purposes?',
    'Archival analysis of the Language Commission''s internal deliberations and Ataturk''s private and public statements in the period immediately preceding the decree, cross-referenced against the sequencing of religious-institution dissolution measures in the same legislative window.',
    'If the rupture was the deliberate primary objective, this reading''s tangled_rope classification with its extraction emphasis is well-supported. If the rupture was substantially a post-hoc narrative constructed to explain an intervention primarily motivated by phonetic and literacy concerns, the phonetic_instrumentalism_reading captures more of the actual causal structure and this reading''s extractiveness score would be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_intentionality_vs_side_effect, conceptual, 'Whether civilizational rupture was the reform''s actual objective or a retrospective narrative.').

omega_variable(
    sibling_reading_incompatibility_location,
    'Where exactly do the kemalist_rupture_reading and ottoman_continuity_reading structurally disagree — is it a factual dispute about what happened to interpretive authority, or an irreducible normative dispute about whether the Ottoman-Islamic past was worth continuing?',
    'Separate the descriptive claim (authority transferred from ulema to state apparatus — likely uncontested by both readings) from the normative claim (this transfer was liberation vs. this transfer was loss) using independent historical consensus on the descriptive facts.',
    'If the disagreement is purely normative once the descriptive facts are held fixed, the two readings coexist as competing evaluations of the same structure rather than competing structural claims — this would argue for coexists_with rather than forecloses in cs_structure.reading_relations, which is the relation this story in fact declares.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_incompatibility_location, conceptual, 'Locating the precise axis of disagreement between the rupture and continuity readings.').

omega_variable(
    false_summit_civilizational_necessity,
    'Is the civilizational-rupture framing itself naturalized by later historiography as an inevitable modernization step (a false summit), obscuring the identifiable beneficiaries — the state apparatus and secular elite — who gained concentrated advantage from a contingent political choice?',
    'Compare comparative cases (e.g., other post-Ottoman successor states that modernized literacy without full script rupture) to test whether civilizational rupture was a necessary condition for the literacy and modernization gains achieved, or a separable political choice layered onto them.',
    'If comparable literacy gains were achievable without full rupture, the rupture component is best read as extraction riding on a coordination function rather than a naturally necessary feature of modernization — reinforcing this reading''s classification as tangled_rope rather than a legitimacy-neutral mountain-like inevitability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_summit_civilizational_necessity, empirical, 'Whether rupture was necessary for modernization gains or a contingent extractive add-on.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 1928, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t1928, script_as_identity__kemalist_rupture_reading, theater_ratio, 1928, 0.12).
narrative_ontology:measurement(scri_tr_t1934, script_as_identity__kemalist_rupture_reading, theater_ratio, 1934, 0.18).
narrative_ontology:measurement(scri_tr_t1940, script_as_identity__kemalist_rupture_reading, theater_ratio, 1940, 0.22).
narrative_ontology:measurement(scri_tr_t1946, script_as_identity__kemalist_rupture_reading, theater_ratio, 1946, 0.25).
narrative_ontology:measurement(scri_tr_t1953, script_as_identity__kemalist_rupture_reading, theater_ratio, 1953, 0.27).
narrative_ontology:measurement(scri_tr_t1960, script_as_identity__kemalist_rupture_reading, theater_ratio, 1960, 0.28).

% Extraction over time
narrative_ontology:measurement(scri_be_t1928, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1928, 0.55).
narrative_ontology:measurement(scri_be_t1934, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1934, 0.6).
narrative_ontology:measurement(scri_be_t1940, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1940, 0.63).
narrative_ontology:measurement(scri_be_t1946, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1946, 0.62).
narrative_ontology:measurement(scri_be_t1953, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1953, 0.6).
narrative_ontology:measurement(scri_be_t1960, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1960, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t1928, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1928, 0.88).
narrative_ontology:measurement(scri_su_t1934, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1934, 0.85).
narrative_ontology:measurement(scri_su_t1940, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1940, 0.8).
narrative_ontology:measurement(scri_su_t1946, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1946, 0.76).
narrative_ontology:measurement(scri_su_t1953, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1953, 0.72).
narrative_ontology:measurement(scri_su_t1960, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1960, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__kemalist_rupture_reading, 0.1).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'the Turkish alphabet reform' / 'script_as_identity' kernel, per the ε-invariance principle: measuring the reform's political-identity function (this story) yields a substantially different ε and beneficiary/victim structure than measuring its phonetic-technical function (phonetic_instrumentalism_reading) or its continuity-rupture-as-loss function (ottoman_continuity_reading). All three are linked via affects_constraints rather than merged into one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
