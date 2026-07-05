% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__native_generation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_living_language__native_generation_reading
 *   human_readable: Hebrew Revival as Native Daily Generative Speech (Ben-Yehuda/Yishuv Standard)
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Between the 1880s and 1948, Hebraist institutions in Ottoman and Mandate
 *   Palestine pursued the deliberate transformation of Hebrew from a
 *   liturgical/literary register into a native, generatively-spoken mother
 *   tongue for a new generation of children (the sabra generation). This was
 *   not language maintenance but language engineering: schools, youth
 *   movements, and labor organizations enforced Hebrew-only norms, actively
 *   displacing the vernaculars immigrants actually brought with them (chiefly
 *   Yiddish, and Ladino among Sephardi communities). The coordination
 *   achievement — a genuinely shared, spoken national vernacular where none
 *   existed — is real. So is the extraction: adult immigrants who could never
 *   fully acquire native generative fluency were permanently disadvantaged
 *   relative to their own Hebrew-native children, and the vernaculars they
 *   arrived speaking were treated as illegitimate obstacles rather than
 *   living languages in their own right.
 *
 * KEY AGENTS:
 *   - yishuv_hebraist_institutions: agenda_setter (institutional/arbitrage) — designs and enforces the native-speaker standard
 *   - sabra_native_speaker_generation: beneficiary (moderate/mobile) — inherits full native fluency and social advantage
 *   - yiddish_vernacular_speakers: payer (powerless/constrained) — native vernacular actively suppressed
 *   - ladino_vernacular_speakers: payer (powerless/constrained) — native vernacular actively suppressed, less institutionally visible
 *   - immigrant_first_generation_adults: payer (powerless/trapped) — cannot fully meet the native-generation bar themselves
 *   - historical_linguists: observer (analytical) — assesses the revival against the suppression it required
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.52).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.68).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew Revival as Native Daily Generative Speech (Ben-Yehuda/Yishuv Standard)").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '3e14eb4c-6786-499c-9a46-7c3fae61ce23').
narrative_ontology:cs_kernel_codification('3e14eb4c-6786-499c-9a46-7c3fae61ce23', distributed).
narrative_ontology:cs_authority_grounding('3e14eb4c-6786-499c-9a46-7c3fae61ce23', practice).
narrative_ontology:cs_interpretation_layer_present('3e14eb4c-6786-499c-9a46-7c3fae61ce23').
narrative_ontology:cs_reading_relation('3e14eb4c-6786-499c-9a46-7c3fae61ce23', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e14eb4c-6786-499c-9a46-7c3fae61ce23', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_axiom('3e14eb4c-6786-499c-9a46-7c3fae61ce23', foundational, native_daily_generative_speech_is_the_criterion_of_life).
narrative_ontology:cs_axiom_status(native_daily_generative_speech_is_the_criterion_of_life, holdable).
narrative_ontology:cs_axiom_grounding('3e14eb4c-6786-499c-9a46-7c3fae61ce23', native_daily_generative_speech_is_the_criterion_of_life, conventional).
narrative_ontology:cs_axiom('3e14eb4c-6786-499c-9a46-7c3fae61ce23', secondary, national_vernacular_unification_justifies_vernacular_displacement).
narrative_ontology:cs_axiom_status(national_vernacular_unification_justifies_vernacular_displacement, holdable).
narrative_ontology:cs_axiom_grounding('3e14eb4c-6786-499c-9a46-7c3fae61ce23', national_vernacular_unification_justifies_vernacular_displacement, instrumental).
narrative_ontology:cs_reference_frame('3e14eb4c-6786-499c-9a46-7c3fae61ce23', hebrew_as_liturgical_and_literary_register_only).
narrative_ontology:cs_drift_state('3e14eb4c-6786-499c-9a46-7c3fae61ce23', yishuv_native_speaker_establishment, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('3e14eb4c-6786-499c-9a46-7c3fae61ce23', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, yishuv_hebraist_institutions).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, sabra_native_speaker_generation).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, zionist_national_project).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, immigrant_first_generation_adults).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, language_can_be_revived_from_liturgical_to_native_status).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, national_identity_requires_a_spoken_national_language).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Vaad HaLashon, the Hebrew school system, and Zionist labor organizations set curriculum, employment, and social norms that make Hebrew the only acceptable medium of daily life in the Yishuv. They administer the shift from liturgical-recitation Hebrew to native generative Hebrew, and their institutional legitimacy is built on the success of that shift.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yishuv_hebraist_institutions, agenda_setter,
    institutional, generational, arbitrage, regional).

% Children raised in Hebrew-speaking households and schools acquire Hebrew as a true native, generative language rather than a recited liturgical register. They inherit full linguistic capital in the emerging national language and social advantage in Yishuv institutions built around it.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, sabra_native_speaker_generation, beneficiary,
    moderate, generational, mobile, regional).

% The political project of a unified national vernacular gains its central proof-point: a language once confined to prayer and text becomes an everyday spoken medium, supplying a unifying identity marker distinct from the diaspora vernaculars immigrants actually brought with them.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, zionist_national_project, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(hebrew_living_language__native_generation_reading, zionist_national_project).

% Ashkenazi immigrants who arrive with Yiddish as a fully generative native language are told the language is not merely inconvenient but illegitimate for the new society. Yiddish theater, press, and schooling are actively discouraged or suppressed by Hebraist enforcement (language patrols, social shaming, institutional exclusion); their children are schooled away from the language they grew up speaking.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_vernacular_speakers, payer,
    powerless, biographical, constrained, regional).

% Sephardi immigrants whose native generative vernacular is Ladino face parallel pressure to abandon daily use of their mother tongue in favor of Hebrew; their linguistic heritage is treated as a diaspora relic incompatible with the new native-speaker standard, with less institutional visibility than the Yiddish suppression campaign.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_vernacular_speakers, payer,
    powerless, biographical, constrained, regional).

% Adults who arrive without childhood Hebrew must acquire fluency through Ulpan-style adult instruction under social and economic pressure; many never achieve the generative native fluency the standard requires, permanently occupying a lesser linguistic status relative to their own children, with no realistic route back to full vernacular life in their mother tongue within Yishuv society.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, immigrant_first_generation_adults, payer,
    powerless, biographical, trapped, regional).

% Religious scholars and communities for whom Hebrew's living status was never in question — sustained through unbroken recitation and study across the diaspora — are largely absent from the secular Hebraist institutions setting the native-generation standard; their claim that Hebrew was already alive is not part of the conversation that adjudicates the revival's success.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, liturgical_hebrew_tradition_bearers, excluded,
    organized, civilizational, constrained, regional).

% Scholars of language revitalization examine the Hebrew case as the paradigm instance of engineered native-speaker transmission, assessing what was gained (a functioning vernacular) against what was suppressed (competing Jewish vernaculars) and whether the native-generation criterion is the right bar for 'living language' status at all.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single shared spoken vernacular for a rapidly assembling, linguistically fragmented immigrant society, solving the coordination problem of a population arriving from dozens of mutually unintelligible mother tongues with no common daily language.
% TRANSFER_FUNCTION: Moves linguistic capital, institutional legitimacy, and intergenerational social standing away from Yiddish- and Ladino-speaking households and toward Hebrew-educated households and the Hebraist institutions that administer the standard; costs are borne disproportionately by adult immigrants and by vernaculars actively displaced rather than merely outcompeted.
% ABSENT_VOICES: Yiddishist cultural organizations and Sephardi Ladino-speaking communities would object that their languages were also living, generative, native vernaculars with their own literatures and speech communities, and that the Hebraist standard required their active suppression rather than neutral coexistence; they were largely excluded from Vaad HaLashon and Zionist labor-movement institutions that set language policy. Liturgical tradition-bearers would object that Hebrew's 'living' status never lapsed to begin with, and that the native-generation bar imports a modern linguistic criterion foreign to their own tradition's self-understanding.
% DISAPPEARANCE_RATIONALE: If the native-generation standard were withdrawn as the operative criterion for 'Hebrew as living language,' the entire justificatory architecture of the Yishuv's language policy — school curricula, labor-movement Hebrew-only norms, the delegitimation of Yiddish press and theater — would lose its warrant; multilingual daily life (Hebrew alongside persistent Yiddish/Ladino vernaculars) would likely have persisted, and the strong monolingual-native outcome that in fact occurred is not obviously what would have emerged absent active institutional enforcement.
% FOUNDING_PROBLEM: A dispersed, multilingual Jewish immigrant population arriving in Ottoman/Mandate Palestine had no shared daily vernacular; Hebrew existed only as a liturgical and literary register, not a native mother tongue for anyone, and the Zionist national project needed a unifying spoken language distinct from the diaspora vernaculars associated with exile.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — the absence of a shared vernacular in a linguistically fragmented Yishuv — is now resolved by any measure; Hebrew has been the majority native language of Israeli society for generations. Contemporary Israeli linguists and historians of the Yishuv period (writing outside the Hebraist institutions that administered the original suppression campaigns) corroborate that the problem is fully solved and that the enforcement apparatus long outlived active necessity, persisting for decades afterward as ideological commitment and institutional inertia rather than continued response to a live coordination gap. Descendants of Yiddish- and Ladino-speaking immigrants, largely outside the benefiting Hebraist institutions, corroborate the suppression's cost and its persistence past the point of functional need.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_living_language__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate, not severe, because a genuine coordination good was produced — a functioning shared vernacular where multilingual chaos previously existed — and the beneficiary generation's gain is not purely rent extraction but includes real linguistic capital creation. Suppression (0.68) is higher than extraction because the mechanism by which the native-generation standard was enforced (language patrols, institutional exclusion of Yiddish press/theater/schooling, social shaming of vernacular use in public) was coercive and targeted, independent of how much net benefit resulted. Accessibility collapse (0.61) reflects that once Hebrew was institutionally established as the sole legitimate vernacular, alternatives for raising children bilingually or maintaining Yiddish/Ladino as equal-status home languages collapsed sharply within a generation. Resistance (0.58) reflects real, documented resistance from Yiddishist cultural organizations and some religious communities, though ultimately unsuccessful against the institutional weight of the Zionist labor movement and school system.
 *
 * PERSPECTIVAL GAP:
 *   From the Hebraist institutional seat, this is coordination succeeding exactly as designed: a shared national vernacular replacing chaotic multilingualism. From the Yiddish- or Ladino-speaking immigrant seat, the same arrangement is experienced as the forced abandonment of a fully living, generative mother tongue in favor of one that had to be actively reconstructed for the purpose. The engine's per-seat computation should register this divergence directly from the beneficiary/victim/enforcement data rather than from either party's self-description.
 *
 * DIRECTIONALITY LOGIC:
 *   Yishuv Hebraist institutions and the sabra generation sit at the beneficiary end: institutions gain the legitimacy of having engineered a national vernacular, and native-Hebrew children inherit uncontested linguistic capital. Yiddish and Ladino vernacular speakers, and first-generation adult immigrants generally, sit at the target end: their exit options are constrained or trapped (leaving Palestine, or the Yishuv's institutional life, is the only real alternative to compliance with the Hebrew-only norm), and the constraint's operation actively displaces the language they came in speaking rather than merely failing to accommodate it. This is what makes the enforcement requirement (requires_active_enforcement: true) non-optional for this reading — a passive, non-coercive Hebrew revival that tolerated Yiddish/Ladino as equal vernaculars would be a different, much less extractive constraint (closer to the literary_revival_reading in character).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no shared vernacular in a fragmented immigrant society) was substantially solved by roughly the 1930s-40s, once the sabra generation reached adulthood and Hebrew was unambiguously the dominant native language. Yet enforcement intensity (suppression_requirement) continued at near-peak levels through 1948 rather than relaxing in step with the problem's resolution — consistent with the founding_problem_status of 'dead' while institutional practice persisted on ideological momentum. This is precisely the mislabeling risk the classification guards against: treating the arrangement purely as ongoing pure extraction ignores the real, already-banked coordination good (a living national vernacular now exists); treating it purely as benign completed coordination ignores that its enforcement apparatus, and the suppression it enacted against competing vernaculars, continued well past the point of functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_choice_native_generation_vs_siblings,
    'Is ''living language'' status for Hebrew properly indexed to native daily generative speech acquisition (this reading), to unbroken liturgical recitation across the diaspora (liturgical_continuity_reading), or to generative literary/written production without native speech (literary_revival_reading)?',
    'No empirical resolution exists — this is a definitional dispute about what ''living language'' means, adjudicated differently by sociolinguists (who typically privilege native acquisition), religious communities (who privilege continuous liturgical use), and literary historians (who privilege generative written production). Each reading is internally coherent and answers a different question.',
    'Choosing the native_generation_reading is what introduces the victim set (Yiddish/Ladino speakers) and the moderate extraction/high suppression profile into this constraint; the liturgical_continuity_reading would show near-zero extraction (nothing needed to be revived) and the literary_revival_reading would show low extraction (Haskalah writers imposed no comparable vernacular-suppression cost). The three readings are not competing measurements of one constraint — they are three different constraints sharing a label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_choice_native_generation_vs_siblings, conceptual, 'Which of three defensible readings of ''Hebrew becomes living'' is instantiated by this story, and why the choice determines the extraction profile.').

omega_variable(
    strict_reachability_break_and_reconstruction,
    'Given that native generative Hebrew speech had no unbroken chain of native transmission for roughly 1,700 years before the Yishuv revival, is the resulting spoken Hebrew properly a ''revival'' of the same language, or a reconstruction that merely uses the historical name and textual corpus?',
    'Historical linguistic analysis of the degree of continuity vs. innovation in Yishuv Hebrew phonology, syntax, and lexicon relative to Biblical/Mishnaic/Medieval Hebrew strata, compared against standard criteria for language continuity vs. reconstruction in other revitalization cases (e.g. Cornish, Wampanoag).',
    'If treated as reconstruction rather than continuous revival, the native_generation_reading''s coordination claim (restoring what was lost) is weakened relative to a claim of constructing something substantially new under an old name — which would somewhat reduce the legitimacy the beneficiary institutions draw from the ''revival'' framing without changing the extraction borne by suppressed vernacular speakers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reachability_break_and_reconstruction, empirical, 'Whether the strict-reachability break in native transmission makes this a revival or a reconstruction, and what that implies for the coordination claim''s legitimacy.').

omega_variable(
    necessity_of_suppression_for_coordination,
    'Was active suppression of Yiddish and Ladino structurally necessary to achieve a unified native Hebrew vernacular, or could the same coordination outcome have been reached through positive Hebrew-promotion alone, without displacing competing vernaculars?',
    'Comparative analysis against other successful vernacular-revival or vernacular-adoption cases where suppression of competing home languages was absent or much weaker, to assess whether comparable native-acquisition outcomes were achieved without the coercive component.',
    'If suppression was not structurally necessary, the extraction measured here is closer to pure extraction riding on a coordination story than to an unavoidable cost of coordination — which would push this reading''s classification toward snare rather than tangled_rope. If necessary given the specific demographic and political urgency of the period, the tangled_rope classification (genuine coordination function bundled with real extraction) is the more defensible read.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_of_suppression_for_coordination, conceptual, 'Whether the vernacular suppression was a necessary cost of coordination or a separable extractive overlay.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 1881, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1881, hebrew_living_language__native_generation_reading, theater_ratio, 1881, 0.1).
narrative_ontology:measurement(hebr_tr_t1897, hebrew_living_language__native_generation_reading, theater_ratio, 1897, 0.13).
narrative_ontology:measurement(hebr_tr_t1909, hebrew_living_language__native_generation_reading, theater_ratio, 1909, 0.16).
narrative_ontology:measurement(hebr_tr_t1921, hebrew_living_language__native_generation_reading, theater_ratio, 1921, 0.2).
narrative_ontology:measurement(hebr_tr_t1936, hebrew_living_language__native_generation_reading, theater_ratio, 1936, 0.24).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_living_language__native_generation_reading, theater_ratio, 1948, 0.22).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1881, hebrew_living_language__native_generation_reading, base_extractiveness, 1881, 0.22).
narrative_ontology:measurement(hebr_be_t1897, hebrew_living_language__native_generation_reading, base_extractiveness, 1897, 0.34).
narrative_ontology:measurement(hebr_be_t1909, hebrew_living_language__native_generation_reading, base_extractiveness, 1909, 0.45).
narrative_ontology:measurement(hebr_be_t1921, hebrew_living_language__native_generation_reading, base_extractiveness, 1921, 0.55).
narrative_ontology:measurement(hebr_be_t1936, hebrew_living_language__native_generation_reading, base_extractiveness, 1936, 0.5).
narrative_ontology:measurement(hebr_be_t1948, hebrew_living_language__native_generation_reading, base_extractiveness, 1948, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1881, hebrew_living_language__native_generation_reading, suppression_requirement, 1881, 0.35).
narrative_ontology:measurement(hebr_su_t1897, hebrew_living_language__native_generation_reading, suppression_requirement, 1897, 0.5).
narrative_ontology:measurement(hebr_su_t1909, hebrew_living_language__native_generation_reading, suppression_requirement, 1909, 0.62).
narrative_ontology:measurement(hebr_su_t1921, hebrew_living_language__native_generation_reading, suppression_requirement, 1921, 0.7).
narrative_ontology:measurement(hebr_su_t1936, hebrew_living_language__native_generation_reading, suppression_requirement, 1936, 0.68).
narrative_ontology:measurement(hebr_su_t1948, hebrew_living_language__native_generation_reading, suppression_requirement, 1948, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__native_generation_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, literary_revival_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, yiddish_cultural_suppression_yishuv).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the hebrew_living_language kernel decomposed per the ε-invariance principle: native_generation_reading (this story, moderate extraction, tangled_rope), liturgical_continuity_reading (near-zero extraction, mountain-adjacent — nothing needed reviving), and literary_revival_reading (low extraction, closer to rope — Haskalah literary production required no vernacular suppression). Each carries its own ε and its own stakeholder set; they are linked here rather than merged because measuring 'is Hebrew a living language' by different observables (native acquisition vs. liturgical continuity vs. literary generativity) yields genuinely different extraction profiles, which is the schema's signal that multiple constraints, not multiple measurements of one constraint, are present.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
