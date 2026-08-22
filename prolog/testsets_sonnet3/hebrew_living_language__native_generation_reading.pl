% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: hebrew_living_language__native_generation_reading
 *   human_readable: Ben-Yehuda Native-Generation Standard for Hebrew Revival
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This story authors the NATIVE-GENERATION reading of the contested 'Hebrew
 *   living language' kernel: the claim that Hebrew only becomes a living
 *   language when native speakers produce daily speech generatively —
 *   memorized liturgical recitation or literary competence does not count.
 *   This is the reading historically associated with Eliezer Ben-Yehuda and
 *   the Yishuv Hebraist institutions, and it is the reading with the sharpest
 *   victim set: it required actively suppressing the genuinely living
 *   vernaculars (Yiddish, Ladino) that immigrants already spoke, redefining
 *   them as obstacles rather than assets to the national project. The 1913
 *   'War of the Languages' (Milchemet HaSafot), in which Hebraist activists
 *   physically and socially confronted a proposal to teach science in German
 *   at the Technion, and the 'Gdud Meginei HaSafa' patrols that shamed
 *   Yiddish speakers in public, are the enforcement mechanisms measured here.
 *   This story's ε is authored strictly from the standing arrangement this
 *   reading itself endorses (native-generation-as-vitality-criterion),
 *   assessed for what it actually cost the vernacular communities it
 *   displaced — not from any sibling reading's alternative endorsement.
 *
 * KEY AGENTS:
 *   - yishuv_hebraist_institutions: agenda_setter (institutional/arbitrage) — sets and enforces the native-generation criterion
 *   - hebrew_only_school_networks: beneficiary/agenda_setter (organized/arbitrage) — institutional legitimacy depends on producing native speakers
 *   - yiddish_speaking_immigrants: payer (moderate/constrained) — bear social and intergenerational cost of suppression
 *   - ladino_speaking_immigrants: payer (powerless/trapped) — doubly marginalized vernacular community
 *   - children_of_diaspora_vernacular_households: payer (powerless/trapped) — the actual site of enforcement
 *   - diaspora_vernacular_literary_communities: excluded (organized/trapped) — flourishing Yiddish/Ladino institutions with no voice in the criterion
 *   - historical_linguists: observer (analytical/analytical) — comparative assessment of revitalization criteria
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.58).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.62).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Ben-Yehuda Native-Generation Standard for Hebrew Revival").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '0b3bd21b-8fd5-4070-bb9b-229f5c910113').
narrative_ontology:cs_kernel_codification('0b3bd21b-8fd5-4070-bb9b-229f5c910113', distributed).
narrative_ontology:cs_authority_grounding('0b3bd21b-8fd5-4070-bb9b-229f5c910113', extraction).
narrative_ontology:cs_interpretation_layer_present('0b3bd21b-8fd5-4070-bb9b-229f5c910113').
narrative_ontology:cs_reading_relation('0b3bd21b-8fd5-4070-bb9b-229f5c910113', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b3bd21b-8fd5-4070-bb9b-229f5c910113', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_axiom('0b3bd21b-8fd5-4070-bb9b-229f5c910113', foundational, generative_daily_speech_is_the_only_vitality_criterion).
narrative_ontology:cs_axiom_status(generative_daily_speech_is_the_only_vitality_criterion, holdable).
narrative_ontology:cs_axiom_grounding('0b3bd21b-8fd5-4070-bb9b-229f5c910113', generative_daily_speech_is_the_only_vitality_criterion, conventional).
narrative_ontology:cs_axiom('0b3bd21b-8fd5-4070-bb9b-229f5c910113', secondary, native_acquisition_requires_monolingual_household_transmission).
narrative_ontology:cs_axiom_status(native_acquisition_requires_monolingual_household_transmission, holdable).
narrative_ontology:cs_axiom_grounding('0b3bd21b-8fd5-4070-bb9b-229f5c910113', native_acquisition_requires_monolingual_household_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('0b3bd21b-8fd5-4070-bb9b-229f5c910113', pre_revival_hebrew_as_liturgical_literary_only).
narrative_ontology:cs_drift_state('0b3bd21b-8fd5-4070-bb9b-229f5c910113', post_1948_state_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0b3bd21b-8fd5-4070-bb9b-229f5c910113', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, yishuv_hebraist_institutions).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_only_school_networks).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, zionist_national_project).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_speaking_immigrants).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_speaking_immigrants).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, children_of_diaspora_vernacular_households).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, language_revival_requires_native_acquisition).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, generative_competence_as_vitality_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Hebrew Language Committee, Hebrew-medium schools, and Zionist labor organizations set the criterion that Hebrew is only 'living' if children acquire it as a first language for spontaneous daily use. They administer curricula, teacher training, and social pressure campaigns (including the 'Gdud Meginei HaSafa' language-defense patrols) that penalize public use of Yiddish and other vernaculars in Hebrew-designated spaces.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yishuv_hebraist_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Schools built their pedagogical and institutional legitimacy entirely on producing native Hebrew speakers through immersion. Their funding, staffing, and prestige depend on the native-generation standard being the accepted measure of success; they enforce Hebrew-only rules on children regardless of home language.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_only_school_networks, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__native_generation_reading, hebrew_only_school_networks, agenda_setter).

% The national movement gains a unifying vernacular that erases visible diaspora fragmentation (Yiddish, Ladino, Judeo-Arabic dialects) and manufactures a continuous线 from ancient Hebrew sovereignty to the new state. It does not administer the constraint directly but collects legitimacy and cohesion from its success.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, zionist_national_project, beneficiary,
    institutional, civilizational, analytical, national).

% Eastern European immigrants arrive with a fully living vernacular literature, press, and daily speech community. Under the native-generation standard their language is redefined as an obstacle to national vitality; they face social shaming, exclusion from institutions, and pressure to abandon Yiddish transmission to their children even in the home.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speaking_immigrants, payer,
    moderate, biographical, constrained, national).

% Sephardi and Mizrahi immigrant communities speaking Ladino or Judeo-Arabic have even less institutional leverage than the Yiddish-speaking Ashkenazi majority. Their vernaculars are treated as doubly irrelevant to the national project, accelerating language loss with little compensating institutional support or documentation effort.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speaking_immigrants, payer,
    powerless, biographical, trapped, regional).

% Children are the actual site where the native-generation criterion is enforced — they are pushed to become the first true native Hebrew speakers, often discouraged or punished for using their parents' home language even in play. They bear the psychological and intergenerational cost of severing transmission, without having chosen the standard.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, children_of_diaspora_vernacular_households, payer,
    powerless, biographical, trapped, national).

% Yiddish and Ladino writers, journalists, and theater communities had a flourishing living literature at the time of the Hebrew revival. They are excluded from the criterion-setting process entirely; their objection — that vitality does not require abandoning an already-living vernacular — is not represented in the institutions that adjudicate what counts as 'living Hebrew.'
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, diaspora_vernacular_literary_communities, excluded,
    organized, generational, trapped, continental).

% Assess whether generative native acquisition is a defensible criterion for language vitality against comparative revitalization cases (Irish, Cornish, Wampanoag), and document the suppression costs paid by vernacular communities during the Yishuv period.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, historical_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__native_generation_reading, hebrew_only_school_networks).
narrative_ontology:fixing_cost_class(hebrew_living_language__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed, multilingual immigrant population around a single national vernacular capable of spontaneous daily generative use — solving the real problem that a purely liturgical or literary Hebrew cannot serve as the working language of a modern society, economy, and army.
% TRANSFER_FUNCTION: Moves linguistic capital and intergenerational transmission away from Yiddish, Ladino, and other diaspora vernaculars and concentrates it in Hebrew-medium institutions; moves social status and institutional access to those who successfully raise or become native Hebrew speakers, at the direct cost of vernacular speech communities and their children's fluency.
% ABSENT_VOICES: Yiddish and Ladino literary and communal institutions, which had functioning living vernaculars, were not consulted on the criterion that redefined their languages as obstacles rather than assets; had they been represented, an alternative standard recognizing plural living Jewish vernaculars alongside a revived Hebrew might have been adopted.
% DISAPPEARANCE_RATIONALE: If the native-generation standard were abandoned and literary or liturgical competence accepted as sufficient for 'living' status, the entire apparatus of Hebrew-only schooling, language-defense enforcement, and the delegitimization of Yiddish/Ladino transmission would lose its justification; diaspora vernaculars could have persisted as coequal living languages within the Yishuv rather than being displaced.
% FOUNDING_PROBLEM: A revived national vernacular is needed that functions as a full first-language medium for administration, education, army, and daily life — literary Hebrew and liturgical Hebrew, however extensive, had ceased for centuries to be anyone's spontaneously generated mother tongue.
% FOUNDING_PROBLEM_CORROBORATION: Hebraist institutions and their historians (e.g., accounts descending from Ben-Yehuda's own circle) attest the founding problem was live and decisively solved by native acquisition. Independent sociolinguists and historians of the Yishuv (outside the Hebraist beneficiary institutions) corroborate that the underlying coordination problem was real, but dispute that it required the suppression of Yiddish and Ladino rather than coexistence; Yiddishist cultural historians attest the problem was addressed at an avoidable cost to their own communities.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects a genuine coordination achievement (a functioning national vernacular capable of full civic and military use) riding on top of substantial, non-incidental suppression of pre-existing living vernaculars — this is not a Mountain because it required continuous, deliberate institutional effort (language patrols, school enforcement, social shaming) rather than emerging as an inevitable natural process. Suppression (0.62) is high because exit from the criterion was actively blocked for children in Hebrew-medium schools and for adults seeking social/economic integration into the Yishuv, though it is not maximal because Yiddish and Ladino institutions persisted, diminished, outside the enforced zones. Theater ratio is low-moderate (0.2): the enforcement was substantially functional (it did produce native speakers) rather than merely performative, though some of the language-patrol activity (public shaming campaigns) had a theatrical, disciplinary-display component beyond what was needed to teach children Hebrew.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebraist institutions and school networks are structural beneficiaries: they collect legitimacy, funding, and their own institutional survival from the native-generation standard being adopted as THE criterion — d sits near the beneficiary end. Vernacular-speaking immigrants and especially their children are structural targets: the standard's success is measured precisely by how completely it severs their transmission — d sits near the full-target end, amplified for children whose trapped exit options (they cannot choose their schooling) push them further toward the target pole than their parents. Ladino speakers, with even less institutional power than the Yiddish-speaking plurality, receive the least mitigating leverage and the least amplification of any counter-voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Hebrew needed a native generative register to function as a national vernacular) was real and, by this reading's own account, has been substantially solved — Hebrew has had native generative speakers for generations. The mandatrophy question is whether the SUPPRESSION APPARATUS (language patrols, institutional delegitimization of Yiddish/Ladino) needed to persist as long and as harshly as it did after the coordination function was largely achieved, or whether it hardened into an identity-marking mechanism (Hebrew monolingualism as national purity) beyond what native-speaker production required. The declining suppression_requirement trajectory after 1929 in the measurements reflects an honest reading that active enforcement intensity peaked during the contested transition (the 1913 language war era) and relaxed once native Hebrew speech was self-sustaining — consistent with a Tangled Rope whose coercive component was load-bearing during construction but became excess afterward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_generation_reading_identity,
    'Is this constraint''s high extractiveness an artifact of measuring the native-generation criterion specifically, rather than the broader ''Hebrew revival'' project — and would a differently-scoped reading of the same historical episode yield a different ε?',
    'This is resolved by construction, not by further analysis: this story deliberately authors ONLY the native-generation reading. The literary_revival_reading and liturgical_continuity_reading are separate constraint files with their own ε values and their own stakeholder sets, linked via network.affects_constraints. No single ε should be assigned across all three readings.',
    'Confirms this file''s ε (0.58) refers strictly to the native-generation criterion''s own operation and victim set, not to a composite or averaged ''Hebrew revival'' judgment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(native_generation_reading_identity, conceptual, 'Kernel decomposition boundary: this reading''s ε is not the kernel''s ε.').

omega_variable(
    strict_reachability_break,
    'Does the native-generation criterion require a genealogically unbroken chain of transmission from ancient spoken Hebrew, or is it satisfied by reconstruction/revival despite an acknowledged historical break in continuous native speech (Hebrew having functioned as a liturgical/literary language, not a vernacular, for roughly 1700 years before the Yishuv)?',
    'Historical linguistic record: comparative documentation of whether any community maintained unbroken native Hebrew vernacular transmission through the diaspora period, versus the Ben-Yehuda-era record of deliberate, effortful, first-generation acquisition planning (e.g., raising Itamar Ben-Avi as the first modern native speaker).',
    'If the reading requires unbroken transmission and none existed, the native-generation reading is itself a reconstruction project rather than a continuity claim — this does not by itself lower ε, but it changes how the founding_problem_status should be read: the ''return to a living language'' framing may itself be a legitimating narrative for what was in fact a first-time construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_reachability_break, empirical, 'Acknowledged reconstruction rather than unbroken continuity underlies this reading''s criterion.').

omega_variable(
    suppression_necessity_ambiguity,
    'Was active suppression of Yiddish and Ladino transmission a structurally necessary component of achieving generative native Hebrew speech, or could coexistent multilingualism have produced native Hebrew speakers without displacing other vernaculars (as in many successful bilingual/multilingual child-acquisition contexts)?',
    'Comparative sociolinguistic evidence from multilingual acquisition contexts elsewhere (e.g., communities that raised children bilingually without one language displacing the other) and archival evidence on whether Hebraist institutions considered non-suppressive alternatives and rejected them for ideological rather than pedagogical reasons.',
    'If suppression was not pedagogically necessary, the tangled_rope classification is strongly supported (the extraction component is separable from the coordination component and was a policy choice, not a functional requirement). If suppression was genuinely required to achieve first-generation native fluency at the needed speed and scale, more of the measured extraction should be attributed to unavoidable coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_necessity_ambiguity, empirical, 'Whether vernacular suppression was functionally required or an ideological additive to the native-generation project.').

omega_variable(
    reading_framing_undertermination,
    'Given that the kernel''s authority (who gets to declare Hebrew ''living'') was itself contested among Hebraist institutions, diaspora literary communities, and religious authorities, is the institutional/extraction framing chosen here (authority_grounding: extraction) the only defensible framing, or could this reading instead be framed as practice-grounded (a speech community''s own practice constituting the standard)?',
    'Compare the degree to which the Hebraist institutions'' authority derived from bottom-up practice uptake (families genuinely choosing to raise Hebrew-speaking children) versus top-down institutional enforcement (school policy, language patrols, social sanction). Archival evidence on voluntary versus coerced adoption in specific Yishuv communities would discriminate.',
    'A practice-grounded framing would treat the norm as emergent from genuine community practice (closer to a Rope) while the extraction framing (adopted here) treats it as institutionally administered and coercively maintained against dissenting vernacular communities (Tangled Rope). This story adopts the extraction framing because the documented enforcement apparatus (language patrols, school-based punishment, institutional delegitimization campaigns) shows the standard was actively administered, not merely practiced into being.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_undertermination, conceptual, 'Alternative CS framing (practice vs. extraction authority) would change classification; extraction chosen based on documented enforcement apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 1881, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1881, hebrew_living_language__native_generation_reading, theater_ratio, 1881, 0.1).
narrative_ontology:measurement(hebr_tr_t1897, hebrew_living_language__native_generation_reading, theater_ratio, 1897, 0.12).
narrative_ontology:measurement(hebr_tr_t1913, hebrew_living_language__native_generation_reading, theater_ratio, 1913, 0.18).
narrative_ontology:measurement(hebr_tr_t1929, hebrew_living_language__native_generation_reading, theater_ratio, 1929, 0.2).
narrative_ontology:measurement(hebr_tr_t1939, hebrew_living_language__native_generation_reading, theater_ratio, 1939, 0.2).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_living_language__native_generation_reading, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1881, hebrew_living_language__native_generation_reading, base_extractiveness, 1881, 0.3).
narrative_ontology:measurement(hebr_be_t1897, hebrew_living_language__native_generation_reading, base_extractiveness, 1897, 0.4).
narrative_ontology:measurement(hebr_be_t1913, hebrew_living_language__native_generation_reading, base_extractiveness, 1913, 0.52).
narrative_ontology:measurement(hebr_be_t1929, hebrew_living_language__native_generation_reading, base_extractiveness, 1929, 0.55).
narrative_ontology:measurement(hebr_be_t1939, hebrew_living_language__native_generation_reading, base_extractiveness, 1939, 0.57).
narrative_ontology:measurement(hebr_be_t1948, hebrew_living_language__native_generation_reading, base_extractiveness, 1948, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1881, hebrew_living_language__native_generation_reading, suppression_requirement, 1881, 0.25).
narrative_ontology:measurement(hebr_su_t1897, hebrew_living_language__native_generation_reading, suppression_requirement, 1897, 0.35).
narrative_ontology:measurement(hebr_su_t1913, hebrew_living_language__native_generation_reading, suppression_requirement, 1913, 0.6).
narrative_ontology:measurement(hebr_su_t1929, hebrew_living_language__native_generation_reading, suppression_requirement, 1929, 0.65).
narrative_ontology:measurement(hebr_su_t1939, hebrew_living_language__native_generation_reading, suppression_requirement, 1939, 0.6).
narrative_ontology:measurement(hebr_su_t1948, hebrew_living_language__native_generation_reading, suppression_requirement, 1948, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__native_generation_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the hebrew_living_language kernel (see kernel_context). liturgical_continuity_reading (lowest ε, closest to Mountain/Rope — no suppression required) and literary_revival_reading (moderate-low ε — Haskalah written production without native vernacular displacement) are separate files. This file (native_generation_reading) carries the highest ε of the three because its own success criterion is structurally incompatible with parallel vernacular vitality in the same population. The three readings should never be averaged into a single 'Hebrew revival' ε; each is ε-invariant on its own terms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_living_language__native_generation_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
