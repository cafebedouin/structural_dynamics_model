% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Hebrew Living Language (Native Generative Production Reading)
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'hebrew_living_language': the native generative production reading. Under
 *   this reading, Hebrew becomes linguistically 'living' (and thus capable of
 *   bearing national identity) only when native speakers produce daily speech
 *   generatively—that is, creatively, not through memorized liturgical
 *   recitation or acquired second-language study. This reading emerged
 *   forcefully in late-nineteenth-century Zionist linguistic thought (Eliezer
 *   Ben-Yehuda and successors) and became institutionalized through
 *   Palestinian Jewish settlement, educational policy, and state formation
 *   after 1948. The reading defines victimhood structurally: vernacular
 *   speakers of Yiddish and Ladino, and diaspora communities whose Hebrew was
 *   textual/liturgical, are judged as linguistically deficient under the
 *   native-generation criterion. The constraint's extractiveness rises
 *   through the period as institutional power consolidates (state formation,
 *   mandatory education, Academy establishment) and suppression becomes more
 *   active (Yiddish displacement, curriculum standardization). The theater
 *   component (performative nativity ideology) remains moderate but steadies,
 *   suggesting the underlying extraction persists even as the enforcement
 *   machinery normalizes.
 *
 * KEY AGENTS:
 *   - hebrew_native_speaker_community: Structurally positioned to define linguistic legitimacy; benefits from prestige and validation; power institutional, exit arbitrage.
 *   - hebrew_language_academy: Institutional gate-keeper; sets standards through lexicon and pedagogy; benefits from prestige and continuity.
 *   - yiddish_vernacular_speakers: Primary victims; face cultural demotion, transmission disruption, suppression of institutional support; power moderate, exit identity-locked.
 *   - ladino_vernacular_speakers: Co-victims; carry Sephardic Jewish continuity; face equivalent suppression and prestige demotion.
 *   - diaspora_jewish_communities_without_hebrew_daily_speech: Powerless victims; lack native-speaker status by definition; constrained exit (geographic, economic barriers to acquiring Hebrew nativity).
 *   - palestine_jewish_immigrant_settlement: Beneficiaries; early adoption of Hebrew as vernacular validates their linguistic choice; children are native speakers.
 *   - hebrew_linguistic_reconstruction_scholars: Beneficiaries and agenda-setters; prestige, institutional positions, vindication of their reconstruction project.
 *   - international_jewish_diaspora_communities: Observers; face institutional pressure to adopt native-generation standard for education and cultural legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.62).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.71).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew Living Language (Native Generative Production Reading)").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, 'fe459d16-edcf-4728-8bc4-183000d44a69').
narrative_ontology:cs_kernel_codification('fe459d16-edcf-4728-8bc4-183000d44a69', distributed).
narrative_ontology:cs_authority_grounding('fe459d16-edcf-4728-8bc4-183000d44a69', extraction).
narrative_ontology:cs_interpretation_layer_present('fe459d16-edcf-4728-8bc4-183000d44a69').
narrative_ontology:cs_reading_relation('fe459d16-edcf-4728-8bc4-183000d44a69', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe459d16-edcf-4728-8bc4-183000d44a69', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('fe459d16-edcf-4728-8bc4-183000d44a69', foundational, nativity_is_necessary_for_linguistic_life).
narrative_ontology:cs_axiom_status(nativity_is_necessary_for_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('fe459d16-edcf-4728-8bc4-183000d44a69', nativity_is_necessary_for_linguistic_life, deontological).
narrative_ontology:cs_axiom('fe459d16-edcf-4728-8bc4-183000d44a69', secondary, generative_competence_requires_native_acquisition).
narrative_ontology:cs_axiom_status(generative_competence_requires_native_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('fe459d16-edcf-4728-8bc4-183000d44a69', generative_competence_requires_native_acquisition, empirically_contingent).
narrative_ontology:cs_reference_frame('fe459d16-edcf-4728-8bc4-183000d44a69', hebrew_as_reconstructed_living_vernacular).
narrative_ontology:cs_drift_state('fe459d16-edcf-4728-8bc4-183000d44a69', post_state_formation_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fe459d16-edcf-4728-8bc4-183000d44a69', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_native_speaker_community).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, diaspora_jewish_communities_without_hebrew_daily_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, palestine_jewish_immigrant_settlement).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_linguistic_reconstruction_scholars).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, hebrew_linguistic_nativity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, generative_competence_vs_liturgical_recitation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the standard for what counts as 'Hebrew living': daily, generative native speech, not memorized recitation or liturgical formulae. The community controls educational curricula, national institutions, and literary-linguistic standards. Defines the boundary of Hebrew authenticity and prestige through daily practice and transmission to children.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_native_speaker_community, agenda_setter,
    institutional, generational, arbitrage, national).

% Institutionalizes the native-generation standard through lexicon curation, orthographic prescription, and pedagogical authority. Sits between the native-speaker community and the broader public, mediating what is 'proper' Hebrew. Benefits from cultural prestige and institutional continuity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_language_academy, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__native_generation_reading, hebrew_language_academy, beneficiary).

% Carried centuries of Jewish daily life in Yiddish and transmitted it across generations. Under the native-generation reading, Yiddish is reframed as a diaspora remnant, not a living Jewish language in its own right. Vernacular speakers face cultural demotion: their fluency counts as 'only Yiddish,' not Hebrew; their children are redirected to Hebrew education, severing the transmission chain. The constraint suppresses Yiddish institutional support, prestige, and transmission.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_vernacular_speakers, payer,
    moderate, biographical, identity_locked, global).

% Carried Sephardic Jewish life in Ladino across diaspora and Ottoman territories. Under the native-generation reading, Ladino faces the same suppression as Yiddish: reframed as non-Hebrew, pre-modern, inadequate for modern Jewish identity. Transmission is interrupted as children learn Hebrew instead. The constraint actively displaces Ladino from prestige and institutional support.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_vernacular_speakers, payer,
    moderate, biographical, identity_locked, global).

% Communities where Hebrew was liturgical and textual, not daily vernacular—the majority of diaspora Jewry for nearly two millennia. The native-generation reading defines them as linguistically deficient: their Hebrew knowledge, even if deep and learned, does not count because it was not native childhood speech. They bear the cost of having to acquire Hebrew as a second language if they seek cultural legitimacy within the native-generation framework, or they accept diminished status. Geographic and economic constraints prevent acquisition for many.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, diaspora_jewish_communities_without_hebrew_daily_speech, payer,
    powerless, biographical, constrained, global).

% Early-twentieth-century Zionist settlers who adopted Hebrew as their daily vernacular and transmitted it natively to their children. They benefit structurally from the native-generation standard: it validates their linguistic choice and marks them as modernizers while delegitimizing diaspora alternatives. Their children are native speakers; the constraint secures their status.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, palestine_jewish_immigrant_settlement, beneficiary,
    powerful, generational, mobile, regional).

% Scholars and language planners (Eliezer Ben-Yehuda and successors) who undertook the reconstruction of Hebrew from liturgical text into a living colloquial tongue. They benefit from prestige, institutional positions, and the successful instantiation of their linguistic vision. They set standards through publication, education policy, and cultural authority. Their work is vindicated by the constraint.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_linguistic_reconstruction_scholars, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__native_generation_reading, hebrew_linguistic_reconstruction_scholars, agenda_setter).

% Nineteenth-century Haskalah writers and scholars who produced generative Hebrew literature without native childhood acquisition (they learned Hebrew as a second or third language through study). The native-generation reading excludes them: their literary Hebrew, however sophisticated, does not count as 'Hebrew living' because it was not native. They would argue that written generative competence—creating new meanings and forms—constitutes linguistic life; they are not in the room to make that argument.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, pre_native_generation_hebrew_literary_community, excluded,
    moderate, biographical, trapped, global).

% Communities across medieval and ancient diaspora (Babylonia, North Africa, Europe, Yemen) who maintained Hebrew textual study and composed Hebrew liturgy and philosophy without daily vernacular speech. The native-generation reading retroactively judges them as having had 'dead' Hebrew, even though they understood themselves as maintaining a continuous, living tradition. They cannot contest the reading.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, medieval_and_ancient_diaspora_hebrew_communities, excluded,
    powerless, civilizational, trapped, global).

% Communities that choose whether to adopt the native-generation standard for their own institutions and educational paths. They observe the constraint in operation: pressure to teach Hebrew-as-native (immersion, native speaker instruction) rather than Hebrew-as-learnable-second-language. Their choice set is shaped by the institutional power of the native-generation framework.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, international_jewish_diaspora_communities, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__native_generation_reading, hebrew_native_speaker_community).
narrative_ontology:fixing_cost_class(hebrew_living_language__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of Hebrew linguistic continuity across diaspora: establishes a shared speech norm, integrates displaced communities through a common language, and creates a mechanism for converting liturgical/textual knowledge into operational daily competence. Enables coordination of a Jewish national project (state-building, cultural revival) around a single, prestige vernacular.
% TRANSFER_FUNCTION: Transfers prestige, institutional resources, and cultural legitimacy from vernacular-transmission communities (Yiddish, Ladino speakers) to native-acquisition communities (Hebrew speakers, immigrant settlers). Moves the locus of 'authentic Jewish speech' from diaspora practices to the Zionist-settler path. Extracts generational transmission disruption: children of Yiddish/Ladino speakers are directed to Hebrew, severing vernacular chains and subordinating parents' fluency to a 'non-living' status.
% ABSENT_VOICES: Yiddish and Ladino literary and linguistic communities would argue that their vernaculars represent living, generative Jewish speech; they carried the majority of Jewish daily life and culture. They are systematically excluded from the conversation about what counts as 'Hebrew living'—their presence would reframe the question (Is Hebrew's revival at the cost of suppressing other Jewish languages legitimate? Is linguistic nativity the correct measure of linguistic life?). Pre-Zionist diaspora Hebrew scholars would testify that textual and liturgical Hebrew constituted a living tradition without daily speech. They are posthumously judged rather than heard.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared—if the native-generation standard were rejected and Hebrew were recognized as living through written generative competence, or through liturgical continuity without nativity requirement—the entire basis for linguistic legitimacy in the Jewish world would shift. Yiddish and Ladino revitalization would become possible; diaspora Hebrew scholarship would be revalidated; immigrant-settler Hebrew would lose its exclusive claim to authenticity. Hebrew education would be reframed as second-language acquisition rather than nativity restoration. The state institutions, curriculum standards, and cultural hierarchies that depend on the native-generation criterion would reorganize.
% FOUNDING_PROBLEM: Hebrew as a living vernacular died with the shift to diaspora languages (Aramaic, Arabic, Yiddish, Ladino) after classical antiquity. By the nineteenth century, Hebrew existed primarily in liturgical and textual form. The founding problem: How can a Jewish national project be built on a language that lacks daily native speakers and contemporary cultural production? How can Hebrew be restored as a means of modern Jewish communication and state formation?
% FOUNDING_PROBLEM_CORROBORATION: The native-generation reading attests the problem is live: without native Hebrew speech, there is no organic cultural basis for the Jewish state. Yiddish and Ladino communities, and twentieth-century diaspora scholars, attest the problem was defined selectively to justify suppressing competing languages: Hebrew literacy and cultural relevance existed through non-native channels; the 'dead language' framing was a political choice to displace vernaculars, not an empirical necessity. Post-1948 testimony from communities that resisted Hebrew-only education attests the founding problem was solvable through multilingual frameworks; the native-generation reading made linguistic domination—not necessity—the organizing principle.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured at 0.62 (interval endpoint) because the constraint transfers prestige and legitimacy from vernacular communities to native-speaker communities while suppressing Yiddish and Ladino. The constraint is not merely a definition; it carries enforcement (institutional education, cultural gatekeeping, resource allocation). Suppression at 0.71 reflects active machinery: Yiddish linguistic institutions faced budget cuts and delegitimization; children were transitioned to Hebrew-only education; vernacular cultural production was reframed as pre-modern. Theater at 0.42 indicates moderate performativity: the constraint maintained an ideology of 'linguistic restoration' and 'authentic nativity' while operating as displacement and prestige transfer. The measurements span 1880–1960 to capture the constraint's intensification: at 1880, the native-generation reading was an emergent intellectual position (low extractiveness 0.32, suppression 0.38—mostly rhetorical). By 1920, it had institutional backing (Palestinian Jewish schools, Academy founding); suppression rose to 0.64 as deliberate policy. By 1948 (state formation), suppression peaked (0.79) as Hebrew-only education and institutional standardization became mandatory. The decline to 0.71 by 1960 reflects normalization: the constraint's victory was consolidated, so active enforcement decreased (suppression_requirement measures active force needed to hold the constraint, not the constraint's achieved dominance).
 *
 * PERSPECTIVAL GAP:
 *   The constraint appears profoundly differently from each seat. From the native-speaker community and Academy perspective: this is linguistic restoration—the recovery of Hebrew as a living language after two millennia of liturgical use, essential to Jewish national revival and cultural continuity. From Yiddish and Ladino speakers: this is linguistic suppression—the delegitimization and displacement of living Jewish vernaculars in favor of a reconstructed, normalized language imposed as a marker of modernity and national belonging. The payer seats (vernacular speakers, diaspora communities) experience active suppression and prestige loss; the beneficiary seats experience validation and institutional advancement. The engine computes this divergence from the structural data: the declared victims and beneficiaries, the exit options (identity-locked vs. arbitrage), and the power differentials (institutional vs. powerless). The authorized claim (tangled_rope) reflects this asymmetry: the constraint coordinates a national linguistic project while asymmetrically extracting from those whose vernaculars and practices do not fit the native-generation norm.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs sharply across seats. For hebrew_native_speaker_community (d ≈ 0.2): benefits from prestige and institutional validation; native-speaker status is affirmed; exit is arbitrage (they could claim linguistic authenticity anywhere Hebrew is spoken natively). For hebrew_language_academy (d ≈ 0.25): institutional beneficiary, sets standards, collects prestige. For yiddish_vernacular_speakers (d ≈ 0.88): clear targets; face active suppression, prestige demotion, transmission interruption; exit is identity-locked (Yiddish identity and linguistic heritage cannot be abandoned without cultural discontinuity). For diaspora_jewish_communities_without_hebrew_daily_speech (d ≈ 0.82): powerless targets; defined as linguistically deficient; constrained exit (acquiring native-speaker status is impossible for adults, difficult and expensive for children across dispersed communities). For palestine_jewish_immigrant_settlement (d ≈ 0.35): modest beneficiaries; mobile exit (they could return to Europe or Americas; Hebrew nativity was a choice, not a trap). The engine derives these from beneficiary/victim declarations and exit modulation; no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy dynamics: the founding problem (restoration of Hebrew as a living language) is substantially solved by 1948. Hebrew is established as a daily vernacular in Palestine/Israel; native speakers exist; a new generation transmits it natively. Yet the constraint persists intensely through 1960 and beyond, because it now serves as an enforcement mechanism for cultural homogenization and state legitimacy rather than for solving the founding problem. The suppression_requirement remains high (0.71) not because Hebrew's status as 'living' is still contested, but because maintaining linguistic dominance requires active suppression of alternatives. The theater component (ideological justification of the native-generation standard as 'authentic restoration') persists despite the original problem being resolved. This is classic mandatrophy: the constraint outlives its founding justification and becomes a vehicle for extraction (prestige transfer, cultural dominance) maintained through institutional inertia and enforced linguistic nationalism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nativity_as_necessity_vs_choice,
    'Is native-speaker status a necessary condition for a language to be ''living'' in a linguistic/cultural sense, or is it a contingent feature that this reading elevated to necessity for political reasons?',
    'Comparative analysis: examine multilingual communities where languages are ''living'' without native-speaker cohorts (e.g., some colonial-contact languages, liturgical languages with active generative use). Examine whether diaspora Hebrew scholarship and literary production were functionally ''living'' despite non-native status.',
    'If nativity is contingent (not necessary), the native-generation reading is a political choice to suppress competing languages, not a linguistic fact. Reclassification from tangled_rope (coordination + extraction) to snare (pure extraction, nativity-via-suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nativity_as_necessity_vs_choice, conceptual, 'Whether linguistic life requires nativity or whether this reading imposed nativity as a political boundary.').

omega_variable(
    foundational_problem_obsolescence,
    'By what date had the founding problem (Hebrew lacking native speakers) been substantially solved, and after that date, what problem does the constraint continue to solve?',
    'Historical analysis: demographic data on native Hebrew speakers by year; educational enrollment tracking Hebrew vs. other languages; institutional documentation of when Hebrew was established as operational daily language in Palestine/Israel.',
    'If the founding problem was solved by 1920–1930 but the constraint''s suppression intensity continued rising through 1948–1960, the constraint''s persistence is mandatrophic—it is enforcing linguistic hegemony, not solving the founding problem. Confirms omega_mandatrophy_resolution classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundational_problem_obsolescence, empirical, 'Dating the obsolescence of the founding problem and identifying what the constraint now enforces.').

omega_variable(
    victim_identity_lock_mechanism,
    'Is the measured suppression of Yiddish/Ladino speakers structural (economic resources, institutional barriers) or internalized (speakers themselves believe Hebrew nativity is superior)?',
    'Post-suppression-lift trajectory: examine communities where Yiddish/Ladino revitalization was attempted (post-1960s scholarship, revival movements, institutional support); track whether suppression persists despite removal of structural barriers.',
    'If suppression persists after structural barriers are removed, the identity-locking in the constraint is partially internalized—the victims carry the suppression cognitively even when enforcement relaxes. Confirms exit=identity_locked classification for Yiddish speakers; suggests deeper mandatrophy (the constraint has rewritten speakers'' self-conception of linguistic legitimacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identity_lock_mechanism, empirical, 'Whether suppression of Yiddish is structural, internalized, or both.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Do the native-generation reading and the literary-revival reading logically foreclose each other, or can both remain live positions held by different parties?',
    'Examine contemporary discourse: do scholars and communities that endorse literary-revival (written generative Hebrew as sufficient) argue that this position is incompatible with native-generation in principle, or do they simply disagree on which standard is preferable?',
    'If foreclosure is real, the relation in cs_structure.reading_relations to literary_revival_reading should be forecloses. If coexistence is defensible (e.g., both readings acknowledge that native speech is valuable, but disagree on whether it is necessary), the relation should be coexists_with.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the readings logically foreclose each other or can remain simultaneous live positions.').

omega_variable(
    strict_reachability_break,
    'Does the native-generation reading break strict reachability for diaspora communities: can diaspora Jews ever satisfy the native-generation criterion without relocating to Israel, and is that relocation itself the extraction mechanism?',
    'Examine whether diaspora communities could establish Hebrew-native cohorts through immersion schooling, immigration, or other means; assess the material and identity costs imposed by the constraint.',
    'If the native-generation criterion is unreachable for diaspora communities without displacement/assimilation, the constraint is extracting diaspora cultural autonomy and linguistic continuity as the price of Jewish identity inclusion. Confirms victim status for diaspora_jewish_communities_without_hebrew_daily_speech and supports classification as tangled_rope (coordination of national project + extraction of diaspora cultural autonomy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reachability_break, empirical, 'Whether the native-generation criterion imposes an unreachable standard that extracts diaspora cultural autonomy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 1880, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__native_generation_reading, theater_ratio, 1880, 0.25).
narrative_ontology:measurement_basis(hebr_tr_t1880, observed).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_living_language__native_generation_reading, theater_ratio, 1900, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t1900, observed).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_living_language__native_generation_reading, theater_ratio, 1920, 0.35).
narrative_ontology:measurement_basis(hebr_tr_t1920, observed).
narrative_ontology:measurement(hebr_tr_t1935, hebrew_living_language__native_generation_reading, theater_ratio, 1935, 0.41).
narrative_ontology:measurement_basis(hebr_tr_t1935, observed).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_living_language__native_generation_reading, theater_ratio, 1948, 0.46).
narrative_ontology:measurement_basis(hebr_tr_t1948, observed).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_living_language__native_generation_reading, theater_ratio, 1960, 0.42).
narrative_ontology:measurement_basis(hebr_tr_t1960, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__native_generation_reading, base_extractiveness, 1880, 0.32).
narrative_ontology:measurement_basis(hebr_be_t1880, observed).
narrative_ontology:measurement(hebr_be_t1900, hebrew_living_language__native_generation_reading, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement_basis(hebr_be_t1900, observed).
narrative_ontology:measurement(hebr_be_t1920, hebrew_living_language__native_generation_reading, base_extractiveness, 1920, 0.54).
narrative_ontology:measurement_basis(hebr_be_t1920, observed).
narrative_ontology:measurement(hebr_be_t1935, hebrew_living_language__native_generation_reading, base_extractiveness, 1935, 0.61).
narrative_ontology:measurement_basis(hebr_be_t1935, observed).
narrative_ontology:measurement(hebr_be_t1948, hebrew_living_language__native_generation_reading, base_extractiveness, 1948, 0.68).
narrative_ontology:measurement_basis(hebr_be_t1948, observed).
narrative_ontology:measurement(hebr_be_t1960, hebrew_living_language__native_generation_reading, base_extractiveness, 1960, 0.62).
narrative_ontology:measurement_basis(hebr_be_t1960, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__native_generation_reading, suppression_requirement, 1880, 0.38).
narrative_ontology:measurement_basis(hebr_su_t1880, observed).
narrative_ontology:measurement(hebr_su_t1900, hebrew_living_language__native_generation_reading, suppression_requirement, 1900, 0.52).
narrative_ontology:measurement_basis(hebr_su_t1900, observed).
narrative_ontology:measurement(hebr_su_t1920, hebrew_living_language__native_generation_reading, suppression_requirement, 1920, 0.64).
narrative_ontology:measurement_basis(hebr_su_t1920, observed).
narrative_ontology:measurement(hebr_su_t1935, hebrew_living_language__native_generation_reading, suppression_requirement, 1935, 0.73).
narrative_ontology:measurement_basis(hebr_su_t1935, observed).
narrative_ontology:measurement(hebr_su_t1948, hebrew_living_language__native_generation_reading, suppression_requirement, 1948, 0.79).
narrative_ontology:measurement_basis(hebr_su_t1948, observed).
narrative_ontology:measurement(hebr_su_t1960, hebrew_living_language__native_generation_reading, suppression_requirement, 1960, 0.71).
narrative_ontology:measurement_basis(hebr_su_t1960, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__native_generation_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__literary_revival_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__liturgical_continuity_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories decompose the contested kernel 'hebrew_living_language.' Each reading instantiates a different definition of what makes Hebrew 'living': (1) native_generation_reading (this story) defines it through native daily speech; (2) literary_revival_reading defines it through written generative sophistication without native acquisition; (3) liturgical_continuity_reading defines it through unbroken textual/liturgical transmission without nativity requirement. The three readings produce different victim sets, beneficiary structures, and ε values because they instantiate different constraints—different claims about linguistic life, different mechanisms of suppression, different foundational problems. Each reading is authored independently as a clean ε-invariant constraint. The sibling relations are declared in cs_structure.reading_relations (this reading influences both siblings by establishing a prestige standard that devalues alternatives; neither relation is foreclosure because all three readings remain live in contemporary discourse). The network links enable contamination analysis: if the native-generation reading's enforcement weakens (as post-1960s language-revival movements attempted), the literary and liturgical readings gain institutional space.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
