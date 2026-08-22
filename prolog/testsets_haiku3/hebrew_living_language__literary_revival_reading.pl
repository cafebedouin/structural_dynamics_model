% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__literary_revival_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Hebrew as Living Language Through Haskalah Literary Production
 *   domain: historical_linguistics/cultural_continuity/commitment_systems
 *
 * SUMMARY:
 *   The literary-revival reading of Hebrew livingness claims that Hebrew
 *   remained genuinely alive as a language through Haskalah-era (18th–early
 *   20th century) literary and philosophical production by elite writers in
 *   Central and Eastern Europe. Without native daily speakers, Hebrew
 *   sustained generative competence through written authorship of poetry,
 *   philosophy, criticism, and journalism. The reading treats the unbroken
 *   chain of textual production as evidence that Hebrew could be regenerated
 *   from its own internal resources—that literacy and literary practice
 *   suffice for a language to be considered living. This reading coexists
 *   with two others: the liturgical-continuity reading (Hebrew lived through
 *   unbroken recitation of sacred texts in diaspora worship) and the
 *   native-generation reading (Hebrew became truly living only when native
 *   speakers in Eretz Israel produced daily speech). The literary-revival
 *   reading is historically central to the Haskalah self-understanding and to
 *   early Zionist ideology but is challenged by both competing readings on
 *   structural grounds.
 *
 * KEY AGENTS:
 *   - Hebrew literary intelligentsia (Mendelsohn, Krochmal, Smolenskin, et al.) — define and produce the texts claimed as evidence of livingness
 *   - European Jewish communities — access to Enlightenment thought through Hebrew medium
 *   - Rabbinic/liturgical establishments — excluded from the conversation, would dispute the reading's premise
 *   - Non-Hebrew-literate diaspora Jews — invisible to the reading, have no claim under its framework
 *   - Native Eretz Israel speakers (later in period) — would eventually displace this reading with native-generation reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.08).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.02).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew as Living Language Through Haskalah Literary Production").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical_linguistics/cultural_continuity/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, 'a0e829f0-2218-4b99-adc0-4bacef593f17').
narrative_ontology:cs_kernel_codification('a0e829f0-2218-4b99-adc0-4bacef593f17', distributed).
narrative_ontology:cs_authority_grounding('a0e829f0-2218-4b99-adc0-4bacef593f17', lineage).
narrative_ontology:cs_interpretation_layer_present('a0e829f0-2218-4b99-adc0-4bacef593f17').
narrative_ontology:cs_reading_relation('a0e829f0-2218-4b99-adc0-4bacef593f17', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('a0e829f0-2218-4b99-adc0-4bacef593f17', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('a0e829f0-2218-4b99-adc0-4bacef593f17', foundational, written_literary_production_sustains_language_livingness).
narrative_ontology:cs_axiom_status(written_literary_production_sustains_language_livingness, holdable).
narrative_ontology:cs_axiom_grounding('a0e829f0-2218-4b99-adc0-4bacef593f17', written_literary_production_sustains_language_livingness, empirically_contingent).
narrative_ontology:cs_axiom('a0e829f0-2218-4b99-adc0-4bacef593f17', foundational, elite_authorial_competence_demonstrates_generative_continuity).
narrative_ontology:cs_axiom_status(elite_authorial_competence_demonstrates_generative_continuity, holdable).
narrative_ontology:cs_axiom_grounding('a0e829f0-2218-4b99-adc0-4bacef593f17', elite_authorial_competence_demonstrates_generative_continuity, instrumental).
narrative_ontology:cs_reference_frame('a0e829f0-2218-4b99-adc0-4bacef593f17', hebrew_as_textual_intellectual_tradition).
narrative_ontology:cs_drift_state('a0e829f0-2218-4b99-adc0-4bacef593f17', post_native_speech_emergence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0e829f0-2218-4b99-adc0-4bacef593f17', '2026-06-15T14:32:00Z').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_literary_intelligentsia).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, european_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enlightenment-era and early-modern Hebrew writers and scholars (Mendelsohn, Krochmal, Smolenskin, and their networks) who produced original literary work, criticism, and philosophical writing in Hebrew. They created a generative written standard that demonstrated Hebrew as a vehicle for contemporary intellectual life. They set the agenda by defining what counts as living Hebrew: original authorial voice through the written word. They benefit from the constraint by gaining cultural prestige, intellectual authority, and the ability to imagine Hebrew as a national/intellectual project.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_literary_intelligentsia, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, hebrew_literary_intelligentsia, beneficiary).

% Jewish communities in Central and Eastern Europe for whom Hebrew literacy meant access to Enlightenment thought and European intellectual frameworks. The literary-revival reading enabled Hebrew to carry contemporary European ideas (philosophy, science, aesthetics) into Jewish intellectual life without requiring wholesale adoption of European vernaculars. They benefit from gaining a path to modernity that preserves Jewish identity through the Hebrew medium.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, european_jewish_communities, beneficiary,
    moderate, biographical, constrained, continental).

% Rabbinic authorities and synagogue leadership whose authority had rested on controlling interpretation of fixed sacred texts and liturgical recitation. The literary-revival reading implied that Hebrew's living status derived from NEW authorial production by potentially secular or heterodox writers, not from recitation and commentary on canonical texts. They would object to the claim that non-liturgical writing sustains Hebrew as genuinely living, but their voices are largely absent from the Haskalah historical record—their objections are structural rather than documented.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, rabbinic_liturgical_authorities, excluded,
    institutional, civilizational, constrained, continental).

% Jews for whom Yiddish, Ladino, or other diaspora languages were primary, who had lost or never acquired Hebrew literacy. The literary-revival reading presupposes and privileges Hebrew literacy, rendering illiterates structurally invisible. They are excluded because the reading does not address their claim to Hebrew's living status through oral/spoken competence in their actual languages.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, non_hebrew_literate_diaspora_jews, excluded,
    powerless, biographical, trapped, regional).

% The continuous chain of Hebrew textual tradition from biblical through rabbinic through medieval to early-modern Hebrew. Treated as a beneficiary (non-agent) in the sense that the constraint vindicates its endurance and generative capacity. The literary-revival reading asserts that this chain remains alive—capable of producing new texts within recognizable continuity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_text_canon, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_living_language__literary_revival_reading, hebrew_text_canon).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__literary_revival_reading, hebrew_literary_intelligentsia).
narrative_ontology:fixing_cost_class(hebrew_living_language__literary_revival_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes Hebrew as a vehicle for original intellectual and literary production within European Enlightenment frameworks, solving the coordination problem: how can Jewish communities participate in modern intellectual life while maintaining continuity with classical Hebrew tradition? The literary-revival reading coordinates this by treating generative written competence as the criterion for linguistic livingness.
% TRANSFER_FUNCTION: Transfers cultural authority from rabbinic interpreters of fixed canonical texts to contemporary literary authors and philosophers. The newly authored texts become evidence that Hebrew is not moribund but generatively alive. The constraint redirects prestige and intellectual leadership toward secular/heterodox Hebrew writers.
% ABSENT_VOICES: Liturgical authorities (who would argue Hebrew's livingness inheres in unbroken recitation practice, not in new authorship); non-Hebrew-literate diaspora Jews (who would dispute that literary-Hebrew literacy is the threshold for claiming Hebrew as living); native speakers of Hebrew in Ottoman Palestine and other vernacular contexts (whose speech would be invisible to a reading centered on written literary production).
% DISAPPEARANCE_RATIONALE: If the literary-revival reading vanished—i.e., if no one claimed that Hebrew could be living through Haskalah-era literary production—the burden of proof for Hebrew's livingness would shift entirely to other readings (liturgical continuity or native speech). The literary intelligentsia would lose a primary vehicle for claiming cultural authority. The Haskalah texts themselves remain; their canonical status and their role in imagining Hebrew as a modern language would collapse without the reading that treats them as evidence of linguistic livingness.
% FOUNDING_PROBLEM: How can Hebrew sustain itself as a culturally and intellectually alive language in the face of diaspora, the dominance of European vernaculars, and three centuries of liturgical-only use? Specifically for the Enlightenment intelligentsia: how can Jews participate in modern European intellectual life while maintaining Hebrew as a living medium rather than ceding all contemporary discourse to European languages?
% FOUNDING_PROBLEM_CORROBORATION: The Haskalah writers themselves (Mendelsohn, Krochmal, Smolenskin) attest the problem as live and claim their literary production as the solution. Modern linguistic scholarship (Rabin, Schwarzwald) and historical analysis (Stanislawski, Myers) outside the literary-intelligentsia seat corroborate that the founding problem was real and the literary-revival approach was one live response among competing readings. Liturgical authorities attest a different problem (the threat to tradition) and deny the founding problem as stated; native-speech advocates attest the problem was never solved by Haskalah—only by 20th-century Eretz Israel speech communities.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__literary_revival_reading_tests).
:- end_tests(hebrew_living_language__literary_revival_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08 final) because the constraint imposes no coercive transfer and no victim class — elite literary practice benefits its practitioners and the Hebrew tradition but does not extract from identifiable targets. Suppression is negligible (0.02) because participation is voluntary and the constraint does not forcibly exclude alternatives (people remain free to use Yiddish, Ladino, or other languages). Theater is modest (0.12) because the core claim (written literary production evidences livingness) is internally coherent and believed within its constituency, though externally contested. Accessibility of alternatives is very low (0.15) because once you accept the reading's premises, alternatives (speech-based livingness) appear inadequate or inauthentic. Resistance is moderate (0.35) because competing readings mount genuine structural objections and because later developments (native speech) eventually render the reading marginal. The measurement series shows slight rise in both extractiveness and theater over the 40-year interval, reflecting institutional entrenchment of the reading within Zionist ideology and its increasing use as a justification for Hebrew revival policy—extractiveness rises as the constraint shifts from descriptive claim to prescriptive norm.
 *
 * PERSPECTIVAL GAP:
 *   The literary intelligentsia and the Hebrew-text tradition see this constraint as self-evident coordination: the chain of texts speaks for itself, livingness is demonstrated by generative production. Liturgical authorities see it as a threat to their authority (if new authorship proves livingness, old recitation does not). Non-Hebrew speakers see themselves as excluded. The native-speech reading, once established, views the literary-revival reading as a way station—necessary in diaspora but superseded by actual speech. The engine computes divergence from the structural data: the literary intelligentsia's power, exit options, and beneficiary status produce one seat's classification; the liturgical establishment's institutional power and excluded role produce another; the observer's analytical position reveals all three as distinct seats with conflicting interests in what counts as living.
 *
 * DIRECTIONALITY LOGIC:
 *   The literary intelligentsia are the primary beneficiaries (d ≈ 0.2: they control the agenda, define the criterion, and collect prestige from authorship). The Hebrew text tradition is a vindicated proposition, not a beneficiary seat (it collects no rents and makes no decisions). European Jewish communities have moderate beneficiary status (d ≈ 0.35: they access modern thought through Hebrew, but they do not set the agenda). Liturgical establishments would be targets if they participated, but they are excluded from the conversation (d ≈ undefined: exclusion is the enforcement mechanism, and their d would only matter if they were seated stakeholders). Non-Hebrew speakers are absent (d ≈ undefined: the reading simply does not address them). This is a rare low-extractiveness, no-victim constraint: no agent is systematically harmed by the reading's operation, though its implicit hierarchy (literate > illiterate, elite author > community member) carries status asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to keep Hebrew alive in diaspora while participating in modernity) remains live throughout the Haskalah period (status: live). The reading's solution (literary production demonstrates livingness) initially works: Haskalah literature does sustain Hebrew competence and cultural vitality. However, the mandatrophy emerges post-interval when native speech becomes possible in Eretz Israel. Once native speakers exist, the literary-revival reading's answer to the founding problem becomes unnecessary—the problem is solved by actual speech, not by elite authorship. The constraint does not resolve mandatrophy within the interval; the interval closes before native-speech generation reaches scale. The reading's mandate (literary production is sufficient evidence of livingness) does not outlive its function in the 19th–early 20th century, because 20th-century linguistic practice (native Hebrew speech in Palestine and Israel) renders the mandate obsolete. This is a classic rope-to-piton pathway: the coordination function remains conceptually coherent but loses urgency and becomes theatrical (invoked to justify Hebrew revival policy after the problem is already solved by speech).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    written_vs_spoken_livingness,
    'Can a language be genuinely living through written literary production by an elite without native daily speakers? Or does linguistic livingness require generative speech competence in ordinary discourse?',
    'Definitional clarification from historical linguistics: is ''living language'' a structural-linguistic term (capable of generative production in the language''s own grammar) or a sociolinguistic term (regularly spoken by native communities)? The literary-revival reading assumes structural competence suffices; competing readings assume speech communities are necessary.',
    'If living requires speech, the literary-revival reading is a false summit — Haskalah texts demonstrate elite competence but not linguistic livingness. If structural generative competence suffices, the reading holds. Classification shifts from rope (viable coordination solution) to rope-with-theater (the texts perform livingness without demonstrating it) or even to piton (the reading persists through institutional inertia after the founding problem is solved by native speech).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(written_vs_spoken_livingness, conceptual, 'Whether linguistic livingness is defined by generative structural capacity or by speech-community presence.').

omega_variable(
    continuity_claim_reachability,
    'Does the Haskalah literary production constitute genuine continuity with biblical and rabbinic Hebrew, or does it represent a reconstructed/artificial standard learned through grammatical study rather than native transmission?',
    'Linguistic analysis of Haskalah Hebrew against biblical/rabbinic precedent: do the texts show spontaneous morpho-syntactic variation consistent with generative native competence, or are they grammatically conservative and consciously archaizing (learned-language artifacts)? Examination of author testimony about how they acquired Hebrew competence (through study vs. native exposure).',
    'If Haskalah Hebrew is genuinely generative within the inherited tradition, the reading demonstrates true continuity and linguistic livingness. If it is learned-language writing (based on study of classical texts rather than native generative competence), the constraint becomes false-summit: a constructed revival, not a living survival. Reclassification could move to snare (the reading conceals constructed status under a naturalness claim) or to scaffold (the revival is temporary, meant to transition to something else).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_claim_reachability, empirical, 'Whether Haskalah Hebrew is generative native continuity or learned-language reconstruction.').

omega_variable(
    kernel_reading_framing_ambiguity,
    'Is the kernel ''Hebrew as living language'' grounded in structural linguistic capacities, sociological speech-community facts, or political/cultural claims about national identity? Different framings yield different readings.',
    'Clarification of what the competing readings take the kernel to be fundamentally ABOUT: Are they disagreeing on facts (does Hebrew have native speakers? do written texts sustain it?) or on what counts as living (should we value liturgical continuity, written production, or speech)? Or on what ''Hebrew'' refers to (biblical standard, medieval developments, Haskalah creations)?',
    'If readings disagree on what the kernel is about, they are not coexisting readings of one constraint but instantiations of different constraints. If they agree on the referent and disagree on what makes it living, the committer structure holds. Misconstrual here leads to false constraint-family decomposition or misidentified kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'What the kernel ''Hebrew as living language'' fundamentally concerns: linguistic, sociological, or political facts?').

omega_variable(
    suppression_and_theater_low_floor,
    'Why are suppression and theater_ratio so low (0.02 and 0.12) for a constraint that persists through an elite practice and excludes non-literate voices?',
    'Clarify the distinctions: (a) Is the constraint enforced coercively, or do participants enter willingly? (b) Is the performance modest because the claim (written production proves livingness) is mostly accepted within the reading''s constituency, or because the reading itself is theatrical (performed for outsiders rather than believed internally)? (c) Is low theater a sign of a genuine rope, or a sign that this reading is marginal to how people actually think about Hebrew?',
    'If suppression and theater are genuinely low, the reading is rope (light-touch coordination without coercion). If they are low because the reading is marginal/theatrical, reclassification to piton is indicated. The low metrics depend on whether we measure relative to the reading''s own constituency (where belief is high) or relative to the broader Hebrew-language question (where this reading competes with others).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_and_theater_low_floor, empirical, 'Whether low suppression/theater indicate genuine rope or marginal/theatrical piton.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__literary_revival_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hebr_tr_t10, hebrew_living_language__literary_revival_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(hebr_tr_t20, hebrew_living_language__literary_revival_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(hebr_tr_t30, hebrew_living_language__literary_revival_reading, theater_ratio, 30, 0.14).
narrative_ontology:measurement(hebr_tr_t40, hebrew_living_language__literary_revival_reading, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__literary_revival_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(hebr_be_t10, hebrew_living_language__literary_revival_reading, base_extractiveness, 10, 0.07).
narrative_ontology:measurement(hebr_be_t20, hebrew_living_language__literary_revival_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(hebr_be_t30, hebrew_living_language__literary_revival_reading, base_extractiveness, 30, 0.09).
narrative_ontology:measurement(hebr_be_t40, hebrew_living_language__literary_revival_reading, base_extractiveness, 40, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_living_language__literary_revival_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__literary_revival_reading, 0.03).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% The kernel hebrew_living_language decomposes into three readings, each instantiating a distinct constraint with different structural properties. The literary_revival_reading is low-extractiveness elite coordination; the liturgical_continuity_reading is high-accessibility mountain or rope with institutional enforcement; the native_generation_reading is a high-extractiveness snare or tangled_rope once speech communities exist (coercive language-replacement dynamics). Each reading affects the others through its implicit claim about what counts as living: if literary production suffices, liturgical continuity is unnecessary; if speech is required, literary production is false-summit; if recitation suffices, native-speech advocates face an uphill institutional battle. All three readings link to the same ε-invariant referent (the standing arrangement of Hebrew across diaspora) but author different ε values (low for literary, high for liturgical-institutional, variable for speech-based) because they measure different aspects of what sustains the language.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
