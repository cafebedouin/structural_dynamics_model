% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__literary_revival_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: hebrew_living_language__literary_revival_reading
 *   human_readable: Hebrew Literary Revival Through Haskalah Written Production
 *   domain: historical linguistics / commitment systems
 *
 * SUMMARY:
 *   The literary-revival reading of Hebrew's 'living language' status asserts
 *   that generative written competence—the ability of educated Jewish
 *   intellectuals to compose new Hebrew texts in productive, grammatical
 *   ways—constitutes linguistic life, even in the absence of native daily
 *   speech. This reading emerged during the Haskalah (Jewish Enlightenment,
 *   ca. 1750–1880) as diaspora literati produced Hebrew novels, philosophy,
 *   correspondence, and criticism. The constraint is claimed as a rope
 *   (genuine coordination of dispersed communities through shared textual
 *   culture) while measured with low extractiveness and minimal
 *   suppression—elite literary practice with no coerced victims. The key
 *   structural ambiguity: whether writing-without-speech can authentically
 *   constitute a living language, or whether the reading is a temporary and
 *   contingent solution displaced by native-speaker emergence.
 *
 * KEY AGENTS:
 *   - Hebrew literati: organized, generationally-positioned, mobile—the agenda-setters who define 'living language' through their own productive writing
 *   - Religious establishment: institutional, civilizationally-positioned—benefits from the literati's assertion that Hebrew remains coherent and continuous through the textual chain
 *   - Diaspora communities: organized, biographically-positioned, constrained—consume the literati's intellectual output, gaining cultural prestige and access to Hebrew thought
 *   - Native Hebrew speakers (emerging late 19th century): powerless, identity-locked to nascent Palestinian Yishuv—structurally excluded from the literati's definition; their lived speech-based experience will contradict this reading's boundary
 *   - Linguistic scholars (later analytical observers): institutional, generationally-positioned—measure the reading's coherence as a definition of language vitality against comparative data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__literary_revival_reading, 0.15).
domain_priors:suppression_score(hebrew_living_language__literary_revival_reading, 0.08).
domain_priors:theater_ratio(hebrew_living_language__literary_revival_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_living_language__literary_revival_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__literary_revival_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__literary_revival_reading, "Hebrew Literary Revival Through Haskalah Written Production").
narrative_ontology:topic_domain(hebrew_living_language__literary_revival_reading, "historical linguistics / commitment systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__literary_revival_reading, 'bde13b22-56fd-4735-affb-c35843848306').
narrative_ontology:cs_kernel_codification('bde13b22-56fd-4735-affb-c35843848306', distributed).
narrative_ontology:cs_authority_grounding('bde13b22-56fd-4735-affb-c35843848306', expertise).
narrative_ontology:cs_interpretation_layer_present('bde13b22-56fd-4735-affb-c35843848306').
narrative_ontology:cs_reading_relation('bde13b22-56fd-4735-affb-c35843848306', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('bde13b22-56fd-4735-affb-c35843848306', hebrew_living_language__native_generation_reading, influences).
narrative_ontology:cs_axiom('bde13b22-56fd-4735-affb-c35843848306', foundational, written_generative_competence_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(written_generative_competence_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('bde13b22-56fd-4735-affb-c35843848306', written_generative_competence_constitutes_linguistic_life, instrumental).
narrative_ontology:cs_axiom('bde13b22-56fd-4735-affb-c35843848306', foundational, native_speaker_requirement_unnecessary_for_living_language).
narrative_ontology:cs_axiom_status(native_speaker_requirement_unnecessary_for_living_language, overridden).
narrative_ontology:cs_axiom_grounding('bde13b22-56fd-4735-affb-c35843848306', native_speaker_requirement_unnecessary_for_living_language, conventional).
narrative_ontology:cs_reference_frame('bde13b22-56fd-4735-affb-c35843848306', diaspora_hebrew_textual_continuity).
narrative_ontology:cs_drift_state('bde13b22-56fd-4735-affb-c35843848306', emergence_of_native_speakers_palestine_1880s, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bde13b22-56fd-4735-affb-c35843848306', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__literary_revival_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, hebrew_literati).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, religious_establishment).
narrative_ontology:constraint_beneficiary(hebrew_living_language__literary_revival_reading, diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, written_language_continuity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_living_language__literary_revival_reading, elite_textual_chain_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Educated Jewish intellectuals across diaspora communities who produce written Hebrew literature, philosophy, and correspondence throughout the 18th and 19th centuries. They coordinate via published texts, journals, and letters. They argue that their written generative competence—the ability to compose new Hebrew texts in grammatically productive ways—sustains Hebrew as a living language despite its absence from daily speech. For them, literacy and textual productivity are the markers of linguistic life.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, hebrew_literati, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__literary_revival_reading, hebrew_literati, beneficiary).

% Traditional rabbinic and liturgical authorities who maintain Hebrew's sacred textual status through liturgy, study, and halakha. They benefit from the literati's assertion that Hebrew remains 'living' and coherent, which reinforces the authority and accessibility of their own textual traditions. The literati's written production does not threaten their institutional position; it extends and validates the written textual chain they stewarded.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, religious_establishment, beneficiary,
    institutional, civilizational, mobile, regional).

% Jewish communities in Eastern Europe, Western Europe, and the Ottoman diaspora who speak Yiddish, Ladino, Aramaic, or local vernaculars. They benefit from the literati's work by gaining access to Hebrew literature, philosophy, and intellectual production in their own time—works that give them cultural continuity with the Jewish textual tradition and prestige within their own communities. They do not participate in the literati's written production but consume and value its output.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, diaspora_jewish_communities, beneficiary,
    organized, biographical, constrained, regional).

% Children born to Yishuv settlers in Palestine in the late 19th century who acquire Hebrew as a native spoken language through daily interaction—a generation that does not yet exist at the height of the Haskalah but emerges as a structural possibility by the 1880s. They are excluded from the literati's framing because they do not fit the definition of 'living language through written production'—their lived experience contradicts the reading's boundary. If their voices were heard, they would declare Hebrew alive through native speech generation, not elite literacy.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, native_hebrew_speakers, excluded,
    powerless, immediate, trapped, local).

% Later historical and comparative linguists who analyze whether generative written competence without native daily speech constitutes a 'living language'. They have no stake in the literati's work but assess it as evidence for or against competing definitions of language vitality. Their methodological tools and evidence base did not exist during the Haskalah itself.
narrative_ontology:constraint_stakeholder(hebrew_living_language__literary_revival_reading, linguistic_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__literary_revival_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_living_language__literary_revival_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed educated Jewish population across diaspora through a shared written language—Hebrew literature, philosophy, and correspondence serve as intellectual common currency, enabling collective self-perception as participants in a continuous textual tradition despite geographic separation and vernacular language diversity.
% TRANSFER_FUNCTION: Moves cultural prestige and textual authority from the liturgical and rabbinic establishment (who stewarded Hebrew's sacred status) to the literati (who assert control over Hebrew's definition as a 'living' language through their own generative writing). The literati gain authority to define what Hebrew is; the religious establishment gains validation that their text-centered tradition remains relevant.
% ABSENT_VOICES: Native Hebrew speakers (children born to settlers in Palestine, emerging late 19th century) are structurally excluded—their lived experience of Hebrew as daily generative speech challenges the reading's core claim that writing without native speech counts as linguistic life. They are not in the conversation because the reading predates them and the literati do not recognize native-speech competence as the relevant criterion.
% DISAPPEARANCE_RATIONALE: If the literary revival constraint disappeared—if the literati ceased writing generative Hebrew—the diaspora communities would lose access to new Hebrew intellectual products, but liturgical Hebrew would persist through prayer and study (religious establishment seat). The contest hinges on whether that residual liturgical continuity counts as the language 'living' or 'dead'. Native speakers (once they exist) would say the language lives regardless of literary output, through their own speech.
% FOUNDING_PROBLEM: How does Hebrew remain a coherent, continuous language when it has no native speakers and exists only in liturgical recitation, rabbinical study, and ancient texts? The literati's answer: through generative written production by educated speakers, who create new texts that demonstrate the language's grammar is productive and its vocabulary can be extended to modern concepts.
% FOUNDING_PROBLEM_CORROBORATION: The literati attest the problem is solved by their own literary output and defend the written-production reading. Later linguistic scholars (external observers) attest the founding problem was framed by the literati themselves and reflect on whether their answer was structurally sound (many conclude it was a contingent choice, not a necessity). Native speakers (emerging late 19th century) attest a different solution: the problem is solved by native childhood acquisition, not by elite literacy. The religious establishment attests the problem was always already solved by liturgical continuity, independent of new literature.
narrative_ontology:disappearance_verdict(hebrew_living_language__literary_revival_reading, contested).
narrative_ontology:founding_problem_status(hebrew_living_language__literary_revival_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__literary_revival_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__literary_revival_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__literary_revival_reading, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The extractiveness is very low (0.15 at interval end) because the literary-revival constraint operates through voluntary intellectual participation, not coerced transfers. No agent pays a structured cost to support the literati's writing; the beneficiary seat (religious establishment) gains validation without outlay. Suppression is minimal (0.08) because the constraint has no enforced closure—alternative definitions of language vitality (liturgical continuity, later native speech) are not actively suppressed, merely not adopted by the literati. Theater ratio is moderate (0.22) because literary production is materially real, but a growing share of the literati's self-narration is devoted to defending the writing-based boundary as the marker of linguistic life, rather than to the writing itself. The measurement series show shallow drift: extractiveness and suppression rise slightly as the reading becomes more consciously theorized (literati write more meta-commentary on language vitality), then stabilize as competing readings (native speech) begin to be articulated without displacing the literary reading. The theater ratio plateaus—the literati's output remains productive, but its function as evidence for linguistic continuity becomes increasingly contested by late century.
 *
 * PERSPECTIVAL GAP:
 *   The literati seat and the excluded native-speaker seat (emerging) should compute radically differently: from the literati's position, writing is linguistic life by definition—the constraint is genuine coordination around a shared textual tradition. From the native-speaker seat, the same constraint is a contingent historical accident, a particular way that diaspora communities happened to understand Hebrew, dissolved as soon as daily speech generation begins. The religious establishment sits between: they benefit from the literati's assertion but do not depend on it (liturgical continuity would persist independently). The engine's per-seat computation should show the literati as beneficiary (d near 0.0), native speakers as structurally excluded and later as contradictors (d undefined at the reading's temporal scope, emergent negation), and the religious establishment as incidentally-benefited (d near 0.3).
 *
 * DIRECTIONALITY LOGIC:
 *   This reading has no coercive extraction structure, no victim set, and no trapped agents—it operates entirely through voluntary intellectual participation and cultural prestige redistribution. The literati are self-positioned as agenda-setters (they define the criterion) and beneficiaries (they gain authority). The religious establishment benefits incidentally (their textual authority is validated). Diaspora communities benefit by gaining intellectual access. Native speakers are excluded not by active suppression but by temporal non-existence and by definitional incompatibility: when they emerge, the literati's written-production criterion will no longer apply to them, but they will not be suppressed—the reading will simply become one option among multiple valid definitions. This is the opposite of a snare: no exit closure, no alternative suppression, no concentrated victim. The low d values across the board reflect the absence of extraction structure entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The literary-revival reading exhibits a classic mandatrophy trajectory: the founding problem ('how does Hebrew remain alive?') is genuinely solved by the literati's generative writing in the 1750–1830 period. By the 1870s, however, the problem has been overtaken by events—native Hebrew speakers are appearing in Palestine, creating a new solution that does not depend on elite literacy. The literati's continued insistence on writing-based vitality becomes increasingly theatrical and historically positioned (a 'reading' rather than a fact). By 1880, the mandate is substantially dead: the problem is solved, but the constraint persists as a particular definition rather than a necessary structure. However, mandatrophy is not resolved through the literary-revival reading itself—it is resolved only when native-speaker generation becomes the dominant reading, at which point this reading becomes historically contingent. The current reading is pre-mandatrophy: the disappearance_verdict is contested precisely because the founding problem is being overtaken by native speech without yet being formally abandoned by the literati.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    writing_vs_speech_definition,
    'Does generative written competence, absent native daily speech, constitute a language ''living'' or merely ''living-in-writing''—a different linguistic phenomenon entirely?',
    'Comparative study of other written-only languages or language communities with long literate traditions but no native speakers (Classical Arabic, ecclesiastical Latin, Sanskrit in certain periods). Linguistic theory of the distinction between ''language vitality'' and ''textual continuity''.',
    'If writing and speech are definitionally independent, the literati''s reading is coherent and valid. If living language requires native-speech competence, this reading becomes historically contingent and displaced by native-generation reading once native speakers emerge.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(writing_vs_speech_definition, conceptual, 'Whether written-only continuity suffices for linguistic life or whether native speech is definitionally required.').

omega_variable(
    kernel_reading_foreclosure,
    'When native Hebrew speakers emerge in Palestine (1880s onward), does their native-speech competence logically foreclose the literary-revival reading, or do both readings remain valid as different frames for the same language?',
    'Historical record of how the literati and later linguists adjudicated the coexistence of written-revival and native-speech readings. Contemporary linguistics debates on multiple valid definitions of language vitality.',
    'If native speech forecloses writing-only vitality, the readings are mutually incompatible within one framework and the literary-revival reading becomes historically superseded. If both remain valid, the readings coexist as different valid perspectives on Hebrew''s status.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether native-speaker emergence forecloses or merely displaces the literary-revival reading.').

omega_variable(
    elite_vs_communal_language_definition,
    'Is a language that lives in elite educated writing but not in most community members'' daily speech truly the language of that community, or a specialized literary register?',
    'Sociolinguistic analysis of the Haskalah literati''s Hebrew versus the Hebrew of diaspora communities'' actual usage and understanding. Historical documents of how non-literati Jews experienced and understood Hebrew in their own vernaculars.',
    'If the literati''s Hebrew is a specialized register distinct from community Hebrew, the reading is validated as elite coordination but not as community language vitality. If the written Hebrew is the authoritative form community members recognize and aspire to, it may constitute community language life despite non-use in daily speech.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_vs_communal_language_definition, empirical, 'Whether written-elite Hebrew constitutes the actual language of diaspora communities or a literary register.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__literary_revival_reading, 1750, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1750, hebrew_living_language__literary_revival_reading, theater_ratio, 1750, 0.18).
narrative_ontology:measurement_basis(hebr_tr_t1750, projected).
narrative_ontology:measurement(hebr_tr_t1790, hebrew_living_language__literary_revival_reading, theater_ratio, 1790, 0.2).
narrative_ontology:measurement_basis(hebr_tr_t1790, observed).
narrative_ontology:measurement(hebr_tr_t1830, hebrew_living_language__literary_revival_reading, theater_ratio, 1830, 0.21).
narrative_ontology:measurement_basis(hebr_tr_t1830, observed).
narrative_ontology:measurement(hebr_tr_t1870, hebrew_living_language__literary_revival_reading, theater_ratio, 1870, 0.23).
narrative_ontology:measurement_basis(hebr_tr_t1870, observed).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__literary_revival_reading, theater_ratio, 1880, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t1880, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1750, hebrew_living_language__literary_revival_reading, base_extractiveness, 1750, 0.08).
narrative_ontology:measurement_basis(hebr_be_t1750, projected).
narrative_ontology:measurement(hebr_be_t1790, hebrew_living_language__literary_revival_reading, base_extractiveness, 1790, 0.12).
narrative_ontology:measurement_basis(hebr_be_t1790, observed).
narrative_ontology:measurement(hebr_be_t1830, hebrew_living_language__literary_revival_reading, base_extractiveness, 1830, 0.15).
narrative_ontology:measurement_basis(hebr_be_t1830, observed).
narrative_ontology:measurement(hebr_be_t1870, hebrew_living_language__literary_revival_reading, base_extractiveness, 1870, 0.16).
narrative_ontology:measurement_basis(hebr_be_t1870, observed).
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__literary_revival_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement_basis(hebr_be_t1880, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1750, hebrew_living_language__literary_revival_reading, suppression_requirement, 1750, 0.05).
narrative_ontology:measurement_basis(hebr_su_t1750, projected).
narrative_ontology:measurement(hebr_su_t1790, hebrew_living_language__literary_revival_reading, suppression_requirement, 1790, 0.07).
narrative_ontology:measurement_basis(hebr_su_t1790, observed).
narrative_ontology:measurement(hebr_su_t1830, hebrew_living_language__literary_revival_reading, suppression_requirement, 1830, 0.08).
narrative_ontology:measurement_basis(hebr_su_t1830, observed).
narrative_ontology:measurement(hebr_su_t1870, hebrew_living_language__literary_revival_reading, suppression_requirement, 1870, 0.09).
narrative_ontology:measurement_basis(hebr_su_t1870, observed).
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__literary_revival_reading, suppression_requirement, 1880, 0.08).
narrative_ontology:measurement_basis(hebr_su_t1880, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__literary_revival_reading, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__literary_revival_reading, 0.05).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__literary_revival_reading, hebrew_living_language__native_generation_reading).

% DUAL FORMULATION NOTE:
% The kernel 'hebrew_living_language' decomposes into three structurally distinct constraints, one for each reading of what counts as linguistic life. This story (literary-revival) has very low extractiveness and no victim set because it operates through voluntary elite intellectual coordination. The liturgical-continuity reading has similar low extractiveness but adds institutional religious authority as a beneficiary. The native-generation reading will have substantially different d-values across seats because it posits native childhood acquisition (a biological/social given) rather than elite choice. All three readings share the same kernel but have different ε values, beneficiary structures, and scope. They are linked via network.affects_constraints to signal the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
