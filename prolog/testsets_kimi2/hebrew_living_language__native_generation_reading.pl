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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Hebrew Native-Generation Vitality Constraint
 *   domain: historical_linguistics/language_revitalization
 *
 * SUMMARY:
 *   This constraint instantiates the native_generation_reading of the
 *   hebrew_living_language kernel: the claim that Hebrew became a living
 *   language only when a critical mass of native speakers began producing it
 *   generatively in daily life, delegitimizing prior liturgical and literary
 *   forms as insufficient. This reading was institutionalized in Zionist
 *   language planning and the Israeli state, which treated diaspora
 *   vernacularsâYiddish, Ladino, and Jewish Arabicâas obstacles to
 *   national coherence. The constraint acknowledges a strict-reachability
 *   break from Hebrew's prior forms and treats reconstruction (rather than
 *   continuity) as necessary for vitality. Sibling readings include
 *   liturgical_continuity_reading (Hebrew lives through unbroken liturgical
 *   recitation) and literary_revival_reading (Hebrew lives through Haskalah
 *   literary production).
 *
 * KEY AGENTS:
 *   - zionist_revivalist_planners: Primary agenda-setter (powerful/mobile) â designed the native-generation ideology and institutionalized it in language-planning bodies
 *   - state_education_system: Secondary agenda-setter (institutional/arbitrage) â enforces Hebrew monolingualism through schooling and public institutions
 *   - hebrew_monolingual_elite: Primary beneficiary (powerful/mobile) â collects status, economic opportunity, and cultural hegemony from Hebrew dominance
 *   - yiddish_vernacular_communities: Primary payer (moderate/constrained) â bear extraction through language shift, stigmatization, and cultural loss
 *   - ladino_vernacular_communities: Secondary payer (powerless/constrained) â marginalized by exclusion from institutional support and rapid intergenerational loss
 *   - mizrahi_arabic_speakers: Tertiary payer (powerless/constrained) â pressured to abandon Arabic in favor of Hebrew through schooling and military culture
 *   - comparative_linguists: Analytical observer (analytical/analytical) â evaluates whether the native-generation criterion is empirically necessary or ideologically motivated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.55).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.68).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew Native-Generation Vitality Constraint").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '33f7680b-fbbd-4068-a64a-2767893fcc13').
narrative_ontology:cs_kernel_codification('33f7680b-fbbd-4068-a64a-2767893fcc13', formalized).
narrative_ontology:cs_authority_grounding('33f7680b-fbbd-4068-a64a-2767893fcc13', expertise).
narrative_ontology:cs_interpretation_layer_present('33f7680b-fbbd-4068-a64a-2767893fcc13').
narrative_ontology:cs_reading_relation('33f7680b-fbbd-4068-a64a-2767893fcc13', hebrew_living_language__liturgical_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('33f7680b-fbbd-4068-a64a-2767893fcc13', hebrew_living_language__literary_revival_reading, forecloses).
narrative_ontology:cs_axiom('33f7680b-fbbd-4068-a64a-2767893fcc13', foundational, native_generative_speech_required).
narrative_ontology:cs_axiom_status(native_generative_speech_required, holdable).
narrative_ontology:cs_axiom_grounding('33f7680b-fbbd-4068-a64a-2767893fcc13', native_generative_speech_required, conventional).
narrative_ontology:cs_axiom('33f7680b-fbbd-4068-a64a-2767893fcc13', secondary, non_native_competence_insufficient).
narrative_ontology:cs_axiom_status(non_native_competence_insufficient, holdable).
narrative_ontology:cs_axiom_grounding('33f7680b-fbbd-4068-a64a-2767893fcc13', non_native_competence_insufficient, conventional).
narrative_ontology:cs_reference_frame('33f7680b-fbbd-4068-a64a-2767893fcc13', native_generative_vitality).
narrative_ontology:cs_drift_state('33f7680b-fbbd-4068-a64a-2767893fcc13', post_statehood_consolidation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('33f7680b-fbbd-4068-a64a-2767893fcc13', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_monolingual_elite).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_vernacular_communities).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_vernacular_communities).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, mizrahi_arabic_speakers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designed and propagated the ideological framework that defines Hebrew as living only through native generative daily speech. Institutionalized this definition in language-planning bodies, settlement patterns, and educational policy in the Yishuv and early Israeli state.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, zionist_revivalist_planners, agenda_setter,
    powerful, generational, mobile, national).

% Enforces Hebrew monolingualism through mandatory schooling, teacher training, language policing in public institutions, and allocation of educational resources exclusively to Hebrew native acquisition. Could theoretically reform to multilingualism but is bound to the nation-building mandate.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, state_education_system, agenda_setter,
    institutional, generational, arbitrage, national).

% Holds the highest linguistic capital in the Israeli social and economic hierarchy. Their native generative competence is treated as the unmarked standard, conferring automatic legitimacy in public discourse, academia, and state administration.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_monolingual_elite, beneficiary,
    powerful, biographical, mobile, national).

% Jewish communities for whom Yiddish was the historical daily vernacular. Subjected to institutional exclusion, social stigmatization as diasporic and weak, and educational pressure to shift to Hebrew. Transmission was disrupted across generations.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_vernacular_communities, payer,
    moderate, generational, constrained, national).

% Sephardic Jewish communities with Ladino as a heritage language. Experienced near-total absence of institutional support, cultural marginalization, and pressure to adopt Hebrew in public and educational life, leading to rapid intergenerational language loss.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_vernacular_communities, payer,
    powerless, generational, constrained, national).

% Mizrahi Jewish immigrants and their descendants whose Arabic dialects were treated as foreign, primitive, or politically suspect. Pressured to abandon Arabic in favor of Hebrew through schooling, military culture, and media, severing intergenerational transmission.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, mizrahi_arabic_speakers, payer,
    powerless, generational, constrained, national).

% Study language vitality, revival, and endangerment cross-linguistically. Positioned to evaluate whether the native-generation criterion is an empirical necessity of language science or an ideological construct serving a specific nation-building project.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__native_generation_reading, hebrew_monolingual_elite).
narrative_ontology:fixing_cost_class(hebrew_living_language__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the genuine collective-action problem of unifying a linguistically fragmented diaspora population under a single modern vernacular, enabling modern state administration, military cohesion, and shared public education.
% TRANSFER_FUNCTION: Moves linguistic capital, educational resources, public legitimacy, and intergenerational transmission from diaspora Jewish vernacularsâYiddish, Ladino, and Arabicâto Hebrew and its native-speaking beneficiaries.
% ABSENT_VOICES: Yiddishist cultural leaders, Ladino preservationists, and Mizrahi Arabic-speaking intellectuals were structurally excluded from the language-planning rooms of the Yishuv and early Israeli state; their descendants remain underrepresented in the linguistic hierarchy and largely absent from policy archives.
% DISAPPEARANCE_RATIONALE: If the constraint vanishedâif Hebrew's 'living' status no longer required native generative speech and liturgical or literary continuity were deemed equally sufficientâthe ideological foundation for suppressing Yiddish, Ladino, and Arabic in Israeli institutions would collapse; diaspora language communities would demand and likely secure public recognition, media space, and transmission support.
% FOUNDING_PROBLEM: The absence of a shared modern spoken vernacular among Jews in the late 19th and early 20th centuries, which complicated national mobilization, modern state-building, and territorial claim in a context of extreme linguistic diversity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish nationalism and linguists outside the Zionist institutional framework attest that diaspora Jews lacked a single modern spoken lingua franca. These same outside scholars document that Hebrew native-speaker communities were successfully established by the mid-20th century, and that the constraint now persists as cultural hierarchy and status extraction rather than vital coordination.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.55) because the constraint genuinely solved a coordination problemâcreating a unified modern vernacularâwhile also systematically transferring linguistic capital and status from diaspora communities to Hebrew. Suppression is high (0.68) because the persistence of this specific vitality definition required active marginalization of Yiddish, Ladino, and Arabic through schooling, media policy, and social stigma. Theater is low-moderate (0.25): the native-speaker community was successfully created, though some enforcement became performative once Hebrew dominance was secure. Resistance (0.42) reflects persistent but ultimately overwhelmed cultural pushback. The measurement series share one time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (Yiddish, Ladino, and Mizrahi Arabic communities) experience this constraint as cultural erasure and the extraction of their intergenerational linguistic capital. The beneficiary seat (Hebrew monolingual elite) experiences it as natural national revival and legitimate status acquisition. The agenda-setter seats experience it as successful state-building. The engine computes this divergence from the structural asymmetry in power, exit options, and role.
 *
 * DIRECTIONALITY LOGIC:
 *   The hebrew_monolingual_elite is the structural beneficiary (collects status and opportunity, mobile exit, low directionality). The diaspora vernacular communities are the structural targets (bear language-shift costs, constrained or powerless, high directionality). The state_education_system and zionist_revivalist_planners sit near the agenda-setting middle: they do not collect the extraction directly but administer the mechanism. Comparative_linguists sit at the analytical pole with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâthe absence of a shared modern Jewish vernacularâwas dead by the mid-20th century, yet the constraint persists in suppressing alternatives. This prevents mislabeling the arrangement as pure coordination (it continued to extract after the coordination was achieved) and prevents mislabeling it as pure extraction (it did solve a real fragmentation problem). The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges flags the constraint as a tangled rope with potential zombie capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_decomposition_nativeness,
    'This constraint is one reading of the hebrew_living_language kernel. Does the exclusivity of the native-generation criterion logically foreclose the liturgical-continuity and literary-revival readings within a single definitional framework, or can they coexist as complementary perspectives?',
    'Formal analysis of whether the predicate ''living language'' can support multiple sufficient conditions simultaneously, or whether the native-generation reading''s ''only'' operator makes it definitionally exclusive.',
    'If definitionally exclusive, the readings are logically competing commitment frameworks rather than complementary views; if coexistent, the constraint''s extraction is reduced because it is one legitimate classification among many.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_decomposition_nativeness, conceptual, 'Structural relationship between native-generation reading and sibling kernel readings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of Yiddish and Ladino structural (state schooling, media policy, legal discrimination, economic incentives) or internalized (parental refusal to transmit, community shame, self-censorship, identity fusion with Hebrew)?',
    'Generational sociolinguistic surveys measuring language transmission rates, self-reported language attitudes, and post-exit behavior among descendants of victim communities.',
    'If primarily internalized, the constraint''s effective suppression is higher than structural measures suggestâthe target communities carry the suppression with them even if state policy changes; if structural, policy reform could substantially revitalize the languages.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    coordination_extraction_separability,
    'Could the coordination function of a unified national language have been achieved without the asymmetric extraction from diaspora vernacular communities, or was the suppression of Yiddish, Ladino, and Arabic inseparable from the nation-building project?',
    'Comparative analysis of multilingual nation-building models (e.g., Switzerland, India) and examination of historical alternatives proposed within the Zionist movement itself.',
    'If separable, the constraint is a tangled rope where genuine coordination is contaminated by unnecessary extraction; if inseparable, the extraction is the necessary cost of coordination and effective extraction is damped accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Separability of coordination and extraction components').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__native_generation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hebr_tr_t15, hebrew_living_language__native_generation_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(hebr_tr_t30, hebrew_living_language__native_generation_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(hebr_tr_t45, hebrew_living_language__native_generation_reading, theater_ratio, 45, 0.23).
narrative_ontology:measurement(hebr_tr_t60, hebrew_living_language__native_generation_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(hebr_tr_t75, hebrew_living_language__native_generation_reading, theater_ratio, 75, 0.25).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__native_generation_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(hebr_be_t15, hebrew_living_language__native_generation_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(hebr_be_t30, hebrew_living_language__native_generation_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(hebr_be_t45, hebrew_living_language__native_generation_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(hebr_be_t60, hebrew_living_language__native_generation_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(hebr_be_t75, hebrew_living_language__native_generation_reading, base_extractiveness, 75, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_living_language__native_generation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(hebr_su_t15, hebrew_living_language__native_generation_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(hebr_su_t30, hebrew_living_language__native_generation_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(hebr_su_t45, hebrew_living_language__native_generation_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement(hebr_su_t60, hebrew_living_language__native_generation_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(hebr_su_t75, hebrew_living_language__native_generation_reading, suppression_requirement, 75, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, literary_revival_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three structurally distinct constraints under the epsilon-invariance principle. liturgical_continuity_reading addresses ritual practice with low extractiveness; literary_revival_reading addresses written generative competence with moderate extractiveness; native_generation_reading addresses native daily speech and state enforcement with moderate-high extractiveness. Their epsilon values differ because their referents differ, and they are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
