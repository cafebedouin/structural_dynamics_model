% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
 *   human_readable: Hebrew Living Language — Native Generation Requirement
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint story models the 'native generation reading' of the
 *   Hebrew living language kernel: the claim that Hebrew becomes a living
 *   language only when native speakers produce daily speech generatively, not
 *   through memorized liturgical recitation or literary production alone.
 *   Historically, this reading powered the Zionist language revival
 *   (1880s–1948) and became state policy in early Israel. It coordinates a
 *   genuine collective action problem — creating a shared vernacular for a
 *   linguistically fragmented immigrant population — but extracts
 *   asymmetrically by suppressing Yiddish, Ladino, Judeo-Arabic, and other
 *   diaspora vernaculars. The constraint requires active enforcement (school
 *   systems, youth movements, public signage, workplace norms) and has no
 *   sunset clause. The metrics reflect the interval 1880–1990: extraction and
 *   suppression rise through the Mandate period and peak at state founding,
 *   then moderate slightly as Hebrew becomes entrenched. Theater ratio rises
 *   as ideological fervor shifts to institutional maintenance.
 *
 * KEY AGENTS:
 *   - zionist_institutional_leadership: agenda_setter (institutional/generational/arbitrage/global) — sets revival policy, controls education and public institutions
 *   - hebrew_education_establishment: agenda_setter/beneficiary (organized/biographical/constrained/national) — implements Hebrew-only schooling, gains professional status
 *   - new_yishuv_settler_population: beneficiary (organized/biographical/mobile/national) — gains shared vernacular, participates in nation-building
 *   - yiddish_speaking_communities: payer (moderate/biographical/identity_locked/national) — lose intergenerational transmission, pressured to abandon Yiddish
 *   - ladino_speaking_communities: payer (moderate/biographical/identity_locked/national) — lose intergenerational transmission, pressured to abandon Ladino
 *   - mizrahi_arabic_speaking_jews: payer (powerless/biographical/trapped/national) — lose Judeo-Arabic vernaculars, marginalized in Hebrew-dominant public sphere
 *   - traditional_cheder_educators: payer (moderate/biographical/constrained/local) — lose pedagogical authority, curriculum replaced by Hebrew-language secular/Zionist schools
 *   - linguistic_scholars: observer (analytical/generational/analytical/universal) — analyze revival as case study in language planning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.48).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.52).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew Living Language — Native Generation Requirement").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '35680d2a-b94f-4880-bdb6-7603b0d8b43e').
narrative_ontology:cs_kernel_codification('35680d2a-b94f-4880-bdb6-7603b0d8b43e', distributed).
narrative_ontology:cs_authority_grounding('35680d2a-b94f-4880-bdb6-7603b0d8b43e', extraction).
narrative_ontology:cs_interpretation_layer_present('35680d2a-b94f-4880-bdb6-7603b0d8b43e').
narrative_ontology:cs_reading_relation('35680d2a-b94f-4880-bdb6-7603b0d8b43e', hebrew_living_language__liturgical_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('35680d2a-b94f-4880-bdb6-7603b0d8b43e', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_axiom('35680d2a-b94f-4880-bdb6-7603b0d8b43e', foundational, vernacular_revival_requires_native_acquisition).
narrative_ontology:cs_axiom_status(vernacular_revival_requires_native_acquisition, holdable).
narrative_ontology:cs_axiom_grounding('35680d2a-b94f-4880-bdb6-7603b0d8b43e', vernacular_revival_requires_native_acquisition, empirically_contingent).
narrative_ontology:cs_axiom('35680d2a-b94f-4880-bdb6-7603b0d8b43e', foundational, liturgical_recitation_insufficient_for_living_language).
narrative_ontology:cs_axiom_status(liturgical_recitation_insufficient_for_living_language, holdable).
narrative_ontology:cs_axiom_grounding('35680d2a-b94f-4880-bdb6-7603b0d8b43e', liturgical_recitation_insufficient_for_living_language, deontological).
narrative_ontology:cs_reference_frame('35680d2a-b94f-4880-bdb6-7603b0d8b43e', pre_revival_diaspora_multilingualism).
narrative_ontology:cs_drift_state('35680d2a-b94f-4880-bdb6-7603b0d8b43e', post_state_founding_hegemony, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('35680d2a-b94f-4880-bdb6-7603b0d8b43e', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, zionist_institutional_leadership).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_education_establishment).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, new_yishuv_settler_population).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_speaking_communities).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_speaking_communities).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, mizrahi_arabic_speaking_jews).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, traditional_cheder_educators).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, language_is_daily_speech).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, vernacular_revival_requires_native_acquisition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets Hebrew revival policy through the Jewish Agency, Histadrut, and later state institutions. Controls education curriculum, public sector language requirements, and immigration absorption programs. Gains nation-building coherence and institutional authority. Could pivot to multilingual policy but would lose ideological legitimacy.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, zionist_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Implements Hebrew-only schooling, teacher training, and curriculum design. Gains professional status and state funding tied to Hebrew monolingualism. Constrained exit: professional identity and livelihood depend on Hebrew-language education system.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_education_establishment, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__native_generation_reading, hebrew_education_establishment, beneficiary).

% Immigrant communities adopting Hebrew as shared vernacular. Gain communicative unity, participation in public life, and national belonging. Mobile exit: could maintain diaspora languages in private or emigrate, but strong social pressure to adopt Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, new_yishuv_settler_population, beneficiary,
    organized, biographical, mobile, national).

% Historically the largest Jewish vernacular (millions of speakers). Pressured by 'Hebrew labor' ideology, school bans, street signage laws, youth movement shaming. Intergenerational transmission collapses within one generation in Palestine/Israel. Identity-locked: Yiddish is fused to Ashkenazi communal identity, religious practice, and socialist Bundist politics — abandoning it feels like betrayal.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speaking_communities, payer,
    moderate, biographical, identity_locked, national).

% Sephardi vernacular with centuries of literary and oral tradition. Marginalized in Zionist narratives as 'diaspora language.' Speakers pressured to Hebrewize names, abandon Ladino in public and schools. Identity-locked: Ladino carries Sephardi communal memory and religious practice.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speaking_communities, payer,
    moderate, biographical, identity_locked, national).

% Judeo-Arabic vernaculars (Baghdadi, Moroccan, Yemenite, etc.) with no institutional support in Zionist polity. Arrive as immigrants 1948–1950s, placed in transit camps, educated in Hebrew-only schools. No resources for vernacular maintenance. Trapped: powerless, no exit to diaspora communities (most left Arab countries), structural marginalization in Hebrew-dominant public sphere.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, mizrahi_arabic_speaking_jews, payer,
    powerless, biographical, trapped, national).

% Teachers in traditional religious elementary schools (cheder) using Yiddish or Hebrew-as-holy-tongue pedagogy. Displaced by secular Hebrew-language school system. Constrained exit: can join new system (retraining) or retreat to ultra-Orthodox enclaves maintaining Yiddish.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, traditional_cheder_educators, payer,
    moderate, biographical, constrained, local).

% Analyze Hebrew revival as unique case of language planning and native acquisition revival. No material stake in outcome. Provide comparative data on language shift, revival mechanics, and policy outcomes.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, linguistic_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__native_generation_reading, zionist_institutional_leadership).
narrative_ontology:fixing_cost_class(hebrew_living_language__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared vernacular for a linguistically fragmented immigrant population building a polity — solves the Babel problem of the yishuv where dozens of languages prevented common civic, military, and economic life.
% TRANSFER_FUNCTION: Moves communicative authority, educational resources, public sphere access, and intergenerational transmission from diaspora vernaculars (Yiddish, Ladino, Judeo-Arabic) to Hebrew. The transfer is effected through school policy, public employment requirements, signage laws, and youth movement socialization.
% ABSENT_VOICES: Diaspora Jewish communities outside Palestine/Israel (especially American Yiddishists, European Bundists, North African Judeo-Arabic speakers who remained in situ) — they would object to the framing of their vernaculars as 'dead' or 'diaspora baggage' but were not present in the yishuv/Israeli policy arena. Ultra-Orthodox communities in Palestine/Israel who rejected Zionist Hebrew — they were present but structurally excluded from the consensus by the 'Hebrew labor' and state-building framework.
% DISAPPEARANCE_RATIONALE: If the native-generation requirement vanished overnight, Hebrew would remain the dominant language (entrenched native speaker base), but the ideological prohibition on diaspora vernaculars in education and public life would lift. Yiddish, Ladino, and Judeo-Arabic might see institutional support for heritage maintenance. The polity would reorganize toward multilingual citizenship — a substantial rearrangement of the linguistic architecture of the state.
% FOUNDING_PROBLEM: The yishuv (pre-state Jewish community in Palestine) faced a coordination crisis: immigrants from Europe, North Africa, the Middle East, and Yemen shared no common spoken language. Yiddish dominated numerically but was ideologically rejected by Zionist leadership as 'diaspora'; Arabic was the regional language but politically charged; no other vernacular had critical mass. A shared spoken language was needed for collective defense, labor organization, civic discourse, and nation-building.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (no shared vernacular for nation-building) is corroborated as dead by: (1) Hebrew is now the native language of ~7 million speakers in a sovereign state — the coordination problem is solved; (2) sociolinguistic surveys confirm Hebrew's dominance across all domains; (3) the Academy of the Hebrew Language and state education system maintain Hebrew not because fragmentation threatens, but as ideological and institutional continuity. The Zionist leadership's own documents (Ben-Gurion, 1950s) acknowledge the revival's success. No credible source outside the benefiting institutional structure argues the founding problem remains live.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.48) reflects moderate but real extraction: the coordination function (shared vernacular for nation-building) is genuine, but the asymmetric suppression of existing vernaculars — especially Yiddish and Ladino which had millions of native speakers — constitutes extraction from those communities. Suppression (0.52) is structural: Hebrew-only policies in schools, workplaces, and public space actively marginalized alternatives. Theater ratio (0.28) reflects that early revival was ideologically driven (low theater), but post-1948 institutional maintenance developed performative elements (ceremonial Hebrew, 'purity' campaigns). Accessibility collapse (0.35) is moderate: Yiddish and Ladino persisted in enclaves and diaspora, but within the yishuv/Israel the vernacular space collapsed. Resistance (0.58) is significant: Yiddishists, Bundists, ultra-Orthodox, and Mizrahi communities resisted Hebrew monolingualism. The claim of tangled_rope fits: genuine coordination + asymmetric extraction + active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (Zionist leadership) experiences this as rope: a coordination problem solved by collective will. The payer seats (Yiddish/Ladino/Mizrahi speakers) experience it as snare: their vernaculars are suppressed to make space for Hebrew. The engine computes this divergence from the structural data — beneficiary declarations for the leadership and education establishment, victim declarations for vernacular communities, and the identity_locked/constrained exit options for payers.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist institutional leadership and Hebrew education establishment are beneficiaries: they gain institutional control, professional status, and nation-building coherence (d near 0.15–0.25). New Yishuv settlers are net beneficiaries: they gain a shared vernacular, though they bear learning costs (d ~ 0.35). Yiddish and Ladino speakers are primary victims: identity_locked exit (language fused to communal identity) makes exit near-impossible; they bear intergenerational transmission loss (d ~ 0.85). Mizrahi Arabic speakers are trapped: powerless, no institutional support for their vernaculars, exit blocked by structural marginalization (d ~ 0.95). Traditional cheder educators are constrained payers: their pedagogical role is displaced (d ~ 0.75). Scholars are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating a shared vernacular for a linguistically fragmented immigrant population building a polity) was live in 1880–1948. By 1990, Hebrew is the dominant native language of a sovereign state — the coordination problem is solved. Yet the constraint persists: Hebrew-only education, public signage laws, Academy of the Hebrew Language prescriptive authority. The mandate has outlived its founding function but is maintained by institutional inertia and ideological commitment — classic mandatrophy. The constraint does not declare mandatrophy_resolved because the arrangement persists without its founding justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested kernel ''hebrew_living_language'', or does it exhaust the kernel''s meaning?',
    'Compare the structural metrics and victim/beneficiary sets of sibling readings: liturgical_continuity_reading and literary_revival_reading produce different ε profiles and different victim sets.',
    'If the kernel supports multiple readings with different classifications, the constraint''s type is reading-indexed, not kernel-indexed. This reading instantiates tangled_rope with moderate extraction; the liturgical reading may compute as mountain or rope with near-zero extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the native generation reading is the only valid reading of the Hebrew living language kernel.').

omega_variable(
    yiddish_ladino_suppression_mechanism,
    'Was the suppression of Yiddish and Ladino structural (policy, schooling, public sphere exclusion) or internalized (speakers abandoning heritage languages voluntarily for Hebrew)?',
    'Historical analysis of language policy in Mandate Palestine and early Israel: school curricula, street signage, workplace rules, youth movement pressure, and speaker testimony on language shift motivations.',
    'If structural, suppression is higher and the constraint is more extractive; if internalized, the constraint''s suppression metric reflects partly carried suppression after exit from the diaspora context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(yiddish_ladino_suppression_mechanism, empirical, 'Structural vs. internalized suppression of diaspora vernaculars during Hebrew revival.').

omega_variable(
    strict_reachability_break_nature,
    'Does the acknowledged break in native transmission (no native speakers 200–1700 CE) represent a true discontinuity or a transformed continuity?',
    'Comparative historical linguistics: assess whether liturgical Hebrew competence provided sufficient structural scaffolding for generative revival, or whether the revival required de novo reconstruction from textual corpora.',
    'If true discontinuity, the constraint''s accessibility_collapse is higher (alternatives genuinely unavailable); if transformed continuity, the liturgical_continuity_reading has stronger structural claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strict_reachability_break_nature, conceptual, 'Whether the native transmission gap constitutes a strict reachability break or a continuity under transformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 1880, 1990).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__native_generation_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_living_language__native_generation_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_living_language__native_generation_reading, theater_ratio, 1920, 0.18).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_living_language__native_generation_reading, theater_ratio, 1948, 0.3).
narrative_ontology:measurement(hebr_tr_t1967, hebrew_living_language__native_generation_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(hebr_tr_t1990, hebrew_living_language__native_generation_reading, theater_ratio, 1990, 0.28).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__native_generation_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(hebr_be_t1900, hebrew_living_language__native_generation_reading, base_extractiveness, 1900, 0.22).
narrative_ontology:measurement(hebr_be_t1920, hebrew_living_language__native_generation_reading, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(hebr_be_t1948, hebrew_living_language__native_generation_reading, base_extractiveness, 1948, 0.48).
narrative_ontology:measurement(hebr_be_t1967, hebrew_living_language__native_generation_reading, base_extractiveness, 1967, 0.52).
narrative_ontology:measurement(hebr_be_t1990, hebrew_living_language__native_generation_reading, base_extractiveness, 1990, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__native_generation_reading, suppression_requirement, 1880, 0.05).
narrative_ontology:measurement(hebr_su_t1900, hebrew_living_language__native_generation_reading, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement(hebr_su_t1920, hebrew_living_language__native_generation_reading, suppression_requirement, 1920, 0.3).
narrative_ontology:measurement(hebr_su_t1948, hebrew_living_language__native_generation_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(hebr_su_t1967, hebrew_living_language__native_generation_reading, suppression_requirement, 1967, 0.58).
narrative_ontology:measurement(hebr_su_t1990, hebrew_living_language__native_generation_reading, suppression_requirement, 1990, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__native_generation_reading, 0.08).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% Part of the Hebrew living language constraint family. This reading (native_generation) instantiates tangled_rope with moderate extraction (ε=0.48) and victims among diaspora vernacular speakers. The liturgical_continuity_reading likely computes as rope or mountain (near-zero extraction, no victims). The literary_revival_reading likely computes as scaffold (transitional coordination function, written modality). All three readings share the kernel but decompose into structurally distinct constraints per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_living_language__native_generation_reading, institutional, 0.15).
constraint_indexing:directionality_override(hebrew_living_language__native_generation_reading, organized, 0.35).
constraint_indexing:directionality_override(hebrew_living_language__native_generation_reading, moderate, 0.85).
constraint_indexing:directionality_override(hebrew_living_language__native_generation_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
