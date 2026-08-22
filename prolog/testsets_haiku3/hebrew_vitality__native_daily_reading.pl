% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__native_daily_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Hebrew Native Vitality Doctrine (Vernacular-First Reading)
 *   domain: sociolinguistic/cultural-nationalist
 *
 * SUMMARY:
 *   This constraint instantiates the native-daily-reading of the contested
 *   Hebrew vitality kernel. The reading holds that a language only truly
 *   'lives' when native speakers acquire it from birth and use it as their
 *   primary medium of daily life. Under this reading, Hebrew's liturgical
 *   continuity across diaspora Judaism (2000+ years of unbroken prayer,
 *   study, and textual use) does not constitute vitality—it constitutes
 *   preservation. The Zionist state-building project adopted and enforced
 *   this reading, making vernacular native-Hebrew-from-birth the exclusive
 *   criterion for authentic Hebrew vitality. This created a tangled
 *   structure: genuine coordination function (building a modern national
 *   language) layered with asymmetric extraction (delegitimating liturgical
 *   traditions, imposing linguistic resocialization on diaspora immigrants,
 *   creating insider/outsider status based on nativeness). The measurement
 *   series track the doctrine's accumulation of enforcement infrastructure
 *   from 1880 (pre-state, ideational) through 1960 (post-statehood,
 *   institutional and coercive). Theater ratio rises as public justification
 *   (security-review-like) grows relative to the doctrine's core function
 *   (excluding non-native competence).
 *
 * KEY AGENTS:
 *   - Zionist state builders and institutional Hebrew planners (institutional power, agenda-setter) — define and enforce the criterion; benefit from monopoly on vitality authority.
 *   - Secular Hebrew speakers and Israeli native speakers (organized power, beneficiary) — enjoy insider status; their linguistic practice is validated as authentic.
 *   - Liturgical Hebrew traditionalists and scholars (moderate power, payer) — bear cost through marginalization; their practice is reclassified as preservation, not life.
 *   - Diaspora Jewish communities (powerless, payer/excluded) — lose authority over their own Hebrew literacy; childhood non-native acquisition triggers authenticity judgment.
 *   - Language Academy of Israel (institutional power, agenda-setter + beneficiary) — administers the criterion; gains authority and resources from enforcing it.
 *   - Religiously observant immigrants (moderate power, identity-locked payer) — trapped between religious identity (liturgical Hebrew) and state pressure (vernacular nativeness).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.62).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.71).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Hebrew Native Vitality Doctrine (Vernacular-First Reading)").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistic/cultural-nationalist").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, 'f464e9a0-a3f5-421b-adaf-c4d0a15efe29').
narrative_ontology:cs_kernel_codification('f464e9a0-a3f5-421b-adaf-c4d0a15efe29', distributed).
narrative_ontology:cs_authority_grounding('f464e9a0-a3f5-421b-adaf-c4d0a15efe29', extraction).
narrative_ontology:cs_interpretation_layer_present('f464e9a0-a3f5-421b-adaf-c4d0a15efe29').
narrative_ontology:cs_reading_relation('f464e9a0-a3f5-421b-adaf-c4d0a15efe29', hebrew_vitality__liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('f464e9a0-a3f5-421b-adaf-c4d0a15efe29', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('f464e9a0-a3f5-421b-adaf-c4d0a15efe29', foundational, nativeness_is_vitality).
narrative_ontology:cs_axiom_status(nativeness_is_vitality, holdable).
narrative_ontology:cs_axiom_grounding('f464e9a0-a3f5-421b-adaf-c4d0a15efe29', nativeness_is_vitality, conventional).
narrative_ontology:cs_axiom('f464e9a0-a3f5-421b-adaf-c4d0a15efe29', foundational, liturgical_use_is_preservation_not_life).
narrative_ontology:cs_axiom_status(liturgical_use_is_preservation_not_life, holdable).
narrative_ontology:cs_axiom_grounding('f464e9a0-a3f5-421b-adaf-c4d0a15efe29', liturgical_use_is_preservation_not_life, empirically_contingent).
narrative_ontology:cs_reference_frame('f464e9a0-a3f5-421b-adaf-c4d0a15efe29', vernacular_nativeness_as_vitality).
narrative_ontology:cs_drift_state('f464e9a0-a3f5-421b-adaf-c4d0a15efe29', contemporary_post_1960_israel, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f464e9a0-a3f5-421b-adaf-c4d0a15efe29', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_builders).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, secular_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_hebrew_traditionalists).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, diaspora_jewish_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, language_academy_of_israel).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, religiously_observant_immigrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drive Hebrew language revival policy in the emerging Israeli state (1920s-1950s). Set school curricula, institutional norms, and public discourse requirements around spoken Hebrew as the marker of authentic vitality. Fund lexical expansion committees, standardize pronunciation, and marginalize liturgical Hebrew as 'dead' or 'preserved' rather than 'living.' Benefit from the constraint by establishing linguistic continuity with ancient Jewry while breaking with diaspora practices, consolidating a national identity distinct from religious authority.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_builders, agenda_setter,
    institutional, generational, arbitrage, national).

% Adopt vernacular Hebrew in daily life, schools, and public institutions. Benefit from belonging to a 'vitality' class that is recognized as authentically Jewish and forward-looking. Experience their linguistic practice as the legitimate form of Jewishness; liturgical Hebrew becomes something to study as heritage, not inhabit as daily identity.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, secular_hebrew_speakers, beneficiary,
    organized, biographical, mobile, national).

% Maintain Hebrew's religious-liturgical use as the primary site of its vitality. Bear the cost of the doctrine through institutional marginalization: their scholarly tradition is re-classified as 'preservation' (passive, backward-looking) rather than 'living use.' Forced to defend their practice against the claim that vernacular dailiness is the only true measure of language life. Their lexicon (halakhic, theological, poetic-liturgical) is treated as archaic rather than constitutive of the language.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_hebrew_traditionalists, payer,
    moderate, generational, constrained, regional).

% Encounter the doctrine as a judgment on their Hebrew literacy and cultural authenticity. If they maintain Hebrew through liturgy and study without native childhood acquisition or daily vernacular use, they are positioned as outsiders to 'real' vitality. Their Hebrew knowledge is reframed as learned, not native; preserved, not vital. This creates pressure to shift to local languages or to undergo linguistic resocialization if they migrate to Israel.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_jewish_communities, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, diaspora_jewish_communities, excluded).

% Operates as the official arbiter of Hebrew vitality. Chairs the commission for lexical expansion, standardizes Modern Hebrew grammar and vocabulary, and certifies what counts as 'authentic' usage. Benefits from institutional authority and funding tied to the doctrine; enforces it by controlling educational standards and public language norms.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, language_academy_of_israel, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, language_academy_of_israel, beneficiary).

% Arrive in Israel with deep liturgical Hebrew competence but little vernacular facility. Experience the constraint as a demand to abandon their primary Hebrew literacy and acquire a new one that the state recognizes as 'vital.' Cannot easily exit: their religious identity ties them to liturgical Hebrew, their relocation ties them to the state, and the doctrine creates institutional pressure (schools, employment, social belonging) to acquire native-like vernacular fluency they cannot achieve in adulthood.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, religiously_observant_immigrants, payer,
    moderate, biographical, identity_locked, national).

% Conduct scholarship on Hebrew's liturgical, biblical, and medieval literary corpus. Systematically excluded from Israeli language policy bodies and public discourse on vitality. Their expertise is treated as historical or comparative, not as evidence of living linguistic practice. They would argue that vitality is a question of use and function, not of when that use began; their exclusion maintains the doctrine's monopoly on defining vitality.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, linguists_of_liturgical_tradition, excluded,
    moderate, generational, constrained, global).

% The accumulated texts, practices, and lexical systems of Hebrew in liturgical and literary use across 2000+ years. Treated under the doctrine not as evidence of vitality but as a substrate — valuable for revival but not itself vital. The non-agent entry keeps the corpus's structural position visible: it is something the doctrine acts upon (through revival), not something that speaks for itself.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, historical_liturgical_hebrew_corpus, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_vitality__native_daily_reading, historical_liturgical_hebrew_corpus).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__native_daily_reading, zionist_state_builders).
narrative_ontology:fixing_cost_class(hebrew_vitality__native_daily_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, state-endorsed criterion for Hebrew linguistic authenticity and vitality: native vernacular dailiness, learned from birth in speech communities. Solves the problem of legitimating Hebrew as a modern national language after centuries of liturgical or learned use; creates a unified public Hebrew that transcends regional diaspora variants and positions Hebrew speakers as a coherent national group.
% TRANSFER_FUNCTION: Transfers cultural authority and institutional resources from liturgical-scholarly elites to state-sponsored vernacular institutions (schools, language academy, media). Moves social status from 'preservers of tradition' to 'native speakers' and from 'learned knowledge' to 'natural fluency.' Extracts legitimacy from the liturgical tradition (which remains as substrate) and concentrates it in the secular state-building project.
% ABSENT_VOICES: Diaspora Hebrew scholars, liturgical traditions (especially Sephardic and Mizrahi variants embedded in prayer), medieval Hebrew literature communities, and younger generations raised in diaspora Hebrew contexts where literacy is strong but childhood vernacularity is low. These parties would argue that vitality is use-in-context, not nativeness-in-genesis, and that the doctrine damages intergenerational transmission by invalidating non-native competence. They are excluded from policy bodies and public consensus-making on what counts as Hebrew vitality.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, Hebrew would still be spoken in Israel, but the boundary between 'vital' and 'preserved' would dissolve. Liturgical and literary traditions would re-enter public legitimacy as living uses of the language rather than archaeological artifacts. Diaspora Hebrew communities would no longer face the judgment that their literacy is inauthentic. The state's monopoly on defining Hebrew vitality would break; multiple forms of Hebrew practice would compete for recognition without one form claiming exclusive authenticity. Educational policy would shift to honor multiple pathways of Hebrew competence.
% FOUNDING_PROBLEM: In the late 19th and early 20th centuries, Hebrew had no living native speaker base: it was a language of scripture, prayer, and Jewish scholarship, used fluently by a tiny literate elite but not as a primary speech community language. The Zionist movement required a vernacular substrate to support national state-building and modern life (government, commerce, daily social coordination). The founding problem: how to recover a national language when the only living continuity is through religious and literary preservation?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is uncontested: historians, linguists, and even opponents of the doctrine acknowledge that Hebrew had no native speaker community in 1880. The contested part is whether the solution required declaring liturgical use non-vital. Zionist state-builders and Israeli educators attest that native vernacularity was necessary and that the doctrine successfully created it. Linguists of liturgical tradition and diaspora Hebrew scholars attest that the founding problem could have been solved (and was partially solved) by cultivating multilingual vitality: native vernacular for state functions, liturgical fluency for religious and cultural identity. Comparative cases (Irish, Basque, Catalan) show that language revitalization does not require the de-legitimation of literary or religious forms. Religious scholars argue the doctrine was not inevitable but ideologically driven—chosen from multiple possible solutions.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The doctrine's extractiveness is moderate-high (0.62 at interval end) because it concentrates authority over Hebrew authenticity in the state while delegitimating competing forms of use. Suppression (0.71) is higher because the constraint must actively enforce exclusion: it requires state institutions (schools, media, culture) to privilege native speakers and marginalize non-native competence; without this enforcement, liturgical and diaspora variants would retain their legitimacy through continuous use and cultural attachment. Theater rises from 0.08 to 0.48 because the doctrine's initial function (solving the genuine coordination problem of building a national language) is real, but as native vernacular becomes established and accessible, an increasing share of enforcement activity defends the exclusive criterion against competing claims (liturgical use, diaspora competence). Accessibility collapse (0.58) is moderate: liturgical Hebrew did not become inaccessible, but access to recognized vitality became sharply constrained—you had to be a native speaker, learned from birth in a Hebrew-speaking household. Resistance (0.64) is substantial because liturgical traditions, diaspora communities, and some linguists resisted the doctrine from the outset, though institutional power favored the enforcement side.
 *
 * PERSPECTIVAL GAP:
 *   From the zionist state-builder and secular Hebrew speaker position, the doctrine is genuine coordination: it solved the problem of creating a living national language by focusing resources on native acquisition and daily use. From the liturgical traditionalist and diaspora position, it is extraction: delegitimating their existing Hebrew competence in order to establish a monopoly on authenticity. The engine should compute these as different types from the same structural data. The state-builder seat sees coordination (beneficiary position, low d); the traditionalist seat sees enforced marginalization (payer position, high d). This divergence is the core measurement the constraint-type exercise exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Zionist state-builders and the Language Academy hold institutional power, set the rules, and benefit from the monopoly on defining vitality—their d is near 0.0 (full beneficiary). Secular Israeli native speakers hold organized power, benefit from insider status, and have mobile exit (can leave Israel, but within Israel the constraint operates to their advantage)—their d is near 0.2-0.3 (moderate beneficiary). Liturgical traditionalists hold moderate power, bear the cost of marginalization, and have constrained exit (religious identity ties them to liturgical practice; diaspora geography ties them to communities outside Israel)—their d is near 0.7-0.8 (near-full target). Diaspora Jewish communities are powerless, bear the cost of authenticity judgment, and are identity-locked (Jewish identity, Hebrew literacy, but childhood non-native acquisition)—their d approaches 1.0 (full target). Religiously observant immigrants hold moderate power but are identity-locked between religion and relocation—their d is near 0.75-0.85.
 *
 * MANDATROPHY ANALYSIS:
 *   The doctrine does NOT suffer mandatrophy in the classical sense (founding problem now dead but constraint persists). The founding problem (lack of native vernacular substrate) is resolved by 1960: Israel has a growing population of native Hebrew speakers. However, the constraint's persistence beyond the resolution of the founding problem reveals the extraction layer. If the doctrine were only about solving the coordination problem, it would relax once native speakers exist—but it continues to enforce non-native exclusion and marginalization. This suggests the doctrine's function has shifted from coordination to extraction of authority. The six-questions interview captures this: the founding_problem_status is contested, and the disappearance_verdict is world_rearranges—both indicators that the constraint's current form exceeds what coordination requires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nativeness_vs_use_function,
    'Is vitality fundamentally about nativeness-in-acquisition (learning from birth as L1) or about use-in-context (serving as primary communication medium, regardless of acquisition age)?',
    'Comparative sociolinguistics: examine living language communities where adult-acquired languages function as primary social media (e.g., lingua francas in multilingual contexts, Irish or Catalan post-revitalization, immigrant-shifted communities). If non-native-acquisition languages show equal functional vitality and social prestige, the nativeness criterion is contingent, not definitional.',
    'If use-function rather than nativeness is the vitality criterion, the constraint''s classification flips: liturgical Hebrew (continuous use for 2000 years) re-enters the vitality category, and the doctrine becomes pure extraction (delegitimizing competing forms to monopolize authority). If nativeness is definitional, the doctrine is genuine coordination with asymmetric distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nativeness_vs_use_function, conceptual, 'Whether vitality is defined by acquisition pathway (nativeness) or by functional role (use-in-context).').

omega_variable(
    liturgical_tradition_as_substrate_vs_victim,
    'Is the liturgical tradition''s desacralization a necessary cost of revival (it had to be devalued to free up social prestige for vernacular use), or is it an unnecessary extractive layer (revitalization could have honored both liturgical and vernacular as complementary forms)?',
    'Historical counterfactual analysis and comparative cases: examine other language revivals (Irish, Welsh, Basque, Catalan) to determine whether successful revitalization requires or benefits from devaluing literary/religious forms. Conduct interviews with second-generation Israeli Hebrew speakers to measure whether the doctrine''s enforcement was necessary for their linguistic acquisition or contingent to political ideology.',
    'If desacralization was necessary, the doctrine''s extraction is a justified cost of coordination. If it was contingent, the constraint is unnecessarily extractive—beneficiaries captured additional authority beyond what coordination required. This would support mandatrophy analysis: the founding coordination problem is solved, but the extractive layer persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_tradition_as_substrate_vs_victim, empirical, 'Whether liturgical devaluation was a necessary cost or an optional extraction in the revitalization process.').

omega_variable(
    internalized_vs_structural_suppression,
    'How much of the measured suppression is structural (policy, institutional exclusion, economic incentives) versus internalized (the non-native speaker''s own belief that their Hebrew is inauthentic)?',
    'Post-exit trajectory: measure diaspora Hebrew speakers'' confidence and use patterns before and after encountering Israeli pressure. If suppression persists after they leave Israel or after Israeli institutions relax the doctrine, it is internalized. If it decays when institutional pressure relaxes, it is primarily structural. Survey non-native speakers about their self-assessment of Hebrew authenticity and track how it changes with institutional messaging.',
    'If primarily internalized, the constraint''s effective suppression is higher than the structural measure suggests—targets carry the suppression with them. If primarily structural, relaxing policy would quickly reduce suppression. This informs the fixing_cost assessment: structural suppression has lower fixing_cost; internalized suppression has higher cost and longer timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Proportion of suppression that is structural (institutional barriers) vs. internalized (self-judgment).').

omega_variable(
    identity_frame_break_effect,
    'If the doctrine''s core frame (nativeness = vitality) were rejected or reversed at the institutional level, how quickly would religiously observant immigrants and diaspora communities rebuild their Hebrew literacy confidence and social integration?',
    'Policy natural experiment: track linguistic and social outcomes in communities that shift from native-privileging to multilingual-honoring policies. Conduct longitudinal studies of diaspora immigrants'' Hebrew use and self-assessment before and after policy shifts.',
    'If recovery is rapid (within 1-2 generations), the identity lock is primarily institutional/reversible. If slow or incomplete, identity frames have deep internalization and the lock is harder to break. This affects mandatrophy resolution: a constraint whose core identity frame has been internalized by its victims will persist longer than structural suppression alone predicts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_frame_break_effect, empirical, 'Reversibility and timeline for identity-frame-recovery if the doctrine''s core claim is institutionally rejected.').

omega_variable(
    reading_frame_container_ambiguity,
    'Which reading of the hebrew_vitality kernel is the ''natural'' or ''default'' frame—is it the one most parties arrive at independently, or is it the one the most powerful institutional actor enforces?',
    'Historical analysis of early Zionist discourse (pre-state, 1880-1920) before institutional enforcement: what did diverse thinkers (state builders, diaspora rabbis, secular intellectuals, linguists) say vitality meant? Parallel question for contemporary diaspora and Israeli communities: what reading do non-enforced populations gravitate toward?',
    'If the native-daily reading is independently embraced by most communities, it is a genuine convergence and the enforcement is marginal. If it is maintained primarily by institutional power against resistant populations, the constraint''s extractiveness is higher and its coordination function is weaker than authorship suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_frame_container_ambiguity, conceptual, 'Whether the native-daily reading is naturally convergent or institutionally imposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1880, 1960).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_vitality__native_daily_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_vitality__native_daily_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_vitality__native_daily_reading, theater_ratio, 1920, 0.28).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_vitality__native_daily_reading, theater_ratio, 1940, 0.42).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_vitality__native_daily_reading, theater_ratio, 1950, 0.46).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_vitality__native_daily_reading, theater_ratio, 1960, 0.48).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_vitality__native_daily_reading, base_extractiveness, 1880, 0.12).
narrative_ontology:measurement(hebr_be_t1900, hebrew_vitality__native_daily_reading, base_extractiveness, 1900, 0.28).
narrative_ontology:measurement(hebr_be_t1920, hebrew_vitality__native_daily_reading, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement(hebr_be_t1940, hebrew_vitality__native_daily_reading, base_extractiveness, 1940, 0.58).
narrative_ontology:measurement(hebr_be_t1950, hebrew_vitality__native_daily_reading, base_extractiveness, 1950, 0.61).
narrative_ontology:measurement(hebr_be_t1960, hebrew_vitality__native_daily_reading, base_extractiveness, 1960, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_vitality__native_daily_reading, suppression_requirement, 1880, 0.18).
narrative_ontology:measurement(hebr_su_t1900, hebrew_vitality__native_daily_reading, suppression_requirement, 1900, 0.32).
narrative_ontology:measurement(hebr_su_t1920, hebrew_vitality__native_daily_reading, suppression_requirement, 1920, 0.48).
narrative_ontology:measurement(hebr_su_t1940, hebrew_vitality__native_daily_reading, suppression_requirement, 1940, 0.64).
narrative_ontology:measurement(hebr_su_t1950, hebrew_vitality__native_daily_reading, suppression_requirement, 1950, 0.69).
narrative_ontology:measurement(hebr_su_t1960, hebrew_vitality__native_daily_reading, suppression_requirement, 1960, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__native_daily_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The hebrew_vitality kernel is contested among three structurally distinct constraints: (1) native_daily_reading (this story) — nativeness-as-vitality, moderate ε, Zionist state extraction; (2) liturgical_reading — liturgical use-as-vitality, low ε, community-sustaining coordination; (3) hybrid_continuity_reading — multilingual vitality, low-moderate ε, integrative coordination. Each story carries its own ε, beneficiary/victim structure, and enforcement profile. They are linked by the shared kernel (the contested definition of Hebrew vitality) but have different classification outcomes. The native-daily reading forecloses neither the liturgical reading (different communities hold both) nor the hybrid reading (they coexist in public debate), but it does exert structural pressure on both by monopolizing state authority and institutional resources.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_vitality__native_daily_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
