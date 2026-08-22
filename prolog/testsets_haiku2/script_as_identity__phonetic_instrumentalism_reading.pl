% ============================================================================
% CONSTRAINT STORY: script_as_identity__phonetic_instrumentalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__phonetic_instrumentalism_reading, []).

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
 *   constraint_id: script_as_identity__phonetic_instrumentalism_reading
 *   human_readable: Script Neutrality as Phonetic Optimization (Instrumentalist Reading)
 *   domain: linguistic_technology/state_policy
 *
 * SUMMARY:
 *   This constraint instantiates the PHONETIC INSTRUMENTALISM READING of the
 *   script-identity kernel: the claim that script choice for Turkish should
 *   be evaluated as a technical optimization problem (which orthographic
 *   system best represents vowel harmony) rather than as a civilization-level
 *   identity choice. This reading frames Latin script adoption as a neutral
 *   engineering solution and Arabic script retention as resistance to
 *   technical progress. It is one of three structurally distinct readings of
 *   the same contested kernel: the Kemalist rupture reading (Latin script
 *   enables secular break from Ottoman-Islamic past) and the Ottoman
 *   continuity reading (Arabic script is constitutive of Turkish-Islamic
 *   identity) both make identity the explicit criterion; the
 *   phonetic-instrumentalism reading depoliticizes by relocating the decision
 *   to phonetic grounds. The three readings share a referent (the historical
 *   choice of script in early Turkish state-building) but produce different
 *   constraint structures with different beneficiaries, costs, and resistance
 *   profiles. This JSON instantiates ONLY the phonetic-instrumentalism
 *   reading: it carries low measured extractiveness (0.28, because the
 *   framing is genuinely about technical optimization) and high theater ratio
 *   (0.62, because the technical frame obscures and performs away the
 *   identity-encoding function).
 *
 * KEY AGENTS:
 *   - Language standardization advocates (beneficiary, organized, mobile exit)
 *   - Education modernizers (beneficiary/agenda-setter, institutional, constrained exit)
 *   - Ottoman traditionalists (payer, moderate power, identity-locked exit)
 *   - Comparative phonetics discipline (vindicated proposition, non-agent)
 *   - Literacy learners (beneficiary/payer dual, powerless, trapped exit)
 *   - Ottoman continuity custodians (excluded, moderate power, trapped exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__phonetic_instrumentalism_reading, 0.28).
domain_priors:suppression_score(script_as_identity__phonetic_instrumentalism_reading, 0.41).
domain_priors:theater_ratio(script_as_identity__phonetic_instrumentalism_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(script_as_identity__phonetic_instrumentalism_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__phonetic_instrumentalism_reading, rope).
narrative_ontology:human_readable(script_as_identity__phonetic_instrumentalism_reading, "Script Neutrality as Phonetic Optimization (Instrumentalist Reading)").
narrative_ontology:topic_domain(script_as_identity__phonetic_instrumentalism_reading, "linguistic_technology/state_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__phonetic_instrumentalism_reading, 'fb3155bd-c104-4759-92a4-d792067b5a3a').
narrative_ontology:cs_kernel_codification('fb3155bd-c104-4759-92a4-d792067b5a3a', fixed_text).
narrative_ontology:cs_authority_grounding('fb3155bd-c104-4759-92a4-d792067b5a3a', extraction).
narrative_ontology:cs_interpretation_layer_present('fb3155bd-c104-4759-92a4-d792067b5a3a').
narrative_ontology:cs_reading_relation('fb3155bd-c104-4759-92a4-d792067b5a3a', script_as_identity__kemalist_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb3155bd-c104-4759-92a4-d792067b5a3a', script_as_identity__ottoman_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('fb3155bd-c104-4759-92a4-d792067b5a3a', foundational, script_is_neutral_technical_choice).
narrative_ontology:cs_axiom_status(script_is_neutral_technical_choice, holdable).
narrative_ontology:cs_axiom_grounding('fb3155bd-c104-4759-92a4-d792067b5a3a', script_is_neutral_technical_choice, empirically_contingent).
narrative_ontology:cs_axiom('fb3155bd-c104-4759-92a4-d792067b5a3a', foundational, phonetic_transparency_optimizes_literacy).
narrative_ontology:cs_axiom_status(phonetic_transparency_optimizes_literacy, holdable).
narrative_ontology:cs_axiom_grounding('fb3155bd-c104-4759-92a4-d792067b5a3a', phonetic_transparency_optimizes_literacy, empirically_contingent).
narrative_ontology:cs_reference_frame('fb3155bd-c104-4759-92a4-d792067b5a3a', technical_script_optimization).
narrative_ontology:cs_drift_state('fb3155bd-c104-4759-92a4-d792067b5a3a', contemporary_cultural_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fb3155bd-c104-4759-92a4-d792067b5a3a', '2026-06-12T14:23:45Z').
narrative_ontology:cs_kernel_id(script_as_identity__phonetic_instrumentalism_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, language_standardization_advocates).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, education_modernizers).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, comparative_phonetics_discipline).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(script_as_identity__phonetic_instrumentalism_reading, literacy_learners).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, ottoman_traditionalists).
narrative_ontology:constraint_victim(script_as_identity__phonetic_instrumentalism_reading, literacy_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Linguists, educators, and policy advisors who endorse script reform on phonetic grounds. They argue Latin orthography provides cleaner representation of Turkish vowel harmony, reducing learning burden and standardizing written output. They benefit from the frame that script choice is technical, not political: it protects them from the charge of severing Ottoman continuity.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, language_standardization_advocates, beneficiary,
    organized, generational, mobile, national).

% State-linked educators tasked with mass literacy and standardization. The phonetic-optimization frame lets them implement script reform while claiming cultural neutrality — the decision becomes 'which system teaches faster' rather than 'which empire's symbols we adopt.' They benefit from the appearance of depoliticized choice.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, education_modernizers, beneficiary,
    institutional, generational, constrained, national).

% Religious scholars, conservative communities, and Ottoman continuity advocates. They bear the cost of script discontinuity: loss of access to centuries of Ottoman-Turkish literature without labor-intensive re-learning, severing of lived practice with inherited texts, and the symbolic message that Turkish identity is being redefined. The phonetic-optimization frame silences their objection by reframing it as 'resistance to technical progress.'
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, ottoman_traditionalists, payer,
    moderate, biographical, identity_locked, national).

% The field of comparative phonetics is vindicated by this reading: it supplies the technical language (vowel harmony, phonetic transparency, graphemic efficiency) that transforms a political choice into an engineering problem. The discipline gains authority and stays neutral by design.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, comparative_phonetics_discipline, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(script_as_identity__phonetic_instrumentalism_reading, comparative_phonetics_discipline).

% Islamic legal scholars, Ottoman historians, and continuity-advocates outside the Turkish state apparatus. They would argue that script is constitutive of identity, not interchangeable technology; that the phonetic frame obscures a civilization-level rupture. They are kept outside the decision apparatus by the frame itself — their objection is preemptively dismissed as 'refusing technical modernization.'
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, ottoman_identity_custodians, excluded,
    moderate, civilizational, trapped, global).

% Students and new literates learning written Turkish. They benefit from a cleaner phonetic mapping (faster learning, fewer exceptions). They also bear an invisible cost: they inherit a script discontinuity they did not choose and have no path back to Ottoman-era texts without specialist training. Their choice set is bounded by state curriculum.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, literacy_learners, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__phonetic_instrumentalism_reading, literacy_learners, payer).

% The corpus of Ottoman-Turkish literature, legal texts, religious scholarship, and administrative records. It is not an agent but carries an outcome: accessibility to these texts becomes conditional on specialist training or translation, rather than continuous with living literacy practice. The phonetic frame treats this loss as irrelevant to the script decision.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, ottoman_textual_heritage, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(script_as_identity__phonetic_instrumentalism_reading, ottoman_textual_heritage).

% International linguists and comparative script analysts who can measure phonetic transparency independently of political context. They are positioned to either ratify or challenge the phonetic-optimization frame by examining whether the claimed technical advantage is genuine, and whether other scripts (e.g., modified Arabic) could achieve equal phonetic clarity.
narrative_ontology:constraint_stakeholder(script_as_identity__phonetic_instrumentalism_reading, phonetic_linguistics_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__phonetic_instrumentalism_reading, education_modernizers).
narrative_ontology:fixing_cost_class(script_as_identity__phonetic_instrumentalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of standardizing written representation of Turkish vowel harmony within a mass-literacy system: establishes a consistent graphemic-to-phonemic mapping so educated speakers and new literates align on what the written form means. A single, transparent orthographic standard reduces ambiguity in written communication across regions and generations.
% TRANSFER_FUNCTION: Moves cultural authority from Ottoman-era continuity custodians to technical modernizers: the power to define 'legitimate Turkish' shifts from religious scholars and tradition-bearers to linguistic specialists and state-linked educators. Transfers access to historical texts from ordinary reading competence to specialized scholarly labor.
% ABSENT_VOICES: Ottoman continuity advocates and Islamic scholars who read the script choice as identity rupture are structurally excluded: their objection is preemptively reframed as 'resisting technical progress,' which disqualifies them from the decision apparatus. Voices from diaspora Ottoman communities and Islamic jurisprudential traditions are not seated in the standardization discourse.
% DISAPPEARANCE_RATIONALE: If the phonetic-optimization frame vanished and script choice were openly adjudicated as an identity question, the decision logic would shift: Ottoman traditionalists would re-enter the debate with full standing, the state would need to make an explicit cultural-rupture choice rather than hiding behind phonetic neutrality, and the decision would be contested rather than settled. The disappearance of the frame (not the script itself — script is independent) would reorganize whose voice counts and how the choice is justified.
% FOUNDING_PROBLEM: Early Republican Turkish faced a literacy crisis: Ottoman scribal Arabic script was opaque to mass education, vowel diacritics were inconsistent, and the state needed a single standardized orthography to unify written Turkish across the new nation. Technical phonetic standardization was genuinely needed for a state-building literacy apparatus.
% FOUNDING_PROBLEM_CORROBORATION: Education historians and literacy specialists outside the Turkish state attest the founding problem was real: early 20th-century Ottoman script literacy WAS constrained by complexity. However, international linguistic analysis from non-Turkish researchers (comparative script studies, phonetic analyses) also shows that modified Arabic orthography WITH systematic vowel marking could have achieved similar phonetic clarity — the alternative was possible. The problem was real; the solution's uniqueness is contested.
narrative_ontology:disappearance_verdict(script_as_identity__phonetic_instrumentalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__phonetic_instrumentalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__phonetic_instrumentalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__phonetic_instrumentalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__phonetic_instrumentalism_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__phonetic_instrumentalism_reading_tests).
:- end_tests(script_as_identity__phonetic_instrumentalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint's extractiveness is LOW (0.28) because the reading's own internal logic is genuinely technical: Latin script DOES provide a cleaner graphemic-to-phonemic correspondence for Turkish vowel harmony, and that technical advantage is real. The beneficiaries (educators, modernizers, the phonetics discipline) are getting a real coordination benefit — standardized, learnable orthography. However, extractiveness is not zero because the frame SUPPRESSES a legitimate competing claim: that modified Arabic script could achieve equal phonetic clarity while preserving continuity. The theater ratio is HIGH (0.62 and rising over the interval, asymptoting at 0.64) because as generations pass and Ottoman literacy disappears, the phonetic frame does less and less actual justificatory work — the suppression of identity-discourse becomes performative maintenance rather than active argumentation. Early in the interval (t=0), the technical phonetic argument had to be articulated and defended; by t=100, it is simply assumed, and the theater is pure maintenance of the depoliticized frame. Suppression requirement rises early (0.25 to 0.41 by t=50) as the traditionalist voices that might contest the frame are socialized out of the system, then plateaus — the suppression becomes structural (generations of new literates who have never known Arabic script) rather than active (state apparatus silencing objectors).
 *
 * PERSPECTIVAL GAP:
 *   From the standardization advocates' seat: this is a genuine rope — a coordination solution to the mass-literacy problem, defended on its technical merits. The phonetic transparency is real and beneficial. From the Ottoman traditionalist seat: this is a tangled rope or snare — a suppressed identity choice dressed as technical optimization, extracting continuity as the price of admission to the modern state. The engine should compute BOTH: the standardization advocate sees low-to-moderate d (beneficiary position), while the traditionalist sees high d (target position, identity-locked). The divergence IS the mandatrophy signal — the same constraint-structure appears as beneficial coordination to one seat and as enforced rupture to another. This reading's authored low extractiveness and the computed per-seat divergence together should flag the constraint as one where framing choice (technical vs. identity) determines classification outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (educators, modernizers, phonetics) derive d from the frame's own logic: they genuinely benefit from standardized, phonetically transparent orthography with minimal suppression (d ~0.15-0.25 for these seats — beneficiary end). The payers (Ottoman traditionalists) derive d from the suppression of their identity-claim: they cannot even articulate their objection without being dismissed as anti-modern (d ~0.75-0.85 — target end, identity-locked because exit means either accepting the new script or remaining illiterate in the new state's literacy system). Literacy learners are dual: they benefit from phonetic clarity (lower d component) and bear the historical rupture cost and compressed choice set (higher d component), netting around 0.5 (symmetric). The excluded custodians have d near target (0.8) because they are structurally prevented from contesting the frame.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint CLAIMS rope (genuine coordination to solve a mass-literacy problem) but SHOWS asymmetric extraction when computed per-seat: beneficiaries in the modernizer coalition get coordination gains without bearing the identity cost; targets bear the cost of historical rupture without consenting to the reframing. The standard mandatrophy test would ask: 'Is this a rope because it genuinely solves a shared problem, or a tangled rope because the 'problem' is defined in a way that benefits one coalition and suppresses objections from another?' The resolution lies in examining whether MODIFIED ARABIC SCRIPT with systematic vowel-marking could have solved the technical problem equally well. If yes, the choice between alternatives was not technically determined — it was a political choice dressed as technical. That reframing from 'which script is phonetically best' to 'could multiple scripts solve the problem equally well' is where mandatrophy surfaces. The founding_problem_corroboration field captures this: literacy scholars (outside the benefiting parties) attested that the founding problem was real BUT that alternative technical solutions existed. That corroboration breaks the purely technical frame and reinstates the identity question. The constraint survives classification as rope because the technical benefit is real; but the mandatrophy question remains open: was this a rope that happened to benefit modernizers, or was it a snare pretending to be a rope?
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phonetic_alternative_viability,
    'Could modified Arabic script with systematic vowel-diacritics achieve equal phonetic transparency for Turkish vowel harmony as Latin orthography?',
    'Comparative phonetic analysis: design parallel texts in both systems (modified Arabic with full vowel marking, Latin) and measure graphemic-to-phonemic ambiguity, learning-curve speed, and standardization burden. If equal clarity is achievable in both, the technical criterion does not uniquely determine script choice.',
    'If modified Arabic is equally viable phonetically, the phonetic-optimization frame is exposed as a rationalization for a politically-motivated choice — ε would increase substantially (from 0.28 toward 0.55-0.65, snare territory). If modified Arabic is genuinely inferior phonetically, the frame''s claim is vindicated and ε remains low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonetic_alternative_viability, empirical, 'Whether the claimed phonetic advantage of Latin is unique or whether alternative technical solutions exist.').

omega_variable(
    frame_depoliticization_mechanism,
    'Is the phonetic frame genuinely neutral (locating the decision criterion in linguistic facts that pre-date the choice), or is it a constructed depoliticization (organizing the decision around technical metrics CHOSEN because they point to the preferred outcome)?',
    'Historical-process analysis: examine the order of reasoning in state documents and linguistic scholarship. Did the Turkish modernizers (1) measure phonetic properties, then conclude Latin was best? Or (2) decide to adopt Latin for modernization reasons, then commission phonetic analysis to justify it? Contemporaneous records can reveal frame construction vs. frame discovery.',
    'If the frame was discovered (phonetic analysis was done first, conclusions followed), it remains a legitimate rope. If constructed (conclusion was prior, analysis was post-hoc), the frame is a cover story and ε increases substantially. This is a conceptual/empirical hybrid — the historical sequence is empirical; the inferential weight (''discovery'' vs ''construction'') is interpretive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(frame_depoliticization_mechanism, empirical, 'Whether the phonetic frame represents genuine technical analysis or post-hoc rationalization.').

omega_variable(
    identity_cost_quantification,
    'What is the magnitude of the identity rupture cost borne by Ottoman traditionalists and continuity advocates? Is it a negligible side effect or a substantial extraction comparable to the standardization benefit?',
    'Comparative cultural history: measure continuity loss (access to historical texts, inheritance of literacy practice, transmission of tradition) against modernization gain (education speed, standardization efficiency, reduced learning burden). No precise quantification is possible, but the relative scale can be estimated through testimony from affected communities and assessment of path-dependency losses.',
    'If the identity cost is negligible, the constraint remains a rope where modernization gains dominate. If substantial, the constraint is a tangled rope — genuine coordination benefit coupled with asymmetric extraction of continuity from traditionalist seats. If catastrophic for traditionalists, it approaches snare territory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_cost_quantification, preference, 'How to weigh cultural continuity loss against literacy-modernization gain — value-dependent, not empirically decidable.').

omega_variable(
    suppression_structural_vs_active,
    'As the interval progresses and Ottoman literacy disappears naturally (generations turn over), does the suppression mechanism transition from active (state institutions silencing objectors) to structural (new literates have no inherited capacity to contest)?',
    'Ethnographic and historical comparison: t=0-25 should show active suppression (Ottoman traditionalists arguing for Arabic, state apparatus dismissing them); t=50-100 should show structural suppression (new generations have no embodied connection to Arabic script, objections become unintelligible rather than suppressed). The rising theater ratio (0.35 to 0.62) should correlate with this transition: early theater is performative argument; late theater is maintenance of invisibility.',
    'If suppression transitions from active to structural, the constraint''s persistence becomes less dependent on enforcement and more dependent on the natural process of socialization and literacy replacement. This would argue for reclassifying from tangled_rope (requires active enforcement) toward rope (coordination maintained by structural path-dependence) or piton (maintenance becomes mostly theatrical). The mechanism of persistence changes even if the type label does not.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_active, empirical, 'Whether suppression is maintained through active institutional action or through structural path-dependency as generations turn over.').

omega_variable(
    reading_identity_obscuration_design,
    'Is the phonetic-instrumentalism reading''s depoliticization a genuine analytical insight (script choice genuinely IS resolvable on technical grounds), or is it a designed obscuration (the technical frame was constructed specifically to hide the identity dimensions)?',
    'Genealogy of the frame: trace the intellectual and institutional development of phonetic-optimization discourse. Did comparative phonetics as a discipline develop independently and then happen to be applied to the Turkish script question? Or did Turkish state-builders commission linguistic research specifically designed to rationalize their political choice? The institutional history of the frame reveals its character.',
    'If genuine insight, the phonetic reading is a legitimate analytical option that reveals real technical tradeoffs. If designed obscuration, the reading itself is an element of the extraction mechanism — it enables the state to extract cultural authority from traditionalists while claiming neutrality. This affects how the constraint should be classified: is it a successful rope (technical coordination), or is it a snare whose success depends partly on an ideology that obscures its extractive dimensions?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_obscuration_design, conceptual, 'Whether the phonetic frame is a genuine discovery or a designed cover story for political choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__phonetic_instrumentalism_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scri_tr_t0, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(scri_tr_t10, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(scri_tr_t25, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(scri_tr_t50, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement(scri_tr_t75, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 75, 0.64).
narrative_ontology:measurement(scri_tr_t100, script_as_identity__phonetic_instrumentalism_reading, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(scri_be_t0, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(scri_be_t10, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(scri_be_t25, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 25, 0.26).
narrative_ontology:measurement(scri_be_t50, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 50, 0.28).
narrative_ontology:measurement(scri_be_t75, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 75, 0.29).
narrative_ontology:measurement(scri_be_t100, script_as_identity__phonetic_instrumentalism_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(scri_su_t0, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(scri_su_t10, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement(scri_su_t25, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 25, 0.4).
narrative_ontology:measurement(scri_su_t50, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement(scri_su_t75, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 75, 0.41).
narrative_ontology:measurement(scri_su_t100, script_as_identity__phonetic_instrumentalism_reading, suppression_requirement, 100, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__phonetic_instrumentalism_reading, information_standard).
narrative_ontology:boltzmann_floor_override(script_as_identity__phonetic_instrumentalism_reading, 0.05).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__kemalist_rupture_reading).
narrative_ontology:affects_constraint(script_as_identity__phonetic_instrumentalism_reading, script_as_identity__ottoman_continuity_reading).

% DUAL FORMULATION NOTE:
% The script-identity kernel decomposes into three structurally distinct constraints: the PHONETIC INSTRUMENTALISM reading (this file) frames script as technical optimization (low ε, obscures identity); the KEMALIST RUPTURE reading frames script as enabling secular modernization (higher ε, identity rupture is acknowledged benefit); the OTTOMAN CONTINUITY reading frames script as constitutive of cultural identity (very high ε from traditionalist standpoint, catastrophic loss). All three readings share the same historical referent (Turkish state script choice) but differ in criterion (phonetics vs. modernization vs. continuity), beneficiary structure, and ε value. No reading forecloses the others — they coexist across different communities. The three files should be linked bidirectionally via network.affects_constraints to enable the contamination and coupling analysis to recognize the family structure and handle the per-reading ε divergence correctly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
