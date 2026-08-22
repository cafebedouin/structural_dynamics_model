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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Native-Generation Vitality Standard (Hebrew Revival)
 *   domain: sociolinguistic/political
 *
 * SUMMARY:
 *   Between roughly 1904 and 2024 (interval units are years from 1904), the
 *   Zionist movement and later the State of Israel operated on the principle
 *   that Hebrew counts as alive only when natively acquired and used for
 *   daily generative life; ritual recitation, however continuous, is
 *   preservation in a glass case, not life. This reading of 'Hebrew vitality'
 *   drove the Hebrew-only school networks, the Language Wars against Yiddish
 *   and German publishing, the Va'ad HaLashon and later Academy's massive
 *   lexical expansion, the army ulpan network, the Hebraization of personal
 *   and place names, and the marginalization of the sacred register to the
 *   synagogue and study house. A genuine coordination good was produced: a
 *   single working vernacular for immigrants from dozens of mutually
 *   unintelligible language backgrounds. Real costs were imposed through the
 *   same machinery: the desacralization of a two-thousand-year liturgical
 *   register, and the one-generation amputation of diaspora mother tongues.
 *   FAMILY NOTE (epsilon-invariance decomposition): the colloquial label
 *   'Hebrew revival' decomposes into three readings of one kernel, linked via
 *   network.affects_constraints. Over the SAME referent arrangement,
 *   liturgical_reading authors near-zero epsilon (continuous ritual use harms
 *   no one; the vernacular project is the deviant from its seat),
 *   hybrid_continuity_reading authors an intermediate value, and this reading
 *   authors moderate epsilon (~0.47 end-state) for the enforced
 *   reconstruction arrangement it brought about. The claim/metric gap is
 *   deliberate: claimed_type is asserted from structural analysis
 *   (coordination plus asymmetric extraction under active enforcement); the
 *   metrics are authored descriptively of the arrangement's actual operation;
 *   the engine computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - zionist_institutional_leadership: agenda-setter and primary beneficiary (institutional/arbitrage) — defined the vitality standard, built the enforcement machinery, and received the unified linguistic sphere the national project required
 *   - hebrew_language_academy: secondary beneficiary (institutional/identity_locked) — collects canonical authority, budgets, and ceremonial precedence from the permanence of the expansion-and-regulation mandate
 *   - sabra_native_generation: beneficiary (organized/identity_locked) — received mother tongue and national identity as a single inheritance; their existence is the standard's proof of concept
 *   - liturgical_tradition_communities: primary target (moderate/identity_locked) — bore the desacralization of the holy tongue; their relationship to the language is constitutive, so exit was never conceivable
 *   - yiddish_speaking_immigrants: target (moderate/trapped) — bore mother-tongue abandonment under Hebrew-only schooling, workplace, and public-sphere pressure
 *   - diaspora_yiddishist_movement: excluded voice (organized/constrained) — held multilingual continuity as itself vitality; locked out of Yishuv decision-making
 *   - language_policy_sociolinguists: analytical observer — sees the full structure, the completed lifecycle, and the costs the victors' own accounting omits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.47).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.42).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Native-Generation Vitality Standard (Hebrew Revival)").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistic/political").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, 'a5abd84a-9edd-438a-ba98-808a86032b2d').
narrative_ontology:cs_kernel_codification('a5abd84a-9edd-438a-ba98-808a86032b2d', distributed).
narrative_ontology:cs_authority_grounding('a5abd84a-9edd-438a-ba98-808a86032b2d', practice).
narrative_ontology:cs_interpretation_layer_present('a5abd84a-9edd-438a-ba98-808a86032b2d').
narrative_ontology:cs_reading_relation('a5abd84a-9edd-438a-ba98-808a86032b2d', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('a5abd84a-9edd-438a-ba98-808a86032b2d', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('a5abd84a-9edd-438a-ba98-808a86032b2d', foundational, native_generation_constitutes_vitality).
narrative_ontology:cs_axiom_status(native_generation_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a5abd84a-9edd-438a-ba98-808a86032b2d', native_generation_constitutes_vitality, empirically_contingent).
narrative_ontology:cs_axiom('a5abd84a-9edd-438a-ba98-808a86032b2d', foundational, ritual_recitation_is_preservation_not_life).
narrative_ontology:cs_axiom_status(ritual_recitation_is_preservation_not_life, holdable).
narrative_ontology:cs_axiom_grounding('a5abd84a-9edd-438a-ba98-808a86032b2d', ritual_recitation_is_preservation_not_life, conventional).
narrative_ontology:cs_reference_frame('a5abd84a-9edd-438a-ba98-808a86032b2d', native_speaker_reproduction_norm).
narrative_ontology:cs_drift_state('a5abd84a-9edd-438a-ba98-808a86032b2d', contemporary_post_nativization, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('a5abd84a-9edd-438a-ba98-808a86032b2d', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_institutional_leadership).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, sabra_native_generation).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, liturgical_tradition_communities).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, yiddish_speaking_immigrants).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, native_acquisition_vitality_criterion).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, language_revival_possibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defined the standard that Hebrew lives only on the tongues of native daily speakers, and built the machinery to make it true: Hebrew-only school networks, the Va'ad HaLashon and later the Academy, army ulpanim, municipal Hebrew-only signage rules, and the campaign battles against Yiddish and German publishing. Invested decades of budget and political capital in lexical expansion so the language could carry medicine, law, and sport. Received the unified public sphere the national project required; the arrangement was their instrument, and abandoning it was never on their table.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Inherits the Va'ad HaLashon's mandate: coin terms, rule on grammar and pronunciation, guard the standard. Collects canonical authority, budgets, and ceremonial precedence from the permanence of the mission; its rulings are reported, debated, and often ignored, which renews demand for its arbitration. Its staff and self-conception are bound up with the institution's function; there is no version of the Academy that is not doing this work.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, hebrew_language_academy, beneficiary,
    institutional, generational, identity_locked, national).

% Born into Hebrew-only kindergartens and schooled, conscripted, and employed entirely in Hebrew. Received a mother tongue and a national identity as a single inheritance, and their existence is the standard's proof of concept. They bear almost none of the arrangement's costs and rarely perceive it as an arrangement at all — to them Hebrew is simply the water they swim in.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, sabra_native_generation, beneficiary,
    organized, generational, identity_locked, national).

% Pray, study, and argue in Hebrew as the holy tongue, a register their communities curated for two millennia with strict boundaries against mundane use. Watched the vernacular project turn that language into street speech, radio banter, and army slang, and saw the center of linguistic gravity move from the study house to the playground. Many refused for generations to use Hebrew for ordinary purposes; stepping out of the relationship was never conceivable, since the language's sanctity is part of who they are.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, liturgical_tradition_communities, payer,
    moderate, civilizational, identity_locked, global).

% Arrived speaking Yiddish (and later Ladino, Judeo-Arabic, Russian, German) and met schools, workplaces, and public life that ran only in Hebrew. Their presses were attacked in the Language Wars, their children answered them in a new tongue, and within a generation their mother languages retreated to kitchen and synagogue. They could not leave the country their families had finally reached, and their communal institutions weakened as the young drifted into the Hebrew sphere.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, yiddish_speaking_immigrants, payer,
    moderate, biographical, trapped, national).

% Held that Yiddish was the living national language of the Jewish street and that a Hebrew-only doctrine amputated diaspora culture to flatter a utopian experiment. Organized congresses, published newspapers, and lobbied for multilingual Jewish life, but were locked out of Yishuv decision-making and increasingly dismissed as relics of exile mentality.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, diaspora_yiddishist_movement, excluded,
    organized, generational, constrained, continental).

% Document and theorize the revival from outside it: the only widely accepted case of a language brought back from liturgical-only status to millions of native speakers. They track what the achievement cost, compare it to other revitalization efforts, and note how rarely the victors' own accounting mentions the price paid by rival codes and registers.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, language_policy_sociolinguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__native_daily_reading, zionist_institutional_leadership).
narrative_ontology:fixing_cost_class(hebrew_vitality__native_daily_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A population assembled from dozens of mutually unintelligible language backgrounds needed a single shared vernacular for schools, army, markets, courts, and science; the native-daily standard solved the common-language problem by making one ancestral language everyone's mother tongue again, and solved the boundary-maintenance problem of what counts as belonging to the new nation.
% TRANSFER_FUNCTION: Moves linguistic loyalty, cultural capital, and the sacred register itself — from diaspora mother tongues (Yiddish, Ladino, Judeo-Arabic, German, Russian) and from Hebrew's liturgical custody — into the state-building project's unified Hebrew public sphere.
% ABSENT_VOICES: Yiddishist cultural autonomists, who held that multilingual diaspora culture was itself Jewish vitality, and traditionalist rabbinic authorities, who held the holy tongue must not serve mundane traffic, were both marginalized in the Yishuv's Hebrew-only institutions; their objections survive mainly in archives, memoirs, and enclave practice. The unanimity of the Hebrew public sphere partly reflects this manufactured absence rather than settled consensus.
% DISAPPEARANCE_RATIONALE: If the native-daily standard and its enforcement machinery vanished overnight, the Israeli public sphere would fragment back toward its immigrant tongues within a generation: schooling, courts, and the army would lose their common medium, the sabra identity category would dissolve, and Hebrew would revert to a prestigious liturgical-and-literary code alongside whichever vernacular filled the vacuum. Liturgical Hebrew would continue, but as heritage practice rather than national infrastructure.
% FOUNDING_PROBLEM: Hebrew had been a liturgical and literary language without native speakers for some seventeen centuries; the Zionist project faced the problem of turning it into a spoken vernacular capable of carrying a modern state and a unified nation gathered from dozens of language communities.
% FOUNDING_PROBLEM_CORROBORATION: Sociolinguistic scholarship outside the benefiting parties treats Hebrew as the completed — indeed unique — case of successful language revival, and demographic data show native-speaker majorities since the state's early decades; no serious external source still characterizes Hebrew as requiring revival-scale intervention. The Academy's own shifted mandate (from creation to regulation) is acknowledged inside the institution. The arrangement's persistence after the problem's resolution is attested by the same external literature, which documents residual enforcement (ulpan requirements, Hebrew-only legal defaults, pressure on Arabic speakers) as politics rather than necessity.
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

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
 *   End-state extractiveness 0.47 is moderate: the arrangement's extractive overlay decayed sharply once nativization became self-sustaining (children arrived speaking Hebrew; enforcement became redundant), but residual pressure on Arabic speakers, ulpan mandates, and purist campaigns keep it above a pure-coordination floor. Suppression 0.42 reflects soft institutional defaults (Hebrew-only schooling and bureaucracy as unmarked choice) replacing the hard coercion of the Language Wars era; suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater 0.33: the function is real (Academy coinages are adopted, ulpanim teach, the vernacular self-perpetuates), but purity campaigns grow increasingly performative against the unstoppable tide of English borrowing, and the ratio rises slowly across the interval. Accessibility_collapse 0.52: alternatives persist in enclaves (Haredi Yiddish, liturgical primacy, Arab-sector multilingualism) but Hebrew-only remains the default that structures opportunity, so alternatives are costly rather than closed. Resistance 0.38: the historic Yiddishist and rabbinic resistance was substantial in the early interval but was exhausted, marginalized, or accommodated; the failed Yiddishist coalition is itself evidence — the excluded seat never got the numbers or the room to mount effective joint resistance. TEMPORAL SHAPE: hump-shaped, not cyclical — enforcement built to a 1920s peak (Language Wars, T0+20), plateaued through statehood, then decayed as nativization completed, with a slight late uptick flagged in omega late_interval_intensification. All three series run on ONE shared time grid (every 20 years, T0-T120) so no metric row borrows another's end-state values.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences triumph: the linguistically impossible achieved, a liturgical language reborn on children's tongues, and every enforcement battle remembered as necessary. The payer seats experience loss: a holy register profaned by bus-stop chatter, mother tongues amputated in a single generation, grandparents and grandchildren unable to joke in the same language. The same school bell sounds like birth from one seat and dispossession from another. The engine computes this per-seat divergence from the structural data (power, exit, role); nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: zionist_institutional_leadership sits nearest the full-beneficiary end (the arrangement was their instrument and they collect its product); hebrew_language_academy derives low d with identity_lock amplifying its attachment to the beneficiary position; sabra_native_generation derives low d despite identity lock, because the lock binds them to a subsidized position. Victim declarations drive high directionality: liturgical_tradition_communities are identity-locked targets (they cannot exit their relationship to the language without ceasing to be who they are, so they sit nearer the full-target end than mobile targets would); yiddish_speaking_immigrants are trapped targets (no exit from the country or the economy that ran in Hebrew). Spatial scope amplifies effective extraction modestly for the wide-reach seats (national/global enforcement surfaces are harder to verify and easier to over-apply).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — turn a liturgical language without native speakers into a working vernacular — is dead: solved by mid-century, corroborated from outside the benefiting parties by sociolinguistic scholarship and demographic data. The arrangement persists anyway, as infrastructure plus residual enforcement. The R5 mismatch consumer will flag status=dead x verdict=world_rearranges; the cross-check against theater_ratio (0.33, low) shows persistence-by-success rather than persistence-by-performance, so this is NOT a piton: the function (maintaining a shared vernacular, regulating expansion) remains real and exercised. The classification prevents two opposite mislabels: calling the arrangement pure extraction ignores the achieved coordination that virtually no participant, including most victims' descendants, would undo; calling it pure coordination ignores the desacralization and language-loss costs imposed through the identical machinery. Tangled rope with decaying extraction is the honest lifecycle label, and the late-intensification omega guards against premature closure of the decay narrative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the native_daily_reading of the hebrew_vitality kernel; what structural changes would adopting a sibling reading produce?',
    'Comparative read of the three sibling stories: shift the assessment seat and recompute beneficiary/victim sets per reading over the same referent arrangement.',
    'Under liturgical_reading the vernacular-enforcement project becomes the deviant party and the desacralization victim disappears; under hybrid_continuity_reading the liturgical substrate is reclassified from victim to necessary enabler, lowering measured extraction for the same historical arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one reading of a contested kernel, not the kernel itself.').

omega_variable(
    enforcement_excess_counterfactual,
    'Could nativization have been achieved with materially less suppression of rival languages and registers, or was the coercive package necessary to the outcome?',
    'Comparative revitalization cases (Catalan, Welsh, Maori, Irish) that pursued nativization with weaker suppression of rival codes; dose-response analysis of Yishuv enforcement intensity against acquisition rates.',
    'If less-coercive paths plausibly reach comparable nativization, the excess suppression is attributable to the specific regime rather than the goal, raising the extractive share of the arrangement; if not, part of the measured suppression is the price of the coordination achieved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_excess_counterfactual, empirical, 'Counterfactual necessity of the coercive enforcement package.').

omega_variable(
    desacralization_valence,
    'Is the desacralization of Hebrew a harm inflicted on the liturgical tradition, or a release of the language from a custody its former custodians had no standing to monopolize?',
    'Not resolvable by data alone; turns on prior commitments about religious ownership of language and the relative goods of sacrality and universal access.',
    'If desacralization is a harm, liturgical_tradition_communities count as genuine victims and the asymmetric-extraction reading stands; if a release, the victim set thins and the arrangement trends toward coordination-with-costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desacralization_valence, preference, 'Value-dependent status of the desacralization cost.').

omega_variable(
    late_interval_intensification,
    'Does the post-2004 uptick in extractiveness and suppression (Nation-State Law era, renewed Hebrew-only pressure on Arabic speakers, purist campaigns against loanwords) mark durable re-intensification or a transient political episode?',
    'Track enforcement indicators past the interval endpoint: legislation, ulpan mandates, municipal signage policy, Academy rulings on usage.',
    'Durable re-intensification would date a second enforcement ratchet and revise the decay-to-infrastructure narrative; a transient episode leaves the lifecycle reading intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(late_interval_intensification, empirical, 'Durability of the late-interval enforcement uptick.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__native_daily_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__native_daily_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(hebr_tr_t20, observed).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__native_daily_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement_basis(hebr_tr_t40, observed).
narrative_ontology:measurement(hebr_tr_t60, hebrew_vitality__native_daily_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement_basis(hebr_tr_t60, observed).
narrative_ontology:measurement(hebr_tr_t80, hebrew_vitality__native_daily_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement_basis(hebr_tr_t80, observed).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__native_daily_reading, theater_ratio, 100, 0.31).
narrative_ontology:measurement_basis(hebr_tr_t100, observed).
narrative_ontology:measurement(hebr_tr_t120, hebrew_vitality__native_daily_reading, theater_ratio, 120, 0.33).
narrative_ontology:measurement_basis(hebr_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__native_daily_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__native_daily_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(hebr_be_t20, observed).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__native_daily_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement_basis(hebr_be_t40, observed).
narrative_ontology:measurement(hebr_be_t60, hebrew_vitality__native_daily_reading, base_extractiveness, 60, 0.54).
narrative_ontology:measurement_basis(hebr_be_t60, observed).
narrative_ontology:measurement(hebr_be_t80, hebrew_vitality__native_daily_reading, base_extractiveness, 80, 0.47).
narrative_ontology:measurement_basis(hebr_be_t80, observed).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__native_daily_reading, base_extractiveness, 100, 0.43).
narrative_ontology:measurement_basis(hebr_be_t100, observed).
narrative_ontology:measurement(hebr_be_t120, hebrew_vitality__native_daily_reading, base_extractiveness, 120, 0.47).
narrative_ontology:measurement_basis(hebr_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__native_daily_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t20, hebrew_vitality__native_daily_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(hebr_su_t20, observed).
narrative_ontology:measurement(hebr_su_t40, hebrew_vitality__native_daily_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(hebr_su_t40, observed).
narrative_ontology:measurement(hebr_su_t60, hebrew_vitality__native_daily_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(hebr_su_t60, observed).
narrative_ontology:measurement(hebr_su_t80, hebrew_vitality__native_daily_reading, suppression_requirement, 80, 0.46).
narrative_ontology:measurement_basis(hebr_su_t80, observed).
narrative_ontology:measurement(hebr_su_t100, hebrew_vitality__native_daily_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement_basis(hebr_su_t100, observed).
narrative_ontology:measurement(hebr_su_t120, hebrew_vitality__native_daily_reading, suppression_requirement, 120, 0.42).
narrative_ontology:measurement_basis(hebr_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'Hebrew revival / Hebrew vitality' conflates three structurally distinct claims about what makes a language alive. Each reading gets its own epsilon, its own beneficiary/victim structure, and its own classification over the same historical referent. This story (native_daily_reading) is the upstream, historically victorious reading: its enforcement success changed the operating environment of both siblings — draining the liturgical_reading's claim that ritual use constitutes vitality, and supplying the historical material from which hybrid_continuity_reading builds its synthesis. Sibling files must link back here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
