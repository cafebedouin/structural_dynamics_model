% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: Native-Generation Criterion for Living Hebrew (Revival Enforcement Regime)
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the native_generation_reading of the kernel
 *   hebrew_living_language: the claim that Hebrew counts as a living language
 *   only when native speakers produce daily speech generatively, not via
 *   memorized recitation or written literary production alone. The standing
 *   arrangement under contest — and the epsilon referent — is the revival
 *   regime that operationalized this criterion: the Hebrew Language Committee
 *   (1890) and its successor Academy, the Hebrew-only school system,
 *   municipal and labor-market Hebrew-only norms, and the language wars (c.
 *   1900-1930) that closed the Yishuv's public sphere to Yiddish, Ladino, and
 *   other diaspora vernaculars. Assessed by this reading's own lights, the
 *   arrangement delivered a genuine coordination good — one shared vernacular
 *   for a linguistically fragmented immigrant society, achieved within
 *   roughly two generations — while imposing asymmetric, actively enforced
 *   costs on the vernacular-speaking populations whose languages were
 *   reclassified as exile debris, and a status cost on the
 *   liturgical-continuity practice the criterion retroactively devalues.
 *   Constraint-family note (epsilon-invariance decomposition): 'Hebrew is a
 *   living language' is one kernel with three structurally distinct readings.
 *   This reading carries the vernacular victim set and the enforcement
 *   history and authors moderate-to-substantial epsilon (0.58 at interval
 *   end) because the criterion's enforcement moved linguistic allegiance
 *   under coercion. The liturgical_continuity_reading's arrangement (unbroken
 *   recitation and study) coerces no one and would author low epsilon; the
 *   literary_revival_reading's arrangement (Haskalah written generative
 *   production) imposed no comparable coercion and would author
 *   low-to-moderate epsilon. The epsilon values differ because the standing
 *   arrangements differ — not because one constraint is measured two ways;
 *   the readings are separate files linked through
 *   network.affects_constraints. Claim/metric independence: claimed_type is
 *   authored from the structural facts (real coordination function +
 *   asymmetric extraction + active enforcement); the metrics are authored as
 *   descriptive estimates of the arrangement's actual operation; the engine
 *   computes per-seat classifications from the structural data.
 *
 * KEY AGENTS:
 *   - hebrew_language_institutions: agenda-setter (institutional/identity_locked) — defines and enforces the native-generation criterion; mandate flows from it
 *   - native_hebrew_speaking_generation: primary beneficiary (organized/identity_locked) — first native cohort in roughly seventeen centuries; the standard's living embodiment; heritage cost borne
 *   - yiddish_speakers: primary target (organized/constrained) — largest displaced vernacular; public sphere closed by rule and stigma
 *   - ladino_speakers: secondary target (moderate/constrained) — faster shift, thinner domestic institutional base
 *   - judeo_arabic_speakers: secondary target (moderate/constrained) — 1950s transit-camp enforcement wave under the same standard
 *   - diaspora_liturgical_communities: status-cost bearers outside the enforcement zone (organized/identity_locked) — practice retroactively devalued, not materially coerced
 *   - yiddishist_cultural_leadership: excluded counter-program (organized/mobile) — full rival standard, kept out of the room
 *   - hebrew_press_publishers: secondary beneficiary (organized/mobile) — protected Hebrew-language market
 *   - sociolinguistic_observers: analytical observer (analytical/analytical) — outside check on vitality claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.58).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.45).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Native-Generation Criterion for Living Hebrew (Revival Enforcement Regime)").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '71449279-23d4-439e-babc-748a47f8a907').
narrative_ontology:cs_kernel_codification('71449279-23d4-439e-babc-748a47f8a907', formalized).
narrative_ontology:cs_authority_grounding('71449279-23d4-439e-babc-748a47f8a907', expertise).
narrative_ontology:cs_interpretation_layer_present('71449279-23d4-439e-babc-748a47f8a907').
narrative_ontology:cs_reading_relation('71449279-23d4-439e-babc-748a47f8a907', hebrew_living_language__liturgical_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('71449279-23d4-439e-babc-748a47f8a907', hebrew_living_language__literary_revival_reading, forecloses).
narrative_ontology:cs_axiom('71449279-23d4-439e-babc-748a47f8a907', foundational, native_generative_speech_constitutes_liveness).
narrative_ontology:cs_axiom_status(native_generative_speech_constitutes_liveness, holdable).
narrative_ontology:cs_axiom_grounding('71449279-23d4-439e-babc-748a47f8a907', native_generative_speech_constitutes_liveness, empirically_contingent).
narrative_ontology:cs_axiom('71449279-23d4-439e-babc-748a47f8a907', secondary, revival_is_reconstruction_across_transmission_break).
narrative_ontology:cs_axiom_status(revival_is_reconstruction_across_transmission_break, holdable).
narrative_ontology:cs_axiom_grounding('71449279-23d4-439e-babc-748a47f8a907', revival_is_reconstruction_across_transmission_break, empirically_contingent).
narrative_ontology:cs_reference_frame('71449279-23d4-439e-babc-748a47f8a907', unbroken_native_speech_community_standard).
narrative_ontology:cs_drift_state('71449279-23d4-439e-babc-748a47f8a907', contemporary_post_revival_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('71449279-23d4-439e-babc-748a47f8a907', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_language_institutions).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, native_hebrew_speaking_generation).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_press_publishers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, judeo_arabic_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, diaspora_liturgical_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, native_hebrew_speaking_generation).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, native_intergenerational_transmission_criterion).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, language_revival_reconstruction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Hebrew Language Committee (founded 1890, successor the Hebrew Language Academy), together with the Hebrew education network and the Yishuv's cultural leadership, defined and administered the standard: Hebrew counts as living only when native speakers produce daily speech generatively. They built the machinery that made the standard true — Hebrew-only schools, terminological committees coining vocabulary for modern life, public campaigns, municipal and hiring norms — and their mandate, funding, and scholarly authority all flow from the standard they administer. The institutions' identity is fused with the revival mission; stepping back from the criterion would dissolve the reason they exist.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_language_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% The children raised in Hebrew-only households from the 1890s onward — the first native speakers in roughly seventeen centuries. The Yishuv's and later the state's entire public sphere came to run in their native language, and they carry the prestige of being the standard's living embodiment. They also paid: their parents' vernaculars were stripped from them, cutting them off from the literature, humor, and family archives carried in Yiddish, Ladino, or Judeo-Arabic. Their identity as the revival's proof is constitutive; exit would mean dissolving what they are.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, native_hebrew_speaking_generation, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__native_generation_reading, native_hebrew_speaking_generation, payer).

% Hebrew newspapers, publishing houses, and theater companies gained a protected and expanding market as Hebrew-only norms spread through schools, municipalities, and the labor market. Several had begun in Yiddish, German, or Russian and switched; publishers who stayed multilingual lost institutional standing and the school-age readership on which the domestic market increasingly ran. Exit remained possible in principle — diaspora publishing in other languages continued — but staying competitive inside the Yishuv meant Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_press_publishers, beneficiary,
    organized, biographical, mobile, national).

% Ashkenazi immigrants from Eastern Europe — the majority of the early waves — arrived carrying the largest modern Jewish vernacular culture. Inside the Yishuv they met Hebrew-only school rules, municipal ordinances, hiring norms, and a stigma campaign that marked Yiddish as the language of exile; Yiddish theater performances were disrupted and Yiddish papers pushed to the margins. Many kept Yiddish at home for a generation while the public sphere closed; the realistic exits were emigration, private retreat, or shift, and the Holocaust simultaneously destroyed the diaspora centers that might have sustained the language from outside.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speakers, payer,
    organized, generational, constrained, continental).

% Sephardi immigrants from the Balkans, Turkey, and North Africa brought Judeo-Spanish, a vernacular with five centuries of print culture. Smaller and less institutionally entrenched in the Yishuv than Yiddish, they met the same Hebrew-only schooling and the same classification of their language as diaspora debris; without a large domestic institutional base to resist with, shift came faster, and Ladino survived mainly in liturgical use, domestic speech with elders, and diaspora communities.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speakers, payer,
    moderate, generational, constrained, continental).

% Mizrahi Jews from Arabic-speaking lands arrived in the mass immigration of the 1950s into transit camps where Hebrew-only schooling and army instruction treated their vernaculars as obstacles to absorption. Children were schooled exclusively in Hebrew and parents were pressed to drop Judeo-Arabic at home to speed integration; the vernaculars receded within a generation or two, surviving in music, family speech, and diaspora communities.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, judeo_arabic_speakers, payer,
    moderate, generational, constrained, regional).

% Traditional communities across the diaspora maintained Hebrew through unbroken liturgical recitation, textual study, and epistolary use for roughly seventeen centuries after native speech ceased. They sit outside the enforcement zone — no school rule or ordinance reached them — but the standard the revival regime enforces retroactively reclassifies their entire practice as not constituting a living language, devaluing the continuity narrative their identity is built on. The practice is constitutive of their religious identity, so exit from it is unthinkable; they hold the devalued claim.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, diaspora_liturgical_communities, payer,
    organized, civilizational, identity_locked, global).

% Writers, journalists, and political organizers centered in Warsaw, Vilna, and New York ran a complete counter-program: Yiddish as the Jewish national language, with its own schools, press, canon, and political parties. They were excluded from the Yishuv's Hebrew-only institutions and from the language-planning conversation itself; their theatrical productions inside the Yishuv were disrupted and their program polemicized as exile-speak. They argued the question of what makes a Jewish language living from outside a room they were never invited into.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddishist_cultural_leadership, excluded,
    organized, generational, mobile, continental).

% Comparative linguists and, later, sociolinguists assess the revival against general theory: intergenerational transmission, domain coverage, corpus development, native-speaker demography. They take no side in the national contest; their findings — that the speech community crossed from reconstructed to natively transmitted within roughly two generations — are the outside check on both the revival's claims and its critics'.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, sociolinguistic_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__native_generation_reading, native_hebrew_speaking_generation).
narrative_ontology:fixing_cost_class(hebrew_living_language__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a real collective problem: a population of immigrants speaking dozens of mutually unintelligible languages needed one shared vernacular for state, school, army, and market within a single generation. The criterion supplied the shared standard and the urgency — raise native-speaking children now — that converted language planning from aspiration into household practice.
% TRANSFER_FUNCTION: Moves daily communicative allegiance from the diaspora vernaculars (Yiddish, Ladino, Judeo-Arabic, and others) to Hebrew; moves public status and institutional access to native Hebrew speakers; moves the cultural-continuity claim from liturgical and literary practice to native generative speech; moves the costs of the shift — heritage-language loss — onto the vernacular-speaking generations who make it.
% ABSENT_VOICES: Yiddishist cultural leadership (the excluded stakeholder) ran a complete counter-program and was kept outside the Yishuv's Hebrew-only institutions; Ladino and Judeo-Arabic community elders were treated as absorption problems rather than parties to the standard's definition; diaspora liturgical authorities, whose continuity claim the criterion invalidates, were polemicized against rather than engaged. Unanimity around the native-generation criterion arose partly because these seats were never in the room.
% DISAPPEARANCE_RATIONALE: Had the criterion and its enforcement machinery vanished around 1930, the Yishuv's language equilibrium would have rearranged: Yiddish had the demographic mass and institutional base to remain a co-official vernacular, German retained the technical-education foothold contested in the Technion language war, and Ladino and Judeo-Arabic presses would not have been pushed to the margins — the likely outcome is a multilingual Jewish society with Hebrew as prestige liturgical-literary language rather than a monolingual Hebrew state. After native transmission became self-sustaining (post-1950), disappearance would rearrange far less: the speech community would persist on its own, though immigrant-absorption practice and the Academy's gatekeeping would lose their justifying standard.
% FOUNDING_PROBLEM: Hebrew had had no native speakers for roughly seventeen centuries; the national movement needed a language that could run a state, an army, a school system, and a market, and its standard-bearers held that only a natively transmitted, generatively spoken language would count as living. The criterion was built to define that threshold and to mobilize the radical household and school measures needed to cross the transmission break within a generation.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting set: academic sociolinguistic assessments of Hebrew vitality — a literature with no stake in the revival institutions' mandate — treat native intergenerational transmission as established and self-sustaining since roughly the state's first decades; the founding problem is solved. The Academy's own attestation of a continuing live mandate (anglicism defense, terminological standard-setting) comes from inside the benefiting set and describes shifted functions, not the founding problem. No outside party attests that the original problem remains unsolved.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction (0.58 at interval end) is moderate: the arrangement's costs fell asymmetrically on vernacular speakers — Yiddish speakers lost a public sphere they had the demographic mass to dominate; Ladino and Judeo-Arabic speakers lost their vernaculars within one to two generations — while the coordination good was real and widely shared. Suppression (0.45) is authored as the raw structural coercive force of the standing arrangement: Hebrew-only school rules, municipal ordinances, hiring norms, disrupted Yiddish theater, and the stigma machinery of the language wars; it is deliberately unscaled here — only extractiveness is scaled by the engine (directionality and scope). Theater (0.38) reflects the post-consolidation phase, in which a growing share of maintenance is ceremonial: Academy pronouncements, dugri heritage performance, and symbolic Hebrew-only signaling that no longer carries the enforcement load. The measurement series run on one shared time grid (1890-1970, nine points) with every tracked metric authored at every point. base_extractiveness peaks in the 1930s (0.66) as the language wars and mass immigration close the public sphere, then declines as the vernacular extraction completes while shifting toward new immigrant languages. suppression_requirement is authored because this story specifically traces enforcement-capacity change: an enforcement arc — built up through the Technion war and language wars (peak 0.70, c. 1930), then decaying (0.45 by 1970) as native transmission became self-sustaining and coercion gave way to accomplished fact. theater_ratio rises monotonically as the function completes and maintenance turns performative. The interval is entirely historical; every point is observed, none projected.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From inside the language institutions, the criterion is the standard they exist to uphold: their authority, mandate, and identity are fused with it (institutional identity lock), and from that seat the arrangement reads as the coordination project they built. From the yiddish_speakers seat — organized, with a continental institutional base, but facing a closing public sphere — the same structure reads as enforced displacement, with exit constrained to emigration, domestic retreat, or shift. Same-level differentiation among payer seats: yiddish_speakers (organized, continental) had the institutional base to resist for decades and did; ladino_speakers (moderate, continental) shifted faster for lack of a comparable domestic base; judeo_arabic_speakers (moderate, regional) met the criterion's enforcement in its 1950s camp-and-ulpan phase — a later wave, the same justifying standard, different exit conditions. The diaspora_liturgical_communities seat differs in kind: outside the enforcement zone and materially untouched, but bearing the criterion's status cost — their practice retroactively reclassified as not constituting liveness — with identity-locked exit (the practice is constitutive of their religious identity). Directionality overrides are deliberately not used: the override surface is keyed by power atom, and this story's differentiation runs between same-power seats (organized payers with different exits, scopes, and timing), which a power-atom-keyed override cannot express; the beneficiary/victim declarations and exit atoms carry the differentiation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit near the beneficiary end of d: hebrew_language_institutions near 0 (they administer the criterion and their mandate flows from it); native_hebrew_speaking_generation near 0 but lifted slightly off the floor — the standard's living embodiment, whose benefit was purchased with their parents' vernaculars; hebrew_press_publishers low-moderate (protected market, but several paid to switch languages). Victims sit near the target end: yiddish_speakers near full-target (largest displaced vernacular, public sphere closed, constrained exit); ladino_speakers and judeo_arabic_speakers high (faster shift, less resistance capacity). Diaspora liturgical communities sit moderately high rather than near full-target: they are targets of the criterion's devaluation but not of its enforcement machinery — a status target with identity-locked exit, outside the national scope where enforcement operated. Scope: the enforcement operated at national scope (the Yishuv and then Israel), which the engine treats as modestly amplifying effective extraction for agents inside it; the continental and diaspora seats sit partly outside that scope, damping their effective extraction relative to their declared victim position — consistent with their situations.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetric mislabels. Mislabeling as pure extraction (snare) would erase the genuine coordination function: a society of mutually unintelligible immigrant communities did need a single vernacular, and the criterion supplied the mobilizing standard — raise native-speaking children now — that accomplished in two generations what ordinary multilingual drift had not accomplished in seventy years. Mislabeling as pure coordination (rope) would erase the asymmetric, enforced cost: Yiddish and Ladino speakers did not merely lose a competition; the public sphere was closed to their languages by school rules, ordinances, hiring norms, and disrupted theater, and alternatives were suppressed rather than outcompeted. Tangled_rope holds both facts, and the identity-lock dynamics matter on both sides: the language institutions' identity is fused with the revival mission (institutional identity — the organization has become its function), and the first native generation's identity is constituted by being the revival's proof (relational/ideological identity — exit is unthinkable without dissolving what they are); if either identity frame broke, the persistence story would change from accomplished fact to contested standard. The R5 genealogy sharpens the picture: the founding problem — cross the native-transmission break, establish native generative Hebrew — is dead, solved by roughly the state's second decade; the criterion persists in shifted functions (standard-setting, anglicism defense, immigrant-absorption gatekeeping) administered by institutions whose mandate the original problem justified. The founding_problem_status=dead combined with disappearance_verdict=world_rearranges is authored deliberately and left for the consumer to cross-check against the theater path rather than reconciled away: the arrangement's persistence is now carried more by accomplished fact and institutional inertia than by the enforcement that built it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the kernel hebrew_living_language. Is ''living language'' properly indexed to native generative speech (this reading), to unbroken liturgical recitation (liturgical_continuity_reading), or to written generative production (literary_revival_reading)? The historical record under-determines the choice, and the readings'' premises are mutually contradictory within any single framework.',
    'Comparative classification across the three sibling stories: if the sibling readings'' structural profiles (victim sets, enforcement burdens, epsilon) account for the same phenomena with less residue, the native-generation reading loses its distinctness; if only this reading accounts for the vernacular displacement and the two-generation crossing of the transmission break, it stands.',
    'Under the liturgical reading the victim set largely dissolves (continuity is vindicated; recitation displaced no vernacular) and epsilon drops toward coordination cost; under the literary reading the enforcement history attaches to no arrangement and the criterion''s coercive phase becomes unowned. Which historical costs are attributed to a ''living Hebrew'' commitment at all follows the choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which truth conditions for ''living language'' the kernel''s contest turns on; each choice yields a structurally different constraint.').

omega_variable(
    vernacular_loss_attribution,
    'How much of the Yiddish and Ladino decline is attributable to the Hebrew-only regime''s enforcement, as opposed to independent causes (destruction of the Yiddish heartlands in the Holocaust, migration disruption, general assimilation pressure)?',
    'Counterfactual trajectory comparison with diaspora Yiddish and Ladino communities outside the enforcement zone (Americas, pre-war Eastern Europe), controlling for Holocaust and migration effects; within-Israel cohort language-retention data by community and arrival wave.',
    'If most of the decline traces to independent causes, the arrangement''s epsilon drops substantially — the regime rode a wave rather than causing it — and the classification trends toward rope; if enforcement was decisive for the public-sphere closure, epsilon holds and the tangled_rope reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_loss_attribution, empirical, 'Attribution of vernacular decline between regime enforcement and independent historical causes.').

omega_variable(
    mizrahi_extraction_phase_boundary,
    'Is the 1950s pressure on Mizrahi Judeo-Arabic speakers part of this constraint (the native-generation criterion''s enforcement) or a distinct state absorption arrangement that merely borrowed the standard''s vocabulary?',
    'Trace the justifying criteria invoked in transit-camp and school language policy of the 1950s: if policy documents invoke native-generative liveness, the phase belongs to this constraint; if they invoke state-integration efficiency alone, it is a separate constraint and should be authored as its own story.',
    'If distinct, this story''s victim set shrinks to the Ashkenazi and Sephardi vernacular displacements and end-state epsilon drops; if the same constraint, epsilon rises (a second extraction wave under the same criterion) and the persistence story strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mizrahi_extraction_phase_boundary, empirical, 'Whether the Mizrahi vernacular displacement belongs to this constraint or to a sibling absorption arrangement.').

omega_variable(
    reconstruction_fidelity,
    'The reading acknowledges the strict-reachability break: how much of revived Hebrew''s structure descends from continuous internal traditions (Mishnaic Hebrew substrate, rabbinic usage) versus transfer from the revivers'' European vernaculars (Yiddish, Russian, German syntax and phonology)?',
    'Diachronic analysis of the revival corpus: distribution of Mishnaic features, calques, and phonological transfers across the first native-speaking cohorts'' output versus the liturgical and literary baseline.',
    'Heavy European transfer would strengthen the reconstruction-required axiom and raise the status cost the criterion imposes on liturgical-continuity claims (the revived language is a new assembly, not a reawakened one); strong Mishnaic continuity would blur this reading''s distinctness from the liturgical sibling and lower the break''s classification weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_fidelity, empirical, 'Continuity versus reconstruction in the revived language''s structure; bears on the reading''s break-acknowledgment axiom.').

omega_variable(
    criterion_scope_general_vs_particular,
    'Does the criterion operate as a general sociolinguistic thesis (any language is living only under native generative transmission) or as a particular instrument of the Hebrew national project (this language, this revival, this standard)?',
    'Test the criterion''s application outside the Hebrew case: if its proponents applied it uniformly (including to Yiddish''s own vitality and to Arabic among Palestinian citizens), it is a general thesis; if it was suspended where application was costly to the national project, it is a particular instrument.',
    'A general thesis reads closer to an identity standard with lower epsilon; a particular instrument reads as an ideological device whose epsilon includes its selective application — raising effective extraction for the seats it was selectively applied against.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criterion_scope_general_vs_particular, conceptual, 'General sociolinguistic thesis versus particular nationalist instrument.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 1890, 1970).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1890, hebrew_living_language__native_generation_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t1890, observed).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_living_language__native_generation_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t1900, observed).
narrative_ontology:measurement(hebr_tr_t1910, hebrew_living_language__native_generation_reading, theater_ratio, 1910, 0.15).
narrative_ontology:measurement_basis(hebr_tr_t1910, observed).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_living_language__native_generation_reading, theater_ratio, 1920, 0.19).
narrative_ontology:measurement_basis(hebr_tr_t1920, observed).
narrative_ontology:measurement(hebr_tr_t1930, hebrew_living_language__native_generation_reading, theater_ratio, 1930, 0.23).
narrative_ontology:measurement_basis(hebr_tr_t1930, observed).
narrative_ontology:measurement(hebr_tr_t1940, hebrew_living_language__native_generation_reading, theater_ratio, 1940, 0.27).
narrative_ontology:measurement_basis(hebr_tr_t1940, observed).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_living_language__native_generation_reading, theater_ratio, 1950, 0.31).
narrative_ontology:measurement_basis(hebr_tr_t1950, observed).
narrative_ontology:measurement(hebr_tr_t1965, hebrew_living_language__native_generation_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement_basis(hebr_tr_t1965, observed).
narrative_ontology:measurement(hebr_tr_t1970, hebrew_living_language__native_generation_reading, theater_ratio, 1970, 0.38).
narrative_ontology:measurement_basis(hebr_tr_t1970, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1890, hebrew_living_language__native_generation_reading, base_extractiveness, 1890, 0.34).
narrative_ontology:measurement_basis(hebr_be_t1890, observed).
narrative_ontology:measurement(hebr_be_t1900, hebrew_living_language__native_generation_reading, base_extractiveness, 1900, 0.41).
narrative_ontology:measurement_basis(hebr_be_t1900, observed).
narrative_ontology:measurement(hebr_be_t1910, hebrew_living_language__native_generation_reading, base_extractiveness, 1910, 0.49).
narrative_ontology:measurement_basis(hebr_be_t1910, observed).
narrative_ontology:measurement(hebr_be_t1920, hebrew_living_language__native_generation_reading, base_extractiveness, 1920, 0.6).
narrative_ontology:measurement_basis(hebr_be_t1920, observed).
narrative_ontology:measurement(hebr_be_t1930, hebrew_living_language__native_generation_reading, base_extractiveness, 1930, 0.66).
narrative_ontology:measurement_basis(hebr_be_t1930, observed).
narrative_ontology:measurement(hebr_be_t1940, hebrew_living_language__native_generation_reading, base_extractiveness, 1940, 0.64).
narrative_ontology:measurement_basis(hebr_be_t1940, observed).
narrative_ontology:measurement(hebr_be_t1950, hebrew_living_language__native_generation_reading, base_extractiveness, 1950, 0.61).
narrative_ontology:measurement_basis(hebr_be_t1950, observed).
narrative_ontology:measurement(hebr_be_t1965, hebrew_living_language__native_generation_reading, base_extractiveness, 1965, 0.59).
narrative_ontology:measurement_basis(hebr_be_t1965, observed).
narrative_ontology:measurement(hebr_be_t1970, hebrew_living_language__native_generation_reading, base_extractiveness, 1970, 0.58).
narrative_ontology:measurement_basis(hebr_be_t1970, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1890, hebrew_living_language__native_generation_reading, suppression_requirement, 1890, 0.25).
narrative_ontology:measurement_basis(hebr_su_t1890, observed).
narrative_ontology:measurement(hebr_su_t1900, hebrew_living_language__native_generation_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement_basis(hebr_su_t1900, observed).
narrative_ontology:measurement(hebr_su_t1910, hebrew_living_language__native_generation_reading, suppression_requirement, 1910, 0.5).
narrative_ontology:measurement_basis(hebr_su_t1910, observed).
narrative_ontology:measurement(hebr_su_t1920, hebrew_living_language__native_generation_reading, suppression_requirement, 1920, 0.66).
narrative_ontology:measurement_basis(hebr_su_t1920, observed).
narrative_ontology:measurement(hebr_su_t1930, hebrew_living_language__native_generation_reading, suppression_requirement, 1930, 0.7).
narrative_ontology:measurement_basis(hebr_su_t1930, observed).
narrative_ontology:measurement(hebr_su_t1940, hebrew_living_language__native_generation_reading, suppression_requirement, 1940, 0.62).
narrative_ontology:measurement_basis(hebr_su_t1940, observed).
narrative_ontology:measurement(hebr_su_t1950, hebrew_living_language__native_generation_reading, suppression_requirement, 1950, 0.54).
narrative_ontology:measurement_basis(hebr_su_t1950, observed).
narrative_ontology:measurement(hebr_su_t1965, hebrew_living_language__native_generation_reading, suppression_requirement, 1965, 0.48).
narrative_ontology:measurement_basis(hebr_su_t1965, observed).
narrative_ontology:measurement(hebr_su_t1970, hebrew_living_language__native_generation_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement_basis(hebr_su_t1970, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, literary_revival_reading).

% DUAL FORMULATION NOTE:
% 'Hebrew is a living language' decomposes per the epsilon-invariance principle into three structurally distinct readings with different truth conditions for the same predicate, different standing arrangements, different epsilon values, and different victim sets. This story authors the native_generation_reading (epsilon 0.58 at interval end; victims: non-Hebrew vernacular speakers plus the status-devalued liturgical continuity practice; enforcement history 1890-1970). The liturgical_continuity_reading authors the recitation-and-study arrangement (low extraction — recitation coerces no outsider; expected mountain/rope-side profile). The literary_revival_reading authors the Haskalah literary-production arrangement (no comparable coercion; low-to-moderate epsilon). Upstream/downstream structure: the literary and liturgical readings supply the corpus and continuity claims this reading cites as the raw material it declares insufficient, while this reading's success retroactively devalues both siblings' liveness claims. Each family file links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
