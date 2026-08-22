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
 *   constraint_id: hebrew_vitality__native_daily_reading
 *   human_readable: Native-Generation Vitality Criterion of the Hebrew Vernacularization Regime
 *   domain: sociolinguistic/political
 *
 * SUMMARY:
 *   This story authors the native_daily_reading of the hebrew_vitality
 *   kernel: only native intergenerational transmission constitutes language
 *   vitality; ritual recitation is preservation, not life. The standing
 *   arrangement under contest is the Hebrew vernacularization regime — the
 *   institutional machinery (the Hebrew Language Committee from 1890,
 *   Hebrew-only schools from the 1900s, the Gedud Meginei Hasafa street
 *   enforcers of the 1920s, state ulpanim and army socialization after 1948,
 *   the Academy of the Hebrew Language from 1953, and the 2018 Nation-State
 *   Law's constitutional ranking of Hebrew) that converted a liturgical
 *   language into the native daily tongue of a sovereign society, together
 *   with the criterion that legitimated it and declared the project
 *   successful. The regime solved a real coordination problem and imposed
 *   real costs: the mother tongues of Yiddish-, Ladino-, and
 *   Judeo-Arabic-speaking immigrants were displaced within a generation, the
 *   sacred register was desacralized, and Arabic speakers live under a
 *   language hierarchy they did not consent to. Family structure and sibling
 *   deltas are documented in network.dual_formulation_note and the
 *   kernel_reading_contest omega; per Rule 1 this file generates only its own
 *   reading as a clean, epsilon-invariant constraint.
 *
 * KEY AGENTS:
 *   - zionist_state_institutions: agenda-setter (institutional/arbitrage) — sets and enforces language policy, collects the arrangement's legitimacy and integration gains
 *   - hebrew_intelligentsia: primary beneficiary (organized/identity_locked) — status and employment from the revival, identity fused with it
 *   - sabra_native_generation: dual-positioned (moderate/identity_locked) — inherits the deliverable as beneficiary, bore the childhood coercion as payer
 *   - yiddish_speaking_immigrants: primary historical target (moderate/trapped) — mother tongue displaced under active enforcement
 *   - traditional_liturgical_communities: target of desacralization (organized/constrained, global scope) — the sacred register profaned
 *   - mizrahi_diaspora_language_speakers: target of absorption-era language shift (powerless/trapped) — fragmented, no coalition leverage at arrival
 *   - arabic_speaking_citizens: ongoing target of the language hierarchy (organized/constrained) — downgraded by the 2018 Nation-State Law
 *   - yiddishist_cultural_movement: excluded voice (organized/trapped, global) — the suppressed alternative program
 *   - sociolinguistic_researchers: analytical observer (institutional/analytical, global) — adjudicates the kernel contest from outside the beneficiary set
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__native_daily_reading, 0.42).
domain_priors:suppression_score(hebrew_vitality__native_daily_reading, 0.22).
domain_priors:theater_ratio(hebrew_vitality__native_daily_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hebrew_vitality__native_daily_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__native_daily_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_vitality__native_daily_reading, "Native-Generation Vitality Criterion of the Hebrew Vernacularization Regime").
narrative_ontology:topic_domain(hebrew_vitality__native_daily_reading, "sociolinguistic/political").

domain_priors:requires_active_enforcement(hebrew_vitality__native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__native_daily_reading, '379271b7-e583-4417-a775-ae274903b73d').
narrative_ontology:cs_kernel_codification('379271b7-e583-4417-a775-ae274903b73d', distributed).
narrative_ontology:cs_authority_grounding('379271b7-e583-4417-a775-ae274903b73d', expertise).
narrative_ontology:cs_interpretation_layer_present('379271b7-e583-4417-a775-ae274903b73d').
narrative_ontology:cs_reading_relation('379271b7-e583-4417-a775-ae274903b73d', hebrew_vitality__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('379271b7-e583-4417-a775-ae274903b73d', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('379271b7-e583-4417-a775-ae274903b73d', foundational, native_transmission_constitutes_vitality).
narrative_ontology:cs_axiom_status(native_transmission_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('379271b7-e583-4417-a775-ae274903b73d', native_transmission_constitutes_vitality, empirically_contingent).
narrative_ontology:cs_axiom('379271b7-e583-4417-a775-ae274903b73d', foundational, ritual_recitation_is_preservation_not_life).
narrative_ontology:cs_axiom_status(ritual_recitation_is_preservation_not_life, holdable).
narrative_ontology:cs_axiom_grounding('379271b7-e583-4417-a775-ae274903b73d', ritual_recitation_is_preservation_not_life, empirically_contingent).
narrative_ontology:cs_reference_frame('379271b7-e583-4417-a775-ae274903b73d', native_transmission_vitality_standard).
narrative_ontology:cs_drift_state('379271b7-e583-4417-a775-ae274903b73d', contemporary_hybridity_debate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('379271b7-e583-4417-a775-ae274903b73d', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__native_daily_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, zionist_state_institutions).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, hebrew_intelligentsia).
narrative_ontology:constraint_beneficiary(hebrew_vitality__native_daily_reading, sabra_native_generation).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, yiddish_speaking_immigrants).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, traditional_liturgical_communities).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, mizrahi_diaspora_language_speakers).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, arabic_speaking_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_vitality__native_daily_reading, sabra_native_generation).
narrative_ontology:constraint_vindicates(hebrew_vitality__native_daily_reading, native_transmission_vitality_criterion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Hebrew Language Committee (founded 1890, reconstituted 1904), the Hebrew school networks of the Yishuv, and after 1948 the Ministry of Education, the state ulpan system, and the Academy of the Hebrew Language (1953). They set language policy: Hebrew-only schooling, army Hebrew socialization, mandatory Hebrew instruction for new immigrants, official coinage of new terms, and the legal hierarchy that places Hebrew first, culminating in the 2018 Nation-State Law's designation of Hebrew as the state's language with Arabic demoted to 'special status'. They collect the legitimacy and administrative integration a shared tongue provides, and they fund and staff the machinery that maintains it.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, zionist_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% The teachers, writers, journalists, and lexicographers of the revived language — from the teachers who taught Hebrew-in-Hebrew in the first Hebrew schools to the Academy's academicians. The revival gave them employment, public standing, and a sense of national mission; figures like Eliezer Ben-Yehuda devoted their entire adult lives to it. Their professional and personal identity is fused with the project; stepping outside it would mean disowning their life's work and social position.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, hebrew_intelligentsia, beneficiary,
    organized, biographical, identity_locked, national).

% The first generations raised with Hebrew as a mother tongue, from the children of the Second and Third Aliyah onward. As children many were punished at school for speaking their parents' languages; as adults they hold a native command their parents never had, and the criterion's success is written in their speech. The language and the national self-understanding built on it are their inheritance; they cannot imagine themselves outside it.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, sabra_native_generation, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__native_daily_reading, sabra_native_generation, payer).

% The mass of immigrants from Eastern Europe across the aliyot and after the Holocaust, for whom Yiddish was the mother tongue of daily life. Their children were punished for Yiddish at school, Yiddish newspapers and theaters were pushed to the margins of public life, and the language was ideologically branded the speech of exile. They had already uprooted once to arrive; leaving again was not a real option, and their children's futures lay inside the Hebrew-speaking economy.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, yiddish_speaking_immigrants, payer,
    moderate, biographical, trapped, national).

% Religious and traditional Jews in Palestine and across the diaspora for whom the holy tongue's sanctity is constitutive — prayer, Torah study, and legal formulae in lashon ha-kodesh. The vernacularization turned the language of the liturgy into the language of the street and the market, coined secular terms from biblical roots, and put scripture's vocabulary to commercial and bureaucratic use. Many experienced this as profanation; their position inside the tradition is not one they can leave without leaving the tradition itself.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, traditional_liturgical_communities, payer,
    organized, civilizational, constrained, global).

% Jewish immigrants from Muslim and Mediterranean lands — Ladino-speaking Sephardim and Judeo-Arabic-speaking communities from Iraq, Yemen, Morocco, and elsewhere — absorbed from 1948 onward through transit camps and state ulpanim. Their mother tongues, several with a millennium of continuous history, receded within a single generation; the state's absorption machinery was built to replace them, not maintain them, and the newcomers arrived fragmented across language groups with no coalition leverage inside the receiving institutions.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, mizrahi_diaspora_language_speakers, payer,
    powerless, biographical, trapped, national).

% Palestinian citizens of the state, roughly a fifth of the population, whose daily language predates the arrangement that ranks languages. Arabic held formal official status until the 2018 Nation-State Law downgraded it; state services, signage, higher education, and economic advancement run through Hebrew. They live inside the language hierarchy without having been party to its founding decisions, and their organized responses run through courts and advocacy rather than any seat in the original settlement.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, arabic_speaking_citizens, payer,
    organized, generational, constrained, national).

% The Bundist and YIVO-centered current that held Yiddish — the actually spoken language of the Jewish masses — to be the authentic basis of Jewish national culture. It was shut out of the Zionist linguistic settlement: its press marginalized in the Yishuv, its program never given a hearing in the institutions that decided, its European base destroyed in the Holocaust. Its claim survives mainly in diaspora archives and later scholarship.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, yiddishist_cultural_movement, excluded,
    organized, generational, trapped, global).

% Scholars of language revival, endangerment, and policy — from the intergenerational-transmission frameworks of Fishman and the UNESCO vitality indicators to the hybridity critique of the revival's standard success narrative. They assess the Hebrew case against comparative evidence, document the costs to the displaced languages, and adjudicate the contest over what the revival proves, from a seat outside the arrangement's beneficiaries.
narrative_ontology:constraint_stakeholder(hebrew_vitality__native_daily_reading, sociolinguistic_researchers, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__native_daily_reading, zionist_state_institutions).
narrative_ontology:fixing_cost_class(hebrew_vitality__native_daily_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A polyglot immigrant society — Yiddish, Russian, Polish, German, Ladino, and Judeo-Arabic speakers arriving in successive waves — needed one shared vernacular for schooling, labor, army, markets, and administration; Hebrew was the only available candidate with pan-Jewish legitimacy and no colonizer's taint. The arrangement solved the common-language problem by making Hebrew the medium of daily life, and the native-transmission criterion supplied the standard by which that solution was declared achieved.
% TRANSFER_FUNCTION: Moves daily linguistic practice — home speech, children's first language, public discourse — out of the immigrants' diaspora mother tongues and into Hebrew; moves Hebrew itself out of the liturgical register into secular daily use; and concentrates cultural authority, publishing, teaching employment, and institutional legitimacy inside the Hebrew-language institutions that administer the standard.
% ABSENT_VOICES: The Yiddishist cultural movement and the diaspora Yiddish establishment would have objected that a national language was being chosen by suppressing the actual vernacular of the Jewish masses; traditional religious authorities would have objected to the desacralization of the holy tongue; the immigrant children punished for their mother tongues had no voice at all in the decision that reshaped their speech. All stood outside the rooms where the language regime was decided.
% DISAPPEARANCE_RATIONALE: If the vernacularization regime and its criterion vanished overnight, Israeli society would reorganize around its actual multilingual practice — Russian, Amharic, Arabic, and English publics would formalize; the state would lose its principal integration instrument and a core of its founding legitimacy narrative; and Hebrew's high functions would drift back toward the liturgical register the criterion displaced. The arrangement is load-bearing for a sovereign society of nine million.
% FOUNDING_PROBLEM: The Yishuv's founding linguistic problem: a nation of immigrants with no common spoken language, whose putative national language nobody spoke natively — how to make a daily vernacular out of a liturgical language before the immigrant streams hardened into separate language communities.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by sociolinguistic scholarship on the revival — the standard account that native transmission was achieved by the first Palestine-born generation — and by the very fact the criterion was designed to test: an unbroken native-speaker chain has existed since the 1920s, and no party disputes that the founding problem as stated was solved. What remains contested is the accounting: what the solution cost, and whether the liturgical chain deserves causal credit (the hybrid reading's claim).
narrative_ontology:disappearance_verdict(hebrew_vitality__native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_vitality__native_daily_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__native_daily_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__native_daily_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__native_daily_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__native_daily_reading_tests).
:- end_tests(hebrew_vitality__native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 at interval end) because the costs were and are real — hundreds of thousands of speakers lost mother tongues within a generation, the liturgical register lost its monopoly on the language, Arabic was constitutionally downgraded in 2018 — while the deliverable, a shared native vernacular, is held by most participants as genuinely theirs. Suppression is now low (0.22) because enforcement atrophied once native transmission made coercion redundant; note that suppression is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater_ratio (0.40) is the metric with the clearest drift: as functional enforcement decayed, the Academy's coinage activity increasingly fails adoption, and Hebrew-only rhetoric diverges from de facto multilingual life. Accessibility_collapse (0.45): alternatives were largely closed during the enforcement era (1920-1960) but have partially re-opened — a Russian-language media sphere, Arabic press, ubiquitous English — so the current arrangement does not fully foreclose exit into other linguistic worlds. Resistance (0.35): current resistance is institutional-legal (Arabic language-rights litigation, immigrant-language advocacy, hybridity scholarship) rather than mass. The three measurement series share one time grid {1890, 1920, 1948, 1970, 1995, 2025}; the suppression_requirement series is authored because the story specifically tracks enforcement-capacity change — the build-up to the 1920s coercive peak and the decay thereafter.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat (Zionist institutions), the arrangement is a nation-building triumph and the criterion its vindication: the founding problem was solved, and the machinery that solved it deserves credit. From the payer seats, the same structure reads as coercive uprooting — Yiddish-speaking immigrants experienced school punishment and social boycott; traditional liturgical communities experienced profanation; Mizrahi immigrants lost thousand-year-old vernaculars inside a decade; Arabic-speaking citizens inherited a hierarchy they never joined. The sabra seat is genuinely dual: beneficiary of the deliverable, payer of the coercion that produced it, now identity-locked into the beneficiary position. The observer seat sees a successful revival with a contested cost accounting. The engine computes per-seat classifications from the structural data; this file's claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to low directionality: Zionist institutions (agenda-setter and gain recipient), the revival intelligentsia, and the native generation sit near the beneficiary end — with identity-lock amplifying the intelligentsia's and sabras' positions, since exit from the arrangement is exit from their selves. The victim declarations map to high directionality: Yiddish-speaking immigrants and Mizrahi speakers are trapped (already uprooted; children's futures inside the Hebrew economy; Mizrahim arrived fragmented across language groups, so no coalition power was available to them despite their numbers), traditional liturgical communities are constrained with civilizational time horizons and global scope (large scope amplifies their effective extraction), and Arabic-speaking citizens are constrained insiders of a hierarchy they did not found. The sabra generation's dual role (beneficiary with payer secondary_role) is the structural hinge: the arrangement's costs were front-loaded onto its beneficiaries' own childhoods, which is why the extraction did not produce durable resistance from the seat that now holds the deliverable.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is dead — native transmission was achieved within a generation — but the arrangement is not a piton: theater_ratio 0.40 sits below the inertial band, the machinery retains a live function (integrating each new immigrant wave, maintaining the shared vernacular, adjudicating the standard), and disappearance would rearrange the world. The tangled_rope claim prevents both mislabelings. Calling the arrangement pure coordination would erase the destroyed mother tongues, the desacralized liturgy, and the subordinated Arabic; calling it pure extraction would erase the coordination good actually delivered and now held as identity by the very generation whose childhoods paid for it. The historical extraction was partly the price of a coordination good — which is the tangled-rope structure exactly. The R5 mismatch (founding_problem_status dead x disappearance_verdict world_rearranges) is authored honestly: the arrangement succeeded its mandate and persists because the world now depends on it, not because its function atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This story authors one reading of the hebrew_vitality kernel — the native_daily_reading. What would the sibling readings change structurally if adopted as the operative constraint?',
    'No dataset resolves a reading choice; the corpus resolves it by authoring each reading as its own story (hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading) and comparing per-seat classifications across the family.',
    'Under the liturgical_reading the arrangement under contest becomes the liturgical-preservation chain itself (near-zero extraction for it; the vernacularizers appear as its desecrators); under the hybrid_continuity_reading the beneficiary and payer sets merge — the liturgical substrate becomes a co-beneficiary and the reconstruction''s costs are split. This file''s moderate epsilon and its beneficiary/payer split hold only within the native_daily_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one of three readings of the hebrew_vitality kernel; siblings would relocate epsilon, beneficiaries, and victims.').

omega_variable(
    vitality_definition_location,
    'Where in the structure do the readings disagree? The native_daily_reading locates vitality in a constitutive criterion (unbroken native intergenerational transmission; ritual recitation is preservation, not life); the liturgical_reading locates it in continuous sacred use; the hybrid reading locates it in causal co-production of substrate and reconstruction.',
    'Comparative sociolinguistic analysis of revival and death cases: whether liturgical-only languages ever regenerate native speech without a deliberate reconstruction project, and whether reconstruction ever succeeds without a preserved high register — the empirical pattern decides which criterion does the explanatory work.',
    'If the constitutive criterion survives, this file''s classification stands; if the co-production account wins, the cost ledger must be rebalanced (part of what this file books as enforcement cost becomes substrate credit) and the family''s classifications shift toward the hybrid reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vitality_definition_location, conceptual, 'The kernel contest is a disagreement over the location of ''vitality'': constitutive criterion vs. sacred continuity vs. causal co-production.').

omega_variable(
    desacralization_cost_attribution,
    'Was the desacralization cost borne by traditional liturgical communities intrinsic to vernacularization as such, or incidental to the specifically secular-national framing the revival took?',
    'Comparative cases of sacred-language vernacularization with retained liturgical deference (Arabic diglossia management, Church Slavonic, Latin after its liturgical monopoly ended): if daily use can coexist with register respect, part of the booked cost was avoidable framing rather than necessity.',
    'If avoidable, the cost attributable to the liturgical-community seat drops and the revival''s secular ideology carries a larger share of the burden; if intrinsic, the cost is a genuine price of the coordination achieved and the tangled_rope reading firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(desacralization_cost_attribution, empirical, 'Whether desacralization was a necessary price of vernacularization or an artifact of the revival''s secular-national ideology.').

omega_variable(
    counterfactual_multilingual_coordination,
    'Could a multilingual settlement (a Yiddish-led federation of language communities, per the excluded Yiddishist program) have delivered comparable coordination at lower cost to the displaced mother tongues?',
    'Comparative evidence from multilingual states with official-language pluralism (Switzerland, Belgium, the post-1918 minority-rights regimes) on integration speed, military cohesion, and intergenerational language maintenance under heavy immigration; plus the observable fact that no comparable mass-immigrant society of the period integrated without assimilation pressure.',
    'If the multilingual path was viable, a large share of the mother-tongue loss was avoidable and the arrangement''s coordination defense weakens toward snare; if not viable, most of the measured extraction books as coordination cost and the reading firms toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_multilingual_coordination, empirical, 'Counterfactual viability of the excluded multilingual alternative.').

omega_variable(
    academy_adoption_rate,
    'What share of the Academy of the Hebrew Language''s coinages and rulings achieve actual public adoption versus being ignored — i.e., how much of its current activity is functional standardization and how much is performative maintenance?',
    'Corpus-linguistic adoption studies tracking Academy-coined terms against actual usage frequencies in broadcast, press, and speech over time.',
    'A low adoption rate pushes theater_ratio upward and drifts the Academy seat toward inertial maintenance; a high rate confirms the standardization function remains live and supports the tangled_rope reading at the current endpoint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(academy_adoption_rate, empirical, 'Functional vs. theatrical share of the Academy''s ongoing standardization activity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__native_daily_reading, 1890, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1890, hebrew_vitality__native_daily_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t1890, observed).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_vitality__native_daily_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement_basis(hebr_tr_t1920, observed).
narrative_ontology:measurement(hebr_tr_t1948, hebrew_vitality__native_daily_reading, theater_ratio, 1948, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t1948, observed).
narrative_ontology:measurement(hebr_tr_t1970, hebrew_vitality__native_daily_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement_basis(hebr_tr_t1970, observed).
narrative_ontology:measurement(hebr_tr_t1995, hebrew_vitality__native_daily_reading, theater_ratio, 1995, 0.36).
narrative_ontology:measurement_basis(hebr_tr_t1995, observed).
narrative_ontology:measurement(hebr_tr_t2025, hebrew_vitality__native_daily_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(hebr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1890, hebrew_vitality__native_daily_reading, base_extractiveness, 1890, 0.28).
narrative_ontology:measurement_basis(hebr_be_t1890, observed).
narrative_ontology:measurement(hebr_be_t1920, hebrew_vitality__native_daily_reading, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement_basis(hebr_be_t1920, observed).
narrative_ontology:measurement(hebr_be_t1948, hebrew_vitality__native_daily_reading, base_extractiveness, 1948, 0.6).
narrative_ontology:measurement_basis(hebr_be_t1948, observed).
narrative_ontology:measurement(hebr_be_t1970, hebrew_vitality__native_daily_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement_basis(hebr_be_t1970, observed).
narrative_ontology:measurement(hebr_be_t1995, hebrew_vitality__native_daily_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement_basis(hebr_be_t1995, observed).
narrative_ontology:measurement(hebr_be_t2025, hebrew_vitality__native_daily_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(hebr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1890, hebrew_vitality__native_daily_reading, suppression_requirement, 1890, 0.12).
narrative_ontology:measurement_basis(hebr_su_t1890, observed).
narrative_ontology:measurement(hebr_su_t1920, hebrew_vitality__native_daily_reading, suppression_requirement, 1920, 0.68).
narrative_ontology:measurement_basis(hebr_su_t1920, observed).
narrative_ontology:measurement(hebr_su_t1948, hebrew_vitality__native_daily_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement_basis(hebr_su_t1948, observed).
narrative_ontology:measurement(hebr_su_t1970, hebrew_vitality__native_daily_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement_basis(hebr_su_t1970, observed).
narrative_ontology:measurement(hebr_su_t1995, hebrew_vitality__native_daily_reading, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement_basis(hebr_su_t1995, observed).
narrative_ontology:measurement(hebr_su_t2025, hebrew_vitality__native_daily_reading, suppression_requirement, 2025, 0.22).
narrative_ontology:measurement_basis(hebr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__liturgical_reading).
narrative_ontology:affects_constraint(hebrew_vitality__native_daily_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The colloquial claim 'Hebrew is a revived living language' decomposes into three structurally distinct constraints — readings of one kernel (hebrew_vitality). This file authors the native_daily_reading: vitality is native intergenerational transmission; the arrangement under contest is the vernacularization regime, with moderate extraction (enforcement against mother tongues, desacralization of the sacred register) and Zionist institutions as the beneficiary seat. hebrew_vitality__liturgical_reading authors vitality as unbroken ritual use — for it the liturgical chain is the living arrangement and the vernacularizers are its violators. hebrew_vitality__hybrid_continuity_reading authors vitality as co-production of substrate and reconstruction, splitting credit and cost. The epsilon values differ across the family because the readings locate vitality differently, not because one observable is measured two ways; each story carries its own epsilon, beneficiaries, and victims, and they are linked here as one constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
