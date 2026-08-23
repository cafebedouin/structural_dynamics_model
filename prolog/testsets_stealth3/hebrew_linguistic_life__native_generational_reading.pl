% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__native_generational_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__native_generational_reading
 *   human_readable: Native-Generational Vitality Criterion (Hebrew Revival Reading)
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   This story instantiates the native_generational_reading of the kernel
 *   hebrew_linguistic_life: a language is alive only when children acquire it
 *   as mother tongue and use it for the full range of daily functions,
 *   secular and mundane included. Under this reading Hebrew was genuinely
 *   dead from roughly 70 to 1880 CE — recited, studied, and written, but
 *   spoken by no children — so its revival was a creation act requiring the
 *   manufacture of native speakers, and the cost of that manufacture was paid
 *   by the Jewish vernacular communities of the day: Yiddish-speaking
 *   Ashkenazim, Ladino-speaking Sephardim, and later Judeo-Arabic-speaking
 *   Mizrahim, whose mother tongues were stigmatized, disciplined out of
 *   schools, and denied institutional space until the intergenerational chain
 *   broke. The epsilon referent is the standing arrangement under contest:
 *   the native-generational criterion as the operative standard governing
 *   Hebrew's status and the treatment of rival Jewish vernaculars, assessed
 *   by this reading's own lights. Claim and metrics are authored
 *   independently: the claimed type is tangled_rope because the criterion
 *   carries both a genuine, still-functioning coordination function and a
 *   documented history of asymmetric, enforced extraction; the metric values
 *   describe the arrangement's actual operation across the interval,
 *   including its enforcement peak and subsequent decay. The sibling readings
 *   are separate constraint files linked through the network section, not
 *   positions argued inside this one.
 *
 * KEY AGENTS:
 *   - hebrew_revivalist_network: agenda-setting core (organized, identity-fused) — authored the criterion's application and enforced it street by street
 *   - zionist_institutional_leadership: agenda setter and beneficiary (institutional, constrained) — adopted the criterion as doctrine and directed its machinery
 *   - israeli_state_institutions: agenda setter and principal beneficiary (institutional, arbitrage) — administers compulsory Hebrew and receives the arrangement's returns
 *   - modern_hebrew_native_speaker_community: beneficiary (moderate, identity-locked) — inherits the finished language at no personal cost
 *   - yiddish_speaking_ashkenazi_immigrants: primary paying seat (moderate, constrained) — surrendered a full literary civilization's daily language
 *   - ladino_speaking_sephardi_communities: paying seat (moderate, trapped) — paid after catastrophe stripped their territorial base
 *   - judeo_arabic_speaking_mizrahi_immigrants: paying seat (powerless, trapped) — paid through state absorption policy with no negotiating lever
 *   - yiddishist_cultural_movement: resisting paying seat (organized, identity-locked) — mounted the open opposition and lost
 *   - liturgical_tradition_authorities: excluded holder of a rival reading (moderate, identity-locked) — answered the question differently and was not seated where it was decided
 *   - palestinian_arabic_speaking_citizens: dual-positioned seat (moderate, constrained) — validated by the criterion, subordinated by the order it legitimates
 *   - comparative_sociolinguists: analytical observer — sees the full structure, analytic yield and enforcement history together
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.55).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.34).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Native-Generational Vitality Criterion (Hebrew Revival Reading)").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, 'fc173af3-863e-4eca-b5b7-6005572b4b49').
narrative_ontology:cs_kernel_codification('fc173af3-863e-4eca-b5b7-6005572b4b49', formalized).
narrative_ontology:cs_authority_grounding('fc173af3-863e-4eca-b5b7-6005572b4b49', expertise).
narrative_ontology:cs_interpretation_layer_present('fc173af3-863e-4eca-b5b7-6005572b4b49').
narrative_ontology:cs_reading_relation('fc173af3-863e-4eca-b5b7-6005572b4b49', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('fc173af3-863e-4eca-b5b7-6005572b4b49', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('fc173af3-863e-4eca-b5b7-6005572b4b49', foundational, native_child_acquisition_necessary_for_language_life).
narrative_ontology:cs_axiom_status(native_child_acquisition_necessary_for_language_life, holdable).
narrative_ontology:cs_axiom_grounding('fc173af3-863e-4eca-b5b7-6005572b4b49', native_child_acquisition_necessary_for_language_life, empirically_contingent).
narrative_ontology:cs_axiom('fc173af3-863e-4eca-b5b7-6005572b4b49', foundational, full_secular_functional_range_required).
narrative_ontology:cs_axiom_status(full_secular_functional_range_required, holdable).
narrative_ontology:cs_axiom_grounding('fc173af3-863e-4eca-b5b7-6005572b4b49', full_secular_functional_range_required, empirically_contingent).
narrative_ontology:cs_reference_frame('fc173af3-863e-4eca-b5b7-6005572b4b49', native_acquisition_vitality_frame).
narrative_ontology:cs_drift_state('fc173af3-863e-4eca-b5b7-6005572b4b49', contemporary_multilingual_repertoire_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('fc173af3-863e-4eca-b5b7-6005572b4b49', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_revivalist_network).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, zionist_institutional_leadership).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, modern_hebrew_native_speaker_community).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speaking_ashkenazi_immigrants).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speaking_sephardi_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, judeo_arabic_speaking_mizrahi_immigrants).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddishist_cultural_movement).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, palestinian_arabic_speaking_citizens).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, palestinian_arabic_speaking_citizens).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, intergenerational_transmission_vitality_thesis).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, hebrew_dormancy_period_hypothesis).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, national_language_requires_native_base).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A network of ideologues, teachers, and journalists centered on figures like Eliezer Ben-Yehuda who committed to making Hebrew the sole spoken language of the Jewish community in Palestine: running Hebrew-only schools, coining vocabulary for every mundane need, policing street speech, and counting every Hebrew sentence a victory and every Yiddish sentence a retreat. Members had staked careers, families, and selfhood on the project — Ben-Yehuda's household vow, his son raised deliberately as the first native speaker — so leaving the project was not a live option for its core.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_revivalist_network, agenda_setter,
    organized, generational, identity_locked, regional).

% Party and Yishuv institutions that adopted the criterion as doctrine and directed its application: Hebrew-only schooling systems, Hebrew labor campaigns, Hebrew press subsidies. They gained a unified administrative language and a legitimating narrative — a people that resurrected its own tongue — and steered the disciplinary machinery that made native transmission close to universal within two generations.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, zionist_institutional_leadership, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, zionist_institutional_leadership, beneficiary).

% After 1948 the Ministry of Education, the army's ulpan system, and the Hebrew Language Academy administered compulsory Hebrew: immigrant adults routed through intensive Hebrew instruction, children schooled exclusively in Hebrew, heritage languages given no curricular space. The state collects the returns — one working language for army, bureaucracy, courts, and market — and retains full authority to reinterpret or relax the standard it enforces.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, israeli_state_institutions, beneficiary).

% Children born into the project from the 1890s onward acquired a complete native language — home, street, school, army, market — at no personal cost, inheriting what their parents' and grandparents' generations were pressed to build and to abandon their own tongues to fund. Their sense of collective self is constituted through the language; imagining themselves as a national community in another tongue is not a available move from where they stand.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, modern_hebrew_native_speaker_community, beneficiary,
    moderate, biographical, identity_locked, national).

% The largest immigrant cohort of the revival decades arrived speaking Yiddish as mother tongue, backed by a vast press, theater, and literary civilization. In Palestine they were told their language was the jargon of exile: children mocked or punished for it at school, its newspapers marginalized, its public use framed as betrayal of the national project. Many complied out of the same Zionist conviction that drove the revival; others bent under school discipline and economic gatekeeping. Their grandchildren grew up Hebrew-monolingual, and the European Yiddish centers that might have anchored an alternative were destroyed in the Holocaust.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speaking_ashkenazi_immigrants, payer,
    moderate, biographical, constrained, continental).

% Descendants of the Sephardi expulsion who spoke Ladino across the former Ottoman world lost Salonika, their great metropolis, in the Holocaust, and those who reached Israel found a state that schooled their children solely in Hebrew and assigned their language no institutional place. Within a generation the language survived mainly in song, household memory, and revival festivals organized around its loss.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speaking_sephardi_communities, payer,
    moderate, biographical, trapped, continental).

% Mass immigration from Arab and Muslim countries in the 1950s brought speakers of Judeo-Arabic and related varieties into transit camps and a Hebrew-only school system. Absorption policy treated their languages as obstacles to integration; parents, dependent on state housing, work, and schooling, had no lever to negotiate curricular space for the mother tongue. The intergenerational chain broke within a single generation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, judeo_arabic_speaking_mizrahi_immigrants, payer,
    powerless, biographical, trapped, continental).

% Writers, Bundists, and scholars in the YIVO orbit who held Yiddish to be the authentic language of the Jewish people and fought the Hebrew monopoly openly: the 1913-14 War of the Languages over the Technion's teaching language, defense of Yiddish press and theater, scholarly work asserting Yiddish's dignity and continuity. They lost ground decade by decade as the constituency whose children would have carried the language shifted, and their opposition was itself an expression of commitment they could not relinquish without dissolving the movement's reason to exist.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddishist_cultural_movement, payer,
    organized, biographical, identity_locked, continental).

% Traditional rabbinic authorities and pious communities for whom Hebrew's life resides in prayer, study, and unbroken textual transmission. They regarded the secular vernacularization of the sacred tongue as profanation and the standard behind it as an alien, nationalist import. They held a coherent answer to the question the revivalists claimed to settle, but were not seated in the secular language-planning bodies where the question was decided.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, liturgical_tradition_authorities, excluded,
    moderate, generational, identity_locked, global).

% Arab citizens whose language meets the criterion fully — children acquire it natively and use it for every daily function — yet who live inside a state whose institutions, built on the criterion's success for Hebrew, rank Arabic beneath it. Their language's aliveness is conceded on the criterion's own terms while its public space contracts; they benefit from the standard's validation of their language and pay in institutional standing within the order that standard legitimates.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, palestinian_arabic_speaking_citizens, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, palestinian_arabic_speaking_citizens, beneficiary).

% Scholars of language endangerment and revival who assess the criterion against the world's languages: intergenerational-transmission research, UNESCO vitality methodologies, documentation of hundreds of dormant and sleeping languages. From outside any party's commitment they can see both what the standard delivers as an analytic instrument and what its application cost the communities that funded the Hebrew case.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, comparative_sociolinguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__native_generational_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__native_generational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a shared, operational standard for judging whether a language is alive — child acquisition as mother tongue plus full functional range including secular mundane speech — letting planners, educators, and revival movements triage endangerment, design transmission interventions, and coordinate on a common metric across unrelated communities.
% TRANSFER_FUNCTION: Moves linguistic legitimacy and institutional investment toward languages meeting the native-transmission standard; concretely, it moved the mother-tongue slot and the daily communicative labor of hundreds of thousands of Jewish immigrants from Yiddish, Ladino, and Judeo-Arabic to Hebrew, along with the prestige and resources of schooling, press, and administration.
% ABSENT_VOICES: Holders of the liturgical-preservation reading (traditional rabbinic authorities) and the marketplace-pidgin reading (pragmatic multilingual communal leaders) sat largely outside the secular Yishuv's definitional conversation. Within it, the people whose daily speech was actually being switched — Yiddish-speaking mothers and children, Ladino-speaking elders — had no seat in the language-planning bodies, which were staffed by Hebrew ideologues.
% DISAPPEARANCE_RATIONALE: If the criterion vanished overnight, Hebrew's official status would lose its operative justification, Israeli language policy would lose the standard that frames compulsory Hebrew as restoration rather than imposition, and the global endangered-language apparatus — UNESCO vitality assessments, revitalization funding, documentation triage — would lose its working metric. Arrangements across language policy visibly depend on it.
% FOUNDING_PROBLEM: Late nineteenth-century nationalists and philologists needed a standard to adjudicate whether Hebrew — sacred, written, recited for eighteen centuries, but spoken by no children — counted as a living language capable of carrying a modern nation, and what evidence would count as linguistic life or death.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: comparative sociolinguistics (intergenerational-transmission research, UNESCO vitality methodology) independently maintains the criterion's relevance; historical linguists attest the dormancy period; and the documentary record of the 1913-14 War of the Languages and the subsequent school and state language regimes attests the enforcement history. None of these sources depends on Hebrew-revival advocacy for standing.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__native_generational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__native_generational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.55: the standing arrangement is consolidated, with residual extraction running through heritage-language attrition among new immigrant cohorts and the institutional subordination of Arabic, down from a 1950 peak of 0.72 when compulsory Hebrew-only schooling processed mass Mizrahi immigration through transit camps. Suppression is 0.34 at interval end because the enforcement machinery — the story's central dynamic — was built up from informal social pressure (0.15 in 1880) through school discipline, the open conflicts of the War of the Languages (0.60 by 1914), municipal signage and press bans (0.68 by 1925), to the full state apparatus of ulpanim and Hebrew-only curricula (0.82 by 1950), and then decayed (0.48 by 1980, 0.34 by 2025) once native transmission became self-sustaining and enforcement was no longer needed to reproduce it. The suppression_requirement series is authored precisely because enforcement-capacity change is the dynamic being traced: a rise-and-decay arc, not a ratchet. Theater_ratio climbs slowly from 0.10 to 0.32 as the Academy's anti-loanword campaigns, anniversary pageantry, and heritage-language memorial festivals grow relative to the shrinking residual function. Accessibility_collapse is 0.58: the rival readings survive fully accessible in academic discourse, while the vernacular alternatives collapsed almost completely inside Israeli society — the standard won its own case demographically without winning it argumentatively. Resistance is 0.62, reflecting the organized Yiddishist counter-mobilization (press, theater, the Technion language war, Bundist and YIVO scholarship) and later Mizrahi and Ladino cultural-revival efforts; the paying seats did coalition-build, which is why resistance sits well above the passive range. All three series run on one shared seven-point grid (1880, 1900, 1914, 1925, 1950, 1980, 2025) so every metric is authored at every examined time point; the dynamics are two monotonic phases (build-up, consolidation-and-decay), not a cycle, so no intermittent-reinforcement reading applies. Identity-lock mechanics differ by seat: the revivalist core fused professionally and ideologically (the household vow made exit self-contradictory), the Yiddishists were locked by ideological commitment that constituted the movement, and the native-speaker majority is locked relationally — Israeli collective selfhood is constituted through the language, so the frame cannot break without re-describing the nation itself.
 *
 * PERSPECTIVAL GAP:
 *   The paying seats and the agenda-setting seats should compute different types from the same structure. From the revivalist and state seats the criterion is the rescue of a murdered language and the proudest achievement of the national project — the arrangement presents as necessary creation. From the Yiddish, Ladino, and Judeo-Arabic seats the same criterion operated as a death warrant for functioning mother tongues, delivered in the language of scientific neutrality. The excluded liturgical seat computes a third thing: a category error that mistook recitation-free decades for death. The engine derives these divergences from the declared roles, power atoms, and exit options; nothing in the authored claim adjudicates between them. The suppression picture is mixed-mechanism: structural enforcement dominated the build-up phase, while the internalized share — parental conviction, shame framing — grew in relative weight as formal enforcement decayed, which is why vernacular loss continued completing after the machinery relaxed.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to directionality near the subsidized end: the revivalist network authored and enforced the standard that legitimates its life's work; the state institutions collect the returns and hold authority to reinterpret the standard (arbitrage-grade exit from any particular enforcement posture); the native-speaker community inherits a complete language at zero acquisition cost. The victim declarations map to directionality near the full-target end: Yiddish, Ladino, and Judeo-Arabic speakers paid their mother tongues, with constrained or trapped exits amplifying effective extraction — the Mizrahi cohort, powerless and state-dependent, sits nearest the full-target pole; the Yiddishists, though organized, are identity-locked, which keeps their effective extraction high despite collective power. The excluded liturgical authorities sit above symmetric: they bear no material levy but lost the standing of their answer. The Palestinian Arab seat is genuinely dual-positioned — the criterion validates their language's aliveness while the order built on it subordinates the language — so its derived directionality lands mid-range rather than at either pole. Scope amplification applies hardest to the continental-scale victim seats, whose linguistic worlds spanned regions the enforcement surface never had to reach directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical misreadings. Reading the criterion as pure neutral science would erase the coerced abandonment that funded the revival — the Yiddish press suppressed, the children punished, the transit-camp generation severed from its mother tongue. Reading it as pure predation would erase the genuine analytic standard that the world's endangered-language apparatus still runs on, and that correctly predicted which languages would die without intervention. The tangled_rope claim holds both halves: real coordination function, real asymmetric cost, active enforcement required throughout the formative period and decaying only after consolidation. On genealogy: the founding problem — adjudicating whether Hebrew counted as alive — is settled for its original instance, but the standard's general mandate is live in revitalization practice worldwide, so founding_problem_status is live paired with disappearance_verdict world_rearranges: no zombie condition, no mismatch flag; the arrangement still does what it was built to do, for better and for worse. The receipt surface records where the gains went: the returns accrued to the state-institutional seat, and fixing is prohibitive — the paying seats' grandchildren are Hebrew-native, so undoing the shift would mean re-engineering the mother tongues of an entire society, a cost no seat could bear relative to any benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dormancy_period_extent,
    'How complete was Hebrew''s dormancy between 70 and 1880 CE — were there continuous pockets of spoken Hebrew use (scholarly contact, trade, certain communities) that this reading''s clean death-and-resurrection narrative papers over?',
    'Archival and dialectological research on pre-revival spoken Hebrew registers: correspondence, glossaries, travel accounts, and rabbinic responsa documenting ad hoc oral use.',
    'If continuous vernacular pockets existed, the revival was less a creation ex nihilo than an expansion of surviving practice, weakening the necessity claim that justified the severity of the enforcement campaign and shifting part of the measured burden off the criterion onto ordinary language spread.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_period_extent, empirical, 'Whether the 70-1880 dormancy was total or left spoken-use pockets.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the enforcement that produced the language shift was structural (school punishment, municipal bans, state policy) versus internalized (parents'' own ideological conviction that Yiddish was exile''s stain, applied inside the home without any external compulsion)?',
    'Compare shift trajectories across communities with identical structural exposure but different ideological alignment — secular Zionist households versus traditionally oriented Yiddishist or rabbinic households — using school records, memoirs, and oral histories.',
    'If the internalized share is large, the arrangement''s suppressive force outlived its enforcement machinery — consistent with the falling suppression series alongside completed shift — and the victim communities'' loss cannot be attributed to coercion alone; the classification''s suppression profile would rest partly on conviction the targets supplied themselves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the language-shift enforcement.').

omega_variable(
    counterfactual_diversity_attribution,
    'Would Yiddish, Ladino, and Judeo-Arabic have survived as daily languages in Palestine absent the criterion-driven campaign — or did immigration pressure, the destruction of the European and Salonika centers, and the economics of integration doom them regardless?',
    'Comparative analysis of immigrant-language survival under weaker official regimes (United States Yiddish decline, Argentine Ladino decline) matched for cohort size and catastrophe exposure, isolating the enforcement differential.',
    'Attribution determines the victim ledger: if exogenous catastrophe dominates, the criterion''s application accelerated losses that were coming anyway and epsilon drops; if enforcement was decisive, the arrangement bears the deaths of functioning literary civilizations and epsilon rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_diversity_attribution, empirical, 'How much vernacular death is attributable to the campaign versus exogenous catastrophe.').

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the kernel hebrew_linguistic_life — the native_generational_reading. Its siblings, liturgical_preservation_reading (life equals continuous recited transmission; Hebrew never died) and marketplace_pidgin_reading (life equals inter-communal medium function; native status irrelevant), instantiate different constraints with different victim sets and different epsilon. Is the single-criterion framing of the kernel itself the only defensible one?',
    'Framing analysis of how the discipline and practitioners actually operationalize linguistic life: if revitalization practice converges on multidimensional vitality indices rather than a single necessary-and-sufficient criterion, the exclusive framing is a choice, not a discovery.',
    'The disagreement is located in the exclusivity operator: this reading''s ''only when'' asserts native acquisition is necessary, directly negating both siblings'' sufficiency claims and generating the forecloses edges. Under a pluralist meta-framework the contradiction dissolves into orthogonal dimensions, the forecloses relations soften to coexistence, and the arrangement reads as one chosen standard among several — lowering measured suppression and recasting the victim set as casualties of a selection rather than of a logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: one reading of a three-reading kernel; framing choice determines foreclosure structure.').

omega_variable(
    arabic_dual_position_trajectory,
    'Will the criterion''s full validation of Arabic''s aliveness — native transmission, complete functional range — eventually translate into restored institutional space for Arabic inside the state the criterion helped consolidate, or does the subordination harden?',
    'Track curriculum policy, official-status legislation, and public-media provision for Arabic across coming decades against the 2018 nation-state-law baseline.',
    'Restored space would show the criterion operating as a neutral standard capable of validating rivals (supporting the coordination half of the claim); hardened subordination would show the standard functioning as a ranking instrument for the beneficiary language (raising the standing arrangement''s effective extraction on this seat).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arabic_dual_position_trajectory, empirical, 'Future trajectory of the criterion''s application to Arabic inside Israel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 1880, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1880, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t1880, observed).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t1900, observed).
narrative_ontology:measurement(hebr_tr_t1914, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1914, 0.18).
narrative_ontology:measurement_basis(hebr_tr_t1914, observed).
narrative_ontology:measurement(hebr_tr_t1925, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1925, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t1925, observed).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement_basis(hebr_tr_t1950, observed).
narrative_ontology:measurement(hebr_tr_t1980, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1980, 0.26).
narrative_ontology:measurement_basis(hebr_tr_t1980, observed).
narrative_ontology:measurement(hebr_tr_t2025, hebrew_linguistic_life__native_generational_reading, theater_ratio, 2025, 0.32).
narrative_ontology:measurement_basis(hebr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1880, 0.28).
narrative_ontology:measurement_basis(hebr_be_t1880, observed).
narrative_ontology:measurement(hebr_be_t1900, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement_basis(hebr_be_t1900, observed).
narrative_ontology:measurement(hebr_be_t1914, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1914, 0.54).
narrative_ontology:measurement_basis(hebr_be_t1914, observed).
narrative_ontology:measurement(hebr_be_t1925, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1925, 0.63).
narrative_ontology:measurement_basis(hebr_be_t1925, observed).
narrative_ontology:measurement(hebr_be_t1950, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1950, 0.72).
narrative_ontology:measurement_basis(hebr_be_t1950, observed).
narrative_ontology:measurement(hebr_be_t1980, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1980, 0.61).
narrative_ontology:measurement_basis(hebr_be_t1980, observed).
narrative_ontology:measurement(hebr_be_t2025, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 2025, 0.55).
narrative_ontology:measurement_basis(hebr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1880, 0.15).
narrative_ontology:measurement_basis(hebr_su_t1880, observed).
narrative_ontology:measurement(hebr_su_t1900, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement_basis(hebr_su_t1900, observed).
narrative_ontology:measurement(hebr_su_t1914, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1914, 0.6).
narrative_ontology:measurement_basis(hebr_su_t1914, observed).
narrative_ontology:measurement(hebr_su_t1925, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1925, 0.68).
narrative_ontology:measurement_basis(hebr_su_t1925, observed).
narrative_ontology:measurement(hebr_su_t1950, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1950, 0.82).
narrative_ontology:measurement_basis(hebr_su_t1950, observed).
narrative_ontology:measurement(hebr_su_t1980, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement_basis(hebr_su_t1980, observed).
narrative_ontology:measurement(hebr_su_t2025, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 2025, 0.34).
narrative_ontology:measurement_basis(hebr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'was Hebrew ever not alive?' decomposes into three structurally distinct claims with different epsilon values, per the epsilon-invariance principle. The liturgical_preservation_reading is the upstream member — the oldest claim, grounded in unbroken transmission practice, implying negligible extraction and no dormancy. This native_generational_reading is the contested middle member: it negates the upstream claim (asserting real dormancy and required revival) and generates the enforcement history and victim set. The marketplace_pidgin_reading is the pragmatic downstream member, indifferent to both native status and sacred function. Each story links the other two through affects_constraints; the upstream reading's historical confidence is routinely cited as evidence in disputes over this one, which is why the edge runs in both directions across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
