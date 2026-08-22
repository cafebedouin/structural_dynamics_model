% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Native-Generational Vitality Criterion and the Hebrew Revival Enforcement Regime
 *   domain: sociolinguistics/religious studies/nationalism studies
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the contested kernel 'what makes
 *   Hebrew a living language': the native-generational reading, on which a
 *   language is alive only when children acquire it as mother tongue and
 *   deploy it across the full functional range including mundane secular
 *   speech. On this reading Hebrew was dead from 70 CE to circa 1880 CE —
 *   liturgy and learned correspondence kept a code in circulation, but no
 *   child spoke it at home — and the revival was therefore a construction
 *   project, not a reawakening. The standing arrangement under contest, and
 *   the epsilon referent assessed by this reading's own lights, is the
 *   revival-and-monolingualization regime: the deliberate manufacture of
 *   native speakers through schools, youth movements, workplaces, ulpanim,
 *   and the army, together with the displacement of Yiddish, Ladino, and
 *   Judeo-Arabic from public and then domestic use. Because the reading holds
 *   the criterion as descriptively sound, it cannot deny the victim set the
 *   criterion's enforcement produced: whole communities surrendered their
 *   mother tongues. Claim and metrics are independent authored facts:
 *   claimed_type records the structure believed true (a genuine coordination
 *   achievement fused with asymmetric extraction, actively enforced); the
 *   metrics record the operation believed descriptively accurate. The sibling
 *   readings are separate files with separate epsilon values: under the
 *   liturgical-preservation reading nothing was done to anyone and there are
 *   no revival victims; under the marketplace-pidgin reading the medieval
 *   commercial networks are the subject.
 *
 * KEY AGENTS:
 *   - - hebrew_language_committee: agenda-setting seat (institutional/identity_locked) — coins terminology, fixes standards, adjudicates usage; its members' life work is the criterion itself
 *   - - hebrew_teaching_corps: enforcement arm (organized/identity_locked) — staffs the schools and ulpanim where the native generation is manufactured
 *   - - native_hebrew_speaking_generation: primary beneficiary (organized/constrained) — the first cohort in eighteen centuries to acquire Hebrew as mother tongue
 *   - - zionist_nation_building_institutions: structural beneficiary (institutional/arbitrage) — receives a unified public sphere, a mobilizable citizenry, and a national culture it directs
 *   - - hebrew_press_and_publishing_sector: secondary beneficiary (organized/constrained) — gains a captive readership, bore the cost of building Hebrew letters from scratch
 *   - - yiddish_speaking_immigrants: primary target (organized/trapped) — surrenders the public and then domestic functions of the largest Jewish vernacular
 *   - - ladino_sephardi_communities: target (moderate/constrained) — smaller institutional base, fewer resources for retention
 *   - - mizrahi_arabic_speaking_jews: primary target (powerless/trapped) — absorbs the deepest erasure through the state absorption apparatus
 *   - - hilfsverein_german_school_network: excluded seat (powerful/arbitrage) — loses the Language Wars and exits the arrangement's history
 *   - - diaspora_yiddishist_movement: excluded seat (organized/trapped) — argues the rival national-language case from outside, never seated
 *   - - comparative_linguistics_community: analytical observer (analytical/analytical) — assesses the criterion and the revival's outcome from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.6).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.7).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Native-Generational Vitality Criterion and the Hebrew Revival Enforcement Regime").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious studies/nationalism studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, '5c8d7f30-15cf-4631-ab35-b00e34cdf880').
narrative_ontology:cs_kernel_codification('5c8d7f30-15cf-4631-ab35-b00e34cdf880', distributed).
narrative_ontology:cs_authority_grounding('5c8d7f30-15cf-4631-ab35-b00e34cdf880', expertise).
narrative_ontology:cs_interpretation_layer_present('5c8d7f30-15cf-4631-ab35-b00e34cdf880').
narrative_ontology:cs_reading_relation('5c8d7f30-15cf-4631-ab35-b00e34cdf880', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c8d7f30-15cf-4631-ab35-b00e34cdf880', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('5c8d7f30-15cf-4631-ab35-b00e34cdf880', foundational, native_intergenerational_transmission_necessary_for_life).
narrative_ontology:cs_axiom_status(native_intergenerational_transmission_necessary_for_life, holdable).
narrative_ontology:cs_axiom_grounding('5c8d7f30-15cf-4631-ab35-b00e34cdf880', native_intergenerational_transmission_necessary_for_life, empirically_contingent).
narrative_ontology:cs_axiom('5c8d7f30-15cf-4631-ab35-b00e34cdf880', secondary, sacred_text_continuity_insufficient_for_life).
narrative_ontology:cs_axiom_status(sacred_text_continuity_insufficient_for_life, holdable).
narrative_ontology:cs_axiom_grounding('5c8d7f30-15cf-4631-ab35-b00e34cdf880', sacred_text_continuity_insufficient_for_life, empirically_contingent).
narrative_ontology:cs_reference_frame('5c8d7f30-15cf-4631-ab35-b00e34cdf880', unbroken_native_transmission_full_range).
narrative_ontology:cs_drift_state('5c8d7f30-15cf-4631-ab35-b00e34cdf880', contemporary_global_english_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('5c8d7f30-15cf-4631-ab35-b00e34cdf880', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, native_hebrew_speaking_generation).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, zionist_nation_building_institutions).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_press_and_publishing_sector).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speaking_immigrants).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_sephardi_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, mizrahi_arabic_speaking_jews).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_teaching_corps).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, native_intergenerational_transmission_criterion).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, planned_language_revival_possibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Founded 1889-1890 as the Hebrew Language Committee, later the Academy of the Hebrew Language. Coins terminology for modern objects and concepts, fixes pronunciation and spelling standards, adjudicates disputes over usage. Its members are ideologically committed revivalists for whom the language's fate is their life's work; several raised Hebrew-only households as deliberate demonstrations. Exit for a member means abandoning the vocation that constitutes their biography; virtually none took it.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_language_committee, agenda_setter,
    institutional, generational, identity_locked, regional).

% Graduates of the Hebrew teachers' seminary who staff kindergartens, schools, and later ulpanim. They administer the day-to-day conversion of immigrant children into Hebrew speakers, including the classroom norms that penalized home-language use. Employment, status, and marriage markets flowed through the Hebrew educational system, so their livelihood depended on the arrangement they enforced; leaving meant exiting the profession and the community that defined them.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_teaching_corps, agenda_setter,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, hebrew_teaching_corps, beneficiary).

% Children born in the Yishuv and later Israel from the 1890s onward — the first cohort in eighteen centuries to grow up with Hebrew as mother tongue. They receive the language effortlessly and inherit the unified public sphere it enables. Their exit options are real but costly: emigrating means losing the native-speaker status and the social world the language anchors; most stayed, and their numbers made the arrangement self-sustaining.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, native_hebrew_speaking_generation, beneficiary,
    organized, generational, constrained, national).

% The Zionist Executive, the national institutions, and after 1948 the state ministries and the army. They funded the school system, standardized curricula, ran ulpanim, and used military service as a Hebrew-immersion machine for new immigrants. They operate comfortably in multiple languages internationally and could have coordinated the polity through another vehicle; Hebrew's success delivered them a unified internal culture, a mobilizable citizenry, and a national narrative linking the state to antiquity.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, zionist_nation_building_institutions, beneficiary,
    institutional, generational, arbitrage, global).

% Newspapers, publishers, and writers who built Hebrew letters from near-nothing. They gain a captive readership once rivals in other Jewish languages lose standing, but they bore the front-loaded cost of inventing a modern press idiom in a language nobody had yet spoken natively. Their market position is bound to the arrangement; shifting to another language would forfeit their accumulated capital.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_press_and_publishing_sector, beneficiary,
    organized, biographical, constrained, regional).

% The largest immigrant pool of the revival era, arriving mainly 1882-1939 with a dense institutional life in Yiddish: press, theater, parties, unions. Public use of Yiddish came under escalating sanction — excluded from schools, mocked in youth movements as a diaspora mark, pushed out of workplaces — and parents shifted to Hebrew at home to secure their children's futures. After 1939 return to the Yiddish heartland ceased to exist as an option; staying meant economic and civic integration on Hebrew terms. Private Yiddish persisted in kitchens for a generation and then thinned.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speaking_immigrants, payer,
    organized, biographical, trapped, regional).

% Sephardi families from the Balkans, Turkey, and the Levant carrying five centuries of Ladino. Smaller in number and thinner in institutional infrastructure than the Ashkenazi majority, they lacked the press networks and political weight to negotiate retention; their communal leadership largely accommodated Hebrew schooling. Home transmission continued longest among older generations and in household registers, then declined across the mid-century cohorts.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_sephardi_communities, payer,
    moderate, biographical, constrained, regional).

% Jews from Morocco, Iraq, Yemen, and elsewhere, brought in mass waves 1948-1956 straight into transit camps and development towns. The absorption apparatus — camp schools, the army, settlement placement — functioned as a total Hebrew-immersion environment; children were punished or shamed for speaking Arabic at school, and parents, dependent on the state for housing and work, had no realistic alternative. Their languages carried no prestige in the receiving culture and no international refuge stood behind them.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, mizrahi_arabic_speaking_jews, payer,
    powerless, biographical, trapped, national).

% The German-Jewish philanthropic system that ran the Technion and a network of German-language schools. It fought the 1913-14 Language Wars to keep German as the language of science and higher instruction, lost, and its position collapsed with the First World War and the later destruction of German Jewry. It had always operated internationally and could relocate its investment; within Palestine its institutional future ended.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hilfsverein_german_school_network, excluded,
    powerful, biographical, arbitrage, continental).

% Bundists, YIVO scholars, and Yiddishist writers in Eastern Europe and America who held that Yiddish, not Hebrew, was the authentic Jewish national language. They published rebuttals and built scholarly institutions but were never seated in the Yishuv's decisions; their core constituency was physically destroyed in the Holocaust and the American branch assimilated within two generations. Inside the arrangement they appear only as the defeated opposition in the Language War record.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, diaspora_yiddishist_movement, excluded,
    organized, generational, trapped, continental).

% Sociolinguists and historians of language who assess the revival as the paradigm case of planned language revival. They take testimony from every seat, compare the case against other revivals (Irish, Welsh, Maori), and hold the vitality criterion itself up for scrutiny; they bear none of its costs and collect none of its rents.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, comparative_linguistics_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__native_generational_reading, zionist_nation_building_institutions).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__native_generational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of a polyglot immigrant society: hundreds of thousands of arrivals speaking mutually unintelligible languages (Yiddish, Ladino, Judeo-Arabic, Persian, Russian, German, Polish) needed one shared vernacular for schooling, commerce, administration, military command, and civic life. Hebrew was the only candidate with legitimacy across every community precisely because it was nobody's mother tongue — neutral ground that privileged no diaspora faction and connected the new polity to the sacred corpus all factions honored.
% TRANSFER_FUNCTION: Moves communicative and cultural capital from the multilingual immigrant population to the Hebrew-speaking sphere: each community surrenders its language's public functions and eventually its domestic functions; institutional access, employment, and status flow to Hebrew competence; the accumulated expressive wealth of the diaspora languages — presses, theaters, scholarly traditions — is forfeited rather than transferred, and the unified sphere's gains accrue to the national institutions that directed the process.
% ABSENT_VOICES: The diaspora Yiddishist movement argued Yiddish was the Jewish national language and was never seated in the Yishuv settlement; Sephardi and Mizrahi communities were absorbed on terms set by the Ashkenazi-Hebrew core without negotiating retention protections; Arab citizens of the country, whose own language was demoted by Hebrew's elevation, had no voice in the arrangement at all. Unanimity about the revival's necessity arose inside a room from which these parties were absent.
% DISAPPEARANCE_RATIONALE: If the criterion-and-enforcement complex vanished overnight, the Israeli sociolinguistic order has no substitute waiting: schooling, law, the army, the media, and everyday street life run on the arrangement's product. The immigrant communities' languages are no longer recoverable at scale — the speech communities that sustained them were dispersed or destroyed — so nothing reverts; the polity would face a communication vacuum in a still-immigrating society.
% FOUNDING_PROBLEM: Between 70 and 1880 CE no child acquired Hebrew as a mother tongue; the language survived as liturgy and learned writing. A modernizing national movement requiring a living vernacular for state-building confronted a language that could not serve one, and the native-generational criterion defined the gap: create native speakers or accept that the national language is dead.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Ottoman and British administrative records attest the absence of Hebrew vernacular use before 1880; the memoir and diary record of the revival generation attests both the initial artificiality (children recruited as the first native speakers) and its completion; comparative sociolinguistics treats the case as the successful terminal point of a planned revival. The Academy of the Hebrew Language now asserts a successor problem (vulnerability to English), but no party outside the arrangement's institutions attests that the original founding problem remains open.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.60: whole speech communities lost their mother tongues within one to two generations — an irreversible forfeiture of expressive capital — bounded below the maximum because uptake was substantially ideological and consensual in the Ashkenazi core and the coordination good was real and widely shared. Suppression 0.70: organized enforcement is documented at every stage — the 1913-14 Language Wars, exclusion of German and Yiddish from instruction, social sanction through youth movements and workplace norms, and after 1948 the ulpan-army-school apparatus including punishment of children for home-language speech in transit camps — bounded because private-sphere retention persisted for decades and no statute ever banned home speech. Theater 0.20: the work was overwhelmingly functional (terminology, teaching, publishing); theatricality concentrated early (one family performing Hebrew in a Yiddish-speaking city) and late (academy coinages increasingly ignored by native speakers). Accessibility collapse 0.45: public multilingualism was progressively closed but never fully collapsed — private retention endured and emigration remained formally open. Resistance 0.55: real and organized (the Language Wars themselves, the Yiddishist press, religious defense of Yiddish, later Mizrahi grievance politics) but defeated and demographically outflanked. All three series run on one shared eight-point grid; the trajectory is a rise-peak-decay enforcement arc, not an oscillation, and the suppression_requirement series is authored because enforcement-capacity change is precisely the dynamic this story traces.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seats compute differently from the same facts: from the committee and the teaching corps the arrangement is a redemptive construction they gave their lives to; from the trapped immigrant seats it is the machine that took their languages. A further divergence is definitional rather than positional: a holder of the liturgical-preservation reading experiences the same eighteen centuries as unbroken continuity with zero extraction, because the criterion differs, not the facts. The engine computes the positional divergence from the structural data; the definitional divergence belongs to the sibling files.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low directionality: the native generation receives the language as an endowment, the Zionist institutions collect the unified sphere (their arbitrage-grade exit pushing them nearest the beneficiary pole), and the press sector collects a consolidated readership. Victims derive high directionality: trapped Yiddish-speaking and Mizrahi seats sit near the full-target end, Ladino communities somewhat less trapped. The committee derives low directionality despite administering enforcement — it collects meaning and status and pays little material cost. Scope interacts with power: verification of compliance was concentrated in schools, camps, and workplaces, so effective pressure landed hardest on the powerless trapped seat (Mizrahi immigrants) even though the Ashkenazi majority supplied the largest aggregate surrender.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Against pure-extraction mislabeling: the arrangement solved a real collective-action problem no rival structure solved — a neutral pan-community vernacular for a polyglot society — and much participation was consensual; a pure-extraction verdict would erase the coordination achievement and the consent. Against coordination-laundering: naming the victim set keeps the mother-tongue forfeiture on the books; a pure-coordination verdict would erase the coerced. The R5 interview sharpens this: the founding problem (no native speakers) is dead — solved by the 1950s — while the world rearranged around the result, so the mismatch signal fires; the honest resolution is completion rather than capture. The enforcement machinery decayed once its object existed, leaving a small prescriptive residue (academy coinages, usage rulings, increasingly ignored) that is the nearest thing to an inertial fragment inside the arrangement. If the Academy's asserted successor problem (English encroachment) were corroborated from outside the benefiting institutions, the arrangement's mandate would read as live again — that assertion is exactly what the corroboration field tests and currently fails.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition,
    'This constraint is one reading of kernel hebrew_linguistic_life (reading: native_generational_reading); what would adopting a sibling reading change structurally?',
    'Author the sibling files (liturgical_preservation_reading, marketplace_pidgin_reading) and compare epsilon, victim sets, and computed types across the triplet; the disagreement is located entirely in the vitality criterion itself, not in the historical facts.',
    'Under the liturgical-preservation reading the victim set empties (no revival was required, nothing was coerced) and epsilon collapses toward zero; under the marketplace-pidgin reading the subject shifts to the medieval networks and the revival becomes an optional upgrade. This file''s classification is valid only within the native-generational criterion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_partition, conceptual, 'Kernel indexicality: one of three readings; sibling readings change the victim set and epsilon wholesale.').

omega_variable(
    coercion_vs_conviction_share,
    'What share of mother-tongue abandonment was coerced by enforcement versus voluntarily adopted out of ideological conviction?',
    'Oral-history corpora, immigrant diaries, and differential uptake across communities exposed to different enforcement intensities (Ashkenazi urban core versus peripheral Mizrahi transit camps).',
    'A high voluntary share pulls effective extraction down toward coordination-cost territory; a high coerced share pushes the arrangement toward the pure-extraction boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_conviction_share, empirical, 'Partition of abandonment into coerced and conviction-driven components.').

omega_variable(
    holocaust_attribution_confound,
    'How much of the Yiddish and Ladino language death is attributable to the revival''s enforcement versus the physical destruction of the European speech communities in 1939-1945?',
    'Compare retention trajectories of communities outside Nazi reach (Palestinian Yiddish before 1939, American Yiddish, Ladino in Turkey) against those inside it.',
    'Re-attributes a potentially large fraction of the measured extraction; the revival''s independent causal contribution to the victim set may be substantially smaller than the raw outcome suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(holocaust_attribution_confound, empirical, 'Attribution confound between enforcement and genocide in the victim-set causation.').

omega_variable(
    multilingual_alternative_stability,
    'Was a tolerant multilingual settlement (Hebrew as public lingua franca alongside protected mother tongues) structurally available, or did mass migration and state formation make monolingualization unavoidable?',
    'Comparative analysis of managed-multilingual polities (Switzerland, Finland, Singapore) under comparable immigration and state-formation pressure.',
    'If a stable alternative existed, the suppression component is excess extraction riding on the coordination function; if not, part of the measured suppression is the price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multilingual_alternative_stability, conceptual, 'Counterfactual availability of a multilingual settlement with equivalent coordination output.').

omega_variable(
    erasure_asymmetry_by_community,
    'Was the cost of abandonment distributed asymmetrically across victim communities — Mizrahim and Ladino speakers bearing deeper erasure than Ashkenazim with global Yiddish networks behind them?',
    'Longitudinal home-language retention surveys by community of origin, second and third generation.',
    'Strong asymmetry concentrates effective extraction on the powerless trapped seat and differentiates directionality among the victim seats; symmetry supports treating the victim set as a single class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(erasure_asymmetry_by_community, empirical, 'Distributional asymmetry of language erasure across victim communities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 1880, 1965).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hll_native_gen_tr_t1880, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1880, 0.26).
narrative_ontology:measurement_basis(hll_native_gen_tr_t1880, observed).
narrative_ontology:measurement(hll_native_gen_tr_t1895, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1895, 0.29).
narrative_ontology:measurement_basis(hll_native_gen_tr_t1895, observed).
narrative_ontology:measurement(hll_native_gen_tr_t1910, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1910, 0.27).
narrative_ontology:measurement_basis(hll_native_gen_tr_t1910, observed).
narrative_ontology:measurement(hll_native_gen_tr_t1922, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1922, 0.23).
narrative_ontology:measurement_basis(hll_native_gen_tr_t1922, observed).
narrative_ontology:measurement(hll_native_gen_tr_t1936, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1936, 0.18).
narrative_ontology:measurement_basis(hll_native_gen_tr_t1936, observed).
narrative_ontology:measurement(hll_native_gen_tr_t1948, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1948, 0.14).
narrative_ontology:measurement_basis(hll_native_gen_tr_t1948, observed).
narrative_ontology:measurement(hll_native_gen_tr_t1956, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1956, 0.17).
narrative_ontology:measurement_basis(hll_native_gen_tr_t1956, observed).
narrative_ontology:measurement(hll_native_gen_tr_t1965, hebrew_linguistic_life__native_generational_reading, theater_ratio, 1965, 0.24).
narrative_ontology:measurement_basis(hll_native_gen_tr_t1965, observed).

% Extraction over time
narrative_ontology:measurement(hll_native_gen_be_t1880, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1880, 0.14).
narrative_ontology:measurement_basis(hll_native_gen_be_t1880, observed).
narrative_ontology:measurement(hll_native_gen_be_t1895, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1895, 0.2).
narrative_ontology:measurement_basis(hll_native_gen_be_t1895, observed).
narrative_ontology:measurement(hll_native_gen_be_t1910, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1910, 0.32).
narrative_ontology:measurement_basis(hll_native_gen_be_t1910, observed).
narrative_ontology:measurement(hll_native_gen_be_t1922, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1922, 0.46).
narrative_ontology:measurement_basis(hll_native_gen_be_t1922, observed).
narrative_ontology:measurement(hll_native_gen_be_t1936, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1936, 0.56).
narrative_ontology:measurement_basis(hll_native_gen_be_t1936, observed).
narrative_ontology:measurement(hll_native_gen_be_t1948, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1948, 0.64).
narrative_ontology:measurement_basis(hll_native_gen_be_t1948, observed).
narrative_ontology:measurement(hll_native_gen_be_t1956, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1956, 0.63).
narrative_ontology:measurement_basis(hll_native_gen_be_t1956, observed).
narrative_ontology:measurement(hll_native_gen_be_t1965, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 1965, 0.6).
narrative_ontology:measurement_basis(hll_native_gen_be_t1965, observed).

% Suppression requirement over time
narrative_ontology:measurement(hll_native_gen_su_t1880, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1880, 0.04).
narrative_ontology:measurement_basis(hll_native_gen_su_t1880, observed).
narrative_ontology:measurement(hll_native_gen_su_t1895, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1895, 0.1).
narrative_ontology:measurement_basis(hll_native_gen_su_t1895, observed).
narrative_ontology:measurement(hll_native_gen_su_t1910, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1910, 0.28).
narrative_ontology:measurement_basis(hll_native_gen_su_t1910, observed).
narrative_ontology:measurement(hll_native_gen_su_t1922, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1922, 0.44).
narrative_ontology:measurement_basis(hll_native_gen_su_t1922, observed).
narrative_ontology:measurement(hll_native_gen_su_t1936, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1936, 0.58).
narrative_ontology:measurement_basis(hll_native_gen_su_t1936, observed).
narrative_ontology:measurement(hll_native_gen_su_t1948, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1948, 0.72).
narrative_ontology:measurement_basis(hll_native_gen_su_t1948, observed).
narrative_ontology:measurement(hll_native_gen_su_t1956, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1956, 0.68).
narrative_ontology:measurement_basis(hll_native_gen_su_t1956, observed).
narrative_ontology:measurement(hll_native_gen_su_t1965, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 1965, 0.52).
narrative_ontology:measurement_basis(hll_native_gen_su_t1965, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'Hebrew is a 3000-year-old living language' into three structurally distinct claims per the epsilon-invariance principle: liturgical continuity (the language never died), inter-communal written and commercial medium (pidgin life), and native generational vernacular transmission (dead 70-1880, revived by construction). Each claim carries its own epsilon, victim set, and classification; this file instantiates the third. The upstream sibling (liturgical preservation) is typically cited as evidence against this reading's death verdict, and this reading's victory in the Language Wars created the conditions under which the liturgical reading's claim became redundant rather than load-bearing. Edges are recorded as coexists_with pending the sibling files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
