% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__native_generational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Native-Generational Criterion of Hebrew Linguistic Life
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   A language is alive, on this reading, only when children acquire it as
 *   mother tongue and wield it for every daily function down to the most
 *   mundane secular speech. Applied to Hebrew, the criterion delivered a
 *   triple verdict: Hebrew died as a vernacular around 70 CE and stayed dead
 *   until 1880; its liturgical survival was the preservation of a corpse, not
 *   a life; and revival therefore required manufacturing native speakers,
 *   which meant getting immigrant parents to abandon Yiddish, Ladino, and the
 *   other Jewish vernaculars in the home, by persuasion where possible and
 *   institutional coercion where not. The criterion was the revival's engine
 *   and its warrant at once: it defined success, mobilized the home-language
 *   switch that classroom instruction alone could never accomplish, and
 *   delegitimated every rival arrangement, multilingual schooling, Yiddish
 *   cultural autonomy, Ladino communal continuity, as a betrayal of the
 *   national task. This story authors the criterion-as-operated: one reading
 *   of the kernel hebrew_linguistic_life, with the sibling readings carried
 *   as separate linked constraints. Claim and metrics are independent: the
 *   claimed type is tangled_rope, a genuine coordination function joined to
 *   asymmetric extraction under active enforcement, while the metrics
 *   describe substantially extractive operation resting on a real, achieved
 *   coordination core. KEY AGENTS (by structural relationship): see
 *   key_agents; the primary targets are the Yiddish-speaking immigrant masses
 *   (powerless/trapped) and the rooted Ladino communities
 *   (moderate/constrained); the primary collectors are the revivalist
 *   leadership (institutional/identity_locked) and the education network
 *   (institutional/constrained); the native first generation is genuinely
 *   dual-positioned; the Yiddishist activists and diaspora communities are
 *   the organized voices excluded from the arena; linguistic scholars
 *   observe.
 *
 * KEY AGENTS:
 *   - - hebrew_revivalist_leadership: Agenda-setter (institutional/identity_locked) — articulates the criterion, adjudicates it, collects its institutional yield
 *   - - hebrew_education_network: Enforcement arm and beneficiary (institutional/constrained) — runs the schools that make the criterion binding
 *   - - eastern_european_yiddish_masses: Primary target (powerless/trapped) — bears the abandonment of the mother tongue
 *   - - sephardic_ladino_communities: Secondary target (moderate/constrained) — rooted communities whose vernacular was reclassified as debris
 *   - - native_hebrew_first_generation: Dual-positioned beneficiary and payer (moderate/identity_locked) — receives the mother tongue, pays the inherited language
 *   - - hebrew_writers_and_teachers: Beneficiary (organized/identity_locked) — careers constituted by the language's centrality
 *   - - yiddishist_cultural_activists: Excluded voice (organized/constrained) — mounted the organized counter-case from outside the arena
 *   - - diaspora_jewish_communities: Excluded voice (organized/mobile) — absorbed the verdict without a seat
 *   - - linguistic_scholars: Analytical observer (analytical/analytical) — supplies the evidence both camps cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.62).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.7).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Native-Generational Criterion of Hebrew Linguistic Life").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, '5002f84c-13ee-4aba-8c48-a55e78d5abcf').
narrative_ontology:cs_kernel_codification('5002f84c-13ee-4aba-8c48-a55e78d5abcf', distributed).
narrative_ontology:cs_authority_grounding('5002f84c-13ee-4aba-8c48-a55e78d5abcf', expertise).
narrative_ontology:cs_interpretation_layer_present('5002f84c-13ee-4aba-8c48-a55e78d5abcf').
narrative_ontology:cs_reading_relation('5002f84c-13ee-4aba-8c48-a55e78d5abcf', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('5002f84c-13ee-4aba-8c48-a55e78d5abcf', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('5002f84c-13ee-4aba-8c48-a55e78d5abcf', foundational, native_child_acquisition_is_necessary_for_life).
narrative_ontology:cs_axiom_status(native_child_acquisition_is_necessary_for_life, holdable).
narrative_ontology:cs_axiom_grounding('5002f84c-13ee-4aba-8c48-a55e78d5abcf', native_child_acquisition_is_necessary_for_life, empirically_contingent).
narrative_ontology:cs_axiom('5002f84c-13ee-4aba-8c48-a55e78d5abcf', foundational, secular_mundane_function_is_necessary_for_life).
narrative_ontology:cs_axiom_status(secular_mundane_function_is_necessary_for_life, holdable).
narrative_ontology:cs_axiom_grounding('5002f84c-13ee-4aba-8c48-a55e78d5abcf', secular_mundane_function_is_necessary_for_life, empirically_contingent).
narrative_ontology:cs_reference_frame('5002f84c-13ee-4aba-8c48-a55e78d5abcf', dormancy_then_native_revival).
narrative_ontology:cs_drift_state('5002f84c-13ee-4aba-8c48-a55e78d5abcf', contemporary_multilingual_vitality_scholarship, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5002f84c-13ee-4aba-8c48-a55e78d5abcf', '2026-06-11T00:00:00Z').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_revivalist_leadership).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_education_network).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_writers_and_teachers).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, native_hebrew_first_generation).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, eastern_european_yiddish_masses).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, sephardic_ladino_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, native_hebrew_first_generation).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, transmission_break_is_language_death).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, revival_requires_domestic_language_shift).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A circle of philologists, teachers, editors, and writers, Eliezer Ben-Yehuda foremost, who articulated the native-acquisition criterion, ran the Hebrew Language Committee from 1890 (the Academy of the Hebrew Language from 1953), coined the missing everyday vocabulary, and pressed families, schools, and parties to adopt Hebrew as sole vernacular. Their livelihoods, reputations, and life-work fused with the mission; abandoning it would have dissolved the meaning of their careers. They set the standard, adjudicated disputes under it, and collected the institutional authority, funding, and founding-narrative prestige that flowed from its adoption.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_revivalist_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% The kindergartens, the Herzliya Gymnasium, the teachers' seminaries, the Vaad Leumi school system, and later the state Ministry of Education: the institutions that made the criterion binding by teaching exclusively in Hebrew, disciplining or expelling pupils heard speaking Yiddish or Ladino in the yard, training the ulpan corps, and employing generations of Hebrew teachers. Enforcement was simultaneously their mandate and their payroll.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_education_network, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, hebrew_education_network, beneficiary).

% The bulk of the Second and Third Aliyah immigrants and later the displaced survivors arrived speaking Yiddish as mother tongue. Jobs, housing, ration cards, military service, and children's schooling were all reached through Hebrew-speaking institutions, and parents were urged, and sometimes ordered, to speak only Hebrew at home so infants would acquire it natively. Many complied, cutting their own fluent language off from their children; grandparents and grandchildren often could no longer converse. Leaving meant returning to devastated Europe or losing access to the national economy, so most stayed and paid.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, eastern_european_yiddish_masses, payer,
    powerless, biographical, trapped, global).

% Long-established Sephardic communities of Jerusalem, Salonika, and the Balkans spoke Judeo-Spanish as their living vernacular. Under the revived regime their language was reclassified as exilic debris; Sephardic schools were absorbed into Hebrew-only networks, and the younger generations shifted within two generations. Unlike newly arriving Ashkenazim they had deep local roots, which meant the pressure to abandon the language of their households and their press came from neighbors and local institutions rather than from distant officials.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, sephardic_ladino_communities, payer,
    moderate, biographical, constrained, regional).

% Children born in the Yishuv from the 1890s onward acquired Hebrew as mother tongue, the first such generation in roughly eighteen centuries. Hebrew handed them a shared tongue across every immigrant origin and full membership in the emerging nation. The flow ran the other way too: many understood their grandparents' Yiddish, Ladino, or Arabic poorly or not at all, and the family archive of songs, jokes, and stories passed out of reach. Their sense of self became bound up with being the first Hebrew generation, which makes retrospective questioning of the arrangement feel like self-negation.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, native_hebrew_first_generation, beneficiary,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, native_hebrew_first_generation, payer).

% Novelists, poets, journalists, and the teaching corps whose entire market existed because Hebrew became a vernacular. Careers, canon formation, literary prizes, and academic chairs depended on the language's official centrality, and they staffed the committees that coined terminology, wrote the textbooks, and reviewed the translations.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_writers_and_teachers, beneficiary,
    organized, biographical, identity_locked, regional).

% Bundists, YIVO scholars, Yiddish writers, and teachers who argued that Yiddish was itself the living national language of the Jewish masses and that layered arrangements, Hebrew for heritage and Yiddish for daily life, served the people better. Inside the Yishuv they were shut out of recognized schooling, their presses marginalized, Yiddish theater performances restricted at various junctures; they operated from Vilna, Warsaw, and New York, outside the arena where the decision was made.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddishist_cultural_activists, excluded,
    organized, generational, constrained, global).

% Communities across Eastern Europe, the Americas, and the Mediterranean that went on living in Yiddish, Ladino, Judeo-Arabic, and other Jewish languages. They were not directly subject to Yishuv enforcement, but the criterion's verdict that their mother tongues were dead or dying jargon circulated through Zionist youth movements, emissaries, and the Hebrew press, reshaping how their own young members valued the speech of their homes long before anyone migrated.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, diaspora_jewish_communities, excluded,
    organized, generational, mobile, global).

% Historical linguists and sociolinguists who document the dormancy period, compare the Hebrew revival with other revivals such as Cornish, Manx, Irish, and Maori, and test whether native child acquisition is necessary or merely sufficient for language vitality. They take no side in the national dispute, but both camps cite their evidence.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, linguistic_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__native_generational_reading, hebrew_revivalist_leadership).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__native_generational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a dispersed collective-action problem: turning a liturgical language into a mother tongue required thousands of households to give up the path of least resistance, raising children in the inherited vernacular, at roughly the same time. The criterion gave each family a crisp, public, checkable definition of doing its part, speak only Hebrew to the child, and gave the movement a success metric, native-born Hebrew speakers, that separated real revival from schoolbook competence.
% TRANSFER_FUNCTION: Moves linguistic labor and inheritance: domestic speech labor from mothers and grandparents into the national language project; cultural capital, fluency, canon access, full membership, to the native Hebrew generation; and status away from the inherited languages, reclassified from living mother tongues to exilic jargon. Materially it also moved funding, teaching posts, and publishing markets to Hebrew-language institutions.
% ABSENT_VOICES: Yiddishist and Ladino cultural activists objected that their languages were living national inheritances and that layered multilingual arrangements would serve the people better; they were excluded from Yishuv decision-making, their schools unrecognized, their presses marginal, their theater restricted. The children could not consent to losing their grandparents' tongue. Diaspora communities lived under the dead-language verdict without a seat where it was rendered.
% DISAPPEARANCE_RATIONALE: If the criterion vanished overnight, the revival loses its success condition and its coordinating slogan: families face no standardized reason to switch the home language, multilingual arrangements, Hebrew for texts and communal languages for life, become the default path, and the delegitimation of Yiddish and Ladino loses its warrant. Hebrew likely remains what classical Greek and Latin were, revered, studied, recited, not spoken. The demographic outcome of the last 140 years was organized around this standard.
% FOUNDING_PROBLEM: A national movement uniting immigrants from dozens of language communities needed a shared vernacular, and the only candidate with deep cultural legitimacy, Hebrew, had no native speakers and no tradition of mundane use; the founding problem was how to manufacture, quickly and permanently, a population that acquires Hebrew as mother tongue.
% FOUNDING_PROBLEM_CORROBORATION: External corroboration of the founding problem's reality: the historical-linguistic record of vernacular discontinuity and comparative revival scholarship attest both the dormancy and the unprecedented character of the revival, and neither source is a beneficiary of the criterion. Corroboration that the problem remains live today comes almost exclusively from Hebrew-language institutions themselves, Academy publications and education-ministry reports; outside scholarship treats native transmission as secure and studies Hebrew under ordinary vitality frameworks. That asymmetry is itself signal.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction (epsilon 0.62; referent: the standing monolingual-standard arrangement and its enforcement history, assessed by this reading's own lights) is substantial but bounded. The criterion genuinely solved the revival's collective-action problem: without a public, checkable definition of success, is the child a native speaker, the home-language switch plausibly stalls as each household free-rides on the others, and the revival it organized succeeded beyond precedent. Against that stand the documented costs borne by non-consenting parties: Yiddish and Ladino reclassified from living mother tongues to exilic debris, school exclusions and social sanction against children caught speaking them, and severed grandparent-to-grandchild transmission lines. Suppression (0.70) is constitutive rather than incidental: the criterion never held by preference alone. It required the Language Wars confrontations and boycotts, Hebrew-only school discipline, and later the state absorption machinery of ulpanim, army instruction, and allocation routed through Hebrew-speaking institutions. Theater (0.28) is low-to-moderate: the core activity, coining words, teaching, parenting in Hebrew, was real work; the late-interval rise reflects commemorative ritual around the revival anniversaries and the Academy's losing purity campaigns against English loanwords, which police a boundary the speech community has already crossed. Accessibility collapse (0.55): the rival criteria were never refuted, only institutionally outpowered, and multilingual practice persisted wherever enforcement did not reach; within the Yishuv proper, however, alternatives were closed by excluding rival schools, presses, and theaters. Resistance (0.60): sustained and organized, Bundist counter-mobilization, YIVO scholarship, Yiddish theater protests, Haredi Yiddish persistence, and the later Ladino and Mizrahi heritage movements. Identity-lock dynamics: the leadership's exit was closed by vocational fusion, their careers were the mission; the native generation's by self-concept, being the first Hebrew generation makes questioning the arrangement feel like self-negation; both locks are load-bearing for persistence. The measurement series share one eight-point grid spanning roughly 1880 to 2020. Suppression_requirement is authored deliberately because the story traces an enforcement ratchet from 1880 to a 1960 peak followed by enforcement decay as demographic victory made coercion redundant, a dynamic invisible in the static scalar. The series are arc-shaped rather than cyclical: rise, peak, partial relaxation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical facts. From the leadership seat the arrangement is the coordination device that made national rebirth possible, and the criterion looks like bedrock, something discovered about languages rather than chosen. From the Yiddish-mass seat the same criterion is the rule that stigmatized the mother tongue, gated jobs and schooling, and told parents to stop speaking to their children in the only language they commanded fluently. From the native generation's seat it is gift and loss at once, which is why that seat carries a dual role declaration rather than a resolved position. Inter-institutionally, the Language Committee, the Mandate-era school boards, and the post-1948 ministries enforced the same standard with different instruments and different exposure to resistance. Same-level lateral dynamics separate Hebrew writers, identity-locked beneficiaries, from Yiddishist writers, equally organized and equally literate but locked out, whose exit ran through diaspora cities the decision-making never touched. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real collection points: the leadership collects institutional authority, funding, and founding-narrative prestige; the education network collects mandate and payroll; writers and teachers collect the entire market for Hebrew letters; the native generation collects the mother tongue itself. Victim declarations map to the payers: the Yiddish-speaking masses bore coerced abandonment under trapped conditions, since economic and institutional gating left no realistic exit, and the rooted Sephardic communities bore the reclassification of their vernacular under constrained exit. The native generation's dual declaration, beneficiary with payer as secondary role, is the story's one genuinely bidirectional seat and is handled through the role structure rather than a directionality override, since the derivation already reads the secondary role. Excluded seats, the Yiddishist activists and the diaspora communities, sit outside the beneficiary/victim derivation but document the counterfactual arrangements, multilingual schooling and Yiddish cultural autonomy, whose suppression is the enforcement object. Scope: enforcement was regional, the Yishuv and later Israel, but the standard's verdicts propagated globally through Zionist media and emissaries, which is why the diaspora-facing seats carry global scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents both mislabels. A pure-rope reading, the celebratory revival narrative, erases the victims: it would treat Yiddish and Ladino abandonment as a regrettable side effect of a benign standard, when the exclusion of rival schools and the gating of immigrant life were constitutive, not incidental. A pure-snare reading, the diaspora-nationalist counter-narrative, erases the coordination: the criterion really did solve a free-rider problem no other instrument solved, and its product, a native Hebrew vernacular after eighteen centuries, is real and irreversible. The founding-problem interview returns status contested with verdict world_rearranges: the original problem, manufacturing native transmission fast enough to serve nation-building, is arguably solved, but the criterion persists because the parties dispute whether perpetual re-securing of native transmission is a live need or a post-hoc warrant for boundary-policing. The mismatch consumer finds no dead-problem zombie flag on this pairing, but the rising theater_ratio series marks where residue would pool if the exclusivity premise falls, see the criterion_universality omega: commemoration and purity campaigns are the shape maintenance takes when the function has been achieved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is the native_generational_reading of kernel hebrew_linguistic_life; which structural element do the sibling readings deny, and what changes if the corpus adopts one of them instead?',
    'Locate the disputed element: liturgical_preservation_reading denies the dormancy premise (Hebrew never ceased being alive), marketplace_pidgin_reading denies the native-acquisition necessity premise (functional mediumship suffices). Each sibling swap removes part of this reading''s structure.',
    'Under liturgical_preservation_reading the coercion record becomes gratuitous (nothing needed reviving) and the arrangement reclassifies toward ceremonial maintenance; under marketplace_pidgin_reading the native-transmission requirement drops out, the beneficiary/victim structure thins toward ordinary standard-setting, and extraction estimates fall sharply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one of three readings of the linguistic-life kernel; sibling swaps relocate the victim set and the enforcement warrant.').

omega_variable(
    dormancy_period_status,
    'Was Hebrew actually without native speakers from roughly 70 to 1880 CE, as this reading''s founding premise asserts?',
    'Demographic-linguistic reconstruction weighing epigraphic, documentary, and travel-record evidence of vernacular discontinuity against claims of continuous domestic Hebrew use in small pockets (e.g., Tiberias-area traditions).',
    'If continuous native use existed somewhere in the interval, the death premise weakens, the revival becomes acceleration rather than resurrection, and the criterion''s necessity claim loses its strongest supporting case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormancy_period_status, empirical, 'Reality of the 70-1880 native-speaker discontinuity asserted by this reading.').

omega_variable(
    coercion_vs_convergence_share,
    'How much of Yiddish/Ladino abandonment was produced by enforcement of this criterion versus ordinary immigrant language-shift pressures operating on all diaspora populations?',
    'Natural-experiment comparison with parallel migrations lacking a nativist language regime (Jewish immigration to the United States shifted to English within two generations with no Hebrew criterion); decompose observed shift rates into a generic-assimilation baseline and a criterion-specific excess.',
    'If most abandonment replicates the US baseline, measured extraction falls toward ordinary coordination cost; the excess over baseline isolates the criterion-specific burden borne by the victim seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_convergence_share, empirical, 'Attribution of language abandonment between institutional enforcement and generic assimilation.').

omega_variable(
    criterion_universality,
    'Is native child acquisition plus total vernacular function a universal law of language life, or a nation-building standard fitted to one movement''s needs?',
    'Cross-linguistic vitality audit: catalog languages widely judged alive that fail the criterion (broad L2 lingua francas with few native speakers, diglossic pairs like the Arabic dialects and classical Arabic, robust adult-L2 vitality cases) and languages meeting it that are nonetheless dying.',
    'If universality fails, the criterion is a constructed standard with identifiable beneficiaries rather than a natural fact about languages, which bears on how much deference its verdicts deserve and on any natural-law framing of linguistic life.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criterion_universality, conceptual, 'Whether the aliveness criterion generalizes beyond the Hebrew case or is parochial to the revival project.').

omega_variable(
    internalized_language_shame,
    'Is the persistence of Yiddish/Ladino abandonment after enforcement relaxed structural or internalized?',
    'Post-relaxation trajectory: measure heritage-program enrollment, family re-learning, and reported shame narratives across descendant cohorts once the institutional gates opened and stigma nominally lifted.',
    'If abandonment persists absent enforcement, part of the suppression travels inside the descendants, raising effective suppression above the structural measure and complicating remediation, since restoring options does not restore use.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_language_shame, empirical, 'Structural versus internalized component of language abandonment among descendant generations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__native_generational_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__native_generational_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement_basis(hebr_tr_t20, observed).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__native_generational_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement_basis(hebr_tr_t40, observed).
narrative_ontology:measurement(hebr_tr_t60, hebrew_linguistic_life__native_generational_reading, theater_ratio, 60, 0.17).
narrative_ontology:measurement_basis(hebr_tr_t60, observed).
narrative_ontology:measurement(hebr_tr_t80, hebrew_linguistic_life__native_generational_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement_basis(hebr_tr_t80, observed).
narrative_ontology:measurement(hebr_tr_t100, hebrew_linguistic_life__native_generational_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement_basis(hebr_tr_t100, observed).
narrative_ontology:measurement(hebr_tr_t120, hebrew_linguistic_life__native_generational_reading, theater_ratio, 120, 0.27).
narrative_ontology:measurement_basis(hebr_tr_t120, observed).
narrative_ontology:measurement(hebr_tr_t140, hebrew_linguistic_life__native_generational_reading, theater_ratio, 140, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t140, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(hebr_be_t20, observed).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(hebr_be_t40, observed).
narrative_ontology:measurement(hebr_be_t60, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 60, 0.66).
narrative_ontology:measurement_basis(hebr_be_t60, observed).
narrative_ontology:measurement(hebr_be_t80, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 80, 0.72).
narrative_ontology:measurement_basis(hebr_be_t80, observed).
narrative_ontology:measurement(hebr_be_t100, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 100, 0.67).
narrative_ontology:measurement_basis(hebr_be_t100, observed).
narrative_ontology:measurement(hebr_be_t120, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 120, 0.64).
narrative_ontology:measurement_basis(hebr_be_t120, observed).
narrative_ontology:measurement(hebr_be_t140, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 140, 0.62).
narrative_ontology:measurement_basis(hebr_be_t140, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t20, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(hebr_su_t20, observed).
narrative_ontology:measurement(hebr_su_t40, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(hebr_su_t40, observed).
narrative_ontology:measurement(hebr_su_t60, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement_basis(hebr_su_t60, observed).
narrative_ontology:measurement(hebr_su_t80, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 80, 0.84).
narrative_ontology:measurement_basis(hebr_su_t80, observed).
narrative_ontology:measurement(hebr_su_t100, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 100, 0.74).
narrative_ontology:measurement_basis(hebr_su_t100, observed).
narrative_ontology:measurement(hebr_su_t120, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 120, 0.68).
narrative_ontology:measurement_basis(hebr_su_t120, observed).
narrative_ontology:measurement(hebr_su_t140, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 140, 0.7).
narrative_ontology:measurement_basis(hebr_su_t140, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Hebrew linguistic life'. The label conflates three structurally distinct claims about what makes a language alive, and per the epsilon-invariance principle they are authored as three stories sharing one kernel. This file authors the native_generational_reading: life equals native child acquisition plus total vernacular function; it carries the dormancy premise (death 70-1880), the revival warrant, and the victim set, Yiddish and Ladino speakers coerced into abandonment, and therefore authors substantial epsilon. The liturgical_preservation_reading shares the referent but authors near-zero extraction and no victims, since under it nothing ever needed reviving; the marketplace_pidgin_reading authors intermediate values, since functional mediumship requires no coerced domestic switch. Upstream and downstream: the liturgical reading supplied the continuity-of-texts premise this reading rejects, and the marketplace reading supplied the functional-medium premise this reading subordinates to native transmission. Family links run through affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
