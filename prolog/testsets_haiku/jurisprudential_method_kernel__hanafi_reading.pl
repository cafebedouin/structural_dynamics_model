% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Jurisprudential Method: Reason as Legitimate Extension of Divine Intent
 *   domain: legal_philosophy/religious_authority/institutional
 *
 * SUMMARY:
 *   Islamic jurisprudence faces the foundational problem: how does law extend
 *   to cases not explicitly addressed by Qur'an or authenticated Hadith? The
 *   Hanafi school answers through extensive use of analogical reasoning
 *   (qiyas) and juristic preference (istihsan), claiming reason is a
 *   legitimate tool for extending divine intent to novel situations. This is
 *   ONE READING of a contested kernel—the jurisprudential method itself—that
 *   divides the major schools. Hanafi rationalism competes with Hanbali
 *   literalism (only text and consensus, reason corrupts), Maliki
 *   practice-based inference (Medinan precedent as preserved authenticity),
 *   and Shafi'i hierarchical ordering (text → hadith → ijma → qiyas, with
 *   hadith as final arbiter). The Hanafi reading becomes institutionalized in
 *   urban courts and the centralizing caliphal state precisely because it can
 *   handle governance at scale. The claim/metric gap is intentional: the
 *   constraint is CLAIMED as tangled_rope (real coordination for novel cases
 *   + asymmetric extraction benefiting rationalist jurists) while the
 *   authored metrics show substantial extraction (0.68) with moderate
 *   suppression (0.52) and low theater (0.28)—early in the interval the ratio
 *   favors coordination, but over 400 years extractiveness rises and theater
 *   rises slightly as the method becomes performatively necessary rather than
 *   functionally justified. The engine computes per-seat classification; the
 *   divergence between Hanafi institutional dominance and the textualist
 *   school's persistent textual legitimacy reveals the constraint's
 *   extractive structure.
 *
 * KEY AGENTS:
 *   - rationalist_jurists: institutional beneficiaries whose training and authority depend on reason being a legitimate tool; they occupy urban courts and scholarly networks where methodological innovation is expected and rewarded
 *   - textualist_hadith_purists: organized resistance whose claim to authenticity is systematically undermined; constrained exit because urban authority has been captured by the rationalist method
 *   - urban_scholarly_networks: institutional beneficiaries that depend on the Hanafi method to handle commercial and governance complexity; they employ rationalist jurists and maintain the infrastructure of reasoned jurisprudence
 *   - bedouin_and_rural_communities: powerless and excluded—dependent on urban judges for dispute resolution but not participants in the reasoning process; their local practices can be overridden by reasoned extension
 *   - caliphal_authority: institutional agenda-setter that benefits from a systematic jurisprudence capable of extending principles to novel administrative problems; the method serves state capacity to govern at scale
 *   - textualist_alternative_authority: organized observer that recognizes Hanafi institutionalization as a consolidation of power, not a demonstration of methodological superiority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.68).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.52).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Jurisprudential Method: Reason as Legitimate Extension of Divine Intent").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "legal_philosophy/religious_authority/institutional").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, '1ab1bcd4-af35-4bdc-a78d-8fb29984d490').
narrative_ontology:cs_kernel_codification('1ab1bcd4-af35-4bdc-a78d-8fb29984d490', fixed_text).
narrative_ontology:cs_authority_grounding('1ab1bcd4-af35-4bdc-a78d-8fb29984d490', lineage).
narrative_ontology:cs_interpretation_layer_present('1ab1bcd4-af35-4bdc-a78d-8fb29984d490').
narrative_ontology:cs_reading_relation('1ab1bcd4-af35-4bdc-a78d-8fb29984d490', jurisprudential_method_kernel__hanbali_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ab1bcd4-af35-4bdc-a78d-8fb29984d490', jurisprudential_method_kernel__maliki_reading, influences).
narrative_ontology:cs_reading_relation('1ab1bcd4-af35-4bdc-a78d-8fb29984d490', jurisprudential_method_kernel__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('1ab1bcd4-af35-4bdc-a78d-8fb29984d490', foundational, reason_extends_divine_intent).
narrative_ontology:cs_axiom_status(reason_extends_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('1ab1bcd4-af35-4bdc-a78d-8fb29984d490', reason_extends_divine_intent, instrumental).
narrative_ontology:cs_axiom('1ab1bcd4-af35-4bdc-a78d-8fb29984d490', foundational, juristic_preference_serves_public_welfare).
narrative_ontology:cs_axiom_status(juristic_preference_serves_public_welfare, holdable).
narrative_ontology:cs_axiom_grounding('1ab1bcd4-af35-4bdc-a78d-8fb29984d490', juristic_preference_serves_public_welfare, conventional).
narrative_ontology:cs_reference_frame('1ab1bcd4-af35-4bdc-a78d-8fb29984d490', quranic_hadith_as_complete_source).
narrative_ontology:cs_drift_state('1ab1bcd4-af35-4bdc-a78d-8fb29984d490', ottoman_codification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1ab1bcd4-af35-4bdc-a78d-8fb29984d490', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, urban_scholarly_networks).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_hadith_purists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, communities_excluded_from_juristic_reasoning).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, human_reason_extends_divine_intent).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, novel_cases_require_methodological_extension).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, juristic_preference_serves_public_interest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Islamic jurists trained in logic, rhetoric, and philosophical reasoning who argue that human reason is a divinely endorsed tool for extending Islamic principles to novel cases. They occupy positions of judicial authority in urban centers and scholarly networks. Their institutional position, prestige, and patronage depend on the legitimacy of reasoned analogical extension (qiyas) and juristic preference (istihsan). They benefit from a methodological framework that elevates their reasoning capacity as indispensable to Islamic governance. Exit would involve abandoning rationalist training and accepting textualist constraints on jurisprudential inference, which would diminish their institutional authority.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_jurists, beneficiary,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, rationalist_jurists, agenda_setter).

% Scholars and religious authorities committed to the literal text of Qur'an and authenticated Hadith, who argue that analogical reasoning and juristic preference are bid'ah (forbidden innovation) that corrupt the divine source. They maintain that Islamic law is complete in the sources and that reason-based extension represents a departure from authenticity. Their claim to jurisprudential legitimacy is systematically undermined by the rationalist framework—they cannot compete in urban courts where analogical extension is the standard method. Exit options are constrained: leaving would require accepting the rationalist apparatus or relocating to communities that reject rationalism.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_hadith_purists, payer,
    organized, biographical, constrained, regional).

% The administrative and intellectual infrastructure of courts, teaching circles, and state-sponsored jurisprudence in major urban centers. These networks depend on the Hanafi rationalist method to handle the complexity of urban commercial law, taxation disputes, inheritance questions, and governance issues that arise in cities. They employ jurists trained in analogical reasoning, maintain libraries of jurisprudential precedent and rulings, and generate demand for specialized training. The method's institutional prestige legitimates their central role in the judicial apparatus.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, urban_scholarly_networks, beneficiary,
    institutional, generational, arbitrage, regional).

% Communities outside major urban centers whose jurisprudential disputes are resolved through the Hanafi rationalist method but who lack training in analogical reasoning and access to the scholarly networks that generate and interpret reasoned extensions of the law. They are dependent on urban-trained judges for rulings on disputes not covered by literal text. Their lack of participation in the reasoning process means their local practices and understandings are overridden when they conflict with reasoned extensions by urban scholars. They bear the cost of legal subordination without participating in the reasoning that produces it.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, bedouin_and_rural_communities, excluded,
    powerless, biographical, trapped, local).

% The centralizing state apparatus that benefits from a rationalist, systematic jurisprudence capable of handling governance at scale. A method that can extend principles to novel administrative situations—taxation of unfamiliar trades, regulation of new types of commerce, frontier settlement law, property questions arising from conquest and resettlement—is more useful to state authority than textualism, which cannot systematize novel cases. The caliphal court employs Hanafi jurists precisely because they can provide reasoned legal solutions to governance problems the sources do not directly address.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, caliphal_authority, agenda_setter,
    institutional, generational, mobile, continental).

% Communities and scholarly networks that explicitly reject Hanafi rationalism and defend Hanbali literalism or alternative methodologies as the correct interpretation of Islamic jurisprudence. They observe the Hanafi framework's dominance in urban and state institutions and recognize that institutional power has concentrated around the rationalist method in ways that marginalize competing approaches, despite their historical and textual legitimacy.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanbali_alternative_authority, observer,
    organized, generational, constrained, regional).

% External analysts examining the relationship between jurisprudential methodology and institutional power consolidation. They observe that the Hanafi elevation of reason as a legitimate tool becomes self-reinforcing: the method is institutionalized in urban courts and state apparatus; this concentration of institutional authority rewards the jurists trained in the method; the method's prestige and necessity grow; alternative methods are pushed to the margins or confined to alternative-authority contexts. The observer seat can document whether this is genuine coordination solving an intractable problem (novel cases require principled reasoning beyond textualism) or extraction (a school claiming jurisprudential exclusivity).
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanafi_reading, rationalist_jurists).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a systematic, scalable methodology for extending Islamic legal principles to novel cases not explicitly covered by Qur'an and authenticated Hadith. Urban centers, expanding commerce, and state governance generate legal questions—What is the status of a tax on a trade not mentioned in the sources? How does intestacy law apply to merchants from distant regions? What contracts are permissible for frontier settlements?—that cannot be answered by literal application of text alone. Analogical reasoning (qiyas) and juristic preference (istihsan) allow coherent, principled legal response to novel situations without abandoning the claim to divine source. This function is genuinely necessary for state capacity at scale.
% TRANSFER_FUNCTION: Transfers jurisprudential authority from those whose legitimacy derives from hadith memorization and authentication to those trained in logic, analogy, and philosophical reasoning. It moves social prestige, institutional position, and judicial power upward toward rationalist jurists and away from textualist scholars. It also transfers interpretive power upward from scattered community consensus and local practice toward centralized urban scholarly networks and the state apparatus, which can sponsor jurisprudential innovation and maintain consistency across diverse regions.
% ABSENT_VOICES: Textualist hadith scholars who argue the rationalist method corrupts authenticity and that only consensus (ijma) can extend the law beyond text; bedouin and rural communities whose local jurisprudential practices and understanding are overridden by reasoned extensions they did not participate in deriving; alternative jurisprudential schools (Hanbali literalism, Maliki practice-based reasoning) whose methods retain textual authority and historical legitimacy but are increasingly marginalized in state institutions. These absent voices would argue that reason-based extension enables jurists to impose their preferences under the guise of extending divine intent, and that textualist constraint better preserves the Qur'an's authority against human manipulation.
% DISAPPEARANCE_RATIONALE: If the Hanafi legitimation of analogical reasoning and juristic preference disappeared—if the institutional framework collapsed and alternative methodologies took over—urban jurisprudence would fragment significantly: novel cases without textual source would either go unresolved (disrupting commercial contracts, taxation, and governance), would be resolved through alternative methods (Hanbali literalism rejecting innovation, Maliki reliance on Medinan practice), or would be handled through ad hoc ijtihad without systematic method. State capacity to govern novel situations consistently would diminish. The judicial infrastructure centered on analogical jurisprudence would lose prestige and patronage. Jurists trained solely in rationalist extension would lose institutional authority.
% FOUNDING_PROBLEM: Early Islamic jurisprudence lacked systematic methodology for handling legal questions that arose in contexts beyond the Prophet's lifetime and immediate successor period: What is the legal status of wine produced from fruits not mentioned in Hadith? How do intestacy principles apply to merchants and converts from distant regions? How does one lawfully govern a newly conquered frontier settlement or administer a tax on a trade unknown during the Prophet's era? The textualist approach—memorize sources, apply them literally, recognize silence where they are silent—was inadequate for the expanding Islamic state and urban economies. The founding problem was methodological: how to extend divine law coherently to novel cases without claiming divine revelation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historians of Islamic jurisprudence outside the Hanafi benefiting circle: even Hanbali and Maliki scholars acknowledge that some jurisprudential extension beyond literal text is necessary for governance. The disagreement is not whether extension occurs, but which method legitimately extends the law—whether through reason (Hanafi), through authenticated hadith hierarchy (Shafi'i), through Medinan practice (Maliki), or through consensus only (Hanbali). Modern scholarship on Islamic legal history (e.g., historians studying Ottoman governance, Indian Ocean trade law, sub-Saharan Islamic courts) documents that the expansion of Islamic jurisprudence into new territories, new economies, and new governance contexts required systematic reasoning about novel cases. The problem persists in contemporary Islamic jurisprudence: contemporary cases (artificial reproduction, digital contracts, cryptocurrency, space law) still require reasoned extension. The founding problem is empirically alive.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over 400 years (approximately the period from early Hanafism through the Ottoman codification era). The early rise (0.42→0.55 by t=100) reflects the period when analogical reasoning genuinely solved novel-case coordination problems and Hanafi jurists were valued for their capability. The middle rise (0.55→0.68 by t=250) reflects the period when Hanafi method becomes institutionalized in urban courts and state apparatus—now the benefit is not just solving novel cases but controlling who gets to solve them. The plateau (0.68 at t=350 and t=400) suggests a mature state of institutionalization: the method is now the only legitimate approach in state institutions, textualist alternatives are marginalized, and the extraction is stable because the beneficiary class has consolidated control. Theater remains low (0.28) because the method is actively used to justify judicial decisions—performance is not theatrical; the performance IS the work. Suppression is moderate (0.52) and stable because textualists are not actively persecuted but structurally excluded from urban authority—suppression operates through institutional gatekeeping rather than direct coercion. Accessibility collapse is low (0.45) because textualist alternatives remain intellectually coherent even if institutionally marginal—the text itself does not collapse as an alternative; rather, institutional power concentrates around the rationalist method. Resistance is moderate (0.58) and stable—textualist scholars maintain a steady counterclaim to methodological authenticity, but institutional structures prevent that claim from translating into judicial power.
 *
 * PERSPECTIVAL GAP:
 *   From the rationalist jurist's institutional seat, the Hanafi method is genuine coordination—they are solving the novel-case problem that fragmented textualism cannot solve, and they deserve the authority they've earned through training and competence. From the textualist seat, the same structure is extractive—a methodological innovation that privileges jurists with access to Greek logical training and urban scholarly networks, while dismissing textualist scholars as insufficient and their authentic sources as incomplete. The analytical observer seat can see both: the method does solve a real problem (novel cases need reasoned extension) AND it concentrates authority in the hands of jurists trained in rationalist methods, marginalizing alternative approaches that might also coherently extend the law. The engine computes per-seat classification from the structural data: rationalist jurists will compute the constraint as rope or tangled_rope (coordination + modest extraction), textualist scholars will compute it as snare (extraction disguised as methodological necessity), and the analytical observer will see the intermediate case (genuine coordination + substantial extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist jurists sit at d ≈ 0.1-0.2 (near the beneficiary end): they collect authority, institutional position, patronage, and prestige from the rationalist method. Their exit is mobile—they can leave rationalist networks, but doing so means abandoning the training and authority they've accumulated. Textualist hadith purists sit at d ≈ 0.75-0.85 (near the target end): they bear the cost of institutional marginalization, their claim to authenticity is systematically undermined, and their exit is constrained—leaving means relocating to alternative authority structures or abandoning their scholarly tradition. Urban scholarly networks sit at d ≈ 0.25 (moderately beneficiary): they benefit from the method's institutional prestige and state patronage, but they also solve a genuine coordination problem, so some of the benefit is legitimate. Bedouin and rural communities sit at d ≈ 0.65 (moderately target): they depend on urban judges for novel-case resolution but don't participate in the reasoning; they pay through loss of local autonomy when reasoned extensions override their practices. Caliphal authority sits at d ≈ 0.2: it benefits from a systematic jurisprudence but also gains from having solved a real problem (novel-case governance). The beneficiary/victim declarations drive these d values; the engine scales them by exit options and power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The Hanafi method's founding mandate was to handle novel cases incoherently addressed by literal textualism. This mandate is LIVE—novel cases continue to arise in commerce, governance, and jurisprudence, and the Hanafi method continues to generate principled responses. However, the mandatrophy risk is substantial: if the rationalist method becomes purely theatrical (judges invoking analogical reasoning as cover for politically desired outcomes rather than genuinely extending principles), or if alternative methods prove capable of handling novel cases equally well, the mandate atrophies while the authority structure persists. The measurement series shows this risk: extractiveness rises faster than the underlying novel-case problem (theater rises only slightly, suggesting the increase in extractiveness is not coming from more reasoning or better solutions, but from the method's institutionalization as a control mechanism). The constraint does NOT have mandatrophy at the interval endpoint—the method is actively used—but mandatrophy is the trajectory if institutional capture accelerates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reason_as_legitimacy_cover,
    'Is the elevation of reason as a tool for extending divine intent genuinely solving a coordination problem (novel cases need principled resolution), or does it primarily serve to legitimize the authority of rationalist jurists by clothing their preferences in methodological necessity?',
    'Comparative analysis of jurisprudential outcomes: do analogical extensions consistently follow from stated principles, or do they cluster around outcomes that benefit rationalist authority? Do courts using alternative methods (Hanbali literalism, Maliki practice-based) produce systematically different outcomes on the same dispute, or similar ones? Do the reasoned extensions change when the beneficiary class or institutional context changes?',
    'If reason-based extension is primarily legitimizing jurists'' preferences, the constraint reclassifies from tangled_rope (coordination + extraction) to snare (pure extraction with methodological cover). If the extensions genuinely follow from principles and solve novel-case coordination, the extraction component is smaller and the coordination genuinely asymmetric (benefiting rationalists more than textualists, but solving a real problem).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reason_as_legitimacy_cover, empirical, 'Whether the rationalist method''s core claim to principled extension is genuine coordination or post-hoc legitimation of jurists'' authority.').

omega_variable(
    alternative_methodologies_foreclosure,
    'Could Hanbali literalism, Maliki practice-based reasoning, or strict Qiyas-only approaches produce equally coherent jurisprudence for the novel cases the Hanafi method claims to solve, or is reasoned analogical extension + juristic preference actually necessary?',
    'Historical analysis of how alternative schools handled the same novel cases (e.g., Ottoman Hanbalis on new tax classes, Maliki courts on merchant disputes, Shafi''i courts on frontier governance). Do outcomes differ systematically? Are there domains where alternatives demonstrably cannot coherently extend the law?',
    'If alternatives can coherently solve the same problems, Hanafi ascendance is institutional/political rather than functionally necessary—the method is extractive even if solving a real problem (the problem could be solved otherwise). If alternatives produce incoherent or fragmentary results on novel cases, the Hanafi method genuinely solves a coordination problem better than competitors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_methodologies_foreclosure, empirical, 'Whether Hanafi rationalist methodology is uniquely capable or institutionally preferred among functional alternatives.').

omega_variable(
    institutional_capture_of_methodology,
    'As the Hanafi method becomes institutionalized in urban courts and state apparatus, does the method itself become capturable—can a jurist claim to use analogical reasoning and juristic preference to legitimize whatever ruling the state or powerful interest desires?',
    'Longitudinal study of jurisprudential reasoning in disputes involving state interests vs. powerless parties: does the method''s procedural rigor remain consistent, or does analogical reasoning become more permissive when state interests are at stake? Do textualist scholars successfully challenge reasoned rulings as arbitrary or result-oriented?',
    'If the method becomes systematically capturable by state power and powerful interests, the constraint evolves from tangled_rope toward snare (the reasoning apparatus becomes a tool for disguising power as principle). Theatrical maintenance would increase—the reasoning would be performed as legitimate while serving concentrated interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_of_methodology, empirical, 'Whether institutionalized rationalist jurisprudence retains methodological integrity or becomes a capture mechanism for state and elite interests.').

omega_variable(
    textualist_suppression_mechanism,
    'Is the suppression of textualist hadith scholars structural (their literalism genuinely cannot handle novel cases, so urban courts reject them) or internalized (textualist scholars have internalized the narrative that their method is inferior and insufficient, even where it could coherently apply)?',
    'Post-institutional-marginalization trajectory: where textualist scholars retain institutional autonomy (rural courts, alternative authority structures), do they successfully apply their method to novel cases, suggesting suppression is structural? Or do textualist approaches remain fragmentary even where institutionally unconstrained, suggesting textualists themselves believe the rationalist narrative?',
    'If suppression is structural, the constraint is a genuine coordination problem solved differently by different methods. If suppression is internalized, the textualist victimhood is deeper—the constraint has colonized the cognitive self-understanding of the competing approach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textualist_suppression_mechanism, empirical, 'Whether suppression of textualism is structural constraint or internalized cognitive capture.').

omega_variable(
    kernel_reading_contest,
    'Is the Hanafi reading of the jurisprudential method kernel genuinely coexistent with Hanbali, Maliki, and Shafi''i readings within a single Islamic legal framework, or does Hanafi ascendance foreclose the alternatives as legitimate methodological choices at the framework level?',
    'Examination of whether texts defending alternative methodologies remain available, taught, and invoked in jurisprudential disputes. If Hanbali and Maliki texts are actively suppressed from circulation, the relation is foreclosure. If they remain available but institutionally marginal, the relation is coexistence with influence.',
    'If Hanafi reasoning forecloses alternatives, the readings are not truly coexistent—one reading has won at the level of framework legitimacy. If coexistent, the constraint is one reading among multiple valid readings, and the Hanafi ascendance is institutional/political rather than conceptually necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The logical relationship between the Hanafi reading and sibling methodological readings of the same jurisprudential kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(juri_tr_t0, observed).
narrative_ontology:measurement(juri_tr_t50, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 50, 0.14).
narrative_ontology:measurement_basis(juri_tr_t50, observed).
narrative_ontology:measurement(juri_tr_t100, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 100, 0.16).
narrative_ontology:measurement_basis(juri_tr_t100, observed).
narrative_ontology:measurement(juri_tr_t150, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 150, 0.2).
narrative_ontology:measurement_basis(juri_tr_t150, observed).
narrative_ontology:measurement(juri_tr_t250, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 250, 0.26).
narrative_ontology:measurement_basis(juri_tr_t250, observed).
narrative_ontology:measurement(juri_tr_t350, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 350, 0.28).
narrative_ontology:measurement_basis(juri_tr_t350, observed).
narrative_ontology:measurement(juri_tr_t400, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 400, 0.28).
narrative_ontology:measurement_basis(juri_tr_t400, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(juri_be_t0, observed).
narrative_ontology:measurement(juri_be_t50, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 50, 0.48).
narrative_ontology:measurement_basis(juri_be_t50, observed).
narrative_ontology:measurement(juri_be_t100, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 100, 0.55).
narrative_ontology:measurement_basis(juri_be_t100, observed).
narrative_ontology:measurement(juri_be_t150, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 150, 0.61).
narrative_ontology:measurement_basis(juri_be_t150, observed).
narrative_ontology:measurement(juri_be_t250, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 250, 0.67).
narrative_ontology:measurement_basis(juri_be_t250, observed).
narrative_ontology:measurement(juri_be_t350, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 350, 0.68).
narrative_ontology:measurement_basis(juri_be_t350, observed).
narrative_ontology:measurement(juri_be_t400, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 400, 0.68).
narrative_ontology:measurement_basis(juri_be_t400, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(juri_su_t0, observed).
narrative_ontology:measurement(juri_su_t50, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 50, 0.4).
narrative_ontology:measurement_basis(juri_su_t50, observed).
narrative_ontology:measurement(juri_su_t100, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 100, 0.45).
narrative_ontology:measurement_basis(juri_su_t100, observed).
narrative_ontology:measurement(juri_su_t150, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 150, 0.48).
narrative_ontology:measurement_basis(juri_su_t150, observed).
narrative_ontology:measurement(juri_su_t250, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 250, 0.52).
narrative_ontology:measurement_basis(juri_su_t250, observed).
narrative_ontology:measurement(juri_su_t350, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 350, 0.52).
narrative_ontology:measurement_basis(juri_su_t350, observed).
narrative_ontology:measurement(juri_su_t400, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 400, 0.52).
narrative_ontology:measurement_basis(juri_su_t400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(jurisprudential_method_kernel__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__hanbali_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel__shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, state_capacity_novel_case_governance).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, urban_judicial_authority_consolidation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested jurisprudential_method_kernel. The kernel is a fixed commitment: how does law extend to cases beyond the sources? The Hanafi reading answers through rationalist analogical extension; Hanbali answers through literalism and consensus only; Maliki answers through Medinan practice; Shafi'i answers through a strict hierarchy with hadith as arbiter. These four readings form a constraint family linked by network.affects_constraints—the ascendance of one reading affects the institutional viability of the others. The constraint's ε is invariant within the Hanafi reading (0.68 at interval endpoint), but the ε-valuations of alternative readings differ: Hanbali literalism has lower ε for novel-case coordination (cannot handle cases without textual source), Maliki practice-based reasoning has moderate ε, Shafi'i hierarchy has high ε for consistency but lower ε for flexibility. The ε-invariance principle requires separate story files for each reading; cross-reading comparison is enabled by the network linkages and the shared kernel_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel__hanafi_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
