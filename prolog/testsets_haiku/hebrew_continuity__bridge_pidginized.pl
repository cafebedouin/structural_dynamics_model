% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__bridge_pidginized
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__bridge_pidginized, []).

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
 *   constraint_id: hebrew_continuity__bridge_pidginized
 *   human_readable: Hebrew as Diaspora Contact Language: Instrumental Bridge Pidginization
 *   domain: sociolinguistics/commitment_system
 *
 * SUMMARY:
 *   Hebrew's survival in diaspora is contested between three incompatible
 *   readings: (1) liturgical preservation — Hebrew survives through classical
 *   texts and ritual recitation, (2) native generative — Hebrew lives only
 *   through native Israeli speakers whose intuition drives real language
 *   evolution, and (3) this reading: bridge pidginization — Hebrew survives
 *   as a contact language serving diaspora coordination, using a flattened
 *   register, instrumental phonetics, and neologisms. The bridge_pidginized
 *   reading treats Hebrew as a tool for identity maintenance, not as the
 *   expression of native speaker competence or classical textual authority.
 *   This constraint story instantiates ONLY this reading: Hebrew as diaspora
 *   lingua franca, neither liturgical nor natively generative, legitimized
 *   through institutional gatekeeping of partial fluency.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities — net beneficiaries; coordinate diaspora identity without native-language gatekeeping
 *   - secular_jewish_institutions — agenda setters; define and enforce the pidginized norm, benefit from institutional legitimacy without native-speaker requirements
 *   - native_hebrew_speaker_communities — payers; bear delegitimization of native authority and semantic flattening
 *   - traditional_liturgical_practitioners — payers (identity-locked); maintain classical authority but cannot prevent marketplace dominance; secondary beneficiary through preserved textual access
 *   - israeli_state_language_authority — observer; monitors but lacks enforcement power
 *   - competing_diaspora_lingua_francas — excluded; functionally available but institutionally illegitimate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, 0.58).
domain_priors:suppression_score(hebrew_continuity__bridge_pidginized, 0.42).
domain_priors:theater_ratio(hebrew_continuity__bridge_pidginized, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(hebrew_continuity__bridge_pidginized, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__bridge_pidginized, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__bridge_pidginized, "Hebrew as Diaspora Contact Language: Instrumental Bridge Pidginization").
narrative_ontology:topic_domain(hebrew_continuity__bridge_pidginized, "sociolinguistics/commitment_system").

domain_priors:requires_active_enforcement(hebrew_continuity__bridge_pidginized).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__bridge_pidginized, '25945e04-3a9b-4c37-a7ab-93f5d47c7be5').
narrative_ontology:cs_kernel_codification('25945e04-3a9b-4c37-a7ab-93f5d47c7be5', distributed).
narrative_ontology:cs_authority_grounding('25945e04-3a9b-4c37-a7ab-93f5d47c7be5', distributed).
narrative_ontology:cs_reading_relation('25945e04-3a9b-4c37-a7ab-93f5d47c7be5', hebrew_continuity__liturgical_preservation, influences).
narrative_ontology:cs_reading_relation('25945e04-3a9b-4c37-a7ab-93f5d47c7be5', hebrew_continuity__native_generative, forecloses).
narrative_ontology:cs_axiom('25945e04-3a9b-4c37-a7ab-93f5d47c7be5', foundational, language_life_through_instrumental_use).
narrative_ontology:cs_axiom_status(language_life_through_instrumental_use, holdable).
narrative_ontology:cs_axiom_grounding('25945e04-3a9b-4c37-a7ab-93f5d47c7be5', language_life_through_instrumental_use, conventional).
narrative_ontology:cs_axiom('25945e04-3a9b-4c37-a7ab-93f5d47c7be5', foundational, diaspora_coordination_legitimates_partial_fluency).
narrative_ontology:cs_axiom_status(diaspora_coordination_legitimates_partial_fluency, holdable).
narrative_ontology:cs_axiom_grounding('25945e04-3a9b-4c37-a7ab-93f5d47c7be5', diaspora_coordination_legitimates_partial_fluency, instrumental).
narrative_ontology:cs_reference_frame('25945e04-3a9b-4c37-a7ab-93f5d47c7be5', diaspora_hebrew_contact_lingua_franca).
narrative_ontology:cs_drift_state('25945e04-3a9b-4c37-a7ab-93f5d47c7be5', contemporary_english_dominance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('25945e04-3a9b-4c37-a7ab-93f5d47c7be5', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__bridge_pidginized, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, secular_jewish_institutions).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, native_hebrew_speaker_communities).
narrative_ontology:constraint_victim(hebrew_continuity__bridge_pidginized, traditional_liturgical_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__bridge_pidginized, traditional_liturgical_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Use Hebrew as a practical lingua franca connecting Ashkenazi, Sephardic, and Mizrahi diaspora members across linguistic boundaries. For them, Hebrew solves a real coordination problem: shared identity marker without imposing any group's native language. The constraint benefits them by making partial fluency acceptable and instrumental use legitimate, lowering the barrier to participation.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Universities, cultural organizations, and youth movements administer and enforce the pidginized norm. They set curriculum standards that treat reading comprehension as sufficient (not native fluency), authorize lexical innovation, and accept phonetic variance. They benefit by maintaining institutional legitimacy as 'Hebrew-speaking' without requiring native-speaker gatekeeping or intensive native-speaker coaching.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, secular_jewish_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the cost of seeing their language treated as a contact lingua franca rather than the expression of native intuition. They experience pidginization as semantic flattening, erosion of register distinction, and delegitimization of native-speaker authority. They cannot exit because Hebrew's institutional authority in diaspora depends on its connection to native Israeli Hebrew, yet diaspora use systematically redefines what 'Hebrew' means in ways native speakers find alienating.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, native_hebrew_speaker_communities, payer,
    moderate, biographical, constrained, national).

% Maintain Hebrew through classical texts and ritual recitation—the biblical and rabbinic register. They derive legitimacy from this preservation function and see themselves as the 'true' carriers. Yet the pidginized reading treats their classical register as one available subset among many, not the authoritative core. They maintain some institutional power (religious authority, textual custody) but cannot prevent the marketplace pidgin from becoming the dominant diaspora practice.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, traditional_liturgical_practitioners, payer,
    powerful, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__bridge_pidginized, traditional_liturgical_practitioners, beneficiary).

% Monitors diaspora Hebrew use and occasionally intervenes in standardization debates, but lacks enforcement power over diaspora institutions. They observe the pidginization as either a threat to Hebrew purity (traditional view) or an inevitable adaptation (descriptive view). Their analytical seat allows them to document the constraint without being captured by it.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, israeli_state_language_authority, observer,
    institutional, generational, analytical, national).

% English, Yiddish, and minority-community native languages would serve similar diaspora coordination functions, but the pidginized Hebrew norm excludes them from institutional legitimacy. They remain functionally available but are not named as acceptable vehicles for Jewish institutional identity. Their exclusion is what the constraint's enforcement machinery maintains.
narrative_ontology:constraint_stakeholder(hebrew_continuity__bridge_pidginized, competing_diaspora_lingua_francas, excluded,
    powerful, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__bridge_pidginized, secular_jewish_institutions).
narrative_ontology:fixing_cost_class(hebrew_continuity__bridge_pidginized, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared identity-marker language bridging diaspora communities across native-language boundaries, enabling institutional participation and cultural continuity without requiring native fluency or classical liturgical mastery.
% TRANSFER_FUNCTION: Moves legitimacy from native-speaker authority and classical register purity to institutional gatekeeping and contact-language pragmatism. Transfers the burden of language maintenance from native speakers (who cannot enforce purity) to institutions (which define acceptability). Extracts from traditional practitioners the authority to define what counts as 'real Hebrew.'
% ABSENT_VOICES: Competing diaspora languages (Yiddish, English, minority native languages) would object to exclusion from institutional legitimacy; diaspora members who prefer their heritage languages over Hebrew would argue for linguistic pluralism. Neither constituency is present in the institutional conversation that defines the norm.
% DISAPPEARANCE_RATIONALE: If the pidginized norm vanished overnight, diaspora institutions would face a three-way split: liturgical traditionalists would restore classical register as the standard (limiting accessibility), native-speaker gatekeeping would require intensive fluency (excluding casual participants), or institutions would switch to competing lingua francas like English (displacing Hebrew's institutional role). The current arrangement persists because it balances these pressures; removing it would force a choice no faction wants.
% FOUNDING_PROBLEM: Early post-Enlightenment diaspora Jewry faced linguistic fragmentation: Ashkenazi used Yiddish, Sephardic used Ladino, Mizrahi used Arabic, and all used local European/Arab languages. No shared diaspora language existed to mark Jewish institutional identity without imposing one group's native tongue.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish language (Spolsky, Kutscher) corroborate the 19th-century fragmentation problem. However, they dispute whether pidginized Hebrew solves it or replaces it with a new extraction: modern scholars argue that the founding problem could be solved equally well through English (which is now facto standard in many diaspora institutions) or through intentional multilingualism. The Israeli state and diaspora traditionalists claim the founding problem is still live; secular diaspora institutions and younger generations claim it is solved and the arrangement now persists as institutional inertia.
narrative_ontology:disappearance_verdict(hebrew_continuity__bridge_pidginized, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__bridge_pidginized, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__bridge_pidginized, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__bridge_pidginized, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__bridge_pidginized_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__bridge_pidginized, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__bridge_pidginized_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1880: Hebrew barely used, cost of coordination still borne by native Yiddish/Ladino) to 0.58 (2026: pidginized norm established, native speakers' authority extracted, classical practitioners forced to inhabit a non-authoritative register). Theater ratio rises sharply from 0.25 to 0.61 by 1990, then plateaus: the constraint increasingly performs 'Hebrew preservation' while the actual function shifts to institutional gatekeeping of contact-language pragmatism. Suppression requirement stays moderate (0.22→0.42) because the constraint operates partly through institutional incentive alignment (institutions get legitimacy-without-purity) and partly through excluding competitors — active suppression is needed to keep alternative lingua francas (English, Yiddish) from displacing Hebrew, but not as forcefully as in pure extraction because the coordination benefit is genuine for diaspora communities. Accessibility collapse is low-moderate (0.48): alternatives are always structurally available (communities could use English, Yiddish, or mother-tongues), but institutional gatekeeping makes them practically illegitimate. Resistance is high (0.67): traditional practitioners contest the norm continuously, Israel's language authority questions it, and native communities feel alienated — yet resistance has not overturned the arrangement because diaspora communities see genuine benefit.
 *
 * PERSPECTIVAL GAP:
 *   From the diaspora community's seat, the constraint is genuine coordination solving a real linguistic fragmentation problem. From the native speaker's seat, it is delegitimizing extraction that redefines their language as a contact code, not a living system. From the traditional practitioner's seat, it is a theft of textual authority. The engine computes these divergences from the structural data: diaspora communities have d≈0.2 (beneficiaries, moderate power, mobile exit), native speakers have d≈0.75 (victims, constrained exit), traditionalists have d≈0.8 (identity-locked victims). The agenda-setter seat (secular institutions) has d≈0.15 (collects legitimacy, shapes the rules, high power). The override for 'powerful' to d=0.35 reflects that one powerful actor (traditional practitioners) are victimized even though power usually correlates with beneficiary status — their power (institutional, textual) is structural but insufficient against the organizational apparatus wielding the pidginized norm.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are diaspora communities and secular institutions: they gain a working solution to diaspora coordination without requiring native fluency or classical mastery. Victims are native speaker communities (whose authority is extracted and whose language is simplified into contact-code semantics) and traditional practitioners (whose classical register is rendered non-authoritative and whose textual custody is decentered). Native speakers cannot easily exit—they carry Hebrew as part of their identity and professional authority, yet the diaspora pidginized norm systematically delegitimizes them. Traditionalists are identity-locked: abandoning Hebrew for another ritual language is unthinkable within their tradition, yet the constraint forces them to occupy a subordinate register. Diaspora communities have mobile exit (they could switch to English or Yiddish) but find the Hebrew solution genuinely useful, so they remain willingly. The directionality override addresses the fact that traditional practitioners are powerful (institutional, religious authority, textual custody) yet victimized by a constraint administered by less-powerful institutions — their power is structured as cultural/textual authority, not organizational enforcement capacity, so it cannot resist the organizational machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the false-mountain trap by declaring both a genuine coordination function AND clear beneficiaries and victims. It is not mountain-disguised-as-rope. The founding problem (diaspora linguistic fragmentation) was real and is partly solved. But the solution now extracts from native speakers and traditionalists—it is a tangled rope, not pure coordination. If the founding problem disappeared (diaspora communities adopted English wholesale, national-origin identities dissolved, Jewry became monolingual), the constraint might persist as Piton: institutions would maintain the Hebrew-teaching infrastructure theatrically because institutional legitimacy depends on the narrative continuity, even though the coordination function evaporated. The theater ratio's plateau at 0.61 suggests that dynamic is beginning: the constraint is already maintaining performative function (Hebrew 'preservation') while the real function (diaspora coordination) is increasingly displaced by English.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pidginization_vs_genuine_evolution,
    'Is diaspora Hebrew a pidginized contact code (structurally simplified, instrumental, non-natively-generative) or a genuine evolved variety with legitimate linguistic autonomy?',
    'Linguistic analysis comparing diaspora Hebrew phonology, morphosyntax, and semantics to native Israeli Hebrew and classical register across large corpora. Sociolinguistic interviewing of diaspora speakers about their sense of ownership and normativity. Evolutionary simulation testing whether diaspora innovations show genuine language-change mechanisms or only borrowing/interference patterns.',
    'If pidginized (contact code): the constraint is extraction from native authority and tradition toward institutional pragmatism — tangled_rope confirmed. If genuinely evolved variety: the constraint could be reframed as rope (legitimate coordination through a new diaspora register) and native-speaker payers would become observers/secondary beneficiaries of diaspora linguistic vitality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pidginization_vs_genuine_evolution, empirical, 'Whether diaspora Hebrew is a genuinely autonomous linguistic variety or a simplified contact code.').

omega_variable(
    institutional_gatekeeping_necessity,
    'Is institutional enforcement necessary to maintain the pidginized norm, or does it persist because diaspora communities find it genuinely useful?',
    'Natural experiment from diaspora communities where institutional gatekeeping weakens (e.g., youth movements defunded or disbanded): does Hebrew usage collapse, or does it persist through community practice? Ethnographic study of diaspora Hebrew peer-teaching and informal transmission outside institutional settings.',
    'If institutional gatekeeping is necessary: suppression is structural and the extraction from native authority is essential to the constraint''s persistence — snare-side dynamics. If communities sustain it informally: the coordination benefit is real and the extraction is incidental — rope-side dynamics. This affects how the constraint reclassifies if institutions weaken.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gatekeeping_necessity, empirical, 'Whether the pidginized norm persists through community utility or institutional enforcement.').

omega_variable(
    reading_foreclosure_empirical,
    'Does the establishment of diaspora Hebrew as a pidginized contact language logically foreclose the native_generative reading for diaspora contexts, or do native and contact readings coexist?',
    'Ethnographic study: can diaspora native-speaker communities maintain native generative norms while the broader diaspora uses contact-language pidgin? Do diaspora native speakers teach children the classical register or the pidginized marketplace norm? Are native-speaker enclaves forming, or has native competence disappeared from diaspora entirely?',
    'If native generativity is foreclosed for diaspora (native speakers exist only in Israel or as isolated nostalgic practitioners): the bridge_pidginized reading forecloses native_generative in diaspora contexts, and the readings do not truly coexist — they are geographically partitioned. If diaspora native-speaker communities persist and maintain separate norms: the readings coexist within diaspora (two seat classes with different relationships to the kernel).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_empirical, empirical, 'Whether native-generative Hebrew is structurally available to diaspora or foreclosed by pidginization.').

omega_variable(
    liturgical_preservation_vs_pidginization,
    'Does institutional enforcement of the pidginized norm suppress or complement liturgical preservation? Can classical register and marketplace pidgin coexist as separate registers within the same institutional framework?',
    'Institutional audit: what proportion of Hebrew instruction time goes to classical texts vs. conversation/marketplace Hebrew? Do institutions teach both registers as distinct competences, or does pedagogical focus on pidgin squeeze out classical training? Do communities maintain separate ritual and secular Hebrew spaces, or has pidginization bled into liturgical contexts?',
    'If pidginization crowds out classical instruction: the constraint influences (degraded downstream pressure on) liturgical_preservation — classical register weakens. If both registers are maintained as distinct: the readings coexist within the same institutional space, and the constraint is compatibility-neutral. If pidginization actively suppresses classical teaching: the relationship intensifies toward foreclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_preservation_vs_pidginization, empirical, 'Whether pidginization and liturgical preservation can coexist as institutional registers or one displaces the other.').

omega_variable(
    native_speaker_identity_lock_mechanism,
    'What is the specific identity-fusion mechanism that locks native Hebrew speakers into the constraint despite alienation? Is it professional identity (career in Hebrew teaching/academia), relational identity (community role as cultural authority), ideological identity (Zionism or Jewish continuity commitment), or institutional identity (organization has become indistinguishable from Hebrew)?',
    'Sociological interview study of diaspora native speakers: why do they not exit to Hebrew-free professions or contexts? What costs (career, social, self-concept) do they report from pidginization? Do native speakers who exit Hebrew-centered institutions report liberation or loss of coherence? Longitudinal tracking of native-speaker engagement/alienation.',
    'Different fusion mechanisms suggest different breaking points: professional identity can shift with job change (tractable exit), ideological identity is more binding (requires value revision), institutional identity is most binding (organization dissolution or role redefinition). Identifying the lock type clarifies whether the victimhood is escapable or constitutional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(native_speaker_identity_lock_mechanism, empirical, 'The specific mechanism binding native speakers to a constraint that delegitimizes them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__bridge_pidginized, 1880, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_continuity__bridge_pidginized, theater_ratio, 1880, 0.25).
narrative_ontology:measurement_basis(hebr_tr_t1880, projected).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_continuity__bridge_pidginized, theater_ratio, 1920, 0.35).
narrative_ontology:measurement_basis(hebr_tr_t1920, observed).
narrative_ontology:measurement(hebr_tr_t1960, hebrew_continuity__bridge_pidginized, theater_ratio, 1960, 0.45).
narrative_ontology:measurement_basis(hebr_tr_t1960, observed).
narrative_ontology:measurement(hebr_tr_t1990, hebrew_continuity__bridge_pidginized, theater_ratio, 1990, 0.58).
narrative_ontology:measurement_basis(hebr_tr_t1990, observed).
narrative_ontology:measurement(hebr_tr_t2010, hebrew_continuity__bridge_pidginized, theater_ratio, 2010, 0.61).
narrative_ontology:measurement_basis(hebr_tr_t2010, observed).
narrative_ontology:measurement(hebr_tr_t2026, hebrew_continuity__bridge_pidginized, theater_ratio, 2026, 0.61).
narrative_ontology:measurement_basis(hebr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_continuity__bridge_pidginized, base_extractiveness, 1880, 0.35).
narrative_ontology:measurement_basis(hebr_be_t1880, projected).
narrative_ontology:measurement(hebr_be_t1920, hebrew_continuity__bridge_pidginized, base_extractiveness, 1920, 0.42).
narrative_ontology:measurement_basis(hebr_be_t1920, observed).
narrative_ontology:measurement(hebr_be_t1960, hebrew_continuity__bridge_pidginized, base_extractiveness, 1960, 0.48).
narrative_ontology:measurement_basis(hebr_be_t1960, observed).
narrative_ontology:measurement(hebr_be_t1990, hebrew_continuity__bridge_pidginized, base_extractiveness, 1990, 0.54).
narrative_ontology:measurement_basis(hebr_be_t1990, observed).
narrative_ontology:measurement(hebr_be_t2010, hebrew_continuity__bridge_pidginized, base_extractiveness, 2010, 0.57).
narrative_ontology:measurement_basis(hebr_be_t2010, observed).
narrative_ontology:measurement(hebr_be_t2026, hebrew_continuity__bridge_pidginized, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(hebr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_continuity__bridge_pidginized, suppression_requirement, 1880, 0.22).
narrative_ontology:measurement_basis(hebr_su_t1880, projected).
narrative_ontology:measurement(hebr_su_t1920, hebrew_continuity__bridge_pidginized, suppression_requirement, 1920, 0.32).
narrative_ontology:measurement_basis(hebr_su_t1920, observed).
narrative_ontology:measurement(hebr_su_t1960, hebrew_continuity__bridge_pidginized, suppression_requirement, 1960, 0.38).
narrative_ontology:measurement_basis(hebr_su_t1960, observed).
narrative_ontology:measurement(hebr_su_t1990, hebrew_continuity__bridge_pidginized, suppression_requirement, 1990, 0.41).
narrative_ontology:measurement_basis(hebr_su_t1990, observed).
narrative_ontology:measurement(hebr_su_t2010, hebrew_continuity__bridge_pidginized, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement_basis(hebr_su_t2010, observed).
narrative_ontology:measurement(hebr_su_t2026, hebrew_continuity__bridge_pidginized, suppression_requirement, 2026, 0.42).
narrative_ontology:measurement_basis(hebr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__bridge_pidginized, information_standard).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__bridge_pidginized, 0.12).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, diaspora_jewish_identity_legitimacy).
narrative_ontology:affects_constraint(hebrew_continuity__bridge_pidginized, yiddish_displacement_in_institutional_contexts).

% DUAL FORMULATION NOTE:
% The hebrew_continuity kernel contains three distinct constraints: (1) liturgical_preservation reads Hebrew through classical texts and ritual recitation (low ε, Mountain-type); (2) native_generative reads Hebrew through native Israeli speaker intuition and daily use (moderate ε, Rope-type); (3) bridge_pidginized (this story) reads Hebrew as diaspora contact language (this story: 0.58 ε, Tangled Rope). Each reading has different beneficiaries, victims, and enforcement mechanisms. The three constraints are linked via reading_relations in their respective cs_structure blocks. The bridge_pidginized reading influences both siblings: it pressures liturgical preservation by treating classical register as one available subset (not authoritative), and it forecloses native_generative for diaspora contexts by establishing that diaspora Hebrew need not meet native-speaker standards. The other two readings coexist with this one in global diaspora Jewry but occupy different institutional and community spaces.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__bridge_pidginized, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
