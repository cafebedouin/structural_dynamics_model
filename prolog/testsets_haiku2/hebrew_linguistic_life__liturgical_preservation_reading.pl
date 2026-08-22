% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__liturgical_preservation_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__liturgical_preservation_reading
 *   human_readable: Hebrew Linguistic Life via Liturgical Preservation (Sacred Transmission Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   Hebrew is declared alive when sacred texts are continuously recited,
 *   studied, and transmitted in an unbroken chain across diaspora
 *   communities, regardless of whether anyone speaks it as a native
 *   vernacular. This is one reading of a contested kernel: what makes a
 *   language 'alive'? The liturgical preservation reading asserts that
 *   transmission of fixed sacred texts through interpretive authority is
 *   sufficient; Hebrew never died because the chain never broke. This reading
 *   is held by rabbinic authorities, traditional diaspora communities, and
 *   some strands of conservative Jewish thought. The rival readings —
 *   native-generational (alive only when children are born into it) and
 *   marketplace-pidgin (alive when it functions as practical inter-communal
 *   medium) — contest the sufficiency of textual transmission and dispute
 *   whether the Ben-Yehuda project was 'revival' or 'creation.' The
 *   constraint story models THIS reading as a Tangled Rope: it coordinates
 *   diaspora identity and textual coherence (real coordination function) but
 *   operates by extracting authority from secular speakers and suppressing
 *   vernacular innovation (asymmetric extraction). The claim/metric gap is
 *   intentional: this reading CLAIMS to be a natural law (the texts keep the
 *   language alive organically) while the authored metrics describe actively
 *   enforced suppression and substantial extraction from secular speakers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.68).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.71).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Linguistic Life via Liturgical Preservation (Sacred Transmission Reading)").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '8a7ecabe-0225-4f1e-9033-ffadc2c35db7').
narrative_ontology:cs_kernel_codification('8a7ecabe-0225-4f1e-9033-ffadc2c35db7', fixed_text).
narrative_ontology:cs_authority_grounding('8a7ecabe-0225-4f1e-9033-ffadc2c35db7', lineage).
narrative_ontology:cs_interpretation_layer_present('8a7ecabe-0225-4f1e-9033-ffadc2c35db7').
narrative_ontology:cs_reading_relation('8a7ecabe-0225-4f1e-9033-ffadc2c35db7', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('8a7ecabe-0225-4f1e-9033-ffadc2c35db7', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('8a7ecabe-0225-4f1e-9033-ffadc2c35db7', foundational, textual_transmission_suffices_for_linguistic_life).
narrative_ontology:cs_axiom_status(textual_transmission_suffices_for_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('8a7ecabe-0225-4f1e-9033-ffadc2c35db7', textual_transmission_suffices_for_linguistic_life, deontological).
narrative_ontology:cs_axiom('8a7ecabe-0225-4f1e-9033-ffadc2c35db7', secondary, sacred_text_preserves_language_immutably).
narrative_ontology:cs_axiom_status(sacred_text_preserves_language_immutably, holdable).
narrative_ontology:cs_axiom_grounding('8a7ecabe-0225-4f1e-9033-ffadc2c35db7', sacred_text_preserves_language_immutably, theological).
narrative_ontology:cs_reference_frame('8a7ecabe-0225-4f1e-9033-ffadc2c35db7', unbroken_transmission_diaspora).
narrative_ontology:cs_drift_state('8a7ecabe-0225-4f1e-9033-ffadc2c35db7', post_state_formation_vernacularization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8a7ecabe-0225-4f1e-9033-ffadc2c35db7', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, religious_institutional_continuity).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_secular_vernacular_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_itself).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, zionist_political_movement).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, ben_yehuda_project_modernizers).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, sacred_language_immutability).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, text_supremacy_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_identity_through_liturgy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the canonical reading of sacred texts, determines which interpretations are legitimate, enforces textual purity through halacha and communal authority. Maintains the unbroken chain of transmission by controlling who may teach, interpret, and transmit the texts. Collects spiritual/institutional authority from being the sole custodian of tradition.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_interpretive_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Use Hebrew as a living, evolving, secular language in daily commerce, literature, politics, and mundane speech. From the liturgical preservation perspective, they are polluting the sacred language by naturalizing it, using it for profane purposes, and innovating neologisms. They cannot exit the language without cultural displacement, but the framing positions them as violating rather than participating.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_secular_vernacular_speakers, payer,
    organized, biographical, constrained, national).

% The corpus of textual tradition (Torah, Mishna, Gemara, liturgy) bears the cost of the constraint insofar as any living language bears cost from prescriptive freezing. The texts are read as unchanging, immutable, and complete — their generative capacity is arrested in service to the preservation claim. Each new speaker who encounters the texts does so under the frame of 'transmission of fixed meaning' rather than 'encounter with living tradition.'
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_itself, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition_itself).

% Benefit from the constraint as a preservation mechanism that keeps Jewish identity coherent across centuries and geographies: the liturgy remains recognizable in Warsaw, Baghdad, Cairo, and Jerusalem. They also bear costs when the constraint forbids the vernacular development that would let Hebrew become fully native to new speakers; children learn it as liturgical language and reserve vernacular speech for host-language or Yiddish/Ladino.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_jewish_communities, beneficiary,
    moderate, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, diaspora_jewish_communities, payer).

% Seek to restore Hebrew as a living vernacular by creating secular neologisms, new grammatical structures, and functional vocabulary for modern life. From the liturgical preservation frame, they are desecrators: they treat sacred language as raw material for profane use, they introduce words the tradition never sanctioned, they enable children to learn Hebrew without learning the texts. Their constraints lie in gaining acceptance from the institutional authority and in framing their work as 'revival' rather than 'creation.'
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, ben_yehuda_project_modernizers, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__liturgical_preservation_reading, ben_yehuda_project_modernizers, excluded).

% Benefits from Hebrew as a national language binding disparate diaspora communities into a single political project. Early Zionism often invoked the liturgical preservation frame ('we restore what was') to justify vernacularization. The institutional interest sits between the Ben-Yehuda modernizers (who need political support) and the rabbinic authority (who must not lose religious legitimacy). The political identity becomes entangled with the language identity in ways the constraint's beneficiaries exploit.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, zionist_political_movement, beneficiary,
    institutional, generational, mobile, national).

% Study the Hebrew case as a test of whether a language can remain alive through non-native transmission and liturgical preservation alone. They measure language vitality independently of the constraint's own framing and ask whether the constraint's definition of 'alive' matches empirical patterns of language use, acquisition, and functional scope.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, academic_comparative_linguists, observer,
    analytical, generational, analytical, global).

% Would argue that Hebrew was already a dead language by the Second Temple period, that any revival required secular innovation and practical use, and that the liturgical preservation reading misdiagnoses what kept it alive (written texts + religious identity) and what made it live again (political project + generational acquisition). They would propose an alternative constraint entirely but are not seated in the transmission authority.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, excluded_secular_hebrew_innovators, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, rabbinic_interpretive_authority).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Hebrew identity and coherence across diaspora communities separated by geography, language, and centuries through continuous recitation and study of the same sacred texts in the same language. The constraint solves the problem of how a community without native speakers, without geographic center, and with generations of host-language adaptation can remain bound by a common tradition and common tongue.
% TRANSFER_FUNCTION: Transfers the authority to define what Hebrew 'is' from living speakers to the rabbinic custodians of the texts. Speakers pay in constrained innovation, neologism suppression, and the requirement to learn the language through the texts rather than through living speech. The rabbinic authority collects institutional power and the ability to define linguistic legitimacy.
% ABSENT_VOICES: Secular Hebrew speakers who use the language for poetry, journalism, politics, and daily mundane speech are not represented in the transmission authority. They would argue that the constraint's definition of 'alive' systematically excludes them and mischaracterizes what actually kept Hebrew alive. The Zionist modernizers who sought vernacularization were technically present but structurally subordinated to the religious authority until the state was established.
% DISAPPEARANCE_RATIONALE: If this constraint — the requirement that Hebrew remain alive through liturgical preservation and transmission rather than through vernacular generational acquisition — disappeared, the language would either (a) be allowed to die as a living tongue and persist only as a scholarly/liturgical artifact (like Latin), or (b) be fully claimed by the secular modernizers as a living language freed from textual constraints, evolving rapidly through use. The religious meaning of Hebrew identity would have to reorganize around something other than the unbroken chain of textual transmission.
% FOUNDING_PROBLEM: After the Second Temple's destruction, Hebrew ceased to be a native mother tongue and existed primarily through written texts and liturgical recitation in diaspora communities speaking Aramaic, Greek, Arabic, Romance languages, Yiddish, and Ladino. The founding problem was: how does a language remain 'alive' when no one is born into it, when it is not used for daily life, and when the living linguistic community is fragmented across incompatible host languages?
% FOUNDING_PROBLEM_CORROBORATION: The liturgical preservation reading attests the founding problem is live and permanent: diaspora Jews will never all be concentrated in a native-speaker community, so the texts must forever carry the language's life. The secular modernizers and academic linguists attest that the founding problem was solved when Hebrew became a native language again in Palestine/Israel, and that continuing to frame it as 'preserved through texts' is now descriptively false and normatively an obstacle to its continued use. Legislative and cultural debates in Israel have sided with the modernizers, but rabbinic authority and diaspora religious communities attest the original problem is not solved — it is merely administratively relocated to diaspora enclaves where the texts remain the primary vector of transmission.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.68 reflects the constraint's dual function: it genuinely coordinates diaspora identity across centuries (real coordination value, ~0.3), but it does this by monopolizing authority over what Hebrew 'is' and suppressing secular innovation (extraction, ~0.38). The rabbinic authority benefits from controlling linguistic legitimacy; secular speakers bear the cost of constrained innovation. Suppression at 0.71 is high because the constraint's persistence depends on actively enforcing the equivalence 'Hebrew alive = texts transmitted' and preventing the alternative equation 'Hebrew alive = native speakers exist.' This enforcement is institutional (halacha, interpretive authority, communal pressure on secular users) and textual (the texts themselves are framed as complete, immutable, and the sole carrier of the language). Theater ratio at 0.42 is moderate because the constraint includes real coordination activity (genuine transmission of real texts, real study practices), but a growing share of its enforcement is spent defending the definition against the modernizers rather than maintaining the texts. The measurement series show extraction and suppression rising through time (t=0 to t=24) as secular Hebrew use expands and the rabbinic authority must work harder to maintain the transmission frame's dominance, then plateauing (t=24 to t=40) as the State of Israel institutionalizes secular Hebrew, forcing the rabbinic authority to accept a coexistence arrangement rather than monopoly.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority's seat, the constraint appears as successful preservation: the chain has not broken, the texts are alive because they are studied and transmitted. From the secular speaker's seat, it appears as exclusionary authority asserting a false monopoly over what counts as linguistic life. From the diaspora community's seat, it is both genuine coordination (keeps identity coherent) and limitation (prevents native acquisition). From the academic linguist's seat outside all three, the constraint is an empirically false definition of language vitality that misidentifies what kept Hebrew alive (political identity, written texts, institutional authority) and what made it live again (state formation, generational native acquisition, functional scope expansion). The constraint's persistence depends on maintaining the rabbinic authority's seat as the sole legitimate definer of 'alive,' suppressing the linguist's seat from adjudication and the secular speaker's seat from institutional representation.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic interpretive authority sits at d ≈ 0.15 (full beneficiary): it collects institutional power from the constraint, has arbitrage-grade exit options (can switch to other languages, other texts), and is institutional power. Secular Hebrew speakers sit at d ≈ 0.82 (high target): they pay through suppressed innovation and identity-locked constraints (Hebrew is their ethnic/national language; exit means cultural displacement), are organized but not institutional, and face accessibility-collapse near the texts themselves (they cannot simply speak a different language without severing connection to Jewish culture). Diaspora communities sit at d ≈ 0.51 (symmetric): they benefit genuinely from the coordination function (the texts hold them together) but also bear costs (their children cannot learn Hebrew natively without secular education, they must maintain two linguistic registers). Sacred tradition sits outside the standard directionality frame (non-agent, analytical power); it bears extraction in the sense that the constraint freezes its interpretation and arrests its generative capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy because it carries an internal contradiction: it claims to be describing a natural fact about what keeps languages alive (the texts themselves do the work), but it requires constant enforcement against an alternative (secular) use of the same texts. If the texts truly kept the language alive by transmission alone, no suppression would be needed; the texts would naturally persist. The fact that massive interpretive and institutional effort must be deployed to maintain the 'transmission = life' equation reveals that the constraint is not describing natural law but enforcing a particular reading of tradition against living alternatives. The Tangled Rope classification captures this: the real coordination function (diaspora identity through shared texts) is genuine; the extraction function (institutional authority monopoly over linguistic legitimacy) is enforced asymmetry. Mandatrophy would resolve when the rabbinic authority either (a) concedes that vernacular secular Hebrew is also 'alive' and the constraint fragments into multiple readings, or (b) redescribes its function explicitly as 'preserving sacred tradition through textual transmission' rather than 'keeping Hebrew alive,' shifting from a claim about language to a claim about continuity of religious practice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    texts_sufficient_for_language_life,
    'Is transmission of texts by non-native speakers sufficient for a language to be considered ''alive,'' or is native generational acquisition a necessary condition?',
    'Comparative historical linguistics: study languages where textual transmission persisted (Sanskrit, Biblical Hebrew pre-Zionism, Classical Latin) and ask whether they are described as ''alive'' or ''dead'' by the linguistic community; study the emergence of new native-speaker communities for preserved languages (Hebrew in Israel, Icelandic, Irish) and whether they required the addition of native acquisition to cross from ''dead/liturgical'' to ''alive/vernacular'' classification.',
    'If native acquisition is necessary, this reading''s constraint becomes contingent on the State of Israel''s creation and Hebrew-medium schooling — the texts alone did not and could not have kept it alive. If transmission alone suffices, the reading''s core claim holds but must explain why the modernizers'' addition of native speakers seemed necessary to the Zionist movement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(texts_sufficient_for_language_life, empirical, 'Whether textual transmission by non-speakers is sufficient for language vitality.').

omega_variable(
    interpretation_layer_suppression,
    'To what extent is the rabbinic interpretive authority suppressing innovation through genuine preservation needs (maintaining textual coherence, preventing corruption) versus through institutional interest (maintaining monopoly on legitimate speech)?',
    'Historical analysis of which neologisms and innovations were rejected and on what grounds: rejected because they corrupted meaning vs. rejected because they threatened institutional authority. Examine cases where neologisms were adopted despite opposition (how institutional authority adapted) and cases where they were permanently forbidden (why the monopoly held).',
    'If most suppression is genuine preservation, the theater ratio is lower and the constraint is more Rope-like. If most suppression is institutional preservation-of-authority, the theater ratio is higher, the extraction is more visible, and the constraint is more Snare-like. The measurement series show theater rising over time, suggesting the institutional half is becoming more prominent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretation_layer_suppression, empirical, 'Whether suppression serves textual preservation or authority monopoly.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the liturgical-preservation reading logically foreclose the native-generational reading, or do both readings remain structurally coherent?',
    'Formal analysis: can a single framework (e.g., Jewish law, comparative linguistics, historical sociology) hold both ''transmission by texts alone suffices'' AND ''native generational acquisition is necessary'' as simultaneous truths? Or does accepting one require rejecting the other as a matter of logical necessity?',
    'If they genuinely foreclose each other, the kernel contest is irreducible — one reading''s acceptance requires the other''s rejection as a matter of core premise, not mere disagreement. If they coexist (both are true under different frames or at different historical periods), the kernel is decomposable and the two readings can be held simultaneously by different parties or sequentially by the same tradition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether this reading''s core claim logically eliminates the native-generational reading''s core claim.').

omega_variable(
    identity_lock_mechanism_for_secular_speakers,
    'Is Hebrew identity-locked for secular speakers primarily through institutional suppression (rabbinic authority, communal pressure), through cognitive identity-fusion (Hebrew = Jewishness), through structural dependency (Hebrew is necessary for access to texts and culture), or through some combination?',
    'Post-exit trajectory analysis: secular speakers who leave Hebrew (adopt only host language, raise children in host language only) and trace whether their Jewish identity remains, whether the suppression continues extraterritorially, whether they report experiencing the suppression as external institutional force or as internalized self-constraint. Compare across diaspora communities with different institutional authority presence.',
    'If primarily institutional, the suppression can be externally removed and the exit cost drops. If primarily identity-fusion, suppression persists after institutional removal — the speaker carries the constraint with them. If primarily structural, breaking the constraint requires reorganizing access to Jewish cultural materials. The exit_options=identity_locked attribution depends on which mechanism dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_secular_speakers, empirical, 'Whether secular Hebrew speakers'' identity-lock is structural, institutional, or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(hebr_tr_t0, projected).
narrative_ontology:measurement(hebr_tr_t8, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement_basis(hebr_tr_t8, observed).
narrative_ontology:measurement(hebr_tr_t16, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(hebr_tr_t16, observed).
narrative_ontology:measurement(hebr_tr_t24, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(hebr_tr_t24, observed).
narrative_ontology:measurement(hebr_tr_t32, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(hebr_tr_t32, observed).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(hebr_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(hebr_be_t0, projected).
narrative_ontology:measurement(hebr_be_t8, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(hebr_be_t8, observed).
narrative_ontology:measurement(hebr_be_t16, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement_basis(hebr_be_t16, observed).
narrative_ontology:measurement(hebr_be_t24, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(hebr_be_t24, observed).
narrative_ontology:measurement(hebr_be_t32, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(hebr_be_t32, observed).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(hebr_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(hebr_su_t0, projected).
narrative_ontology:measurement(hebr_su_t8, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(hebr_su_t8, observed).
narrative_ontology:measurement(hebr_su_t16, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(hebr_su_t16, observed).
narrative_ontology:measurement(hebr_su_t24, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(hebr_su_t24, observed).
narrative_ontology:measurement(hebr_su_t32, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 32, 0.7).
narrative_ontology:measurement_basis(hebr_su_t32, observed).
narrative_ontology:measurement(hebr_su_t40, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(hebr_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__liturgical_preservation_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the hebrew_linguistic_life kernel decomposition. Three readings emit three distinct constraints with non-overlapping victim sets and different ε values. The liturgical_preservation_reading (this file) asserts Hebrew never died; the native_generational_reading asserts it requires native speakers; the marketplace_pidgin_reading asserts it requires functional use. Each reading has its own beneficiary/victim structure, authority grounding, and structural classification. Network links model that these are not independent constraints but readings of the same contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_linguistic_life__liturgical_preservation_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
