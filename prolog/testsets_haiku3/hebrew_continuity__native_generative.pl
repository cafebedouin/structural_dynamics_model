% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__native_generative, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Native Generative Hebrew Requirement for Language Continuity
 *   domain: sociolinguistic/cultural/commitment-system
 *
 * SUMMARY:
 *   Hebrew is a language that died in everyday use and was revived in the
 *   late 19th and 20th centuries as the national language of Israel. The
 *   constraint analyzed here is the native-generative reading: the claim that
 *   Hebrew 'lives' only through native-speaker intuition and daily generative
 *   use by children and native-speaker communities. This reading is one of
 *   three competing framings of Hebrew continuity (the other two are
 *   liturgical preservation and bridge pidginization). The native-generative
 *   reading vindicates native-speaker authority and delegitimizes alternative
 *   forms of Hebrew practice. It is genuinely a coordinate mechanism — it
 *   does solve the real problem of establishing a living lingua franca — but
 *   it simultaneously extracts from liturgical communities and diaspora
 *   practitioners by marking their Hebrew as 'dead' or 'inauthentic.' The
 *   constraint's persistence depends on active institutional enforcement
 *   (school curriculum, media standards, academic credentialing) that
 *   excludes alternative voices and alternative ways of counting as a Hebrew
 *   speaker.
 *
 * KEY AGENTS:
 *   - Native-speaker communities in Israel and diaspora immersion contexts — beneficiaries, defining the standard
 *   - Liturgical-Hebrew communities (religious institutions, prayer communities) — victims, delegitimized as 'dead language' practitioners
 *   - Diaspora Hebrew learners — victims, identity-locked, excluded from native-speaker authority
 *   - Academy of the Hebrew Language, Ministry of Education, media institutions — agenda setters, enforce the standard
 *   - Linguistic researchers — observers, measure constraint effects and compare to other language revivals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.68).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.71).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Native Generative Hebrew Requirement for Language Continuity").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistic/cultural/commitment-system").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '24acdd45-a885-40f2-9176-b25ae9478aa9').
narrative_ontology:cs_kernel_codification('24acdd45-a885-40f2-9176-b25ae9478aa9', fixed_text).
narrative_ontology:cs_authority_grounding('24acdd45-a885-40f2-9176-b25ae9478aa9', extraction).
narrative_ontology:cs_interpretation_layer_present('24acdd45-a885-40f2-9176-b25ae9478aa9').
narrative_ontology:cs_reading_relation('24acdd45-a885-40f2-9176-b25ae9478aa9', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('24acdd45-a885-40f2-9176-b25ae9478aa9', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('24acdd45-a885-40f2-9176-b25ae9478aa9', foundational, hebrew_lives_through_native_generative_use).
narrative_ontology:cs_axiom_status(hebrew_lives_through_native_generative_use, holdable).
narrative_ontology:cs_axiom_grounding('24acdd45-a885-40f2-9176-b25ae9478aa9', hebrew_lives_through_native_generative_use, empirically_contingent).
narrative_ontology:cs_axiom('24acdd45-a885-40f2-9176-b25ae9478aa9', foundational, native_speaker_intuition_irreplaceable_authority).
narrative_ontology:cs_axiom_status(native_speaker_intuition_irreplaceable_authority, holdable).
narrative_ontology:cs_axiom_grounding('24acdd45-a885-40f2-9176-b25ae9478aa9', native_speaker_intuition_irreplaceable_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('24acdd45-a885-40f2-9176-b25ae9478aa9', native_speaker_hebrew_standard).
narrative_ontology:cs_drift_state('24acdd45-a885-40f2-9176-b25ae9478aa9', contemporary_pluralistic_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('24acdd45-a885-40f2-9176-b25ae9478aa9', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, native_speaker_communities).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, hegemonic_israeli_hebrew_standard).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_hebrew_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_hebrew_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_hebrew_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Native Hebrew speakers in Israel and diaspora communities with native-speaker childhood immersion are positioned as the linguistic authority. Their intuitions about grammar, vocabulary, and acceptability define correctness. They benefit from institutional resources optimized around native-acquisition pedagogy, media representation that validates native-speaker norms, and academic credentialing that requires native-speaker authority. They can emigrate or shift to other languages, but generational and cultural attachment to Hebrew is high.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, native_speaker_communities, beneficiary,
    organized, generational, mobile, national).

% Orthodox, Conservative, Reform, and other Jewish religious communities practice Hebrew primarily through prayer, liturgy, and textual scholarship. The native-generative constraint systematically delegitimizes this practice: their Hebrew is called 'dead,' 'artificial,' or 'learned' rather than 'authentic.' Educational and academic resources are redirected toward native-speaker acquisition; their pedagogical pathways are underfunded and treated as non-scholarly. They remain committed to Hebrew through religious obligation (halacha, tradition) even as the constraint extracts legitimacy and resources.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, liturgical_hebrew_communities, payer,
    moderate, generational, constrained, global).

% Jews outside Israel who learn Hebrew for cultural, religious, or intellectual reasons face permanent structural exclusion under the native-generative standard. No amount of study or fluency achieves 'native speaker' status (defined by childhood immersion in a native-speaker environment). Their commitment to Hebrew is locked into their Jewish identity; exiting Hebrew means abandoning cultural continuity. The constraint extracts by ensuring their fluency is always marked as 'acquired,' their participation is conditional, and their claims to linguistic authority are preempted by the native-speaker standard.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_hebrew_learners, payer,
    powerless, biographical, identity_locked, global).

% The Academy of the Hebrew Language, founded in 1953, is the official authority for Hebrew language standardization in Israel. It sets the standard for correctness, credentials language teachers, and publishes authoritative dictionaries and style guides. It justifies the native-generative standard as protecting the language's vitality and authenticity but derives institutional authority and budget from managing the boundary between native and non-native Hebrew. It could change the standard, but doing so would relinquish its gatekeeping authority.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, academy_of_hebrew_language, agenda_setter,
    institutional, generational, analytical, national).

% The Israeli Ministry of Education designs and implements Hebrew-language curriculum for schools and teaching credentials. It enforces the native-speaker standard by requiring teachers to meet native-competence benchmarks, by emphasizing natural acquisition over explicit instruction, and by marginalizing alternative approaches (liturgical study, historical perspective, comparative Hebrew studies). It justifies these policies as maximizing language vitality but derives institutional authority from managing who counts as a qualified Hebrew teacher.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, ministry_of_education, agenda_setter,
    institutional, generational, analytical, national).

% Israeli television, radio, and publishing institutions enforce native-speaker norms through their choice of broadcasters, script approval processes, and representation of Hebrew speakers. They present native-speaker Hebrew as the model and marginalize or caricature non-native speech. This enforcement is justified as protecting media quality but reinforces the institutional standard that native-speaker intuition is the only authentic linguistic authority.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_media_institutions, agenda_setter,
    institutional, generational, analytical, national).

% Sociolinguists, historical linguists, and language revival scholars can observe the native-generative constraint from outside the beneficiary frame. They measure language vitality through corpus studies, functional domains, child acquisition rates, and transmission across generations. They provide comparative evidence from other language revivals (Irish, Basque, Maori) and documentation that language can remain functionally alive through multiple channels (not only native-speaker intuition). Their research can serve as a check on institutional claims about what keeps Hebrew alive.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, linguistic_researchers, observer,
    institutional, generational, analytical, global).

% Communities that practice Hebrew through Yiddish-Hebrew bilingualism, Judeo-Arabic contexts, and other pre-modern multilingual traditions are excluded from the conversation about what Hebrew 'is.' These communities would argue for a polyglot definition of Hebrew and for recognizing Hebrew-in-translation and code-switching as forms of legitimate practice. Their exclusion is maintained by institutional devaluation of multilingualism and by the normalization of monolingual native-speaker standards. They remain present in historical documents and some diaspora communities but are treated as historical artifacts rather than live alternatives.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, alternative_hebrew_traditions, excluded,
    moderate, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__native_generative, academy_of_hebrew_language).
narrative_ontology:fixing_cost_class(hebrew_continuity__native_generative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, standardized Hebrew that can serve as a national language and lingua franca for Israeli society. Native-speaker intuition as the standard allows speakers to coordinate on grammar and vocabulary without explicit rules; children acquire the language naturally through immersion, which is more efficient than formal instruction. The constraint solves the historical problem: how to transform Hebrew from a liturgical language into a living language of daily communication.
% TRANSFER_FUNCTION: Moves epistemic authority, institutional resources, and cultural legitimacy from liturgical-Hebrew and diaspora communities to native-speaker communities and the institutions that represent native-speaker norms. Resources flow toward native-acquisition pedagogy; away from textual scholarship, liturgical instruction, and diaspora Hebrew education. Status and legitimacy are transferred from 'dead-language study' to 'living-language use.' Native speakers gain authority to define correctness; non-native speakers lose the ability to claim authentic participation.
% ABSENT_VOICES: Liturgical-Hebrew scholars, religious leaders, and diaspora communities that practice Hebrew through prayer and study would argue that the native-generative standard falsely equates language vitality with childhood acquisition and that it systematically devalues centuries of textual transmission, ritual use, and multilingual Jewish practice. Yiddish-speaking communities and speakers of other Jewish languages would object to the marginalization of multilingualism and the imposition of monolingual native-speaker norms. These voices are partially present in religious institutions and diaspora communities but are excluded from academic credentialing, media representation, and institutional language policy.
% DISAPPEARANCE_RATIONALE: If the native-generative constraint disappeared — if institutions granted equal legitimacy to liturgical Hebrew, diaspora practice, and native-speaker intuition — the language ecology would reorganize: resources would redistribute toward textual scholarship and liturgical pedagogy; diaspora communities would recover legitimacy as Hebrew practitioners; the definition of Hebrew itself would pluralize. Israeli national identity, currently stabilized by native-speaker consensus, would fracture into competing framings. The institutional authority structures (Academy, Ministry) that depend on managing the native/non-native boundary would be destabilized.
% FOUNDING_PROBLEM: In the early 20th century, Hebrew was primarily a liturgical and textual language with no community of native speakers. The challenge was to transform it into a language of daily communication and child-rearing in a Jewish national homeland. The native-generative constraint solved this by reframing Hebrew vitality in terms of native-speaker acquisition, which motivated parents to raise children in Hebrew and created a self-reinforcing cycle of intergenerational transmission.
% FOUNDING_PROBLEM_CORROBORATION: Israeli institutions (Academy of the Hebrew Language, Ministry of Education, and mainstream media) attest that the founding problem remains live: maintaining native-speaker competence is essential to Hebrew's continued vitality and to national identity. Linguistic researchers and diaspora communities attest that the founding problem is substantially solved: Hebrew is now acquired natively by Israeli children, it is documented in extensive speech corpora, and it functions as the national language. The contestation is documented in sociolinguistic literature (Joshua Fishman, Bernard Spolsky, Chaim Rabin) and in ongoing debates within Jewish communities about whether Hebrew's existence depends on native speakers or whether liturgical, textual, and diaspora traditions also count as forms of Hebrew continuity.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__native_generative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The native-generative reading claims that Hebrew's 'real' existence depends on native-speaker intuition and daily generative use. This is genuinely a coordination mechanism: it provides a unified standard that speakers can coordinate on without explicit rules, and it creates strong incentives for intergenerational transmission (parents raise children in Hebrew to give them 'native' status). BUT it is also substantially extractive from the perspective of non-native communities. Extractiveness rises over time (0.35→0.68 across the 100-unit interval) as institutional resource allocation increasingly privileges native-acquisition pathways and as the native-speaker authority claim hardens in academic and policy domains. Suppression rises correspondingly (0.42→0.71) as institutional gatekeeping intensifies — alternative standards are actively excluded from credentialing, publishing, and media representation. Theater ratio rises more slowly (0.18→0.42) because the native-speaker intuition claim genuinely tracks linguistic behavior in Israel; the performative element grows as diaspora communities are increasingly required to perform 'native' competence they cannot achieve by definition. The measurement series is authored on a single shared time grid; every metric is measured at every time point (0, 15, 30, 50, 75, 100).
 *
 * PERSPECTIVAL GAP:
 *   The native-speaker communities and institutional actors should compute very differently from the victim communities. From the native-speaker seat, the constraint is a rare success story: it genuinely created a living language from liturgical tradition, children acquire it naturally, and native-speaker intuition is the most reliable guide to authentic grammar. From the liturgical-community seat, the same constraint is an exclusionary apparatus: it dismisses centuries of textual transmission, delegitimizes prayer-based practice, and forces diaspora learners into an impossible position (achieve native intuition you cannot have by definition of non-nativeness). The institutional seat benefits from managing the boundary itself — schools, academies, and credentialing bodies derive authority from deciding who counts as a real Hebrew speaker. The engine computes this divergence from the structural data: beneficiary status with mobile/organized power on one side, victim status with constrained/identity-locked exit on the other.
 *
 * DIRECTIONALITY LOGIC:
 *   Native-speaker communities and the Israeli institutional structure are the beneficiaries (d approaches 0.0): they define the standard, collect institutional resources, and derive authority from native-speaker intuition claims. Their exit options are mobile (emigration is possible, language shift is possible, but generational and cultural attachment makes these costs high). Liturgical communities are victims (d approaches 1.0): they bear delegitimization costs, watch resources drain from their pedagogical pathways, and experience their centuries-old tradition as devalued. Their exit is constrained (they remain committed to Hebrew through religious law even as the constraint extracts status and resources). Diaspora practitioners are victims with high identity-lock (d near 1.0): they cannot exit Hebrew without abandoning Jewish cultural identity, yet the native-generative standard ensures they can never achieve 'authentic' status. Institutional actors are agenda-setters with analytical exit (they could change the standard, but doing so would require acknowledging the constraint's extractive dimension and relinquishing authority over language definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The native-generative constraint blocks the mandatrophy diagnosis: it genuinely solved the founding problem (restoring Hebrew to daily use, creating a native-speaker base, enabling intergenerational transmission). The problem it solved remains contested — some argue the problem is dead (Hebrew is alive), others argue the problem is still live (native status is not sustainable for diaspora communities, the constraint excludes legitimate Hebrew practice). The constraint persists because it provides genuine coordination value AND because institutional actors benefit from the boundary work it requires. Mandatrophy would apply if the native-generative standard persisted without solving any coordination problem; here, the problem it solved is real, but the coordination function and the extraction function are inseparably linked. The constraint cannot be cleaned up into 'pure coordination' because the definition of Hebrew itself — what counts as living vs. dead — is the terrain where coordination and extraction meet.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the Hebrew-continuity kernel is structurally true: native generative, liturgical preservation, or bridge pidginization?',
    'Empirical investigation of where Hebrew is actually spoken, acquired by children, and functionally alive in contemporary communities. Comparison with other language revivals and contact languages. Examination of institutional gatekeeping that privileges native-speaker claims over other evidence of vitality.',
    'If the native-generative reading is false (if Hebrew is functionally alive in liturgical and diaspora contexts without native-speaker intuition), the constraint reclassifies from tangled_rope (real coordination + asymmetric extraction) to pure snare (extraction dressed as coordination). If the reading is true, the classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, empirical, 'Whether the native-generative standard accurately identifies what keeps Hebrew alive, or whether it is a cover story for institutional gatekeeping.').

omega_variable(
    authority_naturalization,
    'Is native-speaker intuition an irreducible linguistic resource (genuinely authoritative), or is it a sociolinguistic artifact of the institutional standard itself?',
    'Historical-comparative study of language standardization processes. Examination of what speakers intuit before formal education, and how education reshapes intuition. Study of communities that restored languages without native-speaker criteria.',
    'If native-speaker intuition is genuinely irreplaceable, the beneficiary claim of native-speaker communities is warranted and the constraint provides real coordination. If it is largely constructed through institutional education and socialization, the authority claim is overstated and the constraint''s extraction burden increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_naturalization, conceptual, 'Whether the constraint''s grounding claim — that native intuition is the irreplaceable linguistic authority — is natural or constructed.').

omega_variable(
    identity_lock_mechanism,
    'Is diaspora Hebrew-learner identity-lock structural (genuine inability to exit the Hebrew-learner category) or partly internalized?',
    'Post-institution ethnography: when diaspora learners move to Hebrew-speaking communities and are embedded in native-speaker environments from adulthood onward, do they report reduced identity-lock and experience of authenticity? Do second-generation diaspora children acquire full native-speaker status?',
    'If lock is structural, the constraint is trapped-exit for diaspora communities and the extraction is severe. If lock is partly internalized, some agents could experience reduced suppression through institutional devaluation. If diaspora-born children acquire native status at institutional recognition, the exclusion is generational, not permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether the non-native status assigned to diaspora learners persists empirically or is an artifact of institutional labeling.').

omega_variable(
    institutional_alternative_possibility,
    'Could institutions recognize Hebrew as alive through liturgical continuity or diaspora contact-language use WITHOUT dismantling the native-speaker standard?',
    'Institutional pluralism case studies: jurisdictions that grant equal standing to multiple language standards or definitions of language vitality. Examination of whether competing standards can coexist or whether institutional authority requires exclusive gatekeeping.',
    'If pluralism is possible, the constraint could transform from tangled_rope to rope (real coordination with symmetric participation). If institutional authority requires exclusive gatekeeping, the extraction is structurally necessary to the constraint''s operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_alternative_possibility, conceptual, 'Whether the native-generative standard''s dominance is a necessary feature of language coordination or an artifact of institutional monopoly over language definition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.18).
narrative_ontology:measurement(hebr_tr_t15, hebrew_continuity__native_generative, theater_ratio, 15, 0.22).
narrative_ontology:measurement(hebr_tr_t30, hebrew_continuity__native_generative, theater_ratio, 30, 0.28).
narrative_ontology:measurement(hebr_tr_t50, hebrew_continuity__native_generative, theater_ratio, 50, 0.36).
narrative_ontology:measurement(hebr_tr_t75, hebrew_continuity__native_generative, theater_ratio, 75, 0.4).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__native_generative, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hebr_be_t15, hebrew_continuity__native_generative, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(hebr_be_t30, hebrew_continuity__native_generative, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(hebr_be_t50, hebrew_continuity__native_generative, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(hebr_be_t75, hebrew_continuity__native_generative, base_extractiveness, 75, 0.66).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__native_generative, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(hebr_su_t15, hebrew_continuity__native_generative, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(hebr_su_t30, hebrew_continuity__native_generative, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(hebr_su_t50, hebrew_continuity__native_generative, suppression_requirement, 50, 0.64).
narrative_ontology:measurement(hebr_su_t75, hebrew_continuity__native_generative, suppression_requirement, 75, 0.69).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__native_generative, suppression_requirement, 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__native_generative, 0.12).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% The hebrew_continuity kernel decomposes into three structurally distinct constraints, each instantiating a different reading of what it means for Hebrew to 'live.' The native-generative reading (this story) defines Hebrew as alive through native-speaker intuition and daily generative use; it extracts from liturgical and diaspora communities by delegitimizing their practice. The liturgical-preservation reading defines Hebrew as alive through textual transmission and ritual recitation; it frames the constraint as coordinating religious communities around sacred texts. The bridge-pidginized reading defines Hebrew as alive through cross-community contact and pragmatic communication; it frames the constraint as enabling diaspora coordination. These are not alternative measurements of a single constraint — they have incompatible ε values and incompatible victim/beneficiary structures. Each instantiation of the kernel produces different classification results: native-generative computes as tangled_rope from the native-speaker seat and as snare from the liturgical-victim seat; liturgical-preservation computes differently; bridge-pidginized again differently. The three stories are linked by structural interdependence: the dominance of the native-generative reading constrains the institutional space available to the other readings. Affects_constraints declares this dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__native_generative, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
