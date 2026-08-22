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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Hebrew Language Revival as Native Generational Acquisition
 *   domain: sociolinguistics/nationalism/religious_studies
 *
 * SUMMARY:
 *   The native-generational reading of Hebrew linguistic life declares a
 *   language alive only when children acquire it as their mother tongue and
 *   use it for all daily functions including secular mundane speech. This
 *   reading emerged during the Hebrew revival movement (late 19th–early 20th
 *   century) as a way to construct unified national identity from diaspora
 *   communities speaking Yiddish, Ladino, and Arabic. Under this criterion,
 *   Yiddish and Ladino—despite centuries of living use—were declared
 *   linguistically dead, and their speakers were coerced into linguistic
 *   assimilation. The constraint coordinates national identity formation
 *   around a single standardized language while extracting from linguistic
 *   minorities the cost of abandoning their heritage vernaculars. The
 *   measurement series traces the hardening of enforcement infrastructure and
 *   rising extraction from 1880 (revival movement onset) to 2000 (endpoint of
 *   state-institutionalized Hebrew monolingualism). Theater ratio rises as
 *   the constraint's dual function becomes clearer: initially a genuine
 *   revival project, it increasingly functions to suppress minority languages
 *   while performing national authenticity.
 *
 * KEY AGENTS:
 *   - hebrew_revival_movement: agenda-setter (late 19th century onward) — defines the criterion of linguistic life and promotes Hebrew acquisition
 *   - jewish_national_project: institutional beneficiary — uses the constraint as legitimation for statehood and unified national identity
 *   - yiddish_speakers: victims (pressure to assimilate; linguistic delegitimation)
 *   - ladino_speakers: victims (same pressures; Southern diaspora)
 *   - arabic_speaking_jewish_communities: victims (displaced; geographic and ideological pressure)
 *   - academic_linguists: external observers — contest the universality of the native-generational criterion
 *   - religious_authorities: dual position — some benefit from national revival, others hold competing readings (liturgical_preservation_reading)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__native_generational_reading, 0.72).
domain_priors:suppression_score(hebrew_linguistic_life__native_generational_reading, 0.78).
domain_priors:theater_ratio(hebrew_linguistic_life__native_generational_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(hebrew_linguistic_life__native_generational_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__native_generational_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__native_generational_reading, "Hebrew Language Revival as Native Generational Acquisition").
narrative_ontology:topic_domain(hebrew_linguistic_life__native_generational_reading, "sociolinguistics/nationalism/religious_studies").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__native_generational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__native_generational_reading, 'cbd9036b-0116-4b72-ab17-a74cbcef9702').
narrative_ontology:cs_kernel_codification('cbd9036b-0116-4b72-ab17-a74cbcef9702', fixed_text).
narrative_ontology:cs_authority_grounding('cbd9036b-0116-4b72-ab17-a74cbcef9702', lineage).
narrative_ontology:cs_interpretation_layer_present('cbd9036b-0116-4b72-ab17-a74cbcef9702').
narrative_ontology:cs_reading_relation('cbd9036b-0116-4b72-ab17-a74cbcef9702', hebrew_linguistic_life__liturgical_preservation_reading, forecloses).
narrative_ontology:cs_reading_relation('cbd9036b-0116-4b72-ab17-a74cbcef9702', hebrew_linguistic_life__marketplace_pidgin_reading, coexists_with).
narrative_ontology:cs_axiom('cbd9036b-0116-4b72-ab17-a74cbcef9702', foundational, nativeness_required_for_linguistic_aliveness).
narrative_ontology:cs_axiom_status(nativeness_required_for_linguistic_aliveness, holdable).
narrative_ontology:cs_axiom_grounding('cbd9036b-0116-4b72-ab17-a74cbcef9702', nativeness_required_for_linguistic_aliveness, empirically_contingent).
narrative_ontology:cs_axiom('cbd9036b-0116-4b72-ab17-a74cbcef9702', foundational, secular_daily_use_marks_authentic_language).
narrative_ontology:cs_axiom_status(secular_daily_use_marks_authentic_language, holdable).
narrative_ontology:cs_axiom_grounding('cbd9036b-0116-4b72-ab17-a74cbcef9702', secular_daily_use_marks_authentic_language, conventional).
narrative_ontology:cs_reference_frame('cbd9036b-0116-4b72-ab17-a74cbcef9702', diaspora_vernacular_fragmentation_requiring_unified_state_language).
narrative_ontology:cs_drift_state('cbd9036b-0116-4b72-ab17-a74cbcef9702', contemporary_globalized_multilingual_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cbd9036b-0116-4b72-ab17-a74cbcef9702', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, hebrew_revival_movement).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, jewish_national_project).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, ladino_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jewish_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__native_generational_reading, religious_authorities).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, linguistic_nationalism_doctrine).
narrative_ontology:constraint_vindicates(hebrew_linguistic_life__native_generational_reading, ethno_linguistic_identity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Intellectuals, educators, and Zionist organizers who champion Hebrew as the sole authentic vehicle of Jewish national identity. They set curriculum standards, promote Hebrew-language instruction in schools, establish Hebrew-language academies and publishing houses, and articulate the criterion that a language is alive only through native generational transmission. They directly benefit from the ideological authority this reading grants them and from control over language standardization.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, hebrew_revival_movement, agenda_setter,
    organized, generational, mobile, national).

% Ashkenazi Jewish communities, particularly in Eastern Europe and immigrant diaspora, whose primary vernacular is Yiddish. Under the native-generational criterion, Yiddish is declared linguistically dead regardless of its actual vitality as a spoken language. They face systematic pressure to abandon Yiddish in favor of Hebrew, to identify Yiddish-speaking as backwards or insufficiently national, and to raise children in Hebrew instead. Many voluntarily comply with the ideological reframing; others resist but see Yiddish marginalized in schools and public institutions.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, yiddish_speakers, payer,
    moderate, biographical, constrained, regional).

% Sephardic Jewish communities whose vernacular is Ladino (Judeo-Spanish). Under this reading, Ladino is similarly declared linguistically dead and its speakers face pressure to adopt Hebrew as marker of authentic Jewish identity. Ladino speakers are simultaneously excluded from the conversation about linguistic authenticity: the native-generational criterion is framed as universal but applies selectively to validate Hebrew and delegitimize competing vernaculars.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, ladino_speakers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, ladino_speakers, excluded).

% Mizrahi and other Arabic-speaking Jewish communities in the Middle East and North Africa. Under the native-generational reading, their native Arabic is not recognized as a legitimate marker of Jewishness; pressure mounts to adopt Hebrew as the sole vehicle of Jewish identity, severing them from centuries of Arabic-language Jewish culture and integration with Arab neighbors. Their exit options narrow as geographic displacement and ideological pressure converge.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jewish_communities, payer,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, arabic_speaking_jewish_communities, excluded).

% The institutional infrastructure of Jewish statehood and nation-building, particularly in Palestine/Israel. The native-generational criterion for linguistic life directly serves state-building: it provides a unified, standardized national language disconnected from diaspora vernaculars, enabling ideological consolidation and territorial sovereignty claims. The constraint benefits the state's administrative, educational, and cultural apparatus.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, jewish_national_project, beneficiary,
    institutional, generational, mobile, national).

% Schools, curricula, and state language policy apparatus that enforce Hebrew as the medium of instruction and the standard for nativeness. Administrators and teachers implement the native-generational criterion by requiring Hebrew fluency, marginalizing or prohibiting other languages in classroom settings, and teaching the ideological narrative that linguistic authenticity flows from mother-tongue nativeness.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, israeli_educational_system, agenda_setter,
    institutional, generational, analytical, national).

% Children and families whose mother tongue is not Hebrew but who are subjects of the state education and policy regime. They would argue for multilingualism, recognition of minority languages, and a criterion of linguistic life that does not require abandonment of heritage speech. Their voices are structurally excluded from the conversation that defines what makes a language alive.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, linguistic_minorities_within_state, excluded,
    powerless, immediate, trapped, local).

% Scholars in sociolinguistics, historical linguistics, and language revitalization who evaluate the claim that a language is alive only through native generational transmission. They measure language vitality using empirical criteria (speaker population, functional domains, intergenerational transmission, community attitudes) and recognize multiple pathways to linguistic persistence. Their external assessment challenges the native-generational reading's universality.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, academic_linguists, observer,
    powerful, generational, arbitrage, global).

% Rabbinical and religious institutional authorities who hold competing readings of what keeps Hebrew alive. Some endorse the native-generational reading as consistent with national revival; others maintain that continuous liturgical study and transmission (the liturgical_preservation_reading) sustains Hebrew's vitality independently of vernacular nativeness. They occupy a dual position: beneficiaries of the constraint's validation of religious tradition, but also advocates for alternative readings.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__native_generational_reading, religious_authorities, observer,
    powerful, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(hebrew_linguistic_life__native_generational_reading, religious_authorities, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__native_generational_reading, jewish_national_project).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__native_generational_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, standardized, nationally unified language that binds a dispersed people into a shared political and cultural identity. Solves the coordination problem of how to construct a nation-state from diaspora communities speaking multiple vernaculars (Yiddish, Ladino, Arabic, etc.). By declaring Hebrew the sole authentic linguistic vehicle and the marker of nativeness, it provides a single focal point for identity and institutional coherence.
% TRANSFER_FUNCTION: Moves linguistic authority, educational access, cultural prestige, and nationalist legitimacy FROM Yiddish/Ladino/Arabic-speaking communities TO Hebrew speakers and Hebrew-language institutions. Children in multilingual families are pressured to acquire Hebrew as primary language and to view other languages as inferior or inauthentic. The constraint extracts from linguistic minorities the cost of assimilation and linguistic abandonment.
% ABSENT_VOICES: Linguists who emphasize functional multilingualism, communities with living non-Hebrew vernaculars (Yiddish, Ladino, Arabic speakers), and advocates for linguistic diversity and minority language preservation are excluded or marginalized. They would argue that languages can remain alive through multiple pathways and that enforcing Hebrew nativeness destroys living linguistic ecosystems. Religious authorities holding the liturgical_preservation_reading are also partially excluded from defining linguistic authenticity under this reading.
% DISAPPEARANCE_RATIONALE: If the native-generational criterion disappeared, Hebrew might cease to be the universal medium of state administration and education; Yiddish, Ladino, and Arabic would regain legitimacy as markers of authentic Jewish identity; multilingualism would reshape national identity formation; and the ideological justification for suppressing minority languages would dissolve. The linguistic landscape would reorganize around multiple living languages rather than Hebrew monolingualism.
% FOUNDING_PROBLEM: During the 70 CE to 1880 CE dormancy period, Hebrew existed primarily in liturgical and scholarly contexts, not as a vernacular mother tongue. The Jewish diaspora spoke Yiddish, Ladino, Arabic, and other vernaculars. The founding problem: How can a geographically dispersed people without a shared vernacular construct a modern nation-state with unified political and cultural identity?
% FOUNDING_PROBLEM_CORROBORATION: Hebrew revival proponents attest the founding problem remains live: without Hebrew as a shared mother tongue, national cohesion faces fragmentation. Linguists and minority-language advocates attest the founding problem is partially displaced—modern nation-states successfully manage multiple official and co-official languages; the problem was never inherently unsolvable through multilingualism. Historians and religious scholars document that Hebrew survived the dormancy through continuous liturgical and scholarly transmission, suggesting alternative pathways to linguistic persistence beyond native generational acquisition.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__native_generational_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__native_generational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__native_generational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_linguistic_life__native_generational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__native_generational_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.38 to 0.72 over the interval because the constraint's initial function (coordinate around Hebrew) becomes layered with suppression (enforce Hebrew monolingualism; delegitimize alternatives). Suppression requirement tracks the rising intensity of enforcement: initial voluntary adoption (45%) becomes state-mandated education and institutional barriers (78%). Theater ratio rises because the constraint's narrative changes: from 'we need a shared language' (genuine coordination) to 'Hebrew nativeness is the only authentic marker of Jewishness' (performance of authenticity while suppressing actual linguistic diversity). The measurement series show that extraction accelerates through early state-building, then plateaus as monolingualism becomes institutionalized and normalized. The slot (measurement time points) is shared across all three metrics so no metric appears at a time point the others do not.
 *
 * PERSPECTIVAL GAP:
 *   The key perspectival divergence is between the institutional beneficiary (agenda-setter that controls the definition of linguistic life) and the constrained payers (linguistic minorities whose languages are declared dead by definition). From the Hebrew revival perspective, the native-generational criterion is a universal linguistic fact—languages are alive when spoken natively by children, period. From the Yiddish/Ladino perspective, the criterion is a constructed preference claim that selectively applies to Hebrew while delegitimizing living alternatives. This is not a difference in measurement basis but a difference in how the constraint's core claim is validated: one seat takes 'native generational transmission' as discovery (finding a fact about language), the other takes it as construction (imposing a preference about which languages count).
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation flows from beneficiary/victim declarations: hebrew_revival_movement and jewish_national_project are beneficiaries (low d toward 0.0), while yiddish_speakers, ladino_speakers, and arabic_speaking_jewish_communities are victims (high d toward 1.0). Exit options moderate this: revival movement has arbitrage-grade exit (can pursue alternative nation-building models; not trapped by the constraint itself), while linguistic minorities have constrained or identity_locked exit (leaving the constraint means either linguistic assimilation or geographic displacement, neither of which fully resolves the extraction). The constraint's suppression (0.78 at interval end) is substantially structural—state education, mass media, institutional policy—but also internalized through ideological legitimation ('Hebrew nativeness is authentic; your language is dead'). An omega addresses whether suppression persists post-exit: if a Yiddish speaker immigrates and raises children in Yiddish, does suppression lift, or do they carry internalized delegitimation of their language forward? This distinction matters for full d derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status/disappearance_verdict mismatch flags mandatrophy here. Founding problem: dispersed diaspora needs unified language for national cohesion (status: contestable; initially live, increasingly questioned). Disappearance verdict: world_rearranges—if the native-generational criterion vanished, multilingual nation-states would reorganize. The divergence suggests the constraint persists partly because it solves a founding problem (coordinate national identity) and partly because it serves institutional inertia (monolingual education system, state apparatus) and ideological capture (the native-generational criterion is now treated as natural rather than constructed). Theater ratio rising from 0.20 to 0.42 signals that the constraint's performative function is growing: fewer practical reasons to enforce Hebrew-only education (digital communication, global English, etc.), but continued enforcement to maintain the ideology of linguistic authenticity. This is classic Piton trajectory mixed with Tangled Rope stability. The constraint is not classically dead (mandatrophy_resolved: false) because the founding problem remains live under the reading's own framing—national identity still requires linguistic unity in the native-generational reading's logic. But the zombie signature is present: the constraint persists partly through theater and partly through institutional inertia rather than through continued necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_generational_criterion_universality,
    'Is the native-generational criterion a universal discovery about how languages persist, or a constructed preference about which languages a dominant group wants to recognize as alive?',
    'Comparative sociolinguistic analysis: do languages persist in other domains (liturgical, functional, heritage-transmission) when native-generational speakers are absent? If yes, the criterion is constructed; if no, it is universal.',
    'If constructed, the constraint''s core claim rests on an ideology rather than an empirical fact, and classification shifts from ''natural law about language'' toward ''enforced preference about linguistic legitimacy''—changing from mountain to tangled_rope or snare depending on enforcement intensity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(native_generational_criterion_universality, conceptual, 'Whether native generational transmission is a universal condition for linguistic life or a culturally specific criterion imposed as legitimation for language standardization.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.78 at interval end) primarily structural (external barriers: state education policy, media control, institutional requirements) or internalized (speakers'' own beliefs that their language is dead and must be abandoned)?',
    'Post-exit trajectory analysis: if diaspora Yiddish/Ladino communities regain access to heritage-language education outside state control, do suppression effects persist? If suppression lifts, it was primarily structural; if it persists, it is substantially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—targets carry the suppression with them after exit. This would increase d values for victims and raise the per-seat classification from tangled_rope toward snare. It would also justify an omega on identity-fusion: speakers have internalized the claim that their language is dead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Decomposition of suppression into structural and internalized mechanisms in multilingual assimilation contexts.').

omega_variable(
    founding_problem_displacement,
    'Has the founding problem (need for unified national language to enable nation-state cohesion) been solved, or does it remain live?',
    'Counterfactual scenario: if the native-generational criterion were abandoned and Hebrew coexisted with Yiddish/Ladino/Arabic as co-official or community languages, would the state''s institutional cohesion degrade? Comparative evidence from multilingual nation-states.',
    'If the founding problem is solved (multilingual states are viable), the constraint persists through institutional inertia and ideological capture—classic Piton with theater. If the problem remains live, the constraint solves something real and may warrant a less severe classification. Theater ratio rising while extraction plateaus suggests displacement: initial real problem-solving gives way to performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_displacement, empirical, 'Whether the linguistic-unity founding problem has been displaced by successful institutionalization or remains pressing.').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the native-generational reading logically foreclose the liturgical-preservation reading (continuous Hebrew study in sacred contexts keeps Hebrew alive), or do both readings remain structurally coexistent as held by different parties?',
    'Genealogical analysis: could a party simultaneously hold both ''Hebrew nativeness is required for linguistic life'' AND ''Hebrew liturgical transmission suffices to keep Hebrew alive during dormancy periods''? If yes, coexistence; if no, one forecloses the other.',
    'If foreclosure: the readings are logically incompatible—choosing one rules out the other. If coexistence: both remain live as positions held by different communities and institutions. This determines whether the sibling relationship is ''forecloses'' or ''coexists_with'' in cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Structural compatibility of the native-generational reading with the liturgical-preservation reading in a single analytical framework.').

omega_variable(
    victim_set_completeness,
    'Are the declared victims (yiddish_speakers, ladino_speakers, arabic_speaking_jewish_communities) the complete set of those bearing extraction, or are there additional victim populations whose languages are suppressed under the native-generational criterion?',
    'Sociolinguistic survey of Jewish diaspora communities and their linguistic practices post-revival. Were speakers of other Hebrew-adjacent or minority Jewish languages (Judeo-Greek, Judeo-Persian, etc.) similarly coerced into assimilation?',
    'If incomplete: adding suppressed victim populations changes the scope of extraction (from regional to global Jewish diaspora) and may increase measured extraction values. If complete: the identified victims represent the primary extraction targets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_completeness, empirical, 'Comprehensive identification of all linguistic minorities suppressed by the native-generational criterion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__native_generational_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__native_generational_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t15, hebrew_linguistic_life__native_generational_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement_basis(hebr_tr_t15, observed).
narrative_ontology:measurement(hebr_tr_t30, hebrew_linguistic_life__native_generational_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(hebr_tr_t30, observed).
narrative_ontology:measurement(hebr_tr_t45, hebrew_linguistic_life__native_generational_reading, theater_ratio, 45, 0.37).
narrative_ontology:measurement_basis(hebr_tr_t45, observed).
narrative_ontology:measurement(hebr_tr_t60, hebrew_linguistic_life__native_generational_reading, theater_ratio, 60, 0.4).
narrative_ontology:measurement_basis(hebr_tr_t60, observed).
narrative_ontology:measurement(hebr_tr_t75, hebrew_linguistic_life__native_generational_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement_basis(hebr_tr_t75, observed).
narrative_ontology:measurement(hebr_tr_t90, hebrew_linguistic_life__native_generational_reading, theater_ratio, 90, 0.42).
narrative_ontology:measurement_basis(hebr_tr_t90, observed).
narrative_ontology:measurement(hebr_tr_t120, hebrew_linguistic_life__native_generational_reading, theater_ratio, 120, 0.42).
narrative_ontology:measurement_basis(hebr_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t15, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement_basis(hebr_be_t15, observed).
narrative_ontology:measurement(hebr_be_t30, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(hebr_be_t30, observed).
narrative_ontology:measurement(hebr_be_t45, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 45, 0.65).
narrative_ontology:measurement_basis(hebr_be_t45, observed).
narrative_ontology:measurement(hebr_be_t60, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement_basis(hebr_be_t60, observed).
narrative_ontology:measurement(hebr_be_t75, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 75, 0.71).
narrative_ontology:measurement_basis(hebr_be_t75, observed).
narrative_ontology:measurement(hebr_be_t90, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 90, 0.72).
narrative_ontology:measurement_basis(hebr_be_t90, observed).
narrative_ontology:measurement(hebr_be_t120, hebrew_linguistic_life__native_generational_reading, base_extractiveness, 120, 0.72).
narrative_ontology:measurement_basis(hebr_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t15, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement_basis(hebr_su_t15, observed).
narrative_ontology:measurement(hebr_su_t30, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 30, 0.64).
narrative_ontology:measurement_basis(hebr_su_t30, observed).
narrative_ontology:measurement(hebr_su_t45, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement_basis(hebr_su_t45, observed).
narrative_ontology:measurement(hebr_su_t60, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement_basis(hebr_su_t60, observed).
narrative_ontology:measurement(hebr_su_t75, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 75, 0.77).
narrative_ontology:measurement_basis(hebr_su_t75, observed).
narrative_ontology:measurement(hebr_su_t90, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 90, 0.78).
narrative_ontology:measurement_basis(hebr_su_t90, observed).
narrative_ontology:measurement(hebr_su_t120, hebrew_linguistic_life__native_generational_reading, suppression_requirement, 120, 0.78).
narrative_ontology:measurement_basis(hebr_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__native_generational_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_linguistic_life__native_generational_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__native_generational_reading, hebrew_linguistic_life__marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'hebrew_linguistic_life'. The native-generational reading declares a language alive through mother-tongue acquisition and daily secular use. Sibling readings use different criteria: liturgical-preservation (sacred text transmission) and marketplace-pidgin (functional coordination regardless of nativeness). Each reading has distinct ε, victim sets, and structural classification. They share a common kernel (what keeps Hebrew alive?) but diverge on the criterion for evaluating aliveness. Network edges link all three stories; commentary.kernel_context documents the reading relations and axiom differences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
