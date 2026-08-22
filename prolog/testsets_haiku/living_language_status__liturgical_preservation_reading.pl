% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Living Language Status via Liturgical Preservation (Rabbinical Reading)
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel: 'living
 *   language status.' The kernel is a stabilized textual and institutional
 *   claim — the halachic principle that Hebrew remains a 'living language'
 *   (lashon chaya) because its sacred texts are continuously studied, recited
 *   in prayer, and interpreted by each generation of scholars. This reading
 *   treats liturgical transmission and textual continuity as sufficient for
 *   language vitality, independent of native speaker demographics. The
 *   sibling readings — the native-generation reading (vitality requires
 *   mother-tongue transmission) and the literary-continuity reading (vitality
 *   lives in secular literature and journalism) — contest whether this
 *   definition captures what it means for a language to be alive. The
 *   constraint describes the institutional and social structure that enforces
 *   the liturgical reading's authority over competing definitions.
 *
 * KEY AGENTS:
 *   - Rabbinical authority: institutional agenda-setter, controls the interpretive frame for what counts as living Hebrew
 *   - Secular Hebrew speakers: structural victims of delegitimization; their living speech is framed as corruption or desecration
 *   - Diaspora modernizers: victims whose professional project (reviving Hebrew as native tongue) is de-authorized by the liturgical standard
 *   - Talmudic scholars: beneficiaries; their hermeneutic monopoly is protected by the restriction of legitimate Hebrew use to sacred-text study
 *   - Religious nationalism: institutional beneficiary; the reading anchors national identity in rabbinical authority
 *   - Secular nationalism: excluded from the definition; competes for authority to set language-vitality standards
 *   - Linguistic science: observer; provides an alternative measuring frame (generational transmission, innovation) that contests the rabbinical one
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.48).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.61).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Living Language Status via Liturgical Preservation (Rabbinical Reading)").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '8b7d57d0-aa54-40da-a63f-0253d4dd1592').
narrative_ontology:cs_kernel_codification('8b7d57d0-aa54-40da-a63f-0253d4dd1592', fixed_text).
narrative_ontology:cs_authority_grounding('8b7d57d0-aa54-40da-a63f-0253d4dd1592', lineage).
narrative_ontology:cs_interpretation_layer_present('8b7d57d0-aa54-40da-a63f-0253d4dd1592').
narrative_ontology:cs_reading_relation('8b7d57d0-aa54-40da-a63f-0253d4dd1592', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b7d57d0-aa54-40da-a63f-0253d4dd1592', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('8b7d57d0-aa54-40da-a63f-0253d4dd1592', foundational, textual_continuity_defines_vitality).
narrative_ontology:cs_axiom_status(textual_continuity_defines_vitality, holdable).
narrative_ontology:cs_axiom_grounding('8b7d57d0-aa54-40da-a63f-0253d4dd1592', textual_continuity_defines_vitality, conventional).
narrative_ontology:cs_axiom('8b7d57d0-aa54-40da-a63f-0253d4dd1592', foundational, rabbinical_interpretive_monopoly_legitimacy).
narrative_ontology:cs_axiom_status(rabbinical_interpretive_monopoly_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8b7d57d0-aa54-40da-a63f-0253d4dd1592', rabbinical_interpretive_monopoly_legitimacy, deontological).
narrative_ontology:cs_reference_frame('8b7d57d0-aa54-40da-a63f-0253d4dd1592', unbroken_liturgical_transmission).
narrative_ontology:cs_drift_state('8b7d57d0-aa54-40da-a63f-0253d4dd1592', contemporary_secular_hebrew_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8b7d57d0-aa54-40da-a63f-0253d4dd1592', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_hebrew_speakers).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, diaspora_modernizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, talmudic_scholars).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, religious_nationalism).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, hebrew_native_speakers).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, sacred_text_sanctity).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, rabbinical_interpretive_monopoly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the definition of Hebrew vitality through control of liturgical curriculum, textual interpretation, and validation of authentic language transmission. Benefits from this authority structure by securing an institutional monopoly on language standards and maintaining rabbinical centrality to Jewish identity. Can shift the frame if institutional interests demand it, but has incentive to defend the constraint's core claim (liturgical continuity = vitality) because it legitimates their interpretive role.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Their professional expertise and interpretive authority are constituted through the constraint. The definition of living Hebrew as liturgical transmission ensures their hermeneutic work remains central and uncontested. They benefit from the closed domain of sacred-text study by maintaining their professional monopoly and cultural authority. Exit would require abandoning their entire professional identity.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, talmudic_scholars, beneficiary,
    institutional, civilizational, identity_locked, global).

% Use Hebrew as a living language in daily speech, journalism, literature, and governance. Their speech and writing are acts of vitality — generational transmission, linguistic innovation, and cultural expression. Yet the constraint structures them as delegitimized speakers: their Hebrew is treated as modern corruption, secular desecration, or mere instrumental use rather than authentic participation in the living language. They have state power and literary influence globally, but within the rabbinical authority frame their legitimacy is denied. Exit from the constraint requires either abandoning their own Hebrew use (impossible and unthinkable) or constructing an alternative authority structure (costly, contested).
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_hebrew_speakers, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, secular_hebrew_speakers, excluded).

% Invest professional and intellectual energy in reviving Hebrew as a native spoken language in diaspora and homeland communities. Their vision of vitality contradicts the constraint's definition: they argue living Hebrew requires generational mother-tongue transmission, not liturgical recitation. The constraint delegitimizes their project by framing native revival as impossible (the language has no native speakers) or insufficient (native speech is not the measure of vitality). Their entire professional identity is staked on the alternative definition; exit would mean abandoning their life's work.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, diaspora_modernizers, payer,
    moderate, biographical, identity_locked, global).

% Benefits from the constraint because it provides a non-demographic basis for Jewish national identity. Language vitality defined through liturgical continuity rather than native-speaker majority legitimates a national project that unites diaspora Jews around shared textual and ritual practice. The constraint anchors national identity in rabbinical authority rather than in territorial or linguistic kinship, which suits the interests of religious nationalist institutions.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, religious_nationalism, beneficiary,
    institutional, civilizational, analytical, national).

% Seeks to ground Hebrew revival and national identity in secular justifications: statehood, literature, generational transmission, language innovation. The constraint structures them as excluded from the definition of language vitality because secular nationalisms cannot appeal to rabbinical authority. They must contest the constraint to establish an alternative authority frame. State power gives them institutional leverage, but the cultural and religious embeddedness of the rabbinical reading makes displacement costly and incomplete.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_nationalism, excluded,
    powerful, biographical, constrained, national).

% Native speakers of modern Hebrew benefit from one reading of the constraint: their existence validates the claim that Hebrew is living (if we read 'continuous use' as including their colloquial speech). However, this benefit is shallow and contingent. The constraint's core authority does not require their participation — it rests on ritual and textual continuity, not speaker demographics. Their generational transmission of Hebrew is treated as instrumentally valuable but not as the measure of the language's vitality. In tension with the payer role, because the constraint can ignore their existence without losing authority.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, hebrew_native_speakers, beneficiary,
    organized, biographical, constrained, national).

% Applies linguistic criteria for language vitality: speaker population, demographic transmission, innovation patterns, functional domains. From a scientific standpoint, Hebrew exhibits vitality through generational transmission among native speakers and through productive literary and technical innovation. The observer position is outside the constraint because scientific linguistics does not recognize religious authority as relevant to vitality classification. Provides an alternative measuring frame that competes with and contests the rabbinical reading.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, linguistic_science, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:fixing_cost_class(living_language_status__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed Jewish community around shared sacred texts and interpretive tradition, enabling linguistic-cultural continuity across diaspora and generations without requiring territorial settlement or native-speaker demographic base. Solves the problem of how a stateless people can maintain linguistic identity across time and space through textual and ritual transmission.
% TRANSFER_FUNCTION: Transfers authority from secular speakers and modernizers to rabbinical institutions and Talmudic scholars. The transfer is of legitimacy: secular Hebrew speech is framed as derivative or corrupted unless sanctified through connection to sacred texts. Living-language status is moved to those who control textual interpretation and ritual transmission, away from those who speak Hebrew daily or write in it creatively.
% ABSENT_VOICES: Secular Hebrew speakers and language revivalists are present in the constraint but delegitimized — their voices are structurally excluded from the definition of language vitality. Scientific linguists who measure vitality through speaker demographics and innovation are outside the frame entirely. Daughters, women, and lay people excluded from advanced Talmudic study have limited voice in validating transmission. Diaspora communities that practice Hebrew liturgy without rabbinical authority are partially absent — their practice doesn't count as legitimate validation of continuity.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared — if Hebrew vitality could no longer be claimed through liturgical preservation — the entire authority structure grounding Jewish institutional life would shift. Secular Hebrew revivalists would become the primary validators of language vitality; rabbinical authority over language standards would erode dramatically; native-speaker demographics and literary production would become the measure instead. The religious institutional basis of Jewish continuity claims would be severely weakened, forcing reorganization around secular or territorial grounds.
% FOUNDING_PROBLEM: After the destruction of the Second Temple and diaspora dispersal, Hebrew ceased to be a primary spoken language for most Jewish communities. The founding problem is: how can a dispersed people without territorial base or generational mother-tongue transmission maintain linguistic unity and cultural continuity? The liturgical preservation reading answers: through unbroken recitation and study of sacred texts, the language remains alive and the people remain unified, even without native speakers.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinical authorities attest the founding problem is live: without continuous liturgical transmission and textual study, the language dies and the Jewish people loses its essential medium of identity. Secular modernizers and linguists attest the problem was substantially solved by the Haskalah period and modern Hebrew revival — Hebrew now has native speakers, a territorial base (the Israeli state), and vibrant secular literature; the founding problem is dead. The Israeli national project and growing secular Hebrew culture provide corroboration from outside the rabbinical authority structure that the founding problem's status has changed.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48 at interval end), not high, because the constraint does offer genuine coordination: a diaspora community with no territorial base genuinely faces the problem of maintaining linguistic-cultural unity across generations. Liturgical preservation solves that problem. However, extraction is present because the solution is imposed authoritatively — secular speakers' living Hebrew is delegitimized unless sanctified by connection to sacred texts. Suppression (0.61) is moderately high because maintaining this authority requires actively suppressing competing definitions of vitality. Theater is present (0.42) because much of the enforcement activity involves performative claims about continuity and authenticity rather than defending a mechanism whose function would fail without enforcement — if you stop suppressing the secular reading, the liturgical reading still preserves texts, but its AUTHORITY erodes. The measurement trajectory shows suppression rising slightly as modern secular Hebrew grows in demographic power (speaker base expanding), requiring more active enforcement to maintain the rabbinical authority frame. Extractiveness plateaus after t=15 because the constraint has reached an equilibrium: it preserves the authority structure without expanding its reach further. The theater_ratio rises because the performative work of defending rabbinical definitions against secular usage becomes more visible and effortful over time.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinical authority and Talmudic scholars compute this constraint as genuine coordination with no net extraction — the unbroken chain of transmission is the coordination benefit, the preservation of culture is the collective good. Secular speakers and modernizers compute the same structure as enforced illegitimacy — their living Hebrew is real, but the constraint frames it as not counting. The engine computes per-seat classifications from the directionality data: beneficiaries (rabbinical authority) get low d, targets (secular speakers) get high d. The claimed_type (rope) reflects the rabbinical seat's perception; the metrics reflect the actual structure (moderate extraction + suppression). This divergence is precisely what the measurement apparatus exists to detect.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinical authority: d ≈ 0.15 (beneficiary, institutional power, arbitrage exit — they define the rules and can shift the frame if needed). Talmudic scholars: d ≈ 0.10 (beneficiary, institutional power, identity-locked exit — their professional identity is constituted through this authority structure, but they do not pay the constraint). Secular Hebrew speakers: d ≈ 0.85 (target, powerful but constrained exit — they have demographic and literary power but cannot exit the authority frame without losing legitimacy in the communities where they live; identity-locking occurs through cultural embeddedness). Diaspora modernizers: d ≈ 0.82 (target, moderate power, identity-locked exit — their entire professional identity is staked on the alternative definition; exit requires abandoning their project). Religious nationalism: d ≈ 0.20 (beneficiary, institutional, analytical exit — the reading legitimates their project but they do not directly enforce it). The directionality override for secular_hebrew_speakers could reflect their powerful global institutional position (literary influence, state power) while leaving their local constraint-relationship high (d stays near 0.85) — they are powerful outside the constraint but constrained inside it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is: how do dispersed people maintain linguistic unity without territorial base or native speakers? The liturgical answer (continuous study and recitation of sacred texts) genuinely solves this coordination problem. However, the problem's STATUS is contested: secular modernizers argue it was solved by the Haskalah and modern revival — Hebrew now has native speakers and literary vitality, so the founding problem is dead. The constraint persists despite the dead founding problem because rabbinical authority benefits from it (interpretive monopoly) and because it is deeply embedded in religious practice and identity. This is a mandatrophy case: the original coordination function (preserving a dispersed people's language identity across generations without native speakers) has been substantially solved by modern Hebrew revival, yet the constraint persists through its institutional beneficiaries' continued investment in the authority structure. The theater_ratio rising over time (from 0.28 to 0.42) signals that more of the enforcement is performative (defending authority claims) than functional (preserving a genuinely threatened language). A secular speaker who learns colloquial Hebrew and reads modern literature can participate in the living language; the constraint does not prevent this. What it prevents is their recognition as having legitimate authority to define what the language IS — the suppression and theater are in the ideological domain, not the structural one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the founding problem (preserving a language without native speakers across diaspora) still live, or has it been substantially solved by modern Hebrew revival and territorial settlement?',
    'Empirical: measure Hebrew speaker demographics, literature production, and everyday use across diaspora and homeland at t=now. Conceptual: examine whether rabbinical authorities acknowledge or deny the problem''s changed status.',
    'If the founding problem is dead, the constraint becomes a case of mandatrophy — institutional inertia preserving authority structure past its functional justification. This would support reclassification toward piton. If the problem remains live (rabbinical authorities'' framing), the constraint is genuine coordination despite secular contestation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, conceptual, 'Whether modern Hebrew has solved the diaspora language-preservation problem the constraint was founded to address.').

omega_variable(
    rabbinical_interpretive_monopoly_scope,
    'Does the constraint''s enforcement genuinely depend on rabbinical authority, or could the same liturgical transmission persist without the institutional monopoly on language-vitality definitions?',
    'Natural experiment: secular Jewish communities that maintain Hebrew liturgical practice without recognizing rabbinical interpretive authority. Comparison: does the liturgical transmission degrade in the absence of rabbinical institutional backing?',
    'If transmission persists without institutional authority, the beneficiary is weaker than the narrative suggests — the coordination (liturgical continuity) is separable from the extraction (authority monopoly). If transmission depends on institutional embedding, the two are coupled and the constraint is more tightly integrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rabbinical_interpretive_monopoly_scope, empirical, 'Whether institutional authority is separable from liturgical preservation, or whether they are structurally coupled.').

omega_variable(
    secular_hebrew_suppression_mechanism,
    'Is the measured suppression of secular Hebrew primarily structural (institutional exclusion from authority, resource allocation favoring liturgical study) or internalized (speakers themselves adopt the rabbinical frame and experience their own speech as less legitimate)?',
    'Qualitative: interviews and ethnography of Hebrew speakers'' self-perception. Historical: track how speaker attitudes shifted as secular state power grew. Comparative: examine secular Hebrew speakers'' confidence claims in different institutional contexts (strong state vs. diaspora minority setting).',
    'If structural: remedies focus on institutional change (redistribution of authority over language standards). If internalized: remedies must address cultural narratives and identity-fusion — suppression persists even after external institutional barriers dissolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_hebrew_suppression_mechanism, empirical, 'Structural vs. internalized suppression of secular Hebrew speakers'' legitimacy.').

omega_variable(
    reading_foreclosure_possibility,
    'Does the liturgical reading genuinely foreclose the native-generation reading, or can both coexist in a single framework that recognizes both liturgical and colloquial Hebrew as legitimate expressions of the living language?',
    'Textual: examine whether any rabbinical authority has endorsed a framework that accommodates both definitions. Conceptual: assess whether the readings'' core premises logically contradict or only claim different authorities.',
    'If foreclosure is real (the readings logically contradict), the constraint embeds a zero-sum competition for authority. If coexistence is possible, the constraint''s enforcement is a political choice, not a structural necessity — reclassification toward snare becomes more likely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_possibility, conceptual, 'Whether the liturgical and native-generation readings logically foreclose each other or can coexist in principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(livi_tr_t0, observed).
narrative_ontology:measurement(livi_tr_t5, living_language_status__liturgical_preservation_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(livi_tr_t5, observed).
narrative_ontology:measurement(livi_tr_t10, living_language_status__liturgical_preservation_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(livi_tr_t10, observed).
narrative_ontology:measurement(livi_tr_t15, living_language_status__liturgical_preservation_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(livi_tr_t15, observed).
narrative_ontology:measurement(livi_tr_t20, living_language_status__liturgical_preservation_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(livi_tr_t20, observed).
narrative_ontology:measurement(livi_tr_t25, living_language_status__liturgical_preservation_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(livi_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(livi_be_t0, observed).
narrative_ontology:measurement(livi_be_t5, living_language_status__liturgical_preservation_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(livi_be_t5, observed).
narrative_ontology:measurement(livi_be_t10, living_language_status__liturgical_preservation_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(livi_be_t10, observed).
narrative_ontology:measurement(livi_be_t15, living_language_status__liturgical_preservation_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement_basis(livi_be_t15, observed).
narrative_ontology:measurement(livi_be_t20, living_language_status__liturgical_preservation_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(livi_be_t20, observed).
narrative_ontology:measurement(livi_be_t25, living_language_status__liturgical_preservation_reading, base_extractiveness, 25, 0.48).
narrative_ontology:measurement_basis(livi_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(livi_su_t0, observed).
narrative_ontology:measurement(livi_su_t5, living_language_status__liturgical_preservation_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement_basis(livi_su_t5, observed).
narrative_ontology:measurement(livi_su_t10, living_language_status__liturgical_preservation_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement_basis(livi_su_t10, observed).
narrative_ontology:measurement(livi_su_t15, living_language_status__liturgical_preservation_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement_basis(livi_su_t15, observed).
narrative_ontology:measurement(livi_su_t20, living_language_status__liturgical_preservation_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(livi_su_t20, observed).
narrative_ontology:measurement(livi_su_t25, living_language_status__liturgical_preservation_reading, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(livi_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__liturgical_preservation_reading, 0.12).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The living_language_status kernel decomposes into three structurally distinct constraints, one per reading of what counts as language vitality. Each reading produces different ε values (extractiveness), different beneficiary/victim structures, and different typologies. The liturgical reading (this story) treats authority as grounded in textual and ritual continuity; the native-generation reading grounds vitality in speaker demographics; the literary reading grounds it in secular intellectual production. These readings coexist across different institutional factions within Jewish communities and are linked through network edges indicating structural influence: the growth of secular Hebrew literacy and state power influences (but does not foreclose) the rabbinical reading's authority claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__liturgical_preservation_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
