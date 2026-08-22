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
 *   human_readable: Liturgical Transmission as Living Language Status
 *   domain: sociolinguistics/religious_studies/nationalism_studies
 *
 * SUMMARY:
 *   This reading defines Hebrew's living status through liturgical continuity
 *   alone: so long as sacred texts are recited, studied, and used in ritual,
 *   the language is alive. It emerged in the late 19th century as a response
 *   to Haskalah secularization and later to Zionist Hebrew revival, offering
 *   a counter-criterion that preserves rabbinical authority over the
 *   language's boundaries. The constraint coordinates a global Jewish speech
 *   community around a fixed liturgical corpus — extraction is low because
 *   the coordination is genuine (shared ritual intelligibility across
 *   diaspora) — but suppression is nonzero because the reading actively
 *   delegitimizes secular vernacular development (modern Hebrew, Yiddish
 *   literary culture, spoken Hebrew revival) as desecration rather than
 *   vitality. The beneficiaries are rabbinical courts, yeshiva networks, and
 *   liturgical institutions whose interpretive monopoly depends on the
 *   language remaining anchored to the sacred corpus; the victims are secular
 *   Hebrew speakers, Yiddish writers, and modernizers who are told their
 *   linguistic creativity is not 'real' Hebrew life.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.22).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.38).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Liturgical Transmission as Living Language Status").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism_studies").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '0f0bdaa5-b757-4958-b154-db41abda6e66').
narrative_ontology:cs_kernel_codification('0f0bdaa5-b757-4958-b154-db41abda6e66', fixed_text).
narrative_ontology:cs_authority_grounding('0f0bdaa5-b757-4958-b154-db41abda6e66', lineage).
narrative_ontology:cs_interpretation_layer_present('0f0bdaa5-b757-4958-b154-db41abda6e66').
narrative_ontology:cs_reading_relation('0f0bdaa5-b757-4958-b154-db41abda6e66', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('0f0bdaa5-b757-4958-b154-db41abda6e66', living_language_status__literary_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('0f0bdaa5-b757-4958-b154-db41abda6e66', foundational, liturgical_continuity_suffices_for_vitality).
narrative_ontology:cs_axiom_status(liturgical_continuity_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('0f0bdaa5-b757-4958-b154-db41abda6e66', liturgical_continuity_suffices_for_vitality, deontological).
narrative_ontology:cs_axiom('0f0bdaa5-b757-4958-b154-db41abda6e66', foundational, vernacular_innovation_is_desecration_not_vitality).
narrative_ontology:cs_axiom_status(vernacular_innovation_is_desecration_not_vitality, holdable).
narrative_ontology:cs_axiom_grounding('0f0bdaa5-b757-4958-b154-db41abda6e66', vernacular_innovation_is_desecration_not_vitality, deontological).
narrative_ontology:cs_reference_frame('0f0bdaa5-b757-4958-b154-db41abda6e66', sinaitic_hebrew_continuity).
narrative_ontology:cs_drift_state('0f0bdaa5-b757-4958-b154-db41abda6e66', post_state_of_israel_establishment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0f0bdaa5-b757-4958-b154-db41abda6e66', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, liturgical_institutions).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_speech_community).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, vernacular_modernizers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, diaspora_ritual_participants).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, liturgical_sufficiency_for_vitality).
narrative_ontology:constraint_vindicates(living_language_status__liturgical_preservation_reading, sacred_text_continuity_as_life).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the definition of Jewish linguistic legitimacy through halakhic authority, conversion standards, and communal recognition. Collects interpretive monopoly: the power to say what counts as Hebrew, who counts as a Hebrew speaker, and what linguistic innovations are permissible. Does not bear the cost of maintaining a spoken vernacular — the liturgical corpus is fixed and maintained by tradition.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, rabbinical_authority, beneficiary).

% Yeshivas, synagogue networks, and ritual bodies that transmit the liturgical corpus. They benefit from the reading's validation of their core activity as the definition of linguistic life. Their funding, prestige, and recruitment depend on liturgical Hebrew being treated as the authentic center. Exit is constrained: they could adopt a broader vitality criterion, but that would dilute their distinctive institutional rationale.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, liturgical_institutions, beneficiary,
    organized, generational, constrained, global).

% Modern Hebrew speakers in Israel and diaspora who use Hebrew as a mother tongue. They bear the cost of being told their vitality is 'secular' and therefore lesser — excluded from religious recognition (marriage, conversion, burial), denied communal legitimacy, and forced to justify their Hebrew on terms set by the reading they reject. Exit is constrained: they cannot leave Hebrew (it is their native language) but must negotiate legitimacy within a framework that defines them out of 'true' life.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_speech_community, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, secular_speech_community, excluded).

% Haskalah writers, early Zionist Hebrew revivalists, Yiddish literary figures — historical agents who built secular Hebrew culture. They paid the cost of herem, publication bans, communal ostracism, and the intellectual labor of proving vitality on hostile terms. Their exit was mobile: they could and did build alternative institutions (Hebrew press, universities, state), but only by leaving the religious framework entirely.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, vernacular_modernizers, payer,
    moderate, biographical, mobile, global).

% Jews worldwide who participate in liturgical Hebrew without speaking it vernacularly. They genuinely benefit from the coordination: a shared ritual language enables communal participation across linguistic difference. They are not harmed by the reading — it validates their practice. Exit is mobile: they could adopt vernacular Hebrew or abandon ritual, but the reading serves their actual need.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, diaspora_ritual_participants, beneficiary,
    moderate, biographical, mobile, global).

% Sociolinguists, historians of Hebrew, scholars of language revitalization who study the three readings as competing vitality criteria. They neither collect nor pay; they map the structural conflict. Their exit is analytical: they can adopt any framework or none.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, linguistic_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:fixing_cost_class(living_language_status__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified criterion for Hebrew's living status that maintains diaspora ritual unity without requiring a spoken vernacular — a single liturgical corpus serves as the coordination anchor across geography, generation, and spoken language difference.
% TRANSFER_FUNCTION: Moves interpretive authority and communal legitimacy from vernacular speakers to liturgical transmitters. The secular speech community's creative labor (new words, living grammar, mother-tongue transmission) is rendered illegitimate; the rabbinical authority's monopoly over the fixed corpus is rendered definitive.
% ABSENT_VOICES: The mass of pre-1948 Eastern European Jews who spoke Yiddish daily and used Hebrew liturgically — they inhabited both worlds and would likely reject the forced choice. Also absent: Sephardic communities where liturgical and vernacular Hebrew coexisted without the Ashkenazi fracture. Neither group is represented in the Ashkenazi-dominated rabbinical authority that authored this reading.
% DISAPPEARANCE_RATIONALE: If the liturgical-sufficiency criterion vanished overnight, the rabbinate would lose its definitional monopoly over Hebrew vitality. Modern Hebrew would be the uncontested center of Jewish linguistic life. Conversion, marriage, and burial standards would shift toward vernacular competence. The global diaspora's ritual unity would need a new anchor (likely Israeli Hebrew). The arrangement of authority would rearrange fundamentally.
% FOUNDING_PROBLEM: Late 19th century: How to maintain Jewish linguistic unity and religious authority in the face of Haskalah secularization, Yiddish literary flowering, and emerging Zionist Hebrew revival — all of which threatened to fragment the sacred language into competing vernaculars or relegate it to a museum piece.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinical authorities (e.g., Hildesheimer, Soloveitchik lineages) attest the problem is live: secularization continues, assimilation threatens, liturgical unity remains the only proven anchor. Secular Hebrew scholars (Harshav, Fellman, Kuzar) and sociolinguists (Fishman, Spolsky) attest the problem is substantially solved: Hebrew is a thriving mother tongue, the State exists, diaspora unity is maintained through Israeli cultural hegemony, not liturgical exclusivity. The corroboration is split along the reading lines themselves.
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).
:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the primary function is coordination: a shared liturgical language enables ritual participation across geography and generation without requiring a spoken vernacular. Suppression (0.38) reflects active boundary enforcement — herem against secular Hebrew presses, denial of religious legitimacy to modern Hebrew, exclusion of vernacular innovators from communal recognition — not passive neglect. Theater ratio (0.18) is low but rising: the ritual function is real, but an increasing share of institutional energy defends the boundary against secular competition rather than serving the liturgy itself. Accessibility collapse (0.71) is high because once you accept the premise that sacred text continuity = life, the secular alternatives become invisible or illegitimate by definition. Resistance (0.45) is moderate: the Haskalah, Zionism, and secular Hebrew culture all contested this reading directly.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinical seat, this is pure coordination: a diaspora community maintains unity through shared sacred language. From the secular modernizer seat, it is a snare: the definition of 'living' is weaponized to deny legitimacy to the very vernacular revival that made Hebrew a mother tongue again. The engine will compute these as different types from the same structural data — the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbincal authority and liturgical institutions are structural beneficiaries (d ~ 0.1): they collect interpretive control, communal legitimacy, and resource allocation without bearing the cost of vernacular maintenance. Secular speech community and vernacular modernizers are targets (d ~ 0.8): they bear the cost of delegitimization, exclusion from communal recognition, and the burden of proving vitality on terms the reading rejects. The exit option for targets is 'constrained' — they can build secular Hebrew culture (and did) but must do so outside the religious framework that defines 'living' for the community. The analytical observer sees both the coordination function (real) and the extraction (boundary defense masquerading as definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora unity without territory) is substantially solved by the State of Israel and modern Hebrew — yet the reading persists because the interpretive monopoly it protects remains valuable to rabbinical authority. Mandatrophy is unresolved: the coordination function (ritual unity) is live but the extraction function (delegitimizing secular vitality) has expanded into the space the coordination function vacated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the living_language_status kernel, or does it represent a fundamentally different kernel (e.g., ''ritual_language_status'')?',
    'Test whether the three readings share the same referent (Hebrew''s status) and differ only in the criterion for ''living'', or whether they operate on different referents entirely. Comparative analysis of how each reading''s proponents cite the same historical episodes (Haskalah, revival, State of Israel) as evidence.',
    'If different kernels, the network edges between them are mis-specified; if same kernel, the reading_relations and axiom structure are valid and the engine''s cross-reading analysis applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the three declared readings instantiate one kernel or three.').

omega_variable(
    extraction_boundary_ambiguity,
    'How much of the measured suppression (0.38) is boundary defense of a genuine coordination function vs. active extraction from secular vitality?',
    'Counterfactual: if secular Hebrew had developed without rabbinical opposition (no herem, no denial of religious legitimacy), would the liturgical coordination function have weakened? Historical comparison with communities where liturgical and vernacular Hebrew coexisted without conflict (e.g., some Sephardic communities).',
    'If suppression is mostly boundary defense, the reading is a rope with a defensive perimeter; if mostly extraction, it trends toward tangled_rope. The current ε (0.22) assumes the former.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_boundary_ambiguity, empirical, 'Whether suppression protects coordination or extracts from alternatives.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of secular Hebrew structural (herem, institutional exclusion, state rabbinate monopoly) or internalized (secular writers accepting ''dead language'' framing, secular Israelis feeling religious Hebrew is ''more authentic'')?',
    'Post-1948 trajectory: if internalized suppression persists after structural barriers are removed (state recognition of Hebrew, secular education), the internalized component is confirmed. Compare secular Hebrew writers'' self-positioning vs. Yiddish writers'' self-positioning under similar pressures.',
    'If substantially internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for secular speech community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 1880, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t1880, living_language_status__liturgical_preservation_reading, theater_ratio, 1880, 0.05).
narrative_ontology:measurement(livi_tr_t1920, living_language_status__liturgical_preservation_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(livi_tr_t1948, living_language_status__liturgical_preservation_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(livi_tr_t1970, living_language_status__liturgical_preservation_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(livi_tr_t2000, living_language_status__liturgical_preservation_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(livi_tr_t2024, living_language_status__liturgical_preservation_reading, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(livi_be_t1880, living_language_status__liturgical_preservation_reading, base_extractiveness, 1880, 0.12).
narrative_ontology:measurement(livi_be_t1920, living_language_status__liturgical_preservation_reading, base_extractiveness, 1920, 0.18).
narrative_ontology:measurement(livi_be_t1948, living_language_status__liturgical_preservation_reading, base_extractiveness, 1948, 0.15).
narrative_ontology:measurement(livi_be_t1970, living_language_status__liturgical_preservation_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(livi_be_t2000, living_language_status__liturgical_preservation_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(livi_be_t2024, living_language_status__liturgical_preservation_reading, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t1880, living_language_status__liturgical_preservation_reading, suppression_requirement, 1880, 0.25).
narrative_ontology:measurement(livi_su_t1920, living_language_status__liturgical_preservation_reading, suppression_requirement, 1920, 0.4).
narrative_ontology:measurement(livi_su_t1948, living_language_status__liturgical_preservation_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(livi_su_t1970, living_language_status__liturgical_preservation_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(livi_su_t2000, living_language_status__liturgical_preservation_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(livi_su_t2024, living_language_status__liturgical_preservation_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__liturgical_preservation_reading, 0.08).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, hebrew_revival_as_national_project).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, rabbinical_authority_over_conversion).

% DUAL FORMULATION NOTE:
% This reading and native_generation_reading are mutually foreclosing within a single halakhic framework (one criterion for 'living' excludes the other as necessary). This reading and literary_continuity_reading coexist as live positions — the same actor can value literary Hebrew while denying it confers 'living' status. The native_generation_reading is structurally downstream: the success of spoken Hebrew revival created the condition under which this reading had to defensively reassert liturgical sufficiency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(living_language_status__liturgical_preservation_reading, institutional, 0.15).
constraint_indexing:directionality_override(living_language_status__liturgical_preservation_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
