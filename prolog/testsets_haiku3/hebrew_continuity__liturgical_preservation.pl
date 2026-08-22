% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Liturgical Preservation Through Textual Transmission
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Hebrew exists in a state of institutional contest between two competing
 *   transmission models: the liturgical-preservation reading (this
 *   constraint) and the native-generative reading (the sibling constraint).
 *   In the liturgical-preservation reading, Hebrew lives through fixed
 *   textual tradition, authorized interpretation, and ritual recitation—a
 *   model that does not require native speakers and has kept Hebrew literacy
 *   alive in diaspora communities for two millennia. In the native-generative
 *   reading, Hebrew lives only through the intuition and daily use of native
 *   speakers whose language use drives continuous evolution—the model that
 *   actually operates in Modern Israeli Hebrew. These are not compatible
 *   framings of the same thing; they are structurally distinct constraints
 *   with different beneficiaries, different enforcement mechanisms, and
 *   different victim sets. This story instantiates the
 *   liturgical-preservation reading and treats the native-generative reading
 *   as a sibling constraint with which it coexists.
 *
 * KEY AGENTS:
 *   - religious_institutional_authority: Sets and enforces the liturgical standard; benefits from the constraint's continuation
 *   - textual_tradition_keepers: Maintain the preserved corpus; identity-locked beneficiaries whose professional identity fuses with the preservation mandate
 *   - secular_hebrew_speakers: Bear the cost of delegitimization; their generative usage is marked as incomplete
 *   - generative_language_communities: Linguistically innovate but are suppressed by the constraint; forced to choose between innovation and institutional legitimacy
 *   - secularizing_institutional_forces: Excluded from the authoritative framing; compete but are delegitimized by the constraint's authority structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.68).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.72).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Liturgical Preservation Through Textual Transmission").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, '0a679cbd-5eb9-4054-aa70-d0a1fcb55831').
narrative_ontology:cs_kernel_codification('0a679cbd-5eb9-4054-aa70-d0a1fcb55831', fixed_text).
narrative_ontology:cs_authority_grounding('0a679cbd-5eb9-4054-aa70-d0a1fcb55831', lineage).
narrative_ontology:cs_interpretation_layer_present('0a679cbd-5eb9-4054-aa70-d0a1fcb55831').
narrative_ontology:cs_reading_relation('0a679cbd-5eb9-4054-aa70-d0a1fcb55831', hebrew_continuity__native_generative, coexists_with).
narrative_ontology:cs_reading_relation('0a679cbd-5eb9-4054-aa70-d0a1fcb55831', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('0a679cbd-5eb9-4054-aa70-d0a1fcb55831', foundational, hebrew_continuity_requires_textual_fixation).
narrative_ontology:cs_axiom_status(hebrew_continuity_requires_textual_fixation, holdable).
narrative_ontology:cs_axiom_grounding('0a679cbd-5eb9-4054-aa70-d0a1fcb55831', hebrew_continuity_requires_textual_fixation, conventional).
narrative_ontology:cs_axiom('0a679cbd-5eb9-4054-aa70-d0a1fcb55831', foundational, liturgical_authority_structure_is_necessary).
narrative_ontology:cs_axiom_status(liturgical_authority_structure_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('0a679cbd-5eb9-4054-aa70-d0a1fcb55831', liturgical_authority_structure_is_necessary, empirically_contingent).
narrative_ontology:cs_reference_frame('0a679cbd-5eb9-4054-aa70-d0a1fcb55831', diaspora_textual_hebrew).
narrative_ontology:cs_drift_state('0a679cbd-5eb9-4054-aa70-d0a1fcb55831', modern_israeli_hebrew_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0a679cbd-5eb9-4054-aa70-d0a1fcb55831', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, religious_institutional_authority).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, textual_tradition_keepers).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secular_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, generative_language_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, diaspora_jewish_communities).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinical authorities, synagogue systems, and religious educational frameworks that codify, transmit, and enforce the liturgical reading of Hebrew. They define correct recitation, gate access to textual interpretation, and maintain the boundary between sacred/preserved Hebrew and vernacular approximations. They collect institutional legitimacy and continuity authority from this role.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, religious_institutional_authority, agenda_setter,
    institutional, civilizational, mobile, global).

% Scholars, liturgists, cantors, yeshiva instructors, and continuity advocates who specialize in preserving and transmitting the textual corpus exactly as received. Their professional and spiritual identity fuses with the preservation mandate. They benefit from being the authoritative class over this domain and from the constraint's operation protecting their interpretive monopoly.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, textual_tradition_keepers, beneficiary,
    organized, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, textual_tradition_keepers, agenda_setter).

% Modern Hebrew speakers in Israel and diaspora communities who use Hebrew as a living, generative language—in daily conversation, literature, technology, colloquial expression. They bear the cost of the liturgical constraint as a delegitimization of their lived usage: their everyday Hebrew is marked as 'not real Hebrew' when it deviates from liturgical forms. They are constrained because full participation in Jewish institutional and cultural life requires acknowledging the liturgical standard.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secular_hebrew_speakers, payer,
    moderate, biographical, constrained, national).

% Native speakers and linguistic innovators (writers, tech workers, educators, ordinary Israelis) who drive the living evolution of Hebrew—new vocabulary, syntax, pragmatic registers. The liturgical-preservation constraint suppresses their authority to define Hebrew's evolution; institutional validation flows instead to those who anchor meaning in fixed texts. They have arbitrage options (code-switching, linguistic innovation outside institutional channels) but these require renouncing the institutional legitimacy the constraint controls.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, generative_language_communities, payer,
    powerful, biographical, arbitrage, global).

% State education systems, civil society organizations, and cultural institutions that promote Hebrew as a national living language uncoupled from liturgical control. They are structurally excluded from defining Hebrew's official continuity; their framing of Hebrew as a generative, secular national asset competes with the religious institutional reading but is delegitimized by the liturgical-preservation constraint's authority structure.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secularizing_institutional_forces, excluded,
    institutional, generational, trapped, national).

% Jewish communities outside Israel who maintain Hebrew literacy primarily through liturgical study and ritual. They benefit from the constraint because it provides a stable, centralized standard that does not require native-speaker competence or daily use—one can participate in Jewish life through liturgical Hebrew alone. They also bear costs: the liturgical standard is narrow, difficult to innovate within, and does not support generative use for secular cultural expression.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, diaspora_jewish_communities, payer).

% Linguists, historical scholars, and sociolinguistic researchers who document Hebrew's actual usage patterns, diachronic change, and competing standardization claims. They occupy no seat in the enforcement machinery but provide the analytical frame for measuring divergence between the liturgical-preservation claim and the empirical reality of Hebrew's polycentric evolution.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, linguistic_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__liturgical_preservation, religious_institutional_authority).
narrative_ontology:fixing_cost_class(hebrew_continuity__liturgical_preservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of maintaining Hebrew textual continuity and interpretive consensus across dispersed Jewish communities separated by geography, time, and language environment. A fixed liturgical standard requires no living speech community and survives diaspora fragmentation because meaning is anchored in received texts and authorized interpretation, not in native-speaker intuition or daily generative use.
% TRANSFER_FUNCTION: Moves linguistic authority from living speakers to textual authorities and religious institutions. The arrangement transfers the power to define correct Hebrew, authorize innovation, and control prestige-register usage from generative communities to keepers of fixed tradition. It extracts from speakers who innovate outside the approved channels (they must renounce their innovations or lose institutional legitimacy) and transfers authority-as-extraction to the institutional gatekeepers.
% ABSENT_VOICES: Secular Israeli linguists and writers, generative-use communities, and state education systems are structurally excluded from defining Hebrew's official continuity narrative. They are present as actors but excluded from the authoritative framing—their competence in living Hebrew is marked as incomplete or inauthentic by the liturgical standard. A voice that would be included: historical linguists documenting that Hebrew's diachronic change (Ancient→Mishnaic→Medieval→Modern) has always been driven by generative innovation, not liturgical preservation alone.
% DISAPPEARANCE_RATIONALE: If the liturgical-preservation constraint vanished, Hebrew would continue through generative use without loss of continuity (Modern Israeli Hebrew already operates this way in daily life). The constraint's disappearance would mean institutional authority over Hebrew's definition would no longer concentrate in religious institutions; linguistic innovation would face no delegitimization penalty; and secular educational systems would have equal standing in defining Hebrew's standards. The reorganization would be immediate: state schools would teach Hebrew as a living language rather than as an approximation of a fixed liturgical form; linguistic innovation would accelerate; generative communities would claim their authority.
% FOUNDING_PROBLEM: After the Jewish diaspora scattered the community across the Mediterranean and beyond (post-586 BCE), Hebrew ceased to be a daily vernacular for most Jews. The problem: how to maintain Hebrew literacy, collective identity, and textual access across generations in communities where the language is not natively spoken? A preserved textual standard solved this—you could be Jewish and maintain Hebrew without being born into a Hebrew-speaking household.
% FOUNDING_PROBLEM_CORROBORATION: Religious institutional authorities attest the founding problem is still live—without liturgical preservation, Hebrew literacy would collapse in diaspora communities. Secular Israeli speakers and linguists attest the founding problem is substantially solved: Modern Hebrew proves that living generative communities maintain Hebrew continuity without liturgical fixation, and that daily use is a more robust transmission mechanism than textual preservation alone (generative communities are the actual source of all of Hebrew's continuous change and innovation across its history). Independent linguistic scholarship corroborates the latter reading: diachronic studies show Hebrew has always evolved through generative use; the liturgical constraint is a later institutional overlay, not the original transmission mechanism.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the constraint transfers linguistic authority from living speakers to textual gatekeepers—an extraction of the right to innovate and define correctness. Suppression is high (0.72) because the constraint's persistence depends on actively delegitimizing generative usage, excluding secular institutional framings, and maintaining the boundary between 'correct liturgical Hebrew' and 'incomplete vernacular approximations.' Theater is elevated (0.58, above the tangled-rope median) because the constraint increasingly operates as performative maintenance rather than functional transmission: as Modern Israeli Hebrew proves that generative communities actually sustain the language without liturgical fixation, the liturgical standard shifts toward symbolic/identity function and away from practical necessity. The measurement series track three decades of increasing theater and suppression as the constraint's functional justification erodes but institutional enforcement intensifies to maintain it. Accessibility collapse is high (0.79) because once the liturgical reading is institutionally established, alternatives (generative standards, secular definitions of correct Hebrew) are effectively foreclosed in institutional contexts—you cannot claim Hebrew authority without engaging the textual tradition, even if you reject the authority structure. Resistance is moderate (0.61) because generative communities actively resist the constraint through linguistic innovation, literary deviation from liturgical norms, and state educational systems promoting living-language standards, but this resistance has not yet dislodged the institutional authority structure.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (religious institutional authority) experiences the constraint as a genuine coordination solution and identity-preservation mechanism—essential infrastructure for Jewish continuity. The payer seats (generative communities, secular speakers) experience the same constraint as enforced delegitimization of their linguistic competence and suppression of their authority to define Hebrew's evolution. The gap is not in facts but in structural position: from the institutional seat, the constraint is coordination; from the suppressed-innovation seat, it is extraction. The engine computes this divergence from the structural data—the authored metrics do not arbitrate between the readings, only describe the operation's extractive intensity from the payer's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutional authority benefits from maintaining exclusive gatekeeper status over Hebrew definition; they face no cost from enforcement and high cost from the constraint's removal—their authority derives from it. This generates d ≈ 0.15 (strong beneficiary). Textual tradition keepers (scholars, cantors, liturgists) are secondary beneficiaries but critically identity-locked: their entire professional and spiritual identity is constituted through preservation work. Exit would require renouncing the frame they are embedded in. d ≈ 0.25–0.35 (beneficiary with internalized suppression). Secular Hebrew speakers and generative language communities (writers, tech workers, ordinary Israelis) are the targets: they must renounce or suppress their linguistic innovations to receive institutional legitimacy. They are constrained by institutional control of prestige-register access. d ≈ 0.75–0.85 (high targets). Diaspora communities are mixed: d ≈ 0.45 (they depend on the standard for access without native birth, but constrained by its narrowness).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to preserve Hebrew literacy and continuity across diaspora generations without requiring native-speaker birth—a genuine founding problem that the liturgical model solved for two thousand years. However, Modern Israeli Hebrew demonstrates that generative communities are the actual durable transmission mechanism: Hebrew has evolved continuously through native-speaker innovation, not through liturgical fixation. The evidence (Modern Israeli Hebrew exists; generative use is more robust to generational transmission than textual memorization; linguists document living-language mechanisms as the source of all historical Hebrew change) suggests the founding problem is substantially solved and the constraint persists primarily as a defense of institutional authority over language definition, not as a necessary transmission mechanism. This is not a false claim about facts (the liturgical reading is empirically coherent) but a claim whose functional justification has been overtaken: the constraint now operates more to control who may define Hebrew than to sustain Hebrew continuity itself. The theater_ratio rise (0.42 → 0.58) tracks this drift from functional necessity to performative maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_fixation_vs_living_language,
    'Is Hebrew''s continuity fundamentally dependent on textual preservation and authorized interpretation, or on generative living-language communities, or on both equally?',
    'Comparative historical analysis: Modern Israeli Hebrew demonstrates generative transmission works without liturgical fixation. Conversely, Medieval and Early Modern diaspora Hebrew communities maintained literacy through fixed texts when generative use was minimal. The empirical signal is the direction and magnitude of Hebrew''s change: if change is driven by generative innovation (Modern Israeli evidence shows it is), then the generative mechanism is the actual continuity backbone, and liturgical fixation is secondary.',
    'If generative communities are the primary continuity mechanism, the liturgical-preservation constraint is a Piton—an institutional overlay whose functional justification has atrophied. The constraint would reclassify from Tangled Rope (mixed coordination/extraction) toward Snare (pure institutional defense). If liturgical fixation proves essential (e.g., diaspora communities cannot sustain Hebrew without it), then the constraint''s extraction is a necessary cost of coordination, and the Tangled Rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_fixation_vs_living_language, empirical, 'Whether textual fixation or living generative use is the primary transmission mechanism.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.72) primarily structural (external institutional barriers) or internalized (speakers have adopted the liturgical standard as their own identity, making self-suppression automatic)?',
    'Post-exit trajectory analysis: if secular Hebrew speakers or linguists removed from institutional contexts cease to experience suppression and freely innovate, suppression was primarily structural. If they continue to internalize liturgical norms even without external enforcement, suppression is internalized.',
    'Structural suppression is removable through institutional reform. Internalized suppression persists after the constraint is removed, and targets carry it forward into new arrangements. If suppression is substantially internalized, the constraint''s effective hold is stronger than the raw 0.72 metric suggests—it has colonized the speaker''s own sense of linguistic authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether measured suppression is structural or internalized.').

omega_variable(
    reading_boundary_ambiguity,
    'Does the liturgical-preservation reading remain distinct from the native-generative reading, or have they begun to merge into a single hybrid standard incorporating both fixed texts and generative innovation (Modern Israeli Hebrew''s actual practice)?',
    'Institutional authority mapping: do religious institutions accept Modern Hebrew linguistic innovation as ''correct Hebrew,'' or do they continue to subordinate it to liturgical norms? If Modern Israeli Hebrew is fully accepted as legitimate, the readings have merged; if it remains marked as incomplete or vernacular, the readings are still distinct.',
    'If readings have merged, the constraint''s functional distinction collapses—there is no longer a contest between readings, only a single standard that incorporates both. The constraint''s purpose (to defend the liturgical reading against native generative reading) becomes moot. If readings remain distinct, the constraint continues to serve as a institutional defense mechanism for liturgical authority against generative innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_ambiguity, conceptual, 'Whether the liturgical-preservation and native-generative readings remain institutionally distinct or have begun to merge.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hebr_tr_t5, hebrew_continuity__liturgical_preservation, theater_ratio, 5, 0.46).
narrative_ontology:measurement(hebr_tr_t10, hebrew_continuity__liturgical_preservation, theater_ratio, 10, 0.51).
narrative_ontology:measurement(hebr_tr_t15, hebrew_continuity__liturgical_preservation, theater_ratio, 15, 0.54).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__liturgical_preservation, theater_ratio, 20, 0.56).
narrative_ontology:measurement(hebr_tr_t25, hebrew_continuity__liturgical_preservation, theater_ratio, 25, 0.58).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(hebr_be_t5, hebrew_continuity__liturgical_preservation, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(hebr_be_t10, hebrew_continuity__liturgical_preservation, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(hebr_be_t15, hebrew_continuity__liturgical_preservation, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__liturgical_preservation, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(hebr_be_t25, hebrew_continuity__liturgical_preservation, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(hebr_su_t5, hebrew_continuity__liturgical_preservation, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(hebr_su_t10, hebrew_continuity__liturgical_preservation, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(hebr_su_t15, hebrew_continuity__liturgical_preservation, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__liturgical_preservation, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(hebr_su_t25, hebrew_continuity__liturgical_preservation, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__liturgical_preservation, 0.12).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% The hebrew_continuity kernel admits three structurally distinct readings, each instantiated as a separate constraint: (1) liturgical_preservation (this file) — Hebrew persists through fixed textual transmission, zero native speakers required; (2) native_generative (sibling) — Hebrew persists only through native-speaker intuition and living use; (3) bridge_pidginized (sibling) — Hebrew persists as a contact language for diaspora interaction. Each reading has different ε, different beneficiaries, different victim sets. The three readings coexist as live institutional and community positions; none logically forecloses the others, though they compete for legitimacy and resource control. Link all three via network.affects_constraints for corpus analysis of kernel-reading divergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__liturgical_preservation, organized, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
