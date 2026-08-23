% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Hebrew Linguistic Life â Liturgical Preservation Reading
 *   domain: sociolinguistic/religious/nationalist
 *
 * SUMMARY:
 *   This is the liturgical-preservation reading of the contested kernel
 *   'hebrew_linguistic_life': Hebrew is alive precisely when its sacred texts
 *   are continuously recited, studied, and transmitted in an unbroken chain,
 *   regardless of vernacular use. On this reading, Hebrew never died; the
 *   Zionist revival led by Ben-Yehuda is not resurrection but desecration of
 *   a living sacred tradition. The constraint operates as an
 *   institutionalized definitional gate that delegates modern vernacular
 *   Hebrew to illegitimacy while consecrating liturgical practice as the sole
 *   authentic life of the language. The claimed type is tangled_rope because
 *   the constraint carries a genuine coordination function (diasporic
 *   religious continuity) alongside asymmetric extraction (delegitimization
 *   of secular speakers and the revival project).
 *
 * KEY AGENTS:
 *   - Liturgical authorities (agenda_setter / institutional / identity_locked): administer transmission norms and frame secular revival as desecration.
 *   - Traditional recitation communities (beneficiary / organized / identity_locked): their practice is valorized as the only authentic Hebrew life.
 *   - Modern Hebrew speakers (payer / organized / constrained): bear normative delegitimization of their vernacular practice.
 *   - Zionist revivalists (payer / powerful / constrained): their resurrection narrative is reframed as desecration of an unbroken living tradition.
 *   - Sacred tradition (excluded / non-agent): declared victim of secular appropriation, spoken for by liturgical authorities.
 *   - Comparative linguists (observer / analytical): empirically classify modern Hebrew as a revival language, contradicting the reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__liturgical_preservation_reading, 0.68).
domain_priors:suppression_score(hebrew_linguistic_life__liturgical_preservation_reading, 0.72).
domain_priors:theater_ratio(hebrew_linguistic_life__liturgical_preservation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(hebrew_linguistic_life__liturgical_preservation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__liturgical_preservation_reading, "Hebrew Linguistic Life â Liturgical Preservation Reading").
narrative_ontology:topic_domain(hebrew_linguistic_life__liturgical_preservation_reading, "sociolinguistic/religious/nationalist").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__liturgical_preservation_reading, '9b715da3-2feb-419f-89e4-c8adb5f6b733').
narrative_ontology:cs_kernel_codification('9b715da3-2feb-419f-89e4-c8adb5f6b733', fixed_text).
narrative_ontology:cs_authority_grounding('9b715da3-2feb-419f-89e4-c8adb5f6b733', lineage).
narrative_ontology:cs_interpretation_layer_present('9b715da3-2feb-419f-89e4-c8adb5f6b733').
narrative_ontology:cs_reading_relation('9b715da3-2feb-419f-89e4-c8adb5f6b733', hebrew_linguistic_life__native_generational_reading, forecloses).
narrative_ontology:cs_reading_relation('9b715da3-2feb-419f-89e4-c8adb5f6b733', hebrew_linguistic_life__marketplace_pidgin_reading, forecloses).
narrative_ontology:cs_axiom('9b715da3-2feb-419f-89e4-c8adb5f6b733', foundational, liturgical_continuity_equals_linguistic_life).
narrative_ontology:cs_axiom_status(liturgical_continuity_equals_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('9b715da3-2feb-419f-89e4-c8adb5f6b733', liturgical_continuity_equals_linguistic_life, theological).
narrative_ontology:cs_axiom('9b715da3-2feb-419f-89e4-c8adb5f6b733', foundational, vernacular_revival_is_desecration).
narrative_ontology:cs_axiom_status(vernacular_revival_is_desecration, holdable).
narrative_ontology:cs_axiom_grounding('9b715da3-2feb-419f-89e4-c8adb5f6b733', vernacular_revival_is_desecration, theological).
narrative_ontology:cs_reference_frame('9b715da3-2feb-419f-89e4-c8adb5f6b733', unbroken_liturgical_chain).
narrative_ontology:cs_drift_state('9b715da3-2feb-419f-89e4-c8adb5f6b733', zionist_revival_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('9b715da3-2feb-419f-89e4-c8adb5f6b733', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__liturgical_preservation_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authorities).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__liturgical_preservation_reading, traditional_recitation_communities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, modern_hebrew_speakers).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, zionist_revivalists).
narrative_ontology:constraint_victim(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the norms of correct textual recitation, pronunciation, and halakhic interpretation; certify what counts as legitimate Hebrew use. Frame Ben-Yehuda's secular revival as desecration and the modern vernacular as a corruption of the unbroken chain. Their authority depends on maintaining the exclusivity of the liturgical definition of linguistic life.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Maintain the unbroken chain of sacred text study and recitation across diaspora. Their practice is valorized as the sole locus of Hebrew's authentic life. Exiting this constraint would mean abandoning a core component of religious identity and communal belonging.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, traditional_recitation_communities, beneficiary,
    organized, generational, identity_locked, global).

% Use Hebrew as a daily vernacular for secular, mundane, and creative purposes. Under the liturgical reading, their speech is delegitimized as non-authentic or desecratory; they bear the normative cost of having their linguistic practice classified as a violation rather than a continuation of the language.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, modern_hebrew_speakers, payer,
    organized, biographical, constrained, national).

% Institutionalized the modern Hebrew vernacular through political and educational infrastructure. Their founding narrative frames Hebrew as a dead language that was resurrected. The liturgical reading rejects this narrative as desecration, imposing a persistent legitimacy deficit on the revival project and its institutions.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, zionist_revivalists, payer,
    powerful, generational, constrained, national).

% The corpus of sacred texts and the unbroken transmission chain themselves, which the reading declares to be the victim of secular appropriation. Cannot speak in its own defense; its interests are mediated entirely by liturgical authorities.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition, excluded,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(hebrew_linguistic_life__liturgical_preservation_reading, sacred_tradition).

% Study language vitality and revival empirically. Typically classify modern Hebrew as the canonical successful language revival, directly contradicting the liturgical reading's claim that Hebrew never died and therefore could not have been revived.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__liturgical_preservation_reading, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__liturgical_preservation_reading, liturgical_authorities).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the unbroken chain of sacred textual transmission across diaspora and generations, coordinating communal identity, religious continuity, and educational curricula in the absence of a shared territorial polity or vernacular.
% TRANSFER_FUNCTION: Moves the definition of 'living Hebrew' and the authority to legitimate its use from secular vernacular speakers and Zionist institutions to liturgical gatekeepers and traditional recitation networks.
% ABSENT_VOICES: Rival secular Hebrew academies and modern vernacular poets are present in the national public sphere but excluded from the liturgical legitimacy conversation. The sacred texts themselves are spoken for but cannot speak.
% DISAPPEARANCE_RATIONALE: If the liturgical-preservation definition vanished, traditional communities would lose their exclusive claim to authentic Hebrew continuity, modern Hebrew speakers would be relieved of the delegitimization burden, and the Zionist revival narrative would go uncontested within the religious frame. Diaspora educational structures and boundary-maintenance practices would reorganize around different identity markers.
% FOUNDING_PROBLEM: Maintenance of Jewish religious identity and textual continuity during diaspora without a shared territorial vernacular or political sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Traditional rabbinic authorities and diaspora communities attest to the continuity problem from within the beneficiary set. External secular historians acknowledge the historical rupture and the strategic use of liturgical continuity, though they dispute the normative claim that this is the only valid form of linguistic life.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__liturgical_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__liturgical_preservation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__liturgical_preservation_reading, 0.68, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.68) is high because the constraint systematically transfers legitimacy from secular vernacular use to liturgical gatekeepers. Suppression (0.72) is higher: the reading's persistence depends on actively suppressing the counter-narrative that Hebrew was dead and revived, and on maintaining institutional control over religious education and certification. Theater_ratio (0.45) reflects that while the transmission chain is functionally real, an increasing share of institutional energy is devoted to boundary-policing against secular desecration rather than to transmission itself. Accessibility_collapse (0.65) indicates that alternatives (modern Hebrew legitimacy) are heavily stigmatized but not fully extinguished. Resistance (0.55) captures the active pushback from Zionist institutions and modern speakers. The time series show extraction rising as the modern vernacular gains demographic dominance, forcing the liturgical reading to work harder to maintain its definitional exclusivity.
 *
 * PERSPECTIVAL GAP:
 *   From the liturgical-authority seat, the constraint is genuine coordination protecting a sacred inheritance from desecration. From the modern-speaker and Zionist-revivalist seats, the same structure operates as enforced extraction of legitimacy â their speech is taxed with a deficit of authenticity that only liturgical authorities can waive. The engine computes this divergence from the structural data: agenda-setters with identity-locked exit versus payers with constrained exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Liturgical authorities and traditional communities are structural beneficiaries (low d): the constraint subsidizes their authority and valorizes their practice. Modern Hebrew speakers and Zionist revivalists are structural targets (high d): the constraint extracts legitimacy from their practice and reframes their historical achievement as damage. Sacred tradition is authored as victim but is a non-agent, so it does not feed directionality; the human payers carry the effective extraction. Comparative linguists are analytical observers (d near 0.5, symmetric observation).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mandatrophy mislabeling by preserving the genuine coordination function: the unbroken liturgical chain really did solve the diasporic continuity problem. However, the constraint has become tangled because the same institutional structure that coordinates transmission now extracts from the vernacular revival, reframing a new form of Hebrew life as desecration rather than expansion. The coordination and extraction are inseparable: the exclusivity claim ('only this is life') is what makes the coordination function a constraint on others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_relations,
    'How does the liturgical preservation reading relate structurally to the native generational and marketplace pidgin readings of the same kernel?',
    'Cross-reading logical analysis: the ''regardless of vernacular use'' clause directly contradicts the necessity claims of both sibling readings, supporting forecloses relations.',
    'Determines whether the kernel generates a constraint family with irreducible logical contradictions or merely competitive framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_relations, conceptual, 'Sibling reading structural relation for kernel hebrew_linguistic_life').

omega_variable(
    sacred_tradition_as_victim,
    'Is the victim of this constraint the non-agent sacred tradition itself, or the human agents whose practice is delegitimized by the liturgical framing?',
    'Directionality analysis: non-agents cannot bear effective extraction; if the tradition is the referent victim, the cost must map to the human agents who identify with and transmit it.',
    'If human agents bear all effective extraction, directionality shifts toward institutional power asymmetries; if symbolic harm to tradition is structurally extracted, the constraint functions partly as symbolic defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacred_tradition_as_victim, conceptual, 'Ambiguity of victimhood between abstract tradition and human agents').

omega_variable(
    liturgical_coordination_genuine,
    'Does the liturgical transmission chain solve a genuine coordination problem, or is the coordination story cover for institutional extraction?',
    'Historical counterfactual: would Jewish textual continuity have collapsed without the liturgical-preservation norm, or was the norm selected for gatekeeping capacity?',
    'Genuine coordination supports the tangled_rope classification; purely cover-like operation would push the constraint toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_coordination_genuine, empirical, 'Whether the liturgical coordination function is genuine or cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__liturgical_preservation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(hebr_tr_t60, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(hebr_tr_t80, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 80, 0.44).
narrative_ontology:measurement(hebr_tr_t100, hebrew_linguistic_life__liturgical_preservation_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(hebr_be_t60, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(hebr_be_t80, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 80, 0.67).
narrative_ontology:measurement(hebr_be_t100, hebrew_linguistic_life__liturgical_preservation_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hebr_su_t20, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(hebr_su_t40, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(hebr_su_t60, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(hebr_su_t80, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 80, 0.71).
narrative_ontology:measurement(hebr_su_t100, hebrew_linguistic_life__liturgical_preservation_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, native_generational_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__liturgical_preservation_reading, marketplace_pidgin_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'Hebrew linguistic life' decomposes into three structurally distinct constraints per the epsilon-invariance principle. This story (liturgical_preservation_reading) has high extraction and a fixed-text kernel; siblings have different epsilon values, different beneficiary/victim structures, and different kernel codifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
