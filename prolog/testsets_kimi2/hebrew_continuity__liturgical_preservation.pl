% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Hebrew Continuity via Liturgical Preservation Reading
 *   domain: sociolinguistic/commitment_systems
 *
 * SUMMARY:
 *   This constraint story captures the liturgical_preservation reading of the
 *   contested hebrew_continuity kernel: the claim that Hebrew survives as a
 *   living entity exclusively through preserved ritual recitation and textual
 *   transmission, requiring zero native speakers. Under this reading, the
 *   language's vitality is indexed to sacred performance and unbroken copying
 *   and reading chains rather than to generative vernacular use. The
 *   constraint coordinates a global diasporic community around a shared
 *   textual heritage, but simultaneously extracts legitimacy from modern
 *   Hebrew (Ivrit) advocates and secularizing movements by denying their
 *   alternative criterion of language life. The structural asymmetry places
 *   rabbinic transmission authorities in the agenda-setting seat, traditional
 *   communities as beneficiaries of collective identity, and modern Hebrew
 *   proponents as targets of delegitimization.
 *
 * KEY AGENTS:
 *   - rabbinic_transmission_authorities (agenda_setter, institutional, constrained exit) â control textual norms, scribal standards, and liturgical certification
 *   - traditional_liturgical_communities (beneficiary, organized, identity_locked) â receive group cohesion and historical continuity through ritual Hebrew
 *   - modern_hebrew_proponents (payer, organized, mobile) â bear the delegitimization of native generative competence
 *   - secular_jewish_movements (payer, moderate, constrained) â suppressed as threats to textual tradition
 *   - sociolinguistic_observers (observer, analytical, analytical exit) â document the contest between sacred and vernacular readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.62).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.58).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Continuity via Liturgical Preservation Reading").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistic/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, '9ffdce99-3010-44f2-8f10-ecc2edbf1578').
narrative_ontology:cs_kernel_codification('9ffdce99-3010-44f2-8f10-ecc2edbf1578', fixed_text).
narrative_ontology:cs_authority_grounding('9ffdce99-3010-44f2-8f10-ecc2edbf1578', lineage).
narrative_ontology:cs_interpretation_layer_present('9ffdce99-3010-44f2-8f10-ecc2edbf1578').
narrative_ontology:cs_reading_relation('9ffdce99-3010-44f2-8f10-ecc2edbf1578', hebrew_continuity__native_generative, forecloses).
narrative_ontology:cs_reading_relation('9ffdce99-3010-44f2-8f10-ecc2edbf1578', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('9ffdce99-3010-44f2-8f10-ecc2edbf1578', foundational, textual_transmission_suffices_for_continuity).
narrative_ontology:cs_axiom_status(textual_transmission_suffices_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('9ffdce99-3010-44f2-8f10-ecc2edbf1578', textual_transmission_suffices_for_continuity, theological).
narrative_ontology:cs_axiom('9ffdce99-3010-44f2-8f10-ecc2edbf1578', foundational, native_vernacular_unnecessary).
narrative_ontology:cs_axiom_status(native_vernacular_unnecessary, holdable).
narrative_ontology:cs_axiom_grounding('9ffdce99-3010-44f2-8f10-ecc2edbf1578', native_vernacular_unnecessary, deontological).
narrative_ontology:cs_reference_frame('9ffdce99-3010-44f2-8f10-ecc2edbf1578', sacred_textual_authority).
narrative_ontology:cs_drift_state('9ffdce99-3010-44f2-8f10-ecc2edbf1578', post_modern_hebrew_revival, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9ffdce99-3010-44f2-8f10-ecc2edbf1578', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, traditional_liturgical_communities).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, modern_hebrew_proponents).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secular_jewish_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, rabbinic_transmission_authorities).
narrative_ontology:constraint_vindicates(hebrew_continuity__liturgical_preservation, textual_chain_unbroken_hypothesis).
narrative_ontology:constraint_vindicates(hebrew_continuity__liturgical_preservation, liturgical_sufficiency_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the norms of correct textual recitation, scribal standards, liturgical Hebrew pronunciation, and canonical interpretation. They certify teachers, cantors, and scribes, and their institutional authority depends on the exclusivity of the unbroken textual chain.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, rabbinic_transmission_authorities, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, rabbinic_transmission_authorities, beneficiary).

% Diaspora and Israeli communities whose Jewish practice centers on Hebrew prayer, Torah reading, and liturgical study. They receive group cohesion, historical continuity, and sacred identity from the constraint. Leaving the liturgical framework means abandoning a core constitutive practice.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, traditional_liturgical_communities, beneficiary,
    organized, generational, identity_locked, global).

% Educators, writers, and state institutions promoting Modern Hebrew (Ivrit) as a native, creative, secular language. Their claim that Hebrew lives through generative daily use is structurally delegitimized by the liturgical reading's definitional monopoly.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, modern_hebrew_proponents, payer,
    organized, generational, mobile, national).

% Political and cultural movements seeking to ground Jewish identity in civic nationhood, territorial sovereignty, or secular culture rather than rabbinic textual authority. Under this reading they are cast as threats to Hebrew continuity, and their alternative cultural programs face systematic legitimacy deficits.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secular_jewish_movements, payer,
    moderate, biographical, constrained, national).

% Researchers studying language revival, language death, and the unique sociolinguistic trajectory of Hebrew. They document the structural contest between liturgical preservation and native generative norms without participating in the legitimacy dispute.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, sociolinguistic_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__liturgical_preservation, rabbinic_transmission_authorities).
narrative_ontology:fixing_cost_class(hebrew_continuity__liturgical_preservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a scattered diasporic community's shared textual heritage across generations without requiring territorial concentration, solving the coordination problem of maintaining group identity and sacred continuity through liturgical performance.
% TRANSFER_FUNCTION: Moves cultural legitimacy, educational resources, and communal authority from generative and secular language users to liturgical specialists and textual communities, while transferring the burden of preservation onto community members who must maintain ritual competence.
% ABSENT_VOICES: Native Hebrew-speaking poets, secular Israeli schoolchildren, Yiddish revivalists, and vernacular Jewish cultural producers are absent from the liturgical framing; they would assert that Hebrew lives in playground speech, battlefield slang, and modern literature, not only in sanctified recitation.
% DISAPPEARANCE_RATIONALE: If the liturgical preservation reading vanished overnight, traditional communities would lose their primary cohesion mechanism, rabbinic authority over Hebrew legitimacy would weaken, and the native generative reading would claim uncontested definitional authority â the sociolinguistic and institutional map of Hebrew would rearrange around Ivrit.
% FOUNDING_PROBLEM: Dispersion of Jews across diaspora communities with loss of a territorial base and vernacular Hebrew speech, creating a risk of total language death without a preservation mechanism.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists and Zionist historiographers attest the pre-modern decline of Hebrew vernaculars; however, modern Hebrew sociolinguists and census authorities outside the beneficiary set attest that the founding problem has been solved by the native revival in the late nineteenth and twentieth centuries.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is moderately high because the liturgical reading must actively suppress the empirical reality of Modern Hebrew native speech to maintain its definitional monopoly; this is not merely coexistence but a contest over legitimacy. Suppression (0.58) reflects the institutional and ideological work required to render vernacular Hebrew profane or secondary within traditional communities. Theater ratio (0.42) captures the growing performative dimension of liturgical Hebrew as everyday competence shifts to Modern Hebrew. Accessibility collapse (0.70) is high because within the traditional framework, the native alternative is rendered invisible or illegitimate. Resistance (0.55) reflects the successful institutionalization of Modern Hebrew in Israel and global secular Jewish culture. The founding problem â preventing Hebrew language death in diaspora â is dead, but the arrangement persists, generating a mandatrophy signal via the founding_problem_status x disappearance_verdict mismatch.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority seat experiences this constraint as necessary coordination of a scattered people around an immutable heritage; the modern Hebrew proponent seat experiences it as an extractive gatekeeping mechanism that denies their linguistic reality. The engine computes this divergence from identical structural data via directionality: the agenda_setter and beneficiary seats derive low d (subsidy), while the victim seats derive high d (extraction). The sociolinguistic observer seat sits near d=0.5, seeing both coordination and extraction simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (traditional_liturgical_communities) are identity_locked and organized, pulling their d toward the full-beneficiary end. Victims (modern_hebrew_proponents with mobile exit, secular_jewish_movements with constrained exit) have their d pulled toward the full-target end by the victim declaration, though mobile exit moderates the modern Hebrew seat. The agenda_setter (rabbinic_transmission_authorities) subsidizes its own authority through the constraint. No override is required because the structural derivation matches the known asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â Hebrew language death in dispersion â is sociolinguistically dead: Modern Hebrew has acquired native speakers, a standard language infrastructure, and state backing. Yet the liturgical preservation reading persists as an authority structure. This creates a classic mandatrophy pattern: the constraint's coordination function (preserving diasporic identity) has atrophied into an extraction mechanism (defining Hebrew authenticity against the native revival). The persistence is driven by institutional identity fusion rather than by the original problem. The theater_ratio measurement series shows this drift â performative maintenance rises as functional necessity falls.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_speech_legitimacy_foreclosure,
    'Does the liturgical reading logically foreclose the native generative reading within a single party''s framework, or can they be compartmentalized by the same actors?',
    'Survey and ethnographic interview of traditional community members who also speak Modern Hebrew, measuring cognitive dissonance versus seamless compartmentalization.',
    'If foreclosed, the constraint''s effective suppression is higher than modeled; if compartmentalized, the two readings are less extractive toward each other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speech_legitimacy_foreclosure, conceptual, 'Whether liturgical and native readings are mutually exclusive or cohabitable').

omega_variable(
    extraction_beneficiary_concentration,
    'Do the gains of liturgical preservation accrue to institutional rabbinic authorities or diffuse across the liturgical community?',
    'Economic and network analysis of resource flows: yeshiva funding, religious institutional budgets, and authority prestige metrics.',
    'If concentrated, directionality for rabbinic authorities is lower (strong beneficiary); if diffuse, the community itself is the primary beneficiary seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_beneficiary_concentration, empirical, 'Concentration versus diffusion of extraction benefits').

omega_variable(
    suppression_internalization,
    'Is the delegitimization of Modern Hebrew within traditional communities structurally enforced or cognitively internalized?',
    'Post-exit ethnographic observation: do individuals leaving the liturgical community immediately adopt Modern Hebrew, or does residual stigma persist?',
    'If internalized, the constraint''s effective suppression exceeds the structural measure because the target carries the suppression after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hebr_tr_t10, hebrew_continuity__liturgical_preservation, theater_ratio, 10, 0.3).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__liturgical_preservation, theater_ratio, 20, 0.45).
narrative_ontology:measurement(hebr_tr_t30, hebrew_continuity__liturgical_preservation, theater_ratio, 30, 0.55).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__liturgical_preservation, theater_ratio, 40, 0.58).
narrative_ontology:measurement(hebr_tr_t50, hebrew_continuity__liturgical_preservation, theater_ratio, 50, 0.6).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(hebr_be_t10, hebrew_continuity__liturgical_preservation, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__liturgical_preservation, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(hebr_be_t30, hebrew_continuity__liturgical_preservation, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__liturgical_preservation, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(hebr_be_t50, hebrew_continuity__liturgical_preservation, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(hebr_su_t10, hebrew_continuity__liturgical_preservation, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__liturgical_preservation, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(hebr_su_t30, hebrew_continuity__liturgical_preservation, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__liturgical_preservation, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(hebr_su_t50, hebrew_continuity__liturgical_preservation, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% This constraint is the liturgical_preservation reading of the hebrew_continuity kernel, distinct from the native_generative and bridge_pidginized readings, which carry different epsilon values, beneficiary structures, and scope profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
