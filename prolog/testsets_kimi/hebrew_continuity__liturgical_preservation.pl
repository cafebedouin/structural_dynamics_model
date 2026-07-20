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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Hebrew Liturgical Preservation Regime
 *   domain: sociolinguistic/religious/commitment_system
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical_preservation reading of the
 *   contested hebrew_continuity kernel: the claim that Hebrew persists as a
 *   living language not through native speaker communities or daily
 *   generative use, but through preserved ritual recitation and textual
 *   transmission across diaspora Jewish communities. The constraint is
 *   administered by rabbinic authorities who control liturgical standards,
 *   orthography, and educational certification, while extracting definitional
 *   legitimacy from modern Hebrew movements and secularizing institutions. It
 *   coordinates genuine transnational liturgical uniformity but
 *   asymmetrically concentrates interpretive authority and suppresses
 *   alternative continuity claims (notably the native_generative reading).
 *   The authored metrics describe a constraint with moderate-high extraction
 *   and active enforcement; the claimed type is tangled_rope. Divergence
 *   between claim and metrics is intentional and diagnostic.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: Agenda-setter (institutional/constrained) â controls textual and liturgical standards, adjudicates Hebrew legitimacy
 *   - traditional_communities: Beneficiary (organized/identity_locked) â receives transnational liturgical coordination, bears educational costs
 *   - liturgical_specialists: Payer (moderate/identity_locked) â performs the labor of recitation and scribal transmission
 *   - modern_hebrew_proponents: Excluded/victim (moderate/mobile) â advocates for native generative Hebrew, delegitimized by the liturgical framework
 *   - comparative_linguists: Observer (analytical/analytical) â studies the sociolinguistic structure from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.62).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.58).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.62).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Hebrew Liturgical Preservation Regime").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistic/religious/commitment_system").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, '498c0740-04e2-48fa-98c0-a93dc78f1674').
narrative_ontology:cs_kernel_codification('498c0740-04e2-48fa-98c0-a93dc78f1674', fixed_text).
narrative_ontology:cs_authority_grounding('498c0740-04e2-48fa-98c0-a93dc78f1674', lineage).
narrative_ontology:cs_interpretation_layer_present('498c0740-04e2-48fa-98c0-a93dc78f1674').
narrative_ontology:cs_reading_relation('498c0740-04e2-48fa-98c0-a93dc78f1674', hebrew_continuity__native_generative, forecloses).
narrative_ontology:cs_reading_relation('498c0740-04e2-48fa-98c0-a93dc78f1674', hebrew_continuity__bridge_pidginized, coexists_with).
narrative_ontology:cs_axiom('498c0740-04e2-48fa-98c0-a93dc78f1674', foundational, liturgical_recitation_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(liturgical_recitation_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('498c0740-04e2-48fa-98c0-a93dc78f1674', liturgical_recitation_constitutes_linguistic_life, conventional).
narrative_ontology:cs_axiom('498c0740-04e2-48fa-98c0-a93dc78f1674', foundational, native_intuition_unnecessary_for_continuity).
narrative_ontology:cs_axiom_status(native_intuition_unnecessary_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('498c0740-04e2-48fa-98c0-a93dc78f1674', native_intuition_unnecessary_for_continuity, conventional).
narrative_ontology:cs_reference_frame('498c0740-04e2-48fa-98c0-a93dc78f1674', classical_liturgical_authority).
narrative_ontology:cs_drift_state('498c0740-04e2-48fa-98c0-a93dc78f1674', modern_hebrew_revival_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('498c0740-04e2-48fa-98c0-a93dc78f1674', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, traditional_communities).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, modern_hebrew_proponents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, liturgical_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls orthographic, pronunciation, and liturgical standards across diaspora Jewish communities; certifies scribal and cantorial competence; adjudicates which textual transmissions count as valid Hebrew continuity; receives communal deference and institutional resources from the monopoly on linguistic legitimacy within traditional frameworks.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, rabbinic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Receive transnational liturgical uniformity enabling shared prayer and textual study across dispersed settlements; invest generational resources in classical Hebrew education for children and adults; community identity is fused with liturgical practice and rabbinic textual norms.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, traditional_communities, beneficiary,
    organized, generational, identity_locked, global).

% Individual cantors, scribes, and advanced students who perform the direct labor of ritual recitation and handwritten textual transmission; bear substantial educational costs and career path dependence; their professional identity is constituted through competence in classical liturgical registers.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, liturgical_specialists, payer,
    moderate, biographical, identity_locked, global).

% Advocates for Modern Hebrew native speech and secular Jewish cultural expression as the legitimate fulfillment of Hebrew continuity; structurally excluded from the rabbinic legitimacy framework and classified by it as threats to textual purity; their linguistic practice is delegitimized within traditional institutions.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, modern_hebrew_proponents, excluded,
    moderate, biographical, mobile, global).

% Study Hebrew language history and sociolinguistic vitality from outside the rabbinic authority structure; document the asymmetry between liturgical symbolic continuity and native speaker generative competence; classify the liturgical regime as one continuity claim among several.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, comparative_linguists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a unified liturgical Hebrew across diaspora Jewish communities, enabling shared prayer, textual study, and ritual observance without territorial concentration or reliance on a native speech community.
% TRANSFER_FUNCTION: Moves educational resources, communal authority, and definitional legitimacy from secular and modern Hebrew forms toward classical liturgical competence; concentrates interpretive control in rabbinic transmission chains.
% ABSENT_VOICES: Modern Hebrew speakers, Zionist linguistic planners, and secular Jewish cultural institutions are excluded from the liturgical legitimacy framework; they would argue that native generative use is the genuine fulfillment of Hebrew continuity but are not seated in the rabbinic authority structure.
% DISAPPEARANCE_RATIONALE: If liturgical preservation as the exclusive locus of Hebrew continuity vanished overnight, traditional diaspora communities would lose their transnational liturgical lingua franca, educational curricula would reorient toward modern Hebrew or vernaculars, rabbinic authority over linguistic legitimacy would collapse, and secular Hebrew forms would gain uncontested legitimacy.
% FOUNDING_PROBLEM: The loss of Hebrew as a daily spoken language in the diaspora after the Roman period; the need to maintain Jewish textual coherence, liturgical unity, and religious observance across dispersed communities without an intact speech community.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish antiquity and linguists attest to the shift from spoken to liturgical Hebrew after the Second Temple period. Zionist linguists and Israeli educators contest that the problem required a liturgical rather than revivalist solution; they corroborate the historical loss but dispute the liturgical remedy as the sole or primary response.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.62) is moderate-high because the constraint channels substantial communal resources into liturgical education and delegitimizes modern Hebrew alternatives. Suppression (0.58) reflects active normative and institutional enforcement against secular Hebrew forms. Theater ratio (0.40) captures the growing performative dimension of liturgical Hebrew in communities where semantic comprehension is low. Accessibility collapse (0.60) indicates that once committed to the liturgical framework, modern Hebrew alternatives appear structurally illegitimate. Resistance (0.48) reflects the success of Modern Hebrew in Israel and secular diaspora movements. The temporal series run on a single shared grid (0-100) showing gradual intensification as the Modern Hebrew revival created competitive pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, the constraint is a necessary coordination mechanism preserving Jewish textual civilization across diaspora; from the modern Hebrew proponent seat, it is an extractive structure that denies legitimacy to native speech and captures communal resources for ritual maintenance. The traditional community seat experiences both coordination benefit and extraction through educational labor. The engine computes this divergence from the structural asymmetry in exit options (identity_locked vs. mobile) and the beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities are declared agenda-setters with constrained exit (low directionality) because they receive institutional deference and control the legitimacy mechanism. Traditional communities are beneficiaries with identity_locked exit, placing them nearer the target end than pure beneficiaries because their community survival is fused to the liturgical economy. Liturgical specialists are payers with identity_locked exit (high directionality) bearing the direct performative and educational costs. Modern Hebrew proponents are declared victims with mobile exit (high directionality) because the constraint extracts legitimacy from their linguistic practice and structurally excludes them from the continuity narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination function (transnational liturgical uniformity) and asymmetric extraction (delegitimization of modern Hebrew, concentration of authority). A pure rope reading would fail because modern Hebrew proponents are identifiable victims and the constraint requires active enforcement to maintain the liturgical boundary. A pure snare reading would fail because the coordination function is genuine: diaspora communities do gain a shared liturgical language that enables joint ritual practice across territories. The Tangled Rope classification captures this hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_speaker_necessity,
    'Is native speaker intuition and generative daily use necessary for a language to be considered linguistically alive, or can ritual recitation and textual transmission constitute genuine linguistic continuity?',
    'Cross-linguistic typological analysis comparing liturgically preserved languages to creole and revival languages; sociolinguistic vitality metrics applied to Hebrew across Israeli and diaspora populations.',
    'If native generative use is necessary, the liturgical preservation reading is a false summit (constructed constraint benefiting liturgical authorities); if symbolic preservation suffices, the native reading is displaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_necessity, conceptual, 'Whether linguistic life requires native speech or permits symbolic preservation.').

omega_variable(
    kernel_reading_contest,
    'This constraint is the liturgical_preservation reading of the hebrew_continuity kernel. How would classification change if the native_generative or bridge_pidginized reading were adopted instead?',
    'Generate sibling constraint stories for native_generative and bridge_pidginized readings and compare computed per-seat classifications across the constraint family.',
    'The native reading would reverse the beneficiary/victim structure (modern Hebrew speakers as beneficiaries, liturgical authorities as agenda-setters losing power); the bridge reading would diffuse extraction across contact-language domains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Commit structure: this is one reading of a contested kernel with structurally distinct siblings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of modern and secular Hebrew forms structural (institutional control of education and religious norms) or internalized (identity fusion within traditional communities)?',
    'Post-exit trajectory analysis: if traditional communities that shift to modern Hebrew still suppress secular forms within their own discourse, suppression is internalized; if suppression ceases when institutional control relaxes, it is structural.',
    'If internalized, effective suppression exceeds the structural measure and the constraint functions partly as identity coordination; if structural, the constraint is maintained by enforceable authority and is more readily addressable by institutional reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in liturgical Hebrew continuity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__liturgical_preservation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(hebr_tr_t20, hebrew_continuity__liturgical_preservation, theater_ratio, 20, 0.28).
narrative_ontology:measurement(hebr_tr_t40, hebrew_continuity__liturgical_preservation, theater_ratio, 40, 0.32).
narrative_ontology:measurement(hebr_tr_t60, hebrew_continuity__liturgical_preservation, theater_ratio, 60, 0.35).
narrative_ontology:measurement(hebr_tr_t80, hebrew_continuity__liturgical_preservation, theater_ratio, 80, 0.38).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__liturgical_preservation, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__liturgical_preservation, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(hebr_be_t20, hebrew_continuity__liturgical_preservation, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(hebr_be_t40, hebrew_continuity__liturgical_preservation, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(hebr_be_t60, hebrew_continuity__liturgical_preservation, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(hebr_be_t80, hebrew_continuity__liturgical_preservation, base_extractiveness, 80, 0.6).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__liturgical_preservation, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__liturgical_preservation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hebr_su_t20, hebrew_continuity__liturgical_preservation, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(hebr_su_t40, hebrew_continuity__liturgical_preservation, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(hebr_su_t60, hebrew_continuity__liturgical_preservation, suppression_requirement, 60, 0.53).
narrative_ontology:measurement(hebr_su_t80, hebrew_continuity__liturgical_preservation, suppression_requirement, 80, 0.56).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__liturgical_preservation, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
