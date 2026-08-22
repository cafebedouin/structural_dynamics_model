% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Hebrew Vitality â Liturgical Continuity Reading
 *   domain: sociolinguistic/religious
 *
 * SUMMARY:
 *   This constraint instantiates the liturgical_reading of the
 *   hebrew_vitality kernel, which holds that unbroken liturgical use
 *   constitutes the living kernel of the Hebrew language. Sibling readings
 *   include native_daily_reading (native generation only) and
 *   hybrid_continuity_reading (liturgical substrate plus vernacular
 *   reconstruction). The liturgical reading treats ritual preservation as
 *   self-sufficient vitality, locating the primary beneficiary seat with
 *   rabbinic authorities and asserting no victim set because preservation
 *   imposes no identifiable cost.
 *
 * KEY AGENTS:
 *   - Rabbinic authorities: Agenda-setter and beneficiary (institutional/identity_locked) â steward liturgical norms and derive legitimacy from the continuity frame.
 *   - Liturgical communities: Beneficiary (organized/identity_locked) â their practice is framed as the living language itself.
 *   - Modern Hebrew revivalists: Excluded (organized/mobile) â define vitality through native speech and are outside this reading's conversation.
 *   - Sociolinguistic observers: Analytical seat (analytical/analytical) â map the competing vitality frameworks without theological commitment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.18).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.12).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Hebrew Vitality â Liturgical Continuity Reading").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistic/religious").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, 'a420c3c8-f682-4eb0-8066-a64c07039f47').
narrative_ontology:cs_kernel_codification('a420c3c8-f682-4eb0-8066-a64c07039f47', fixed_text).
narrative_ontology:cs_authority_grounding('a420c3c8-f682-4eb0-8066-a64c07039f47', lineage).
narrative_ontology:cs_interpretation_layer_present('a420c3c8-f682-4eb0-8066-a64c07039f47').
narrative_ontology:cs_reading_relation('a420c3c8-f682-4eb0-8066-a64c07039f47', hebrew_vitality__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('a420c3c8-f682-4eb0-8066-a64c07039f47', hebrew_vitality__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('a420c3c8-f682-4eb0-8066-a64c07039f47', foundational, liturgical_continuity_constitutes_vitality).
narrative_ontology:cs_axiom_status(liturgical_continuity_constitutes_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a420c3c8-f682-4eb0-8066-a64c07039f47', liturgical_continuity_constitutes_vitality, theological).
narrative_ontology:cs_axiom('a420c3c8-f682-4eb0-8066-a64c07039f47', secondary, native_speech_nonessential_to_sacred_life).
narrative_ontology:cs_axiom_status(native_speech_nonessential_to_sacred_life, holdable).
narrative_ontology:cs_axiom_grounding('a420c3c8-f682-4eb0-8066-a64c07039f47', native_speech_nonessential_to_sacred_life, theological).
narrative_ontology:cs_reference_frame('a420c3c8-f682-4eb0-8066-a64c07039f47', classical_liturgical_continuity).
narrative_ontology:cs_drift_state('a420c3c8-f682-4eb0-8066-a64c07039f47', post_vernacular_revival, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a420c3c8-f682-4eb0-8066-a64c07039f47', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the liturgical tradition, adjudicating norms of prayer, sacred text recitation, and ritual Hebrew competence. Their institutional role and legitimacy depend on the continuity of Hebrew as a liturgical language, and they organize educational and judicial resources around maintaining that continuity.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, rabbinic_authorities, beneficiary).

% Engage in daily prayer, Torah reading, and ritual study in Hebrew across diverse diasporic settings. Their practice is understood within this reading as the active, living body of the language, rather than as preservation of a moribund tongue.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liturgical_communities, beneficiary,
    organized, generational, identity_locked, global).

% Advance native daily Hebrew as the sole genuine marker of language vitality. They are not party to the liturgical reading's internal conversation, which assigns vitality to ritual function rather than to native speaker communities.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, modern_hebrew_revivalists, excluded,
    organized, biographical, mobile, national).

% Document and analyze the competing frameworks for Hebrew vitality, noting that the liturgical reading coordinates a global community around a low-overhead, non-territorial standard of continuity without requiring mass native fluency.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, sociolinguistic_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(hebrew_vitality__liturgical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a dispersed diasporic community around a shared, achievable standard of Hebrew continuity that does not depend on territorial concentration, state institutions, or mass native fluency.
% TRANSFER_FUNCTION: Moves legitimacy and institutional authority to rabbinic gatekeepers of liturgical practice; orients communal resources toward ritual education and away from secular vernacular revival projects.
% ABSENT_VOICES: Modern Hebrew nativists and secular Zionist language planners, who define vitality through native daily speech, are structurally excluded from the liturgical framework's self-description; they are present in the broader kernel contest but not in this reading's internal conversation.
% DISAPPEARANCE_RATIONALE: For liturgical communities, losing this constraint would dissolve the theological justification for their practice as 'living Hebrew'; for secular revivalists, its disappearance would validate their alternative. The rearrangement is interpretive and allocative, not materially catastrophic.
% FOUNDING_PROBLEM: How to maintain Hebrew as a meaningful Jewish language after the loss of native speech communities in antiquity and the medieval dispersion.
% FOUNDING_PROBLEM_CORROBORATION: Medieval responsa and modern sociolinguistic historiography from outside the rabbinic beneficiary set corroborate that liturgical continuity solved the dispersion problem. Modern Hebrew nativists and secular linguists contest this, arguing the founding problem was only partially addressed and persists until native daily use is fully secured.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, contested).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint operates almost entirely within the ritual domain and does not extract material resources from identifiable targets. Suppression is low (0.12) because persistence depends on normative and theological consensus rather than active coercion. Theater ratio is moderate-low (0.25): ritual is inherently performative, but the functional coordination of diasporic community life is genuine. Accessibility collapse is elevated (0.65) because, within the liturgical frame, alternative definitions of vitality (native daily use) become conceptually illegitimate once the frame is accepted. Resistance is moderate (0.35) due to the vigorous presence of the native-daily reading in the broader sociolinguistic field.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic and liturgical seats experience the constraint as identity-constituting coordination: Hebrew is alive because they pray in it. The modern Hebrew revivalist seat, when forced into the same frame, experiences delegitimization of its own project; however, because that seat is structurally mobile (it built a successful rival framework in Israel), the constraint does not extract from it directlyâit merely excludes it from its own definitional conversation. The engine will compute different per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and liturgical communities are both beneficiaries, sitting near the low-d end of the spectrum: the constraint subsidizes their identity and institutional role. No victim group is declared, so no high-d target seat is structurally derived. Modern Hebrew revivalists are excluded rather than targeted; their directionality is not computed as extraction because the constraint does not operate upon themâit operates around them.
 *
 * MANDATROPHY ANALYSIS:
 *   The liturgical reading prevents mandatrophy mislabeling by clearly separating the founding problemâHebrew continuity after the loss of a native speech communityâfrom its solution. The arrangement was built to solve dispersion, and the liturgical frame continues to coordinate diasporic communities without territorial concentration. It does not present as extraction because the 'cost' of the constraint is borne by no identifiable agent: liturgical communities engage voluntarily, and native revivalists operate in a separate institutional sphere with their own resources. The risk of mislabeling would arise if the reading actively suppressed vernacular Hebrew; the authored metrics, no-victim declaration, and low suppression score reject that reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_daily_exclusion_cost,
    'Does the liturgical reading''s definitional exclusion of native daily use as the kernel of vitality impose hidden costs on vernacular revival projects, or is it genuinely costless outside the ritual domain?',
    'Comparative analysis of resource flows and legitimacy allocation between liturgical institutions and vernacular education systems across Jewish communities.',
    'If exclusion redirects resources or delegitimizes revival, the victim-free structure is incomplete; if not, the low-epsilon reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_daily_exclusion_cost, conceptual, 'Whether the liturgical definition of vitality imposes hidden costs on native-daily Hebrew').

omega_variable(
    kernel_naturalness,
    'Is the priority of liturgical Hebrew over vernacular Hebrew a naturally emergent feature of language shift in religious communities, or a theologically constructed hierarchy?',
    'Historical sociolinguistics comparing religious language outcomes across traditions (Latin, Arabic, Sanskrit).',
    'If natural, the constraint trends toward mountain-like immunity; if constructed, it remains a rope coordination susceptible to revision.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_naturalness, empirical, 'Natural emergence vs theological construction of liturgical priority').

omega_variable(
    liturgical_reading_sibling_influence,
    'Does the liturgical reading''s institutional dominance structurally foreclose hybrid or native-daily readings, or merely coexist with them?',
    'Analysis of institutional resource allocation and curricular design in Jewish educational systems.',
    'If foreclosing, the reading functions with higher suppression than authored; if coexisting, the low-epsilon reading is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_reading_sibling_influence, conceptual, 'Structural relationship between liturgical and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hebr_tr_t500, hebrew_vitality__liturgical_reading, theater_ratio, 500, 0.18).
narrative_ontology:measurement(hebr_tr_t1000, hebrew_vitality__liturgical_reading, theater_ratio, 1000, 0.2).
narrative_ontology:measurement(hebr_tr_t1500, hebrew_vitality__liturgical_reading, theater_ratio, 1500, 0.22).
narrative_ontology:measurement(hebr_tr_t2000, hebrew_vitality__liturgical_reading, theater_ratio, 2000, 0.25).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hebr_be_t500, hebrew_vitality__liturgical_reading, base_extractiveness, 500, 0.12).
narrative_ontology:measurement(hebr_be_t1000, hebrew_vitality__liturgical_reading, base_extractiveness, 1000, 0.14).
narrative_ontology:measurement(hebr_be_t1500, hebrew_vitality__liturgical_reading, base_extractiveness, 1500, 0.16).
narrative_ontology:measurement(hebr_be_t2000, hebrew_vitality__liturgical_reading, base_extractiveness, 2000, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_vitality__liturgical_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(hebr_su_t500, hebrew_vitality__liturgical_reading, suppression_requirement, 500, 0.06).
narrative_ontology:measurement(hebr_su_t1000, hebrew_vitality__liturgical_reading, suppression_requirement, 1000, 0.08).
narrative_ontology:measurement(hebr_su_t1500, hebrew_vitality__liturgical_reading, suppression_requirement, 1500, 0.1).
narrative_ontology:measurement(hebr_su_t2000, hebrew_vitality__liturgical_reading, suppression_requirement, 2000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the hebrew_vitality kernel. It decomposes the colloquial label 'Hebrew language vitality' into structurally distinct claims: liturgical continuity (this file), native daily use (native_daily_reading), and hybrid reconstruction (hybrid_continuity_reading). Each has distinct epsilon, beneficiary structure, and victim set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
