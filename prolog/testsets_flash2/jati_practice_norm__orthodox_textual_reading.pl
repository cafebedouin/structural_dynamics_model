% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Jati Boundaries as Fixed Scriptural Varna Framework (Orthodox Textual Reading)
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'orthodox textual reading' of jati
 *   boundaries, where social divisions are understood as fixed and divinely
 *   ordained by scriptural varna frameworks. Deviation from these boundaries
 *   is considered ritual pollution, justifying severe social and economic
 *   penalties for lower jatis. This reading emphasizes the immutability of
 *   the system and the inherent purity/impurity associated with birth. The
 *   high extractiveness and suppression reflect the structural delta expected
 *   for this reading, where lower jatis are assigned polluting occupations
 *   with blocked mobility, and the authority structure benefits from
 *   categorical rigidity.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: Primary agenda_setter (institutional/arbitrage) — interprets and enforces scriptural norms.
 *   - upper_jati_landowners: Primary beneficiary (powerful/mobile) — benefits from social hierarchy and labor control.
 *   - lower_jati_laborers: Primary payer (powerless/trapped) — bears economic and social costs.
 *   - dalit_communities: Primary payer (powerless/identity_locked) — bears extreme social exclusion and economic deprivation.
 *   - social_reformers: Analytical observer (organized/analytical) — challenges the system and advocates for change.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.9).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.95).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Jati Boundaries as Fixed Scriptural Varna Framework (Orthodox Textual Reading)").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '591cf840-c7c5-4bde-884e-78e6b5b8c65f').
narrative_ontology:cs_kernel_codification('591cf840-c7c5-4bde-884e-78e6b5b8c65f', fixed_text).
narrative_ontology:cs_authority_grounding('591cf840-c7c5-4bde-884e-78e6b5b8c65f', lineage).
narrative_ontology:cs_interpretation_layer_present('591cf840-c7c5-4bde-884e-78e6b5b8c65f').
narrative_ontology:cs_reading_relation('591cf840-c7c5-4bde-884e-78e6b5b8c65f', jati_practice_norm__localized_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('591cf840-c7c5-4bde-884e-78e6b5b8c65f', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('591cf840-c7c5-4bde-884e-78e6b5b8c65f', foundational, varna_jati_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_jati_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('591cf840-c7c5-4bde-884e-78e6b5b8c65f', varna_jati_divinely_ordained, theological).
narrative_ontology:cs_axiom('591cf840-c7c5-4bde-884e-78e6b5b8c65f', foundational, ritual_purity_hierarchy).
narrative_ontology:cs_axiom_status(ritual_purity_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('591cf840-c7c5-4bde-884e-78e6b5b8c65f', ritual_purity_hierarchy, deontological).
narrative_ontology:cs_reference_frame('591cf840-c7c5-4bde-884e-78e6b5b8c65f', ancient_scriptural_injunctions).
narrative_ontology:cs_drift_state('591cf840-c7c5-4bde-884e-78e6b5b8c65f', contemporary_globalized_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('591cf840-c7c5-4bde-884e-78e6b5b8c65f', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmin_priestly_class).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, upper_jati_landowners).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, dalit_communities).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, lower_jati_laborers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces scriptural injunctions regarding varna and jati, legitimizing the hierarchical structure. Benefits from ritual purity and social deference, and often controls access to religious services and knowledge. Their authority is directly tied to the maintenance of the orthodox textual reading.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, arbitrage, regional).

% Benefit from the social and economic stratification enforced by the jati system, which provides a stable, cheap labor force and reinforces their land ownership and social status. They uphold the orthodox textual reading as it justifies their position.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, upper_jati_landowners, beneficiary,
    powerful, generational, mobile, local).

% Are assigned specific, often stigmatized, occupations and face severe restrictions on social and economic mobility. They bear the direct costs of the system through exploitation and discrimination, with limited avenues for advancement or exit due to social ostracization and economic dependency.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, lower_jati_laborers, payer,
    powerless, biographical, trapped, local).

% Are considered outside the varna system, facing extreme social exclusion, ritual pollution, and economic deprivation. Their identity is deeply intertwined with their marginalized status, making exit from the system a profound challenge involving social and personal rupture.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dalit_communities, payer,
    powerless, generational, identity_locked, local).

% Advocate for the abolition or reform of the jati system, challenging the scriptural basis and highlighting its discriminatory practices. They analyze the system's impact and work to mobilize resistance and legal change.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, social_reformers, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a rigid social order and division of labor, ensuring specific tasks are performed by designated groups and maintaining social hierarchy based on perceived ritual purity.
% TRANSFER_FUNCTION: Transfers social status, economic resources, and ritual purity from lower jatis to upper jatis, while transferring labor and deference from lower jatis to upper jatis.
% ABSENT_VOICES: Historical and contemporary voices from marginalized communities, particularly those who have attempted to defy or escape the system, are often silenced or dismissed as illegitimate by the orthodox textual framework. Their experiences of oppression and calls for equality are excluded from the dominant narrative.
% DISAPPEARANCE_RATIONALE: If the orthodox textual reading of jati boundaries vanished, the entire social, economic, and ritual structure of many communities would collapse. Labor relations, marriage patterns, social deference, and religious practices would undergo profound and rapid reorganization, leading to significant social upheaval and the emergence of new forms of social organization.
% FOUNDING_PROBLEM: To establish a divinely ordained social order, maintain ritual purity, and ensure the performance of essential societal functions through a hierarchical division of labor.
% FOUNDING_PROBLEM_CORROBORATION: The Brahmin priestly class and upper jati landowners attest that the founding problem of maintaining social order and ritual purity is still live and essential. Social reformers and Dalit activists, however, contest this, arguing that the 'problem' is a justification for exploitation, and that the system's original intent has been corrupted or was inherently unjust; their corroboration comes from historical accounts of discrimination and contemporary sociological studies.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.9) because the system systematically channels resources, status, and labor upwards, while imposing severe costs on lower jatis. Suppression is also very high (0.95) due to the combination of religious injunctions, social ostracization, economic dependency, and the threat of ritual pollution, which effectively blocks mobility and resistance. Theater ratio is low (0.1) because the system is actively functional in its extractive and suppressive roles, with little performative maintenance for a defunct purpose. Accessibility collapse is high (0.9) as the system is presented as natural and divinely ordained, making alternatives seem unthinkable or sacrilegious. Resistance is moderate (0.7) reflecting ongoing, though often suppressed, challenges from marginalized groups and reformers.
 *
 * PERSPECTIVAL GAP:
 *   The Brahmin priestly class and upper jati landowners perceive this system as a legitimate, divinely sanctioned social order that ensures stability and ritual purity. For lower jati laborers and Dalit communities, it is a system of profound oppression and exploitation. The engine's classification will highlight this divergence, showing a Snare for the victims and a perceived Rope or even Mountain for the beneficiaries, based on their structural positions and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin priestly class and upper jati landowners are clear beneficiaries, with their power and status directly derived from the system's enforcement (low directionality). Lower jati laborers and Dalit communities are the primary targets, bearing the brunt of the system's costs and having severely constrained or identity-locked exit options (high directionality). Social reformers act as analytical observers, seeking to expose the system's true nature.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Snare because its coordination story (divinely ordained social order) is a cover for systematic extraction and suppression. The persistence of the constraint depends heavily on active enforcement and the suppression of alternatives, rather than genuine collective benefit for all participants. The high extractiveness and suppression, coupled with identifiable victims, prevent it from being mislabeled as a Rope or Mountain, despite claims of naturalness or divine origin.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_interpretation_ambiguity,
    'Is the orthodox textual reading of varna and jati the only valid scriptural interpretation, or do alternative interpretations exist that support more fluid or egalitarian social structures?',
    'Comparative theological and historical textual analysis by independent scholars, examining the evolution of interpretations and the existence of dissenting traditions within the religious framework.',
    'If alternative, less rigid interpretations are validated, the ''fixed'' nature of the constraint would be undermined, potentially reducing its perceived naturalness and suppression, shifting it towards a Tangled Rope or even a Piton if the textual basis for extraction weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_interpretation_ambiguity, conceptual, 'Ambiguity in the foundational scriptural interpretation of jati boundaries.').

omega_variable(
    internalized_suppression_component,
    'To what extent is the suppression experienced by lower jatis and Dalits internalized (e.g., self-policing, belief in karma, identity fusion) versus purely structural (e.g., economic barriers, social ostracization)?',
    'Sociological studies examining post-migration or post-reform attitudes and behaviors: if discriminatory practices persist even after structural barriers are removed, it indicates a significant internalized component.',
    'If internalized suppression is substantial, the effective suppression is higher than the structural measure suggests, making exit even more difficult and potentially requiring different intervention strategies (e.g., psychological support, identity reconstruction) beyond legal or economic reforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_component, empirical, 'Structural vs. internalized suppression mechanism in the jati system.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''orthodox textual reading'' of the jati practice norm, or does it conflate elements of other readings (e.g., localized practice, colonial census) that would yield different structural classifications?',
    'Detailed historical and ethnographic analysis to isolate the specific interpretive tradition and its enforcement mechanisms, ensuring it does not incorporate elements that belong to sibling readings.',
    'If conflated, the current classification might be inaccurate. Decomposing into purer readings would likely yield different extractiveness and suppression values for each, potentially revealing a more complex network of constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ensuring the purity of the ''orthodox textual reading'' against conflation with sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jati_tr_t25, jati_practice_norm__orthodox_textual_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(jati_tr_t50, jati_practice_norm__orthodox_textual_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(jati_tr_t75, jati_practice_norm__orthodox_textual_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(jati_tr_t100, jati_practice_norm__orthodox_textual_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(jati_be_t25, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 25, 0.88).
narrative_ontology:measurement(jati_be_t50, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 50, 0.9).
narrative_ontology:measurement(jati_be_t75, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 75, 0.91).
narrative_ontology:measurement(jati_be_t100, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 100, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(jati_su_t25, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 25, 0.92).
narrative_ontology:measurement(jati_su_t50, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 50, 0.95).
narrative_ontology:measurement(jati_su_t75, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 75, 0.93).
narrative_ontology:measurement(jati_su_t100, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 100, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
