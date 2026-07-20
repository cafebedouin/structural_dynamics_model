% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Orthodox Literal Reading of the Gita Kurukshetra Discourse
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the orthodox literal reading of the
 *   Bhagavad Gita kernel (gita_kurukshetra_discourse). In this reading, the
 *   Kurukshetra battlefield is a literal theatre of dharmic war, Krishna's
 *   command to Arjuna mandates actual violent combat, and the varna (caste)
 *   system is a divinely ordained social structure where svadharma (one's own
 *   duty) is determined by birth. The Brahminical class retains interpretive
 *   monopoly over the text and its application, while the Kshatriya warrior
 *   class receives divine legitimation for violence performed as duty. Lower
 *   castes are structurally bound to hereditary service and subordination,
 *   with spiritual advancement contingent on accepting their station. Those
 *   slain in 'righteous' war have their deaths reclassified as cosmic
 *   necessity rather than violence. The constraint coordinates social order
 *   through fixed hierarchy while extracting obedience, labor, and life from
 *   its lower seats.
 *
 * KEY AGENTS:
 *   - brahmin_interpreters: Agenda-setter (institutional/constrained) â controls textual meaning and ritual legitimacy
 *   - kshatriya_warriors: Beneficiary (powerful/constrained) â receives license for violence and political dominance
 *   - lower_caste_communities: Payer (powerless/trapped) â locked in hereditary subordination and labor extraction
 *   - those_slain_in_dharmic_war: Payer (powerless/trapped) â bear the ultimate cost of legitimated warfare
 *   - modern_reform_scholars: Observer (analytical) â analytical seat outside the orthodox framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.72).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.78).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Orthodox Literal Reading of the Gita Kurukshetra Discourse").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '45d04604-3357-45d7-ac69-0e613e687ce4').
narrative_ontology:cs_kernel_codification('45d04604-3357-45d7-ac69-0e613e687ce4', fixed_text).
narrative_ontology:cs_authority_grounding('45d04604-3357-45d7-ac69-0e613e687ce4', lineage).
narrative_ontology:cs_interpretation_layer_present('45d04604-3357-45d7-ac69-0e613e687ce4').
narrative_ontology:cs_reading_relation('45d04604-3357-45d7-ac69-0e613e687ce4', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_reading_relation('45d04604-3357-45d7-ac69-0e613e687ce4', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('45d04604-3357-45d7-ac69-0e613e687ce4', foundational, svadharma_by_birth_ordinate).
narrative_ontology:cs_axiom_status(svadharma_by_birth_ordinate, holdable).
narrative_ontology:cs_axiom_grounding('45d04604-3357-45d7-ac69-0e613e687ce4', svadharma_by_birth_ordinate, theological).
narrative_ontology:cs_axiom('45d04604-3357-45d7-ac69-0e613e687ce4', foundational, dharmic_war_violence_sanctioned).
narrative_ontology:cs_axiom_status(dharmic_war_violence_sanctioned, holdable).
narrative_ontology:cs_axiom_grounding('45d04604-3357-45d7-ac69-0e613e687ce4', dharmic_war_violence_sanctioned, theological).
narrative_ontology:cs_reference_frame('45d04604-3357-45d7-ac69-0e613e687ce4', divine_varna_order).
narrative_ontology:cs_drift_state('45d04604-3357-45d7-ac69-0e613e687ce4', contemporary_postcolonial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('45d04604-3357-45d7-ac69-0e613e687ce4', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpreters).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warriors).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_caste_communities).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, those_slain_in_dharmic_war).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, varna_dharma_doctrine).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, karma_yoga_as_duty).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, divine_sanction_of_social_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the authoritative interpretation of the Gita and allied Dharmashastra texts, determining which duties are binding by birth and which wars are righteous. Their social authority and material support (patronage, temple endowments, ritual fees) depend on maintaining the literal reading's monopoly. They cannot abandon orthodoxy without dissolving their institutional role.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmin_interpreters, agenda_setter,
    institutional, generational, constrained, global).

% Receive divine legitimation for lethal violence when framed as dharmic duty. Political and military dominance is coordinated through the caste-order narrative: they rule and fight because the text says they must. Exit is constrained because refusal of the warrior role is defined as cowardice and adharma, while non-Kshatriya claimants to power are defined as usurpers.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warriors, beneficiary,
    powerful, generational, constrained, national).

% Bound to hereditary occupations and ritual subordination; spiritual advancement is available only through acceptance of present station (karma, rebirth). Social mobility is defined as adharma. Historical exit via conversion or rebellion is met with exclusion or violence, and identity is fused with the caste role from birth.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, lower_caste_communities, payer,
    powerless, generational, trapped, national).

% Killed in warfare that the orthodox reading classifies as righteous. Their deaths are reinterpreted as cosmic necessity or karmic fulfillment rather than as violence done to them. They have no voice in the discourse that names their killing dharma, and no exit from the interpretive frame that justifies it.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, those_slain_in_dharmic_war, payer,
    powerless, immediate, trapped, regional).

% Analyze the Gita's historical composition, the sociology of varna, and the political function of dharmic-war rhetoric. They operate outside the orthodox commitment framework, treating the literal reading as one historically situated interpretation among others.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, modern_reform_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social order across a stratified society by assigning fixed duties (svadharma) determined by birth, thereby reducing conflict over status, occupation, and political legitimacy through a shared metaphysical narrative.
% TRANSFER_FUNCTION: Moves obedience, labor, and life from lower-caste communities and those slain in war to the maintenance of Brahminical interpretive authority and Kshatriya political-military dominance.
% ABSENT_VOICES: Lower-caste theologians who would read equality into the text; pacifist refusers (early Buddhist, Jain, and Gandhian traditions) who reject violence altogether; the slain who cannot speak back to the discourse that names their deaths righteous; women largely outside the warrior-and-dharma frame.
% DISAPPEARANCE_RATIONALE: If the orthodox literal reading vanished, caste-based duty claims would lose their primary textual anchor, Kshatriya violence would require secular or ethical justification, Brahmin interpretive monopoly would weaken, and lower-caste subordination would lose its cosmic legitimation. Social order would rearrange toward alternative ethics (constitutional, human-rights, or bhakti-universalist).
% FOUNDING_PROBLEM: Cosmic and social disorder (adharma) arising from refusal to perform birth-assigned duties and from unregulated violence outside ritual/ethical bounds.
% FOUNDING_PROBLEM_CORROBORATION: Brahminical commentators (Shankara, Ramanuja, Madhva) attest the problem from within the tradition. External corroboration from historical sociology notes Vedic-period warfare and social stratification, but reform movements (Buddhist, Bhakti, Gandhi, Ambedkar) dispute that birth-duty was the actual problem or that the orthodox reading solved it; these sources corroborate the contested status from outside the beneficiary set.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint moves life chances, labor, and death from lower-caste and slain agents to Brahminical authority and Kshatriya power. Suppression (0.78) is high because alternatives (equality, refusal of violence, social mobility) are suppressed through metaphysical framing (karma, rebirth) that makes acceptance of station spiritually mandatory. Theater ratio (0.45) reflects that while the text performs genuine coordination (social order, cosmic meaning), nearly half of its institutional maintenance is performative (ritual recitation, commentarial defense) that sustains hierarchy rather than solving a current collective-action problem. Accessibility collapse (0.82) is high because once the theological frame is accepted, empirical alternatives (equality, human rights) appear as adharma. Resistance (0.55) is moderate because reform movements (Buddhist, Bhakti, modern constitutional) have historically contested the reading, yet within the orthodox frame such resistance is defined as ignorance.
 *
 * PERSPECTIVAL GAP:
 *   The Brahminical agenda-setter seat experiences the constraint as preservation of cosmic order and textual authority; the Kshatriya beneficiary seat experiences it as honorable duty and political license. The lower-caste and slain payer seats experience the same structure as immutable subordination and sanctified violence. The engine will compute these seats as divergent types because directionality derives from beneficiary/victim declarations combined with exit options: beneficiaries with constrained exit sit near low d, while trapped victims sit near high d.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin interpreters are structural beneficiaries (low d) because the constraint subsidizes their interpretive monopoly and social authority; they have constrained exit because abandoning orthodoxy dissolves their authority. Kshatriya warriors are beneficiaries (moderate-low d) because they receive violence legitimation, though their exit is constrained by honor codes. Lower-caste communities are full targets (high d) because the constraint extracts labor and subordination while foreclosing mobility; they are trapped by identity-locked social definition. Those slain in war are full targets (high d) because the constraint extracts their lives while defining that extraction as righteousness; they have no exit from the frame that names their death dharma.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â cosmic disorder from adharma â is contested. External observers (historical sociologists, reformers) argue the constraint was built to legitimate Aryan/Kshatriya political expansion and Brahminical social control, not to solve a neutral coordination problem. The classification as tangled_rope captures that the constraint DOES coordinate (it stabilizes social order across generations) but simultaneously extracts asymmetrically. A pure snare reading would miss the genuine coordination function for upper-caste social cohesion; a pure rope reading would miss the extraction from lower castes and enemies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    literal_vs_allegorical_kernel_ambiguity,
    'Does the orthodox literal reading foreclose the Gandhian allegorical reading, or do they coexist as parallel commitments within Hindu hermeneutic tradition?',
    'Historical sociology of reception: catalog instances where the same institutional lineage maintained both literal and allegorical registers simultaneously versus instances where allegorical readings were expelled as apologetic.',
    'If foreclosed, the orthodox reading is logically brittle and internally exclusive; if coexisting, it is capacious but classification must account for multi-register tolerance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literal_vs_allegorical_kernel_ambiguity, conceptual, 'Ambiguity of structural relation between orthodox literal and Gandhian allegorical readings.').

omega_variable(
    caste_as_divine_vs_social_construct,
    'Is the varna hierarchy authored into the constraint as a genuine metaphysical reality (theological mountain), or as a human social structure legitimated by textual interpretation (constructed extraction)?',
    'Comparative philology and archaeological sociology: assess whether pre-Gita Vedic society already exhibited rigid caste hierarchy, or whether the Gita textualized and rigidified a fluid social arrangement.',
    'If genuinely theological, extraction is partially reclassified as coordination cost of cosmic order; if socially constructed, extraction is naked asymmetric transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_as_divine_vs_social_construct, empirical, 'Natural-law versus constructed ambiguity of caste hierarchy.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of lower-caste alternatives structural (enforced by violence and exclusion) or internalized (accepted as karma/dharma by the dominated)?',
    'Ethnographic and oral-history study of lower-caste communities: measure resistance behavior, hidden transcripts, and conversion/rebellion rates under orthodox dominance.',
    'If internalized, effective suppression exceeds structural measure and the constraint operates as deep identity-lock; if purely structural, resistance potential is higher than metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in caste hierarchy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gita_tr_t5, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(gita_tr_t10, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(gita_tr_t15, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(gita_tr_t20, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement(gita_tr_t25, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gita_be_t5, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(gita_be_t10, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(gita_be_t15, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(gita_be_t20, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(gita_be_t25, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 25, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t0, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gita_su_t5, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(gita_su_t10, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(gita_su_t15, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement(gita_su_t20, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(gita_su_t25, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 25, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, gandhian_allegorical_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% This constraint is the orthodox literal reading of the Gita kernel; sibling readings (gandhian allegorical, universalist devotional) decompose the same textual kernel into structurally distinct constraints with different epsilon values and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
