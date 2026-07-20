% ============================================================================
% CONSTRAINT STORY: living_language_status__literary_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__literary_continuity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: living_language_status__literary_continuity_reading
 *   human_readable: Literary Continuity Reading of Living Language Status
 *   domain: sociolinguistic/religious/nationalist
 *
 * SUMMARY:
 *   This constraint story instantiates the literary continuity reading of the
 *   contested 'living language status' kernel. It treats a language as living
 *   if it sustains productive literary and intellectual work, regardless of
 *   native-speaker demographics. The Haskalah periodicals and modern Hebrew
 *   literature are the canonical evidence. The reading coordinates secular
 *   intellectuals around a revival legitimacy framework while structurally
 *   excluding illiterate and non-literary speakers from the vitality
 *   calculus. It is authored as a tangled rope with low extractiveness:
 *   genuine coordination function (enabling secular Hebrew culture) married
 *   to asymmetric exclusion.
 *
 * KEY AGENTS:
 *   - maskilim: Primary beneficiary (moderate/mobile) â gains cultural authority through literary production without mass adoption.
 *   - secular_intellectuals: Agenda-setter and secondary beneficiary (organized/mobile) â defines and administers the vitality standard.
 *   - illiterate_speakers: Target (powerless/trapped) â rendered invisible by the literary definition.
 *   - non_literary_communities: Target (powerless/identity_locked) â communal oral identity excluded from vitality recognition.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__literary_continuity_reading, 0.3).
domain_priors:suppression_score(living_language_status__literary_continuity_reading, 0.45).
domain_priors:theater_ratio(living_language_status__literary_continuity_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(living_language_status__literary_continuity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__literary_continuity_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__literary_continuity_reading, "Literary Continuity Reading of Living Language Status").
narrative_ontology:topic_domain(living_language_status__literary_continuity_reading, "sociolinguistic/religious/nationalist").

domain_priors:requires_active_enforcement(living_language_status__literary_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__literary_continuity_reading, '6d3382b5-ee81-4fef-9a02-621f7dd9c755').
narrative_ontology:cs_kernel_codification('6d3382b5-ee81-4fef-9a02-621f7dd9c755', distributed).
narrative_ontology:cs_authority_grounding('6d3382b5-ee81-4fef-9a02-621f7dd9c755', practice).
narrative_ontology:cs_interpretation_layer_present('6d3382b5-ee81-4fef-9a02-621f7dd9c755').
narrative_ontology:cs_reading_relation('6d3382b5-ee81-4fef-9a02-621f7dd9c755', living_language_status__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d3382b5-ee81-4fef-9a02-621f7dd9c755', living_language_status__native_generation_reading, forecloses).
narrative_ontology:cs_axiom('6d3382b5-ee81-4fef-9a02-621f7dd9c755', foundational, literary_productivity_suffices_for_vitality).
narrative_ontology:cs_axiom_status(literary_productivity_suffices_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('6d3382b5-ee81-4fef-9a02-621f7dd9c755', literary_productivity_suffices_for_vitality, conventional).
narrative_ontology:cs_axiom('6d3382b5-ee81-4fef-9a02-621f7dd9c755', secondary, native_speaker_irrelevance_to_vitality).
narrative_ontology:cs_axiom_status(native_speaker_irrelevance_to_vitality, holdable).
narrative_ontology:cs_axiom_grounding('6d3382b5-ee81-4fef-9a02-621f7dd9c755', native_speaker_irrelevance_to_vitality, conventional).
narrative_ontology:cs_reference_frame('6d3382b5-ee81-4fef-9a02-621f7dd9c755', literary_productivity_as_vitality).
narrative_ontology:cs_drift_state('6d3382b5-ee81-4fef-9a02-621f7dd9c755', contemporary_native_speaker_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d3382b5-ee81-4fef-9a02-621f7dd9c755', '').
narrative_ontology:cs_kernel_id(living_language_status__literary_continuity_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, maskilim).
narrative_ontology:constraint_beneficiary(living_language_status__literary_continuity_reading, secular_intellectuals).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, illiterate_speakers).
narrative_ontology:constraint_victim(living_language_status__literary_continuity_reading, non_literary_communities).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, haskalah_revival_narrative).
narrative_ontology:constraint_vindicates(living_language_status__literary_continuity_reading, literary_vitality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Produced Haskalah periodicals and early modern Hebrew literature. Gained cultural authority and a legitimate secular medium for Jewish intellectual expression without needing to secure a mass native-speaking audience. Their literary output is cited as paradigmatic proof of the language's vitality.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, maskilim, beneficiary,
    moderate, biographical, mobile, national).

% Administer the definitional standard for linguistic vitality in academic journals, curricula, and cultural institutions. Assert that sustained literary and intellectual productivity suffices to declare a language living, regardless of native-speaker demographics. Derive prestige, canon-setting authority, and institutional resources from this framing.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, secular_intellectuals, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__literary_continuity_reading, secular_intellectuals, beneficiary).

% Speak the language or related vernaculars but do not read or write it. Their oral linguistic practice is excluded from the vitality calculus; they are rendered invisible by a definition that counts only literary and intellectual productivity.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, illiterate_speakers, payer,
    powerless, immediate, trapped, local).

% Maintain communal, oral, or religious traditions that fall outside the literary-intellectual frame. Their identity is bound to non-literary linguistic registers, which the constraint classifies as non-vital. They cannot exit to the literary standard without abandoning their communal identity.
narrative_ontology:constraint_stakeholder(living_language_status__literary_continuity_reading, non_literary_communities, payer,
    powerless, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared criterion for declaring a language 'living' that enables dispersed secular Jewish intellectuals to coordinate a revival project without first achieving mass native-speaker adoption. Unites literary production under a single legitimacy standard that bypasses rabbinic and liturgical gatekeepers.
% TRANSFER_FUNCTION: Moves cultural authority, institutional recognition, and canonical status from liturgical custodians and native-speaking communities to maskilim and secular intellectuals who produce literary and scholarly work in the target language.
% ABSENT_VOICES: Illiterate speakers and non-literary communities are excluded from the vitality calculus; traditional religious authorities who ground vitality in sacred continuity and linguists who insist on mother-tongue transmission are structurally absent from the definitional conversation. They would argue that recitation and daily speech are the genuine markers of linguistic life.
% DISAPPEARANCE_RATIONALE: If the literary continuity criterion vanished, the Haskalah revival narrative would lose its foundational legitimacy claim, modern Hebrew literature's sociolinguistic status would weaken, and recognition would shift toward liturgical or native-speaker criteria. The arrangement of cultural authority and language-policy funding would reorganize.
% FOUNDING_PROBLEM: Jewish secular intellectuals in the eighteenth and nineteenth centuries needed a legitimacy framework for producing new, non-sacred work in Hebrew without relying on rabbinic authority or an existing mass native-speaking base. The traditional liturgical framework treated Hebrew as a sacred language unsuited to secular modernity; the diaspora lacked a Hebrew mother-tongue community.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiographers and sociolinguists partially corroborate the historical need for a non-liturgical Hebrew legitimacy framework, but they are genealogically aligned with the beneficiary tradition. Traditional religious authorities outside the beneficiary set dispute that the problem was genuine, arguing that liturgical continuity already constituted full vitality. No fully independent corroboration exists.
narrative_ontology:disappearance_verdict(living_language_status__literary_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(living_language_status__literary_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__literary_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(living_language_status__literary_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__literary_continuity_reading, 0.3, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__literary_continuity_reading_tests).
:- end_tests(living_language_status__literary_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.30) because the constraint operates primarily as a definitional standard rather than a resource-extraction mechanism; its cost is borne as recognition-denial, not material transfer. Suppression is moderate (0.45) because the reading must actively displace liturgical and native-speaker alternatives in institutional discourse to maintain its dominance. Theater ratio is low (0.18): the literary productivity is genuine, though some later invocation becomes post-hoc justification. Accessibility collapse is moderate (0.55): once the literary frame is accepted, alternative vitality criteria lose institutional standing. Resistance is moderate (0.40): traditional and nativist linguists contest the framing.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seats (secular intellectuals, maskilim) experience the constraint as enabling coordination that liberated Hebrew from sacred monopoly. The payer seats (illiterate and non-literary speakers) experience it as a definitional erasure that denies their linguistic practice standing. The engine will compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular intellectuals and maskilim are beneficiaries: the constraint subsidizes their cultural authority and coordinates their project (low d). Illiterate speakers and non-literary communities are victims: the constraint extracts recognition and standing from them (high d). The exit asymmetry is sharp â intellectuals are mobile across frameworks, while non-literary communities are identity-locked or trapped.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimizing secular Hebrew without native speakers) was genuine and the coordination was real. However, modern Hebrew now possesses a robust native-speaker community, potentially rendering the original problem obsolete. The constraint persists because it has been generalized into a sociolinguistic standard and because the beneficiary class continues to derive authority from it. Mandatrophy is contested: if the problem is dead, the constraint drifts toward piton; if it remains live for other language revival contexts, it retains tangled-rope status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_exclusion_cost,
    'Does the literary continuity reading of living language status inherently foreclose liturgical and native-speaker definitions, or can it coexist with them as complementary dimensions of vitality?',
    'Comparative analysis of language policy documents and sociolinguistic frameworks that adopt multi-dimensional vitality indices versus those that employ single-criterion definitions.',
    'If the reading is inherently exclusive, its extraction component is higher than a coordination-only account suggests; if complementary, the victim classification may overstate the structural cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_exclusion_cost, conceptual, 'Whether the literary continuity reading necessarily excludes sibling readings or can coexist.').

omega_variable(
    elite_coordination_extraction_boundary,
    'Is the exclusion of illiterate and non-literary speakers an unavoidable side effect of elite literary coordination, or does the definition actively suppress alternative vitality criteria to maintain cultural authority?',
    'Historical analysis of Haskalah periodicals and language-policy debates: whether non-literary registers were merely ignored or actively delegitimized.',
    'If active suppression, the constraint trends toward snare; if passive side effect, it remains a low-extraction tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_coordination_extraction_boundary, empirical, 'Whether exclusion is incidental or instrumental.').

omega_variable(
    founding_problem_obsolescence,
    'Has the original founding problem â legitimizing Hebrew secular literature without native speakers â been rendered obsolete by the emergence of modern Hebrew as a native language, turning this constraint into a mandatrophy candidate?',
    'Sociolinguistic census and policy review: whether the literary continuity criterion is still invoked in contexts where native speaker bases exist or are irrelevant.',
    'If obsolete, the constraint''s persistence is inertial or performative, suggesting piton drift; if still live for other languages, it retains genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem is historically superseded.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__literary_continuity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__literary_continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(livi_tr_t4, living_language_status__literary_continuity_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(livi_tr_t8, living_language_status__literary_continuity_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(livi_tr_t12, living_language_status__literary_continuity_reading, theater_ratio, 12, 0.14).
narrative_ontology:measurement(livi_tr_t16, living_language_status__literary_continuity_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(livi_tr_t20, living_language_status__literary_continuity_reading, theater_ratio, 20, 0.18).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__literary_continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(livi_be_t4, living_language_status__literary_continuity_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(livi_be_t8, living_language_status__literary_continuity_reading, base_extractiveness, 8, 0.23).
narrative_ontology:measurement(livi_be_t12, living_language_status__literary_continuity_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement(livi_be_t16, living_language_status__literary_continuity_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(livi_be_t20, living_language_status__literary_continuity_reading, base_extractiveness, 20, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__literary_continuity_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(livi_su_t4, living_language_status__literary_continuity_reading, suppression_requirement, 4, 0.36).
narrative_ontology:measurement(livi_su_t8, living_language_status__literary_continuity_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(livi_su_t12, living_language_status__literary_continuity_reading, suppression_requirement, 12, 0.43).
narrative_ontology:measurement(livi_su_t16, living_language_status__literary_continuity_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(livi_su_t20, living_language_status__literary_continuity_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__liturgical_preservation_reading).
narrative_ontology:affects_constraint(living_language_status__literary_continuity_reading, living_language_status__native_generation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the living_language_status kernel. It is structurally distinct from the liturgical preservation reading (which locates vitality in sacred recitation) and the native generation reading (which requires mother-tongue transmission). Each reading carries a different Îµ, beneficiary set, and victim set; they are linked as a constraint family per the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
