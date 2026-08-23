% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__orthodox_literalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__orthodox_literalist, []).

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
 *   constraint_id: dharmasastra_corpus__orthodox_literalist
 *   human_readable: Dharmasastra Orthodox Literalist Reading: Eternal Varna Hierarchy
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint instantiates the orthodox literalist reading of the
 *   Dharmasastra kernel: the claim that varna and jati hierarchy, along with
 *   associated ritual and educational exclusions, constitute eternal
 *   (sanatana) revealed truth demanding literal observance. Under this
 *   reading, Dharmasastra is not historical law but transcendental
 *   legislation. The structural effect is an expansive victim set â
 *   Shudras, Dalits, and women â excluded from Vedic study, ritual
 *   authority, and full social personhood, while Brahminical interpreters and
 *   upper-caste householders concentrate ritual status and economic
 *   privilege. The reading is one of three in the dharmasastra_corpus kernel
 *   family; it forecloses both contextual reform and abolitionist rejection
 *   because its core premise (eternal, infallible text) logically contradicts
 *   historicization and legitimacy-denial.
 *
 * KEY AGENTS:
 *   - brahminical_interpreter: Agenda-setter and primary beneficiary (institutional/constrained) â controls textual interpretation and ritual gatekeeping.
 *   - upper_caste_householders: Secondary beneficiary (powerful/constrained) â receive status and labor advantages from the hierarchy.
 *   - shudra_laborers: Primary target (powerless/trapped) â serve the labor needs of upper varnas, excluded from ritual equality.
 *   - dalit_communities: Primary target (powerless/trapped) â positioned outside the varna system, subjected to untouchability and severe exclusion.
 *   - women_across_varnas: Target (powerless/identity_locked) â excluded from Vedic education and ritual autonomy across caste lines.
 *   - reformist_scholars: Excluded voice (moderate/constrained) â argue for contextual or egalitarian readings but are denied authority in orthodox institutions.
 *   - critical_historian: Analytical observer (analytical/analytical) â examines the textual and social history from outside the normative frame.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, 0.88).
domain_priors:suppression_score(dharmasastra_corpus__orthodox_literalist, 0.92).
domain_priors:theater_ratio(dharmasastra_corpus__orthodox_literalist, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, extractiveness, 0.88).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(dharmasastra_corpus__orthodox_literalist, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__orthodox_literalist, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__orthodox_literalist, "Dharmasastra Orthodox Literalist Reading: Eternal Varna Hierarchy").
narrative_ontology:topic_domain(dharmasastra_corpus__orthodox_literalist, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__orthodox_literalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__orthodox_literalist, 'f7be6a79-0915-488e-aed4-4c3f8bf98032').
narrative_ontology:cs_kernel_codification('f7be6a79-0915-488e-aed4-4c3f8bf98032', fixed_text).
narrative_ontology:cs_authority_grounding('f7be6a79-0915-488e-aed4-4c3f8bf98032', lineage).
narrative_ontology:cs_interpretation_layer_present('f7be6a79-0915-488e-aed4-4c3f8bf98032').
narrative_ontology:cs_reading_relation('f7be6a79-0915-488e-aed4-4c3f8bf98032', dharmasastra_corpus__reformist_contextual, forecloses).
narrative_ontology:cs_reading_relation('f7be6a79-0915-488e-aed4-4c3f8bf98032', dharmasastra_corpus__abolitionist_rejection, forecloses).
narrative_ontology:cs_axiom('f7be6a79-0915-488e-aed4-4c3f8bf98032', foundational, vedic_revelation_infallible).
narrative_ontology:cs_axiom_status(vedic_revelation_infallible, holdable).
narrative_ontology:cs_axiom_grounding('f7be6a79-0915-488e-aed4-4c3f8bf98032', vedic_revelation_infallible, theological).
narrative_ontology:cs_axiom('f7be6a79-0915-488e-aed4-4c3f8bf98032', foundational, birth_based_dharma_obligation).
narrative_ontology:cs_axiom_status(birth_based_dharma_obligation, holdable).
narrative_ontology:cs_axiom_grounding('f7be6a79-0915-488e-aed4-4c3f8bf98032', birth_based_dharma_obligation, deontological).
narrative_ontology:cs_reference_frame('f7be6a79-0915-488e-aed4-4c3f8bf98032', eternal_revelation_framework).
narrative_ontology:cs_drift_state('f7be6a79-0915-488e-aed4-4c3f8bf98032', contemporary_secular_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f7be6a79-0915-488e-aed4-4c3f8bf98032', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, brahminical_interpreter).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__orthodox_literalist, upper_caste_householders).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, shudra_laborers).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, dalit_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__orthodox_literalist, women_across_varnas).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, varna_dharma_doctrine).
narrative_ontology:constraint_vindicates(dharmasastra_corpus__orthodox_literalist, birth_based_ritual_eligibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls access to Vedic learning, ritual performance, and textual interpretation. Monopolizes the authority to declare what Dharmasastra literally requires. Receives ritual prestations, social deference, and educational gatekeeping power. Renouncing this role means losing caste identity and authority, though they possess the widest option set within the system.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, brahminical_interpreter, agenda_setter,
    institutional, civilizational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(dharmasastra_corpus__orthodox_literalist, brahminical_interpreter, beneficiary).

% Receive labor services, ritual purity status, and marriage-network advantages from the varna hierarchy. Their social standing depends on the maintenance of caste boundaries. Exit is constrained because abandoning the hierarchy would collapse their accumulated status and inter-caste economic relationships.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, upper_caste_householders, beneficiary,
    powerful, generational, constrained, regional).

% Provide agricultural, artisanal, and service labor to upper-varna households. Excluded from Vedic study, upanayana initiation, and ritual equality. Caste ascription is birth-bound; geographic or religious exit is blocked by social boycott and economic dependency.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, shudra_laborers, payer,
    powerless, biographical, trapped, regional).

% Positioned outside the four-varna framework as avarna, subjected to untouchability rules, residential segregation, and exclusion from temples and public wells. Perform sanitation and carcass-removal work under severe coercion. Conversion or migration offers limited relief because untouchability stigma often follows.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, dalit_communities, payer,
    powerless, biographical, trapped, regional).

% Excluded from Vedic study, upanayana, and independent ritual authority across all varnas. Their social identity is fused with patrilineal household roles. Exit from the gendered constraints of Dharmasastra is unavailable without severing familial and communal belonging.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, women_across_varnas, payer,
    powerless, biographical, identity_locked, regional).

% Argue for historical contextualization, ethical core separability, or egalitarian reinterpretation. They are systematically denied standing within Brahminical interpretive institutions and ritual forums; their voices are absent from the orthodox seat of authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, reformist_scholars, excluded,
    moderate, generational, constrained, regional).

% Examines the Dharmasastra corpus as historically situated law-texts, analyzing the relationship between textual claims and social power. Neither benefits from nor is extracted by the constraint; evaluates the kernel and its readings from outside the normative frame.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__orthodox_literalist, critical_historian, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__orthodox_literalist, brahminical_interpreter).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__orthodox_literalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes ritual, social, and economic life into a hierarchically ordered cosmos where each varna and jati has prescribed duties (svadharma), purportedly ensuring cosmic stability (rita) and interdependent social function through fixed rank and obligation.
% TRANSFER_FUNCTION: Transfers labor surplus, ritual privilege, educational access, and social status from Shudras, Dalits, and women to Brahminical interpreters and upper-caste householders, under the sign of divine ordering.
% ABSENT_VOICES: Dalit theologians, women ritualists, Shudra philosophers, and reformist scholars who would argue for egalitarian spiritual access and against birth-based ascription are excluded from textual interpretation and ritual authority. Their absence is enforced by denying them Sanskrit education and public ritual standing.
% DISAPPEARANCE_RATIONALE: If the literalist prescription vanished, varna-based exclusions would collapse, Shudras and Dalits would enter ritual and educational spheres previously monopolized, women would access Sanskrit learning and public ritual autonomy, and the Brahminical interpretive monopoly would lose its foundational authority â social and religious order would reorganize around non-hierarchical principles.
% FOUNDING_PROBLEM: How to maintain cosmic and social order (rita/dharma) in a population with diverse occupations and ritual needs, by assigning fixed duties and privileges according to birth.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist and Jain texts from the same era critique the Brahminical order, attesting the hierarchy was contested from its inception. Modern historians, anthropologists, and constitutional scholars outside the beneficiary set attest that the founding problem was a claim of the Brahminical class rather than an emergent social necessity, and that less extractive coordination mechanisms now exist.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__orthodox_literalist, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__orthodox_literalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__orthodox_literalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dharmasastra_corpus__orthodox_literalist, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__orthodox_literalist, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__orthodox_literalist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__orthodox_literalist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__orthodox_literalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88) is high because the constraint systematically transfers ritual authority, educational access, and labor surplus from Shudras, Dalits, and women to Brahminical and upper-caste beneficiaries. Suppression (0.92) is very high: the constraint depends on active enforcement through ritual ostracism, pollution rules, social boycott, and in many periods state-backed caste courts. Accessibility_collapse (0.75) is high within the orthodox frame because alternatives (Buddhism, Jainism, bhakti egalitarianism) are doctrinally delegitimized as adharma, though they persist at the margins. Resistance (0.55) reflects persistent but fragmented opposition across millennia. Theater_ratio (0.25) is low because the orthodox literalist reading treats the prescriptions as genuinely functional cosmic law rather than performative maintenance; ritual action is understood as efficacious, not theatrical. The temporal series shows extraction and suppression intensifying as the textual canon crystallized (T=500), then remaining severe through the medieval period, with a modest rise in theater_ratio as modern secular challenges erode literal enforcement and orthodox performance becomes more defensive.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Brahminical interpreter) experiences the constraint as necessary cosmic order and personal dharma; the engine will compute a low directionality and may read the constraint as coordination-dominant from that seat. The payer seats (Shudra, Dalit, women) experience the same structure as enforced extraction with no viable exit; the engine will compute high directionality and high effective extraction. This divergence is not a contradiction but the structural signature of a tangled rope: the same arrangement coordinates the cosmos for the beneficiary while extracting life-chances from the target.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (brahminical_interpreter, upper_caste_householders) are positioned at low d: they control the rules, collect ritual and status rents, and have the widest (though still constrained) exit options. Victims (shudra_laborers, dalit_communities, women_across_varnas) are positioned at high d: they bear the costs of exclusion, have trapped or identity_locked exit, and face amplification of extraction through large spatial_scope (the constraint claims regional/civilizational scope). The derivation chain requires no override; the structural data already maps the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â maintaining social and cosmic order through fixed duties â is contested as either dead (modern constitutional equality solves coordination without hierarchy) or transformed (bhakti and secular civics provide non-extractive coordination). The orthodox literalist reading persists despite these alternatives, suggesting mandatrophy. However, because the constraint still delivers genuine coordination benefits to beneficiaries (ritual community, social identity, interdependence), it is classified as tangled_rope rather than snare or piton. The persistence of the literalist frame after the founding problem has been superseded by less extractive alternatives is exactly the drift the temporal measurements capture: theater_ratio slowly rising as enforcement becomes defensive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression in Dharmasastra enforcement structural (social ostracism, economic exclusion, violence) or internalized (belief in karma and divine order justifying birth-status)?',
    'Post-exit suppression trajectory: if Dalits and Shudras who convert to Buddhism or relocate continue to face exclusion, suppression is partially structural; if psychological subordination persists independently of external enforcement, it is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â targets carry the suppression with them after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in caste enforcement').

omega_variable(
    dharmasastra_kernel_reading_relation,
    'Does the orthodox literalist reading foreclose the reformist contextual and abolitionist rejection readings, or can they coexist within a single interpretive framework?',
    'Examination of whether any orthodox literalist tradition admits historical contextualization or legitimacy-denial as valid hermeneutic methods without collapsing into reformism or abolition.',
    'Determines whether the kernel readings are mutually exclusive (forecloses) or competing social positions (coexists_with), affecting contamination propagation across the constraint family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dharmasastra_kernel_reading_relation, conceptual, 'Logical relationship between orthodox literalist and sibling kernel readings').

omega_variable(
    eternality_vs_historicity,
    'Are Dharmasastra varna prescriptions genuinely eternal and revealed, or are they historically contingent law-code compositions reflecting specific socio-political moments?',
    'Philological and historical analysis of textual stratification, anachronisms, and sociological embedding of Dharmasastra texts.',
    'If historically contingent, the constraint''s authority_grounding shifts from lineage/theological to extraction/conventional, reclassifying the seat structure and falsifying the mountain claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(eternality_vs_historicity, empirical, 'Whether the textual kernel is natural-law eternal or constructed historical law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__orthodox_literalist, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__orthodox_literalist, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dhar_tr_t500, dharmasastra_corpus__orthodox_literalist, theater_ratio, 500, 0.15).
narrative_ontology:measurement(dhar_tr_t1000, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1000, 0.2).
narrative_ontology:measurement(dhar_tr_t1500, dharmasastra_corpus__orthodox_literalist, theater_ratio, 1500, 0.25).
narrative_ontology:measurement(dhar_tr_t2000, dharmasastra_corpus__orthodox_literalist, theater_ratio, 2000, 0.35).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(dhar_be_t500, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 500, 0.82).
narrative_ontology:measurement(dhar_be_t1000, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1000, 0.9).
narrative_ontology:measurement(dhar_be_t1500, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 1500, 0.88).
narrative_ontology:measurement(dhar_be_t2000, dharmasastra_corpus__orthodox_literalist, base_extractiveness, 2000, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(dhar_su_t500, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 500, 0.88).
narrative_ontology:measurement(dhar_su_t1000, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1000, 0.95).
narrative_ontology:measurement(dhar_su_t1500, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 1500, 0.92).
narrative_ontology:measurement(dhar_su_t2000, dharmasastra_corpus__orthodox_literalist, suppression_requirement, 2000, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__orthodox_literalist, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested dharmasastra_corpus kernel. The orthodox literalist reading treats the varna/jati hierarchy as eternal revealed truth; the reformist contextual reading historicizes it; the abolitionist rejection reading denies its legitimacy entirely. Each reading instantiates a distinct constraint with different epsilon values and victim/beneficiary structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
