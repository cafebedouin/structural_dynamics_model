% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra Reformist Contextual Reading
 *   domain: religious/legal/philosophical
 *
 * SUMMARY:
 *   The reformist contextual reading of Dharmasastra argues that the texts
 *   embody universal ethical principles (dharma as righteousness, duty, and
 *   moral order) that are separable from historically-contingent social
 *   prescriptions (the varna/caste system, gender hierarchy). This reading
 *   dominates modern Hindu scholarship and theological education, allowing
 *   Hindu identity to persist in secular democratic nation-states while
 *   publicly disavowing caste oppression. The constraint operates as a
 *   tangled rope: it coordinates Hindu identity across modernity (genuine
 *   coordination function) while simultaneously extracting interpretive
 *   authority away from the historically oppressed castes and women whose
 *   suffering Dharmasastra prescribed (asymmetric extraction). The reformist
 *   reading requires active enforcement—suppressing the abolitionist claim
 *   that the texts are irredeemably oppressive, and managing the gap between
 *   what the texts literally prescribe and what modern practitioners claim
 *   the texts actually mean.
 *
 * KEY AGENTS:
 *   - Brahminical scholarly establishment: custodians of Dharmasastra texts, authors of reformist interpretations, retain institutional authority
 *   - Upper-caste modernizers: benefit from a reading that allows them to claim Hindu identity without defending caste hierarchy
 *   - Historically oppressed castes and women: pay through subordination under both literal hierarchy and reformist reinterpretation; constrained exit
 *   - Abolitionist Hindu reformers: excluded from the conversation because their premise forecloses the reformist compromise
 *   - Hindu nationalist movements: observe and instrumentalize the debate; claim Hindu state authority via reformist Vedic sources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.52).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.48).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.52).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra Reformist Contextual Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious/legal/philosophical").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, 'c6ee3d4e-ec79-4996-8de4-00205bf5e837').
narrative_ontology:cs_kernel_codification('c6ee3d4e-ec79-4996-8de4-00205bf5e837', fixed_text).
narrative_ontology:cs_authority_grounding('c6ee3d4e-ec79-4996-8de4-00205bf5e837', lineage).
narrative_ontology:cs_interpretation_layer_present('c6ee3d4e-ec79-4996-8de4-00205bf5e837').
narrative_ontology:cs_reading_relation('c6ee3d4e-ec79-4996-8de4-00205bf5e837', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('c6ee3d4e-ec79-4996-8de4-00205bf5e837', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('c6ee3d4e-ec79-4996-8de4-00205bf5e837', foundational, dharma_universality_varna_contingency).
narrative_ontology:cs_axiom_status(dharma_universality_varna_contingency, holdable).
narrative_ontology:cs_axiom_grounding('c6ee3d4e-ec79-4996-8de4-00205bf5e837', dharma_universality_varna_contingency, deontological).
narrative_ontology:cs_axiom('c6ee3d4e-ec79-4996-8de4-00205bf5e837', foundational, textual_authority_preservable_through_contextual_interpretation).
narrative_ontology:cs_axiom_status(textual_authority_preservable_through_contextual_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('c6ee3d4e-ec79-4996-8de4-00205bf5e837', textual_authority_preservable_through_contextual_interpretation, conventional).
narrative_ontology:cs_reference_frame('c6ee3d4e-ec79-4996-8de4-00205bf5e837', universal_dharmic_ethics_within_contextual_varna).
narrative_ontology:cs_drift_state('c6ee3d4e-ec79-4996-8de4-00205bf5e837', contemporary_secular_democratic_nation_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c6ee3d4e-ec79-4996-8de4-00205bf5e837', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, brahminical_scholarly_establishment).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, upper_caste_modernizers).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, historically_oppressed_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, women_under_dharmic_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodians and transmitters of Dharmasastra texts. They author interpretations that preserve textual authority while reframing oppressive prescriptions as time-bound rather than eternal. This allows them to maintain their role as authoritative interpreters without abandoning the texts themselves. Their scholarly legitimacy depends on the texts remaining binding, even if selectively reinterpreted.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, brahminical_scholarly_establishment, agenda_setter,
    institutional, generational, identity_locked, regional).

% Educated elites from upper castes who want to retain Hindu identity and textual tradition while adopting modern egalitarian values. They benefit from a reading that allows them to claim ethical authority from Dharmasastra without defending caste hierarchy literally. The reformist framing permits them to be 'both Hindu and modern' without ideological rupture.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, upper_caste_modernizers, beneficiary,
    powerful, biographical, arbitrage, national).

% Subordinated under both literal caste prescriptions and the reformist reinterpretation. The reformist reading softens the prescription language but preserves textual authority—the same authority that originally justified their oppression. They are offered inclusion via reinterpretation rather than systemic change. Any exit via rejecting Dharmasastra frames them as rejecting Hindu tradition itself, raising the cost of resistance.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, historically_oppressed_castes, payer,
    moderate, generational, constrained, national).

% Subordinated under prescriptions about female duty, obedience, and life stages. The reformist reading reframes women's roles as contextual rather than eternal, but offers no mechanism for women themselves to author new interpretations. Women remain subjects of reinterpretation rather than agents of textual authority.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, women_under_dharmic_hierarchy, payer,
    powerless, biographical, identity_locked, regional).

% Reformers who argue that Dharmasastra is fundamentally oppressive and that Hindu modernization requires rejecting the textual framework entirely. They are excluded from the orthodox scholarly conversation because their position forecloses the reformist compromise: if the texts are intrinsically oppressive, no reinterpretation rescues them.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, abolitionist_hindu_reformers, excluded,
    moderate, generational, mobile, national).

% Political movements that claim Hindu identity and invoke Dharmasastra to validate claims about Hindu nationalism, Hindu law, and Hindu state authority. They observe and sometimes instrumentalize the scholarly debate. The reformist reading's success in preserving textual authority is useful to nationalist projects that want legitimacy from ancient Hindu sources.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, hindu_nationalist_movements, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dharmasastra_corpus__reformist_contextual, brahminical_scholarly_establishment).
narrative_ontology:fixing_cost_class(dharmasastra_corpus__reformist_contextual, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains scholarly coherence and textual transmission: a framework that allows Dharmasastra to function as binding authority for Hindu identity while filtering out prescriptions now recognized as harmful. Solves the problem of how tradition-oriented Hindus can modernize without experiencing cultural rupture.
% TRANSFER_FUNCTION: Transfers interpretive authority away from those historically oppressed by literal Dharmasastra (lower castes, women) toward educated upper-caste scholars and modernizers. The constraint moves legitimacy to those who can reinterpret the texts, and away from those who would reject them entirely. Bhakti devotionalism and pluralist Hindu theology lose standing relative to Vedic Dharmasastra scholarship.
% ABSENT_VOICES: Historically oppressed castes and women are named as victims but have no seat at the scholarly interpretation table. Abolitionist reformers are structurally excluded because their core claim (the framework is irredeemable) contradicts the reformist premise. The voices absent are those who would demand either full abandonment of the texts or equal authority to reinterpret them.
% DISAPPEARANCE_RATIONALE: If the reformist reading collapsed and Dharmasastra lost its scholarly authority, Hindu modernizers would face the choice between literal adherence to caste hierarchy (impossible in modern secular nation-states) and finding legitimacy outside Vedic texts entirely. The entire modern Hindu identity project that claims both textual rootedness and democratic egalitarianism would require reconstruction. This framework's disappearance would force much sharper ideological choices.
% FOUNDING_PROBLEM: Colonial and postcolonial Hindu reformers faced irreconcilable demands: modernize while preserving tradition; reject caste hierarchy while retaining Hindu identity; maintain the Vedic texts as authoritative while acknowledging their oppressive prescriptions. The reformist contextual reading emerged to solve this bind by separating the ethical core (dharma) from the institutional structure (caste).
% FOUNDING_PROBLEM_CORROBORATION: Modern Hindu theologians and scholars from the Ramakrishna Mission, Hindu Renaissance thinkers, and academic Indology cite this founding problem and defend the reformist solution. Lower-caste scholars and dalit intellectuals like B. R. Ambedkar contested the solution itself, arguing that the founding problem cannot be solved through reinterpretation—only through rejection. Independent observers in religious studies scholarship note that the reformist reading, while historically real and widely adopted, has not resolved tensions between traditionalism and egalitarianism; it has merely deferred them.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.52 at interval end) because interpretive authority is concentrated—those who reinterpret the texts accumulate power relative to those subordinated by them. Suppression is measured moderate (0.48) because the constraint does not require violent enforcement; instead, it manages discourse and frames alternatives as incompatible with Hindu identity itself. Theater ratio climbs from 0.25 to 0.41 as the reading matures: early reformism emphasized actual ethical principles (lower theater); maturation emphasizes textual authority preservation and scholarly legitimacy (higher theater—more performance, less functional change to hierarchy). Accessibility of alternatives collapses moderately (0.65): a person subordinated by the texts can reject Dharmasastra, but doing so frames them as rejecting Hindu identity itself, raising the exit cost. Resistance is high (0.72) because the constraint faces constant pressure from two directions: abolitionist reformers argue it doesn't go far enough, and orthodox literalists argue it goes too far. The measurement series track the constraint's stabilization: extractiveness plateaus as the reading becomes institutionalized; suppression falls slightly as social norms internalize the reframed hierarchy; theater rises as the reading matures from reform movement to scholarly establishment.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (brahminical scholars) and beneficiary seats experience this as genuine coordination and justified reinterpretation. From their perspective, the reading solves a real problem: how to be Hindu in modernity. From the payer seats (oppressed castes, women), the constraint operates as enforced interpretive monopoly—they are told what the texts mean by those with institutional authority to say so, and their own reading (that the texts are oppressive) is foreclosed by being labeled incompatible with Hinduism itself. The engine computes these divergences from the structural asymmetry: the agenda-setter has high power and arbitrage exit; the payer seats have constrained or identity-locked exit relative to Hindu identity. The same texts and the same interpretations produce radically different constraint classifications across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahminical scholars have d near 0.2–0.3 (beneficiaries with institutional power and arbitrage exit—low effective extraction, possible subsidy). Upper-caste modernizers have d near 0.25–0.35 (beneficiaries with powerful institutional position and high exit mobility). Historically oppressed castes have d near 0.70–0.80 (targets with moderate power and constrained exit—trapped by the identity cost of rejecting Hinduism). Women under the hierarchy have d near 0.75–0.85 (targets with powerless position and identity-locked exit—leaving Hinduism forecloses community, family, and cultural belonging). Abolitionist reformers have d near 0.55 (moderate position: they benefit from the constraint's failure to foreclose their argument, but pay through exclusion). The asymmetry is structural: the constraint's persistence depends on those with institutional power to interpret texts, and those without such power must accept or pay the exit cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The reformist reading was founded to solve a real problem: how to maintain Hindu identity and practice in contexts (secular nation-states, modern ethics, egalitarian norms) where literal Dharmasastra prescriptions are impossible to implement. That founding problem is still live but contested. Orthodox literalists argue the problem is false—the prescriptions are still valid and should be observed. Abolitionists argue the founding problem cannot be solved through reinterpretation; only rejection works. The mandatrophy risk is high: if the founding problem is reframed as solved (we have successfully modernized Hinduism), the reading becomes a zombie constraint—it persists as textual authority and scholarly infrastructure but no longer serves the problem it was designed for. The theater ratio's climb to 0.41 suggests this is already happening: the reading is maintained partly through scholarly performance and interpretive authority, not purely through functional modernization. The constraint is not yet fully mandatrophied because Hindu identity still needs the reading to navigate modernity; but if secular Hindu identity becomes fully decoupled from Dharmasastra (as may be happening in diaspora contexts), the constraint risks becoming purely theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_core_separability,
    'Are the universal ethical principles (dharma as righteousness) actually separable from the caste-bound prescriptions in the Dharmasastra texts, or is the attempt to separate them a retrospective reading imposed by modern reformers?',
    'Philological and textual analysis: do the foundational texts themselves treat dharma as universal and caste-dharma as particular? Or is the universality reading a modern scholarly construction applied backward to the texts?',
    'If genuinely separable, the reformist reading succeeds at preserving textual authority while discarding oppressive elements. If retrospectively imposed, the reading is a noble lie—it preserves authority through reinterpretation, not through accurate understanding of what the texts actually assert.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_core_separability, empirical, 'Whether dharma''s universality is inherent to Dharmasastra or imposed by reformist interpretation.').

omega_variable(
    identity_cost_of_rejection,
    'What portion of the constraint''s persistence is due to the identity cost of rejecting Dharmasastra (leaving Hinduism) versus the constraint''s own functional utility?',
    'Observation of secular Hindu identity development: if Hindus increasingly practice, identify, and transmit Hinduism without reference to Dharmasastra authority, the identity cost is falling. If Dharmasastra remains central to what ''being Hindu'' means, the identity lock is strong.',
    'If the identity lock weakens, the constraint''s classification may shift from tangled_rope (coordination + extraction) toward snare (pure extraction), because the coordination function (Hindu modernization) becomes independent of Dharmasastra authority. Conversely, if the identity lock strengthens, the constraint''s effectiveness increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_cost_of_rejection, empirical, 'Whether the constraint''s hold depends on identity fusion or functional necessity.').

omega_variable(
    authority_concentration_in_reformist_reading,
    'Does the reformist reading actually distribute interpretive authority more widely, or does it concentrate authority in the hands of modern educated elites (scholars, theologians, journalists) who have the literacy and institutional position to offer authoritative reinterpretations?',
    'Mapping of who authors, teaches, and enforces interpretations in practice: do women, lower-caste members, and historically oppressed groups have equal standing to offer competing reformist readings, or is interpretive authority still gatekept by brahminical and upper-caste scholars?',
    'If authority is gatekept, the reformist reading preserves brahminical interpretive monopoly under a modern egalitarian framing—it is a false summit claim (natural-law-style authority preservation) rather than genuine democratization. If authority is genuinely distributed, the reading succeeds at pluralism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_concentration_in_reformist_reading, empirical, 'Whether the reformist reading distributes or concentrates interpretive authority.').

omega_variable(
    reformist_vs_abolitionist_foreclosure,
    'Does the reformist reading''s core premise (separability of dharma from varna) logically foreclose the abolitionist claim that the texts are irredeemably oppressive, or do these remain coexisting live positions?',
    'Logical analysis: if a reformist successfully argues that dharma is separable and retainable, have they thereby proven the abolitionist wrong? Or have they only offered a different solution to the same problem (oppressive texts in modern Hinduism)?',
    'If foreclosure is genuine, the reformist reading is winning a logical battle. If coexistence is maintained, both readings remain live—the reformist reading does not eliminate the abolitionist option. The relationship between these readings affects whether the reformist reading itself becomes a boundary-maintenance mechanism (enforcing the choice between reformist modernization and complete rejection, with abolition left out).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reformist_vs_abolitionist_foreclosure, conceptual, 'Whether the reformist and abolitionist readings are logically foreclosed or coexisting alternatives.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of abolitionist voices and alternative readings structural (institutional gatekeeping, control of scholarly platforms) or internalized (people internalize the idea that rejecting Dharmasastra means rejecting Hinduism)?',
    'Post-suppression trajectory: if voices that rejected the constraint were given full institutional platform and resources, would abolitionist and alternative readings flourish, or have they become psychologically embedded as incompatible with Hindu identity?',
    'If suppression is structural, removing institutional gatekeeping could rapidly shift the constraint. If internalized, the suppression persists even after institutional barriers fall—the constraint has become self-perpetuating through identity fusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternatives is structural or internalized in Hindu identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t0, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dhar_tr_t4, dharmasastra_corpus__reformist_contextual, theater_ratio, 4, 0.3).
narrative_ontology:measurement(dhar_tr_t8, dharmasastra_corpus__reformist_contextual, theater_ratio, 8, 0.35).
narrative_ontology:measurement(dhar_tr_t12, dharmasastra_corpus__reformist_contextual, theater_ratio, 12, 0.39).
narrative_ontology:measurement(dhar_tr_t18, dharmasastra_corpus__reformist_contextual, theater_ratio, 18, 0.41).
narrative_ontology:measurement(dhar_tr_t25, dharmasastra_corpus__reformist_contextual, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(dhar_be_t0, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dhar_be_t4, dharmasastra_corpus__reformist_contextual, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(dhar_be_t8, dharmasastra_corpus__reformist_contextual, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(dhar_be_t12, dharmasastra_corpus__reformist_contextual, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(dhar_be_t18, dharmasastra_corpus__reformist_contextual, base_extractiveness, 18, 0.52).
narrative_ontology:measurement(dhar_be_t25, dharmasastra_corpus__reformist_contextual, base_extractiveness, 25, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t0, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dhar_su_t4, dharmasastra_corpus__reformist_contextual, suppression_requirement, 4, 0.53).
narrative_ontology:measurement(dhar_su_t8, dharmasastra_corpus__reformist_contextual, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(dhar_su_t12, dharmasastra_corpus__reformist_contextual, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(dhar_su_t18, dharmasastra_corpus__reformist_contextual, suppression_requirement, 18, 0.48).
narrative_ontology:measurement(dhar_su_t25, dharmasastra_corpus__reformist_contextual, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:boltzmann_floor_override(dharmasastra_corpus__reformist_contextual, 0.12).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, hindu_caste_system_institutional_persistence).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, brahminical_scholarly_authority_gate).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Dharmasastra corpus kernel. The orthodox_literalist reading (constraint ID: dharmasastra_corpus__orthodox_literalist) maintains that varna hierarchy is eternal and binding. The abolitionist_rejection reading (constraint ID: dharmasastra_corpus__abolitionist_rejection) argues the entire textual framework must be abandoned. The reformist_contextual reading (this constraint) coexists with both but attempts to preserve textual authority while rejecting oppressive prescriptions. Each reading has its own epsilon, beneficiary/victim structure, and classification. The readings are linked through network.affects_constraints to show how they influence each other: the reformist reading's success depends partly on its ability to preempt the abolitionist challenge by offering a middle path.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__reformist_contextual, powerful, 0.28).
constraint_indexing:directionality_override(dharmasastra_corpus__reformist_contextual, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
