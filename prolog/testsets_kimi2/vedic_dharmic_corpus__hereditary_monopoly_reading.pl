% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__hereditary_monopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__hereditary_monopoly_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Hereditary Brahmin Monopoly on Vedic Ritual Authority
 *   domain: religious_authority/social_stratification
 *
 * SUMMARY:
 *   This constraint instantiates the hereditary_monopoly_reading of the
 *   vedic_dharmic_corpus kernel. It treats ritual and interpretive authority
 *   as fixed by birth into Brahmin lineage, with varna hierarchy justified as
 *   divinely ordained and textually prescribed. The reading extracts material
 *   and status resources from lower-caste communities and women while
 *   providing genuine but monopolized ritual coordination. The sibling
 *   bhakti_devotional_reading and reformist_egalitarian_reading instantiate
 *   structurally different constraints from the same textual kernel.
 *
 * KEY AGENTS:
 *   - brahmin_priestly_class: Primary agenda-setter and beneficiary (institutional/arbitrage) â controls ritual and textual monopoly
 *   - lower_caste_communities: Primary target (powerless/trapped) â bear material extraction and exclusion
 *   - women_across_varnas: Secondary target (powerless/identity_locked) â barred by gender from authority regardless of varna
 *   - bhakti_practitioners: Excluded alternative (organized/mobile) â devotional movements bypassing priestly mediation
 *   - modern_reform_institutions: Analytical observer (institutional/analytical) â constitutional and statutory monitors
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65).
domain_priors:suppression_score(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.72).
domain_priors:theater_ratio(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Brahmin Monopoly on Vedic Ritual Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious_authority/social_stratification").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, '15ea7094-287b-4f48-8abc-93d4ba31b32d').
narrative_ontology:cs_kernel_codification('15ea7094-287b-4f48-8abc-93d4ba31b32d', fixed_text).
narrative_ontology:cs_authority_grounding('15ea7094-287b-4f48-8abc-93d4ba31b32d', lineage).
narrative_ontology:cs_interpretation_layer_present('15ea7094-287b-4f48-8abc-93d4ba31b32d').
narrative_ontology:cs_reading_relation('15ea7094-287b-4f48-8abc-93d4ba31b32d', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('15ea7094-287b-4f48-8abc-93d4ba31b32d', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('15ea7094-287b-4f48-8abc-93d4ba31b32d', foundational, birth_derived_ritual_authority).
narrative_ontology:cs_axiom_status(birth_derived_ritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('15ea7094-287b-4f48-8abc-93d4ba31b32d', birth_derived_ritual_authority, theological).
narrative_ontology:cs_axiom('15ea7094-287b-4f48-8abc-93d4ba31b32d', foundational, varna_hierarchy_divine_ordinance).
narrative_ontology:cs_axiom_status(varna_hierarchy_divine_ordinance, holdable).
narrative_ontology:cs_axiom_grounding('15ea7094-287b-4f48-8abc-93d4ba31b32d', varna_hierarchy_divine_ordinance, theological).
narrative_ontology:cs_reference_frame('15ea7094-287b-4f48-8abc-93d4ba31b32d', brahminical_ritual_supremacy).
narrative_ontology:cs_drift_state('15ea7094-287b-4f48-8abc-93d4ba31b32d', post_independence_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('15ea7094-287b-4f48-8abc-93d4ba31b32d', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_caste_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women_across_varnas).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary custodians of Vedic textual tradition and ritual performance. They alone may perform core sacrifices, interpret scripture, and train successors. Their authority derives from birth lineage and is maintained through endogamy, textual transmission, and control of temple economies. They receive material support, land grants, and deference in exchange for ritual services.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, beneficiary).

% Provide labor, agricultural surplus, and ritual fees to temples and Brahmin households. Excluded from Vedic learning, initiation, and priestly roles by birth. Their social and economic mobility is bound to the ritual economy; alternatives such as independent religious practice draw penalties or exclusion from communal resources.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, lower_caste_communities, payer,
    powerless, generational, trapped, continental).

% Excluded from Vedic study, initiation, and priestly authority regardless of birth varna. Their ritual participation is mediated through male relatives or specialized female roles that do not confer interpretive authority. Gender is treated as a fixed identity category that permanently bars access to the textual and ritual core, while their labor and reproductive roles sustain the lineage system.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women_across_varnas, payer,
    powerless, generational, identity_locked, continental).

% Offer direct devotional worship that bypasses Brahminical ritual mediation. They are structurally excluded from temple priesthoods and orthodox textual institutions, yet their movements attract large followings. Their existence challenges the monopoly but they lack institutional authority within the hereditary framework.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_practitioners, excluded,
    organized, biographical, mobile, continental).

% Constitutional and statutory bodies that document caste discrimination, mandate affirmative action, and assert egalitarian principles over traditional authority. They observe and legally intervene in the constraint but are denied legitimacy within the orthodox interpretive framework.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, modern_reform_institutions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious ritual, textual preservation, and social order by assigning hereditary priestly specialists to maintain sacrificial continuity, interpret divine law, and regulate pollution-purity boundaries across the community.
% TRANSFER_FUNCTION: Moves material resources (land, gifts, fees, labor) and deference from lower castes and women to the Brahmin priestly class, justified as necessary support for ritual specialists who mediate between the divine and social order.
% ABSENT_VOICES: Bhakti practitioners who claim devotional access bypasses birth requirements; reformist movements arguing for textual egalitarianism; lower-caste intellectuals asserting self-taught scriptural authority. They are excluded from temples, Vedic schools, and interpretive institutions.
% DISAPPEARANCE_RATIONALE: If hereditary ritual authority vanished, temple economies would collapse, inter-caste service obligations would unravel, and the social map of purity and pollution would lose its institutional anchor. The Brahmin class would lose its exclusive occupational niche, while lower castes would face both liberation and the collapse of a predictable, if extractive, social order.
% FOUNDING_PROBLEM: How to maintain correct sacrificial procedure, textual transmission, and social integration in a complex agrarian society without universal literacy or shared access to esoteric knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Colonial ethnographers and modern sociologists attest that the ritual economy served social integration; modern constitutional framers and lower-caste political movements attest that the founding problem is superseded by modern institutions and the arrangement now functions as hereditary rent. Corroboration from outside the Brahmin beneficiary set is extensive.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_dharmic_corpus__hereditary_monopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_dharmic_corpus__hereditary_monopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_dharmic_corpus__hereditary_monopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the hereditary monopoly channels substantial material flows to a closed birth-group, though it partially reflects genuine ritual labor. Suppression (0.72) is high because persistence depends on active temple control, social boycott, and marriage restrictions. Theater_ratio (0.48) indicates substantial performative maintenance of purity rules that masks declining functional necessity in the modern economy. Accessibility_collapse (0.78) is high because within the orthodox frame alternatives (bhakti, self-study) are delegitimized. Resistance (0.55) reflects sustained reform and lower-caste mobilization despite repression. The measurement series run on a single shared grid.
 *
 * PERSPECTIVAL GAP:
 *   From the Brahmin seat the arrangement is necessary sacrificial infrastructure; from lower-caste and women seats it is enforced extraction with religious justification. The engine computes divergent per-seat types from the same structural data â the agenda-setter/beneficiary experiences low directionality while the payers experience high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin class is declared agenda-setter and beneficiary, giving it low directionality (constraint subsidizes its position). Lower-caste communities and women are declared payers with trapped and identity_locked exit options respectively, placing them near the full-target end. The differential exit options â trapped by economic-social coercion versus identity_locked by immutable gender ascription within this system â produce differentiated effective extraction even at the same power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ritual coordination in a non-literate agrarian society) is contested. If the problem is dead but the arrangement persists, the constraint is a zombie scaffold or piton. Here the metrics show sustained extraction (epsilon 0.65) and rising theater (0.48), suggesting the coordination skeleton survives while the extraction function dominates â consistent with tangled_rope rather than pure coordination or pure extraction. The contested founding_problem_status flags the mandatrophy tension without resolving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    birth_varna_textual_authenticity,
    'Is the assignment of ritual authority by birth genuinely prescribed in the earliest Vedic textual strata, or is it a later sociological accretion?',
    'Philological analysis of Rigvedic and Brahmanic textual layers combined with archaeological evidence of non-Brahmin ritual specialists.',
    'If birth-varna is shown to be a later accretion, the hereditary reading''s textual foundation collapses and extraction is revealed as institutionalized retrofit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(birth_varna_textual_authenticity, empirical, 'Empirical uncertainty about scriptural origin of birth-based varna').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the persistence of the varna system maintained primarily by external structural coercion (temple control, economic boycott) or by internalized identity commitments among lower castes and women?',
    'Post-exit trajectory studies and survey data on caste salience after geographic or economic migration outside the ritual economy.',
    'If internalized, the constraint''s effective suppression exceeds structural measures and the target carries the constraint after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the vedic_dharmic_corpus kernel. How do the bhakti devotional and reformist egalitarian readings alter the beneficiary and victim structure?',
    'Comparative analysis of the sibling constraint stories instantiated from the same kernel.',
    'The kernel''s classification is underdetermined without disambiguating the reading; this reading computes as tangled_rope, but sibling readings may compute as rope or scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested kernel reading under-determination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vdh_hmr_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(vdh_hmr_tr_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(vdh_hmr_tr_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 40, 0.48).
narrative_ontology:measurement(vdh_hmr_tr_t60, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement(vdh_hmr_tr_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 80, 0.5).
narrative_ontology:measurement(vdh_hmr_tr_t100, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(vdh_hmr_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(vdh_hmr_be_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(vdh_hmr_be_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(vdh_hmr_be_t60, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement(vdh_hmr_be_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(vdh_hmr_be_t100, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vdh_hmr_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(vdh_hmr_su_t20, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(vdh_hmr_su_t40, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(vdh_hmr_su_t60, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement(vdh_hmr_su_t80, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 80, 0.58).
narrative_ontology:measurement(vdh_hmr_su_t100, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% This constraint is the hereditary-monopoly reading of the vedic_dharmic_corpus kernel, distinct from the bhakti devotional and reformist egalitarian readings which instantiate structurally different constraints from the same textual kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
