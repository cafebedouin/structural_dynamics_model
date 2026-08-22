% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_corpus_social_prescription__orthodox_varna_reading, []).

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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Orthodox Varna Hierarchy as Divine Cosmic Order
 *   domain: religious/social/hermeneutic
 *
 * SUMMARY:
 *   This constraint instantiates the orthodox reading of the Vedic corpus in
 *   which Vedic hymns and Dharmashastra texts are interpreted as literally
 *   prescribing a four-fold varna hierarchy plus Dalit exclusion as divinely
 *   mandated cosmic order (rta). Birth determines ritual entitlement,
 *   occupational specialization, and marital boundaries; the Brahminical
 *   interpretive community holds authority to adjudicate dharma, while Shudra
 *   and Dalit communities bear the extractive load of labor, deference, and
 *   exclusion. The constraint is a high-epsilon snare: the coordination story
 *   (interdependent social harmony through complementary roles) serves as
 *   theological cover for asymmetric extraction. This story is ONE READING of
 *   the kernel 'vedic_corpus_social_prescription'; sibling readings
 *   (reformist spiritual, colonial orientalist) are separate constraints.
 *
 * KEY AGENTS:
 *   - brahmin_caste: Primary agenda-setter and beneficiary (institutional/arbitrage) â holds interpretive authority and collects ritual and material flows.
 *   - shudra_caste: Primary target (powerless/trapped) â performs labor under occupational and marital closure.
 *   - dalit_communities: Primary target (powerless/trapped) â excluded from varna order, subject to extreme ritual and economic extraction.
 *   - intermediate_varna_groups: Secondary beneficiary (powerful/constrained) â benefits from boundary enforcement relative to lower castes while owing deference upward.
 *   - reformist_movements: Excluded voice (moderate/mobile) â rejects hierarchy but lacks orthodox authority.
 *   - critical_scholars: Analytical observer (institutional/analytical) â documents the structure without power to alter it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.92).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.9).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Orthodox Varna Hierarchy as Divine Cosmic Order").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious/social/hermeneutic").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, 'b4160030-9be4-4600-b938-966dba1300f8').
narrative_ontology:cs_kernel_codification('b4160030-9be4-4600-b938-966dba1300f8', fixed_text).
narrative_ontology:cs_authority_grounding('b4160030-9be4-4600-b938-966dba1300f8', lineage).
narrative_ontology:cs_interpretation_layer_present('b4160030-9be4-4600-b938-966dba1300f8').
narrative_ontology:cs_reading_relation('b4160030-9be4-4600-b938-966dba1300f8', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('b4160030-9be4-4600-b938-966dba1300f8', vedic_corpus_social_prescription__colonial_orientalist_reading, influences).
narrative_ontology:cs_axiom('b4160030-9be4-4600-b938-966dba1300f8', foundational, varna_dharma_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_dharma_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('b4160030-9be4-4600-b938-966dba1300f8', varna_dharma_divinely_ordained, theological).
narrative_ontology:cs_axiom('b4160030-9be4-4600-b938-966dba1300f8', foundational, birth_based_varna_inherent).
narrative_ontology:cs_axiom_status(birth_based_varna_inherent, holdable).
narrative_ontology:cs_axiom_grounding('b4160030-9be4-4600-b938-966dba1300f8', birth_based_varna_inherent, theological).
narrative_ontology:cs_reference_frame('b4160030-9be4-4600-b938-966dba1300f8', divinely_mandated_varna_order).
narrative_ontology:cs_drift_state('b4160030-9be4-4600-b938-966dba1300f8', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b4160030-9be4-4600-b938-966dba1300f8', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, intermediate_varna_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds hermeneutic authority over Vedic and Dharmashastra texts, adjudicating ritual entitlement and social boundaries. Collects ritual service, deference, and economic flows from subordinate groups. Could theoretically redefine orthodoxy or abandon the varna framework but is structurally incentivized to maintain it as the source of authority and material benefit.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, beneficiary).

% Performs labor and service functions deemed ritually inferior. Excluded from Vedic study and sacraments. Bound by occupational and marital restrictions that prevent upward mobility. Compliance enforced through social ostracism and pollution norms.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_caste, payer,
    powerless, generational, trapped, continental).

% Situated outside the four-fold varna order, performing stigmatized labor. Subject to extreme ritual exclusion and violence. Alternatives are structurally blocked by residential segregation, occupational monopoly, and social boycott.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, dalit_communities, payer,
    powerless, generational, trapped, continental).

% Kshatriya and Vaishya groups who benefit from varna boundary enforcement relative to Shudra and Dalit communities, receiving deference and economic advantage, while owing ritual and social deference to Brahmins. Their mobility is constrained by the same hierarchy they help enforce.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, intermediate_varna_groups, beneficiary,
    powerful, generational, constrained, continental).

% Bhakti, Buddhist, Dalit, and modern reformist movements that reject birth-based hierarchy and argue for spiritual equality or civic egalitarianism. Excluded from orthodox interpretive authority and ritual institutions; their presence is suppressed in orthodox pedagogy.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_movements, excluded,
    moderate, generational, mobile, national).

% Historians, anthropologists, and philologists who analyze the Vedic corpus and varna system as socially constructed and historically evolving. They document divergence between textual layers and orthodox interpretation but hold no authority to alter ritual practice.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, critical_scholars, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains ritually integrated social order by assigning fixed occupational, marital, and sacramental roles to birth groups, theoretically ensuring interdependence and cosmic harmony through complementary functions.
% TRANSFER_FUNCTION: Moves labor, material surplus, ritual deference, and social subordination from Shudra and Dalit communities to Brahminical and dominant varna groups; extracts compliance through occupational closure, endogamy enforcement, and ritual exclusion.
% ABSENT_VOICES: Reformist spiritual traditions, heterodox sects, Dalit theological voices, and subaltern interpreters who reject birth-based hierarchy are structurally excluded from orthodox hermeneutic authority.
% DISAPPEARANCE_RATIONALE: If the divine varna mandate disappeared overnight, occupational specialization would reorganize on market or contractual terms, marital boundaries would shift dramatically, ritual exclusivity would collapse, and the Brahminical interpretive monopoly would lose its primary legitimacy anchor; the social world would rearrange.
% FOUNDING_PROBLEM: Maintaining cosmic and social order (rita/dharma) in an agrarian society through ritually integrated but hierarchically ordered labor and priestly functions.
% FOUNDING_PROBLEM_CORROBORATION: Modern historians and anthropologists attest that the social-order problem has been superseded by civic and economic institutions; orthodox Brahminical authorities attest it remains live through divine mandate. No corroboration from outside the benefiting parties accepts the divine-mandate framing as currently necessary.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.92, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vedic_corpus_social_prescription__orthodox_varna_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.92) because occupational, marital, and ritual restrictions directly transfer labor value and social deference from Shudra/Dalit to Brahminical groups. Suppression is equally high (0.90) due to social ostracism, violence, pollution norms, and economic boycott that enforce compliance. Theater_ratio is moderately high (0.58) and rising: as legal abolition removes state enforcement, maintenance of the constraint becomes increasingly performative (ritual assertions of purity, caste association politics). Accessibility_collapse is high (0.80) because alternatives such as inter-marriage or occupational mobility are heavily taboo and structurally blocked. Resistance is moderate (0.55) owing to persistent historical and contemporary movements (Buddhist, Bhakti, Dalit, constitutional) that contest the hierarchy but have not fully dismantled it.
 *
 * PERSPECTIVAL GAP:
 *   The Brahminical agenda-setter seat experiences the constraint as sacred duty and cosmic maintenance; Shudra and Dalit payer seats experience it as enforced extraction with near-zero exit. Intermediate varna seats experience partial benefit offset by deference obligations. The engine will compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin caste is the declared beneficiary with institutional power and arbitrage-grade exit options, placing its directionality near the full-beneficiary end. Shudra and Dalit are declared victims with powerless/trapped exit, placing their directionality near the full-target end. Intermediate varna groups are beneficiaries relative to the lower castes but constrained in their own upward exit, sitting between the two poles. The effective extraction (chi) is therefore amplified for Shudra/Dalit and damped or inverted for Brahmins.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ritually integrated agrarian social order) has been substantially superseded by modern civic, legal, and economic institutions, yet the orthodox reading persists through theological inertia and active ritual performance. The mismatch between founding_problem_status (contested) and disappearance_verdict (world_rearranges) signals mandatrophy: the arrangement continues extracting after its original functional mandate has eroded. However, because concentrated beneficiaries (Brahmin caste) still capture substantial gains and because suppression remains high, the constraint has not degraded into a piton; it remains an active snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the varna prescription an intrinsic property of the Vedic textual kernel, or a product of the orthodox interpretive tradition that reads prescriptive intent into cosmological hymns?',
    'Philological stratification of the Vedic corpus and historical analysis of Dharmashastra commentaries to distinguish descriptive cosmology from prescriptive social law.',
    'If the prescriptive content is hermeneutic overlay, the constraint''s extraction is generated by the interpretive apparatus rather than the kernel, shifting the seat of agency from text to tradition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether varna prescription is textual or interpretive').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the persistence of varna hierarchy enforced primarily by external social sanctions or by internalized identity-fusion where subordinate groups accept the hierarchy as cosmically justified?',
    'Comparative analysis of varna compliance in contexts of weak external enforcement; study of post-conversion or diasporic communities to test whether suppression persists after structural barriers are removed.',
    'If internalized, effective extraction exceeds structural measures because the target carries the suppression beyond exit; the constraint operates partly as cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in varna hierarchy').

omega_variable(
    orthodox_to_colonial_influence,
    'To what extent does the modern orthodox reading''s institutional form depend on colonial-era codification of ''Hindu law'' rather than pre-colonial lineage transmission?',
    'Historical institutional analysis comparing pre-colonial decentralized caste enforcement with colonial legal codification and census categorization.',
    'High colonial dependence would indicate the authority_grounding is partly extraction/administrative rather than pure lineage, altering the commitment-system classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodox_to_colonial_influence, empirical, 'Colonial legacy in orthodox varna institutionalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(vedi_tr_t20, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(vedi_tr_t40, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(vedi_tr_t60, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(vedi_tr_t80, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 80, 0.5).
narrative_ontology:measurement(vedi_tr_t100, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(vedi_be_t20, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(vedi_be_t40, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 40, 0.83).
narrative_ontology:measurement(vedi_be_t60, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 60, 0.87).
narrative_ontology:measurement(vedi_be_t80, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 80, 0.9).
narrative_ontology:measurement(vedi_be_t100, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 100, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vedi_su_t20, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(vedi_su_t40, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(vedi_su_t60, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement(vedi_su_t80, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 80, 0.92).
narrative_ontology:measurement(vedi_su_t100, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the vedic_corpus_social_prescription kernel family. The natural-language concept 'Vedic social prescription' decomposes into structurally distinct claims: orthodox literal prescription (this file), reformist spiritual denial of prescriptive content, and colonial orientalist codification. Each has its own epsilon, stakeholders, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
