% ============================================================================
% CONSTRAINT STORY: vedic_corpus_social_prescription__orthodox_varna_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: vedic_corpus_social_prescription__orthodox_varna_reading
 *   human_readable: Orthodox Varna Hierarchy as Divinely Mandated Cosmic Order
 *   domain: religious/social/hermeneutic
 *
 * SUMMARY:
 *   This constraint instantiates the orthodox varna reading of the
 *   vedic_corpus_social_prescription kernel: the claim that Vedic texts
 *   literally and bindingly prescribe a four-fold hereditary hierarchy as
 *   divinely mandated cosmic order. In this reading, the Brahmin caste
 *   occupies the agenda-setting and beneficiary seat, monopolizing textual
 *   interpretation and ritual authority, while Shudra and Dalit communities
 *   occupy the payer seat, subject to occupational, marital, and ritual
 *   extraction. The reformist spiritual reading and colonial orientalist
 *   reading are structurally excluded sibling constraints. The claim/metric
 *   independence is maintained: the orthodox reading claims mountain-like
 *   divine necessity or rope-like social coordination, while the authored
 *   metrics describe a high-extraction snare.
 *
 * KEY AGENTS:
 *   - brahmin_caste: agenda-setter and beneficiary â monopolizes ritual/textual authority and receives labor/deference transfers
 *   - shudra_dalit_communities: payer â hereditary laboring groups subject to occupational, marital, and ritual extraction
 *   - reformist_spiritual_readers: excluded â denied voice in orthodox interpretive institutions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_corpus_social_prescription__orthodox_varna_reading, 0.86).
domain_priors:suppression_score(vedic_corpus_social_prescription__orthodox_varna_reading, 0.79).
domain_priors:theater_ratio(vedic_corpus_social_prescription__orthodox_varna_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(vedic_corpus_social_prescription__orthodox_varna_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_corpus_social_prescription__orthodox_varna_reading, snare).
narrative_ontology:human_readable(vedic_corpus_social_prescription__orthodox_varna_reading, "Orthodox Varna Hierarchy as Divinely Mandated Cosmic Order").
narrative_ontology:topic_domain(vedic_corpus_social_prescription__orthodox_varna_reading, "religious/social/hermeneutic").

domain_priors:requires_active_enforcement(vedic_corpus_social_prescription__orthodox_varna_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_corpus_social_prescription__orthodox_varna_reading, 'b88ffde5-32b7-4673-9327-3c5c2e29942f').
narrative_ontology:cs_kernel_codification('b88ffde5-32b7-4673-9327-3c5c2e29942f', fixed_text).
narrative_ontology:cs_authority_grounding('b88ffde5-32b7-4673-9327-3c5c2e29942f', lineage).
narrative_ontology:cs_interpretation_layer_present('b88ffde5-32b7-4673-9327-3c5c2e29942f').
narrative_ontology:cs_reading_relation('b88ffde5-32b7-4673-9327-3c5c2e29942f', vedic_corpus_social_prescription__reformist_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('b88ffde5-32b7-4673-9327-3c5c2e29942f', vedic_corpus_social_prescription__colonial_orientalist_reading, coexists_with).
narrative_ontology:cs_axiom('b88ffde5-32b7-4673-9327-3c5c2e29942f', foundational, varna_as_literal_divine_prescription).
narrative_ontology:cs_axiom_status(varna_as_literal_divine_prescription, holdable).
narrative_ontology:cs_axiom_grounding('b88ffde5-32b7-4673-9327-3c5c2e29942f', varna_as_literal_divine_prescription, theological).
narrative_ontology:cs_axiom('b88ffde5-32b7-4673-9327-3c5c2e29942f', foundational, brahmin_exclusive_ritual_authority).
narrative_ontology:cs_axiom_status(brahmin_exclusive_ritual_authority, holdable).
narrative_ontology:cs_axiom_grounding('b88ffde5-32b7-4673-9327-3c5c2e29942f', brahmin_exclusive_ritual_authority, theological).
narrative_ontology:cs_reference_frame('b88ffde5-32b7-4673-9327-3c5c2e29942f', vedic_cosmic_order).
narrative_ontology:cs_drift_state('b88ffde5-32b7-4673-9327-3c5c2e29942f', post_independence_modernity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b88ffde5-32b7-4673-9327-3c5c2e29942f', '').
narrative_ontology:cs_kernel_id(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_corpus_social_prescription).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:constraint_victim(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_dalit_communities).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, ritual_purity_doctrine).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, karmic_rebirth_justification).
narrative_ontology:constraint_vindicates(vedic_corpus_social_prescription__orthodox_varna_reading, vedic_apauruseyatva).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary ritual and textual elite claiming sole authority to pronounce, interpret, and enforce Vedic prescription. Monopolizes access to sacred knowledge, performs rituals that legitimate social hierarchy, and receives material support, labor service, and deference from subordinate groups through jajmani, temple, and agrarian economies.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, agenda_setter,
    powerful, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste, beneficiary).

% Hereditary laboring and service communities excluded from Vedic study, sacrificial participation, and intermarriage with upper orders. Bound to serve upper-caste households and temples; their labor value, reproductive autonomy, and spatial mobility are extracted through ritual and occupational enclosure. Exit is structurally barred because caste identity is fused with social existence and economic survival.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, shudra_dalit_communities, payer,
    powerless, generational, identity_locked, national).

% Hermeneuts and religious reformers who deny that Vedic texts contain prescriptive social hierarchy, reading them instead as symbolic cosmology or interior spiritual discipline. Their interpretive framework is systematically excluded from orthodox ritual pedagogy, canonical authority, and temple institutions, though it circulates in counter-publics and reform movements.
narrative_ontology:constraint_stakeholder(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_spiritual_readers, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_corpus_social_prescription__orthodox_varna_reading, brahmin_caste).
narrative_ontology:fixing_cost_class(vedic_corpus_social_prescription__orthodox_varna_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the assignment of ritual status, occupational enclosure, and endogamous boundaries across a population by hereditary station, purportedly to mirror divine cosmic order and secure a dedicated priestly and laboring class.
% TRANSFER_FUNCTION: Moves labor value, ritual deference, material surplus, and reproductive autonomy from Shudra and Dalit communities to the Brahmin caste and associated elite orders, enforced through occupational monopoly, endogamy rules, and spatial and ritual segregation.
% ABSENT_VOICES: Reformist spiritual interpreters who deny the prescriptive social content of Vedic texts, and subaltern ritual practitioners who would claim autonomous religious authority, are excluded from the interpretive institutions controlled by Brahminical pedagogy.
% DISAPPEARANCE_RATIONALE: If the divinely mandated Varna hierarchy vanished overnight, marriage markets would reconstitute across previous boundaries, occupational monopolies would collapse, temple and jajmani economies would lose their ritual legitimation, and the Brahmin caste would lose its exclusive claim to sacred authority and the material flows tied to it.
% FOUNDING_PROBLEM: The integration of diverse occupational and kin groups into a sacrificial society requiring specialized ritual labor, the prevention of inter-group status competition, and the maintenance of a hereditary priesthood for Vedic sacrifice and textual transmission.
% FOUNDING_PROBLEM_CORROBORATION: Colonial ethnographic and post-colonial historical scholarship from outside the Brahminical beneficiary set attest that the founding problem of agrarian labor coordination and ritual monopoly has been substantially transformed by modern political economy; reformist movements from within the tradition also contest the eternal-necessity claim. Orthodox ritual authorities alone assert the problem is still live.
narrative_ontology:disappearance_verdict(vedic_corpus_social_prescription__orthodox_varna_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_corpus_social_prescription__orthodox_varna_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vedic_corpus_social_prescription__orthodox_varna_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_corpus_social_prescription__orthodox_varna_reading, 0.86, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.86) because labor value, reproductive autonomy, and ritual deference are transferred through hereditary enclosure with near-zero marginal return to the payer. Suppression is high (0.79) because persistence depends on actively excluding alternative social arrangements, intermarriage, and ritual participation. Accessibility collapse is very high (0.88): birth determines station and alternatives are almost entirely closed. Resistance is moderate (0.52) because subaltern movements and reformist traditions have historically contested the arrangement, though they are fragmented and met with suppression. Theater ratio is moderate-high (0.48) and rose over the interval: as modernity and legal abolition challenged the constraint, maintenance became increasingly performative (public assertions of tradition, political identity mobilization) even as material extraction adapted rather than vanished.
 *
 * PERSPECTIVAL GAP:
 *   The Brahmin seat perceives the arrangement as sacred trusteeship and cosmic necessity; the Shudra/Dalit seat perceives enforced extraction and identity-lock. The reformist seat, excluded from orthodox institutions, perceives a misreading of metaphor as mandate. The engine computes this divergence from structural data rather than adjudicating which perception is true.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahmin caste is the structural beneficiary and agenda-setter (low d, subsidized by the constraint), with arbitrage-grade exit options that allow spatial and institutional mobility without loss of status. Shudra and Dalit communities are the structural targets (high d, amplified extraction), with identity-locked exit options that fuse social existence to caste position. The reformist readers are excluded from the constraint's operation and sit outside the directionality derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   The orthodox reading presents the constraint as either a mountain (divine law) or a rope (natural social coordination). The classification as snare is prevented from mislabeling by the explicit victim declaration (shudra_dalit_communities), the active enforcement requirement, the absence of a sunset clause, and the high extractiveness metrics. Without these structural declarations, the divine-necessity framing could obscure the extraction; with them, the divergence between claim and structure becomes the signal the engine measures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Does the orthodox varna reading represent an unbroken textual tradition or a retroactive Brahminical codification that hardened fluid social identities into rigid hierarchy?',
    'Historical philology and archaeology tracing the evolution of Varna terminology from the Rigvedic Purusha Sukta through Dharmashastra codification to medieval commentators.',
    'If the rigid hierarchy is a later codification, the constraint''s claim to mountain-like divine naturality collapses, and its classification as snare strengthens; if literally present in the earliest stratum, the extraction claim requires a more nuanced tangled_rope or mountain reading depending on whether enforcement is structural or supernatural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, empirical, 'Historical origins of varna prescription in Vedic corpus').

omega_variable(
    extraction_material_vs_symbolic,
    'Is the primary extraction from Shudra/Dalit communities material (surplus labor, economic transfers) or symbolic (status purity, recognition), and does the balance vary across time?',
    'Economic history of jajmani, temple, and agrarian systems combined with ethnography of contemporary caste-based labor markets.',
    'Material dominance would cement snare classification; symbolic dominance with minimal material transfer could push toward identity_coordination or piton if enforcement is largely performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_material_vs_symbolic, conceptual, 'Material versus symbolic extraction in varna hierarchy').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression maintained by structural coercion (state law, economic enclosure, physical violence) or by internalized identity-lock (self-policing of purity boundaries, belief in karmic justification)?',
    'Comparative analysis of caste persistence in jurisdictions with strong anti-discrimination law versus weak enforcement; post-exit trajectory of converts and migrants.',
    'If suppression is primarily internalized, effective extraction exceeds structural measures and the constraint operates as deep identity capture; if structural, it is a conventional enforcement snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    sibling_reading_influence,
    'How would adopting the reformist spiritual reading or the colonial orientalist reading change the structural classification of this constraint?',
    'Side-by-side constraint stories for each kernel reading; this story isolates the orthodox reading only.',
    'The reformist reading would likely classify as rope or mountain (spiritual metaphor with no victims); the colonial reading as tangled_rope (coordination of governance plus extraction of ethnographic knowledge); the orthodox reading alone yields a snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_influence, conceptual, 'Structural delta across kernel sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_corpus_social_prescription__orthodox_varna_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orthodox_varna_tr_t0, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(orthodox_varna_tr_t15, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(orthodox_varna_tr_t30, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(orthodox_varna_tr_t50, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement(orthodox_varna_tr_t75, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 75, 0.55).
narrative_ontology:measurement(orthodox_varna_tr_t100, vedic_corpus_social_prescription__orthodox_varna_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(orthodox_varna_be_t0, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(orthodox_varna_be_t15, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 15, 0.71).
narrative_ontology:measurement(orthodox_varna_be_t30, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(orthodox_varna_be_t50, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 50, 0.84).
narrative_ontology:measurement(orthodox_varna_be_t75, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 75, 0.81).
narrative_ontology:measurement(orthodox_varna_be_t100, vedic_corpus_social_prescription__orthodox_varna_reading, base_extractiveness, 100, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(orthodox_varna_su_t0, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(orthodox_varna_su_t15, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(orthodox_varna_su_t30, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(orthodox_varna_su_t50, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 50, 0.88).
narrative_ontology:measurement(orthodox_varna_su_t75, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(orthodox_varna_su_t100, vedic_corpus_social_prescription__orthodox_varna_reading, suppression_requirement, 100, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_corpus_social_prescription__orthodox_varna_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, reformist_spiritual_reading).
narrative_ontology:affects_constraint(vedic_corpus_social_prescription__orthodox_varna_reading, colonial_orientalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vedic_corpus_social_prescription kernel. The orthodox varna reading instantiates a high-epsilon snare with Brahmin beneficiaries and Shudra/Dalit victims. Sibling readings (reformist spiritual, colonial orientalist) instantiate structurally distinct constraints with different epsilon, beneficiary, and victim profiles. The kernel itself is the stabilized Vedic text; the readings produce divergent constraints through interpretive framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
