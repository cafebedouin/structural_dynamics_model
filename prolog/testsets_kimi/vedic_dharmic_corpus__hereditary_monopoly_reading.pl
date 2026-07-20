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
 *   constraint_id: vedic_dharmic_corpus__hereditary_monopoly_reading
 *   human_readable: Hereditary Brahminical Monopoly on Ritual and Interpretive Authority
 *   domain: religious/social_stratification
 *
 * SUMMARY:
 *   This constraint story instantiates the hereditary_monopoly_reading of the
 *   vedic_dharmic_corpus kernel. It models the claim that ritual and
 *   interpretive authority in orthodox Hinduism derive exclusively from birth
 *   into Brahmin lineage, with varna hierarchy understood as divinely
 *   ordained and textually prescribed in the Vedic corpus. The constraint
 *   operates through temple control, ritual economy, and social enforcement
 *   of caste boundaries. It is claimed as tangled_rope because a genuine
 *   coordination functionâstandardized ritual performance and textual
 *   transmissionâcoexists with asymmetric extraction from lower castes and
 *   women. The high theater_ratio reflects increasing performative
 *   maintenance of divine-ordination claims under modernity, while
 *   base_extractiveness remains elevated due to the continued economic and
 *   social capture of temple institutions.
 *
 * KEY AGENTS:
 *   - Brahmin priestly class: Primary beneficiary and agenda-setter (institutional/mobile) â controls ritual economy and textual interpretation
 *   - Shudra and Dalit communities: Primary target (powerless/identity_locked) â pay ritual fees, excluded from authority
 *   - Women in Hindu communities: Secondary target (powerless/identity_locked) â excluded from study and priesthood
 *   - Bhakti movement leaders: Excluded challenger (organized/constrained) â offers bypass, denied temple legitimacy
 *   - Modern Indian state: Analytical observer (institutional/analytical) â constitutionally opposed, enforcement inconsistent
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
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__hereditary_monopoly_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__hereditary_monopoly_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__hereditary_monopoly_reading, "Hereditary Brahminical Monopoly on Ritual and Interpretive Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__hereditary_monopoly_reading, "religious/social_stratification").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__hereditary_monopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__hereditary_monopoly_reading, '1846bf30-59d9-4d79-be30-132294b46243').
narrative_ontology:cs_kernel_codification('1846bf30-59d9-4d79-be30-132294b46243', fixed_text).
narrative_ontology:cs_authority_grounding('1846bf30-59d9-4d79-be30-132294b46243', lineage).
narrative_ontology:cs_interpretation_layer_present('1846bf30-59d9-4d79-be30-132294b46243').
narrative_ontology:cs_reading_relation('1846bf30-59d9-4d79-be30-132294b46243', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_reading_relation('1846bf30-59d9-4d79-be30-132294b46243', vedic_dharmic_corpus__reformist_egalitarian_reading, forecloses).
narrative_ontology:cs_axiom('1846bf30-59d9-4d79-be30-132294b46243', foundational, ritual_authority_by_birth).
narrative_ontology:cs_axiom_status(ritual_authority_by_birth, holdable).
narrative_ontology:cs_axiom_grounding('1846bf30-59d9-4d79-be30-132294b46243', ritual_authority_by_birth, theological).
narrative_ontology:cs_axiom('1846bf30-59d9-4d79-be30-132294b46243', foundational, varna_divinely_ordained).
narrative_ontology:cs_axiom_status(varna_divinely_ordained, holdable).
narrative_ontology:cs_axiom_grounding('1846bf30-59d9-4d79-be30-132294b46243', varna_divinely_ordained, theological).
narrative_ontology:cs_reference_frame('1846bf30-59d9-4d79-be30-132294b46243', vedic_lineage_supremacy).
narrative_ontology:cs_drift_state('1846bf30-59d9-4d79-be30-132294b46243', post_independence_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1846bf30-59d9-4d79-be30-132294b46243', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__hereditary_monopoly_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_dalit_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__hereditary_monopoly_reading, women_in_hinduhouseholds).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__hereditary_monopoly_reading, varna_dharma_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls temple administration, Vedic ritual performance, and textual interpretation through birth-determined lineage; collects daksina, temple fees, and land-based support; maintains exclusive access to sacred knowledge and rites of passage; defines who may enter temples and perform which rituals.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class, beneficiary).

% Must employ Brahmin priests for life-cycle rituals and temple ceremonies; excluded from Vedic study and priestly ordination; pay ritual fees without recourse to alternative authority structures; caste status is ascribed at birth and enforced through social and religious boundaries.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, shudra_dalit_communities, payer,
    powerless, generational, identity_locked, continental).

% Excluded from Vedic study, ritual authority, and priestly roles regardless of personal capability or devotion; must access scripture and divine mediation through male Brahmin intermediaries; bear the costs of ritual dependence while denied the status and economic benefits of interpretive authority.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, women_in_hinduhouseholds, payer,
    powerless, generational, identity_locked, continental).

% Promote direct devotional access to the divine that bypasses Brahminical ritual requirements; denied legitimacy and material support from the temple ritual economy; treated as spiritually inferior or dangerous by orthodox hereditary authorities; operate outside the scriptural and institutional control of the Brahmin class.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_movement_leaders, excluded,
    organized, biographical, constrained, national).

% Constitutionally committed to equality and prohibition of caste discrimination; intermittently intervenes in temple entry and priestly appointments through legislation and court orders; enforcement is inconsistent due to political sensitivity and the autonomy claimed by religious institutions.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__hereditary_monopoly_reading, modern_indian_state, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__hereditary_monopoly_reading, brahmin_priestly_class).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__hereditary_monopoly_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates standardized Vedic ritual performance, life-cycle sacraments, and textual interpretation across dispersed Hindu communities by assigning exclusive authority to a hereditary priestly class, creating a unified but hierarchical religious framework.
% TRANSFER_FUNCTION: Moves material wealth in the form of ritual fees and temple endowments from lower-caste laity and women to the Brahmin priestly class; transfers spiritual authority, interpretive control, and social prestige from the broader community to a birth-determined lineage.
% ABSENT_VOICES: Lower-caste religious specialists who perform parallel rituals; women theologians and priests; Dalit spiritual leaders; devotional reformers who argue for meritocratic or egalitarian authority â all structurally excluded from the temple economy and textual institutions.
% DISAPPEARANCE_RATIONALE: If hereditary ritual authority disappeared, temple economies would shift to meritocratic or devotional recruitment, lower-caste communities and women would enter priestly and interpretive roles, the Brahmin class would lose its exclusive economic base, and Hindu religious practice would reorganize around alternative authority structures such as bhakti or democratic congregation.
% FOUNDING_PROBLEM: Standardizing and preserving Vedic ritual and textual knowledge across diverse regional communities without centralized church structures, while maintaining social order through religious hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox Brahminical institutions and traditional dharmashastra authorities attest the founding problem persists. Independent historians, sociologists, and Dalit studies scholars from outside the beneficiary set attest that the standardization narrative served primarily to consolidate Brahminical land rights and social dominance; corroboration from non-beneficiaries supports the reading that the problem was retroactively constructed to justify hierarchy.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__hereditary_monopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__hereditary_monopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__hereditary_monopoly_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Base extractiveness (0.65) reflects the substantial transfer of wealth and authority from lower castes and women to the Brahmin class, moderated slightly by the real coordination benefit of ritual standardization. Suppression (0.72) is high because the constraint requires active enforcementâtemple entry denial, social ostracism, and economic control of the ritual sphereâto prevent lower-caste and devotional alternatives from displacing hereditary authority. Theater_ratio (0.48) captures the increasing proportion of activity devoted to performative maintenance of divine-ordination narratives as empirical and constitutional challenges mount; orthodox ritual becomes partly a demonstration of unbroken lineage rather than purely functional coordination. Accessibility_collapse (0.70) is high because once inside the orthodox framework, alternatives appear illegitimate or sinful; resistance (0.55) reflects sustained but partially suppressed challenges from bhakti movements, reformers, and the constitutional state.
 *
 * PERSPECTIVAL GAP:
 *   The Brahmin seat experiences the constraint as legitimate coordination and sacred dutyâlow directionality, subsidized by religious prestige and economic capture. The lower-caste and women seats experience high directionality as targets of extraction, with identity-locked exit amplifying effective extraction. The modern state seat computes as analytical with neutral directionality, observing the structural asymmetry but constrained by political costs from intervening. The engine will compute these divergences from the authored beneficiary/victim declarations and exit asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the Brahmin priestly class, which collects ritual fees, temple endowments, and social prestige through exclusive control of legitimate religious performance. Victim declarations map to Shudra/Dalit communities and women, who bear the costs of ritual dependence and exclusion from authority. The Bhakti movement is excluded rather than victimized by direct extraction, operating in a parallel devotional space. Directionality is structurally derived: Brahmin (beneficiary + mobile) â low d; lower castes/women (victim + identity_locked) â high d; Bhakti leaders (excluded + constrained) â moderate d near target side.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problemâstandardizing ritual across diverse communitiesâwas historically live, but the hereditary_monopoly solution has outlived the conditions that might have justified it. The constraint persists not because alternative coordination mechanisms (devotional, democratic, meritocratic) are impossible, but because the Brahmin class captures sufficient gains to maintain the structure and the political cost of reform is prohibitive. This prevents mislabeling as pure extraction (snare) because the coordination function is real and historically grounded, while the tangled_rope classification captures the asymmetric extraction layered onto that coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the persistence of hereditary ritual authority maintained primarily by institutional enforcement (temple control, economic discrimination, social boycott) or by internalized identity-lock where lower castes and women accept Brahminical supremacy as cosmically legitimate?',
    'Comparative study of post-conversion or post-migration communities: if hereditary authority beliefs persist after institutional enforcement is removed, the suppression is partially internalized; if they collapse immediately, it was primarily structural.',
    'If internalized, effective extraction exceeds the structural measure because the targets reproduce the constraint after exit; reclassification toward higher suppression and potential snare dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in caste hierarchy').

omega_variable(
    coordination_extraction_boundary,
    'Does the Brahmin class provide a genuine coordination benefitâstandardized ritual and textual preservationâthat would be lost without hereditary monopoly, or is the coordination achievable through non-hereditary structures?',
    'Comparative analysis of religious communities that have shifted to meritocratic or democratic priesthoods; assessment of whether ritual standardization and textual transmission degrade when authority is opened.',
    'If coordination is separable from hereditary authority, the constraint is extraction riding on a coordination cover story; if inseparable, part of the extraction is inherent coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether ritual coordination requires hereditary authority').

omega_variable(
    historical_origin_retroactive_justification,
    'Does the hereditary monopoly reading represent the authentic original textual prescription of the Vedic corpus, or is it a retroactive justification constructed by Brahminical interests to legitimize birth-based hierarchy?',
    'Historical philological analysis of Vedic textual strata combined with archaeological and epigraphic evidence of early social organization; tracing when explicit hereditary monopoly claims crystallize in commentarial literature.',
    'If retroactive, the constraint''s claimed type as tangled_rope overstates its coordination legitimacy and the kernel itself is a false summit; if authentic, the extraction is textually sanctioned but still extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_origin_retroactive_justification, empirical, 'Authentic textual origin versus retroactive legitimation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__hereditary_monopoly_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(vedi_tr_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(vedi_tr_t30, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(vedi_tr_t50, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(vedi_tr_t70, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 70, 0.52).
narrative_ontology:measurement(vedi_tr_t85, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 85, 0.55).
narrative_ontology:measurement(vedi_tr_t100, vedic_dharmic_corpus__hereditary_monopoly_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vedi_be_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(vedi_be_t30, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(vedi_be_t50, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement(vedi_be_t70, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 70, 0.68).
narrative_ontology:measurement(vedi_be_t85, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 85, 0.65).
narrative_ontology:measurement(vedi_be_t100, vedic_dharmic_corpus__hereditary_monopoly_reading, base_extractiveness, 100, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(vedi_su_t15, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(vedi_su_t30, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(vedi_su_t50, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(vedi_su_t70, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 70, 0.72).
narrative_ontology:measurement(vedi_su_t85, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 85, 0.68).
narrative_ontology:measurement(vedi_su_t100, vedic_dharmic_corpus__hereditary_monopoly_reading, suppression_requirement, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__hereditary_monopoly_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, bhakti_devotional_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__hereditary_monopoly_reading, reformist_egalitarian_reading).

% DUAL FORMULATION NOTE:
% The vedic_dharmic_corpus kernel admits multiple structurally distinct readings. The hereditary_monopoly_reading (this file) asserts birth-based ritual authority and divine varna ordination with epsilon ~0.65. The bhakti_devotional_reading (sibling) asserts devotional bypass of caste with lower extraction. The reformist_egalitarian_reading (sibling) asserts constitutional/rational supersession of hereditary authority. Each reading carries a distinct beneficiary/victim structure, epsilon, and classification. They form a constraint family linked by shared kernel but separate empirical and normative commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
