% ============================================================================
% CONSTRAINT STORY: hebrew_linguistic_life__marketplace_pidgin_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_linguistic_life__marketplace_pidgin_reading, []).

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
 *   constraint_id: hebrew_linguistic_life__marketplace_pidgin_reading
 *   human_readable: Marketplace Pidgin Reading of Hebrew Linguistic Vitality
 *   domain: sociolinguistic/religious/nationalist
 *
 * SUMMARY:
 *   This constraint instantiates the marketplace_pidgin_reading of the kernel
 *   hebrew_linguistic_life. The kernel asks what makes a language 'alive,'
 *   with three live readings: liturgical_preservation_reading (sacred
 *   transmission), marketplace_pidgin_reading (inter-communal practical
 *   function), and native_generational_reading (mother-tongue acquisition).
 *   This reading claims Hebrew was continuously alive in pre-1880 Jerusalem
 *   markets as a modified Medieval Hebrew pidgin, treating sacred function
 *   and native-speaker status as irrelevant to vitality. It is claimed as
 *   tangled_rope because it offers a genuine coordination
 *   functionâcross-linguistic classificationâwhile asymmetrically
 *   extracting authority from liturgical and natalist frameworks.
 *
 * KEY AGENTS:
 *   - Functionalist sociolinguists: agenda_setter (institutional/analytical/global) â enforces the definitional standard through peer review and disciplinary gatekeeping.
 *   - Continuity nationalists: beneficiary (institutional/constrained/national) â gains historiographical legitimacy for unbroken-continuity claims.
 *   - Liturgical rabbinic authorities: payer (institutional/identity_locked/global) â loses authoritative status over what counts as living Hebrew.
 *   - Native acquisition advocates: payer (organized/constrained/global) â loses policy influence as mother-tongue criterion is marginalized.
 *   - Marketplace minority communities: excluded (powerless/trapped/local) â historical participants erased from nationalist narrative.
 *   - Comparative linguists: observer (institutional/analytical/global) â external analytical seat evaluating selective application.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, 0.55).
domain_priors:suppression_score(hebrew_linguistic_life__marketplace_pidgin_reading, 0.45).
domain_priors:theater_ratio(hebrew_linguistic_life__marketplace_pidgin_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hebrew_linguistic_life__marketplace_pidgin_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_linguistic_life__marketplace_pidgin_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_linguistic_life__marketplace_pidgin_reading, "Marketplace Pidgin Reading of Hebrew Linguistic Vitality").
narrative_ontology:topic_domain(hebrew_linguistic_life__marketplace_pidgin_reading, "sociolinguistic/religious/nationalist").

domain_priors:requires_active_enforcement(hebrew_linguistic_life__marketplace_pidgin_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_linguistic_life__marketplace_pidgin_reading, '8fb52add-49fb-41e3-b0d7-a52536309126').
narrative_ontology:cs_kernel_codification('8fb52add-49fb-41e3-b0d7-a52536309126', implicit).
narrative_ontology:cs_authority_grounding('8fb52add-49fb-41e3-b0d7-a52536309126', expertise).
narrative_ontology:cs_interpretation_layer_present('8fb52add-49fb-41e3-b0d7-a52536309126').
narrative_ontology:cs_reading_relation('8fb52add-49fb-41e3-b0d7-a52536309126', hebrew_linguistic_life__liturgical_preservation_reading, coexists_with).
narrative_ontology:cs_reading_relation('8fb52add-49fb-41e3-b0d7-a52536309126', hebrew_linguistic_life__native_generational_reading, coexists_with).
narrative_ontology:cs_axiom('8fb52add-49fb-41e3-b0d7-a52536309126', foundational, intercommunal_function_defines_vitality).
narrative_ontology:cs_axiom_status(intercommunal_function_defines_vitality, holdable).
narrative_ontology:cs_axiom_grounding('8fb52add-49fb-41e3-b0d7-a52536309126', intercommunal_function_defines_vitality, conventional).
narrative_ontology:cs_axiom('8fb52add-49fb-41e3-b0d7-a52536309126', foundational, historical_continuity_through_adaptation).
narrative_ontology:cs_axiom_status(historical_continuity_through_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('8fb52add-49fb-41e3-b0d7-a52536309126', historical_continuity_through_adaptation, empirically_contingent).
narrative_ontology:cs_reference_frame('8fb52add-49fb-41e3-b0d7-a52536309126', marketplace_functionalism).
narrative_ontology:cs_drift_state('8fb52add-49fb-41e3-b0d7-a52536309126', contemporary_nationalist_hegemony, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8fb52add-49fb-41e3-b0d7-a52536309126', '').
narrative_ontology:cs_kernel_id(hebrew_linguistic_life__marketplace_pidgin_reading, hebrew_linguistic_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, functionalist_sociolinguists).
narrative_ontology:constraint_beneficiary(hebrew_linguistic_life__marketplace_pidgin_reading, continuity_nationalists).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, liturgical_rabbinic_authorities).
narrative_ontology:constraint_victim(hebrew_linguistic_life__marketplace_pidgin_reading, native_acquisition_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and enforce disciplinary frameworks that classify language vitality through practical inter-communal function. Their peer-review standards, conference agendas, and citation networks determine which historical evidence counts as proof of a living language, and which definitions are treated as theoretically sound.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, functionalist_sociolinguists, agenda_setter,
    institutional, generational, analytical, global).

% Draw on the marketplace-pidgin historiography to claim Hebrew never died in Palestine, supporting educational curricula and cultural-political narratives of unbroken Jewish presence. The reading gives their territorial and linguistic continuity claims an empirical anchor in pre-1880 Jerusalem markets.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, continuity_nationalists, beneficiary,
    institutional, generational, constrained, national).

% Bear the cost of having centuries of Hebrew transmission through prayer, legal study, and ritual reclassified as mechanical preservation rather than living linguistic practice. Their authority to adjudicate legitimate Hebrew usage is displaced by functionalist academic standards they do not control.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, liturgical_rabbinic_authorities, payer,
    institutional, civilizational, identity_locked, global).

% Promote mother-tongue acquisition as the defining criterion for language vitality. Their influence in language-policy debates is marginalized when functional-interaction standards replace acquisition-based standards in academic publishing and institutional guidelines.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, native_acquisition_advocates, payer,
    organized, biographical, constrained, global).

% Historically used Hebrew or Hebrew-Arabic contact varieties in Jerusalem trade. Their participatory role in the pidgin's development is erased in nationalist historiography, which claims the marketplace Hebrew as exclusively internal Jewish continuity rather than inter-communal practice.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, marketplace_minority_communities, excluded,
    powerless, biographical, trapped, local).

% Study language vitality across diverse global contexts. They observe that functional definitions have cross-linguistic analytical utility, but also note when those definitions are applied selectively to serve particular political projects rather than as consistent theoretical standards.
narrative_ontology:constraint_stakeholder(hebrew_linguistic_life__marketplace_pidgin_reading, comparative_linguists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_linguistic_life__marketplace_pidgin_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_linguistic_life__marketplace_pidgin_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an empirically observable standard for classifying language vitality that does not depend on contested notions of sacred status or mother-tongue acquisition, enabling cross-linguistic comparison and resolving historiographical disputes about whether Hebrew was continuously alive.
% TRANSFER_FUNCTION: Moves academic legitimacy and historiographical authority from liturgical-transmission and native-acquisition frameworks to functional-interaction frameworks; moves curricular and research priority toward continuity narratives and away from revivalist rupture narratives.
% ABSENT_VOICES: Palestinian and minority communities who participated in the historical marketplace pidgin are excluded from nationalist historiography; traditional liturgical communities whose transmission chains are reclassified as preservation rather than life are also structurally marginalized in the academic conversation.
% DISAPPEARANCE_RATIONALE: Functionalist sociolinguists argue the world would rearrange as classification standards collapsed and historiography retreated to intuition; liturgical authorities argue the world would remain unchanged because sacred transmission persists independent of academic definition; nationalists dispute whether continuity claims could survive without this empirical pillar.
% FOUNDING_PROBLEM: The need to determine whether Hebrew was a dead language awaiting modern revival or a living language undergoing continuous transformation, particularly to adjudicate competing nationalist, religious, and linguistic historiographies.
% FOUNDING_PROBLEM_CORROBORATION: Comparative linguists outside the Israeli nationalist frame acknowledge that functional-use criteria have cross-linguistic analytical utility, but note they are one definition among many; liturgical historians contest the empirical foundation, and no purely external corroboration exists that the specific historical claim is settled.
narrative_ontology:disappearance_verdict(hebrew_linguistic_life__marketplace_pidgin_reading, contested).
narrative_ontology:founding_problem_status(hebrew_linguistic_life__marketplace_pidgin_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_linguistic_life__marketplace_pidgin_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_linguistic_life__marketplace_pidgin_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_linguistic_life__marketplace_pidgin_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_linguistic_life__marketplace_pidgin_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate: the definition genuinely solves classification problems, but it also systematically transfers authority from liturgical and natalist seats to functionalist and nationalist ones. Suppression (0.45) is moderate: peer-review standards and curricular adoption marginalize alternative definitions without overt coercion. Theater_ratio (0.35) is moderate-low: there is real analytical work, but nationalist discourse increasingly uses the marketplace-pidgin claim performatively to ward off revivalist rupture narratives. Accessibility_collapse (0.60) reflects that once the functionalist framework is accepted, liturgical and natalist alternatives become academically untenable. Resistance (0.40) captures ongoing pushback from religious historians and generative linguists.
 *
 * PERSPECTIVAL GAP:
 *   From the functionalist agenda-setter seat, the constraint is genuine coordination: a cross-culturally valid standard that resolves parochial debates. From the liturgical-authority and native-advocate payer seats, the same structure operates as extraction: their centuries of transmission or theoretical frameworks are downgraded to secondary status. The engine computes this divergence from the structural data rather than resolving it editorially.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (functionalist_sociolinguists, continuity_nationalists) sit near the full-beneficiary end: the constraint subsidizes their authority and narratives. Victims (liturgical_rabbinic_authorities, native_acquisition_advocates) sit near the full-target end: the constraint extracts their historiographical legitimacy. The marketplace_minority_communities seat is excluded rather than directly targetedâits erasure is a side effect of the nationalist capture of the reading. Comparative_linguists are analytical and symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring both genuine coordination and asymmetric extraction. A pure rope reading would miss the systematic marginalization of liturgical and natalist definitions in funding, publishing, and curricula. A pure snare reading would miss the real cross-linguistic utility of functional vitality metrics. Tangled_rope captures that the same standard that classifies Somali market Arabic or Swahili trade registers also, in this specific historical application, reallocates authority toward secular nationalist historiography.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marketplace_pidgin_reading_committer,
    'This constraint is the marketplace_pidgin_reading of kernel hebrew_linguistic_life; how would the beneficiary structure change if the liturgical_preservation_reading or native_generational_reading were adopted instead?',
    'Comparative analysis of the three sibling constraints'' stakeholder surfaces and directionality derivations.',
    'Would shift beneficiaries from functionalist scholars and continuity nationalists to liturgical authorities or child-language acquisition researchers, respectively, reversing the extraction direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_pidgin_reading_committer, conceptual, 'Sibling reading substitution effect on classification').

omega_variable(
    historical_empirical_foundation,
    'Was the pre-1880 Jerusalem marketplace Hebrew a stable pidgin, transient code-switching, or a mixed register?',
    'Archival discovery of mercantile records and contemporary traveler accounts from 1700-1880 Jerusalem.',
    'If the empirical foundation is weak, the constraint''s coordination function is undermined and it slides toward pure identity-coordination or snare; if strong, the tangled_rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_empirical_foundation, empirical, 'Historical evidentiary basis for marketplace pidgin claim').

omega_variable(
    functionalism_as_cover,
    'Does the functional-interaction definition of language vitality genuinely serve cross-linguistic classification, or does it primarily legitimize a specific nationalist continuity narrative?',
    'Cross-corpus analysis testing whether functionalist sociolinguists apply the same marketplace-pidgin criterion consistently to other languages with similar histories, or reserve it for Hebrew.',
    'If reserved for Hebrew, the coordination function is cover story and the constraint is more extractive than measured; if consistently applied, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functionalism_as_cover, empirical, 'Whether functionalism is universally applied or Hebrew-specific').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_linguistic_life__marketplace_pidgin_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(hebr_tr_t10, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(hebr_tr_t20, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 20, 0.29).
narrative_ontology:measurement(hebr_tr_t30, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(hebr_tr_t40, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 40, 0.37).
narrative_ontology:measurement(hebr_tr_t50, hebrew_linguistic_life__marketplace_pidgin_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(hebr_be_t10, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(hebr_be_t20, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 20, 0.47).
narrative_ontology:measurement(hebr_be_t30, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(hebr_be_t40, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(hebr_be_t50, hebrew_linguistic_life__marketplace_pidgin_reading, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(hebr_su_t10, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 10, 0.39).
narrative_ontology:measurement(hebr_su_t20, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 20, 0.43).
narrative_ontology:measurement(hebr_su_t30, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 30, 0.47).
narrative_ontology:measurement(hebr_su_t40, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(hebr_su_t50, hebrew_linguistic_life__marketplace_pidgin_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_linguistic_life__marketplace_pidgin_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, liturgical_preservation_reading).
narrative_ontology:affects_constraint(hebrew_linguistic_life__marketplace_pidgin_reading, native_generational_reading).

% DUAL FORMULATION NOTE:
% The hebrew_linguistic_life kernel decomposes into three structurally distinct constraints because the definition of linguistic life has different epsilon profiles depending on whether the observable is sacred recitation, marketplace function, or native acquisition. Each reading carries a different beneficiary/victim structure and different empirical commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
