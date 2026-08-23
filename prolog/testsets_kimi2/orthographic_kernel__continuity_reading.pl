% ============================================================================
% CONSTRAINT STORY: orthographic_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_kernel__continuity_reading, []).

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
 *   constraint_id: orthographic_kernel__continuity_reading
 *   human_readable: Arabic Script Continuity in Ottoman and Islamic Textual Tradition
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the continuity_reading of the
 *   orthographic_kernel: the claim that Arabic script preserves Ottoman
 *   cultural continuity and Islamic textual tradition. In the late Ottoman
 *   context, the Arabic script functions as a commitment-system kernel
 *   defended by religious and traditional authorities. It coordinates genuine
 *   transnational Islamic textual access while extracting asymmetrically from
 *   the Ottoman literate class, whose professional identity and economic
 *   livelihood are locked to a writing system progressively mismatched with
 *   Turkish phonology and modern administrative needs. State modernizers
 *   experience the constraint as a blocked reform path. This reading is one
 *   of three sibling constraints: modernization_reading and rupture_reading
 *   instantiate structurally distinct claims from the same orthographic
 *   debate.
 *
 * KEY AGENTS:
 *   - Ottoman literate class: Primary target (moderate power / identity_locked exit) â bears the cognitive and economic costs of script maintenance.
 *   - Religious establishment: Primary beneficiary and agenda-setter (institutional power / identity_locked exit) â enforces continuity and collects interpretive authority.
 *   - State modernizers: Secondary target (institutional power / constrained exit) â blocked reformers who suffer lower effective extraction due to eventual rupture capacity.
 *   - European orientalists: Analytical observer (organized power / analytical exit) â external scholarly corroboration.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, 0.68).
domain_priors:suppression_score(orthographic_kernel__continuity_reading, 0.72).
domain_priors:theater_ratio(orthographic_kernel__continuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(orthographic_kernel__continuity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_kernel__continuity_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_kernel__continuity_reading, "Arabic Script Continuity in Ottoman and Islamic Textual Tradition").
narrative_ontology:topic_domain(orthographic_kernel__continuity_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(orthographic_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_kernel__continuity_reading, '7e1a62cf-b4df-4097-bc2a-e3b65b895ee9').
narrative_ontology:cs_kernel_codification('7e1a62cf-b4df-4097-bc2a-e3b65b895ee9', fixed_text).
narrative_ontology:cs_authority_grounding('7e1a62cf-b4df-4097-bc2a-e3b65b895ee9', lineage).
narrative_ontology:cs_interpretation_layer_present('7e1a62cf-b4df-4097-bc2a-e3b65b895ee9').
narrative_ontology:cs_reading_relation('7e1a62cf-b4df-4097-bc2a-e3b65b895ee9', orthographic_kernel__modernization_reading, coexists_with).
narrative_ontology:cs_reading_relation('7e1a62cf-b4df-4097-bc2a-e3b65b895ee9', orthographic_kernel__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('7e1a62cf-b4df-4097-bc2a-e3b65b895ee9', foundational, arabic_script_as_divine_textual_vehicle).
narrative_ontology:cs_axiom_status(arabic_script_as_divine_textual_vehicle, holdable).
narrative_ontology:cs_axiom_grounding('7e1a62cf-b4df-4097-bc2a-e3b65b895ee9', arabic_script_as_divine_textual_vehicle, theological).
narrative_ontology:cs_axiom('7e1a62cf-b4df-4097-bc2a-e3b65b895ee9', foundational, orthographic_continuity_as_civilizational_bond).
narrative_ontology:cs_axiom_status(orthographic_continuity_as_civilizational_bond, holdable).
narrative_ontology:cs_axiom_grounding('7e1a62cf-b4df-4097-bc2a-e3b65b895ee9', orthographic_continuity_as_civilizational_bond, conventional).
narrative_ontology:cs_reference_frame('7e1a62cf-b4df-4097-bc2a-e3b65b895ee9', ottoman_islamic_textual_continuity).
narrative_ontology:cs_drift_state('7e1a62cf-b4df-4097-bc2a-e3b65b895ee9', late_ottoman_modernization_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7e1a62cf-b4df-4097-bc2a-e3b65b895ee9', '').
narrative_ontology:cs_kernel_id(orthographic_kernel__continuity_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_kernel__continuity_reading, religious_establishment).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, ottoman_literate_class).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(orthographic_kernel__continuity_reading, state_modernizers).
narrative_ontology:constraint_vindicates(orthographic_kernel__continuity_reading, islamic_textual_unity).
narrative_ontology:constraint_vindicates(orthographic_kernel__continuity_reading, ottoman_dynastic_legitimacy_through_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their professional identity, social status, and economic livelihood depend on mastery of Arabic script for Turkish, Persian, and religious texts. They must invest years acquiring competence in a non-phonetic writing system poorly suited to Turkish phonology and modern technical vocabulary, which progressively devalues their cultural capital as administrative and military modernization favors new skills. Exit means abandoning the script and thus their self-concept as guardians of imperial and sacred literacy.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, ottoman_literate_class, payer,
    moderate, biographical, identity_locked, national).

% Controls the educational and juridical institutions that train scribes and interpreters, enforcing the norm that Arabic script is the necessary vehicle of Islamic textual authenticity and Ottoman legal continuity. They administer examinations, certify competence, and issue rulings that stigmatize script deviation. Their institutional authority and social role as necessary intermediaries between the laity and sacred text depend on the script's persistence.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, religious_establishment, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(orthographic_kernel__continuity_reading, religious_establishment, beneficiary).

% Reform-oriented bureaucrats, military officers, and educators who encounter the Arabic script as a structural bottleneck for mass literacy, standardized military-technical terminology, and centralized record-keeping. Their reform memoranda and curricular proposals are blocked by the script's institutional embeddedness and the political cost of confronting the religious establishment. They possess state power but face high political friction in changing the script, forcing them to route around the constraint through parallel institutions or wait for a rupture moment.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, state_modernizers, payer,
    institutional, biographical, constrained, national).

% External scholars and colonial administrators who study the Ottoman script regime from outside, documenting its role in preserving Islamic textual unity and noting its friction with modernizing reforms. They neither pay into nor collect from the constraint, but their published analyses are sometimes cited by both traditionalists and modernizers as corroborating evidence.
narrative_ontology:constraint_stakeholder(orthographic_kernel__continuity_reading, european_orientalist_observers, observer,
    organized, biographical, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_kernel__continuity_reading, religious_establishment).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a unified transnational Islamic textual community by ensuring that religious, legal, and scholarly documents remain mutually legible across generations and geography, preventing interpretive fragmentation of the Quran, hadith corpora, and Ottoman administrative precedent.
% TRANSFER_FUNCTION: Moves cognitive labor costs onto the literate class, who must master a morphologically complex non-phonetic script; concentrates interpretive authority in the religious scholarly establishment who control educational certification; and transfers the opportunity costs of blocked administrative and military modernization onto state reformers.
% ABSENT_VOICES: Anatolian peasant majorities and women are excluded from formal literacy channels and thus from the script debate; non-Muslim Ottoman subjects using Armenian, Greek, or Hebrew scripts are structurally outside the continuity framework; phonetic-reform nationalists are marginalized within traditional educational and juridical institutions.
% DISAPPEARANCE_RATIONALE: If Arabic script vanished overnight, the Ottoman legal and educational apparatus would lose its documentary basis, the literate class would face immediate devaluation of their cultural capital, religious courts would lose their textual continuity, and the empire's link to the broader Islamic scholarly world would sever. State modernizers would gain sudden capacity for phonetic reform and mass literacy campaigns.
% FOUNDING_PROBLEM: The need to preserve the coherence and authority of Islamic divine revelation and Ottoman dynastic law across a multi-ethnic, multi-lingual empire, ensuring that religious scholars and state scribes could communicate through a shared sacred script anchored in the language of the Quran.
% FOUNDING_PROBLEM_CORROBORATION: European Orientalist scholars attested from outside the benefiting religious establishment to the script's role in Islamic civilizational unity. Late Ottoman modernizing bureaucrats and military reformers attested from outside the traditionalist beneficiary set that the founding problem had been superseded by print technology and was now functioning as a barrier to state capacity. No neutral intra-Ottoman party exists; corroboration is split along the reform-tradition axis.
narrative_ontology:disappearance_verdict(orthographic_kernel__continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_kernel__continuity_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_kernel__continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(orthographic_kernel__continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(orthographic_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) is substantial because the script's mismatch with Turkish phonology and modern technical vocabulary imposes heavy cognitive costs on the literate class while blocking state modernization. Suppression (0.72) is high because the constraint persists through active enforcement by religious educational institutions and social stigmatization of script deviation, not through participant preference. Theater ratio (0.45) rises over the interval: initial defense of the script was functionally integrated with genuine textual transmission, but under modernization pressure an increasing share of enforcement activity became performative defense of tradition against reform. Accessibility collapse (0.78) is high because once an agent is socialized into the Arabic-script literate tradition, alternatives like Latin-script Turkish are coded as heretical or treasonous, making exit culturally inaccessible. Resistance (0.62) reflects sustained modernizer opposition through memoranda, parallel institutions, and eventually state-level rupture.
 *
 * PERSPECTIVAL GAP:
 *   The literate class and the religious establishment should compute to different constraint types from the same structural data. The literate class experiences high effective extraction: they are declared victims with identity-locked exit at moderate power, pushing directionality toward the full-target end. The religious establishment experiences low or negative effective extraction: they are declared beneficiaries with institutional power, pushing directionality toward the beneficiary end. State modernizers, though blocked, are not declared victims in base_properties; their directionality defaults toward moderate values, producing lower effective extraction than the literate class. This three-way divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declaration maps to the religious establishment, who collect preserved gatekeeping authority and institutional centrality. Victim declaration maps to the Ottoman literate class, who pay through devalued cultural capital and declining administrative relevance. State modernizers are structurally situated as payers in the stakeholder layer but are omitted from the base_properties victims array because their institutional power and eventual rupture capacity mean they do not absorb extraction in the same way as the identity-locked literate class; the engine will derive their directionality from canonical fallback rather than victim amplification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both genuine coordination and asymmetric extraction for tangled_rope certification. The coordination function is real: the Arabic script did preserve Quranic textual unity and Ottoman legal coherence across a multi-ethnic empire. The extraction is also real: the same structure concentrated interpretive authority and imposed heavy costs on those locked into the script. Neither a pure coordination (rope) reading nor a pure extraction (snare) reading would capture the dual structure. The rising theater ratio and contested founding problem status suggest mandatrophy pressure â the coordination story is partially decaying into performance â but the genuine coordination function has not fully atrophied, ruling out piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the continuity_reading of orthographic_kernel. How would the classification change if the modernization_reading (Latin script enables technological modernization) or rupture_reading (script change as deliberate cultural rupture) were adopted instead?',
    'Comparison across the three sibling constraint files in the orthographic_kernel family, examining shifts in beneficiary-victim structure, epsilon values, and directionality profiles.',
    'The continuity reading extracts from the literate class and blocks state reformers. A modernization reading would likely reverse the victim set, extracting from traditional religious authorities and identity-locked scribes through forced adaptation. A rupture reading would classify the state as agenda-setter with high extraction from cultural memory itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Sibling reading structural delta for orthographic_kernel').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination function of Islamic textual continuity be preserved while decoupling the asymmetric extraction from the Ottoman literate class, or are the coordination and extraction structurally inseparable?',
    'Historical counterfactual analysis: could a dual-script or gradual transition period have preserved Quranic and legal textual access while lowering the cognitive burden on administrative scribes?',
    'If separable, the constraint demotes toward rope; if inseparable, it remains tangled_rope or shifts toward snare as the coordination story decays into pure gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether Islamic textual coordination is separable from literate-class extraction').

omega_variable(
    agenda_setter_capture_or_trap,
    'Does the religious establishment enforce script continuity as a capturing beneficiary accruing institutional rents, or are they equally identity-locked to the script without meaningful net gain?',
    'Comparative analysis of ulema economic and social mobility versus non-religious scribes during late Ottoman modernization pressure.',
    'If the agenda_setter is captured and accrues gains, gain_flow is concentrated and the constraint reads as tangled_rope. If the agenda_setter is equally trapped without capture, gain_flow becomes diffuse and the constraint may read as piton or degraded inertial structure rather than actively extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agenda_setter_capture_or_trap, empirical, 'Whether the enforcing religious authority captures extraction or is itself trapped').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_kernel__continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orthographic_continuity_tr_t0, orthographic_kernel__continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(orthographic_continuity_tr_t10, orthographic_kernel__continuity_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(orthographic_continuity_tr_t20, orthographic_kernel__continuity_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(orthographic_continuity_tr_t30, orthographic_kernel__continuity_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(orthographic_continuity_tr_t40, orthographic_kernel__continuity_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(orthographic_continuity_tr_t50, orthographic_kernel__continuity_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(orthographic_continuity_be_t0, orthographic_kernel__continuity_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(orthographic_continuity_be_t10, orthographic_kernel__continuity_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(orthographic_continuity_be_t20, orthographic_kernel__continuity_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(orthographic_continuity_be_t30, orthographic_kernel__continuity_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement(orthographic_continuity_be_t40, orthographic_kernel__continuity_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(orthographic_continuity_be_t50, orthographic_kernel__continuity_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(orthographic_continuity_su_t0, orthographic_kernel__continuity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(orthographic_continuity_su_t10, orthographic_kernel__continuity_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(orthographic_continuity_su_t20, orthographic_kernel__continuity_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(orthographic_continuity_su_t30, orthographic_kernel__continuity_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(orthographic_continuity_su_t40, orthographic_kernel__continuity_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(orthographic_continuity_su_t50, orthographic_kernel__continuity_reading, suppression_requirement, 50, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_kernel__continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__modernization_reading).
narrative_ontology:affects_constraint(orthographic_kernel__continuity_reading, orthographic_kernel__rupture_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the orthographic_kernel family. The natural-language label 'Ottoman/Turkish script question' decomposes into three structurally distinct constraints (continuity, modernization, rupture readings) with different epsilon values, beneficiary/victim structures, and claimed types. The epsilon-invariance principle requires separate stories because measuring via cultural continuity yields a different epsilon than measuring via modernization friction or nation-building rupture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
