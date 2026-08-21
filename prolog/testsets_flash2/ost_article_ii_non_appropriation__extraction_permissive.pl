% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__extraction_permissive, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: Outer Space Treaty Article II: Non-Appropriation (Extraction-Permissive Reading)
 *   domain: international_space_law/commons_governance
 *
 * SUMMARY:
 *   This constraint represents the 'extraction-permissive' reading of Outer
 *   Space Treaty (OST) Article II, which states that outer space is not
 *   subject to national appropriation. This reading interprets the article as
 *   barring sovereign territorial claims but not private ownership of
 *   resources extracted from celestial bodies. This interpretation
 *   facilitates private commercial space resource utilization, primarily
 *   benefiting technologically advanced nations and their private companies,
 *   while potentially excluding non-spacefaring nations from the benefits of
 *   space resources. The constraint is claimed as a Tangled Rope because it
 *   provides a coordination function (clarity for private investment) but
 *   also enables asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.85).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.7).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.85).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "Outer Space Treaty Article II: Non-Appropriation (Extraction-Permissive Reading)").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_space_law/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, 'f6934391-ba54-41f0-89bd-37bccc58a901').
narrative_ontology:cs_kernel_codification('f6934391-ba54-41f0-89bd-37bccc58a901', fixed_text).
narrative_ontology:cs_authority_grounding('f6934391-ba54-41f0-89bd-37bccc58a901', lineage).
narrative_ontology:cs_interpretation_layer_present('f6934391-ba54-41f0-89bd-37bccc58a901').
narrative_ontology:cs_reading_relation('f6934391-ba54-41f0-89bd-37bccc58a901', ost_article_ii_non_appropriation__commons_conservation, coexists_with).
narrative_ontology:cs_reading_relation('f6934391-ba54-41f0-89bd-37bccc58a901', ost_article_ii_non_appropriation__international_regime, coexists_with).
narrative_ontology:cs_axiom('f6934391-ba54-41f0-89bd-37bccc58a901', foundational, private_property_rights_in_extracted_resources).
narrative_ontology:cs_axiom_status(private_property_rights_in_extracted_resources, holdable).
narrative_ontology:cs_axiom_grounding('f6934391-ba54-41f0-89bd-37bccc58a901', private_property_rights_in_extracted_resources, conventional).
narrative_ontology:cs_axiom('f6934391-ba54-41f0-89bd-37bccc58a901', foundational, non_appropriation_applies_only_to_sovereign_claims).
narrative_ontology:cs_axiom_status(non_appropriation_applies_only_to_sovereign_claims, holdable).
narrative_ontology:cs_axiom_grounding('f6934391-ba54-41f0-89bd-37bccc58a901', non_appropriation_applies_only_to_sovereign_claims, conventional).
narrative_ontology:cs_reference_frame('f6934391-ba54-41f0-89bd-37bccc58a901', freedom_of_access_and_use_of_space).
narrative_ontology:cs_drift_state('f6934391-ba54-41f0-89bd-37bccc58a901', contemporary_commercial_space_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f6934391-ba54-41f0-89bd-37bccc58a901', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, space_resource_companies).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, flag_states_of_space_companies).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_nations).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, future_generations).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, freedom_of_access_to_space).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, private_property_rights_in_space).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These companies invest heavily in technology to extract resources from celestial bodies. They interpret Article II as permitting private ownership of extracted resources, provided no sovereign claim is made. They benefit directly from the lack of a clear prohibition on extraction and the ability to operate under national licenses.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, space_resource_companies, agenda_setter,
    powerful, biographical, mobile, global).

% These nations license and supervise the activities of their private space companies, benefiting from economic activity, technological advancement, and prestige. They support the extraction-permissive reading as it allows their national industries to flourish without the burden of an international resource-sharing regime.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, flag_states_of_space_companies, beneficiary,
    institutional, generational, arbitrage, national).

% These nations lack the technological capability to access space resources. They bear the cost of potential future exclusion from resources that are nominally the 'province of all mankind,' with no mechanism for compensation or participation in the benefits of extraction. Their only recourse is diplomatic protest or future legal challenges.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_nations, payer,
    powerless, generational, trapped, global).

% These are the ultimate bearers of the costs if space resources are depleted or monopolized without an equitable framework. They have no voice in current policy and are structurally unable to exit the consequences of present decisions.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations, payer,
    powerless, civilizational, trapped, universal).

% Analyze the legal implications of the extraction-permissive reading, its consistency with the spirit of the OST, and its potential to create future conflicts. They highlight the ambiguities and the need for a clearer international framework.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for private entities to operate in space by clarifying that their activities, while not sovereign claims, can lead to ownership of extracted resources, thus incentivizing investment and innovation in space resource utilization.
% TRANSFER_FUNCTION: Transfers potential future wealth from celestial bodies to private space resource companies and their flag states, at the expense of non-spacefaring nations and future generations who are excluded from direct benefit or participation.
% ABSENT_VOICES: Non-spacefaring nations and future generations are largely absent from the current decision-making processes that shape the interpretation of Article II. They would advocate for a more equitable and inclusive regime for space resource governance.
% DISAPPEARANCE_RATIONALE: If this extraction-permissive reading vanished, the legal basis for private space resource extraction would become highly uncertain, likely halting investment and operations until a new, clearer international regime emerged. The current 'gold rush' mentality would cease, and the space economy would reorganize around a more restrictive or collectively managed framework.
% FOUNDING_PROBLEM: The Outer Space Treaty aimed to prevent a new 'scramble for Africa' in space by prohibiting national appropriation, while also promoting the peaceful exploration and use of outer space.
% FOUNDING_PROBLEM_CORROBORATION: Spacefaring nations and resource companies argue the problem of preventing sovereign claims is live and their interpretation upholds it. Non-spacefaring nations and many legal scholars argue the problem has shifted to preventing de facto appropriation by private actors, and the current reading fails to address this, creating a new form of enclosure. UN committees and academic papers from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ost_article_ii_non_appropriation__extraction_permissive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading allows for the effective enclosure of valuable resources by those with the means to extract them, without a clear mechanism for sharing benefits or compensating excluded parties. Suppression (0.7) is also high, as the lack of a prohibitive international regime effectively suppresses alternative, more equitable resource governance models. The 'enforcement' is the absence of prohibition, allowing a 'first-come, first-served' dynamic. Theater ratio is low (0.1) as the constraint is actively functional in enabling resource extraction, not merely performative.
 *
 * PERSPECTIVAL GAP:
 *   The space resource companies and their flag states perceive this reading as a legitimate interpretation that fosters innovation and economic growth, consistent with the OST's spirit of free access. Non-spacefaring nations and many international legal scholars view it as a loophole that undermines the 'common heritage of mankind' principle, leading to an inequitable distribution of benefits. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Space resource companies and their flag states are clear beneficiaries (d near 0.0), as this reading enables their commercial activities and national interests. Non-spacefaring nations and future generations are victims (d near 1.0), bearing the costs of exclusion and potential resource depletion without representation. International legal scholars act as observers, analyzing the structural implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    de_facto_appropriation_ambiguity,
    'Does private ownership of extracted resources constitute de facto national appropriation, given the flag state''s licensing and supervision?',
    'An International Court of Justice advisory opinion or a new multilateral treaty explicitly defining ''appropriation'' in the context of private resource extraction.',
    'If deemed de facto appropriation, this reading would be reclassified as a Snare, as its coordination function (clarity for private investment) would be revealed as cover for a prohibited act. If not, the current Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(de_facto_appropriation_ambiguity, conceptual, 'Ambiguity regarding whether private resource ownership is equivalent to national appropriation.').

omega_variable(
    common_heritage_principle_conflict,
    'Is the extraction-permissive reading compatible with the ''common heritage of mankind'' principle, which implies equitable sharing of benefits?',
    'Negotiation and adoption of an international regime for space resource utilization that includes benefit-sharing mechanisms, or a definitive ruling on the legal status of the ''common heritage'' principle in space law.',
    'If found incompatible, the legitimacy of this reading would be severely undermined, potentially leading to its reclassification as a Snare due to its inherent conflict with a foundational principle of international space law. If found compatible (e.g., through a narrow interpretation of ''common heritage''), the current classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(common_heritage_principle_conflict, preference, 'Conflict between extraction-permissive reading and the common heritage principle.').

omega_variable(
    technological_capability_as_suppression,
    'To what extent does the technological barrier to space resource access function as a form of structural suppression, effectively excluding non-spacefaring nations?',
    'Empirical analysis of global space technology development aid, capacity-building initiatives, and the actual participation rates of non-spacefaring nations in space resource ventures over time.',
    'If technological disparity is a primary and persistent form of suppression, the constraint''s overall suppression metric would be higher, pushing it closer to a Snare, as the ''freedom'' to extract is only available to a select few. If capacity-building efforts are effective, suppression might decrease over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_capability_as_suppression, empirical, 'Technological capability as a structural barrier to access and participation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t1967, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1967, 0.05).
narrative_ontology:measurement(ost__tr_t1980, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1980, 0.08).
narrative_ontology:measurement(ost__tr_t1995, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(ost__tr_t2010, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(ost__tr_t2020, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(ost__tr_t2024, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(ost__be_t1967, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1967, 0.2).
narrative_ontology:measurement(ost__be_t1980, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(ost__be_t1995, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(ost__be_t2010, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(ost__be_t2020, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(ost__be_t2024, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t1967, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1967, 0.1).
narrative_ontology:measurement(ost__su_t1980, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(ost__su_t1995, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(ost__su_t2010, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(ost__su_t2020, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2020, 0.65).
narrative_ontology:measurement(ost__su_t2024, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, national_space_resource_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Outer Space Treaty's Article II non-appropriation principle. This 'extraction-permissive' reading directly influences national space resource laws and is in tension with the 'commons-conservation' and 'international-regime' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
