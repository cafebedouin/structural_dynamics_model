% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: ost_article_ii_non_appropriation__extraction_permissive
 *   human_readable: Article II Extraction-Permissive Reading: Private Resource Ownership Without Sovereignty
 *   domain: international_space_law/treaty_interpretation/commons_governance
 *
 * SUMMARY:
 *   This constraint instantiates the extraction-permissive reading of the
 *   Article II non-appropriation kernel in the 1967 Outer Space Treaty. Under
 *   this reading, Article II prohibits sovereign territorial claims to
 *   celestial bodies but does not prohibit private ownership of extracted
 *   resources, creating a legal regime where technologically capable states
 *   and their licensed commercial actors can effectuate de facto enclosure
 *   through extraction and flag-state property recognition. The
 *   natural-language label 'Article II non-appropriation' conflates three
 *   structurally distinct readings; this file models only the
 *   extraction-permissive reading, which carries a high-extractiveness
 *   ledger: resource access is gated by technological capability and
 *   flag-state legal recognition, with no compensation mechanism for excluded
 *   states. The constraint combines genuine coordination (preventing
 *   interstate territorial conflict) with asymmetric extraction (enclosure
 *   via fait accompli).
 *
 * KEY AGENTS:
 *   - Space-faring nations (agenda_setter / institutional / arbitrage): Administer the treaty interpretation, license private actors, and capture strategic first-mover advantage.
 *   - Private resource extractors (beneficiary / powerful / constrained): Licensed commercial entities who capture resource rents under flag-state legal recognition.
 *   - Excluded states (payer / powerless / trapped): Non-space-faring treaty parties locked into a regime where they renounced sovereignty but gained no compensatory resource access.
 *   - Global South advocates (excluded / organized / trapped): Structurally absent from bilateral negotiations, voice intergenerational equity and common heritage claims.
 *   - International space law scholars (observer / analytical): Analytical seat tracking doctrinal divergence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.82).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.68).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.82).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "Article II Extraction-Permissive Reading: Private Resource Ownership Without Sovereignty").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_space_law/treaty_interpretation/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '15a6c882-344b-4c8c-ab04-aca1b56a9be4').
narrative_ontology:cs_kernel_codification('15a6c882-344b-4c8c-ab04-aca1b56a9be4', formalized).
narrative_ontology:cs_authority_grounding('15a6c882-344b-4c8c-ab04-aca1b56a9be4', lineage).
narrative_ontology:cs_interpretation_layer_present('15a6c882-344b-4c8c-ab04-aca1b56a9be4').
narrative_ontology:cs_reading_relation('15a6c882-344b-4c8c-ab04-aca1b56a9be4', ost_article_ii_non_appropriation__commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('15a6c882-344b-4c8c-ab04-aca1b56a9be4', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('15a6c882-344b-4c8c-ab04-aca1b56a9be4', foundational, extraction_not_equivalent_to_appropriation).
narrative_ontology:cs_axiom_status(extraction_not_equivalent_to_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('15a6c882-344b-4c8c-ab04-aca1b56a9be4', extraction_not_equivalent_to_appropriation, conventional).
narrative_ontology:cs_axiom('15a6c882-344b-4c8c-ab04-aca1b56a9be4', foundational, flag_state_property_recognition_valid_in_space).
narrative_ontology:cs_axiom_status(flag_state_property_recognition_valid_in_space, holdable).
narrative_ontology:cs_axiom_grounding('15a6c882-344b-4c8c-ab04-aca1b56a9be4', flag_state_property_recognition_valid_in_space, conventional).
narrative_ontology:cs_reference_frame('15a6c882-344b-4c8c-ab04-aca1b56a9be4', territorial_prohibition_resource_neutrality).
narrative_ontology:cs_drift_state('15a6c882-344b-4c8c-ab04-aca1b56a9be4', artemis_accords_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('15a6c882-344b-4c8c-ab04-aca1b56a9be4', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, space_faring_nations).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, private_resource_extractors).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, excluded_states).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, private_property_in_space_resources).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, flag_state_regulatory_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Technologically capable states that interpret Article II as permitting private resource extraction while prohibiting territorial sovereignty. They license domestic private actors, pass national space resource legislation, and negotiate bilateral agreements (Artemis Accords) that normalize this reading. They shape treaty interpretation through state practice and diplomatic influence, capturing strategic and economic advantage from first-mover extraction rights without assuming formal sovereignty obligations.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, space_faring_nations, agenda_setter,
    institutional, generational, arbitrage, global).

% Commercial entities licensed by flag-states to prospect for and extract space resources. Under this reading, they obtain legally recognized ownership of extracted materials while operating on celestial bodies without claiming sovereignty over the territory itself. Their exit is constrained by capital requirements, regulatory dependence on flag-state authorization, and the long lead times of space technology, but they are structurally positioned to capture the resource rents.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, private_resource_extractors, beneficiary,
    powerful, biographical, constrained, global).

% States parties to the Outer Space Treaty that lack launch capability or domestic space industry. They are legally bound by Article II's renunciation of sovereignty but receive no compensatory access to resources extracted by others. Their nationals cannot independently undertake space mining, and they depend on technology transfer or international redistribution mechanisms that do not currently exist. They cannot exit the treaty regime and cannot prevent flag-state licensing of extraction.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, excluded_states, payer,
    powerless, generational, trapped, global).

% Civil society and advocacy groups representing populations in non-space-faring regions who argue for common heritage of mankind principles and intergenerational equity. They are structurally absent from flag-state licensing decisions and bilateral Artemis Accord negotiations where the extraction-permissive reading is operationalized.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, global_south_advocates, excluded,
    organized, generational, trapped, global).

% Academic and legal experts who analyze competing interpretations of Article II. They document the divergence between the extraction-permissive reading advanced by space-faring states and the commons-conservation reading grounded in the Moon Agreement and equitable access principles. They do not collect or pay within the constraint, but their doctrinal work shapes how states justify their positions.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, international_space_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__extraction_permissive, private_resource_extractors).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__extraction_permissive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents interstate conflict by prohibiting sovereign territorial claims to celestial bodies, establishing space as a legally non-appropriable commons at the state level.
% TRANSFER_FUNCTION: Moves de facto resource access and future wealth from states without launch or mining capability to technologically advanced states and their licensed private extractors, through national legislation recognizing private ownership of extracted materials without requiring international redistribution or compensation.
% ABSENT_VOICES: Non-space-faring states in bilateral Artemis Accord negotiations; indigenous and intergenerational equity advocates who assert common heritage principles but are excluded from flag-state licensing frameworks; future generations who cannot contest current extraction allocations.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, sovereign territorial claims might re-emerge as states seek to secure resource access formally, or a strict commons-conservation regime might take hold, freezing current investment patterns and altering the strategic calculus of space-faring nations.
% FOUNDING_PROBLEM: Preventing Cold War-era national sovereignty scrambles for the Moon and celestial bodies that could replicate colonial land rushes and trigger interstate conflict.
% FOUNDING_PROBLEM_CORROBORATION: Space historians and non-space-faring states in COPUOS attest to the genuine territorial concern that motivated Article II. However, competition economists and excluded-state delegations attest that the current extraction-permissive interpretation has shifted the arrangement's function from conflict-prevention to enclosure, and that the original problem is now used to justify a commercially extractive regime.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness is high (0.82) because the reading converts a formal commons into a de facto enclosure system where extraction rights are allocated by technological capacity and flag-state patronage without redistribution. Suppression (0.68) reflects the active enforcement required to maintain this contested interpretation against the commons-conservation reading and excluded-state resistance, primarily through national legislation, bilateral accord normalization, and diplomatic pressure in COPUOS. Theater ratio (0.45) acknowledges that the territorial prohibition is a genuine, functional coordination mechanism, but an increasing share of the regime's activity is performative maintenance of 'commons' framing while extraction proceeds. Accessibility collapse (0.75) is high because fait accompli extraction and vested property expectations make reverting to an international-regime or pure-conservation reading increasingly costly. Resistance (0.55) is moderate and rising, led by excluded states and Moon Agreement advocates, but insufficient to override the dominant interpretation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (space-faring nations) computes the constraint as solving a genuine coordination problemâpreventing war over territory and enabling commercial development. The payer seat (excluded states) computes it as an asymmetric extraction mechanism dressed in commons language, where the renunciation of sovereignty without resource guarantees amounts to unilateral disarmament. The engine derives this divergence from the structural data: identical treaty text, opposite directionality depending on technological capability and flag-state alignment.
 *
 * DIRECTIONALITY LOGIC:
 *   Space-faring nations sit near the beneficiary end: they control the interpretive agenda, license extraction, and benefit from domestic industry and strategic resource positioning. Private resource extractors are direct beneficiaries, though their exit is constrained by capital and regulatory dependence. Excluded states are full targets: they bear the opportunity cost of foregone resource access and lost sovereignty without compensation, and they are trapped in the treaty regime. The directionality asymmetry is stark: the same legal text that prevents powerful states from claiming territory also prevents powerless states from claiming equitable share, while enabling private capture by actors aligned with powerful states.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint prevents mislabeling by requiring both coordination and extraction to be present for tangled_rope certification. The genuine coordination functionâpreventing sovereign territorial scramblesâis historically documented and structurally real. However, the reading adds an extraction layer by interpreting 'non-appropriation' to reach only territorial sovereignty while permitting private resource ownership. Without the extraction layer (victims, asymmetric transfer, active enforcement against competing readings), the constraint would classify as rope or scaffold. Without the coordination layer (genuine conflict-prevention function, territorial prohibition), it would be a pure snare. The tangled_rope classification captures the hybrid accurately: the coordination is real, but the same structure that prevents territorial war also enables technological enclosure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_as_appropriation_ambiguity,
    'Does physical extraction and permanent removal of celestial resources constitute ''appropriation'' prohibited by Article II, or is it permissible ''use'' distinct from territorial sovereignty?',
    'International Court of Justice advisory opinion on the Moon Agreement''s relationship to OST Article II, or coherent state practice establishing a definitive interpretation.',
    'If extraction is adjudicated as appropriation, this reading collapses toward commons_conservation; if separable, extraction_permissive remains stable and extractiveness stays high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_as_appropriation_ambiguity, conceptual, 'Core ambiguity between extraction-permissive and commons-conservation readings').

omega_variable(
    technological_vs_legal_exclusion,
    'Is resource access inequality driven primarily by natural technological capability gaps, or by flag-state legal recognition that converts capability into enforceable property rights excluded from redistribution?',
    'Comparative analysis of extraction feasibility without flag-state licensing and legal title recognition versus extraction under a hypothetical international licensing regime.',
    'If primarily legal, the constraint''s extractiveness is constructed rather than natural, and the directionality toward excluded states is higher than a raw capability gap would imply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_vs_legal_exclusion, empirical, 'Whether exclusion is technological or legal-construct').

omega_variable(
    reading_stabilization_uncertainty,
    'Which reading of Article II will be stabilized by future state practice: permissive extraction, commons conservation, or deferred international regime?',
    'Longitudinal tracking of national space legislation, Artemis Accord adherence, and multilateral negotiation outcomes over the next two decades.',
    'Determines whether the current high-extractiveness ledger is a transitional scaffold toward an international regime or a terminal steady-state enclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_stabilization_uncertainty, empirical, 'Stabilization trajectory among kernel readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost_artii_extr_tr_t0, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ost_artii_extr_tr_t16, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 16, 0.22).
narrative_ontology:measurement(ost_artii_extr_tr_t32, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 32, 0.28).
narrative_ontology:measurement(ost_artii_extr_tr_t48, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 48, 0.38).
narrative_ontology:measurement(ost_artii_extr_tr_t56, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 56, 0.42).
narrative_ontology:measurement(ost_artii_extr_tr_t60, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 60, 0.45).

% Extraction over time
narrative_ontology:measurement(ost_artii_extr_be_t0, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ost_artii_extr_be_t16, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 16, 0.22).
narrative_ontology:measurement(ost_artii_extr_be_t32, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 32, 0.28).
narrative_ontology:measurement(ost_artii_extr_be_t48, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 48, 0.58).
narrative_ontology:measurement(ost_artii_extr_be_t56, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 56, 0.72).
narrative_ontology:measurement(ost_artii_extr_be_t60, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 60, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(ost_artii_extr_su_t0, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(ost_artii_extr_su_t16, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(ost_artii_extr_su_t32, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 32, 0.38).
narrative_ontology:measurement(ost_artii_extr_su_t48, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 48, 0.6).
narrative_ontology:measurement(ost_artii_extr_su_t56, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 56, 0.65).
narrative_ontology:measurement(ost_artii_extr_su_t60, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 60, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__extraction_permissive, 0.1).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, international_regime).

% DUAL FORMULATION NOTE:
% The natural-language label 'Article II non-appropriation' conflates three structurally distinct readings: extraction_permissive (private extraction permitted, territorial claims barred), commons_conservation (extraction equals prohibited appropriation), and international_regime (deferral to future multilateral framework). Each reading has different beneficiary/victim structures, different epsilon values, and different coordination/extraction balances. They are modeled as separate constraints linked by network edges, not as one constraint with observable-dependent classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
