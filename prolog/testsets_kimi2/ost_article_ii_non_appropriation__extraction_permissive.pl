% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__extraction_permissive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Article II Extraction-Permissive Reading: Private Resource Ownership
 *   domain: international_space_law/commons_governance
 *
 * SUMMARY:
 *   This constraint instantiates the extraction_permissive reading of the
 *   ost_article_ii_non_appropriation kernel. Article II of the Outer Space
 *   Treaty is read here as prohibiting sovereign territorial claims over
 *   celestial bodies while permitting private actors to acquire and own
 *   extracted resources under flag-state legal recognition. The kernel is
 *   contested: the commons_conservation reading holds that extraction
 *   constitutes de facto appropriation prohibited by Article II, and the
 *   international_regime reading holds that Article II defers the question to
 *   a future multilateral framework. This reading produces a
 *   high-extractiveness ledger in which resource access is gated by
 *   technological capability and national legal frameworks, excluded states
 *   receive no compensation, and enclosure proceeds via fait accompli rather
 *   than formal annexation.
 *
 * KEY AGENTS:
 *   - Spacefaring states: Primary agenda-setter and beneficiary (institutional/arbitrage) â interpret and enforce the permissive reading through national legislation.
 *   - Commercial extractors: Primary beneficiary (powerful/constrained) â receive legal title to extracted resources under flag-state licensing.
 *   - Non-spacefaring states: Primary payer (organized/constrained) â bear the cost of commons exclusion and foregone benefit-sharing.
 *   - Multilateral governance bodies: Analytical observer (institutional/analytical) â lack enforcement authority to compel multilateral resource governance.
 *   - Future generations: Excluded seat (powerless/trapped) â have no voice in current allocation but bear long-term depletion costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__extraction_permissive, 0.78).
domain_priors:suppression_score(ost_article_ii_non_appropriation__extraction_permissive, 0.65).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__extraction_permissive, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, extractiveness, 0.78).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__extraction_permissive, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__extraction_permissive, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__extraction_permissive, "Article II Extraction-Permissive Reading: Private Resource Ownership").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__extraction_permissive, "international_space_law/commons_governance").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__extraction_permissive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__extraction_permissive, '514736bf-3f57-44b9-9087-a3b3e4baa46e').
narrative_ontology:cs_kernel_codification('514736bf-3f57-44b9-9087-a3b3e4baa46e', fixed_text).
narrative_ontology:cs_authority_grounding('514736bf-3f57-44b9-9087-a3b3e4baa46e', distributed).
narrative_ontology:cs_reading_relation('514736bf-3f57-44b9-9087-a3b3e4baa46e', ost_article_ii_non_appropriation__commons_conservation, forecloses).
narrative_ontology:cs_reading_relation('514736bf-3f57-44b9-9087-a3b3e4baa46e', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('514736bf-3f57-44b9-9087-a3b3e4baa46e', foundational, private_title_to_extracted_resources).
narrative_ontology:cs_axiom_status(private_title_to_extracted_resources, holdable).
narrative_ontology:cs_axiom_grounding('514736bf-3f57-44b9-9087-a3b3e4baa46e', private_title_to_extracted_resources, conventional).
narrative_ontology:cs_axiom('514736bf-3f57-44b9-9087-a3b3e4baa46e', foundational, non_appropriation_limited_to_territory).
narrative_ontology:cs_axiom_status(non_appropriation_limited_to_territory, holdable).
narrative_ontology:cs_axiom_grounding('514736bf-3f57-44b9-9087-a3b3e4baa46e', non_appropriation_limited_to_territory, conventional).
narrative_ontology:cs_reference_frame('514736bf-3f57-44b9-9087-a3b3e4baa46e', flag_state_extraction_liberty).
narrative_ontology:cs_drift_state('514736bf-3f57-44b9-9087-a3b3e4baa46e', artemis_accords_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('514736bf-3f57-44b9-9087-a3b3e4baa46e', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__extraction_permissive, commercial_extractors).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states).
narrative_ontology:constraint_vindicates(ost_article_ii_non_appropriation__extraction_permissive, flag_state_resource_jurisdiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact domestic legislation recognizing private property rights in extracted space resources and negotiate bilateral agreements normalizing extraction. They interpret Article II as prohibiting only sovereign territorial claims while permitting commercial extraction and ownership. Their exit is to shape or abandon the treaty interpretation, but they benefit from maintaining the permissive reading to legitimize national commercial programs.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__extraction_permissive, spacefaring_states, beneficiary).

% Invest in extraction technology under the legal certainty provided by national space resource laws. They acquire title to extracted materials under flag-state legal recognition. Their operations depend on continued permissive interpretation and state enforcement against competing claims.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, commercial_extractors, beneficiary,
    powerful, biographical, constrained, global).

% Are excluded from resource benefits by lack of technological capacity and denied compensation or benefit-sharing under the permissive reading. They bear the cost of commons enclosure as orbital and surface resources are claimed through fait accompli by spacefaring actors. Their diplomatic objections are recorded but do not block national licensing regimes.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, non_spacefaring_states, payer,
    organized, generational, constrained, global).

% Attempt to negotiate a multilateral framework for space resource governance but are preempted by unilateral and bilateral permissive regimes. They provide forums for dispute but lack enforcement authority to compel benefit-sharing or restrain extraction.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, multilateral_governance_bodies, observer,
    institutional, civilizational, analytical, global).

% Have no voice in current treaty interpretation or resource licensing but bear the long-term cost of commons depletion and enclosure through fait accompli extraction.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__extraction_permissive, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__extraction_permissive, commercial_extractors).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__extraction_permissive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents sovereign territorial claims over celestial bodies that could trigger geopolitical conflict and arms races in outer space; provides legal certainty for commercial investment in resource extraction by distinguishing territorial annexation from resource use.
% TRANSFER_FUNCTION: Moves legal title to extracted space resources from the international commons to private actors and flag states with technological capacity, while transferring the cost of exclusion and foregone commons access to non-spacefaring states.
% ABSENT_VOICES: Non-spacefaring states are present in UN forums but structurally excluded from the bilateral and national legal processes that determine resource allocation. Indigenous cosmological perspectives on celestial bodies and future generations who might benefit from a preserved commons have no seat.
% DISAPPEARANCE_RATIONALE: If the permissive extraction reading vanished and Article II were uniformly interpreted to prohibit private resource ownership, commercial extraction investment would halt or restructure, spacefaring states would lose the legal foundation for national resource licensing regimes, and the distribution of space resource benefits would shift from unilateral fait accompli to negotiated multilateral arrangement.
% FOUNDING_PROBLEM: The 1967 Outer Space Treaty was built to prevent the extension of national sovereignty and Cold War territorial competition into outer space, while leaving the legal status of resource extraction ambiguous because no state had the capacity to extract.
% FOUNDING_PROBLEM_CORROBORATION: Space historians and legal scholars attest the founding problem was Cold War territorial scramble prevention. Non-spacefaring states and multilateral bodies attest the problem of sovereign territorial claims has been replaced by commercial enclosure, and the arrangement now serves extraction rather than prevention; the US State Department and commercial space advocates assert the founding problem is still live but frame it as safety and sustainability rather than territorial competition.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__extraction_permissive, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__extraction_permissive, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__extraction_permissive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__extraction_permissive, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__extraction_permissive, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.78) because the permissive reading enables transfer of commons resources to private title without compensation or multilateral benefit-sharing, and technological capacity acts as a de facto gate. Suppression is substantial (0.65) because the reading actively suppresses the commons_conservation and international_regime alternatives through unilateral national legislation and bilateral accords that preempt multilateral frameworks. Theater ratio is moderate (0.45): the constraint performs compliance with the non-appropriation principle by avoiding formal territorial claims while accomplishing de facto resource enclosure through extraction licensing. Accessibility collapse is high (0.70) because once the permissive reading is entrenched in national law and bilateral practice, reverting to a commons regime becomes legally and politically difficult. Resistance is moderate (0.55) because non-spacefaring states object diplomatically but lack the power to block fait accompli extraction. Temporal measurements trace the shift from low-extraction coordination in 1967 to high-extraction enclosure by 2025 as national space resource laws and Artemis-style bilateral agreements accumulated.
 *
 * PERSPECTIVAL GAP:
 *   The spacefaring state seat experiences the constraint as necessary legal coordination that prevents territorial war and enables commercial investment. The non-spacefaring state seat experiences the same legal structure as extractive enclosure that transfers commons wealth to technologically advanced actors without consent or compensation. The commercial extractor seat sees property rights and legal certainty; the excluded future-generations seat sees irreversible depletion. The engine computes this divergence from the structural asymmetry in power, exit, and directional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Spacefaring states are beneficiaries with arbitrage-grade exit options â they can shape interpretation, negotiate bilateral agreements, or withdraw from treaty obligations; their directionality sits near the beneficiary end. Commercial extractors are beneficiaries but have constrained exit because their investment depends on the specific permissive legal framework; their directionality is beneficiary-leaning but less extreme. Non-spacefaring states are payers with constrained exit â they can object diplomatically but cannot exit the commons loss or replicate extraction; their directionality sits near the full-target end. Future generations are excluded and trapped, sitting at the extreme target end. Multilateral bodies are observers with analytical exit, near symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate was preventing sovereign territorial competition in space. That problem is largely dead, but the constraint persists and has been repurposed to legitimate resource extraction. Classifying as tangled_rope rather than snare preserves the genuine residual coordination function â the constraint still prevents formal territorial annexation â while capturing the asymmetric extraction layered on top through the permissive reading. A snare classification would erase the real coordination that remains; a rope classification would ignore the fait accompli enclosure and uncompensated exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_ii_scope_ambiguity,
    'Does Article II''s non-appropriation principle apply to extracted resources, or only to sovereign territorial claims over celestial bodies?',
    'ICJ advisory opinion or ratification of the Moon Agreement by major spacefaring powers establishing binding interpretation.',
    'If non-appropriation covers extracted resources, this reading collapses toward snare classification with extraction constituting de facto appropriation; if territorially limited, the coordination function of preventing sovereign claims dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_ii_scope_ambiguity, conceptual, 'Ambiguity in Article II scope regarding extracted resources').

omega_variable(
    fait_accompli_enclosure,
    'Does the permissive reading enable irreversible de facto enclosure of space resources by technologically advanced actors before a multilateral benefit-sharing regime can form?',
    'Tracking of licensed extraction claims, actual resource utilization rates, and comparative analysis with terrestrial commons enclosure precedents.',
    'If yes, the constraint functions as extraction-forward tangled rope with suppressed alternatives; if no, it remains a legal coordination mechanism with moderate distributional asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fait_accompli_enclosure, empirical, 'Whether extraction permissiveness leads to irreversible commons enclosure').

omega_variable(
    reading_independence_from_siblings,
    'Is the extraction-permissive reading structurally independent from its sibling readings, or does its classification depend on rejecting the commons_conservation reading as a legal possibility?',
    'Comparative legal analysis of state practice and domestic legislation to determine whether the permissive reading is held as an affirmative legal doctrine or merely as a default in the absence of prohibition.',
    'If the reading is structurally dependent on rejecting conservation, its high extractiveness is actively contested; if independent, it may represent emergent customary law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_independence_from_siblings, conceptual, 'Structural relationship between extraction-permissive and conservation readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__extraction_permissive, 0, 58).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 0, 0.1).
narrative_ontology:measurement(ost__tr_t15, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 15, 0.16).
narrative_ontology:measurement(ost__tr_t30, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 30, 0.24).
narrative_ontology:measurement(ost__tr_t45, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 45, 0.34).
narrative_ontology:measurement(ost__tr_t58, ost_article_ii_non_appropriation__extraction_permissive, theater_ratio, 58, 0.45).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(ost__be_t15, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(ost__be_t30, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(ost__be_t45, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(ost__be_t58, ost_article_ii_non_appropriation__extraction_permissive, base_extractiveness, 58, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t0, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(ost__su_t15, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(ost__su_t30, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(ost__su_t45, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 45, 0.55).
narrative_ontology:measurement(ost__su_t58, ost_article_ii_non_appropriation__extraction_permissive, suppression_requirement, 58, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__extraction_permissive, resource_allocation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__commons_conservation).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__extraction_permissive, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% The ost_article_ii_non_appropriation kernel decomposes into three structurally distinct constraints: commons_conservation (high coordination, low extraction), extraction_permissive (high extraction, coordination residual), and international_regime (coordination through deferral). Each has a different epsilon and different stakeholder asymmetry. The confusion was in the label 'Article II,' not in the structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
