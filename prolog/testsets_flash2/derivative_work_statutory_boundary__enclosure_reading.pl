% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Statutory Boundary (Enclosure Reading)
 *   domain: intellectual_property/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint represents the 'enclosure reading' of the derivative work
 *   statutory boundary, where any use of copyrighted expression in creating
 *   new work is considered the preparation of a derivative work. This
 *   interpretation mandates licensing pre-creation, bottlenecks downstream
 *   innovation, and channels significant extraction to incumbent copyright
 *   holders. The claimed type is 'snare' because the coordination story
 *   (incentivizing creation) is largely cover for active extraction and
 *   suppression of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.88).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.92).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary (Enclosure Reading)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '57b097c3-33fd-4ff1-88d5-715db99fb19e').
narrative_ontology:cs_kernel_codification('57b097c3-33fd-4ff1-88d5-715db99fb19e', fixed_text).
narrative_ontology:cs_authority_grounding('57b097c3-33fd-4ff1-88d5-715db99fb19e', extraction).
narrative_ontology:cs_interpretation_layer_present('57b097c3-33fd-4ff1-88d5-715db99fb19e').
narrative_ontology:cs_reading_relation('57b097c3-33fd-4ff1-88d5-715db99fb19e', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('57b097c3-33fd-4ff1-88d5-715db99fb19e', derivative_work_statutory_boundary__hybrid_carveout_reading, forecloses).
narrative_ontology:cs_axiom('57b097c3-33fd-4ff1-88d5-715db99fb19e', foundational, any_use_is_derivative).
narrative_ontology:cs_axiom_status(any_use_is_derivative, holdable).
narrative_ontology:cs_axiom_grounding('57b097c3-33fd-4ff1-88d5-715db99fb19e', any_use_is_derivative, conventional).
narrative_ontology:cs_axiom('57b097c3-33fd-4ff1-88d5-715db99fb19e', secondary, maximal_control_incentivizes_creation).
narrative_ontology:cs_axiom_status(maximal_control_incentivizes_creation, holdable).
narrative_ontology:cs_axiom_grounding('57b097c3-33fd-4ff1-88d5-715db99fb19e', maximal_control_incentivizes_creation, instrumental).
narrative_ontology:cs_reference_frame('57b097c3-33fd-4ff1-88d5-715db99fb19e', maximal_copyright_control_framework).
narrative_ontology:cs_drift_state('57b097c3-33fd-4ff1-88d5-715db99fb19e', contemporary_generative_ai_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('57b097c3-33fd-4ff1-88d5-715db99fb19e', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, licensing_agencies).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, ai_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, transformative_artists).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, open_source_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own vast catalogs of copyrighted works and assert broad control over any subsequent use, demanding licenses and royalties for even minimal incorporation into new creations. They benefit directly from the expanded scope of 'derivative work'.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Act as intermediaries, facilitating the collection of licensing fees from creators of new works and distributing them to copyright holders. Their business model thrives on the expansive definition of derivative work.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, licensing_agencies, beneficiary,
    organized, biographical, mobile, global).

% Require access to large datasets of existing works for training generative AI models. Under this reading, even internal processing of copyrighted material for training could be deemed derivative work, necessitating extensive and costly licensing, or facing legal challenges.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, ai_developers, payer,
    powerful, immediate, constrained, global).

% Create new works that build upon, reinterpret, or sample existing copyrighted material. This reading forces them into complex and expensive licensing negotiations, stifling creative expression and leading to self-censorship or legal risk.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, transformative_artists, payer,
    moderate, biographical, identity_locked, local).

% Rely on the free and open exchange of code and content. An expansive derivative work definition creates legal uncertainty and potential liability for collaborative projects, hindering innovation and community growth.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, open_source_communities, payer,
    organized, generational, constrained, global).

% Argue for a robust public domain and limited copyright terms to foster creativity and access to knowledge. Their arguments are largely ignored or dismissed by the legal and commercial structures that benefit from this expansive reading.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, public_domain_advocates, excluded,
    moderate, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate the fair compensation of original creators by ensuring they benefit from all subsequent uses of their work, thereby incentivizing creation.
% TRANSFER_FUNCTION: Transfers potential revenue from any new work that incorporates or is inspired by existing copyrighted expression, from the new creator to the original copyright holder.
% ABSENT_VOICES: The voices of future innovators, artists, and researchers who are deterred from creating due to prohibitive licensing costs or legal uncertainty are absent. Public domain advocates are also excluded from meaningful influence.
% DISAPPEARANCE_RATIONALE: If this expansive reading of derivative work vanished, there would be an immediate explosion of new creative and technological works building on existing material without prior authorization. Licensing markets would collapse, and incumbent copyright holders would lose a significant revenue stream, forcing a re-evaluation of their business models.
% FOUNDING_PROBLEM: To protect the economic rights of authors and artists, ensuring they receive compensation when their original works are adapted or transformed into new, marketable creations.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent copyright holders and licensing agencies assert the problem is live, citing the need to protect creators in the digital age. AI developers, transformative artists, and open-source communities argue the problem has been over-solved, and the current interpretation stifles innovation rather than incentivizing it; legal scholars and economists outside the benefiting parties corroborate this shifted-function reading.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the interpretation grants broad control over subsequent creation, allowing incumbent copyright holders to demand significant fees for uses that might otherwise be considered transformative or fair. Suppression is also very high (0.92) due to the legal and technical enforcement mechanisms that prevent unauthorized use, effectively trapping new creators. Theater ratio is low (0.15) as the enforcement is genuinely aimed at revenue collection, not merely performance. Accessibility collapse is high (0.75) because the legal interpretation significantly narrows the scope of non-infringing use, making alternatives to licensing difficult to find. Resistance is high (0.70) from AI developers and transformative artists who actively challenge this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of incumbent copyright holders, this is a necessary protection of their intellectual property, ensuring fair compensation. From the perspective of new creators, it is an extractive barrier that stifles innovation and creativity. The engine's classification as a snare reflects the latter, emphasizing the coercive and extractive aspects over the claimed coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent copyright holders and licensing agencies are clear beneficiaries, collecting substantial revenue. AI developers, transformative artists, and open-source communities are victims, bearing the costs of licensing or facing legal risks. Public domain advocates are excluded, their arguments having little impact on the prevailing legal interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_expression_vs_idea,
    'Where is the precise boundary between unprotectable ''idea'' and protectable ''expression'' in the context of generative AI training data?',
    'Landmark court rulings specifically addressing the ''input'' vs. ''output'' debate for AI models, or legislative clarification on data ingestion as ''use''.',
    'A narrow interpretation would reduce extractiveness and suppression for AI developers, potentially shifting the constraint towards a Tangled Rope or even Rope. A broad interpretation would solidify its Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_expression_vs_idea, conceptual, 'Ambiguity in the idea-expression dichotomy for AI training.').

omega_variable(
    transformative_use_doctrine_applicability,
    'To what extent does the ''transformative use'' doctrine apply to new works created using copyrighted material under this expansive derivative work reading?',
    'Consistent judicial application of transformative use principles to AI-generated content and other new forms of creation, or legislative codification of specific carve-outs.',
    'If transformative use is broadly applied, it would reduce the effective extractiveness and suppression, offering an ''exit option'' for creators and potentially reclassifying the constraint. If narrowly applied, the Snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_doctrine_applicability, empirical, 'Uncertainty regarding the scope of transformative use in new creative contexts.').

omega_variable(
    mandate_vs_rent_seeking,
    'Is the primary function of this expansive derivative work definition still to incentivize original creation, or has it become primarily a mechanism for rent-seeking by incumbent copyright holders?',
    'Economic studies comparing new creation rates and licensing revenues under different derivative work interpretations, and analysis of market concentration in creative industries.',
    'If primarily rent-seeking, the Snare classification is strongly validated. If a genuine incentive function is dominant, the constraint might lean towards a Tangled Rope, acknowledging a coordination function alongside extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_vs_rent_seeking, empirical, 'Whether the constraint''s mandate has drifted from incentive to extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 10, 0.85).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 15, 0.87).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 20, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 10, 0.89).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 15, 0.91).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 20, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, ai_training_data_access).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, fair_use_doctrine_application).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, digital_rights_management_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is the 'enclosure_reading' of the 'derivative_work_statutory_boundary' kernel. It is linked to 'coordination_reading' and 'hybrid_carveout_reading' as sibling interpretations of the same legal concept.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
