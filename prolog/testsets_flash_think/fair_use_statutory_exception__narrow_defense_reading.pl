% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense (Property-Centric Reading)
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the fair use statutory
 *   exception (Section 107 of the US Copyright Act) that prioritizes
 *   copyright holders' property rights and market interests. Under this
 *   'narrow defense' reading, fair use is strictly construed as an
 *   affirmative defense, placing a heavy burden on the defendant, with
 *   commercial nature and potential market harm often being determinative
 *   factors. Transformative use, while acknowledged, is often underweighted
 *   compared to market considerations. This reading is one of several
 *   competing interpretations of fair use.
 *
 * KEY AGENTS:
 *   - copyright_holders: Primary beneficiary/agenda_setter (institutional/arbitrage)
 *   - content_licensors: Beneficiary (powerful/arbitrage)
 *   - content_creators_reusers: Primary target/payer (moderate/constrained)
 *   - innovators: Target/payer (moderate/constrained)
 *   - public_domain_advocates: Excluded (organized/analytical)
 *   - courts: Agenda_setter/observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.75).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.7).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense (Property-Centric Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, 'b51e07a2-1b57-4b06-9d64-54caee6eba85').
narrative_ontology:cs_kernel_codification('b51e07a2-1b57-4b06-9d64-54caee6eba85', fixed_text).
narrative_ontology:cs_authority_grounding('b51e07a2-1b57-4b06-9d64-54caee6eba85', lineage).
narrative_ontology:cs_interpretation_layer_present('b51e07a2-1b57-4b06-9d64-54caee6eba85').
narrative_ontology:cs_reading_relation('b51e07a2-1b57-4b06-9d64-54caee6eba85', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('b51e07a2-1b57-4b06-9d64-54caee6eba85', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('b51e07a2-1b57-4b06-9d64-54caee6eba85', foundational, copyright_as_absolute_property).
narrative_ontology:cs_axiom_status(copyright_as_absolute_property, holdable).
narrative_ontology:cs_axiom_grounding('b51e07a2-1b57-4b06-9d64-54caee6eba85', copyright_as_absolute_property, deontological).
narrative_ontology:cs_axiom('b51e07a2-1b57-4b06-9d64-54caee6eba85', foundational, market_harm_test_primacy).
narrative_ontology:cs_axiom_status(market_harm_test_primacy, holdable).
narrative_ontology:cs_axiom_grounding('b51e07a2-1b57-4b06-9d64-54caee6eba85', market_harm_test_primacy, empirically_contingent).
narrative_ontology:cs_reference_frame('b51e07a2-1b57-4b06-9d64-54caee6eba85', traditional_property_rights_framework).
narrative_ontology:cs_drift_state('b51e07a2-1b57-4b06-9d64-54caee6eba85', digital_era_mass_reproduction, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b51e07a2-1b57-4b06-9d64-54caee6eba85', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, content_licensors).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, content_creators_reusers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, innovators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns the exclusive rights to creative works and actively enforces them. Benefits from a narrow interpretation of fair use that maximizes licensing opportunities and market control. Litigates against perceived infringements, placing the burden of defense on users.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Companies that manage and monetize intellectual property rights on behalf of creators. A narrow fair use interpretation expands their potential licensing revenue and strengthens their negotiating position.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, content_licensors, beneficiary,
    powerful, biographical, arbitrage, global).

% Artists, educators, and digital creators who wish to incorporate existing copyrighted material into new works. They face legal risk and potential litigation costs, even for uses they believe are fair, due to the narrow and unpredictable nature of the defense.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, content_creators_reusers, payer,
    moderate, biographical, constrained, global).

% Developers of new technologies (e.g., AI training models, search engines, digital archives) that rely on processing large datasets of copyrighted material. They face significant legal uncertainty and potential liability, hindering innovation that could benefit the public.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, innovators, payer,
    moderate, biographical, constrained, global).

% Organizations and individuals who champion the public's right to access, use, and build upon cultural works. They are often excluded from the direct legal enforcement process but advocate for legislative and judicial changes to broaden fair use and strengthen the public domain.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, public_domain_advocates, excluded,
    organized, generational, analytical, global).

% Interpret and apply copyright law, including fair use. This reading emphasizes judicial deference to copyright holders' property rights and market interests, often requiring defendants to prove their use is fair under strict criteria.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, courts, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal framework for the ownership and commercial exploitation of creative works, providing incentives for creators by granting them exclusive rights and a mechanism to defend those rights.
% TRANSFER_FUNCTION: Transfers economic value from potential reusers and innovators to copyright holders by limiting the scope of free use, thereby increasing the necessity and value of licensing agreements.
% ABSENT_VOICES: Transformative artists, educators, open-source developers, and digital archivists who advocate for a broader interpretation of fair use as a right that facilitates cultural production and innovation, rather than merely a narrow defense against infringement.
% DISAPPEARANCE_RATIONALE: If copyright were not treated as property and fair use as a narrow defense, the entire legal and economic structure of creative industries would collapse. Content creators would lose their primary means of monetization, leading to a fundamental reorganization of how creative works are produced, distributed, and valued, likely shifting towards alternative funding models or public patronage.
% FOUNDING_PROBLEM: To incentivize the creation and dissemination of useful works by granting authors exclusive rights for a limited time, balancing public access with private reward, and preventing free-riding that would undermine creative industries.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and content licensors assert the problem is still live, citing the need for continued incentives in a digital age. However, many legal scholars, economists, and public interest groups outside these benefiting parties argue that the problem is over-served by current copyright terms and interpretations, leading to rent-seeking rather than optimal innovation.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the significant economic value transferred from potential users to copyright holders through licensing, driven by the narrow scope of fair use. Suppression (0.70) is high due to the legal uncertainty and litigation risk that discourages many potentially fair uses. The low theater ratio (0.15) indicates that the enforcement of this reading is highly functional, with real legal and economic consequences, rather than merely performative. The increasing extractiveness and suppression over time reflect the hardening of this interpretation in response to digital technologies that facilitate widespread copying and reuse.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of copyright holders, this reading is a necessary 'rope' that coordinates property rights and incentivizes creation. From the perspective of reusers and innovators, it functions more like a 'snare' or 'tangled_rope,' extracting value and suppressing innovation under the guise of protecting creators. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders and content licensors are clear beneficiaries, as this reading directly enhances their control and revenue streams. Content creators/reusers and innovators are targets, bearing the costs of licensing or litigation risk. Public domain advocates are excluded, as their perspective is often marginalized in the legal interpretation that favors property rights. Courts act as agenda-setters by shaping the interpretation, but also as observers in the broader legal landscape.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent interpretation of fair use, or is it merely a strategic framing by copyright holders to maximize extraction?',
    'Analysis of judicial opinions and legislative history to determine the extent to which this reading is grounded in legal principles versus economic lobbying efforts.',
    'If primarily strategic, the constraint''s effective extractiveness is higher, and its coordination function is more theatrical; if genuinely principled, the extraction is a byproduct of a legitimate (though contested) legal philosophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''narrow_defense_reading'' of the ''fair_use_statutory_exception'' kernel.').

omega_variable(
    market_harm_definition_ambiguity,
    'How broadly should ''market harm'' be defined in fair use analysis? Does it include hypothetical markets that copyright holders *could* enter, or only established, existing markets?',
    'Judicial clarification or legislative amendment providing specific criteria for assessing market harm, distinguishing between actual and speculative markets.',
    'A broad definition of market harm increases extractiveness and suppression for reusers; a narrow definition would reduce it, shifting the balance towards more permissive fair use.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_definition_ambiguity, empirical, 'Ambiguity in the ''market harm'' factor of fair use analysis.').

omega_variable(
    transformative_use_weighting,
    'What is the appropriate weight to give to the ''transformativeness'' factor in fair use analysis, relative to the ''commercial nature'' and ''market harm'' factors?',
    'Judicial precedent that explicitly elevates the importance of transformativeness, or legislative guidance that clarifies its role in promoting innovation and cultural discourse.',
    'If transformativeness is underweighted, extractiveness for reusers remains high; if it is given greater weight, more uses would be deemed fair, reducing extraction and fostering innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_weighting, preference, 'Ambiguity in balancing transformativeness against commerciality in fair use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fair_tr_t6, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 6, 0.12).
narrative_ontology:measurement(fair_tr_t12, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(fair_tr_t18, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement(fair_tr_t24, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(fair_tr_t30, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(fair_be_t6, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(fair_be_t12, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(fair_be_t18, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 18, 0.72).
narrative_ontology:measurement(fair_be_t24, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(fair_be_t30, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fair_su_t6, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(fair_su_t12, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(fair_su_t18, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(fair_su_t24, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(fair_su_t30, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, digital_rights_management_enforcement).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, content_licensing_markets).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception__market_licensing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'fair_use_statutory_exception' kernel, each with different ε values and structural implications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
