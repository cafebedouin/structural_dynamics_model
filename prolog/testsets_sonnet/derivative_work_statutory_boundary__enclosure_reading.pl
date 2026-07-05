% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Any-Use-Constitutes-Derivative-Work Enclosure Reading of the Derivative Work Boundary
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This story instantiates the enclosure reading of the contested
 *   derivative-work statutory boundary kernel: the position that any use of
 *   copyrighted expression in the process of creating new work — not just
 *   fixed, substantially incorporating final products — already constitutes
 *   preparation of a derivative work. This reading has expanded via
 *   litigation strategy and licensing-market lobbying over the last two
 *   decades, particularly as generative AI training and digital sampling
 *   created new surfaces for pre-creation liability claims. The coordination
 *   reading (only fixed, substantially incorporating recastings count) and
 *   the hybrid carveout reading (commercial/non-commercial split) are
 *   separate constraints, not alternate measurements of this one — each has
 *   its own stable epsilon and its own stakeholder structure, linked here
 *   only for network/family purposes.
 *
 * KEY AGENTS:
 *   - major_content_licensors: institutional beneficiary and agenda-setter, monetizes the broad boundary through pre-clearance licensing
 *   - independent_creators: powerless, trapped payer, cannot afford licensing counsel, absorbs suppression directly
 *   - ai_model_developers_without_licensing_capital: moderate power payer, foreclosed from the field absent capital for blanket licenses
 *   - legislative_and_judicial_observers: institutional analytical seat, could narrow the standard but faces resourcing asymmetry in the cases reaching them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.81).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.78).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Any-Use-Constitutes-Derivative-Work Enclosure Reading of the Derivative Work Boundary").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '1439f4f4-197f-4a04-9481-e11766d47801').
narrative_ontology:cs_kernel_codification('1439f4f4-197f-4a04-9481-e11766d47801', fixed_text).
narrative_ontology:cs_authority_grounding('1439f4f4-197f-4a04-9481-e11766d47801', extraction).
narrative_ontology:cs_interpretation_layer_present('1439f4f4-197f-4a04-9481-e11766d47801').
narrative_ontology:cs_reading_relation('1439f4f4-197f-4a04-9481-e11766d47801', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('1439f4f4-197f-4a04-9481-e11766d47801', derivative_work_statutory_boundary__hybrid_carveout_reading, influences).
narrative_ontology:cs_axiom('1439f4f4-197f-4a04-9481-e11766d47801', foundational, use_during_creation_is_preparation_regardless_of_transformation).
narrative_ontology:cs_axiom_status(use_during_creation_is_preparation_regardless_of_transformation, holdable).
narrative_ontology:cs_axiom_grounding('1439f4f4-197f-4a04-9481-e11766d47801', use_during_creation_is_preparation_regardless_of_transformation, conventional).
narrative_ontology:cs_axiom('1439f4f4-197f-4a04-9481-e11766d47801', secondary, market_harm_irrelevant_to_liability_trigger).
narrative_ontology:cs_axiom_status(market_harm_irrelevant_to_liability_trigger, holdable).
narrative_ontology:cs_axiom_grounding('1439f4f4-197f-4a04-9481-e11766d47801', market_harm_irrelevant_to_liability_trigger, instrumental).
narrative_ontology:cs_reference_frame('1439f4f4-197f-4a04-9481-e11766d47801', market_substitution_prevention_standard).
narrative_ontology:cs_drift_state('1439f4f4-197f-4a04-9481-e11766d47801', post_digital_licensing_market_maturation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1439f4f4-197f-4a04-9481-e11766d47801', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, major_content_licensors).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_aggregators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, litigation_specialist_law_firms).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, independent_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, fan_communities).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, ai_model_developers_without_licensing_capital).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, archivists_and_researchers).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, strong_author_control_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold large back-catalog rights portfolios and litigate aggressively to establish that any incorporation of copyrighted expression during the creation process — training, sampling, reference, quotation used as scaffolding — is itself preparation of a derivative work, triggering liability before any final product exists. They monetize this reading through mandatory pre-clearance licensing programs and settlement extraction, and lobby to keep the standard broad because a narrower coordination reading would eliminate most of their leverage.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, major_content_licensors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, major_content_licensors, agenda_setter).

% Operate licensing clearinghouses that sell blanket permissions for the broad range of activity this reading sweeps into liability. Their business model depends on the boundary being wide: the wider the definition of 'preparation of derivative work,' the larger the addressable market for their clearance products.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_aggregators, beneficiary,
    institutional, generational, arbitrage, global).

% Build practice groups around enforcement actions premised on the broad boundary. They advise licensors on cease-and-desist and pre-litigation demand strategies that rely on the low threshold for triggering derivative-work liability, and their fee structures scale with the volume of enforceable claims the broad reading generates.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, litigation_specialist_law_firms, beneficiary,
    organized, biographical, arbitrage, national).

% Write fan fiction, remix video, sample-based music, or otherwise build new work using fragments of existing copyrighted expression as raw material. Under this reading, that act of use during creation is already the derivative work's preparation, regardless of how transformed the final output is. They cannot afford licensing counsel or pre-clearance fees, so most either self-censor, work underground and risk takedown, or abandon projects entirely.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, independent_creators, payer,
    powerless, biographical, trapped, national).

% Produce and circulate transformative fan works as a form of community participation, not commercial competition. The enclosure reading gives rightsholders standing to demand takedowns or licensing fees for any incorporation of source material regardless of transformative intent or absence of market harm, collapsing the space these communities previously occupied informally.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, fan_communities, payer,
    powerless, biographical, trapped, global).

% Train or fine-tune models using text, image, or audio corpora that include copyrighted expression. Under this reading, the training process itself — any use of copyrighted expression during creation — is preparation of a derivative work, exposing them to liability before a single output is generated. Only firms with capital to negotiate blanket licenses with major aggregators can operate; smaller developers are foreclosed from the field entirely.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, ai_model_developers_without_licensing_capital, payer,
    moderate, biographical, constrained, global).

% Build annotated corpora, critical editions, and computational research datasets that necessarily quote or incorporate copyrighted material during construction. The broad boundary treats the incorporation itself as derivative-work preparation, making standard scholarly and preservation practices legally precarious absent institutional licensing budgets most research libraries lack.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, archivists_and_researchers, payer,
    moderate, generational, constrained, national).

% Courts and legislatures periodically revisit the derivative-work standard through fair-use litigation and copyright reform proposals. They hear testimony from all sides and can narrow or widen the operative boundary, but incumbent licensors' resource advantage in litigation shapes which cases reach precedent-setting courts.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, legislative_and_judicial_observers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__enclosure_reading, major_content_licensors).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its strongest form, the boundary purports to coordinate a licensing market: rightsholders can predict and monetize any use of their expression, and downstream creators know in advance what requires a license, reducing negotiation costs relative to case-by-case litigation over transformation and market harm.
% TRANSFER_FUNCTION: Moves control over new-work creation from independent creators, fan communities, smaller AI developers, and researchers to major licensors and the aggregators/law firms that monetize enforcement — via mandatory licensing fees, settlement payments, and abandoned or suppressed projects that never reach the market at all.
% ABSENT_VOICES: Independent creators and fan communities have no organized lobbying presence comparable to major licensors' trade associations; open-source AI developers and library/archive coalitions raise objections in comment periods and amicus briefs but are structurally outmatched in litigation resourcing and lobbying access to the legislative bodies that could narrow the standard.
% DISAPPEARANCE_RATIONALE: If the any-use-constitutes-preparation standard were replaced by a narrower coordination reading, licensing clearinghouses' addressable market would shrink sharply, enforcement litigation volume would drop, and a large volume of currently-suppressed transformative work, sampling, AI training activity, and archival practice would become legally viable overnight — the boundary is doing active suppressive work, not merely describing a fixed feature of authorship.
% FOUNDING_PROBLEM: Copyright's derivative-work right was built to prevent someone from taking a copyrighted work, making superficial changes, and selling it as a substitute that captures the original author's market without authorization or payment.
% FOUNDING_PROBLEM_CORROBORATION: Major licensors and their trade associations attest the broad standard is necessary to prevent unauthorized exploitation at any stage of production. Independent creator advocacy groups, digital rights organizations, library and archive coalitions, and a substantial body of law-review commentary from scholars outside the licensing industry attest that the founding problem — market-substitutive copying — is narrow, while the enclosure reading has expanded far beyond it to capture non-substitutive, non-competing transformative and preparatory activity that poses no market harm to the original.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored high (0.81 at interval end) because the enclosure reading captures value far beyond the founding market-substitution problem — it monetizes preparatory activity with no market harm. Suppression is authored high (0.78) because persistence depends on active enforcement: cease-and-desist campaigns, pre-litigation demand letters, and platform takedown regimes that operate whether or not a final work would ever compete with the original. Theater ratio is kept moderate-low (0.28) because the enforcement machinery is largely functional extraction, not performance — the licensing fees and settlements are real transfers, not symbolic gestures. Accessibility collapse (0.62) reflects that alternatives (proceeding without a license, relying on fair use as an affirmative defense) technically exist but collapse in practice once litigation risk and defense costs are internalized by risk-averse creators and platforms. Resistance (0.58) is substantial: creator advocacy, digital rights organizations, and open-model developers actively contest the standard in courts, comment periods, and public discourse, but without matching institutional resources.
 *
 * DIRECTIONALITY LOGIC:
 *   Major content licensors, aggregators, and specialist law firms sit near the full-beneficiary end: they set the operative standard through litigation strategy, collect licensing revenue and settlement payments, and have arbitrage-grade exit (they choose which jurisdictions and forums to litigate in). Independent creators and fan communities sit near the full-target end: trapped exit options, no resources to negotiate around the boundary, and the broad standard directly suppresses their existing practice. AI developers and archivists occupy an intermediate position — moderate power, constrained rather than trapped exit — because some can negotiate licenses or route around the standard through jurisdiction shopping, but smaller entities cannot.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing market-substitutive copying) is largely solved by existing infringement law covering fixed, competing derivative products; the enclosure reading's expansion to preparatory and non-substitutive uses persists not because the founding problem is live at that scope but because incumbent licensors have built revenue infrastructure around the broad standard. This is the classic mandatrophy signature: the mandate (protecting authors from market substitution) has been decoupled from the mechanism (liability for any preparatory use), and the mechanism now serves the businesses built to administer it rather than the harm it was designed to prevent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enclosure_vs_coordination_kernel_ambiguity,
    'Does the statutory derivative-work right, properly construed, actually extend liability to preparatory and non-fixed uses of copyrighted expression, or does the enclosure reading represent an extension beyond the kernel''s coordination content driven by incumbent litigation strategy?',
    'Comparative doctrinal analysis across jurisdictions that have ruled on preparatory/training-stage liability versus fixed-recasting liability; tracking which reading prevails in appellate precedent over the coming decade would resolve which reading the kernel actually supports as controlling law.',
    'If courts converge on the coordination reading, the enclosure reading''s current enforcement basis collapses and this constraint''s effective extraction should be reassessed downward sharply; if courts entrench the enclosure reading, its extraction is durable and the sibling readings become the contested minority position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enclosure_vs_coordination_kernel_ambiguity, conceptual, 'Whether the kernel''s controlling content is the enclosure reading or the coordination reading — a live doctrinal fight, not a settled fact this story can assume.').

omega_variable(
    training_use_market_harm_empirics,
    'Does the preparatory use this reading captures (e.g., AI training, sampling, reference incorporation) cause the kind of market substitution the derivative-work right was built to prevent, or is market harm from preparatory use empirically negligible compared to harm from fixed competing derivative products?',
    'Empirical market studies comparing revenue impact on original rightsholders from preparatory/training uses versus fixed substitutive derivative products; expert economic testimony in ongoing litigation.',
    'If preparatory use causes negligible market harm, the enclosure reading''s extraction is decoupled from any coordination function and the constraint is purely extractive; if preparatory use is shown to cause substantial harm, some portion of the measured extraction would be recharacterized as legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(training_use_market_harm_empirics, empirical, 'Whether the harm this reading targets is real market substitution or a constructed liability surface.').

omega_variable(
    sibling_reading_relative_naturalness,
    'Among the three declared readings of the derivative_work_statutory_boundary kernel, is the enclosure reading a genuine, if aggressive, interpretation of existing statutory text, or is it a post-hoc extraction strategy retrofitted onto ambiguous language that could equally support the coordination or hybrid readings?',
    'Legislative history analysis of the statutory language''s original drafting intent, cross-referenced against contemporaneous case law at time of enactment versus current enforcement practice.',
    'If the enclosure reading is shown to be a drift from original legislative intent rather than a faithful reading, it strengthens the case for judicial or legislative correction toward the coordination or hybrid readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_relative_naturalness, conceptual, 'Whether this reading is a faithful interpretation or a strategic drift from the kernel''s original content.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(deri_tr_t4, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(deri_tr_t8, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(deri_tr_t12, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(deri_tr_t16, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(deri_be_t4, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(deri_be_t8, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 8, 0.67).
narrative_ontology:measurement(deri_be_t12, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement(deri_be_t16, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 16, 0.76).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 20, 0.79).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 24, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(deri_su_t4, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(deri_su_t8, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(deri_su_t12, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement(deri_su_t16, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 16, 0.73).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 20, 0.76).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 24, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(derivative_work_statutory_boundary__enclosure_reading, 0.1).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the derivative_work_statutory_boundary kernel. coordination_reading models the narrow fixed-recasting standard (likely rope/scaffold, low-to-moderate extraction). hybrid_carveout_reading models the commercial/non-commercial split (likely tangled_rope, moderate extraction). This file, enclosure_reading, models the broadest standard and carries the highest authored extraction of the three. All three share the same statutory text as their contested kernel but instantiate structurally distinct constraints with distinct beneficiary/victim sets, distinct epsilon values, and distinct classifications — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
