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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense (Property-Centric Reading)
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.78).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.85).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense (Property-Centric Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property_law/legal_interpretation/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, 'de126399-67c8-4529-8f4f-69539be920fe').
narrative_ontology:cs_kernel_codification('de126399-67c8-4529-8f4f-69539be920fe', fixed_text).
narrative_ontology:cs_authority_grounding('de126399-67c8-4529-8f4f-69539be920fe', lineage).
narrative_ontology:cs_interpretation_layer_present('de126399-67c8-4529-8f4f-69539be920fe').
narrative_ontology:cs_reading_relation('de126399-67c8-4529-8f4f-69539be920fe', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('de126399-67c8-4529-8f4f-69539be920fe', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('de126399-67c8-4529-8f4f-69539be920fe', foundational, copyright_as_exclusive_property).
narrative_ontology:cs_axiom_status(copyright_as_exclusive_property, holdable).
narrative_ontology:cs_axiom_grounding('de126399-67c8-4529-8f4f-69539be920fe', copyright_as_exclusive_property, deontological).
narrative_ontology:cs_axiom('de126399-67c8-4529-8f4f-69539be920fe', foundational, fair_use_as_limited_exception).
narrative_ontology:cs_axiom_status(fair_use_as_limited_exception, holdable).
narrative_ontology:cs_axiom_grounding('de126399-67c8-4529-8f4f-69539be920fe', fair_use_as_limited_exception, conventional).
narrative_ontology:cs_reference_frame('de126399-67c8-4529-8f4f-69539be920fe', property_rights_framework).
narrative_ontology:cs_drift_state('de126399-67c8-4529-8f4f-69539be920fe', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('de126399-67c8-4529-8f4f-69539be920fe', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, licensing_agencies).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, content_creators_reusers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, public_domain_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own the exclusive rights to copyrighted works. They actively monitor for infringement and pursue legal action to protect their market value, viewing fair use as a limited exception to their property rights. They benefit directly from the constraint's enforcement.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, copyright_holders, agenda_setter,
    institutional, generational, arbitrage, global).

% Represent copyright holders and facilitate the licensing of works. Their business model thrives on the clear definition of copyright as property and the narrow interpretation of fair use, as it expands the scope of licensable uses.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, licensing_agencies, beneficiary,
    organized, biographical, mobile, global).

% Seek to incorporate existing copyrighted material into new works (e.g., remixes, commentary, parody). Under this reading, they face significant legal risk and the burden of proving fair use, often leading to self-censorship or costly licensing fees. Their creative output is constrained by the fear of litigation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, content_creators_reusers, payer,
    moderate, biographical, constrained, global).

% Utilize copyrighted materials for teaching and research. The narrow interpretation of fair use increases their legal exposure and administrative burden, often requiring them to purchase licenses for uses that might otherwise be considered fair, or to limit access to educational resources.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, educational_institutions, payer,
    organized, generational, constrained, national).

% Argue for a robust public domain and broad rights for cultural reuse. This reading of fair use directly opposes their goals by prioritizing private property rights over public access and transformative creativity, effectively excluding their perspective from shaping the core interpretation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, public_domain_advocates, excluded,
    organized, civilizational, constrained, global).

% Interpret and apply copyright law, including the fair use doctrine. Under this reading, they tend to emphasize the market impact of a use and place a high burden on defendants to prove fair use, reinforcing the property-centric view.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear property rights for creators, providing an economic incentive for the production of original works by allowing them to control and profit from their creations.
% TRANSFER_FUNCTION: Transfers potential market value from unauthorized uses (even those with some public benefit) to copyright holders, by requiring licensing or deterring uses that might otherwise occur.
% ABSENT_VOICES: Transformative creators, public domain advocates, and those who prioritize cultural remixing and innovation would argue for a broader, more flexible interpretation of fair use, but their perspectives are often marginalized in legal discourse dominated by property rights.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the incentive structure for creative works would fundamentally shift. Copyright holders would lose significant control and revenue, leading to a reorganization of creative industries and potentially a different landscape of cultural production and access.
% FOUNDING_PROBLEM: To incentivize the creation and dissemination of original works by granting authors exclusive rights for a limited time, preventing free-riding and ensuring creators could profit from their labor.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, industry associations, and many individual creators attest that incentivizing creation remains a live problem. However, critics (e.g., public domain advocates) argue that the current interpretation over-solves the problem, leading to over-protection and stifled creativity.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_value_definition_ambiguity,
    'How is ''market value'' (or ''potential market value'') precisely defined and measured in fair use analysis, and does this definition accurately reflect actual economic harm or merely potential licensing opportunities?',
    'Empirical studies on actual market displacement versus hypothetical licensing markets, and judicial clarification on the weight given to each in fair use factors.',
    'If ''potential market value'' is broadly construed to include any hypothetical licensing opportunity, it amplifies extractiveness. A narrower definition focusing on actual market harm would reduce extractiveness and suppression for reusers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_value_definition_ambiguity, conceptual, 'Ambiguity in defining market harm vs. licensing opportunity in fair use.').

omega_variable(
    transformative_use_weighting,
    'To what extent should the ''transformative'' nature of a new work outweigh its commercial nature or potential market impact in fair use analysis?',
    'Judicial precedent that explicitly rebalances the fair use factors, giving greater weight to transformativeness, or legislative reform clarifying the intent of fair use.',
    'Increased weighting of transformativeness would reduce extractiveness and suppression for creators of new, transformative works, potentially shifting the constraint towards a more Rope-like classification for those specific uses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_weighting, preference, 'The relative weight of transformativeness vs. market impact in fair use.').

omega_variable(
    kernel_reading_impact,
    'This constraint is the ''narrow_defense_reading'' of the fair_use_statutory_exception kernel. How would the constraint''s structural properties (extractiveness, suppression, beneficiaries, victims) change if the ''transformative_right_reading'' or ''market_licensing_reading'' were adopted?',
    'Comparative legal analysis of judicial outcomes under different interpretive regimes, or counterfactual modeling of legislative changes.',
    'The ''transformative_right_reading'' would likely decrease extractiveness and suppression for reusers, while the ''market_licensing_reading'' would likely increase both, potentially pushing the constraint closer to a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_impact, conceptual, 'Impact of alternative kernel readings on constraint structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1980, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(fair_tr_t1990, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(fair_tr_t2000, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(fair_tr_t2010, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(fair_tr_t2020, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(fair_be_t1980, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(fair_be_t1990, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(fair_be_t2000, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(fair_be_t2010, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2010, 0.77).
narrative_ontology:measurement(fair_be_t2020, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2020, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1980, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(fair_su_t1990, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(fair_su_t2000, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(fair_su_t2010, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(fair_su_t2020, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2020, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
