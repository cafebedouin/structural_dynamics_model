% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Commons Reading)
 *   domain: software licensing / intellectual property / open source governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'copyleft as commons' reading of
 *   the GPL reciprocity obligation kernel. Under this reading, the GPL is
 *   institutional technology that solves the free-rider problem in digital
 *   public goods by mandating reciprocity: anyone who distributes derivative
 *   works must provide source code under the same terms. The beneficiary is
 *   the commons as institution â the sustainable, non-enclosable pool of
 *   software â while the victim is the individual or firm seeking to exit
 *   the reciprocity loop through proprietary appropriation. The reading
 *   treats the extraction as the price of commons preservation, not as an end
 *   in itself. Sibling readings (freedom-frame, restriction-frame) are
 *   structurally related but not adjudicated here.
 *
 * KEY AGENTS:
 *   - copyleft_community: Primary beneficiary (organized/identity_locked) â receives the flow of reciprocated source and assurance against enclosure
 *   - proprietary_adopters: Primary target (moderate/constrained) â bears the cost of foregone proprietary optionality
 *   - copyleft_enforcers: Agenda setter (organized/mobile) â administers license interpretation and enforcement
 *   - commons_users: Secondary beneficiary (moderate/mobile) â draws from the commons without necessarily contributing
 *   - permissive_advocates: Excluded voice (organized/mobile) â contests the necessity of mandatory reciprocity from outside the license framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.55).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.5).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation (Copyleft as Commons Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software licensing / intellectual property / open source governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'b7b22dae-9124-4646-8f8a-1248a2da1b3d').
narrative_ontology:cs_kernel_codification('b7b22dae-9124-4646-8f8a-1248a2da1b3d', formalized).
narrative_ontology:cs_authority_grounding('b7b22dae-9124-4646-8f8a-1248a2da1b3d', lineage).
narrative_ontology:cs_interpretation_layer_present('b7b22dae-9124-4646-8f8a-1248a2da1b3d').
narrative_ontology:cs_reading_relation('b7b22dae-9124-4646-8f8a-1248a2da1b3d', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7b22dae-9124-4646-8f8a-1248a2da1b3d', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('b7b22dae-9124-4646-8f8a-1248a2da1b3d', foundational, mandatory_reciprocity_sustains_commons).
narrative_ontology:cs_axiom_status(mandatory_reciprocity_sustains_commons, holdable).
narrative_ontology:cs_axiom_grounding('b7b22dae-9124-4646-8f8a-1248a2da1b3d', mandatory_reciprocity_sustains_commons, instrumental).
narrative_ontology:cs_axiom('b7b22dae-9124-4646-8f8a-1248a2da1b3d', foundational, enclosure_risk_in_digital_goods).
narrative_ontology:cs_axiom_status(enclosure_risk_in_digital_goods, holdable).
narrative_ontology:cs_axiom_grounding('b7b22dae-9124-4646-8f8a-1248a2da1b3d', enclosure_risk_in_digital_goods, empirically_contingent).
narrative_ontology:cs_reference_frame('b7b22dae-9124-4646-8f8a-1248a2da1b3d', reciprocal_commons).
narrative_ontology:cs_drift_state('b7b22dae-9124-4646-8f8a-1248a2da1b3d', contemporary_permissive_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b7b22dae-9124-4646-8f8a-1248a2da1b3d', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_community).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_users).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_adopters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developers and institutions committed to preserving a reciprocal software commons. They contribute code under GPL terms and benefit from a growing pool of improvements that cannot be enclosed by proprietary actors. Participation is tied to ideological commitment to commons governance; exit would mean abandoning the communal project and its reciprocal guarantees.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_community, beneficiary,
    organized, generational, identity_locked, global).

% End users and downstream developers who draw software from the copyleft commons without necessarily contributing. They benefit from the assurance that derivatives remain open and inspectable. They can exit to proprietary or permissive alternatives but lose the guarantee against enclosure.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_users, beneficiary,
    moderate, biographical, mobile, global).

% Developers and firms seeking to integrate GPL-licensed code into proprietary products or services without distributing corresponding source. They bear the cost of either refraining from use, reimplementing functionality, or releasing their own source under GPL. Their exit is constrained by copyright enforcement backing the license.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_adopters, payer,
    moderate, biographical, constrained, global).

% Copyright holders and organizations that enforce GPL terms through legal action and compliance engineering. They set interpretive boundaries and initiate enforcement. They could choose reduced enforcement or relicensing, though multi-contributor projects face collective-action barriers.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_enforcers, agenda_setter,
    organized, generational, mobile, global).

% Advocates for MIT, BSD, and other permissive licenses who argue that commons preservation does not require mandatory reciprocity. They are structurally excluded from GPL license design but present in broader discourse; their empirical counterexamples challenge the necessity of the constraint.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, permissive_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents enclosure of a shared software commons by requiring that distributed derivatives remain under the same license terms, solving the free-rider problem in digital public goods.
% TRANSFER_FUNCTION: Moves source code disclosure obligations and licensing terms from derivative-work distributors to the commons, enforced via copyright leverage.
% ABSENT_VOICES: Permissive license advocates who argue commons preservation does not require mandatory reciprocity; proprietary software advocates who view source disclosure as an uncompensated taking. They are present in broader discourse but structurally excluded from GPL license design.
% DISAPPEARANCE_RATIONALE: Without the reciprocity obligation, GPL-licensed code could be integrated into proprietary systems without source disclosure, leading to enclosure of the commons. The licensing landscape would shift toward permissive or proprietary models, and the institutional mechanism preserving reciprocal access would collapse.
% FOUNDING_PROBLEM: The tragedy of the commons in software: code released without reciprocity requirements could be freely appropriated, improved privately, and redistributed without source, leading to depletion of the shared codebase and free-rider capture.
% FOUNDING_PROBLEM_CORROBORATION: Commons governance scholars in the Ostrom tradition and Free Software advocates attest to the enclosure risk. However, empirical successes of large permissive-licensed commons are cited by outside observers to argue the founding problem is overstated or solvable through non-reciprocal means.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is medium (0.55) because the constraint genuinely withholds a valuable option (proprietary enclosure) from adopters, but this withholding is inseparable from the coordination mechanism that preserves the commons. Suppression is moderate (0.50) because persistence depends on copyright enforcement and compliance infrastructure rather than raw force. Theater is low (0.20) because the reciprocity mechanism is largely functional, though some compliance activity is ritualized. Accessibility collapse is moderate (0.40) because alternatives (permissive licenses, proprietary reimplementation, non-use) remain visible and viable. Resistance is moderate (0.45) because proprietary adopters and permissive advocates actively contest the model.
 *
 * PERSPECTIVAL GAP:
 *   The copyleft community seat experiences the constraint as protective infrastructure; the proprietary adopter seat experiences it as a forced transfer of strategic optionality. The engine computes this divergence from structural position â the coordination function is real from the beneficiary side and costly from the payer side.
 *
 * DIRECTIONALITY LOGIC:
 *   The copyleft_community and commons_users sit near the beneficiary end (low d): they receive the flow of reciprocated source code and the assurance against enclosure. Proprietary_adopters sit near the target end (high d): they lose the option to proprietize derivatives. Copyleft_enforcers sit ambiguously â they administer the constraint and could be captured, but structurally they align with the beneficiary side.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both coordination (commons preservation) and asymmetric extraction (proprietary adopters bear costs). If the coordination function were absent, it would be a snare; if extraction were absent, it would be a rope. The founding problem â digital commons enclosure â is contested but not dead, and the mechanism is actively enforced, preventing piton classification despite low theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permissive_commons_viability,
    'Does a sustainable software commons require mandatory reciprocity, or can permissive licensing achieve equivalent preservation?',
    'Longitudinal comparative study of codebase enclosure rates and contributor retention across matched GPL and permissive projects, controlling for project age and domain.',
    'If permissive commons prove equally durable, the coordination justification for GPL''s extraction collapses, pushing classification toward snare. If GPL commons show superior longevity and lower enclosure, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permissive_commons_viability, empirical, 'Whether mandatory reciprocity is necessary for commons preservation.').

omega_variable(
    enforcement_as_extraction,
    'Does selective GPL enforcement by copyright holders extract benefits beyond the coordination function of commons preservation?',
    'Audit of enforcement settlements and compliance actions: whether gains flow to community infrastructure or to individual copyright holders, and whether enforcement targets are disproportionately wealthy or foreign actors.',
    'If enforcement primarily enriches specific holders rather than the commons, the beneficiary structure misaligns with the coordination story, indicating extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_as_extraction, empirical, 'Whether enforcement machinery extracts for private benefit.').

omega_variable(
    commons_beneficiary_ambiguity,
    'Is the copyleft community the true beneficiary, or does the commons frame obscure benefits accruing to specific enforcement organizations?',
    'Financial and organizational network analysis tracing resource flows from enforcement actions to community infrastructure versus concentrated organizations.',
    'If gains concentrate in enforcement organizations, the directionality logic shifts and the false-summit or snarish character increases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_beneficiary_ambiguity, conceptual, 'Ambiguity in who actually captures the commons benefit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(gpl__tr_t7, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 7, 0.08).
narrative_ontology:measurement(gpl__tr_t14, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 14, 0.12).
narrative_ontology:measurement(gpl__tr_t21, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 21, 0.15).
narrative_ontology:measurement(gpl__tr_t28, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 28, 0.18).
narrative_ontology:measurement(gpl__tr_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 35, 0.2).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gpl__be_t7, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 7, 0.45).
narrative_ontology:measurement(gpl__be_t14, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 14, 0.52).
narrative_ontology:measurement(gpl__be_t21, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 21, 0.55).
narrative_ontology:measurement(gpl__be_t28, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 28, 0.55).
narrative_ontology:measurement(gpl__be_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 35, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gpl__su_t7, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 7, 0.35).
narrative_ontology:measurement(gpl__su_t14, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 14, 0.48).
narrative_ontology:measurement(gpl__su_t21, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 21, 0.52).
narrative_ontology:measurement(gpl__su_t28, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 28, 0.5).
narrative_ontology:measurement(gpl__su_t35, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 35, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
