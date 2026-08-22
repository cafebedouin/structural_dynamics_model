% ============================================================================
% CONSTRAINT STORY: software_source_status__property_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__property_rights_reading, []).

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
 *   constraint_id: software_source_status__property_rights_reading
 *   human_readable: Software Source Status: Proprietary Property Rights Reading
 *   domain: software_engineering/political_economy/intellectual_property
 *
 * SUMMARY:
 *   This constraint instantiates the property_rights_reading of the contested
 *   software_source_status kernel. Under this reading, source code is a
 *   proprietary asset analogous to physical property; creators possess a
 *   legitimate moral and legal right to restrict access, modification, and
 *   redistribution through licensing. Users are positioned as consumers with
 *   purely contractual rights, and the state enforces these exclusions
 *   through copyright law, trade-secret doctrine, and anti-circumvention
 *   statutes. The reading presents itself as natural and necessary market
 *   infrastructure, but structurally depends on active enforcement against
 *   sharing and modification.
 *
 * KEY AGENTS:
 *   - Commercial rights holders (agenda_setter/beneficiary, institutional/arbitrage) â administer licenses and capture monopoly rents.
 *   - Software users (payer, organized/constrained) â bear access restrictions and pay licensing costs.
 *   - Independent modifiers (payer, moderate/constrained) â bear legal and technical barriers to modification.
 *   - Free software advocates (excluded, organized/mobile) â reject the property framing but are outside the licensing conversation.
 *   - Policy analysts (observer, analytical) â measure effects without being shaped by the revenue flow.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__property_rights_reading, 0.72).
domain_priors:suppression_score(software_source_status__property_rights_reading, 0.8).
domain_priors:theater_ratio(software_source_status__property_rights_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(software_source_status__property_rights_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__property_rights_reading, tangled_rope).
narrative_ontology:human_readable(software_source_status__property_rights_reading, "Software Source Status: Proprietary Property Rights Reading").
narrative_ontology:topic_domain(software_source_status__property_rights_reading, "software_engineering/political_economy/intellectual_property").

domain_priors:requires_active_enforcement(software_source_status__property_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__property_rights_reading, 'c4aa1575-c121-4ec4-a816-83a2c560c332').
narrative_ontology:cs_kernel_codification('c4aa1575-c121-4ec4-a816-83a2c560c332', formalized).
narrative_ontology:cs_authority_grounding('c4aa1575-c121-4ec4-a816-83a2c560c332', lineage).
narrative_ontology:cs_interpretation_layer_present('c4aa1575-c121-4ec4-a816-83a2c560c332').
narrative_ontology:cs_reading_relation('c4aa1575-c121-4ec4-a816-83a2c560c332', software_source_status__freedom_imperative_reading, forecloses).
narrative_ontology:cs_reading_relation('c4aa1575-c121-4ec4-a816-83a2c560c332', software_source_status__pragmatic_development_reading, influences).
narrative_ontology:cs_reading_relation('c4aa1575-c121-4ec4-a816-83a2c560c332', software_source_status__utilitarian_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('c4aa1575-c121-4ec4-a816-83a2c560c332', foundational, code_as_exclusively_ownable_asset).
narrative_ontology:cs_axiom_status(code_as_exclusively_ownable_asset, holdable).
narrative_ontology:cs_axiom_grounding('c4aa1575-c121-4ec4-a816-83a2c560c332', code_as_exclusively_ownable_asset, conventional).
narrative_ontology:cs_axiom('c4aa1575-c121-4ec4-a816-83a2c560c332', foundational, creator_right_to_restrict_use).
narrative_ontology:cs_axiom_status(creator_right_to_restrict_use, holdable).
narrative_ontology:cs_axiom_grounding('c4aa1575-c121-4ec4-a816-83a2c560c332', creator_right_to_restrict_use, deontological).
narrative_ontology:cs_reference_frame('c4aa1575-c121-4ec4-a816-83a2c560c332', exclusive_creator_control).
narrative_ontology:cs_drift_state('c4aa1575-c121-4ec4-a816-83a2c560c332', digital_commons_challenge_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c4aa1575-c121-4ec4-a816-83a2c560c332', '').
narrative_ontology:cs_kernel_id(software_source_status__property_rights_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__property_rights_reading, commercial_rights_holders).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, software_users).
narrative_ontology:constraint_victim(software_source_status__property_rights_reading, independent_modifiers).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, intellectual_property_doctrine).
narrative_ontology:constraint_vindicates(software_source_status__property_rights_reading, contractual_sovereignty_over_code).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft software licenses, lobby for copyright term extensions and anti-circumvention law, and treat source code as a trade-secret asset. They collect monopoly rents through licensing fees and control over derivative works. Can pivot to dual-licensing or SaaS if the property frame weakens, but aggressively defend the exclusionary model.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, commercial_rights_holders, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_source_status__property_rights_reading, commercial_rights_holders, beneficiary).

% Purchase licenses granting limited use rights; are contractually barred from reverse engineering, sharing, or modifying the software they depend on for work and communication. Face civil and criminal liability for circumvention. Exit is limited to piracy (high risk) or accepting terms.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, software_users, payer,
    organized, biographical, constrained, global).

% Developers, security researchers, and hobbyists who would adapt or repair software but are blocked by license terms and laws like the DMCA. Their craft depends on access to source code and modification rights, which the property frame criminalizes or contractually forbids.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, independent_modifiers, payer,
    moderate, biographical, constrained, global).

% Maintain that software freedom is an ethical imperative and that proprietary restrictions harm users. They are structurally excluded from the property-rights framework's bargaining table: their preferred terms (copyleft, public domain) are treated as charity or hobbyism rather than legitimate industrial organization.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, free_software_advocates, excluded,
    organized, generational, mobile, global).

% Study the economic and social effects of software IP regimes. They document deadweight loss, innovation blocking, and the spread of open-source alternatives, but do not themselves collect from or pay into the licensing structure.
narrative_ontology:constraint_stakeholder(software_source_status__property_rights_reading, policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__property_rights_reading, commercial_rights_holders).
narrative_ontology:fixing_cost_class(software_source_status__property_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a tradable asset from non-rival information goods, enabling firms to recoup fixed development costs through exclusionary licensing and organized distribution channels.
% TRANSFER_FUNCTION: Moves the right to use, inspect, modify, and redistribute software from the general public and adjacent developers to the commercial rights holder, in exchange for license fees and contractual subordination.
% ABSENT_VOICES: Free software advocates and users who treat code as a public good or speech act are excluded from the property-rights framework; they would object that exclusion is unnecessary and unjust but are not party to the standard-form licenses that instantiate the constraint.
% DISAPPEARANCE_RATIONALE: If proprietary restrictions on software access and modification vanished overnight, the current SaaS and shrink-wrap revenue models would collapse; development would reorganize around services, commons-based peer production, and support contracts. The existing commercial software industry is arranged around the exclusionary right.
% FOUNDING_PROBLEM: How to incentivize costly software creation when digital reproduction is near-zero marginal cost and unrestricted copying threatens investment recovery.
% FOUNDING_PROBLEM_CORROBORATION: Rights holders and industry associations attest the problem is live and require stronger enforcement. Independent economists and empirical studies of open-source production (from outside the beneficiary set) contest that exclusion is necessary, pointing to functioning alternative incentive structures.
narrative_ontology:disappearance_verdict(software_source_status__property_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__property_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__property_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(software_source_status__property_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__property_rights_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__property_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_source_status__property_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_source_status__property_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint extracts control and money from users and adjacent developers to sustain a monopoly rent model; suppression (0.8) is higher because the arrangement would collapse without active legal and technical enforcement (DMCA, DRM, EULAs). Theater ratio (0.42) reflects performative enforcement disproportionate to actual harm prevention. Accessibility collapse (0.70) captures how the property frame makes free alternatives appear illegitimate. Resistance (0.65) reflects the sustained free-software and piracy counter-movements. The measurement series share one time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   The commercial rights holder seat should compute as beneficiary (low directionality, low effective extraction) because the constraint subsidizes their revenue model. The user and modifier seats should compute as targets (high directionality, high effective extraction) because they pay through restricted autonomy and license fees. The free-software advocate seat, though excluded, would compute near full-target if forced into the frame. The divergence between these computed seats is the signal the engine is meant to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (commercial_rights_holders) drive d toward the beneficiary pole for the agenda setter. Victim declarations (software_users, independent_modifiers) drive d toward the target pole for payers. The engine will amplify extraction for the constrained payer seats and damp or invert it for the institutional beneficiary with arbitrage-grade exit. No override is needed because the structural derivation matches the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the constraint as pure coordination (it actively extracts from modifiers and users who receive no proportional benefit) and prevents mislabeling it as pure extraction (it does solve a genuine investment-recovery coordination problem for non-rival goods). The Tangled Rope classification captures that the same legal mechanism coordinates markets and extracts rents simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_naturalness_ambiguity,
    'Is software property a natural extension of creator rights to intangible works, or a purely constructed legal monopoly that would not persist without state enforcement?',
    'Comparative legal-historical analysis of copyright''s contingent origins, plus examination of whether norms of exclusion persist in domains without IP law (e.g., pre-copyright software distribution).',
    'If purely constructed, the constraint''s emerges_naturally flag is false and its legitimacy rests on contingent policy rather than natural right; this would alter the False Summit Mountain assessment if the reading were ever claimed as Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_naturalness_ambiguity, conceptual, 'Whether the property frame is natural or constructed.').

omega_variable(
    extraction_necessity_empirical,
    'Does proprietary exclusivity actually produce more or better software than commons-based production models?',
    'Large-scale econometric comparison of innovation rates, security outcomes, and development velocity across matched proprietary and open-source code bases, controlling for funding level.',
    'If commons-based production matches or exceeds proprietary output, the coordination justification weakens and the constraint shifts toward Snare classification; if proprietary output is substantially superior, the coordination function is vindicated and Tangled Rope remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_necessity_empirical, empirical, 'Whether IP extraction is necessary for production.').

omega_variable(
    committer_sibling_foreclosure,
    'Does the foundational axiom of this reading (legitimate right to restrict) logically foreclose the freedom_imperative_reading, or can they coexist as live positions in different communities?',
    'Logical analysis of whether a single legal framework can consistently treat the same act (restricting code redistribution) as both a legitimate property right and an injustice.',
    'If strict foreclosure holds, the engine will register a non-viable sibling under the commitment system apparatus; if not, the kernel remains a live dispute rather than a contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_sibling_foreclosure, conceptual, 'Logical relationship between property and freedom readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__property_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sssprr_tr_t0, software_source_status__property_rights_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sssprr_tr_t8, software_source_status__property_rights_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(sssprr_tr_t16, software_source_status__property_rights_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(sssprr_tr_t24, software_source_status__property_rights_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(sssprr_tr_t32, software_source_status__property_rights_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(sssprr_tr_t40, software_source_status__property_rights_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(sssprr_be_t0, software_source_status__property_rights_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sssprr_be_t8, software_source_status__property_rights_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(sssprr_be_t16, software_source_status__property_rights_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(sssprr_be_t24, software_source_status__property_rights_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(sssprr_be_t32, software_source_status__property_rights_reading, base_extractiveness, 32, 0.71).
narrative_ontology:measurement(sssprr_be_t40, software_source_status__property_rights_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(sssprr_su_t0, software_source_status__property_rights_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(sssprr_su_t8, software_source_status__property_rights_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(sssprr_su_t16, software_source_status__property_rights_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(sssprr_su_t24, software_source_status__property_rights_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement(sssprr_su_t32, software_source_status__property_rights_reading, suppression_requirement, 32, 0.81).
narrative_ontology:measurement(sssprr_su_t40, software_source_status__property_rights_reading, suppression_requirement, 40, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__property_rights_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_source_status__property_rights_reading, 0.15).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
