% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Constitutive Recognition Requirement for Statehood
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The 1933 Montevideo Convention lists four objective criteria for
 *   statehood — defined territory, permanent population, government, capacity
 *   to enter relations with other states — but international practice has
 *   never operated on those criteria alone. This story instantiates the
 *   constitutive reading: recognition by existing states is not evidence of
 *   statehood but a constitutive precondition of it. Under this reading,
 *   entities that satisfy the objective criteria (Somaliland, Taiwan, Western
 *   Sahara/SADR, various indigenous nations) remain legally unable to access
 *   treaty regimes, UN bodies, and sovereign lending markets until incumbent
 *   states choose to admit them. This is a distinct constraint from the
 *   declaratory reading (where objective satisfaction of criteria IS
 *   statehood, and non-recognition is simply other states acting wrongfully)
 *   and from the hybrid reading (which adds a normative legitimacy layer —
 *   democratic governance, non-aggression — atop the objective criteria).
 *   Each reading produces a different victim set and a different account of
 *   where the extraction, if any, is located; per the ε-invariance principle,
 *   they are authored as three separate constraint files linked through
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.72).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Recognition Requirement for Statehood").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, '14e21a58-5b65-48b2-acf7-8f7b1b0c2855').
narrative_ontology:cs_kernel_codification('14e21a58-5b65-48b2-acf7-8f7b1b0c2855', formalized).
narrative_ontology:cs_authority_grounding('14e21a58-5b65-48b2-acf7-8f7b1b0c2855', distributed).
narrative_ontology:cs_reading_relation('14e21a58-5b65-48b2-acf7-8f7b1b0c2855', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('14e21a58-5b65-48b2-acf7-8f7b1b0c2855', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('14e21a58-5b65-48b2-acf7-8f7b1b0c2855', foundational, recognition_is_constitutive_of_legal_personhood).
narrative_ontology:cs_axiom_status(recognition_is_constitutive_of_legal_personhood, holdable).
narrative_ontology:cs_axiom_grounding('14e21a58-5b65-48b2-acf7-8f7b1b0c2855', recognition_is_constitutive_of_legal_personhood, conventional).
narrative_ontology:cs_axiom('14e21a58-5b65-48b2-acf7-8f7b1b0c2855', secondary, incumbent_state_consent_required_for_new_membership).
narrative_ontology:cs_axiom_status(incumbent_state_consent_required_for_new_membership, holdable).
narrative_ontology:cs_axiom_grounding('14e21a58-5b65-48b2-acf7-8f7b1b0c2855', incumbent_state_consent_required_for_new_membership, conventional).
narrative_ontology:cs_reference_frame('14e21a58-5b65-48b2-acf7-8f7b1b0c2855', westphalian_incumbent_veto).
narrative_ontology:cs_drift_state('14e21a58-5b65-48b2-acf7-8f7b1b0c2855', post_cold_war_multipolar_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('14e21a58-5b65-48b2-acf7-8f7b1b0c2855', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_recognized_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, permanent_security_council_members).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, regional_hegemons).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_de_facto_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, secessionist_populations).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, stateless_indigenous_nations).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, state_sovereignty_as_club_membership).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, westphalian_order_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively hold the power to admit or refuse a new entrant to the community of states through bilateral recognition, UN membership votes, and treaty admission. They set the informal and formal criteria applied to claimants and can withhold recognition indefinitely without needing to justify the refusal on the same objective terms they apply to themselves.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, existing_recognized_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Hold veto power over UN membership, which is the practical gateway to near-universal recognition. Use recognition decisions as leverage in geopolitical disputes (e.g. conditioning recognition on alignment, resource access, or resolution of territorial disputes favorable to their interests). Bear no symmetrical cost when they withhold recognition from a claimant.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, permanent_security_council_members, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, permanent_security_council_members, beneficiary).

% Benefit from the recognition regime because it lets them block secessions or breakaway regions within their own sphere by coordinating non-recognition among neighbors and allies, preserving territorial configurations favorable to their regional position.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, regional_hegemons, beneficiary,
    powerful, generational, mobile, regional).

% Function as states internally — they administer territory, provide services, maintain a population and a government — but cannot access international treaties, cannot join the UN or its specialized agencies, cannot borrow from IMF/World Bank on sovereign terms, and cannot open normal diplomatic relations, because the community of states withholds recognition. Their exit from this bind runs entirely through the goodwill of the very states with an interest in refusing it.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_de_facto_polities, payer,
    powerless, generational, trapped, regional).

% Live under a governance arrangement they did not choose and cannot exit into an internationally cognizable alternative, because their preferred new state would not be recognized regardless of how well it satisfied objective statehood criteria — recognition depends on the interests of existing states, not on the population's own assessment of viability or legitimacy.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, secessionist_populations, payer,
    powerless, biographical, trapped, local).

% Possess long-standing territorial, cultural, and governance continuity that would satisfy objective criteria under a declaratory framework, but under the constitutive reading their claim to statehood is contingent entirely on being recognized by states whose own territorial integrity would be affected by that recognition — a structural conflict of interest with no appeal mechanism.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, stateless_indigenous_nations, payer,
    powerless, civilizational, trapped, regional).

% Study and debate whether recognition is constitutive of statehood or merely declaratory of a fact already established by objective criteria. Their scholarship documents the gap between the constitutive reading's practical operation and its own stated justifications, without having the power to alter state practice.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__constitutive_reading, diffuse).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__constitutive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides existing states with a stable, legible process for admitting new members into treaties, international organizations, and diplomatic relations, avoiding chaotic proliferation of unverified claimants and preserving predictability in the international system.
% TRANSFER_FUNCTION: Moves the practical benefits of sovereignty — treaty access, IMF/World Bank borrowing, UN membership, diplomatic immunity, seat at negotiating tables — from any entity that satisfies objective statehood criteria to only those entities that existing states choose to recognize, transferring a veto over new statehood to the incumbent community.
% ABSENT_VOICES: Unrecognized polities, secessionist populations, and stateless indigenous nations have no vote in the recognition process that determines their own status — the very states asked to recognize them frequently have a direct interest in refusing (territorial integrity concerns, precedent-setting fears, alliance politics), and no independent tribunal can compel recognition against that interest.
% DISAPPEARANCE_RATIONALE: If constitutive recognition were abolished overnight in favor of a purely declaratory standard, dozens of de facto administrations meeting the objective Montevideo criteria (defined territory, permanent population, government, capacity for foreign relations) would immediately qualify for treaty participation, UN engagement, and international borrowing without needing the assent of incumbent states — reshaping UN membership rolls, contested-territory diplomacy, and international financial access essentially overnight.
% FOUNDING_PROBLEM: In the early-to-mid 20th century, as decolonization accelerated and irregular polities proliferated, the international system needed a way to distinguish genuine states from claimants, secessionist factions, puppet regimes, and unstable territories, so that treaty partners and international organizations would know with whom they could reliably transact.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and UN member states from the Global South (in General Assembly debates on universality of membership) attest that the founding verification problem is largely solved by modern monitoring, satellite verification, and established diplomatic practice, and that constitutive recognition now functions primarily as a geopolitical veto rather than a genuine verification mechanism. Permanent Security Council members and states benefiting from contested-territory status quos continue to attest the founding problem remains live, citing risks of destabilizing precedent from over-easy recognition.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.68 and rising because the constitutive reading structurally routes the practical value of sovereignty (treaty access, borrowing capacity, diplomatic standing) through the discretion of incumbent states, and that discretion has hardened over the post-Cold War period into an increasingly instrumentalized veto (Kosovo, South Ossetia/Abkhazia, Western Sahara, Taiwan all show recognition decisions driven by great-power alignment rather than by fresh assessment of the objective criteria). Suppression is high (0.72) because the mechanism by which non-recognition is enforced — exclusion from UN bodies, denial of sovereign borrowing, refusal of treaty counterparty status — requires no active coercive violence but functions as comprehensive structural exclusion that unrecognized polities cannot appeal past. Theater ratio (0.40) reflects that a substantial share of recognition practice still performs genuine verification (checking territorial control, government functionality) even as an increasing share defends geopolitical alignment interests dressed in the language of criteria-assessment.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of an existing recognized state, the recognition requirement looks like ordinary prudent gatekeeping consistent with orderly international relations. From the seat of an unrecognized de facto polity meeting every objective criterion, the same requirement looks like an indefinite, interest-conflicted veto with no path to appeal. The engine should compute these as structurally different experiences of the identical rule, driven by the beneficiary/victim declarations and exit-option asymmetry, not by any difference in the rule's text.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing recognized states, and especially P5 members, sit at the beneficiary end: they hold the veto, bear none of its costs, and can use it as leverage. Regional hegemons benefit similarly at smaller scope. Unrecognized polities, secessionist populations, and stateless indigenous nations sit at the full-target end: trapped exit options, no appeal mechanism, and the object of the enforcement machinery. This is the central directional asymmetry the constitutive reading names: the states asked to judge a claim to statehood are frequently the same states with an interest in the claim's rejection (territorial integrity, allied bloc politics, precedent-avoidance).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding verification problem (distinguishing real governments from unstable or fraudulent claimants) is substantially solved by modern monitoring capacity, yet the constitutive machinery persists and has hardened rather than relaxed — the founding_problem_status is authored contested precisely because incumbent states and their beneficiary allies continue to assert the verification function is live while independent legal scholarship and much of the Global South treat it as a residual veto. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (the international system does benefit from a legible admission process) while still naming the asymmetric extraction that rides on it — collapsing to snare would erase the coordination story's partial truth; collapsing to rope would erase the victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_declaratory_ontology,
    'Is recognition genuinely constitutive of statehood (an unrecognized entity is not yet legally a state) or merely declaratory (an unrecognized entity already is a state, and non-recognition is other states acting wrongfully toward an existing legal fact)? This is the kernel''s central contest.',
    'No empirical test resolves this; it is a jurisprudential commitment. Partial evidence: ICJ advisory opinions (e.g. Kosovo 2010) that avoid ruling on statehood directly while adjudicating the legality of declarations of independence suggest international law itself has not settled the ontology.',
    'If the declaratory reading is correct, the victims here (unrecognized de facto polities) are victims of a different wrong — wrongful non-recognition of an existing fact, not exclusion from a constitutive gate — and this story''s classification would need re-examination as a distinct constraint rather than treated as settled tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutive_vs_declaratory_ontology, conceptual, 'Whether recognition constitutes or merely declares statehood — the kernel''s foundational contest.').

omega_variable(
    hybrid_normative_overlay_effect,
    'Would adding a normative legitimacy layer (the hybrid_reading''s addition of democratic governance, human rights compliance, non-aggression) reduce or increase the extraction this story identifies, by giving incumbent states additional principled grounds for withholding recognition, or by giving genuinely legitimate claimants a stronger appeal against interest-conflicted incumbents?',
    'Compare recognition outcomes for claimants meeting objective criteria alone versus objective-plus-normative criteria across historical cases (South Sudan, Kosovo, Somaliland) to see whether normative criteria correlate with faster/slower recognition independent of great-power interest.',
    'If normative criteria mostly provide cover for interest-driven refusal, the hybrid reading would show similar or higher extraction than the constitutive reading; if normative criteria genuinely constrain arbitrary refusal, the hybrid reading would show lower extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_normative_overlay_effect, empirical, 'Whether the hybrid reading''s normative overlay reduces or masks the extraction identified here.').

omega_variable(
    recognition_as_natural_vs_constructed_practice,
    'Is the requirement that new states be recognized by the existing community an unavoidable structural feature of any system of mutual state relations (any club must have an admission process), or is it a constructed arrangement that could be replaced by automatic recognition upon satisfaction of verifiable criteria?',
    'Examine whether international organizations with automatic-admission rules for other kinds of entry (e.g., automatic treaty accession upon meeting technical criteria in some regimes) function coherently without a discretionary admission veto, as a structural analogy.',
    'If some form of gatekeeping is unavoidable in any club-like system, part of the measured extraction may be irreducible coordination cost rather than pure rent; if automatic-criteria admission is workable, the entire constitutive apparatus is more plausibly read as constructed extraction with no coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_as_natural_vs_constructed_practice, conceptual, 'Whether some discretionary admission gate is structurally unavoidable or fully constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1933, 0.2).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(mont_tr_t1975, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(mont_tr_t1991, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1991, 0.34).
narrative_ontology:measurement(mont_tr_t2008, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2008, 0.37).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1933, 0.42).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1960, 0.48).
narrative_ontology:measurement(mont_be_t1975, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(mont_be_t1991, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1991, 0.6).
narrative_ontology:measurement(mont_be_t2008, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2008, 0.64).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1933, 0.5).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1960, 0.58).
narrative_ontology:measurement(mont_su_t1975, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(mont_su_t1991, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1991, 0.65).
narrative_ontology:measurement(mont_su_t2008, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2008, 0.69).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__constitutive_reading, 0.1).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the montevideo_statehood_criteria kernel. The constitutive_reading (this file) treats recognition as a precondition of statehood and locates the victim set in unrecognized polities excluded from treaty/financial/diplomatic access. The declaratory_reading treats objective criteria satisfaction as sufficient and locates the wrong in other states' refusal to acknowledge an existing fact rather than in a missing precondition — a materially different beneficiary/victim structure even though it discusses the same historical cases. The hybrid_reading adds a normative legitimacy overlay that can shrink or reshape the victim set again by excluding claimants that meet objective criteria but fail normative tests (e.g. entities formed through aggression). Each carries its own ε per the ε-invariance principle; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
