% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Freedom of Navigation as Customary Law Enforced by Naval Presence (Non-Ratifier Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint models the reading of the UNCLOS sovereignty-boundary
 *   kernel under which freedom-of-navigation principles are held to be
 *   customary international law, binding on all states independent of treaty
 *   ratification, and enforceable through naval presence (FONOPs) rather than
 *   tribunal adjudication. This is the reading dominant naval powers —
 *   including at least one major non-ratifier — rely on to justify continued
 *   transit through contested straits and EEZs while declining UNCLOS's
 *   compulsory dispute-settlement regime. The coordination story is real
 *   (predictable global sea lanes prevent fragmentation), but the arrangement
 *   also decouples enforcement from treaty accountability: the enforcing
 *   powers get the substantive benefit of navigational rules they never
 *   agreed to be bound by in dispute, while coastal states — especially small
 *   island states with no naval leverage — bear the cost of an EEZ claim that
 *   exists on paper but cannot be enforced against a naval power invoking
 *   custom over treaty text.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.62).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Freedom of Navigation as Customary Law Enforced by Naval Presence (Non-Ratifier Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '288eb777-41ec-49a1-a121-7cc884597c91').
narrative_ontology:cs_kernel_codification('288eb777-41ec-49a1-a121-7cc884597c91', distributed).
narrative_ontology:cs_authority_grounding('288eb777-41ec-49a1-a121-7cc884597c91', distributed).
narrative_ontology:cs_reading_relation('288eb777-41ec-49a1-a121-7cc884597c91', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('288eb777-41ec-49a1-a121-7cc884597c91', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('288eb777-41ec-49a1-a121-7cc884597c91', foundational, customary_navigation_rights_bind_non_signatories).
narrative_ontology:cs_axiom_status(customary_navigation_rights_bind_non_signatories, holdable).
narrative_ontology:cs_axiom_grounding('288eb777-41ec-49a1-a121-7cc884597c91', customary_navigation_rights_bind_non_signatories, conventional).
narrative_ontology:cs_axiom('288eb777-41ec-49a1-a121-7cc884597c91', secondary, naval_presence_constitutes_valid_enforcement_absent_treaty_accession).
narrative_ontology:cs_axiom_status(naval_presence_constitutes_valid_enforcement_absent_treaty_accession, holdable).
narrative_ontology:cs_axiom_grounding('288eb777-41ec-49a1-a121-7cc884597c91', naval_presence_constitutes_valid_enforcement_absent_treaty_accession, instrumental).
narrative_ontology:cs_reference_frame('288eb777-41ec-49a1-a121-7cc884597c91', post_1982_unclos_navigational_consensus).
narrative_ontology:cs_drift_state('288eb777-41ec-49a1-a121-7cc884597c91', contemporary_south_china_sea_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('288eb777-41ec-49a1-a121-7cc884597c91', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, non_ratifying_maritime_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_industry).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, small_island_developing_states).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_ratifying_states_seeking_treaty_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Conducts freedom-of-navigation operations (FONOPs) through contested waters, asserting that customary international law guarantees transit rights regardless of whether it has ratified UNCLOS. Maintains the fleet capacity to enforce this reading against any coastal state's contrary claim. Bears no treaty obligations under UNCLOS dispute mechanisms while still claiming the benefit of its navigational provisions as customary law.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers, beneficiary).

% Operates commercial and military vessels through international straits and EEZs without having accepted UNCLOS's compulsory dispute-settlement regime, while invoking UNCLOS's navigational-freedom provisions as binding customary law that applies to it anyway. Gets the benefit of the treaty's navigation rules without the burden of its adjudicative constraints.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, non_ratifying_maritime_states, beneficiary,
    powerful, generational, arbitrage, global).

% Depends on predictable transit rights through straits and EEZs for commercial shipping routes. Benefits from naval enforcement of open sea lanes regardless of the legal theory used to justify it, and lobbies naval powers to maintain FONOPs where coastal states attempt to restrict passage.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_industry, beneficiary,
    organized, biographical, mobile, global).

% Claims regulatory authority over foreign military activity within its 200-nautical-mile EEZ under its own reading of UNCLOS Article 56/58, but faces naval transits and surveillance operations it cannot lawfully block because the customary-law reading of navigation rights overrides its preferred treaty interpretation. Has no naval capacity to contest the transits directly and no forum where the enforcing power is bound to appear.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity, payer,
    moderate, generational, constrained, regional).

% Possesses maritime zones vastly larger than land territory and depends on exclusive resource rights within its EEZ for economic survival, but has no naval or diplomatic leverage to resist foreign naval transits or resource-adjacent surveillance justified under the customary-navigation reading. Cannot exit the arrangement; its entire claim to maritime resources rests on a treaty interpretation the dominant naval powers are not bound to honor.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, small_island_developing_states, payer,
    powerless, generational, trapped, regional).

% Ratified UNCLOS in good faith, accepted its compulsory dispute-settlement obligations, and expected reciprocal treaty commitment from other maritime actors. Finds that non-ratifying naval powers claim the treaty's navigational benefits as customary law while evading its adjudicative obligations, which undercuts the ratifying state's own leverage in tribunal proceedings and normalizes selective compliance.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_ratifying_states_seeking_treaty_primacy, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_ratifying_states_seeking_treaty_primacy, excluded).

% Adjudicates UNCLOS disputes between ratifying parties, but has no jurisdiction over a major naval power that has never ratified and does not submit to compulsory dispute settlement; its rulings on navigation and EEZ boundaries carry no binding force against the very actors most capable of contesting them by force.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_tribunals_and_arbitral_bodies, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, major_naval_powers).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, predictable global regime of sea-lane access so that commercial and naval traffic can transit straits and exclusive economic zones without needing case-by-case permission from every coastal state, preventing a patchwork of unilateral maritime claims from fragmenting global shipping.
% TRANSFER_FUNCTION: Moves effective control over EEZ waters from coastal states (particularly small and militarily weak ones) to naval powers capable of asserting customary-law transit rights by presence, while relieving non-ratifying naval powers of the compulsory dispute-settlement obligations that would otherwise bind treaty parties.
% ABSENT_VOICES: Small island developing states and weaker coastal states bear the practical cost of this reading but have no naval capacity to contest FONOPs and no forum in which the enforcing naval power is obligated to appear; their objections are registered in UN debates and academic commentary but do not alter operational practice.
% DISAPPEARANCE_RATIONALE: Naval powers and shipping interests would say the world rearranges catastrophically — sea lanes fragment into contested, unilaterally policed zones and global trade costs rise. Coastal states asserting EEZ exclusivity would say the world rearranges favorably for them — their treaty-based claims to control military activity and resources within 200 nautical miles finally become enforceable without a competing customary-law override. Both camps agree something would change; they disagree about whose claim currently controls and whose interest the change would serve.
% FOUNDING_PROBLEM: Prior to a settled navigational regime, coastal states could unilaterally restrict passage through adjacent waters, fragmenting global sea lanes and creating unpredictable friction for both military and commercial transit; UNCLOS negotiators built the navigation provisions to solve exactly this coordination problem.
% FOUNDING_PROBLEM_CORROBORATION: Naval powers and shipping associations attest the founding problem (fragmented, unpredictable sea-lane access) remains live and requires ongoing enforcement. Independent international law scholars and UN Division for Ocean Affairs commentary — sources outside the beneficiary set — note that the specific mechanism now used (a non-ratifier claiming customary-law status for treaty provisions while declining the treaty's own enforcement and dispute-resolution architecture) was not the mechanism UNCLOS negotiators designed, and constitutes a structural workaround rather than the originally intended solution.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, contested).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68 by interval end) because the practical effect is a one-way benefit: naval powers gain guaranteed passage and freedom from adjudicative obligation, while coastal states lose practical control of resources and security in their own declared zones. Suppression (0.62) reflects that this reading persists chiefly through naval presence rather than negotiated consent — a warship in a contested strait is the enforcement mechanism, not a court ruling. Theater ratio (0.40) captures that a meaningful share of 'freedom of navigation' rhetoric functions to legitimize what is, in cases of resource-rich EEZs, closer to a demonstration of who can enforce their reading by force.
 *
 * PERSPECTIVAL GAP:
 *   From the naval-power seat, this reading is genuine rope: it maintains a global coordination good (open sea lanes) that serves shipping and security interests broadly. From the small-island-state seat, the identical structure computes as extraction: a rule invoked selectively, enforced by force rather than adjudication, that removes exactly the resource and security control the treaty was supposed to guarantee them. The engine should register this seat divergence directly from the beneficiary/victim declarations and exit-option asymmetry, not from any narrative framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Major naval powers and non-ratifying maritime states sit at the beneficiary end: they collect the navigational benefit of customary law while bearing none of the treaty's adjudicative burden, and their exit options are effectively unconstrained (arbitrage — they can invoke custom or ignore tribunals selectively). Coastal states attempting EEZ exclusivity, and especially small island states, sit at the target end: they are structurally trapped, having built their entire economic claim on a treaty interpretation they cannot enforce against actors not bound by the treaty's dispute mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmented, unpredictable sea-lane access — remains partly live, which is why this is not simply relabeled as a pure snare; there is a real coordination function still being served. But the specific enforcement mechanism (a non-ratifier claiming the treaty's benefit as custom while rejecting its adjudicative burden) has drifted from a coordination solution toward an asymmetric extraction mechanism, which is why tangled_rope — not rope — is the structurally accurate claim: both a genuine coordination function and an asymmetric, actively enforced extraction coexist in the same structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_status_ambiguity,
    'Is freedom of navigation genuinely established customary international law binding on non-ratifiers (opinio juris plus consistent state practice), or is the customary-law characterization itself a post-hoc justification constructed by naval powers to retain treaty benefits without treaty obligations?',
    'Systematic review of state practice and opinio juris across ratifying and non-ratifying states, including whether coastal states'' protests constitute persistent objection sufficient to block customary-law formation as against them specifically.',
    'If the customary-law claim is genuine and universally recognized, the reading is closer to a rope reinforced by widespread consent. If it is a constructed justification maintained primarily by naval capability, the reading is closer to a snare wearing coordination language as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_status_ambiguity, conceptual, 'Whether freedom of navigation is settled customary law or a constructed legal cover for asymmetric enforcement.').

omega_variable(
    reading_selection_and_committer_structure,
    'This constraint is one reading (non_ratifier_enforcement_reading) of the contested unclos_sovereignty_boundary kernel. The sibling readings — strict_eez_reading and historical_rights_reading — are structurally incompatible with this reading in different ways: strict_eez_reading holds that EEZ boundaries are fully exclusive and no overlay claim (including customary navigational override) is valid, which directly negates this reading''s core premise for EEZ-internal transit; historical_rights_reading asserts a third, independent sovereignty basis that could either reinforce or compete with a customary-navigation claim depending on the specific water body. Where is the disagreement actually located?',
    'Map specific contested waters (e.g. South China Sea, Northwest Passage, Strait of Hormuz) against which of the three readings each disputing party invokes, to determine whether the disagreement is fundamentally about the existence of customary navigational law (this reading vs. strict_eez_reading) or about a prior sovereignty question that customary navigation law never reaches (this reading vs. historical_rights_reading).',
    'If most disputes are actually strict_eez_reading vs. non_ratifier_enforcement_reading conflicts, the kernel contest is primarily about whether treaty text or custom controls. If most disputes route through historical_rights_reading, the customary-navigation question is often moot because a prior sovereignty claim preempts it entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_committer_structure, conceptual, 'Locating where the three kernel readings actually conflict versus talk past each other.').

omega_variable(
    naval_capacity_as_enforcement_proxy,
    'Does the practical enforceability of this reading depend entirely on naval capacity (i.e., only states with blue-water navies can assert or resist it), making the ''customary law'' framing formally universal but practically available only to a handful of powers?',
    'Compare outcomes of navigation disputes involving naval powers against outcomes of structurally identical disputes involving states without comparable naval capacity, controlling for the legal merits asserted.',
    'If enforceability tracks naval capacity rather than legal merit, this substantially strengthens the case that the coordination story is cover for capability-based extraction, supporting a tangled_rope-to-snare drift assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naval_capacity_as_enforcement_proxy, empirical, 'Whether enforcement of this reading tracks legal merit or naval capability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(uncl_tr_t8, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(uncl_tr_t16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 16, 0.32).
narrative_ontology:measurement(uncl_tr_t24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(uncl_tr_t32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(uncl_tr_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(uncl_be_t8, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(uncl_be_t16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(uncl_be_t24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(uncl_be_t32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(uncl_be_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(uncl_su_t8, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(uncl_su_t16, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(uncl_su_t24, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement(uncl_su_t32, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(uncl_su_t40, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, historical_rights_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the unclos_sovereignty_boundary kernel. strict_eez_reading treats UNCLOS Article 57's 200nm EEZ boundary as exclusive and enforceable, with no valid overlay claims — coastal states are the beneficiaries there and naval powers asserting transit rights are the constrained party. historical_rights_reading treats historical usage/occupation as creating sovereign rights predating and overriding EEZ provisions, producing yet another beneficiary/victim structure centered on claimant states with historical presence. This story (non_ratifier_enforcement_reading) inverts the strict_eez_reading's beneficiary/victim assignment: naval powers and non-ratifiers benefit, coastal states pay. Each reading has a distinct, stable epsilon assessed by that reading's own lights; they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
