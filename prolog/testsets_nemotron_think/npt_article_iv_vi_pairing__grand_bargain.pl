% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__grand_bargain, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Article IV-VI Grand Bargain (Reciprocal Obligations Reading)
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   The NPT's Articles IV and VI are interpreted in this reading as a
 *   reciprocal grand bargain: non-weapon states accept intrusive verification
 *   and permanent renunciation of nuclear weapons (Article IV restraint) in
 *   exchange for weapon states' binding commitment to pursue nuclear
 *   disarmament in good faith (Article VI). The reading holds that breach of
 *   Article VI — the failure to achieve meaningful disarmament progress —
 *   undermines the legitimacy of Article IV's non-proliferation obligations,
 *   licensing non-weapon states to withdraw or expand their Article IV
 *   rights. This is not the treaty's only reading; it contests the
 *   nonproliferation-primary reading (which treats Article VI as
 *   aspirational) and the abolitionist reading (which treats Article IV as
 *   illegitimate dual-use proliferation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.62).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.48).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.62).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Article IV-VI Grand Bargain (Reciprocal Obligations Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, '9534fe1c-fdab-4d33-8b18-ce7cb3c012ac').
narrative_ontology:cs_kernel_codification('9534fe1c-fdab-4d33-8b18-ce7cb3c012ac', formalized).
narrative_ontology:cs_authority_grounding('9534fe1c-fdab-4d33-8b18-ce7cb3c012ac', lineage).
narrative_ontology:cs_interpretation_layer_present('9534fe1c-fdab-4d33-8b18-ce7cb3c012ac').
narrative_ontology:cs_reading_relation('9534fe1c-fdab-4d33-8b18-ce7cb3c012ac', npt_article_iv_vi_pairing__nonproliferation_primary, coexists_with).
narrative_ontology:cs_reading_relation('9534fe1c-fdab-4d33-8b18-ce7cb3c012ac', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('9534fe1c-fdab-4d33-8b18-ce7cb3c012ac', foundational, reciprocal_obligation).
narrative_ontology:cs_axiom_status(reciprocal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('9534fe1c-fdab-4d33-8b18-ce7cb3c012ac', reciprocal_obligation, conventional).
narrative_ontology:cs_axiom('9534fe1c-fdab-4d33-8b18-ce7cb3c012ac', foundational, conditional_legitimacy).
narrative_ontology:cs_axiom_status(conditional_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9534fe1c-fdab-4d33-8b18-ce7cb3c012ac', conditional_legitimacy, conventional).
narrative_ontology:cs_reference_frame('9534fe1c-fdab-4d33-8b18-ce7cb3c012ac', original_bargain).
narrative_ontology:cs_drift_state('9534fe1c-fdab-4d33-8b18-ce7cb3c012ac', contemporary_review_conferences, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9534fe1c-fdab-4d33-8b18-ce7cb3c012ac', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, nuclear_nonproliferation_norm).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, disarmament_obligation).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, reciprocal_bargain_conditionality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear weapons and benefit from the non-proliferation commitments of non-weapon states under Article IV while making minimal disarmament progress under Article VI. They control the treaty's verification agenda through the IAEA Board and UN Security Council. Their exit option is to ignore review conference outcomes or withdraw from the treaty with minimal immediate cost.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, weapon_states, beneficiary,
    institutional, generational, arbitrage, global).

% Forego nuclear weapons in exchange for peaceful nuclear technology access (Article IV) and the promise of disarmament (Article VI). They bear the cost of intrusive verification and the opportunity cost of weapons renunciation. Exit (withdrawal under Article X) carries severe political, economic, and security penalties. Their leverage is collective action through the Non-Aligned Movement and review conference consensus rules.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, non_weapon_states, payer,
    organized, generational, constrained, global).

% Administers the verification regime (Article III) that polices Article IV compliance. Its authority derives from the treaty text and Board of Governors decisions. It does not collect extraction but its operational mandate expands when the bargain is stressed (e.g., additional protocols). It is structurally insulated from the disarmament obligation (Article VI) which it does not verify.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, iaea_verification, agenda_setter,
    institutional, generational, analytical, global).

% Survivors of nuclear use and testing, and transnational advocacy networks, who argue the bargain legitimizes perpetual nuclear apartheid. They are formally excluded from treaty governance (no standing in review conferences) but exert normative pressure through the Humanitarian Initiative and TPNW. Their exit is impossible — they live with the consequences regardless of treaty status.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, civil_society_hibakusha, excluded,
    moderate, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal nuclear proliferation by offering peaceful nuclear technology access (Article IV) in exchange for non-weapon states' renunciation of nuclear weapons, conditioned on weapon states pursuing nuclear disarmament in good faith (Article VI).
% TRANSFER_FUNCTION: Moves security assurance and technology access from weapon states to non-weapon states, while moving disarmament obligation costs onto weapon states. In practice, the transfer is asymmetric: non-weapon states forego weapons options irreversibly, weapon states delay disarmament indefinitely.
% ABSENT_VOICES: Non-weapon states that have not joined the NPT (India, Pakistan, Israel, North Korea) are excluded from the bargain; future generations who bear proliferation risks; civil society and hibakusha voices marginalized in review conferences.
% DISAPPEARANCE_RATIONALE: The NPT grand bargain structures the global nuclear order; its collapse would remove the legal basis for non-proliferation commitments and disarmament expectations, leading to a world of unconstrained proliferation or new security arrangements.
% FOUNDING_PROBLEM: The 1960s proliferation threat: multiple states on the verge of acquiring nuclear weapons, creating instability; the bargain solved this by offering a deal: non-proliferation for peaceful use and disarmament.
% FOUNDING_PROBLEM_CORROBORATION: Historical record of 1968 negotiations corroborates the bargain; non-aligned movement statements and NPT review conference documents corroborate the conditional nature; weapon states' official positions contest the conditionality.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the asymmetric payoff: weapon states collect the non-proliferation benefit while deferring disarmament costs. Suppression (0.48) is moderate — the regime relies on verification and political pressure, not total closure of alternatives (withdrawal remains legally possible). Theater ratio (0.42) captures the performative character of review conferences where disarmament language is recycled without implementation. Accessibility collapse (0.58) and resistance (0.68) reflect the real but costly exit options for non-weapon states and their sustained diplomatic pushback.
 *
 * PERSPECTIVAL GAP:
 *   From the weapon-state seat, the arrangement is a rope: they built and maintain a coordination mechanism that prevents proliferation. From the non-weapon-state seat, it is a snare: they are locked into a deal whose other party has not performed. The engine computes this divergence from the structural data — the authored claim (tangled_rope) captures the hybrid reality without adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states are structural beneficiaries (d near 0.15): they gain non-proliferation without paying the disarmament cost, and hold arbitrage-grade exit (Security Council veto, withdrawal impunity). Non-weapon states are structural targets (d near 0.85): they pay the full renunciation cost upfront, face constrained exit (Article X withdrawal triggers sanctions/isolation), and are identity-locked into the non-proliferation norm. IAEA sits near symmetric (d ~0.5) — it administers the constraint but does not extract. Civil society is trapped (d ~0.9) — they bear humanitarian consequences with no governance voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1960s proliferation wave) is contested: weapon states argue it persists (justifying the bargain), non-weapon states argue it has mutated into a disarmament deficit (justifying conditionality). The bargain's mandate has not atrophied — it is actively contested. Mandatrophy is unresolved because the coordination function (non-proliferation) remains live while the extraction function (disarmament avoidance) has intensified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the NPT Article IV-VI pairing a single constraint with multiple interpretations, or are the rival readings (grand_bargain, nonproliferation_primary, abolitionist) structurally distinct constraints with different ε values?',
    'Apply the ε-invariance test: if measuring extraction under the grand_bargain reading (conditional reciprocity) yields a different ε than under nonproliferation_primary (asymmetric coordination), they are distinct constraints. Compare stakeholder sets, beneficiary/victim structures, and enforcement logics across readings.',
    'If distinct, each reading gets its own constraint story linked by network.affects_constraints. If one constraint, the engine must compute a single ε from a unified stakeholder surface — but the readings have incompatible beneficiary/victim declarations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints.').

omega_variable(
    conditionality_enforceability,
    'Does breach of Article VI legally license non-weapon states to withdraw from Article IV obligations or expand peaceful nuclear activities beyond IAEA safeguards?',
    'Legal analysis of Article X (withdrawal) and Article IV text; state practice at review conferences (1995, 2000, 2010, 2015, 2022); ICJ advisory opinions on treaty breach and termination.',
    'If conditionality is legally enforceable, the constraint is a conditional rope/scaffold with a built-in sunset trigger. If not, the extraction is locked in and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_enforceability, empirical, 'Whether the reciprocal conditionality has legal operative force.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of non-weapon-state exit structural (sanctions, security guarantees, technology denial) or internalized (norm internalization, identity as ''responsible non-nuclear-weapon state'')?',
    'Post-exit suppression trajectory: examine states that withdrew (North Korea) or hedged (Iran, Brazil, Japan) — does suppression persist after the extractive mechanism is challenged?',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them after exit, making the constraint more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-weapon states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_grand_bargain_tr_t1968, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1968, 0.15).
narrative_ontology:measurement_basis(npt_grand_bargain_tr_t1968, observed).
narrative_ontology:measurement(npt_grand_bargain_tr_t1985, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1985, 0.25).
narrative_ontology:measurement_basis(npt_grand_bargain_tr_t1985, observed).
narrative_ontology:measurement(npt_grand_bargain_tr_t1995, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1995, 0.32).
narrative_ontology:measurement_basis(npt_grand_bargain_tr_t1995, observed).
narrative_ontology:measurement(npt_grand_bargain_tr_t2000, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(npt_grand_bargain_tr_t2000, observed).
narrative_ontology:measurement(npt_grand_bargain_tr_t2010, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2010, 0.4).
narrative_ontology:measurement_basis(npt_grand_bargain_tr_t2010, observed).
narrative_ontology:measurement(npt_grand_bargain_tr_t2024, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(npt_grand_bargain_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(npt_grand_bargain_be_t1968, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1968, 0.35).
narrative_ontology:measurement_basis(npt_grand_bargain_be_t1968, observed).
narrative_ontology:measurement(npt_grand_bargain_be_t1985, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement_basis(npt_grand_bargain_be_t1985, observed).
narrative_ontology:measurement(npt_grand_bargain_be_t1995, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement_basis(npt_grand_bargain_be_t1995, observed).
narrative_ontology:measurement(npt_grand_bargain_be_t2000, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement_basis(npt_grand_bargain_be_t2000, observed).
narrative_ontology:measurement(npt_grand_bargain_be_t2010, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement_basis(npt_grand_bargain_be_t2010, observed).
narrative_ontology:measurement(npt_grand_bargain_be_t2024, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(npt_grand_bargain_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt_grand_bargain_su_t1968, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1968, 0.3).
narrative_ontology:measurement_basis(npt_grand_bargain_su_t1968, observed).
narrative_ontology:measurement(npt_grand_bargain_su_t1985, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1985, 0.4).
narrative_ontology:measurement_basis(npt_grand_bargain_su_t1985, observed).
narrative_ontology:measurement(npt_grand_bargain_su_t1995, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement_basis(npt_grand_bargain_su_t1995, observed).
narrative_ontology:measurement(npt_grand_bargain_su_t2000, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2000, 0.47).
narrative_ontology:measurement_basis(npt_grand_bargain_su_t2000, observed).
narrative_ontology:measurement(npt_grand_bargain_su_t2010, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement_basis(npt_grand_bargain_su_t2010, observed).
narrative_ontology:measurement(npt_grand_bargain_su_t2024, npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 2024, 0.48).
narrative_ontology:measurement_basis(npt_grand_bargain_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__grand_bargain, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__grand_bargain, 0.1).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% This constraint is one member of the NPT Article IV-VI pairing kernel family. The grand_bargain reading treats the pairing as a conditional reciprocal bargain (tangled_rope). The nonproliferation_primary reading treats Article IV as the core obligation and Article VI as aspirational (rope/snare hybrid). The abolitionist reading treats Article VI as the supreme obligation and Article IV as illegitimate dual-use enabler (snare). All three share the same treaty text but instantiate different ε values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__grand_bargain, institutional, 0.15).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__grand_bargain, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
