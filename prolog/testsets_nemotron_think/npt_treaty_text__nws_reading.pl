% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT NWS Reading: Non-Proliferation Binding, Disarmament Aspirational
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty (NPT) is read by the five recognized
 *   Nuclear Weapon States (NWS: US, Russia, UK, France, China) as imposing a
 *   binding, verifiable non-proliferation obligation on Non-Nuclear Weapon
 *   States (NNWS) through IAEA safeguards, while treating Article VI's
 *   disarmament obligation ('pursue negotiations in good faith... at an early
 *   date') as an aspirational long-term goal without enforcement mechanism or
 *   specific timeline. This reading leverages the interpretive ambiguity of
 *   'at an early date' and 'good faith' to avoid binding disarmament
 *   commitments while maintaining the non-proliferation regime that
 *   constrains NNWS. The IAEA safeguards system — funded disproportionately
 *   by NNWS and focused on horizontal proliferation verification — operates
 *   as the enforcement arm. The NWS reading thus instantiates a
 *   high-extraction tangled_rope: genuine coordination function (preventing
 *   horizontal proliferation) combined with asymmetric extraction (NWS retain
 *   nuclear arsenals indefinitely while NNWS bear verification costs and
 *   forego the nuclear option).
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: Primary beneficiaries (institutional/arbitrage) — retain nuclear arsenals, control interpretation, avoid disarmament enforcement
 *   - non_nuclear_weapon_states: Primary payers (organized/constrained) — bear safeguards costs, forego nuclear option, face compliance pressure
 *   - iaea_secretariat: Agenda setter (institutional/biographical) — administers verification, budget concentrated on NNWS monitoring
 *   - un_security_council: Agenda setter (institutional/analytical) — enforces non-compliance via sanctions, dominated by NWS
 *   - civil_society_disarmament_advocates: Excluded (organized/analytical) — push for Article VI enforcement, structurally marginalized
 *   - threshold_states: Payers (moderate/constrained) — states with latent capability (Japan, Germany, Brazil) bearing highest verification burden
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.78).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.85).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT NWS Reading: Non-Proliferation Binding, Disarmament Aspirational").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, 'b98ff91b-90c5-4116-ae7b-47ded8767a73').
narrative_ontology:cs_kernel_codification('b98ff91b-90c5-4116-ae7b-47ded8767a73', formalized).
narrative_ontology:cs_authority_grounding('b98ff91b-90c5-4116-ae7b-47ded8767a73', lineage).
narrative_ontology:cs_interpretation_layer_present('b98ff91b-90c5-4116-ae7b-47ded8767a73').
narrative_ontology:cs_reading_relation('b98ff91b-90c5-4116-ae7b-47ded8767a73', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('b98ff91b-90c5-4116-ae7b-47ded8767a73', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('b98ff91b-90c5-4116-ae7b-47ded8767a73', foundational, article_vi_aspirational_only).
narrative_ontology:cs_axiom_status(article_vi_aspirational_only, holdable).
narrative_ontology:cs_axiom_grounding('b98ff91b-90c5-4116-ae7b-47ded8767a73', article_vi_aspirational_only, conventional).
narrative_ontology:cs_axiom('b98ff91b-90c5-4116-ae7b-47ded8767a73', foundational, nws_interpretive_authority).
narrative_ontology:cs_axiom_status(nws_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('b98ff91b-90c5-4116-ae7b-47ded8767a73', nws_interpretive_authority, conventional).
narrative_ontology:cs_reference_frame('b98ff91b-90c5-4116-ae7b-47ded8767a73', id_1968_grand_bargain_nws_interpretation).
narrative_ontology:cs_drift_state('b98ff91b-90c5-4116-ae7b-47ded8767a73', post_2010_review_conference_failures, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b98ff91b-90c5-4116-ae7b-47ded8767a73', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, threshold_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, threshold_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five NPT-recognized NWS (US, Russia, UK, France, China) retain nuclear arsenals indefinitely under this reading. They control the P5 process that defines disarmament steps, veto UNSC action on disarmament enforcement, and set the interpretation of 'at an early date.' They bear minimal verification burden (voluntary offer safeguards only) while collecting the strategic value of nuclear deterrence. Exit is arbitrage-grade: they could withdraw but the regime legitimizes their status.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, beneficiary,
    institutional, generational, arbitrage, global).

% 185+ NNWS accept comprehensive IAEA safeguards, forego the nuclear option permanently, and submit to technology denial regimes (NSG). They pay the bulk of IAEA safeguards costs through assessed contributions and national implementation. Withdrawal under Article X triggers immediate security crisis (North Korea precedent). Some NNWS (threshold states) bear higher costs due to latent capability scrutiny. Identity-locked for many: NPT membership is constitutive of 'responsible state' identity.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% Administers the verification regime. Budget and staffing are overwhelmingly directed at NNWS comprehensive safeguards (95%+ of inspection effort). NWS facilities receive only voluntary offer safeguards (minimal). The Secretariat's institutional survival depends on the non-proliferation coordination function, creating structural alignment with the NWS reading's enforcement priorities. It cannot compel NWS disarmament verification.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_secretariat, agenda_setter,
    institutional, biographical, analytical, global).

% Enforces non-compliance via Chapter VII sanctions (Iraq 1990s, Iran 2006-2015, North Korea). Permanent members are the NWS, giving them veto power over enforcement direction. The UNSC has never sanctioned an NWS for Article VI non-compliance. Its enforcement function is structurally captured by the NWS reading's priority on non-proliferation over disarmament.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, un_security_council, agenda_setter,
    institutional, generational, analytical, global).

% NGOs, academics, and activist networks (ICAN, Pugwash, IPPNW) advocating for binding Article VI implementation, TPNW, and disarmament verification. They participate as observers at Review Conferences but have no vote, no enforcement power, and no seat at the P5 process. Their exclusion is structural: the NWS reading's interpretive monopoly requires marginalizing the disarmament-as-binding frame.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, civil_society_disarmament_advocates, excluded,
    organized, biographical, mobile, global).

% States with advanced nuclear latency (Japan, Germany, Brazil, South Korea, Canada) that bear the highest verification burden (frequent inspections, technology denial) while receiving security assurances from NWS allies. They benefit from the non-proliferation coordination (regional stability) but pay disproportionate costs. Their exit is constrained: acquisition breaks alliance commitments; withdrawal breaks security guarantees.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, threshold_states, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, threshold_states, beneficiary).

% Sees the full structure: a kernel generating competing readings, each instantiating a distinct constraint with different beneficiary/victim maps. The NWS reading is a high-epsilon tangled_rope; the NNWS reading would instantiate a different constraint type. The analytical seat computes per-seat classifications from the structural data.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal nuclear proliferation by establishing a verified, near-universal commitment by NNWS not to acquire nuclear weapons, backed by IAEA safeguards and UNSC enforcement. Solves the collective action problem of mutual suspicion driving proliferation cascades.
% TRANSFER_FUNCTION: Moves the strategic value of nuclear foreclosure from NNWS (who permanently renounce the nuclear option and bear verification costs) to NWS (who retain nuclear arsenals without binding disarmament timeline or external verification). The IAEA budget transfers resources from NNWS-assessed contributions to a verification system focused on NNWS compliance.
% ABSENT_VOICES: States that never joined NPT (India, Pakistan, Israel, South Sudan) — they would object to the NWS reading's claim of universality but are structurally outside. Future generations who inherit the disarmament deficit — they have no seat. Nuclear-affected communities (test veterans, downwinders, uranium miners) — their harm is not counted in the treaty's calculus.
% DISAPPEARANCE_RATIONALE: If the NWS reading vanished overnight, the non-proliferation verification regime would lose its interpretive anchor (NWS define compliance), the P5 process would dissolve, and NNWS would face a choice: demand binding disarmament verification as the price of continued compliance, or pursue hedging/withdrawal. The mobile nuclear latent states (Japan, Germany, etc.) would likely reassess within years. The regime would not persist in its current form.
% FOUNDING_PROBLEM: 1960s fear of uncontrolled nuclear proliferation: 20+ states predicted to acquire nuclear weapons by 1980s (Kennedy 1963). The NPT was built to cap the nuclear club at five through a grand bargain: NNWS forego nuclear weapons in exchange for NWS disarmament (Art VI) and peaceful nuclear technology access (Art IV).
% FOUNDING_PROBLEM_CORROBORATION: NWS attest the non-proliferation half remains live (proliferation risks persist) while the disarmament half is aspirational (security conditions not met). NNWS and NAM attest the bargain is conditional — non-proliferation compliance was purchased by the disarmament promise, which remains unfulfilled after 50+ years. ICJ 1996 Advisory Opinion unanimously found Article VI creates a binding obligation to pursue and conclude disarmament negotiations — corroboration from outside the NWS beneficiary set that the founding problem's disarmament half is legally live.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the NWS reading extracts the strategic value of nuclear retention from NNWS's permanent foreclosure of the nuclear option, while the NWS disarmament obligation remains structurally unfalsifiable. Suppression is very high (0.85) because the constraint's persistence depends on active enforcement: IAEA inspections, UNSC sanctions for non-compliance (Iraq, Iran, North Korea), and export control regimes (NSG) that deny technology to NNWS. Theater ratio is moderate (0.42): the non-proliferation coordination function is real and valued, but a growing share of the regime's diplomatic and verification energy defends the NWS interpretive monopoly on Article VI rather than the coordination core. Accessibility collapse is moderate-high (0.65): alternatives (nuclear acquisition, withdrawal) exist but carry extreme costs (sanctions, isolation, security guarantees loss). Resistance is moderate (0.55): NNWS coalition (NAM, G-77) regularly challenges the asymmetry at Review Conferences but lacks leverage to alter the structural bargain.
 *
 * PERSPECTIVAL GAP:
 *   The NWS seat experiences this as a rope (genuine coordination preventing chaotic proliferation). The NNWS seat experiences it as a snare (permanent asymmetry enforced by NWS monopoly on interpretation and enforcement). The engine computes this divergence from the structural data: same constraint, different directionalities, different effective extractions. The claimed type (tangled_rope) acknowledges both the coordination reality and the extraction reality.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS are structural beneficiaries (d ≈ 0.15): they collect the strategic value of nuclear retention, control the interpretive framework through P5 process and UNSC veto, and face arbitrage-grade exit (they could withdraw but the regime serves their interests). NNWS are structural targets (d ≈ 0.85): they pay the transfer (foregone nuclear option + safeguards costs + technology denial), have constrained exit (withdrawal triggers Article X security crisis), and face identity-locked dynamics for some (NPT as cornerstone of security identity). IAEA Secretariat sits near symmetric (d ≈ 0.5): it administers the coordination function but its budget and mandate are structurally captured by the verification asymmetry. UNSC is a beneficiary-adjacent agenda setter (d ≈ 0.25): NWS dominance of UNSC makes enforcement serve the NWS reading. Civil society advocates are excluded — their opposition is the coordination function's external critique, not a structural seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing nuclear war through non-proliferation + disarmament) is contested: NWS claim the non-proliferation half is live while the disarmament half is aspirational; NNWS claim the bargain is conditional — non-proliferation compliance purchased NWS disarmament progress. The arrangement persists because the coordination function (non-proliferation) remains valuable to all, but the extraction asymmetry has accumulated as NWS arsenals modernized rather than eliminated. This is not pure mandatrophy (the coordination function is not atrophied) but a frozen bargain where one side's obligation became aspirational while the other's became binding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the NPT treaty text a single constraint or a kernel generating multiple distinct constraints via competing readings?',
    'Apply epsilon-invariance test: if measuring ''the NPT'' via non-proliferation verification yields low epsilon but measuring via disarmament obligation yields high epsilon, they are distinct constraints. Decompose into separate stories per reading.',
    'If kernel decomposition holds, the NWS reading instantiates a high-epsilon tangled_rope; the NNWS reading instantiates a different constraint with different beneficiary/victim structure. The engine classifies each reading independently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the treaty label ''NPT'' covers one constraint or a kernel of multiple structurally distinct constraints').

omega_variable(
    article_vi_ambiguity_naturalness,
    'Is the ''at an early date'' ambiguity in Article VI a genuine textual indeterminacy or a constructed ambiguity that benefits NWS?',
    'Compare negotiating history (1968 record), subsequent review conference outcomes, and ICJ advisory opinion (1996) on whether a specific disarmament timeline was contemplated vs. deliberately left open.',
    'If constructed ambiguity, the NWS reading''s interpretive control is active extraction; if genuine indeterminacy, the reading is one of several plausible interpretations. Affects whether the constraint is tangled_rope (constructed) or rope with contested scope (genuine).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_ambiguity_naturalness, conceptual, 'Natural-law vs. constructed status of the Article VI temporal ambiguity').

omega_variable(
    iaea_resource_allocation_bias,
    'Does the IAEA safeguards budget concentration on horizontal proliferation verification (vs. disarmament verification) reflect technical necessity or political capture?',
    'Analyze IAEA budget allocations 1970-present: fraction to NWS facility verification vs. NNWS comprehensive safeguards. Compare with technical requirements for verifying disarmament steps (warhead dismantlement, fissile material cutoff).',
    'If political capture, the verification asymmetry is part of the extraction mechanism; if technical necessity, the coordination function genuinely requires asymmetric verification. Affects extractiveness attribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iaea_resource_allocation_bias, empirical, 'Whether verification resource asymmetry is functional or extractive').

omega_variable(
    nws_disarmament_progress_measurement,
    'What constitutes measurable disarmament progress under the NWS reading, and who adjudicates compliance?',
    'Examine NPT Review Conference final documents, NWS unilateral declarations, and P5 process outputs for operational criteria. Assess whether any NWS accepts external verification of disarmament steps.',
    'If no measurable criteria and no external adjudication exist, the aspirational framing is structurally unfalsifiable — a hallmark of extraction cover. If criteria exist but are unmet, the constraint is a snare with coordination cover.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nws_disarmament_progress_measurement, empirical, 'Whether the disarmament obligation has operational content under this reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_nws_tr_t1970, npt_treaty_text__nws_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(npt_nws_tr_t1985, npt_treaty_text__nws_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(npt_nws_tr_t1995, npt_treaty_text__nws_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(npt_nws_tr_t2000, npt_treaty_text__nws_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(npt_nws_tr_t2010, npt_treaty_text__nws_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(npt_nws_tr_t2020, npt_treaty_text__nws_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(npt_nws_tr_t2025, npt_treaty_text__nws_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(npt_nws_be_t1970, npt_treaty_text__nws_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(npt_nws_be_t1985, npt_treaty_text__nws_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(npt_nws_be_t1995, npt_treaty_text__nws_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(npt_nws_be_t2000, npt_treaty_text__nws_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(npt_nws_be_t2010, npt_treaty_text__nws_reading, base_extractiveness, 2010, 0.72).
narrative_ontology:measurement(npt_nws_be_t2020, npt_treaty_text__nws_reading, base_extractiveness, 2020, 0.76).
narrative_ontology:measurement(npt_nws_be_t2025, npt_treaty_text__nws_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt_nws_su_t1970, npt_treaty_text__nws_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(npt_nws_su_t1985, npt_treaty_text__nws_reading, suppression_requirement, 1985, 0.72).
narrative_ontology:measurement(npt_nws_su_t1995, npt_treaty_text__nws_reading, suppression_requirement, 1995, 0.78).
narrative_ontology:measurement(npt_nws_su_t2000, npt_treaty_text__nws_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(npt_nws_su_t2010, npt_treaty_text__nws_reading, suppression_requirement, 2010, 0.82).
narrative_ontology:measurement(npt_nws_su_t2020, npt_treaty_text__nws_reading, suppression_requirement, 2020, 0.84).
narrative_ontology:measurement(npt_nws_su_t2025, npt_treaty_text__nws_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nws_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, iaea_safeguards_system).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, nuclear_suppliers_group).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, ctbt_entry_into_force).

% DUAL FORMULATION NOTE:
% NPT kernel decomposition: this NWS reading (tangled_rope, high epsilon) and the NNWS reading (different beneficiary/victim structure) are distinct constraints linked by shared treaty text but divergent operational referents. The withdrawal threshold reading is a third constraint governing exit conditions. The coordination function (non-proliferation verification) is shared; the extraction asymmetry divides the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__nws_reading, institutional, 0.15).
constraint_indexing:directionality_override(npt_treaty_text__nws_reading, organized, 0.85).
constraint_indexing:directionality_override(npt_treaty_text__nws_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
