% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV/VI Pairing — Abolitionist Reading
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint story captures the abolitionist reading of the NPT's
 *   Article IV/VI pairing. It reads Article VI's disarmament mandate as
 *   immediate and categorical, and Article IV's 'inalienable right' to
 *   peaceful use as conditional on compliance with a prohibition norm derived
 *   from humanitarian law (ICJ 1996) and codified in the TPNW. The reading
 *   treats nuclear weapon possession as categorically illegal — no
 *   distinction between peaceful and military programs survives the
 *   prohibition lens. The NPT itself is delegitimized as insufficient; the
 *   constraint is the interpretive framework that makes the treaty an
 *   instrument of abolition rather than managed proliferation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.78).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV/VI Pairing — Abolitionist Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, '3e353358-b578-42c3-b4cc-587739f47a51').
narrative_ontology:cs_kernel_codification('3e353358-b578-42c3-b4cc-587739f47a51', formalized).
narrative_ontology:cs_authority_grounding('3e353358-b578-42c3-b4cc-587739f47a51', lineage).
narrative_ontology:cs_interpretation_layer_present('3e353358-b578-42c3-b4cc-587739f47a51').
narrative_ontology:cs_reading_relation('3e353358-b578-42c3-b4cc-587739f47a51', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('3e353358-b578-42c3-b4cc-587739f47a51', npt_article_iv_vi_pairing__grand_bargain, influences).
narrative_ontology:cs_axiom('3e353358-b578-42c3-b4cc-587739f47a51', foundational, nuclear_weapons_categorically_illegal).
narrative_ontology:cs_axiom_status(nuclear_weapons_categorically_illegal, holdable).
narrative_ontology:cs_axiom_grounding('3e353358-b578-42c3-b4cc-587739f47a51', nuclear_weapons_categorically_illegal, deontological).
narrative_ontology:cs_axiom('3e353358-b578-42c3-b4cc-587739f47a51', foundational, article_iv_conditioned_on_prohibition_compliance).
narrative_ontology:cs_axiom_status(article_iv_conditioned_on_prohibition_compliance, holdable).
narrative_ontology:cs_axiom_grounding('3e353358-b578-42c3-b4cc-587739f47a51', article_iv_conditioned_on_prohibition_compliance, conventional).
narrative_ontology:cs_reference_frame('3e353358-b578-42c3-b4cc-587739f47a51', npt_grand_bargain_1970).
narrative_ontology:cs_drift_state('3e353358-b578-42c3-b4cc-587739f47a51', tpnw_entry_into_force_2021, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3e353358-b578-42c3-b4cc-587739f47a51', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, civil_society_disarmament_actors).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_institutions).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, dual_use_technology_exporters).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nuclear_energy_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal grounding to demand disarmament and resist dual-use pressure. Their Article IV 'inalienable right' is reinterpreted as conditional on prohibition compliance. They bear inspection costs but gain normative leverage against nuclear weapon states.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states, beneficiary,
    organized, generational, constrained, global).

% Use the abolitionist reading as an advocacy framework. Their campaigns (ICAN, TPNW negotiation) gain treaty-text foothold. They do not bear direct compliance costs but depend on state adoption for legal effect.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, civil_society_disarmament_actors, beneficiary,
    moderate, biographical, mobile, global).

% Interpret and apply IHL/ICJ advisory opinions to nuclear weapons. The abolitionist reading extends their authority into nuclear governance. They adjudicate legality but do not enforce disarmament.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_institutions, agenda_setter,
    institutional, generational, analytical, universal).

% Face categorical illegitimacy of their arsenals under this reading. Article VI becomes an immediate, enforceable obligation, not aspirational. Their security doctrines (deterrence) are structurally incompatible. Exit requires identity transformation (renouncing nuclear status), which is politically existential.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, payer,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states, payer).

% Export controls tighten as Article IV is read through prohibition lens. Enrichment/reprocessing transfers become presumptively illegitimate. They lose commercial and strategic leverage; compliance costs rise. Exit means abandoning nuclear commerce markets.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, dual_use_technology_exporters, payer,
    powerful, biographical, constrained, global).

% Civilian programs face stigmatization and supply-chain restrictions because the peaceful/military distinction is rejected. Investment climate degrades. They bear costs of a constraint they did not create and cannot influence.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nuclear_energy_industry, payer,
    organized, biographical, constrained, global).

% IAEA safeguards architecture built on Article III/IV distinction. This reading undermines their operational mandate by treating all nuclear activity as proliferation risk. They cannot adopt it without institutional suicide but cannot ignore its normative force.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, nonproliferation_regime_bureaucracy, excluded,
    institutional, biographical, trapped, global).

% Track the interpretive contest across treaty bodies, ICJ, UNGA, TPNW meetings. They map the structural positions but hold no enforcement power.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, legal_analysts_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global prohibition regime: aligns non-weapon state restraint, humanitarian law authority, and disarmament obligation into a single normative framework that replaces the NPT's discriminatory structure.
% TRANSFER_FUNCTION: Moves legitimacy and legal risk from nuclear weapon states (who lose lawful status for arsenals) to the prohibition norm; moves compliance burden onto dual-use exporters and nuclear industry (stricter controls, stigmatization); moves normative authority to humanitarian law institutions and civil society.
% ABSENT_VOICES: Nuclear weapon states' security establishments and their allied deterrence intellectuals are structurally excluded from the prohibition framework — they would object that disarmament without security guarantees is destabilizing, but the reading treats deterrence itself as the illegitimate object.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the NPT's grand bargain framing would regain uncontested interpretive dominance, TPNW's normative force would lose its NPT-embedded foothold, and nuclear weapon states would face no treaty-internal categorical illegitimacy — the prohibition regime's legal architecture would collapse.
% FOUNDING_PROBLEM: The NPT's grand bargain failed: nuclear weapon states did not disarm (Article VI), while non-weapon states were denied full peaceful-use benefits (Article IV) under a discriminatory regime that legitimized perpetual nuclear apartheid.
% FOUNDING_PROBLEM_CORROBORATION: ICJ 1996 Advisory Opinion (unanimous on Article VI obligation), TPNW negotiating record (122 states), humanitarian consequences conferences (Oslo, Nayarit, Vienna), and NPT RevCon final documents (2010, 2015) citing 'deep concern' at disarmament failure — all from states and bodies outside the nuclear weapon state beneficiary set.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the reading extracts the legal legitimacy of nuclear weapon states' arsenals and the commercial/strategic value of dual-use technology — it transfers both to a prohibition regime. Suppression (0.72) is high because the reading's persistence depends on active normative enforcement (TPNW, ICJ opinions, humanitarian initiatives) against the structural power of nuclear weapon states. Theater (0.42) is moderate: the NPT review process performs compliance rituals while the prohibition norm advances outside it. Accessibility collapse (0.35) is low because alternative readings (grand bargain, nonproliferation primary) remain live and institutionally embedded. Resistance (0.68) is high because nuclear weapon states and their allies actively reject this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the abolitionist seat, this is a coordination rope turning into a prohibition scaffold — it solves the collective action problem of nuclear apartheid. From the nuclear weapon state seat, it is a snare: a categorical demand that extracts their security doctrine's legal foundation while offering no reciprocal security guarantee. The engine computes this divergence from the structural data (identity_locked exit for nuclear weapon states vs. constrained/mobile for beneficiaries).
 *
 * DIRECTIONALITY LOGIC:
 *   Non-weapon states and civil society are structural beneficiaries (d ~ 0.15–0.25): they gain normative leverage without bearing enforcement costs. Humanitarian law institutions are agenda-setters (d ~ 0.1): they interpret but do not extract. Nuclear weapon states are identity-locked payers (d ~ 0.9): their deterrence identity fuses with the constraint's target — exit requires renouncing nuclear status, which is politically existential. Dual-use exporters and nuclear industry are constrained payers (d ~ 0.75): they bear compliance costs but retain some commercial exit. The nonproliferation bureaucracy is excluded (d ~ 0.6): trapped in an institutional role the reading undermines.
 *
 * MANDATROPHY ANALYSIS:
 *   The NPT's founding mandate (managed nonproliferation with disarmament horizon) has atrophied into a discriminatory status quo. This reading does not suffer mandatrophy — it reactivates the mandate's disarmament limb as a prohibition norm. The risk is misclassification: the grand bargain reading calls this extraction; the nonproliferation reading calls it incoherent. The engine's per-seat computation prevents either mislabeling by showing the reading is coordination for some, extraction for others — a true tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prohibition_norm_legal_status,
    'Does the humanitarian prohibition norm (TPNW, ICJ 1996) have binding legal force on non-party nuclear weapon states, or is it an aspirational political commitment?',
    'ICJ advisory opinion on TPNW obligations erga omnes; state practice regarding TPNW norms in non-party states; UNGA resolutions on nuclear disarmament as customary law.',
    'If binding, nuclear weapon states are in continuous legal violation — extractiveness and suppression are structurally necessary. If aspirational, the reading''s coercive force is political, not legal — reclassification toward rope or scaffold may follow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibition_norm_legal_status, conceptual, 'Legal status of the prohibition norm against non-party nuclear weapon states.').

omega_variable(
    peaceful_military_distinction_collapse,
    'Is the peaceful/military nuclear distinction empirically collapsible (all enrichment/reprocessing is dual-use) or legally maintainable under verification?',
    'Technical assessment of breakout timelines and safeguards detectability for enrichment/reprocessing; historical record of diversion from civilian programs.',
    'If collapsible, Article IV''s ''inalienable right'' is structurally incoherent — the reading''s categorical prohibition follows. If maintainable, the reading overreaches — a more nuanced conditional-right reading (grand bargain) may be structurally superior.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peaceful_military_distinction_collapse, empirical, 'Whether the technical distinction between peaceful and military nuclear programs can be sustained.').

omega_variable(
    committer_frame_legitimacy,
    'Is this reading a legitimate instantiation of the NPT kernel, or does it constitute a distinct kernel (TPNW) projected backward onto NPT text?',
    'Treaty interpretation methodology (VCLT Arts. 31-33): subsequent practice, subsequent agreements, object and purpose. Compare TPNW''s explicit prohibition with NPT''s Article VI ''pursue negotiations''.',
    'If distinct kernel, this constraint story should be authored as TPNW_prohibition, not NPT_reading — affects network edges and family decomposition. If legitimate NPT reading, the kernel family structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_legitimacy, conceptual, 'Whether the abolitionist reading is internal to NPT or an external kernel projection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(npt__tr_t1995, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2010, 0.33).
narrative_ontology:measurement(npt__tr_t2017, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2017, 0.38).
narrative_ontology:measurement(npt__tr_t2025, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1985, 0.22).
narrative_ontology:measurement(npt__be_t1995, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(npt__be_t2017, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2017, 0.71).
narrative_ontology:measurement(npt__be_t2025, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1970, 0.25).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(npt__su_t1995, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2010, 0.61).
narrative_ontology:measurement(npt__su_t2017, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2017, 0.68).
narrative_ontology:measurement(npt__su_t2025, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__abolitionist, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, tpnw_prohibition_regime).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, icj_1996_advisory_opinion_authority).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, humanitarian_consequences_initiative).

% DUAL FORMULATION NOTE:
% This is the abolitionist reading of the npt_article_iv_vi_pairing kernel. It differs from the nonproliferation_primary reading in epsilon (0.78 vs ~0.35), claimed_type (tangled_rope vs rope), and beneficiary/victim structure (non-weapon states benefit here; weapon states benefit in nonproliferation_primary). It differs from the grand_bargain reading in the categorical vs. conditional nature of the Article IV/VI linkage. All three readings share the same treaty text but instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__abolitionist, institutional, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
