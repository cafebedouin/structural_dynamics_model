% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__universalist_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Universalist Jurisdiction Mandate
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint instantiates the universalist reading of the
 *   rome_statute_jurisdiction kernel. It treats the Rome Statute as
 *   establishing a universal mandate for international criminal justice that
 *   transcends state consent, extending ICC jurisdiction to non-party
 *   nationals via territorial presence on a state party's soil or through
 *   UNSC Chapter VII referral. The reading is contested by a sovereigntist
 *   reading (strict consent required) and a hybrid complementarity reading
 *   (balance between universal aspiration and sovereign primacy). Those
 *   siblings are modeled as separate constraints in the same family.
 *
 * KEY AGENTS:
 *   - ICC Office of the Prosecutor (agenda_setter): Asserts and operationalizes the universalist jurisdictional claims.
 *   - UN Security Council (agenda_setter): Can trigger jurisdiction over non-party situations via referral.
 *   - Victims of core crimes (beneficiary): Potential recipients of justice and reparations under the expanded mandate.
 *   - Human rights advocacy networks (beneficiary): Derive normative leverage and institutional support from the court's broad claims.
 *   - Non-party states (payer): Bear sovereignty costs and compliance pressure without having consented to the Statute.
 *   - Targeted individuals (payer): Non-party nationals exposed to prosecution without their state's consent.
 *   - International law scholars (observer): Analytical seat constructing and contesting the universalist interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.72).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.68).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universalist Jurisdiction Mandate").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, '250c397f-cfd1-49bf-92cf-212af0eac159').
narrative_ontology:cs_kernel_codification('250c397f-cfd1-49bf-92cf-212af0eac159', formalized).
narrative_ontology:cs_authority_grounding('250c397f-cfd1-49bf-92cf-212af0eac159', lineage).
narrative_ontology:cs_interpretation_layer_present('250c397f-cfd1-49bf-92cf-212af0eac159').
narrative_ontology:cs_reading_relation('250c397f-cfd1-49bf-92cf-212af0eac159', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('250c397f-cfd1-49bf-92cf-212af0eac159', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('250c397f-cfd1-49bf-92cf-212af0eac159', foundational, core_crimes_concern_all_humanity).
narrative_ontology:cs_axiom_status(core_crimes_concern_all_humanity, holdable).
narrative_ontology:cs_axiom_grounding('250c397f-cfd1-49bf-92cf-212af0eac159', core_crimes_concern_all_humanity, deontological).
narrative_ontology:cs_axiom('250c397f-cfd1-49bf-92cf-212af0eac159', foundational, icc_jurisdiction_non_party_territorial).
narrative_ontology:cs_axiom_status(icc_jurisdiction_non_party_territorial, holdable).
narrative_ontology:cs_axiom_grounding('250c397f-cfd1-49bf-92cf-212af0eac159', icc_jurisdiction_non_party_territorial, conventional).
narrative_ontology:cs_reference_frame('250c397f-cfd1-49bf-92cf-212af0eac159', rome_statute_universal_authority).
narrative_ontology:cs_drift_state('250c397f-cfd1-49bf-92cf-212af0eac159', contemporary_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('250c397f-cfd1-49bf-92cf-212af0eac159', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, human_rights_advocacy_networks).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, targeted_individuals).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, universal_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, erga_omnes_obligations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates investigations and asserts jurisdiction over non-party nationals on the territory of state parties or via UNSC referral, independent of the suspect's home state consent. Controls the docket and legal strategy that operationalizes the universalist reading.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_office_of_prosecutor, agenda_setter,
    institutional, generational, analytical, global).

% Can activate ICC jurisdiction over non-party states by referring a situation under Chapter VII, bypassing the consent requirement entirely. Its referrals create the strongest form of the universalist mandate in action.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, un_security_council, agenda_setter,
    institutional, immediate, analytical, global).

% Rely on the ICC as a court of last resort when national systems are unwilling or unable to prosecute genocide, crimes against humanity, or war crimes. The universalist mandate extends this potential remedy to victims regardless of whether the perpetrator's state is a party.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes, beneficiary,
    powerless, biographical, trapped, global).

% Derive institutional leverage, funding, and normative momentum from a standing international court with expansive jurisdictional claims. Use the universalist mandate to pressure states and sustain a global accountability agenda.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, human_rights_advocacy_networks, beneficiary,
    organized, generational, mobile, global).

% Bear sovereignty costs when their nationals are prosecuted or when their territorial jurisdiction is used as a hook for ICC authority without their treaty consent. Face diplomatic and reputational pressure to cooperate despite never having joined the Statute.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_states, payer,
    institutional, generational, constrained, national).

% Non-party nationals exposed to prosecution for conduct on territorial state party soil or via UNSC referral. They lack the sovereign consent barrier that would otherwise shield them from ICC arrest warrants and trial.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, targeted_individuals, payer,
    powerless, biographical, trapped, global).

% Construct, critique, and defend the universalist interpretation of Articles 12 and 13. Their professional discourse shapes whether the mandate is read as transcending consent or as treaty-bound and conditional.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a standing international criminal court to end impunity for genocide, crimes against humanity, war crimes, and aggression when national jurisdictions are unwilling or unable to act, providing a centralized accountability mechanism for the international community.
% TRANSFER_FUNCTION: Transfers prosecutorial authority and sovereignty costs from non-party states and alleged perpetrators to the ICC and its beneficiaries; moves compliance obligations and reputational exposure onto all states, including those that never consented.
% ABSENT_VOICES: Non-party state governments and their domestic legal communities are structurally underrepresented in the universalist reading's normative architecture; major powers outside the Rome Statute system (U.S., Russia, China, Israel) dispute the mandate but are treated as jurisdictional subjects rather than partners in its design.
% DISAPPEARANCE_RATIONALE: If the universalist mandate vanished overnight, non-party states would regain a sovereign consent barrier over their nationals, the OTP could no longer assert territorial jurisdiction over non-party nationals without explicit UNSC cover, and the global human rights network would lose its most expansive legal lever; international criminal law would revert toward a consent-based or ad hoc tribunal model.
% FOUNDING_PROBLEM: The most serious international crimes were either prosecuted only by ad hoc victors' tribunals or not at all; national courts were frequently unwilling or unable to prosecute senior perpetrators, and the international community lacked a permanent, impartial mechanism to deter and punish core crimes.
% FOUNDING_PROBLEM_CORROBORATION: The Nuremberg and Tokyo precedents and the Yugoslav and Rwanda ad hoc tribunals corroborate the historical problem from outside the ICC beneficiary set. However, major non-party states and realist international relations scholars contest that a permanent supranational court is the necessary solution, attesting instead that national jurisdictions, hybrid tribunals, or political settlements remain adequate.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the universalist mandate asserts authority over actors who never consented, extracting sovereignty from non-party states and liberty from targeted individuals. Suppression is substantial (0.68) because the constraint's persistence depends on overriding the default sovereignty barrier and on institutional pressure to isolate non-cooperating states. Theater_ratio is moderate (0.45): the Court has issued genuine arrest warrants and trials, but a growing share of its universalist activity consists of unenforced warrants and normative proclamations that outrun actual custody. Accessibility_collapse is moderate (0.60): national courts remain available in principle via complementarity, but the universalist claim normatively collapses the alternative of sovereign immunity for core crimes. Resistance is high (0.75) because major non-party states actively contest the mandate, withhold cooperation, and in some cases have withdrawn from or denounced the Statute.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (non-party states, targeted individuals) and the beneficiary/agenda-setter seats should compute to markedly different types. From the ICC and human rights networks, the constraint is genuine coordination toward ending impunity; from non-party states, it is unconsented extraction of sovereign authority. The engine computes this divergence from the structural data rather than from any authored classification override.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (victims_of_core_crimes, human_rights_advocacy_networks) sit near the full-beneficiary end: they collect justice access and normative leverage without bearing sovereignty costs. Victims are powerless and trapped, which would amplify extraction if they were targets, but their beneficiary role inverts the effective extraction into subsidy. The payer seats (non_party_states with constrained exit, targeted_individuals with trapped exit) sit near the full-target end, amplifying effective extraction. The agenda-setter seats (ICC OTP, UNSC) are not in either beneficiary or victim array and receive the canonical fallback for institutional power, which sits closer to the beneficiary side than the target side.
 *
 * MANDATROPHY ANALYSIS:
 *   The universalist mandate prevents mislabeling as pure extraction because a genuine coordination function is present: the Court prosecutes atrocity crimes that national systems often ignore, and victims receive real procedural standing. It prevents mislabeling as pure coordination because the jurisdictional claim is asymmetrically imposed on non-consenting states and their nationals, creating identifiable sovereignty and liberty costs that require active enforcement and normative suppression to maintain. The metrics are authored to reflect this hybridity independently of the claimed type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rome_statute_kernel_reading_contest,
    'This constraint instantiates the universalist reading of the rome_statute_jurisdiction kernel. How would the structural classification change under the sovereigntist or hybrid sibling readings?',
    'Comparative analysis of sibling constraint stories in the same family; the disagreement is located on the interpretation of Articles 12 and 13 and the legal status of non-party state nationals.',
    'A sovereigntist reading would reduce extractiveness and shift claimed_type toward rope or scaffold by making jurisdiction conditional on consent. A hybrid reading would produce a tangled_rope with moderated epsilon, as complementarity would act as a stronger sovereignty shield.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rome_statute_kernel_reading_contest, conceptual, 'Kernel reading contest location and sibling structural deltas').

omega_variable(
    universal_jurisdiction_customary_basis,
    'Does the universalist jurisdictional claim over non-party nationals derive solely from the Rome Statute treaty, or from independent customary international law that would bind non-parties even absent the Statute?',
    'International Court of Justice advisory opinion or systematic analysis of state practice and opinio juris regarding universal jurisdiction for core crimes.',
    'If purely conventional, the constraint''s persistence depends on treaty ratification and UNSC dynamics. If customary, it claims greater durability, but declaring beneficiaries on a mountain-like claim would trigger false-summit evaluation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_jurisdiction_customary_basis, conceptual, 'Basis of universal jurisdiction in treaty vs customary law').

omega_variable(
    enforcement_sovereignty_tension,
    'Is the ICC''s reliance on state cooperation for arrest and surrender a fundamental limit that makes the universalist mandate largely theatrical, or does normative pressure from arrest warrants and diplomatic stigma constitute real extraction?',
    'Longitudinal study of compliance rates with ICC arrest warrants and cooperation requests across state parties and non-parties.',
    'If compliance is negligible, theater_ratio should be higher and effective extraction lower. If normative pressure operates even without physical custody, extraction is real and the current metrics are validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_sovereignty_tension, empirical, 'Theater vs real extraction in ICC enforcement gap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 0, 22).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__universalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(rome_tr_t4, rome_statute_jurisdiction__universalist_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(rome_tr_t8, rome_statute_jurisdiction__universalist_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(rome_tr_t12, rome_statute_jurisdiction__universalist_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement(rome_tr_t16, rome_statute_jurisdiction__universalist_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(rome_tr_t22, rome_statute_jurisdiction__universalist_reading, theater_ratio, 22, 0.45).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(rome_be_t4, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(rome_be_t8, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(rome_be_t12, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement(rome_be_t16, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(rome_be_t22, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 22, 0.72).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(rome_statute_jurisdiction__universalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% The Rome Statute jurisdiction kernel decomposes into three structurally distinct readings: universalist (high extraction via sovereignty override), sovereigntist (conditional consent-based jurisdiction), and hybrid complementarity (balanced coordination with sovereign primacy). Each reading has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
