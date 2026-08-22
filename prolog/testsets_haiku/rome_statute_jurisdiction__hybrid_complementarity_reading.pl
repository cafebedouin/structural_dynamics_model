% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Complementarity Mechanism (Hybrid Reading)
 *   domain: international_law/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute (1998) establishes the International Criminal Court with
 *   jurisdiction over genocide, crimes against humanity, war crimes, and
 *   aggression. The constraint under analysis is the complementarity
 *   mechanism — a doctrine holding that the ICC is a court of last resort,
 *   with primary criminal justice authority residing in state legal systems.
 *   This reading (hybrid_complementarity_reading) interprets the Rome Statute
 *   as grounding ICC authority in a dual legitimacy: universal aspiration
 *   (derived from natural law conceptions of international crimes as
 *   violations of humanity itself, not mere treaty violation) combined with
 *   pragmatic deference to state sovereignty. The ICC has residual universal
 *   jurisdiction — the authority exists — but operationally exercises it only
 *   after state systems are deemed unwilling or unable to prosecute. The
 *   hybrid reading differs from the sovereigntist reading (which treats
 *   complementarity as a ceiling, not a floor — jurisdiction exists only by
 *   state consent) and the universalist reading (which treats complementarity
 *   as a procedural courtesy, not a structural constraint — the ICC has
 *   primary authority and state deference is discretionary). The authored
 *   extractiveness (0.38) reflects that the constraint does impose costs on
 *   sovereigntist governments (exposure to ICC prosecution without their
 *   consent) and non-signatories (subject to ICC reach through state-party
 *   cooperation), but the extraction is moderate because complementarity
 *   operationally constrains ICC reach and most state parties retain primacy
 *   in practice. The measurement series shows slow rise in extractiveness and
 *   theater ratio through the interval, reflecting gradual hardening of
 *   complementarity doctrine via case law and states' experience with ICC
 *   probes.
 *
 * KEY AGENTS:
 *   - International Criminal Court — institutional agenda-setter; interprets Rome Statute; operationally defers to state cooperation; benefits from complementarity as a legitimacy frame
 *   - State parties to Rome Statute — beneficiaries and constrained payers; retain primary jurisdiction but accept ICC supervision; coordinate with ICC on investigation and prosecution
 *   - Non-signatory states and sovereigntist governments — payers; subject to ICC reach without consent; resist the hybrid reading's authority grounding
 *   - Universal justice advocates — organized beneficiaries; press for maximum ICC jurisdiction; benefit from asserting residual universal authority
 *   - UNSC permanent members — institutionally privileged; functionally exempted from aggression-crimes jurisdiction; outside complementarity deference
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.38).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.22).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Complementarity Mechanism (Hybrid Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, '8b0b9b12-173f-43d4-b732-cc8f8902c3e6').
narrative_ontology:cs_kernel_codification('8b0b9b12-173f-43d4-b732-cc8f8902c3e6', formalized).
narrative_ontology:cs_authority_grounding('8b0b9b12-173f-43d4-b732-cc8f8902c3e6', extraction).
narrative_ontology:cs_interpretation_layer_present('8b0b9b12-173f-43d4-b732-cc8f8902c3e6').
narrative_ontology:cs_reading_relation('8b0b9b12-173f-43d4-b732-cc8f8902c3e6', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b0b9b12-173f-43d4-b732-cc8f8902c3e6', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('8b0b9b12-173f-43d4-b732-cc8f8902c3e6', foundational, complementarity_operationally_constrains_jurisdiction).
narrative_ontology:cs_axiom_status(complementarity_operationally_constrains_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('8b0b9b12-173f-43d4-b732-cc8f8902c3e6', complementarity_operationally_constrains_jurisdiction, deontological).
narrative_ontology:cs_axiom('8b0b9b12-173f-43d4-b732-cc8f8902c3e6', foundational, universal_crimes_ground_icc_authority_alongside_consent).
narrative_ontology:cs_axiom_status(universal_crimes_ground_icc_authority_alongside_consent, holdable).
narrative_ontology:cs_axiom_grounding('8b0b9b12-173f-43d4-b732-cc8f8902c3e6', universal_crimes_ground_icc_authority_alongside_consent, deontological).
narrative_ontology:cs_reference_frame('8b0b9b12-173f-43d4-b732-cc8f8902c3e6', rome_statute_balanced_authority).
narrative_ontology:cs_drift_state('8b0b9b12-173f-43d4-b732-cc8f8902c3e6', contemporary_enforcement_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8b0b9b12-173f-43d4-b732-cc8f8902c3e6', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, universal_jurisdiction_advocates).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, non_signatory_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, sovereigntist_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_to_rome_statute).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, universal_justice_advocates).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, permanent_security_council_members).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_justice_mandate).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, state_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and administers the Rome Statute; claims residual universal jurisdiction over genocide, crimes against humanity, war crimes, and aggression; operationally defers to state prosecution via complementarity doctrine; depends entirely on state parties for cooperation in investigation, arrest, and prosecution. The hybrid reading assigns the ICC structural authority that it then willingly constrains by doctrine rather than treaty requirement — the Court exists as a backstop for cases where states fail, but exercises this authority only through state-cooperative channels.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court, agenda_setter,
    institutional, generational, analytical, global).

% Have committed via treaty ratification to cooperate with the ICC in investigation and prosecution of core crimes. They benefit from a unified legal framework replacing ad-hoc tribunals, gain legitimacy by alignment with international norms, and retain primary prosecutorial authority over their own nationals and crimes on their territory. They can withdraw after seven-year notice period (exit option), but withdrawal carries significant reputational cost. Their cooperation is voluntary and constitutive of ICC power — without state assistance in investigation and arrest, the Court is operationally paralyzed.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_to_rome_statute, beneficiary,
    institutional, generational, mobile, global).

% Are subject to ICC jurisdiction when crimes are committed on the territory of a state party or by nationals of a state party (Rome Statute Article 12), without having consented to the treaty. They do not participate in Rome Statute governance, cannot vote on Assembly decisions, and cannot leverage state-party cooperation mechanisms in their favor. Their nationals can be prosecuted based on territorial jurisdiction triggered by another state's membership. They can refuse bilateral cooperation with the ICC but at diplomatic and economic cost; they cannot exit the regime (no seven-year withdrawal option because they never consented). The hybrid reading positions them as bound by universal aspiration even without their consent.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_signatory_states, payer,
    powerful, generational, constrained, global).

% Reject the premise that international courts can legitimately override state prosecutorial sovereignty. They may be state parties (and can withdraw) or non-signatories (constrained exit). Their structural position is active resistance to the hybrid reading's authority grounding, which they see as assertion of universal jurisdiction disguised as complementary deference. They bear the cost of ICC investigations targeting their nationals or leaders, the reputational cost of non-cooperation, and the institutional cost of defending against ICC claims. They contest the reading's framing at every stage — in legal arguments, state-party forums, and customary-law development.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, sovereigntist_governments, payer,
    powerful, generational, mobile, global).

% Include human rights organizations, accountability coalitions, and victims' advocacy groups that press for maximum ICC jurisdiction and enforcement. They benefit from the hybrid reading's assertion of residual universal authority because it legitimates ICC expansion and investigation even when state parties withhold full cooperation. They do not set the ICC's agenda or operations but exercise influence through advocacy, litigation (amicus briefs), and public pressure. They have analytical exit — they can withdraw support if the ICC fails to meet accountability standards, but this is a choice rather than a structural constraint.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, universal_justice_advocates, beneficiary,
    organized, generational, analytical, global).

% Are formally state parties to the Rome Statute (or some are non-signatories) but are functionally exempted from complementarity deference. The Kampala decision (2018) authorized indefinite deferral of aggression-crimes investigations when the UNSC requests it — a power exercised to shield permanent members' nationals from prosecution. They participate in Rome Statute governance but have leveraged outside-treaty authority (UNSC prerogatives) to constrain the Court's reach. Their exclusion from complementarity constraint reveals the hybrid reading's structural asymmetry: not all jurisdictional subjects experience the same authority structure. They are trapped by the constraint (cannot fully exit without reputational cost) but benefited by its exceptions.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, permanent_security_council_members, excluded,
    institutional, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, permanent_security_council_members, beneficiary).

% Seek ICC investigation and prosecution when domestic legal systems have failed, are weaponized against victims, or are unable to reach powerful perpetrators. They participate in ICC proceedings as witnesses and through victim participation procedures, giving them a structural voice in the Court's operations. However, the constraint's gatekeeping (complementarity deference to state prosecution, state cooperation requirements for investigation and arrest) means most victim groups cannot directly access ICC jurisdiction. Their testimony and advocacy influence ICC interpretation pressure, but they remain dependent on state-party governments' willingness to cooperate. They are trapped in the constraint's reach (cannot exit) but have limited power over whether the ICC acts.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, transnational_victims_and_witness_movements, observer,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__hybrid_complementarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces fragmented post–Cold War system of national prosecutions and ad-hoc tribunals with a unified, standing international criminal court that applies consistent law to the four most serious crimes and reduces the burden on individual state systems to prosecute complex transnational crimes.
% TRANSFER_FUNCTION: Transfers prosecutorial authority over core international crimes from individual sovereigns to the ICC, operationally gated by complementarity deference so that state systems retain primary jurisdiction and the ICC activates when states are unwilling or unable to prosecute.
% ABSENT_VOICES: Non-signatories cannot participate in Rome Statute governance and cannot vote on Assembly of States Parties decisions that shape complementarity doctrine; they experience the constraint but have no institutional voice in its development. Sovereigntist governments that reject the ICC's authority grounding are inside the formal regime (if they are state parties) but outside the consensus on authority legitimacy — their objections are heard but structurally overruled. Victims' movements without powerful state patronage lack direct voice in complementarity deference decisions that determine whether their cases are investigated.
% DISAPPEARANCE_RATIONALE: If complementarity doctrine disappeared, states would lose the buffer of ICC oversight; ICC would gain primary jurisdiction over all four core crimes; non-signatories would either submit to universal jurisdiction (shifting toward universalist reading) or major powers would withdraw and build parallel regimes (shifting toward sovereigntist reading). State-party cooperation regimes would restructure. The accountability architecture would reorganize around either pure universal authority (universalist) or pure state-consent regimes (sovereigntist) — the hybrid equilibrium would collapse.
% FOUNDING_PROBLEM: Post–Cold War accountability crisis: states were committing mass atrocities (Rwanda, Bosnia, Cambodia) without international consequences because state-level prosecution was politically paralyzed or structurally incapable; ad-hoc tribunals were expensive and temporary. The Rome Statute answered by creating a standing court, but states were unwilling to surrender sovereign prosecutorial authority; complementarity doctrine bridged the tension by positioning the ICC as a backup that activates when state systems fail.
% FOUNDING_PROBLEM_CORROBORATION: ICC prosecutors and advocacy organizations attest the founding problem is live: state systems routinely fail to prosecute, are weaponized against victims, or lack capacity to handle complex transnational crimes. Sovereigntist states and legal scholars attest the problem has shifted: post-1990 most state parties have functional courts; the operative constraint is now political will and great-power immunity, not institutional absence. Independent empirical analysis (Human Rights Watch, Amnesty International, academic studies) confirms that complementarity deference is honored inconsistently — some states cooperate fully, others invoke complementarity to block ICC action, and non-cooperative states face no enforcement cost except diplomatic pressure. The problem has not disappeared; it has transmuted into a question about whether state cooperation can be compelled and whether complementarity is constraint or excuse.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).
:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.38 is moderate because complementarity doctrine, operationally honored, limits ICC reach in practice. The ICC cannot force state prosecution; it can only investigate and prosecute when a state is unwilling or unable. This gatekeeping makes the effective extraction lower than if the ICC had primary/concurrent jurisdiction. Suppression is low (0.22) because the constraint relies on voluntary state cooperation more than coercion; sovereigntist states can resist by refusing to cooperate, and the ICC has no enforcement machinery independent of state will. Theater ratio rises slowly (0.08 to 0.18) because early ICC operations emphasized genuine accountability work, but as the caseload has broadened and states have used complementarity deferral as a stalling tactic, the performative share has increased — complementarity language is now used both to legitimize ICC action and to block it. Accessibility collapse is moderate (0.45) because non-signatories do have exit options (some can refuse cooperation, can defend nationals at the ICC, can withdraw after a seven-year period) and state parties can calibrate their level of engagement. Resistance is substantial (0.58) because sovereigntist governments actively contest the ICC's authority interpretations, major powers use alternative venues (UNSC, bilateral immunity agreements), and the constraint's survival depends on continuous state-party buy-in, which is not assured. The measurement trajectory shows the constraint stabilizing rather than escalating: extractiveness rises to t=25 and then plateaus, theater ratio similarly plateaus, suggesting the hybrid reading has found an equilibrium where neither universalist nor sovereigntist pressure has definitively displaced it.
 *
 * PERSPECTIVAL GAP:
 *   From the ICC's perspective, complementarity is a wise delegation doctrine that respects state capacity and legitimacy while preserving residual universal authority for cases where states fail — a coordination mechanism balancing accountability and sovereignty. From sovereigntist governments' perspectives, complementarity is a cover story: it asserts universal jurisdiction that exists in fact and uses state cooperation as a temporary workaround, not as a structural constraint. From non-signatories' perspectives, complementarity is a mechanism of subordination — they are bound by a regime they did not consent to, via the state-party system's ability to trigger ICC jurisdiction on their territory or nationals. From universal justice advocates' perspectives, complementarity is a regrettable compromise with state power: it allows powerful states to block ICC action by withholding cooperation, making accountability selective rather than universal. These perspectives are not reconcilable within a single reading; the engine computes them as per-seat classification divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC sits at d near 0.2–0.3 (beneficiary): it collects institutional authority and operates the constraint's machinery, though it does so voluntarily constrained by complementarity doctrine. State parties sit near d=0.5 (symmetric): they benefit from coordinated accountability and ICC legitimacy but bear the cost of ICC oversight and reduced prosecutorial discretion; they retain exit (mobile) so directionality is moderated. Non-signatories sit near d=0.75 (high target): they bear exposure to ICC jurisdiction without consent and cannot control the mechanism; they have constrained exit (can resist cooperation but at diplomatic cost). Sovereigntist governments, whether signatories or non-signatories, experience high d because they reject the authority grounding; their resistance costs (reputational, legal, institutional) are high. Universal justice advocates sit near d=0.1 (beneficiary): they do not run the ICC but benefit from its expanded reach; they have analytical exit options. The UNSC permanent members occupy a contradictory position: formally state parties (d moderated) but functionally privileged by deferral power (d effectively inverted); this contradiction is a feature of the hybrid reading, not a bug — the exclusion of UNSC powers from complementarity deference reveals that the constraint's fairness is asymmetric, benefiting great powers and the ICC unequally.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading avoids false designation as rope by acknowledging the asymmetry: complementarity is not pure coordination because non-signatories and sovereigntist governments are subjected without participation. It avoids false designation as snare by recognizing genuine coordination benefits for state parties (unified legal framework, reduced burden of ad-hoc tribunals) and the real authority constraint that complementarity imposes on the ICC. The reading's stability depends on both the universal aspiration (the ICC's authority grounding) and pragmatic state cooperation (the complementarity deference) being honored. If the universal aspiration is abandoned, the constraint becomes pure sovereigntist coordination (rope). If the complementarity deference is abandoned, the constraint becomes universalist extraction (snare from non-signatories' perspective). The mandate — international criminal justice — remains live (not dead) for state parties and advocates, but contested for sovereigntist governments and non-signatories who experience the mandate as overreach. The constraint resolves mandatrophy by maintaining both elements in tension: the ICC asserts universal authority but operationally constrains itself by state cooperation, preserving legitimacy with both coordination-seeking state parties and universal-aspiration advocates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_operationalization_ambiguity,
    'Is complementarity operationally constraining ICC jurisdiction in practice, or is it a legitimacy frame the ICC uses while retaining de facto primary jurisdiction?',
    'Empirical analysis of ICC case selection over time: if the ICC systematically prosecutes cases where state prosecution is available (and state parties do not block), complementarity is rhetorical; if the ICC systematically defers when state systems exist, complementarity is structural.',
    'If rhetorical, the constraint is functionally closer to snare (extraction via authority claim + limited deference); if structural, the hybrid reading holds and extractiveness is accurate. Complementarity-as-performance would indicate rising theater ratio and hidden extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_operationalization_ambiguity, empirical, 'Whether complementarity doctrine operationally gates ICC jurisdiction or is used selectively.').

omega_variable(
    universal_authority_grounding_contestation,
    'Can the ICC''s authority legitimately derive from natural law conceptions of universal crimes, or is all ICC authority contingent on Rome Statute state consent?',
    'Legal scholarship and ICC jurisprudence evolution: does the ICC ground jurisdiction in the treaty alone (sovereigntist) or in treaty-plus-universal-aspiration (hybrid) or in universal principle with treaty as one vehicle (universalist)?',
    'If authority must be treaty-contingent, the sovereigntist reading forecloses the hybrid reading and the constraint becomes pure state-consent-based coordination. If universal aspiration is legitimate, the hybrid and universalist readings remain live. If universal aspiration is primary, the universalist reading displaces the hybrid reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_authority_grounding_contestation, conceptual, 'Whether the ICC''s authority grounding is purely treaty-consent or includes natural law elements.').

omega_variable(
    non_signatory_jurisdiction_legitimacy,
    'Can the Rome Statute legitimately bind non-signatories to ICC jurisdiction via Article 12 (territoriality and nationals), or does Article 12 constitute illegitimate assertion of authority over non-consenting states?',
    'Sovereigntist state challenges and customary international law development: if non-signatories establish a counter-norm rejecting Article 12 jurisdiction, the constraint''s reach shrinks; if customary practice solidifies Article 12, the constraint''s universal reach is confirmed.',
    'If Article 12 is illegitimate, the constraint becomes a rope (state-party-only coordination) and extractiveness drops to ~0.2 because non-signatories are removed from the victim set. If Article 12 is legitimate, the hybrid reading holds and non-signatories remain payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_signatory_jurisdiction_legitimacy, empirical, 'Whether Article 12 jurisdiction over non-signatories is customarily accepted or contested.').

omega_variable(
    hybrid_reading_stability,
    'Can the hybrid reading (universal aspiration + complementarity deference) remain stable as an equilibrium, or will jurisdictional conflicts force resolution toward either sovereigntist or universalist poles?',
    'Trajectory analysis: monitor whether major powers (especially UNSC permanent members) shift toward withdrawal/non-cooperation (sovereigntist pressure) or whether ICC powers expand despite state-party resistance (universalist pressure). Key moment: aggression-crimes deferral renewal and major-power compliance.',
    'Instability toward sovereigntism would manifest as rising withdrawal rate, deferral extensions, and reduced state cooperation (extractiveness stable but suppression/theater rising). Instability toward universalism would manifest as ICC expansion despite state resistance and increasing non-cooperation (extractiveness rising, suppression rising, theater rising).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_stability, empirical, 'Whether the hybrid equilibrium is self-sustaining or vulnerable to displacement by sovereigntist or universalist pressure.').

omega_variable(
    unsc_permanent_member_privilege_constitutionality,
    'Does the UNSC deferral power over aggression crimes (Kampala decision) constitute a legitimate exception to universal jurisdiction, or is it an illegitimate privilege for great powers?',
    'Challenge to the deferral mechanism at the ICC and within state parties: if major powers defend deferral as necessary for great-power buy-in, the hybrid reading accommodates structural inequality. If states pressure to remove the deferral, the reading shifts toward universalist.',
    'If deferral is legitimated, the constraint''s extractiveness from non-UNSC states rises (they are more exposed) and from UNSC states drops (they are privileged). If deferral is eliminated, extractiveness becomes more uniform but universalist pressure increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unsc_permanent_member_privilege_constitutionality, preference, 'Whether great-power exemption from ICC jurisdiction is legitimate under the Rome Statute.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(rome_tr_t0, observed).
narrative_ontology:measurement(rome_tr_t5, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(rome_tr_t5, observed).
narrative_ontology:measurement(rome_tr_t10, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(rome_tr_t10, observed).
narrative_ontology:measurement(rome_tr_t15, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement_basis(rome_tr_t15, observed).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(rome_tr_t20, observed).
narrative_ontology:measurement(rome_tr_t25, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(rome_tr_t25, observed).
narrative_ontology:measurement(rome_tr_t30, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(rome_tr_t30, observed).
narrative_ontology:measurement(rome_tr_t35, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 35, 0.18).
narrative_ontology:measurement_basis(rome_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(rome_be_t0, observed).
narrative_ontology:measurement(rome_be_t5, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 5, 0.31).
narrative_ontology:measurement_basis(rome_be_t5, observed).
narrative_ontology:measurement(rome_be_t10, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement_basis(rome_be_t10, observed).
narrative_ontology:measurement(rome_be_t15, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement_basis(rome_be_t15, observed).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 20, 0.37).
narrative_ontology:measurement_basis(rome_be_t20, observed).
narrative_ontology:measurement(rome_be_t25, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(rome_be_t25, observed).
narrative_ontology:measurement(rome_be_t30, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(rome_be_t30, observed).
narrative_ontology:measurement(rome_be_t35, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement_basis(rome_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t0, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(rome_su_t0, observed).
narrative_ontology:measurement(rome_su_t5, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 5, 0.17).
narrative_ontology:measurement_basis(rome_su_t5, observed).
narrative_ontology:measurement(rome_su_t10, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 10, 0.19).
narrative_ontology:measurement_basis(rome_su_t10, observed).
narrative_ontology:measurement(rome_su_t15, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 15, 0.21).
narrative_ontology:measurement_basis(rome_su_t15, observed).
narrative_ontology:measurement(rome_su_t20, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement_basis(rome_su_t20, observed).
narrative_ontology:measurement(rome_su_t25, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 25, 0.22).
narrative_ontology:measurement_basis(rome_su_t25, observed).
narrative_ontology:measurement(rome_su_t30, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement_basis(rome_su_t30, observed).
narrative_ontology:measurement(rome_su_t35, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 35, 0.22).
narrative_ontology:measurement_basis(rome_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, unsc_security_council_enforcement).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, state_cooperation_mechanisms_international_law).

% DUAL FORMULATION NOTE:
% The Rome Statute jurisdiction constraint exists in three structurally distinct readings: hybrid_complementarity_reading (this file) interprets the statute as balancing universal aspiration with state sovereignty via operationally constraining complementarity doctrine; sovereigntist_reading treats complementarity as a ceiling on ICC authority (jurisdiction exists only by state consent); universalist_reading treats complementarity as discretionary (the ICC has primary authority over universal crimes). These are not the same constraint viewed differently — they have distinct ε values, distinct beneficiary/victim structures, and distinct authority groundings. They are linked via network.affects_constraints because each reading's stability depends on suppressing the others' core premises. The sovereigntist reading forecloses the hybrid reading if state consent is the only legitimate authority source; the universalist reading forecloses the hybrid reading if universal aspiration is primary. The hybrid reading influences both siblings by occupying the middle ground and thereby constraining how far each can extend.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
