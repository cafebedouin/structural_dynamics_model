% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Universal Jurisdiction — Universalist Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute, adopted in 1998 and entered into force in 2002, creates
 *   the International Criminal Court as a permanent institution to prosecute
 *   genocide, crimes against humanity, and war crimes. This constraint models
 *   ONE READING of the Statute's jurisdictional scope: the universalist
 *   reading asserts that the Statute establishes a mandate transcending state
 *   consent, such that the ICC can prosecute crimes occurring on party
 *   territory or referred by the UNSC regardless of the perpetrator's state's
 *   treaty status. Under this reading, the Statute's authority claims
 *   override national sovereignty for core crimes — a structural assertion
 *   that puts beneficiary victims and the ICC institution against the costs
 *   borne by non-party state nationals and state sovereigns. The sibling
 *   readings (sovereigntist and hybrid complementarity) take different
 *   structural positions on the same kernel: they argue the Statute requires
 *   tighter consent gates or emphasize domestic-court primacy. This file
 *   instantiates ONLY the universalist reading; its metrics and stakeholder
 *   structure describe this reading's own internal logic, not a comparison
 *   across readings.
 *
 * KEY AGENTS:
 *   - ICC institutional authority — agenda-setter; asserts universal jurisdiction; justifies expanded reach
 *   - victim_constituencies — beneficiary; gain access to justice transcending state boundaries
 *   - non_party_state_nationals — payer; face prosecution despite their state's non-consent
 *   - non_party_state_sovereigns — payer (and excluded); lose veto over ICC prosecution
 *   - icc_party_states — beneficiary; gain legitimacy and reach through the universalist reading
 *   - unsc_permanent_members — excluded; retain leverage via referral veto but lack prosecution immunity
 *   - complementarity_advocates — observer; contest the reading's interpretation of domestic-court primacy
 *   - universal_justice_advocates — beneficiary; actively champion the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.68).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.72).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universal Jurisdiction — Universalist Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, 'cc77775f-1d93-4437-b89c-31f6971f6e96').
narrative_ontology:cs_kernel_codification('cc77775f-1d93-4437-b89c-31f6971f6e96', formalized).
narrative_ontology:cs_authority_grounding('cc77775f-1d93-4437-b89c-31f6971f6e96', lineage).
narrative_ontology:cs_interpretation_layer_present('cc77775f-1d93-4437-b89c-31f6971f6e96').
narrative_ontology:cs_reading_relation('cc77775f-1d93-4437-b89c-31f6971f6e96', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc77775f-1d93-4437-b89c-31f6971f6e96', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('cc77775f-1d93-4437-b89c-31f6971f6e96', foundational, universal_accountability_transcends_consent).
narrative_ontology:cs_axiom_status(universal_accountability_transcends_consent, holdable).
narrative_ontology:cs_axiom_grounding('cc77775f-1d93-4437-b89c-31f6971f6e96', universal_accountability_transcends_consent, deontological).
narrative_ontology:cs_axiom('cc77775f-1d93-4437-b89c-31f6971f6e96', foundational, core_crimes_harm_humanity_itself).
narrative_ontology:cs_axiom_status(core_crimes_harm_humanity_itself, holdable).
narrative_ontology:cs_axiom_grounding('cc77775f-1d93-4437-b89c-31f6971f6e96', core_crimes_harm_humanity_itself, deontological).
narrative_ontology:cs_reference_frame('cc77775f-1d93-4437-b89c-31f6971f6e96', universal_justice_mandate).
narrative_ontology:cs_drift_state('cc77775f-1d93-4437-b89c-31f6971f6e96', contemporary_great_power_resistance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cc77775f-1d93-4437-b89c-31f6971f6e96', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_criminal_justice_system).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_state_nationals).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, state_sovereignty_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victim_constituencies).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, icc_party_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, universal_justice_advocates).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_state_sovereigns).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, universal_human_accountability_principle).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__universalist_reading, crimes_against_humanity_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ICC and its constituent organs (Pre-Trial Chamber, Trial Chamber, Office of the Prosecutor) exercise interpretive authority over the Rome Statute's jurisdiction clauses. Under the universalist reading, they assert authority to prosecute core crimes (genocide, crimes against humanity, war crimes) based on territorial presence or UNSC referral, regardless of state-party status. The institution's mandate and operational legitimacy rest on the expansive interpretation of universal jurisdiction; this reading justifies the institution's reach and activity level.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_institutional_authority, agenda_setter,
    institutional, generational, constrained, universal).

% Persons harmed by genocide, crimes against humanity, and war crimes gain theoretical access to justice regardless of the perpetrator's state's treaty status or their own. The universalist reading expands the population with standing to seek prosecution: a genocide victim from a non-party state gains ICC access if the crime occurred on party territory or the UNSC refers it, not merely if their state consented.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victim_constituencies, beneficiary,
    powerless, biographical, constrained, universal).

% Citizens of non-party states face ICC prosecution for actions taken in non-party territory if those actions occur on party territory or are triggered by UNSC referral. A national of a non-consenting state (e.g., a military officer) can be prosecuted for conduct in a party state or if the UNSC refers a situation retroactively. Exit is identity-locked: the status of nationality cannot be shed; geographic exit is possible but does not permanently escape the constraint.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_state_nationals, payer,
    powerless, biographical, identity_locked, universal).

% States that have not ratified the Rome Statute lose the formal veto over ICC prosecution of their nationals, but retain some structural leverage: they can refuse to surrender suspects (though this risks isolation), negotiate bilateral immunity agreements, or block UNSC referrals (if permanent members). The universalist reading subordinates the sovereign's consent to the treaty's universal jurisdiction logic; the state bears the structural cost of jurisdiction assertion it did not authorize.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_state_sovereigns, payer,
    institutional, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, non_party_state_sovereigns, excluded).

% States that ratified the Rome Statute participate in the Assembly of States Parties, theoretically setting policy, but operationally the universalist reading expands the institution's reach beyond what many drafting parties expected. The institutional benefit accrues through the system's broader legitimacy and the symbolic affirmation of universal justice norms, though this comes at the cost of sovereignty constraints that apply to all states equally.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_party_states, beneficiary,
    institutional, generational, constrained, universal).

% Hold veto over UNSC referrals, which are a primary trigger for ICC jurisdiction over non-party situations. The permanent members can block or shape referrals based on national interest; they are excluded from prosecution via the complementarity compromise (pre-negotiated immunity for UNSC permanent members and their troops). The universalist reading's reach is curtailed precisely where great-power interests lie.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, unsc_permanent_members, excluded,
    powerful, generational, arbitrage, universal).

% International lawyers, state delegations, and institutional actors who argue that the Rome Statute's complementarity principle (domestic courts have first dibs) should strictly gate ICC jurisdiction. Under the universalist reading, complementarity becomes a filter on *when* the ICC acts, not a substantive limit on *whether* it can claim jurisdiction. These advocates see the universalist reading as degrading domestic judicial sovereignty.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, complementarity_advocates, observer,
    institutional, generational, analytical, universal).

% NGOs, victim groups, and human-rights organizations that champion the universalist reading as the expression of the Statute's true purpose: ensuring no perpetrator of core crimes escapes accountability due to state non-compliance. These actors actively argue for expansive interpretation in filings, public advocacy, and state engagement.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, universal_justice_advocates, beneficiary,
    organized, generational, mobile, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__universalist_reading, icc_institutional_authority).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, treaty-constituted legal framework for prosecuting core international crimes (genocide, crimes against humanity, war crimes) on a basis that transcends individual state consent, so that perpetrators cannot hide behind non-party status or loci of sovereignty.
% TRANSFER_FUNCTION: Transfers prosecutorial authority from scattered domestic systems and ad-hoc tribunals into a single institutional body (the ICC) with permanent mandate; transfers the burden of enforcement compliance onto all states, including non-parties, by claiming jurisdiction over territorial events and UNSC-referred situations regardless of state treaty status.
% ABSENT_VOICES: Non-party states that would invoke strict sovereignty and consent-based limits; domestic judiciaries threatened by complementarity's secondary status; states with strong national security interests (esp. permanent UNSC members who negotiated immunity). These parties are structurally excluded from shaping the interpretation because the reading is framed as emerging from the Statute's text and the principle of universal human rights, not from consensual negotiation. Their objections are treated as obstructing justice rather than as legitimate sovereignty claims.
% DISAPPEARANCE_RATIONALE: If the universalist jurisdiction reading vanished overnight and the sovereigntist reading took hold, non-party states would regain effective veto over ICC prosecution of their nationals. Prosecution would contract to only party-consented situations. Perpetrators of core crimes would gain a route to escape ICC jurisdiction (non-party status or location outside party territory). The incentive structure for ratification and state cooperation would shift; the Statute would revert to a treaty of limited scope, not a universal commitment.
% FOUNDING_PROBLEM: After WWII and through the Cold War, major perpetrators of genocide and crimes against humanity escaped accountability because no forum existed to try them on a universal basis independent of victorious powers or state consent. The Rome Statute was drafted to establish that forum, grounded in the principle that core crimes harm humanity itself, not merely the state where they occur, and therefore require universal jurisdiction regardless of state boundaries.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the treaty's drafting history (the preamble invokes 'grave crimes that threaten the peace, security and well-being of the world') and by victim advocates who cite ongoing impunity as evidence the problem persists. Non-party states and sovereignty-prioritizing scholars attest that the problem has shifted: they argue the *present* problem is institutional overreach that violates state consent and domestic judicial primacy. The corroboration is split: universal-justice advocates and victim groups outside the beneficiary structure confirm the founding problem; sovereigntist commentators from non-party states argue the reading invents a problem (universal accountability mandate) that contradicts the treaty's text.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderately high (0.68) because the constraint asserts authority over non-consenting parties and imposes prosecution risk on non-party nationals without their state's authorization. However, it is not extreme (not 0.8+) because the constraint's scope is limited to specific crimes and the ICC lacks enforcement power without state compliance — it cannot capture suspects without cooperation. Suppression is substantial (0.72) because the reading's persistence depends on actively excluding or marginalizing sovereigntist and complementarity-based interpretations; the universalist reading must suppress legal and political challenges to the premise that universal justice can override state consent. Theater is moderate (0.41) because the ICC does undertake real prosecutions (not purely performative), but a growing portion of its activity is justificatory — defending the expansive interpretation against state non-cooperation, producing opinionated judgments that reinforce the mandate. The measurement series shows extractiveness and suppression-requirement rising sharply through the observed period (t=0 to t=12), then plateauing as the interpretation solidified institutionally (t=16 onward); theater ratio accelerates earlier and plateaus by t=12, consistent with the reading becoming entrenched in institutional practice rather than newly invented.
 *
 * PERSPECTIVAL GAP:
 *   The ICC institutional authority and universal-justice advocates perceive the constraint as legitimate coordination establishing universal accountability. Non-party states and their nationals perceive it as institutional overreach extracting sovereignty concessions without consent. The sovereigntist reading is held by powerful institutional actors (permanent UNSC members, major non-parties like the USA and China) whose exit options include refusing to recognize ICC authority; the universalist reading is held by the ICC itself, victim advocates (who are powerless but coordinated through NGOs), and party states that benefit from the moral authority the reading confers. The engine should compute dramatically different types for the agenda-setter (ICC) versus the payer (non-party state nationals): the ICC sits near the beneficiary end of directionality (d ~0.15-0.30), while non-party nationals sit near the target end (d ~0.75-0.85), despite inhabiting the same structural constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC institutional authority and beneficiary seats (victims, party states, universal-justice advocates) gain legitimacy, reach, and moral authority from the universalist reading. The payer seats (non-party nationals, non-party state sovereigns) bear the enforcement burden: they face prosecution risk, loss of veto, and sovereignty constraints without having consented to the treaty. Non-party nationals are trapped (nationality identity-locked; geography allows exit but not permanent escape from the constraint). Non-party state sovereigns are constrained (they can refuse cooperation, but isolation is costly; they can negotiate bilaterals, but the universalist reading's legitimacy makes these agreements fragile). The directionality asymmetry is high: beneficiaries have d near 0.2-0.3 (positive benefit without running the institution), while payers have d near 0.75-0.85 (bearing extraction without consent). No override is needed — the structural derivation (beneficiary status + victim status + exit options + power atoms) should produce this asymmetry naturally.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question here is whether the founding problem (impunity for core-crime perpetrators) remains live or has shifted. The universalist reading avoids mandatrophy if the problem of impunity persists despite the Statute's existence — i.e., if the reading is still solving the problem it was built for. However, sovereigntist and complementarity readings would argue that the real present problem is not impunity but institutional overreach: the ICC is prosecuting cases and asserting jurisdiction beyond what the Statute intended, driven by activist interpretation, not by unsolved impunity. This reading avoids mandatrophy by pointing to cases like the UNSC referrals (Sudan, Libya) where non-party situations were prosecuted and perpetrators were held accountable (or attempted to be). But in cases where the ICC asserts jurisdiction over non-party nationals in non-referred situations (using the territorial trigger), the mandatrophy risk rises: if the constraint is enforced primarily through legal interpretation rather than state cooperation, and if that interpretation is contested by powerful actors, then the constraint may be persisting theatrically rather than functionally. The theater-ratio trajectory supports this: rising theater from t=0 to t=12, then plateau, suggests the institution moved from defending the interpretation (high rhetoric) to accepting it as baseline and focusing on prosecutions (lower marginal theater). This is a sign of institutional normalization, not mandatrophy, but only if the underlying extractiveness is justified by the solving of impunity. If the underlying extractiveness is instead the ICC asserting power for its own institutional sake, mandatrophy warning fires.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_universal_justice_axiom,
    'Does the Rome Statute''s invocation of ''universal'' principles and ''crimes threatening humanity'' signal a commitment to jurisdiction transcending state consent, or is ''universal'' a rhetorical frame around a fundamentally consent-based treaty?',
    'Comparison of treaty text (preamble, jurisdictional articles), negotiation records from the Rome Conference, and subsequent state practice (ratification speeches, Security Council referrals, state participation in Assembly of States Parties decisions on jurisdiction interpretation).',
    'If the text and historical record support universal-jurisdiction framing, the universalist reading is well-grounded and the constraint''s type (tangled_rope) holds. If the record shows states negotiated consent limits that the reading overrides, the constraint reclassifies toward snare and the extraction metric should rise above the authored 0.68.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_universal_justice_axiom, conceptual, 'Whether the Statute''s universalist language expresses foundational commitment or rhetorical aspiration.').

omega_variable(
    complementarity_as_substantive_gate,
    'Is complementarity (domestic courts have first dibs) a substantive limit on ICC jurisdiction, or a procedural filter on *when* the ICC acts given that it can claim jurisdiction?',
    'Analysis of ICC jurisprudence on complementarity: do rulings treat complementarity as foreclosing jurisdiction (substantive), or merely delaying ICC action while domestic courts have a chance (procedural)? Comparison with the sovereigntist and hybrid readings'' treatment of the same jurisprudence.',
    'If complementarity is substantive, the universalist reading''s extraction is overstated and the sovereigntist reading is better-grounded. If procedural, the universalist reading''s jurisdiction claim is accurate and extraction is correctly estimated. This omega affects the sibling readings'' structural positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(complementarity_as_substantive_gate, empirical, 'Whether complementarity is a gate on jurisdiction or a timing filter on enforcement.').

omega_variable(
    non_party_state_capacity_exit,
    'Can non-party states effectively escape the constraint''s enforcement reach by refusing cooperation, or does the universalist reading''s legitimacy make non-cooperation diplomatically and politically costly enough to constitute suppression?',
    'Case-by-case analysis of state compliance with ICC arrest warrants and cooperation requests. Compare compliance rates for party states, non-party states that recognize ICC authority, and non-party states that actively resist (e.g., bilateral immunity agreements). If non-parties have lower compliance but the constraint persists, suppression is primarily achieved through legitimacy, not formal enforcement.',
    'If non-parties can effectively refuse cooperation without severe cost, exit_options for non-party nationals should reclassify from ''identity_locked'' to ''constrained'' (geographic exit is viable; prosecution risk is lower). If refusal is diplomatically costly, the constraint is more suppressive than the authored 0.72 suggests. If suppression operates primarily through legitimacy (conviction that the constraint is right) rather than coercive capacity, the constraint risks mandatrophy if the legitimacy erodes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_party_state_capacity_exit, empirical, 'Whether non-cooperation with the universalist reading is feasible or suppressed by legitimacy.').

omega_variable(
    institutional_mandate_drift,
    'Has the ICC''s actual practice drifted from prosecuting the most severe core crimes (genocide, large-scale crimes against humanity) toward prosecuting a broader set of conduct under the universalist jurisdiction umbrella, shifting the constraint from solving foundational impunity to expanding institutional reach?',
    'Docket analysis comparing early (2002–2010) cases to recent (2015–2026) cases on geographic scope, crime severity, and relationship to UNSC referrals versus autonomous ICC triggers. If the distribution shifts toward lower-severity conduct or autonomous prosecution, mandate drift has occurred.',
    'If drift is substantial, the constraint risks reclassification as piton (atrophied function, persistent through institutional theater) rather than tangled_rope. The theater_ratio trajectory supports this hypothesis (theater plateau from t=12 onward). If drift is negligible, the constraint remains tangled_rope and the extractiveness is justified by the solving of real impunity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_mandate_drift, empirical, 'Whether the ICC''s practice has expanded beyond the foundational problem of core-crime impunity.').

omega_variable(
    reading_foreclosure_test,
    'Does the universalist reading''s core axiom (universal jurisdiction transcends state consent) logically foreclose the sovereigntist reading (strict consent requirement) in any single legal or institutional framework, or do the readings merely represent competing normative commitments held by different parties?',
    'Structural analysis of the axioms: if the universalist axiom is ''core crimes harm humanity itself, not merely territorial states'' and the sovereigntist axiom is ''state consent is the only legitimate basis for international obligation,'' these axioms directly contradict. No single institutional framework (no single court, legal system, or state) could endorse both. But if the universalist axiom is ''the Statute''s text authorizes universal jurisdiction'' and the sovereigntist axiom is ''the Statute''s text requires consent,'' the axioms are about textual interpretation, not foundational principle — and the same framework (an international court interpreting the Statute) could, in theory, find one side''s interpretation more persuasive without logically foreclosing the other.',
    'If axioms are foundational and contradictory, reading_relations should include ''forecloses''; if axioms are interpretive and competing, the relation is ''coexists_with''. This determines how the constraint family is structured and how version-comparison queries should treat the sibling readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_test, conceptual, 'Whether the universalist and sovereigntist readings logically foreclose or coexist.').

omega_variable(
    victims_access_vs_enforcement_asymmetry,
    'Does the constraint''s beneficiary status for victims (theoretical access to justice transcending state boundaries) translate into actual access, or is the theoretical benefit suppressed by enforcement asymmetry (the ICC cannot capture suspects without state cooperation)?',
    'Analysis of victim participation rates in ICC proceedings; comparison of victim access for cases from party states versus non-party-state-crime situations (where the universalist reading is most contested). If victim access is high in practice, the beneficiary benefit is real. If victim access is theoretically broad but practically restricted by enforcement gaps, the benefit is largely performative.',
    'If victim access is performative, the constraint is more snare-like (extraction from non-party states without commensurate benefit delivery) and less tangled_rope (which requires genuine coordination). If access is real, the constraint legitimately claims tangled_rope status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victims_access_vs_enforcement_asymmetry, empirical, 'Whether victim beneficiaries receive actual access to justice or only theoretical standing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_statute_universalist_tr_t0, rome_statute_jurisdiction__universalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(rome_statute_universalist_tr_t0, observed).
narrative_ontology:measurement(rome_statute_universalist_tr_t4, rome_statute_jurisdiction__universalist_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement_basis(rome_statute_universalist_tr_t4, observed).
narrative_ontology:measurement(rome_statute_universalist_tr_t8, rome_statute_jurisdiction__universalist_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement_basis(rome_statute_universalist_tr_t8, observed).
narrative_ontology:measurement(rome_statute_universalist_tr_t12, rome_statute_jurisdiction__universalist_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement_basis(rome_statute_universalist_tr_t12, observed).
narrative_ontology:measurement(rome_statute_universalist_tr_t16, rome_statute_jurisdiction__universalist_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(rome_statute_universalist_tr_t16, projected).
narrative_ontology:measurement(rome_statute_universalist_tr_t20, rome_statute_jurisdiction__universalist_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(rome_statute_universalist_tr_t20, projected).
narrative_ontology:measurement(rome_statute_universalist_tr_t24, rome_statute_jurisdiction__universalist_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(rome_statute_universalist_tr_t24, projected).
narrative_ontology:measurement(rome_statute_universalist_tr_t28, rome_statute_jurisdiction__universalist_reading, theater_ratio, 28, 0.41).
narrative_ontology:measurement_basis(rome_statute_universalist_tr_t28, projected).

% Extraction over time
narrative_ontology:measurement(rome_statute_universalist_be_t0, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(rome_statute_universalist_be_t0, observed).
narrative_ontology:measurement(rome_statute_universalist_be_t4, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 4, 0.58).
narrative_ontology:measurement_basis(rome_statute_universalist_be_t4, observed).
narrative_ontology:measurement(rome_statute_universalist_be_t8, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(rome_statute_universalist_be_t8, observed).
narrative_ontology:measurement(rome_statute_universalist_be_t12, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement_basis(rome_statute_universalist_be_t12, observed).
narrative_ontology:measurement(rome_statute_universalist_be_t16, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement_basis(rome_statute_universalist_be_t16, projected).
narrative_ontology:measurement(rome_statute_universalist_be_t20, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(rome_statute_universalist_be_t20, projected).
narrative_ontology:measurement(rome_statute_universalist_be_t24, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(rome_statute_universalist_be_t24, projected).
narrative_ontology:measurement(rome_statute_universalist_be_t28, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 28, 0.68).
narrative_ontology:measurement_basis(rome_statute_universalist_be_t28, projected).

% Suppression requirement over time
narrative_ontology:measurement(rome_statute_universalist_su_t0, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(rome_statute_universalist_su_t0, observed).
narrative_ontology:measurement(rome_statute_universalist_su_t4, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement_basis(rome_statute_universalist_su_t4, observed).
narrative_ontology:measurement(rome_statute_universalist_su_t8, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(rome_statute_universalist_su_t8, observed).
narrative_ontology:measurement(rome_statute_universalist_su_t12, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(rome_statute_universalist_su_t12, observed).
narrative_ontology:measurement(rome_statute_universalist_su_t16, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement_basis(rome_statute_universalist_su_t16, projected).
narrative_ontology:measurement(rome_statute_universalist_su_t20, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(rome_statute_universalist_su_t20, projected).
narrative_ontology:measurement(rome_statute_universalist_su_t24, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(rome_statute_universalist_su_t24, projected).
narrative_ontology:measurement(rome_statute_universalist_su_t28, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 28, 0.72).
narrative_ontology:measurement_basis(rome_statute_universalist_su_t28, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__universalist_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, icc_complementarity_domestic_primacy).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, international_criminal_accountability_norm).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the rome_statute_jurisdiction kernel. The sibling readings (sovereigntist_reading, hybrid_complementarity_reading) share the same jurisdictional kernel but instantiate different structural relationships between ICC authority, victim access, and state sovereignty. Each reading has its own ε, beneficiary/victim structure, and classified type. The network edge indicates that the universalist reading's assertion of universal jurisdiction creates downstream pressure on both sibling readings by establishing the institutional and legal precedent for universal-scope prosecution; changes in this reading's legitimacy or effectiveness propagate to the others. The readings coexist in the corpus as alternative structural framings of the same text, not as sequential evolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
