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
 *   human_readable: Rome Statute Universal Jurisdiction Mandate (Universalist Reading)
 *   domain: international_law/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute of the International Criminal Court is a contested
 *   kernel—a fixed text that different legal traditions read in fundamentally
 *   different ways. The universalist reading interprets Article 13(b) (UNSC
 *   referral) and the Statute's preamble language about universal justice to
 *   mean that the ICC has authority to prosecute core crimes regardless of
 *   whether the accused's state ratified the treaty. Non-party states
 *   (especially the USA, Russia, and China) reject this reading, arguing that
 *   unratified treaties cannot bind non-signers and that jurisdiction
 *   requires explicit state consent. A third reading (hybrid complementarity)
 *   claims the Statute balances universal aspiration with sovereign primacy
 *   through the complementarity mechanism. This constraint story instantiates
 *   ONLY the universalist reading—a clean, ε-invariant constraint story
 *   describing how the ICC institutional authority and its supporting legal
 *   constituency interpret and enforce the Rome Statute as establishing
 *   universal jurisdiction that transcends state consent.
 *
 * KEY AGENTS:
 *   - ICC institutional authority: sets the universal jurisdiction interpretation and enforces it through arrest warrants and prosecution; agenda-setter role
 *   - Non-party states (USA, Russia, China, India): face claimed jurisdiction without consent; powerful but constrained by the assertion of universal authority; payer role
 *   - States with competing jurisdiction: party states whose prosecutorial primacy is overridden by ICC claims; payer role
 *   - Defendants from powerful states: nationals of non-party states arrested under the universalist mandate; trapped exit; payer role
 *   - Victims of core crimes: powerless beneficiaries whose justice claims the universalist reading vindicates
 *   - International criminal justice constituency: organized beneficiary (human rights groups, transitional justice advocates)
 *   - UN Security Council: can trigger ICC jurisdiction via Article 13(b) referral, extending the universalist mandate to non-party situations
 *   - Sovereignty doctrine adherents: excluded seat; would argue consent-based jurisdiction is mandatory
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
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universal Jurisdiction Mandate (Universalist Reading)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, '59dd2a69-7b80-4330-a1ed-8d96cc7a08f0').
narrative_ontology:cs_kernel_codification('59dd2a69-7b80-4330-a1ed-8d96cc7a08f0', fixed_text).
narrative_ontology:cs_authority_grounding('59dd2a69-7b80-4330-a1ed-8d96cc7a08f0', extraction).
narrative_ontology:cs_interpretation_layer_present('59dd2a69-7b80-4330-a1ed-8d96cc7a08f0').
narrative_ontology:cs_reading_relation('59dd2a69-7b80-4330-a1ed-8d96cc7a08f0', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('59dd2a69-7b80-4330-a1ed-8d96cc7a08f0', rome_statute_jurisdiction__hybrid_complementarity_reading, influences).
narrative_ontology:cs_axiom('59dd2a69-7b80-4330-a1ed-8d96cc7a08f0', foundational, universal_jurisdiction_overrides_consent).
narrative_ontology:cs_axiom_status(universal_jurisdiction_overrides_consent, holdable).
narrative_ontology:cs_axiom_grounding('59dd2a69-7b80-4330-a1ed-8d96cc7a08f0', universal_jurisdiction_overrides_consent, deontological).
narrative_ontology:cs_axiom('59dd2a69-7b80-4330-a1ed-8d96cc7a08f0', foundational, victims_right_to_justice_transcends_state_boundaries).
narrative_ontology:cs_axiom_status(victims_right_to_justice_transcends_state_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('59dd2a69-7b80-4330-a1ed-8d96cc7a08f0', victims_right_to_justice_transcends_state_boundaries, deontological).
narrative_ontology:cs_reference_frame('59dd2a69-7b80-4330-a1ed-8d96cc7a08f0', universal_human_dignity_framework).
narrative_ontology:cs_drift_state('59dd2a69-7b80-4330-a1ed-8d96cc7a08f0', contemporary_unsc_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('59dd2a69-7b80-4330-a1ed-8d96cc7a08f0', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, international_criminal_justice_constituency).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, icc_institutional_authority).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, states_with_competing_jurisdiction).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, defendants_from_powerful_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, un_security_council).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the Rome Statute as establishing universal jurisdiction over core crimes (genocide, crimes against humanity, war crimes) regardless of whether the accused's state has ratified the treaty. Sets prosecutorial priorities, issues arrest warrants, and claims authority to act on behalf of the international community. Justifies universal mandate as necessary to prevent impunity for atrocity crimes and protect human dignity at the highest level.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_institutional_authority, agenda_setter,
    institutional, generational, constrained, universal).

% Face ICC jurisdiction and arrest warrants for their nationals without having consented to the Rome Statute (e.g., United States, Russia, China, India). Bear the cost of defending nationals in international proceedings, managing diplomatic friction with the ICC, and accepting limitations on their sovereignty that non-signers never agreed to. Their exit option is rhetorical rejection and non-cooperation, but they cannot legally escape the claimed jurisdiction.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_states, payer,
    powerful, generational, constrained, universal).

% Party states that claim their own judicial authority over crimes by their nationals or on their territory experience the ICC jurisdiction as overriding their primacy. Even where they have ratified the Statute, the universalist reading subordinates their prosecutorial discretion to ICC determinations of complementarity. They pay in eroded sovereignty and reduced control over their criminal justice system.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, states_with_competing_jurisdiction, payer,
    institutional, generational, constrained, national).

% Nationals of non-party powerful states (USA, Russia, China, India) or states with poor ICC cooperation face arrest warrants without any jurisdictional consent by their home state. Their home states will not extradite them to the ICC, but they become subject to arrest if they travel internationally. They bear the cost of restricted movement and the threat of prosecution under a framework they did not agree to.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, defendants_from_powerful_states, payer,
    powerful, biographical, trapped, universal).

% Survivors and family members of genocide, crimes against humanity, and war crimes gain a judicial forum independent of the perpetrator state's consent or capacity to prosecute. The universalist reading asserts their right to justice transcends state boundaries and state willingness. They benefit from the claim that accountability is owed regardless of whether the accused's state ratified the Statute.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes, beneficiary,
    powerless, biographical, mobile, universal).

% Human rights organizations, transitional justice advocates, and anti-impunity movements view the universalist mandate as establishing the principle that core crimes cannot escape accountability through state non-consent. They benefit from the institutional assertion that universal human dignity overrides the consent-based model of international law. They have the capacity to litigate, lobby, and shape ICC priorities.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, international_criminal_justice_constituency, beneficiary,
    organized, generational, mobile, universal).

% Can refer situations to the ICC (Article 13(b)) even when neither party ratified the Statute, effectively extending ICC jurisdiction and legitimating the universalist reading by invoking the Council's own universal authority. Shapes prosecutorial priorities through referral decisions and can shield allies through strategic non-referral or threat of veto.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, un_security_council, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, un_security_council, beneficiary).

% States and legal traditions that ground international law in sovereign consent (especially non-party states) reject the universalist reading as illegitimate. They would argue that unratified treaties cannot bind non-signers and that the Rome Statute's jurisdiction is conditional on state consent. They are excluded from the ICC's authority structure and cannot prevent its exercise.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, sovereignty_doctrine_adherents, excluded,
    institutional, generational, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__universalist_reading, icc_institutional_authority).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a permanent international forum for prosecuting and adjudicating core crimes that transcend state jurisdiction, coordinating accountability across borders and preventing perpetrators from escaping prosecution through territorial sanctuary or state non-cooperation.
% TRANSFER_FUNCTION: Transfers prosecutorial authority from national courts and ad-hoc tribunals to a permanent international institution; transfers sovereignty authority from individual states to a collective international mandate; transfers accountability claims from victims and states to the ICC as the authorized enforcer.
% ABSENT_VOICES: Non-party states (especially major powers like the United States, Russia, China) are structurally excluded from ratification but subject to the claimed universal jurisdiction. Their legal traditions and sovereignty models would argue that jurisdiction requires consent, not universal principle. Weaker states that cannot defend their nationals against ICC prosecution have limited voice in the institution's governance structure.
% DISAPPEARANCE_RATIONALE: If the universalist mandate and its enforcement vanished, accountability for core crimes would revert to national courts (often unavailable or compromised), ad-hoc tribunals (costly and temporary), or no prosecution at all. Perpetrators from non-cooperating states would escape accountability entirely. The international law framework would reorganize around strict consent-based jurisdiction and state sovereignty primacy.
% FOUNDING_PROBLEM: Post-Cold War atrocities (Rwanda, Yugoslavia, Cambodia) revealed that state-based justice systems are often unable or unwilling to prosecute their own leaders for genocide and crimes against humanity. The Rome Statute was designed to create a permanent institution authorized to prosecute core crimes regardless of state capacity, complicity, or sovereignty claims.
% FOUNDING_PROBLEM_CORROBORATION: The ICC itself and its supporting advocacy constituency attest the founding problem persists: the 2023 ICC referral of the Ukraine situation by the UNSC, ongoing Syrian atrocities prosecuted nowhere, and Bangladeshi genocide trials in absentia all demonstrate that state sovereignty still blocks accountability. Independent human rights groups and the International Criminal Court Assembly of States Parties corroborate that state-level accountability remains inadequate; non-party states dispute this framing and argue that the Statute's universal reach exceeds its lawful scope.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate-to-high (0.68 at interval end) because the constraint imposes costs on non-party states and competing jurisdictions without their consent. The universalist reading claims authority that no non-party state agreed to. Suppression is high (0.72) because enforcement depends on sustained assertion of universal jurisdiction against the competing sovereigntist interpretation and on practical cooperation (arrest warrants require state cooperation, which powerful non-party states withhold). Theater is moderate (0.41): the ICC's prosecutorial function is real, but part of its enforcement activity defends the claim to universal jurisdiction itself—rehearsing the legal argument that it has the authority to act at all. The measurement series tracks extraction rising slowly (0.51→0.68 over the interval) as ICC precedent accumulates and the institutional assertion of universal jurisdiction becomes more entrenched, while suppression rises in tandem (0.58→0.72) because non-party states and sovereigntist-tradition states must invest more in resistance and legal challenge. Theater ratio rises modestly (0.28→0.41) because the jurisdictional question itself becomes a proxy litigation front as much as the underlying crimes.
 *
 * PERSPECTIVAL GAP:
 *   The ICC institutional authority perceives the constraint as legitimate universal justice authority grounded in the Statute's text and the international community's need for accountability; non-party states perceive it as illegitimate extraterritorial overreach violating the consent-based model of international law. These are structural divergences, not disagreements about facts. The engine computes different type classifications for each seat: the agenda-setter (ICC) sees rope (genuine coordination, shared benefit); the constrained payers (non-party states) see snare (extraction under duress, no exit). These divergences flow from the structural data (power, exit, beneficiary/victim position) and are intentionally authored independently of the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-party powerful states benefit from the sovereigntist interpretation (they retain sovereignty and exit from ICC jurisdiction) and bear extraction costs under the universalist interpretation (they cannot escape claimed jurisdiction). Their resistance (0.73) is high because they have the capacity to refuse cooperation and the legitimacy claim to challenge the ICC's authority. Victims and human rights constituencies benefit from the universalist interpretation (accountability without state consent) and bear costs under sovereigntist interpretation (victims in non-party states go unaccounted for). The asymmetry is structural: the universalist mandate transfers authority from national sovereigns to international institutions and from states to victims, extracting from the former and benefiting the latter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (impunity for atrocity crimes) is live according to human rights constituencies and the ICC (ongoing Syrian atrocities, Myanmar persecution, Uganda rebel prosecutions remain unprosecuted or prosecuted only by the ICC); non-party states contest whether the ICC is the legitimate solution. The disappearance verdict is world_rearranges: if the universalist mandate vanished, accountability would fragment back to national courts and ad-hoc mechanisms. The constraint is not mandatrophic—it serves an active function (prosecuting core crimes) that was not fully served before. However, the constraint's FUNCTION differs between readings: universalists read it as establishing permanent international justice; sovereigntists read it as a treaty that binds only signers; hybrids read it as balancing both. A mandatrophy diagnosis would require one of these readings to prevail and then the constraint to become obsolete, which has not occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_universal_authority_contradiction,
    'Is universal jurisdiction over non-consenting states a legitimate exercise of international law authority, or does it violate the foundational principle that unratified treaties cannot bind non-signers?',
    'The contest between the universalist reading (authority overrides consent) and the sovereigntist reading (consent is mandatory) is a foundational dispute about what international law IS. Resolution would require the international legal community to formally adjudicate which treaty interpretation model is binding—a question internal to the kernel itself, not resolvable by external facts.',
    'If the sovereigntist reading prevails (consent-based jurisdiction is mandatory), the universalist mandate collapses and the Rome Statute reverts to a purely contractual framework. If universalism prevails, non-party states must accept binding ICC jurisdiction. The reading itself IS the resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_vs_universal_authority_contradiction, conceptual, 'Whether universal justice authority can override the consent-based model of international treaty law.').

omega_variable(
    complementarity_doctrine_erosion,
    'Does the universalist reading''s emphasis on universal mandate necessarily erode the complementarity principle (primacy of national courts when willing and able), or can both coexist?',
    'Examine ICC prosecution priorities and state communication over 5–10 year intervals: if the ICC increasingly prosecutes cases where national courts are nominally available but the ICC deems them insufficient, complementarity is being subordinated to universalism in practice.',
    'If complementarity erodes, state sovereignty over domestic justice shrinks further and the constraint becomes more extractive (higher effective suppression of state discretion). If complementarity holds, the universalist mandate is modulated by respect for state capacity, reducing extraction relative to the pure universal claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_doctrine_erosion, empirical, 'Whether universalism and complementarity remain in equilibrium or whether universal mandate undermines state primacy in practice.').

omega_variable(
    unsc_referral_asymmetry,
    'Does the UN Security Council''s ability to refer situations (Article 13(b)) to the ICC without triggering the jurisdictional constraints that bind party states create a two-tier enforcement structure where powerful states control jurisdiction expansion?',
    'Analyze UNSC referral patterns: compare the geographic distribution, temporal clustering, and P5 veto usage across two equal time intervals to detect whether referrals concentrate on non-P5 states and avoid P5 allies.',
    'If UNSC referrals show systematic bias toward non-P5 targets, the universalist mandate is actually selective: universal in theory but enforced asymmetrically through P5 discretion. This would mean the constraint''s victims are primarily non-aligned states while powerful states'' nationals escape prosecution through Council protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unsc_referral_asymmetry, empirical, 'Whether the universalist mandate applies symmetrically across all states or whether UNSC gatekeeping creates selective enforcement.').

omega_variable(
    reading_identity_jurisdictional_scope,
    'This reading is one interpretation of the Rome Statute kernel. The universalist reading claims ICC jurisdiction reaches non-party nationals through territorial or UNSC triggers; the sovereigntist reading claims jurisdiction requires state consent; the hybrid reading claims the Statute balances both. Which reading is the ''true'' interpretation of what the Statute''s framers intended?',
    'This is a hermeneutical question, not an empirical one. Resolution would require the legal community to converge on an authoritative reading of the Statute''s text, travaux préparatoires, and institutional practice—a question the Rome Statute itself does not answer conclusively.',
    'The answer determines which constraint story is ''correct.'' If the sovereigntist reading is canonical, the universalist mandate is an institutional overreach and should be classified as snare or piton (maintenance through institutional expansion rather than legitimate authority). If the universalist reading is canonical, the Rome Statute does establish universal jurisdiction and this constraint story is properly classified. The reading IS the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_jurisdictional_scope, conceptual, 'Whether the universalist reading reflects the true institutional mandate of the Rome Statute or represents a unilateral reinterpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__universalist_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(rome_tr_t5, rome_statute_jurisdiction__universalist_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(rome_tr_t10, rome_statute_jurisdiction__universalist_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(rome_tr_t15, rome_statute_jurisdiction__universalist_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__universalist_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(rome_tr_t25, rome_statute_jurisdiction__universalist_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(rome_tr_t30, rome_statute_jurisdiction__universalist_reading, theater_ratio, 30, 0.41).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 0, 0.51).
narrative_ontology:measurement(rome_be_t5, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(rome_be_t10, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(rome_be_t15, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(rome_be_t25, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(rome_be_t30, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t0, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(rome_su_t5, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(rome_su_t10, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(rome_su_t15, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(rome_su_t20, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(rome_su_t25, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(rome_su_t30, rome_statute_jurisdiction__universalist_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__universalist_reading, 0.18).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, unsc_referral_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, icc_complementarity_doctrine).

% DUAL FORMULATION NOTE:
% The Rome Statute jurisdiction is a kernel (stabilized text: the Statute itself) interpreted through three structurally distinct readings. This constraint story is the universalist reading: ICC jurisdiction extends to non-party nationals; victims include all persons regardless of state consent; authority claims override national sovereignty. The sovereigntist reading (separate constraint file) interprets jurisdiction as conditional on state consent and reads the complementarity mechanism as subordinate to consent. The hybrid reading (separate constraint file) claims the Statute balances both principles. The three readings have different ε values, different beneficiary/victim structures, and different types. They are linked via network.affects_constraints because they share a referent (the Rome Statute) and each reading's institutional authority and advocates seek to establish their interpretation as the canonical reading. Sibling readings compete for legitimacy through the same institutional channels (ICC Assembly of States Parties, UN organs, national courts deciding extradition and ICC cooperation). The universalist reading's enforcement depends on defeating the sovereigntist interpretation; the sovereigntist reading's enforcement depends on constraining ICC claims; the hybrid reading seeks to mediate both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
