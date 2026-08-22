% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__r2p_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__r2p_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_2_7_chapter_vii_tension__r2p_reading
 *   human_readable: Responsibility to Protect (R2P) Intervention Authorization
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   The Responsibility to Protect (R2P) reading of the Article 2(7)/Chapter
 *   VII tension holds that sovereignty is not a shield for atrocity but a
 *   conditional responsibility. Adopted at the 2005 World Summit, R2P
 *   reinterprets the UN Charter to permit — indeed require — international
 *   intervention when a state manifestly fails to protect its population from
 *   four specified crimes. This reading extracts authority from the
 *   sovereignty norm (victim) to legitimize intervention (beneficiary:
 *   at-risk populations). The constraint is a tangled rope: it coordinates
 *   genuine collective action on atrocity prevention (pillars one and two)
 *   while extracting sovereignty prerogatives from targeted states and the
 *   norm itself through pillar three's coercive enforcement. The Libya 2011
 *   intervention (Resolution 1973) and its regime-change outcome marked a
 *   turning point where extraction became visibly asymmetric, raising theater
 *   ratio and resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, 0.68).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__r2p_reading, 0.45).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__r2p_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__r2p_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__r2p_reading, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__r2p_reading, "Responsibility to Protect (R2P) Intervention Authorization").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__r2p_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__r2p_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__r2p_reading, 'b51733d9-a1f3-497c-a0bb-2f73549256f6').
narrative_ontology:cs_kernel_codification('b51733d9-a1f3-497c-a0bb-2f73549256f6', formalized).
narrative_ontology:cs_authority_grounding('b51733d9-a1f3-497c-a0bb-2f73549256f6', lineage).
narrative_ontology:cs_interpretation_layer_present('b51733d9-a1f3-497c-a0bb-2f73549256f6').
narrative_ontology:cs_reading_relation('b51733d9-a1f3-497c-a0bb-2f73549256f6', article_2_7_chapter_vii_tension__sovereignty_first_reading, coexists_with).
narrative_ontology:cs_axiom('b51733d9-a1f3-497c-a0bb-2f73549256f6', foundational, sovereignty_conditional_on_protection).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_protection, holdable).
narrative_ontology:cs_axiom_grounding('b51733d9-a1f3-497c-a0bb-2f73549256f6', sovereignty_conditional_on_protection, conventional).
narrative_ontology:cs_axiom('b51733d9-a1f3-497c-a0bb-2f73549256f6', foundational, international_responsibility_to_intervene).
narrative_ontology:cs_axiom_status(international_responsibility_to_intervene, holdable).
narrative_ontology:cs_axiom_grounding('b51733d9-a1f3-497c-a0bb-2f73549256f6', international_responsibility_to_intervene, conventional).
narrative_ontology:cs_reference_frame('b51733d9-a1f3-497c-a0bb-2f73549256f6', post_westphalian_protection_framework).
narrative_ontology:cs_drift_state('b51733d9-a1f3-497c-a0bb-2f73549256f6', post_libya_2011, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b51733d9-a1f3-497c-a0bb-2f73549256f6', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__r2p_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, at_risk_civilians).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, human_rights_ngos).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, targeted_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_norm).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__r2p_reading, non_aligned_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__r2p_reading, regional_organizations).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_as_responsibility_doctrine).
narrative_ontology:constraint_vindicates(article_2_7_chapter_vii_tension__r2p_reading, human_security_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populations facing systematic atrocity crimes (genocide, war crimes, ethnic cleansing, crimes against humanity). They have no exit from the violence except through external intervention. The constraint authorizes intervention that could end their persecution, making them the primary intended beneficiaries. Their survival depends on the constraint's operationalization.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, persecuted_populations, beneficiary,
    powerless, immediate, trapped, local).

% Civilians in conflict zones not yet facing full atrocity crimes but at imminent risk. They benefit from the preventive dimension of R2P (pillar two: international assistance and capacity-building). Their situation is structurally similar to persecuted populations but earlier in the escalation trajectory.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, at_risk_civilians, beneficiary,
    powerless, immediate, trapped, local).

% International human rights organizations and advocacy networks that championed R2P's adoption. They benefit from the norm's existence as a tool for advocacy, fundraising, and political leverage. They help set the agenda by documenting atrocities and pressuring states to act. Their exit options are high — they can shift focus to other campaigns.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, human_rights_ngos, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, human_rights_ngos, agenda_setter).

% States accused of perpetrating or failing to prevent atrocity crimes within their borders. They bear the costs of intervention: loss of sovereignty prerogatives, potential regime change, military defeat, international isolation, and prosecution of leaders. Their exit is constrained — they cannot easily escape the international system's reach, but they can resist through diplomatic, legal, and military means.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, targeted_states, payer,
    institutional, biographical, constrained, national).

% The Westphalian principle of non-intervention in domestic affairs (UN Charter Article 2(7)). This abstract norm bears the cost of R2P's erosion of the absolute sovereignty barrier. It is not an actor but a structural proposition that loses coherence and authority each time R2P is invoked. Its 'exit' is conceptual — it cannot leave the international legal order but its content is reshaped by practice.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_norm, payer,
    institutional, generational, constrained, universal).
narrative_ontology:stakeholder_non_agent(article_2_7_chapter_vii_tension__r2p_reading, sovereignty_norm).

% States (particularly in the Global South) that are not current perpetrators but fear R2P as a tool of great-power interventionism. They bear the cost of a weakened sovereignty shield and the precedent of external military action justified by human protection claims. Their exit is constrained — they remain subject to Security Council authorization but can resist normatively and diplomatically.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, non_aligned_states, payer,
    moderate, generational, constrained, global).

% The five permanent members of the UN Security Council who hold veto power over Chapter VII authorization. They administer the constraint by deciding when and whether to authorize intervention. They benefit from controlling the gate but pay the cost of legitimacy erosion when they deadlock or act selectively. Their exit options are maximal — they can veto, abstain, or shape mandates to serve their interests.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, un_security_council_p5, agenda_setter,
    institutional, generational, arbitrage, global).

% Regional bodies (AU, EU, OAS, ECOWAS) that implement R2P's pillar two (capacity building) and sometimes pillar three (coercive action) under Chapter VIII. They gain institutional relevance and resources from the R2P framework but also bear operational burdens. They can choose engagement level — exit is mobile.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, regional_organizations, agenda_setter,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(article_2_7_chapter_vii_tension__r2p_reading, regional_organizations, beneficiary).

% Scholars of international law, political philosophy, and security studies who analyze R2P's doctrinal coherence, state practice, and normative trajectory. They neither collect nor pay directly but shape the interpretive environment. Their exit is analytical — they can change frameworks but remain in the epistemic community.
narrative_ontology:constraint_stakeholder(article_2_7_chapter_vii_tension__r2p_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective action problem of mobilizing international response to mass atrocity crimes when the territorial state fails or refuses to protect its population. Converts moral horror into a structured decision procedure (three pillars, Security Council authorization) that coordinates states, regional organizations, and civil society around a shared responsibility.
% TRANSFER_FUNCTION: Transfers the authority to use coercive force across borders from the exclusive domain of the territorial state (sovereignty) to the international community (Security Council acting under Chapter VII). Transfers the risk and cost of intervention from potential victims to intervening states and organizations. Transfers legitimacy from the sovereignty norm to the human protection norm.
% ABSENT_VOICES: Victims of past interventions justified by humanitarian claims (e.g., Iraq 2003, Libya 2011 aftermath) who would object to the pattern of selective application and regime change drift. Populations in states that avoid scrutiny due to great-power patronage (e.g., China's Xinjiang, Israel/Palestine) who are excluded from the norm's protection by geopolitical veto. Future generations who inherit a more intervention-prone international order with eroded sovereignty barriers.
% DISAPPEARANCE_RATIONALE: If R2P vanished overnight, the legal and normative basis for authorized humanitarian intervention would collapse to the pre-2001 status quo: intervention would be legally dubious except with host-state consent or clear inter-state aggression. The Security Council would lose its agreed framework for atrocity response. Atrocity crimes would continue but the international community would have no agreed 'responsibility' to act — only ad hoc coalitions of the willing. The norm's disappearance would rearrange the legitimation landscape for cross-border force.
% FOUNDING_PROBLEM: The international community's repeated failure to prevent or halt genocide and mass atrocities in the 1990s (Rwanda, Srebrenica, Kosovo) despite the legal obligation under the Genocide Convention and the moral imperative of 'never again.' The tension between UN Charter Article 2(7) (non-intervention) and the moral/legal duty to protect populations from their own governments created a paralysis that R2P was designed to resolve.
% FOUNDING_PROBLEM_CORROBORATION: The ICISS report (2001), UN World Summit Outcome Document (2005), and Secretary-General reports (2009, 2012, 2017) attest the founding problem as live — atrocity crimes persist and the international response remains inadequate. The Non-Aligned Movement and several Global South states attest the problem is substantially solved in principle but the arrangement persists as a Western intervention tool; their statements at UNGA debates and NAM summits corroborate the shifted-function reading. Independent scholarship (Bellamy, Welsh, Thakur, Hehir) documents both the persistence of the atrocity problem and the selective application of the response.
narrative_ontology:disappearance_verdict(article_2_7_chapter_vii_tension__r2p_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_2_7_chapter_vii_tension__r2p_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_2_7_chapter_vii_tension__r2p_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(article_2_7_chapter_vii_tension__r2p_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_2_7_chapter_vii_tension__r2p_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__r2p_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_2_7_chapter_vii_tension__r2p_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the substantial transfer of sovereign authority to an international decision procedure controlled by P5 states, with selective application that benefits great-power interests. Suppression (0.45) is moderate — the constraint operates through Security Council authorization, not unilateral force, but the veto power suppresses alternatives (non-authorized intervention remains illegal). Theater ratio (0.38) captures the gap between R2P's preventive pillars (underfunded, rhetorical) and its coercive pillar (operationalized selectively). Accessibility collapse (0.42) is partial — alternatives (consent-based peacekeeping, diplomatic pressure, ICC referral) persist but are weakened by R2P's framing as the *exclusive* framework. Resistance (0.58) is high from non-aligned states and targeted regimes who contest the norm's legitimacy and application.
 *
 * PERSPECTIVAL GAP:
 *   From the persecuted_populations seat (powerless, trapped), the constraint appears as a lifeline — a rare coordination mechanism that might trigger their rescue. From the targeted_states seat (institutional, constrained exit), it appears as an existential threat — a legal weapon that legitimizes regime change. From the P5 seat (institutional, arbitrage exit), it appears as a discretionary tool — authorization granted or withheld based on strategic interest. The engine computes these divergent classifications from the structural data; the author does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Persecuted populations and at-risk civilians are structural beneficiaries (d ≈ 0.1–0.2) — the constraint exists to subsidize their protection. Human rights NGOs are partial beneficiaries (d ≈ 0.3) — they gain advocacy leverage but don't control outcomes. Targeted states are full targets (d ≈ 0.9) — they bear the coercive transfer. The sovereignty_norm is a structural victim (d ≈ 0.85) — its coherence is extracted to fund the intervention legitimacy. Non-aligned states are partial targets (d ≈ 0.6) — they lose normative cover without gaining protection. P5 are near-symmetric (d ≈ 0.45) — they control the gate but pay legitimacy costs. Regional organizations are slight beneficiaries (d ≈ 0.35) — they gain role relevance.
 *
 * MANDATROPHY ANALYSIS:
 *   R2P prevents mislabeling by maintaining the coordination function (pillars one and two: prevention, capacity building, diplomatic response) alongside the extraction function (pillar three: coercive intervention). The founding problem (atrocity prevention) remains live and contested — the arrangement has not fully atrophied into pure extraction. However, the Libya precedent and subsequent P5 deadlocks (Syria, Myanmar) have shifted the balance toward extraction. The mandate has not been resolved — the atrocity problem persists — but the constraint's operation increasingly serves the interests of the gatekeepers (P5) rather than the intended beneficiaries. This is a tangled rope trending toward snare if pillar three continues to dominate practice while pillars one and two wither.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    r2p_reading_of_kernel,
    'How does this reading''s structural classification change if the sibling sovereignty_first_reading is the operative framework instead?',
    'Comparative classification: generate the sovereignty_first_reading constraint story and compare its computed type, beneficiary/victim structure, and extraction profile to this reading.',
    'If the sibling reading computes as mountain or rope while this reading computes as tangled_rope/snare, the kernel itself is a false summit — the ''tension'' is not a natural legal structure but a contested political settlement. The reading with lower extraction would be the genuine coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(r2p_reading_of_kernel, conceptual, 'Commitment of this reading to the article_2_7_chapter_vii_tension kernel; structural delta from sovereignty_first_reading').

omega_variable(
    selective_application_mechanism,
    'Is the observed selectivity in R2P authorization (Libya yes, Syria no; Kosovo yes, Gaza no) a feature of the constraint''s design (P5 veto) or a bug in its implementation?',
    'Counterfactual analysis: would a modified constraint without P5 veto but with supermajority voting produce less selective outcomes? Compare regional organization interventions (ECOWAS, AU) where veto does not apply.',
    'If selectivity is a design feature, the constraint is structurally a snare (coordination cover for great-power extraction). If a bug, reform of the authorization mechanism could restore coordination purity — the constraint would be a salvageable tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selective_application_mechanism, conceptual, 'Whether extraction asymmetry derives from institutional design or contingent practice').

omega_variable(
    regime_change_drift,
    'Does the Libya 2011 precedent (Resolution 1973 authorized civilian protection; NATO executed regime change) represent a structural drift in the constraint''s transfer function?',
    'Doctrinal analysis of subsequent mandates (Mali, CAR, DRC) and state practice: does the ''protection'' mandate now implicitly authorize regime change? Track Security Council language evolution and General Assembly debates.',
    'If regime change has become the implicit transfer function, the constraint''s extraction is qualitatively higher — it transfers not just sovereign authority but political order itself. This would increase base_extractiveness and shift classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_change_drift, empirical, 'Whether pillar three''s operational mandate has drifted from protection to regime change').

omega_variable(
    prevention_pillar_hollow,
    'Are pillars one and two (state responsibility, international assistance) genuinely operational or have they become performative cover for pillar three''s selective enforcement?',
    'Resource flow analysis: compare funding for atrocity prevention/capacity building vs. peacekeeping/enforcement operations. Track early-warning activation rates and preventive diplomacy outcomes.',
    'If prevention pillars are hollow, theater ratio understates the performative share — the constraint''s coordination function is a facade. This would support reclassification toward snare and increase the mandate''s unresolved status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prevention_pillar_hollow, empirical, 'Whether the coordination function (prevention) is substantively resourced or rhetorically maintained').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__r2p_reading, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_tr_t2001, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_tr_t2005, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_tr_t2011, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2011, 0.35).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_tr_t2013, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2013, 0.38).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_tr_t2017, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2017, 0.37).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_tr_t2024, article_2_7_chapter_vii_tension__r2p_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_be_t2001, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2001, 0.15).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_be_t2005, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2005, 0.25).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_be_t2011, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2011, 0.55).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_be_t2013, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2013, 0.62).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_be_t2017, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2017, 0.65).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_be_t2024, article_2_7_chapter_vii_tension__r2p_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_su_t2001, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2001, 0.2).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_su_t2005, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2005, 0.3).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_su_t2011, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2011, 0.55).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_su_t2013, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2013, 0.48).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_su_t2017, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2017, 0.45).
narrative_ontology:measurement(article_2_7_chapter_vii_tension__r2p_reading_su_t2024, article_2_7_chapter_vii_tension__r2p_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__r2p_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_2_7_chapter_vii_tension__r2p_reading, 0.12).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, genocide_convention_obligation).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, un_charter_chapter_vii_authorization).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, icc_complementarity_principle).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__r2p_reading, regional_organization_intervention_mandates).

% DUAL FORMULATION NOTE:
% This constraint (r2p_reading) and its sibling (sovereignty_first_reading) form a constraint family decomposing the article_2_7_chapter_vii_tension kernel. The R2P reading has higher extractiveness (0.68 vs. ~0.15 for sovereignty_first) because it transfers sovereign authority to an international decision procedure. The sovereignty_first reading treats the kernel as a mountain (non-intervention as near-absolute); the R2P reading treats it as a tangled rope (conditional sovereignty with enforcement). They are linked because the R2P reading cites the sovereignty norm's erosion as its legitimating transfer, while the sovereignty_first reading cites R2P's selective application as evidence of the norm's corruption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__r2p_reading, institutional, 0.45).
constraint_indexing:directionality_override(article_2_7_chapter_vii_tension__r2p_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
