% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Rome Statute Hybrid Complementarity Jurisdiction
 *   domain: international_law/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute and International Criminal Court embody a tension
 *   between universal justice aspiration and sovereign state primacy. This
 *   reading (hybrid_complementarity_reading) instantiates the constraint as a
 *   compromise: the ICC claims universal jurisdiction grounded in natural law
 *   and crimes against humanity doctrine, but operationally defers to state
 *   prosecution via the complementarity principle. The constraint
 *   simultaneously vindicates both the international justice imperative AND
 *   the state sovereignty doctrine through institutional architecture — the
 *   ICC has formal authority that it rarely uses because enforcement depends
 *   on state cooperation. This reading sees the hybrid as structurally
 *   coherent, not as failure; the other readings (sovereigntist_reading and
 *   universalist_reading) argue either that sovereignty swallows the
 *   universal mandate or that complementarity is a betrayal of it.
 *
 * KEY AGENTS:
 *   - International Criminal Court — institutional agenda-setter; maintains universal jurisdiction claim but operationally constrained by complementarity deference.
 *   - Treaty signatory states — institutional beneficiaries; retain prosecutorial supremacy through complementarity while gaining legitimacy from the international framework.
 *   - Non-signatory states — excluded from formal framework; subject to Security Council referral gatekeeping.
 *   - Accused nationals of weak states — powerless targets; face ICC prosecution without state protection.
 *   - Accused nationals of powerful states — powerful beneficiaries; de facto immune through state non-cooperation.
 *   - Crime victims in underperforming jurisdictions — powerless victims; entitled to justice on paper but excluded in practice.
 *   - Permanent Security Council members — institutional gatekeepers; control the referral mechanism for non-signatories.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.62).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.48).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Hybrid Complementarity Jurisdiction").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, 'fd79e18e-661d-4f28-a725-14e7ea37ce94').
narrative_ontology:cs_kernel_codification('fd79e18e-661d-4f28-a725-14e7ea37ce94', formalized).
narrative_ontology:cs_authority_grounding('fd79e18e-661d-4f28-a725-14e7ea37ce94', lineage).
narrative_ontology:cs_interpretation_layer_present('fd79e18e-661d-4f28-a725-14e7ea37ce94').
narrative_ontology:cs_reading_relation('fd79e18e-661d-4f28-a725-14e7ea37ce94', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd79e18e-661d-4f28-a725-14e7ea37ce94', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('fd79e18e-661d-4f28-a725-14e7ea37ce94', foundational, complementarity_as_structural_principle).
narrative_ontology:cs_axiom_status(complementarity_as_structural_principle, holdable).
narrative_ontology:cs_axiom_grounding('fd79e18e-661d-4f28-a725-14e7ea37ce94', complementarity_as_structural_principle, conventional).
narrative_ontology:cs_axiom('fd79e18e-661d-4f28-a725-14e7ea37ce94', foundational, universal_justice_aspiration_and_state_primacy_coexist).
narrative_ontology:cs_axiom_status(universal_justice_aspiration_and_state_primacy_coexist, holdable).
narrative_ontology:cs_axiom_grounding('fd79e18e-661d-4f28-a725-14e7ea37ce94', universal_justice_aspiration_and_state_primacy_coexist, deontological).
narrative_ontology:cs_reference_frame('fd79e18e-661d-4f28-a725-14e7ea37ce94', rome_statute_balanced_framework).
narrative_ontology:cs_drift_state('fd79e18e-661d-4f28-a725-14e7ea37ce94', contemporary_prosecution_asymmetry_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fd79e18e-661d-4f28-a725-14e7ea37ce94', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, treaty_signatory_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, non_signatory_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_nationals_without_state_cooperation).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, crime_victims_in_underperforming_jurisdictions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_nationals_of_powerful_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_nationals_of_weak_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, non_aligned_movement_states).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_justice_imperative).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, complementarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates crimes against humanity, genocide, and war crimes under the Rome Statute. Claims universal jurisdiction grounded in natural law; operationally deferred by complementarity provisions requiring it to defer to domestic prosecution where the accused state is able and willing. Maintains investigative capacity and formal legal authority while enforcement depends entirely on state arrest warrants and cooperation. Justifies the compromise as respecting state sovereignty while advancing international justice.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court, agenda_setter,
    institutional, generational, analytical, universal).

% Benefit from a framework that appears to advance international criminal justice while preserving their de facto prosecutorial supremacy. They retain the right to prosecute or not prosecute their own nationals; the ICC cannot proceed without their cooperation. Many leverage the deferential posture for legitimacy while avoiding actual prosecution of powerful figures. Can withdraw from the treaty (exit option exists but carries political cost).
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, treaty_signatory_states, beneficiary,
    institutional, generational, mobile, global).

% Are structurally outside the treaty framework; nationals can only be prosecuted via ad hoc Security Council referral. Excluded from the constraint's formal rules but still subject to its effects when their nationals are referred to the ICC without their consent. Their exclusion is maintained by their non-membership and by the referral mechanism's political gatekeeping.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_signatory_states, excluded,
    organized, generational, constrained, global).

% Face ICC prosecution when their home state is unwilling or unable to prosecute and has either signed the treaty or been referred via the Security Council. They bear the cost of jurisdiction without the benefit of state protection or domestic legal recourse. Exit is not available; the framework legally binds them despite their powerlessness.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_nationals_of_weak_states, payer,
    powerless, biographical, trapped, universal).

% Benefit from de facto immunity through the complementarity deferral: if their state chooses not to prosecute, the ICC cannot move without state arrest cooperation, which powerful states consistently refuse. The constraint protects them through state sovereignty even where universal jurisdiction is formally claimed.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_nationals_of_powerful_states, beneficiary,
    powerful, biographical, arbitrage, global).

% Are formally entitled to international justice by the Rome Statute's universal aspiration but practically excluded where domestic states are unwilling to prosecute and the accused state does not cooperate. The constraint's deferential architecture leaves them uncompensated; justice exists on paper but not in practice. They cannot compel jurisdiction.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, crime_victims_in_underperforming_jurisdictions, payer,
    powerless, biographical, trapped, local).

% Control the referral mechanism: only the Security Council can refer non-signatory state nationals to the ICC. They exercise veto power over which conflicts enter the international justice system, selectively deploying the ICC against geopolitical rivals while protecting allies. Their nationals are de facto exempt from ICC prosecution absent a referral their own veto blocks.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, permanent_security_council_members, agenda_setter,
    institutional, generational, arbitrage, universal).

% Often resist the Rome Statute as neo-colonial institutional authority that in practice protects powerful signatories while targeting weaker nations without great-power patrons. Many have withdrawn or refused signature, claiming the framework is asymmetric. Those that signed are subject to the constraint; those that haven't are excluded unless referred by the Security Council, which is dominated by the same powers.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_aligned_movement_states, excluded,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, non_aligned_movement_states, payer).

% Document and contest the constraint's operation. Advocate for stronger universal jurisdiction and lower barriers to ICC prosecution. Monitor state compliance with arrest warrants and highlight cases where complementarity deferral leaves perpetrators unprosecuted. Their analysis surfaces the gap between the constraint's aspiration and its actual enforcement.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_justice_advocacy_community, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__hybrid_complementarity_reading, treaty_signatory_states).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__hybrid_complementarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal framework for prosecuting the gravest international crimes (genocide, crimes against humanity, war crimes) through a permanent institution, replacing ad hoc tribunals and jurisdictional fragmentation. Coordinates state parties around a shared definition of universal justice while preserving their domestic capacity to prosecute.
% TRANSFER_FUNCTION: Transfers prosecution authority and legitimacy to the ICC, but operationally returns prosecutorial discretion to signatory states via complementarity: the constraint moves the appearance of universal justice from states to the ICC, but moves actual enforcement power back to states through the deferential requirement that the ICC defer where the accused state is willing and able to prosecute.
% ABSENT_VOICES: Non-signatory states cannot formally object (they are structurally excluded); nations without permanent Security Council seats have no voice in the referral mechanism for non-signatories; populations in underperforming domestic jurisdictions are formally entitled to international justice but cannot enforce it and are not seated at decision points. Their absence is maintained by the treaty structure and Security Council gatekeeping. The crime victims in weak-state jurisdictions where the state will not prosecute and the accused is not referred are systematically unheard.
% DISAPPEARANCE_RATIONALE: If the Rome Statute and ICC disappeared, international criminal justice would revert to ad hoc tribunals, national prosecutions, and geopolitical power plays. The institutional coordination would dissolve; prosecution rates for perpetrators of mass atrocity would shift dramatically depending on whether their home states (or patrons) had domestic political incentive. The constraint's disappearance would not restore a prior equilibrium but would radically reshape which crimes are prosecuted and which go unpunished. The accountability infrastructure would fragment geographically and politically.
% FOUNDING_PROBLEM: Post-Cold War international community lacked a permanent mechanism for prosecuting systematic mass atrocities; ad hoc tribunals for Yugoslavia and Rwanda were slow, expensive, and incomplete. The founding problem was the gap between universal justice aspiration and fragmented national capacity.
% FOUNDING_PROBLEM_CORROBORATION: The ICC itself attests the founding problem is still live, citing ongoing mass atrocities and the absence of consistent domestic prosecution. International justice advocates attest the problem persists. However, non-aligned states and some international law scholars attest that the Rome Statute has NOT solved the founding problem because complementarity deferral systematically privileges powerful signatories and excludes non-signatories; they argue the Statute is now primarily a means to selectively prosecute weak-state actors while protecting the strong. Academic analysis and historical data from institutions outside the ICC's orbit (human rights NGOs, independent legal scholars, academic international relations) support both the gap-closure reading and the selective-prosecution critique. The Uganda case file (prosecuting Ugandan nationals while refraining from investigating US conduct) and the African Union's withdrawal threat are cited by the critical camp as evidence the founding problem remains unsolved.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the constraint creates asymmetric vulnerability: weak-state and non-signatory nationals face prosecution, while powerful-state nationals enjoy de facto immunity through complementarity deferral. The constraint's formal universality is instrumentally extracted by powerful signatories that use it against rivals while protecting themselves. Theater ratio is elevated (0.41) because complementarity performance masks selective enforcement: the ICC performs universal justice while state gatekeeping determines actual prosecution rates. Suppression is moderate (0.48) because the constraint's authority is grounded in treaty consent and natural law aspiration that most parties formally accept; resistance comes from non-aligned states and from the practical evidence that prosecution is selective, not from wholesale rejection of the framework's legitimacy. The measurement series shows extractiveness rising steeply (0.48 to 0.62) in the first 15 time units as the ICC's caseload concentrates on African and weak-state defendants, then plateaus as the pattern stabilizes — this tracks the corpus of historical indictments showing geographic and power-based asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   From the ICC's seat: the constraint is genuine coordination (unifying prosecution standards, ending impunity for gravest crimes, respecting state capacity). From the powerless-accused seat: the constraint is selective targeting (prosecution depends on whether your state cooperates, which is itself a function of geopolitical power). From the powerful-state seat: the constraint is protective (complementarity deferral means no prosecution unless we consent). From the crime-victim seat in an underperforming jurisdiction: the constraint is exclusionary (justice is promised but operationally inaccessible). The engine computes directionality per seat from power + exit + beneficiary/victim status; the authored structural data (beneficiaries list both ICC and signatory states; victims list non-signatory and weak-state targets, plus excluded victims) captures the asymmetry without adjudicating whose reading is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC derives directionality near 0.3 (moderate beneficiary) — it benefits from institutional authority and legitimacy but depends entirely on state cooperation for enforcement, so it is not a pure extractor. Treaty signatory states derive d near 0.2–0.3 (moderate beneficiary) — they maintain domestic prosecutorial supremacy while gaining international legitimacy without cost. Non-signatory states derive d near 0.7 (moderate target) — they are subject to the constraint through Security Council referral without having consented to its terms. Weak-state accused derive d near 0.95 (near-total target) — they have trapped exit, no domestic protection, and face prosecution without being able to opt out. Powerful-state accused derive d near 0.05 (near-total beneficiary) — they enjoy de facto immunity through state non-cooperation and suffer no cost. The asymmetry is structural, not a difference of opinion.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids the mandatrophy trap by explicitly naming what the constraint coordinates (unified prosecution standards, end to jurisdictional fragmentation) AND what it extracts (selective prosecution against weak states, de facto immunity for the powerful). The founding problem (post-Cold War justice gap) is real and addresses a genuine coordination failure; the constraint does solve it partially. However, the solution preserves state sovereignty at the cost of systematic selectivity — the mandate to advance universal justice is attenuated through complementarity, not because complementarity is a technical failure but because it operationally prioritizes state primacy. Mandatrophy would charge that the constraint has become primarily a tool for selective prosecution dressed in universal language; this reading accepts that trade-off as the price of state consent while flagging the asymmetry via the elevated theater ratio and the measurement of rising selectivity over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_as_core_vs_deferral,
    'Is complementarity a core principle of the Rome Statute''s authority structure, or is it a temporary deferential accommodation pending state capacity development?',
    'Textual analysis of the Statute''s preamble and provisions against the travaux préparatoires (negotiation records); statements from founding state delegations; ICC jurisprudence on whether complementarity is being interpreted as a structural brake or as a transient capacity backstop.',
    'If complementarity is core: the Statute is a sovereigntist framework with limited universal scope (sovereigntist_reading prevails). If deferential: the Statute is a universalist framework with practical constraints (universalist_reading prevails). The hybrid reading holds both; the resolution would establish whether that hybridity is stable or unstable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(complementarity_as_core_vs_deferral, conceptual, 'Whether complementarity is the Statute''s foundational principle or a temporary accommodation.').

omega_variable(
    selectivity_inherent_vs_contingent,
    'Is the observed geographic and power-based asymmetry in ICC prosecution an inevitable consequence of the complementarity mechanism, or a contingent artifact of selective state referral and non-cooperation?',
    'Comparative analysis: design a counterfactual where the Security Council is reformed to remove veto power, or where non-signatory states voluntarily join, and assess whether prosecution patterns would symmetrize. Qualitative analysis of why states cooperate or refuse arrest warrants.',
    'If inherent: the constraint''s extraction is structural and permanent; the hybrid reading describes a stable compromise with built-in asymmetry. If contingent: the extraction could be reduced through institutional reform; the hybrid reading describes a transitional state awaiting structural adjustment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_inherent_vs_contingent, empirical, 'Whether selective prosecution stems from the mechanism itself or from contingent state behavior.').

omega_variable(
    universal_jurisdiction_vs_treaty_consent,
    'Can universal jurisdiction grounded in natural law coexist with the Rome Statute''s foundation in treaty consent, or does one axiom ultimately override the other?',
    'Jurisdictional test: can the ICC legitimately claim authority over non-signatory nationals absent Security Council referral? If yes, natural law universal jurisdiction is real and treaty consent is secondary. If no, treaty consent is the foundational ground and universal jurisdiction is aspirational language only.',
    'If universal jurisdiction is real: the Statute has latent universalist authority that could expand as enforcement grows stronger (universalist_reading is foreclosed by the constraint''s formal authority). If treaty consent is foundational: the Statute''s universalism is rhetorical, and complementarity + sovereignty are the true constraints (sovereigntist_reading prevails). The hybrid reading requires both axioms to coexist; a resolution would establish whether that is stable or contradictory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_jurisdiction_vs_treaty_consent, conceptual, 'Whether universal jurisdiction and treaty consent are compatible axioms or ultimately contradictory.').

omega_variable(
    state_capacity_development,
    'As non-signatory states and weaker signatories develop domestic criminal justice capacity, will complementarity deferral mechanism reduce or will it become more selective?',
    'Longitudinal analysis: track whether increasing domestic prosecution capacity in previously weak jurisdictions correlates with less ICC prosecution or with ICC choosing to prosecute more aggressively in non-cooperating states.',
    'If capacity development reduces ICC prosecution: complementarity is working as intended and the constraint is moving toward state-led justice (sovereigntist_reading prevails). If ICC prosecutes more where states don''t cooperate: the ICC''s universal authority is expanding as enforcement capacity grows (universalist_reading gains ground). The hybrid reading predicts stable selectivity regardless of capacity development.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_capacity_development, empirical, 'Trajectory of complementarity mechanism as global justice capacity evolves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(rome_tr_t0, observed).
narrative_ontology:measurement(rome_tr_t5, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(rome_tr_t5, observed).
narrative_ontology:measurement(rome_tr_t10, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(rome_tr_t10, observed).
narrative_ontology:measurement(rome_tr_t15, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement_basis(rome_tr_t15, observed).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(rome_tr_t20, observed).
narrative_ontology:measurement(rome_tr_t25, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(rome_tr_t25, observed).
narrative_ontology:measurement(rome_tr_t30, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(rome_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(rome_be_t0, observed).
narrative_ontology:measurement(rome_be_t5, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(rome_be_t5, observed).
narrative_ontology:measurement(rome_be_t10, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(rome_be_t10, observed).
narrative_ontology:measurement(rome_be_t15, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(rome_be_t15, observed).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(rome_be_t20, observed).
narrative_ontology:measurement(rome_be_t25, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(rome_be_t25, observed).
narrative_ontology:measurement(rome_be_t30, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(rome_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t0, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(rome_su_t0, observed).
narrative_ontology:measurement(rome_su_t5, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(rome_su_t5, observed).
narrative_ontology:measurement(rome_su_t10, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 10, 0.41).
narrative_ontology:measurement_basis(rome_su_t10, observed).
narrative_ontology:measurement(rome_su_t15, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement_basis(rome_su_t15, observed).
narrative_ontology:measurement(rome_su_t20, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement_basis(rome_su_t20, observed).
narrative_ontology:measurement(rome_su_t25, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement_basis(rome_su_t25, observed).
narrative_ontology:measurement(rome_su_t30, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(rome_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.14).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, security_council_referral_gatekeeping).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, state_arrest_warrant_cooperation).

% DUAL FORMULATION NOTE:
% The rome_statute_jurisdiction kernel decomposes into three readings with structurally distinct ε values. The hybrid_complementarity_reading (this story) treats complementarity as a stable principle that operationally constrains but does not negate the ICC's universal authority — ε=0.62 reflects the asymmetric extraction that results. The sovereigntist_reading instantiates the Statute as fundamentally consent-based with complementarity as core — lower ε reflecting lower extraction if sovereignty is genuinely operative. The universalist_reading instantiates the Statute as establishing universal mandate — higher ε reflecting the gap between aspiration and complementarity-constrained enforcement. These are not different observations of one constraint; they are different constraints grounded in different interpretive commitments to the same text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
