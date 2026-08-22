% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy Reading of Constitutional Interpretive Authority
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This story instantiates the judicial supremacy reading of the
 *   constitutional text kernel: courts hold final, conclusive interpretive
 *   authority, and judicial invalidation of legislation cannot be
 *   legislatively overridden. This is a distinct constraint from the
 *   legislative_sovereignty_reading (parliament retains final say via
 *   override/notwithstanding mechanisms) and the popular_sovereignty_reading
 *   (the demos retains ultimate authority via amendment or constituent power)
 *   — those are separate constraint files with their own epsilon and
 *   structural data, linked via network.affects_constraints. Under judicial
 *   supremacy, courts function as gatekeepers on legislation, producing high
 *   interpretive rigidity: once a court fixes a reading, only a subsequent
 *   court decision or a supermajority constitutional amendment can revise it.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.42).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.55).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy Reading of Constitutional Interpretive Authority").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, '8858a6ab-4862-4c96-ab74-1f86fbc598bc').
narrative_ontology:cs_kernel_codification('8858a6ab-4862-4c96-ab74-1f86fbc598bc', fixed_text).
narrative_ontology:cs_authority_grounding('8858a6ab-4862-4c96-ab74-1f86fbc598bc', lineage).
narrative_ontology:cs_interpretation_layer_present('8858a6ab-4862-4c96-ab74-1f86fbc598bc').
narrative_ontology:cs_reading_relation('8858a6ab-4862-4c96-ab74-1f86fbc598bc', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('8858a6ab-4862-4c96-ab74-1f86fbc598bc', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('8858a6ab-4862-4c96-ab74-1f86fbc598bc', foundational, judicial_finality_over_constitutional_meaning).
narrative_ontology:cs_axiom_status(judicial_finality_over_constitutional_meaning, holdable).
narrative_ontology:cs_axiom_grounding('8858a6ab-4862-4c96-ab74-1f86fbc598bc', judicial_finality_over_constitutional_meaning, conventional).
narrative_ontology:cs_axiom('8858a6ab-4862-4c96-ab74-1f86fbc598bc', secondary, counter_majoritarian_check_justifies_unreviewable_veto).
narrative_ontology:cs_axiom_status(counter_majoritarian_check_justifies_unreviewable_veto, holdable).
narrative_ontology:cs_axiom_grounding('8858a6ab-4862-4c96-ab74-1f86fbc598bc', counter_majoritarian_check_justifies_unreviewable_veto, instrumental).
narrative_ontology:cs_reference_frame('8858a6ab-4862-4c96-ab74-1f86fbc598bc', marbury_style_judicial_finality).
narrative_ontology:cs_drift_state('8858a6ab-4862-4c96-ab74-1f86fbc598bc', contemporary_countermajoritarian_debate, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8858a6ab-4862-4c96-ab74-1f86fbc598bc', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimant_minorities).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, constitutional_court_judiciary).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, legislative_majorities).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, electorate_seeking_policy_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds final word on whether legislation conforms to constitutional text. Invalidates statutes it deems unconstitutional, with no legislative override available. Its interpretive rulings become the operative constitutional meaning until it revisits them itself; it both administers and benefits from the arrangement in institutional authority and stature.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_court_judiciary, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__judicial_supremacy_reading, constitutional_court_judiciary, beneficiary).

% Groups lacking legislative majority support rely on courts to strike down majoritarian laws that burden them. Their protection depends entirely on this reading holding — without judicial supremacy, hostile majorities could legislate around them with no independent check.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimant_minorities, beneficiary,
    powerless, biographical, trapped, national).

% Enact statutes reflecting electoral mandates, which courts may nullify with no mechanism to override or revisit the ruling through ordinary legislative process. Their remedy is limited to constitutional amendment, a far higher bar than the majority that elected them could otherwise clear.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislative_majorities, payer,
    powerful, biographical, constrained, national).

% Votes for representatives promising policy change, only to see enacted programs invalidated by unelected judges applying contestable interpretations of open-textured constitutional language. Their democratic input is filtered through a body they did not select and cannot recall.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, electorate_seeking_policy_change, payer,
    organized, generational, constrained, national).

% Argue the legislature, as the elected branch, should retain final say on constitutional meaning through override mechanisms. Under this reading their position is structurally foreclosed — there is no notwithstanding clause or override vote available to act on their view.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legislative_sovereignty_advocates, excluded,
    organized, generational, trapped, national).

% Study and debate whether judicial supremacy is textually compelled or a doctrinal accretion (e.g., Marbury-style self-assertion). Their scholarship shapes legitimacy discourse without directly altering the arrangement's operation.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, depoliticized final arbiter of constitutional meaning so that fundamental rights and structural limits are not subject to renegotiation by whatever coalition currently holds a legislative majority.
% TRANSFER_FUNCTION: Moves ultimate interpretive and vetoing authority over legislation from elected legislatures to appointed or life-tenured judges; moves protective certainty toward rights-claimants and away from majoritarian policy flexibility.
% ABSENT_VOICES: Legislative sovereignty advocates and popular-constitutionalism proponents would object that unelected judges should not have unreviewable final say, but under this reading's own terms there is no institutional channel for their objection to become binding.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished overnight, legislatures could enact and re-enact laws courts previously struck down, rights protections currently insulated from majoritarian revision would become contestable through ordinary politics, and the entire architecture of constitutional litigation as a check on legislation would need to be replaced by political or popular mechanisms.
% FOUNDING_PROBLEM: Written constitutions needed an authoritative, non-political mechanism to prevent transient legislative majorities from eroding entrenched rights and structural limits, and to resolve genuine interpretive disputes about ambiguous constitutional text.
% FOUNDING_PROBLEM_CORROBORATION: Judges and rights-advocacy organizations attest the problem remains live, citing ongoing legislative attempts to erode minority protections. Legislative sovereignty scholars and popular-constitutionalism theorists, writing from outside the judiciary and the rights-litigation bar, argue the mechanism has drifted from error-correction toward routine policy override, citing the volume of economic and social legislation invalidated on contestable textual grounds.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).
:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) reflects the reading's genuine tangled-rope character: it solves a real coordination problem (insulating rights and structural limits from transient majorities) while imposing a real cost on democratic responsiveness that grows over time as case law accretes and legislative options narrow. Suppression (0.55) captures that legislative majorities have no formal channel to contest an adverse ruling other than constitutional amendment — a high, often practically unreachable bar. Accessibility collapse (0.6) is moderate-high: once a court fixes a doctrine, alternative interpretive paths for legislatures largely close, though scholarly and political contestation persists. Resistance (0.5) reflects ongoing, serious political and academic contestation of judicial supremacy itself (court-packing debates, calls for legislative override, popular constitutionalism movements) rather than quiet acceptance.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's and rights-claimants' seats, the arrangement looks like principled constitutional guardianship. From legislative majorities' and the broader electorate's seats, the same structure looks like an unaccountable veto exercised by an unelected body applying its own contestable readings of open-textured text. The engine computes this divergence from the declared power/exit structure; the claimed_type does not resolve it.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-claimant minorities and the judiciary itself are structural beneficiaries: the judiciary because it holds and exercises the authority, minorities because their protections are shielded from majoritarian revision. Legislative majorities and the broader electorate seeking policy change are the structural payers: their enacted preferences can be nullified with no ordinary recourse. Legislative sovereignty advocates are excluded outright — the reading's own terms give them no institutional foothold, which is precisely the structural delta this reading instantiates relative to its sibling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting entrenched rights and structural limits from transient majoritarian erosion) remains partly live — genuine rights-protective judicial interventions still occur — but the corroboration split (judiciary and rights advocates say live; legislative-sovereignty and popular-constitutionalism scholars say the mechanism has drifted into routine policy override) prevents a clean mandatrophy_resolved declaration. Classifying this as tangled_rope rather than snare or rope avoids two errors: treating the coordination function as illusory (it is not — genuine minority protection occurs) and treating the extraction as absent (it is not — legislative majorities structurally lose recourse regardless of the doctrinal merits of any particular ruling).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_compulsion_vs_doctrinal_accretion,
    'Does the constitutional text itself compel judicial supremacy (final, unreviewable interpretive authority), or is judicial supremacy a self-asserted doctrinal accretion (e.g., a Marbury-style claim) not textually mandated?',
    'Comparative textual analysis across constitutions that explicitly grant judicial finality versus those where judicial review was judicially self-established without explicit textual warrant; historical analysis of founding-era debates over whether courts were intended to have the final word.',
    'If textually compelled, the reading is closer to a mountain-adjacent structural fact within its own constitutional order; if doctrinally self-asserted, the extraction component (courts assigning themselves gatekeeping power) is more clearly an institutional acquisition rather than a designed feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_compulsion_vs_doctrinal_accretion, conceptual, 'Whether judicial supremacy is textually mandated or judicially self-granted.').

omega_variable(
    committer_reading_disagreement_location,
    'This constraint is one reading (judicial_supremacy_reading) of the contested constitutional_text kernel. Where exactly do the three readings (judicial_supremacy, legislative_sovereignty, popular_sovereignty) locate their disagreement structurally?',
    'The disagreement is located at a single structural element: who holds the FINAL, non-reviewable word on constitutional meaning. Judicial supremacy locates it in courts; legislative sovereignty locates it in the elected legislature (via override mechanisms); popular sovereignty locates it in the constituent power of the demos itself (via amendment or convention), treating both courts and legislatures as merely provisional agents. A sibling reading adopting legislative_sovereignty would restore legislative override capacity and correspondingly lower rigidity and extraction against legislative_majorities; a sibling adopting popular_sovereignty would treat both institutional readings as themselves provisional and subject to constituent revision.',
    'Confirms this story''s epsilon and beneficiary/victim structure are specific to the judicial_supremacy reading and must not be treated as representative of ''the'' constitutional text kernel as a whole.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_disagreement_location, conceptual, 'Structural location of disagreement among the three kernel readings.').

omega_variable(
    amendment_threshold_as_effective_override,
    'Is the constitutional amendment process a genuine, functionally available override mechanism for legislative majorities, or is it so high a bar (supermajority, multi-stage ratification) as to be effectively unavailable, making judicial rulings de facto permanent?',
    'Empirical study of amendment frequency and success rate specifically targeting judicial constitutional interpretations, across jurisdictions using this reading.',
    'If amendment is a live, used channel, suppression and extraction are somewhat overstated; if amendment is empirically near-impossible, the tangled_rope classification''s extraction component is understated relative to lived reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_threshold_as_effective_override, empirical, 'Whether constitutional amendment functions as a real check or a theoretical one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__judicial_supremacy_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__judicial_supremacy_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__judicial_supremacy_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__judicial_supremacy_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__judicial_supremacy_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cons_be_t10, constitutional_text__judicial_supremacy_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(cons_be_t20, constitutional_text__judicial_supremacy_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(cons_be_t30, constitutional_text__judicial_supremacy_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(cons_be_t40, constitutional_text__judicial_supremacy_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(cons_be_t50, constitutional_text__judicial_supremacy_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__judicial_supremacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cons_su_t10, constitutional_text__judicial_supremacy_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(cons_su_t20, constitutional_text__judicial_supremacy_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(cons_su_t30, constitutional_text__judicial_supremacy_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement(cons_su_t40, constitutional_text__judicial_supremacy_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(cons_su_t50, constitutional_text__judicial_supremacy_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, constitutional_text__popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'constitutional interpretive authority' per the epsilon-invariance principle. Each sibling reading (judicial_supremacy, legislative_sovereignty, popular_sovereignty) authors its own epsilon, beneficiary/victim structure, and classification rather than a single averaged constraint. This file's epsilon (0.42, tangled_rope) reflects the coordination-versus-extraction balance as the judicial supremacy reading's own proponents and critics would describe it; the sibling files will differ in beneficiary/victim assignment and likely in classification, reflecting the structurally distinct claims each reading makes about where final interpretive authority resides.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
