% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling — Constitutional Nullity Reading (14th Amendment Section 4 Supersession)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This story instantiates the constitutional-nullity reading of the debt
 *   ceiling kernel: the position, most associated with Fourteenth Amendment
 *   Section 4 scholarship ('the validity of the public debt... shall not be
 *   questioned'), that the statutory ceiling is void whenever its application
 *   would force the United States into default on validated obligations.
 *   Under this reading the ceiling has no operative legal force as applied to
 *   that scenario; Treasury's actual obligation runs directly from
 *   appropriations and the amendment, and the recurring congressional votes
 *   to 'raise' the ceiling are ceremonial confirmations of a fait accompli
 *   rather than genuine authorizations. This is a Mountain-with-FSM-candidate
 *   structure: the reading claims the statute is constitutionally inert
 *   (natural-law-like — a fixed constitutional command overrides it), yet
 *   identifiable beneficiaries (leadership extracting negotiating leverage
 *   from the public's belief the ceiling binds, and academics whose
 *   reputational capital rides on the doctrine) persist around the ceremony.
 *   This is the exact FSM signature: a claimed-natural constraint with
 *   declared beneficiaries.
 *
 * KEY AGENTS:
 *   - treasury_department: agenda_setter that must execute borrowing regardless of the statute's nominal cap (institutional/constrained)
 *   - congressional_leadership_using_ceiling_votes_as_leverage_theater: primary beneficiary of the ceremony's persistence (institutional/arbitrage)
 *   - legal_academics_advancing_nullity_doctrine: secondary beneficiary, reputational (analytical/analytical)
 *   - bondholders_and_credit_markets: observer whose pricing behavior is structurally consistent with this reading (organized/mobile)
 *   - executive_branch_office_of_legal_counsel: excluded from formally acting on the doctrine despite institutional standing to do so (institutional/trapped)
 *   - general_public_and_federal_beneficiaries: excluded, bears the anxiety cost of theater without legal exposure under this reading (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.04).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.15).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.88).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.04).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.88).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, mountain).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling — Constitutional Nullity Reading (14th Amendment Section 4 Supersession)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'bb70c164-2632-48e6-9ab5-ad2b8ec42b5c').
narrative_ontology:cs_kernel_codification('bb70c164-2632-48e6-9ab5-ad2b8ec42b5c', fixed_text).
narrative_ontology:cs_authority_grounding('bb70c164-2632-48e6-9ab5-ad2b8ec42b5c', lineage).
narrative_ontology:cs_interpretation_layer_present('bb70c164-2632-48e6-9ab5-ad2b8ec42b5c').
narrative_ontology:cs_reading_relation('bb70c164-2632-48e6-9ab5-ad2b8ec42b5c', statutory_debt_ceiling__coordination_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('bb70c164-2632-48e6-9ab5-ad2b8ec42b5c', statutory_debt_ceiling__extraction_snare_reading, influences).
narrative_ontology:cs_axiom('bb70c164-2632-48e6-9ab5-ad2b8ec42b5c', foundational, section_four_self_executing_supersession).
narrative_ontology:cs_axiom_status(section_four_self_executing_supersession, holdable).
narrative_ontology:cs_axiom_grounding('bb70c164-2632-48e6-9ab5-ad2b8ec42b5c', section_four_self_executing_supersession, conventional).
narrative_ontology:cs_axiom('bb70c164-2632-48e6-9ab5-ad2b8ec42b5c', secondary, statutory_ceiling_void_as_applied_to_default_forcing).
narrative_ontology:cs_axiom_status(statutory_ceiling_void_as_applied_to_default_forcing, holdable).
narrative_ontology:cs_axiom_grounding('bb70c164-2632-48e6-9ab5-ad2b8ec42b5c', statutory_ceiling_void_as_applied_to_default_forcing, conventional).
narrative_ontology:cs_reference_frame('bb70c164-2632-48e6-9ab5-ad2b8ec42b5c', fourteenth_amendment_section_four_supremacy_over_ordinary_statute).
narrative_ontology:cs_drift_state('bb70c164-2632-48e6-9ab5-ad2b8ec42b5c', post_2011_and_2023_debt_ceiling_standoffs, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('bb70c164-2632-48e6-9ab5-ad2b8ec42b5c', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, congressional_leadership_using_ceiling_votes_as_leverage_theater).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, legal_academics_advancing_nullity_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, bondholders_and_credit_markets).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, fourteenth_amendment_section_four_self_executing_supremacy).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, public_debt_validity_not_to_be_questioned).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Executes appropriated spending and must issue debt to fund obligations Congress already authorized. Under this reading, the ceiling statute is void as applied whenever it would force default on validated public debt, so Treasury's actual legal duty is to continue borrowing regardless of the nominal statutory cap; the ceiling votes function as a ritual Treasury indulges rather than a binding limit it obeys.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, agenda_setter,
    institutional, immediate, constrained, national).

% Continues to hold ceremonial ceiling votes and extract policy concessions in negotiations built around the statute's apparent bindingness, even though under this reading the underlying threat (default) is not legally available because the amendment nullifies the statute's coercive force. Benefits from the persistence of the public belief that the ceiling binds.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congressional_leadership_using_ceiling_votes_as_leverage_theater, beneficiary,
    institutional, biographical, arbitrage, national).

% Develop and publish the Section 4 argument, gaining scholarly and public-influence standing from being right about a live constitutional question ahead of judicial or executive confirmation. Their claim would be vindicated if a court or the executive ever acted on it.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, legal_academics_advancing_nullity_doctrine, beneficiary,
    analytical, generational, analytical, national).

% Price sovereign debt on the assumption that public debt validity is not to be questioned; under the nullity reading, their pricing is structurally correct because the ceiling cannot lawfully force default, even though headline political risk during ceiling standoffs still moves markets.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, bondholders_and_credit_markets, observer,
    organized, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, bondholders_and_credit_markets, beneficiary).

% Has repeatedly declined to formally invoke the Section 4 nullity argument despite its availability, preferring negotiated resolution to untested constitutional confrontation. Their institutional caution keeps the nullity reading a scholarly position rather than an operative legal doctrine, even though under this reading they are the party best positioned to act on it.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, executive_branch_office_of_legal_counsel, excluded,
    institutional, immediate, trapped, national).

% Rely on continued federal payments (Social Security, salaries, contracts) that would be disrupted by an actual default. Under this reading their exposure to disruption is a political-theater risk, not a real legal risk, because the constraint that would produce default is void — but they experience the anxiety and disruption of standoffs as if the risk were real.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, general_public_and_federal_beneficiaries, excluded,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__constitutional_nullity_reading, congressional_leadership_using_ceiling_votes_as_leverage_theater).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__constitutional_nullity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading there is no residual coordination function performed by the statute itself — the actual coordination of federal borrowing against appropriations is accomplished by Congress's spending and taxing decisions plus the Fourteenth Amendment's validity guarantee. The ceiling vote is a redundant ritual layered atop a settled constitutional command.
% TRANSFER_FUNCTION: Nothing is legitimately transferred by the constraint itself, because it has no binding legal force. What IS transferred is political leverage and media attention: leadership captures bargaining position from the public's belief that the ceiling is binding, at the cost of public anxiety and market volatility during standoffs.
% ABSENT_VOICES: The Supreme Court has never ruled on Section 4's application to the debt ceiling, so no authoritative voice has confirmed or foreclosed this reading. The Office of Legal Counsel has voice but has chosen not to exercise it. Ordinary debt-ceiling-dependent beneficiaries (federal retirees, contractors) have no seat in the negotiations that treat the ceiling as though it binds.
% DISAPPEARANCE_RATIONALE: If the statutory ceiling were repealed, nothing would rearrange under this reading, because the ceiling was never a binding constraint on Treasury's actual obligation to service validated public debt — the amendment already does the constraining work. The only change would be the loss of a ceremonial occasion for political theater; borrowing continues exactly as appropriations require.
% FOUNDING_PROBLEM: The 1917 Second Liberty Bond Act ceiling was built to let Treasury issue bonds more flexibly without a separate act of Congress for every issuance, replacing case-by-case authorization with an aggregate cap.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional law scholars outside Congress (e.g., commentary from law professors specializing in fiscal constitutional law, and Treasury Secretary statements during the 2011 and 2023 standoffs acknowledging the Section 4 argument's existence) attest that the flexibility-granting function the ceiling was built for has been fully superseded by modern appropriations practice and that the amendment forecloses using the ceiling to force default; no institutional actor benefiting from ceiling brinkmanship disputes the historical facts, only the doctrine's legal operability.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.04, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, ExtMetricName, E),
    domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_debt_ceiling__constitutional_nullity_reading),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.03-0.04) across the interval because, by this reading's own lights, the statute performs no binding extraction — it is constitutionally inoperative as a default-forcing mechanism, so nothing is actually transferred by its legal operation. Suppression is low-moderate (0.15) reflecting only the residual psychological/political pressure the ceremony exerts, not genuine legal coercion. Theater ratio rises sharply over the interval (0.55 to 0.88) because as the doctrine has become more widely known among specialists (post-2011, sharply post-2023) while remaining formally uninvoked by any executive branch, an increasing share of ceiling-related activity is pure performance: votes over a limit that, on this reading, cannot lawfully bind. Accessibility collapse is low (0.2) because the alternative (simply not treating the ceiling as binding) is legally available and increasingly well-articulated, not foreclosed. Resistance is moderate (0.35): the doctrine faces real institutional resistance from OLC caution and congressional incentive to preserve the leverage ritual, but it is not suppressed by force.
 *
 * PERSPECTIVAL GAP:
 *   Treasury's seat and the leadership-beneficiary seat diverge sharply: Treasury, forced to actually decide what to do as a deadline approaches, experiences the statute as either genuinely binding (if it capitulates to the theater) or void (if it acts on the amendment) — an unresolved live choice. Leadership experiences the same structure as a renewable leverage instrument regardless of its legal status, because the leverage derives from public and market *belief* in bindingness, not from the statute's actual force. The engine should register this as seat divergence between an agenda_setter under genuine operational pressure and a beneficiary profiting from perception rather than a legal fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Congressional leadership is coded as beneficiary (d near the subsidized end) because ceiling votes generate negotiating leverage and media attention without exposing leadership to the underlying constitutional risk. Academics are coded beneficiary at the analytical power level because their gains are reputational and non-material — they collect prestige, not rents. Treasury and OLC are coded near the trapped/constrained end despite institutional power, because they bear the operational and reputational cost of resolving an unresolved constitutional question under acute time pressure, with no clean exit (acting on the doctrine invites litigation and political backlash regardless of which reading is correct). The general public and market participants are downstream, bearing anxiety/volatility costs disproportionate to their voice in the process.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for flexible aggregate bond issuance authority in 1917) is dead by this reading's own account — modern appropriations and Section 4 fully perform that coordination role without the ceiling's help — yet the ceiling ceremony persists and expands (rising theater_ratio) precisely because it has been repurposed into a leverage instrument. Classifying the underlying statute as a nullified Mountain-adjacent structure with FSM-flagged beneficiaries prevents two mislabeling errors: (1) treating the ceiling as still-necessary coordination (it is not, under this reading — its original function is dead), and (2) treating leadership's continued invocation of it as pure extraction with no genuine constitutional question behind it (there IS a live, unresolved constitutional question, which is why OLC has not acted). The FSM signature — claimed natural/void constraint with declared beneficiaries collecting from the ceremony surrounding it — is exactly the divergence this reading is built to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    section_four_justiciability_and_standing,
    'Would any court ever reach the merits of a Section 4 nullity claim, or would justiciability doctrines (standing, political question) prevent judicial resolution regardless of the underlying constitutional merits?',
    'A live case would need a plaintiff with concrete injury from an actual or imminent default event; absent an actual breach, courts have historically avoided reaching the substantive question. Resolution requires either an actual near-default event generating a justiciable injury, or a change in standing doctrine.',
    'If courts would never reach the merits, the nullity reading remains permanently unconfirmed and its operative status is entirely a function of executive branch willingness to act unilaterally — strengthening the case that the current near-zero extractiveness score describes an aspirational/scholarly state rather than a confirmed operative one. If a clear justiciable path exists, the doctrine could be tested and become authoritative, closing the loop this story currently leaves open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(section_four_justiciability_and_standing, empirical, 'Whether the constitutional question could ever be judicially resolved absent an actual default.').

omega_variable(
    natural_law_vs_constructed_doctrine_ambiguity,
    'Is the claim that the ceiling is constitutionally void a discovery of a pre-existing constitutional fact (natural-law-like, hence the Mountain claim), or is it a constructed legal argument whose persuasiveness depends on contestable interpretive choices about Section 4''s scope and self-execution?',
    'Compare with the historical drafting purpose of Section 4 (Civil War Confederate debt repudiation context) against its modern application to routine peacetime fiscal politics; examine whether originalist and living-constitutionalist methodologies converge or diverge on the doctrine''s reach.',
    'If the doctrine is a genuine discovered constitutional fact, the Mountain claim is well-grounded and the beneficiaries (academics, leadership) are incidental riders on a true structural feature. If it is substantially a constructed interpretive argument advanced partly because of its rhetorical and reputational payoff to its proponents, the FSM flag is doing real diagnostic work — this constraint may be closer to a contested tangled_rope dressed as settled law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine_ambiguity, conceptual, 'Whether Section 4 nullity is discovered constitutional bedrock or a constructed doctrine with identifiable beneficiaries.').

omega_variable(
    committer_disagreement_locus,
    'Where exactly do the three sibling readings of the debt ceiling kernel disagree — is it a disagreement about the statute''s current legal validity (this reading vs. coordination_scaffold_reading), or about the statute''s functional purpose and effects regardless of validity (this reading vs. extraction_snare_reading)?',
    'Structural comparison of the three constraint files: the nullity reading and the scaffold reading disagree about whether the statute currently binds at all; the nullity reading and the snare reading can actually agree that the ceiling is misused for extraction while disagreeing about whether that misuse has any genuine legal force behind it.',
    'If the disagreement with extraction_snare_reading is primarily about legal operability rather than about whether extraction-shaped behavior occurs, then this reading and the snare reading may be less opposed than the labels suggest — both could agree leadership extracts leverage from the ceremony, differing only on whether that leverage is legally backed. This affects how the network edges (coexists_with vs. influences) should be read downstream.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_disagreement_locus, conceptual, 'Locating precisely which premise each sibling reading disputes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(stat_tr_t5, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(stat_tr_t10, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 10, 0.7).
narrative_ontology:measurement(stat_tr_t15, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 15, 0.76).
narrative_ontology:measurement(stat_tr_t20, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 20, 0.81).
narrative_ontology:measurement(stat_tr_t25, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 25, 0.85).
narrative_ontology:measurement(stat_tr_t30, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 30, 0.88).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 0, 0.03).
narrative_ontology:measurement(stat_be_t5, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 5, 0.03).
narrative_ontology:measurement(stat_be_t10, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 10, 0.04).
narrative_ontology:measurement(stat_be_t15, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 15, 0.04).
narrative_ontology:measurement(stat_be_t20, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 20, 0.04).
narrative_ontology:measurement(stat_be_t25, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 25, 0.04).
narrative_ontology:measurement(stat_be_t30, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 30, 0.04).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(statutory_debt_ceiling__constitutional_nullity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_debt_ceiling__constitutional_nullity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(statutory_debt_ceiling__constitutional_nullity_reading, 0.05).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling__extraction_snare_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the statutory_debt_ceiling kernel. The coordination_scaffold_reading treats the same statute as a genuinely functional procedural mechanism (low-moderate extraction, real coordination benefit). The extraction_snare_reading treats it as a weaponized default-threat mechanism enabling minority extraction (high extraction, identifiable victims among federal beneficiaries and market participants). This reading's ε (0.03-0.04) differs from the scaffold reading's expected mid-range ε and the snare reading's expected high ε because, on this reading, the mechanism that would produce either genuine coordination cost or genuine extraction (a legally credible default threat) is constitutionally unavailable — collapsing extraction toward zero regardless of the political theater surrounding the statute. All three readings share the same statutory text, history, and stakeholder cast but diverge entirely on operative legal status and therefore on structural type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
