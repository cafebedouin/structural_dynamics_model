% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Compact Federalism Reading of the Provincial Sovereignty Boundary
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This story instantiates the compact_federalism reading of the provincial
 *   sovereignty boundary kernel: Confederation is read as a voluntary compact
 *   among sovereign provinces that retained residual sovereignty never fully
 *   surrendered to the federal order, with exit theoretically negotiable
 *   rather than categorically foreclosed. Under this reading, federal
 *   authority over national programs — equalization formulas, climate policy,
 *   resource-adjacent regulation — is conditional on provincial consent
 *   rather than a matter of settled constitutional subordination. This is NOT
 *   a story about which reading is correct; it authors the compact reading's
 *   own structural consequences as if that reading governs. The coordination
 *   function (voluntary union without absorption) is genuine and historically
 *   grounded, but the same framing that protects provincial policy space is
 *   used asymmetrically: resource-exporting provinces with credible exit
 *   leverage extract concessions that equalization-dependent provinces and
 *   national climate constituencies cannot resist, because they lack
 *   comparable bargaining chips. That asymmetry, not the compact idea itself,
 *   is what the metrics track.
 *
 * KEY AGENTS:
 *   - resource_exporting_provinces: primary beneficiary (powerful/arbitrage) — uses compact framing as negotiating leverage
 *   - federal_equalization_recipient_provinces: primary payer (moderate/trapped) — bears formula instability with no comparable leverage
 *   - national_climate_policy_constituencies: secondary payer (moderate/constrained) — bears delayed/diluted national policy
 *   - federal_government: institutional payer/agenda_setter (institutional/constrained) — bears negotiation costs and implementation friction
 *   - indigenous_nations_outside_bilateral_process: excluded (powerless/trapped) — sovereignty claim absent from the compact's own account
 *   - constitutional_law_scholars: analytical observer — documents the contest without resolving it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.52).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.44).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.52).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Compact Federalism Reading of the Provincial Sovereignty Boundary").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, '4f35e9ff-9e4e-42be-a301-e0f89bcdc026').
narrative_ontology:cs_kernel_codification('4f35e9ff-9e4e-42be-a301-e0f89bcdc026', distributed).
narrative_ontology:cs_authority_grounding('4f35e9ff-9e4e-42be-a301-e0f89bcdc026', distributed).
narrative_ontology:cs_reading_relation('4f35e9ff-9e4e-42be-a301-e0f89bcdc026', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('4f35e9ff-9e4e-42be-a301-e0f89bcdc026', provincial_sovereignty_boundary__resource_sovereignty_primacy, coexists_with).
narrative_ontology:cs_axiom('4f35e9ff-9e4e-42be-a301-e0f89bcdc026', foundational, residual_provincial_sovereignty_predates_federation).
narrative_ontology:cs_axiom_status(residual_provincial_sovereignty_predates_federation, holdable).
narrative_ontology:cs_axiom_grounding('4f35e9ff-9e4e-42be-a301-e0f89bcdc026', residual_provincial_sovereignty_predates_federation, conventional).
narrative_ontology:cs_axiom('4f35e9ff-9e4e-42be-a301-e0f89bcdc026', foundational, exit_requires_negotiation_not_federal_permission).
narrative_ontology:cs_axiom_status(exit_requires_negotiation_not_federal_permission, holdable).
narrative_ontology:cs_axiom_grounding('4f35e9ff-9e4e-42be-a301-e0f89bcdc026', exit_requires_negotiation_not_federal_permission, conventional).
narrative_ontology:cs_reference_frame('4f35e9ff-9e4e-42be-a301-e0f89bcdc026', confederation_era_sovereign_compact).
narrative_ontology:cs_drift_state('4f35e9ff-9e4e-42be-a301-e0f89bcdc026', post_1982_patriation_and_secession_reference, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4f35e9ff-9e4e-42be-a301-e0f89bcdc026', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, resource_exporting_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, provincial_governments).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_equalization_recipient_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, national_climate_policy_constituencies).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, indigenous_nations_outside_bilateral_process).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, residual_provincial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__compact_federalism, consent_based_confederation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke the compact reading to resist federal carbon pricing, pipeline vetoes, and equalization formulas they see as extractive of their resource wealth. Threaten non-cooperation, court challenges, and periodic secession rhetoric as leverage. Their exit threat is credible enough to extract policy concessions without ever being exercised — they benefit from perpetual renegotiation rather than actual departure.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, resource_exporting_provinces, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, resource_exporting_provinces, agenda_setter).

% As a class, gain constitutional leverage from the compact framing: it grounds claims to opt out of federal programs, negotiate side deals, and demand unanimous consent for constitutional amendment. Weaker provinces benefit less concretely than resource-rich ones but still gain rhetorical ground against federal overreach.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, provincial_governments, beneficiary,
    institutional, generational, constrained, national).

% Depend on equalization transfers funded partly by resource-derived federal revenue. When resource-exporting provinces successfully renegotiate equalization formulas or resist federal fiscal coordination under the compact framing, transfer levels and predictability suffer. These provinces have no comparable leverage — no resource endowment to threaten withholding, and no realistic exit option of their own.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_equalization_recipient_provinces, payer,
    moderate, biographical, trapped, national).

% Support binding national emissions targets and carbon pricing floors. Under the compact reading, provincial override of climate policy is treated as a legitimate exercise of residual sovereignty rather than a defection from a settled national commitment. Their preferred policy path is repeatedly delayed or diluted through provincial carve-outs negotiated under threat of non-compliance.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, national_climate_policy_constituencies, payer,
    moderate, generational, constrained, national).

% Hold treaty and inherent sovereignty claims that predate and are structurally independent of the provincial-federal compact. The compact_federalism reading treats sovereignty as bilaterally shared between two orders of settler government, with no seat at the negotiating table for a third sovereignty claim. Their objections surface in litigation and treaty rights advocacy but are not incorporated into the compact's own account of who holds residual sovereignty.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, indigenous_nations_outside_bilateral_process, excluded,
    powerless, civilizational, trapped, regional).

% Must treat national programs as subject to provincial consent rather than unilateral constitutional authority. Retains formal jurisdiction on paper but bears the cost of the compact framing whenever it needs provincial cooperation to implement policy with national reach — carbon pricing, health transfers, securities regulation. Cannot compel compliance without protracted negotiation, litigation risk, or asymmetric side deals.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, federal_government, agenda_setter).

% Debate whether the 1867/1982 constitutional settlement is best read as a compact among pre-existing sovereign entities or as a unilateral constitutional grant. Their scholarship is cited by all sides but does not itself resolve which reading governs; they document the contest rather than settle it.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(provincial_sovereignty_boundary__compact_federalism, resource_exporting_provinces).
narrative_ontology:fixing_cost_class(provincial_sovereignty_boundary__compact_federalism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The compact reading genuinely solves a coordination problem for the founding provinces and their successors: it lets jurisdictions with different resource endowments, populations, and policy preferences enter and remain in a shared federation without surrendering the capacity to protect locally distinct interests, avoiding both outright secession and total centralization.
% TRANSFER_FUNCTION: Moves de facto policy authority and fiscal leverage from the federal government and from provinces with weaker exit threats toward resource-exporting provinces able to credibly threaten non-cooperation; moves predictability and program uniformity away from equalization-dependent provinces and national climate constituencies.
% ABSENT_VOICES: Indigenous nations whose sovereignty claims predate and are independent of the provincial-federal compact are not parties to the compact's own account of residual sovereignty; their treaty rights are litigated at the margins of a framework that was never built to include a third sovereign order. Future generations bearing climate costs are also absent from a bargaining table organized around present provincial governments.
% DISAPPEARANCE_RATIONALE: If the compact reading were repudiated overnight in favor of unconditional constitutional subordination, resource-exporting provinces would lose their principal leverage over equalization formulas and climate policy, federal programs could be imposed without provincial consent mechanisms, and the negotiated-exit posture that currently substitutes for actual secession would disappear — provincial governments would either accept subordination or escalate toward genuine constitutional crisis.
% FOUNDING_PROBLEM: At Confederation, distinct colonies with different economies, religious settlements, and degrees of self-government needed a framework to unite without any one colony being simply absorbed or extinguished by the others — a problem of voluntary union among parties wary of domination.
% FOUNDING_PROBLEM_CORROBORATION: Resource-exporting provincial governments and their legal counsel attest the founding problem remains live — that residual sovereignty is a continuing constitutional fact, not a historical artifact. Federal officials, most constitutional law scholars outside provincial government retainer, and equalization-recipient provinces attest the compact framing has been substantially overtaken by 1982 patriation and Supreme Court doctrine (notably the Secession Reference's rejection of unilateral secession), and that its current invocation functions more as bargaining leverage than as settled constitutional law.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) reflects moderate but real asymmetric transfer: the compact framing does not extract wealth directly, but it extracts policy predictability and fiscal reliability from weaker provinces and diffuse national constituencies, redirected toward provinces with credible exit threats. Suppression (0.44) is mid-range — provincial governments are not coerced into accepting the compact reading, but federal officials and equalization-dependent provinces face real constraint on their ability to contest it without triggering constitutional crisis rhetoric. Theater ratio (0.28) is modest: much of the compact's coordination function (avoiding secession, managing genuine regional diversity) is real, though an increasing share of invocation is strategic positioning rather than functional necessity, hence the slow upward drift over the 1982-2025 interval as resource federalism disputes (carbon pricing, pipeline approvals, equalization referenda) intensified.
 *
 * PERSPECTIVAL GAP:
 *   Resource-exporting provinces experience this constraint as a rope — a genuine, hard-won coordination structure protecting legitimate local interests against federal overreach. Equalization-recipient provinces and national climate constituencies experience the same structure as an enforcement mechanism that lets wealthier provinces extract policy concessions under threat of non-cooperation. The federal government experiences it as a tangled rope: real coordination benefit (avoiding secession, preserving national unity) bundled with real extraction cost (inability to implement uniform national policy without asymmetric side-payments).
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-exporting provinces sit near the beneficiary end: their exit threat is credible (arbitrage-grade — they have genuine alternative revenue and periodic separatist movements to draw on), so the constraint subsidizes their bargaining position. Equalization-recipient provinces sit near the target end: trapped exit (no comparable resource base, no credible secession threat), so the same compact framing that empowers resource provinces extracts predictability from them. National climate constituencies are diffuse and constrained rather than trapped, but bear real policy delay costs. The federal government occupies an unusual dual position — nominally the highest-power institutional actor, but constrained by the compact reading's consent requirement, which functions here as a downward-directionality force despite the government's formal institutional power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (voluntary union of wary, unequal colonies) was substantially real in 1867 and remains partially live insofar as genuine regional diversity persists. But the specific compact framing's current use — leveraging residual sovereignty rhetoric to resist post-1982 constitutional developments like the Secession Reference's centralizing implications — is better read as adaptive reuse of a founding-era justification for present-day bargaining advantage, rather than continuous fidelity to the original coordination problem. This is why founding_problem_status is authored as contested rather than simply dead: the coordination function has not fully atrophied, but its invocation has drifted from managing genuine union anxiety toward managing fiscal and regulatory leverage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_vs_subordination_framing,
    'Is the 1867/1982 constitutional settlement structurally a compact among pre-existing sovereign entities, or a unilateral constitutional grant creating provinces with only delegated authority?',
    'This is not resolvable by further constitutional text analysis alone — the Supreme Court''s Secession Reference (1998) addressed adjacent questions without fully adjudicating the compact-vs-subordination framing itself. Resolution would require either a definitive constitutional amendment settling the question or a sustained judicial and political consensus that has not yet emerged.',
    'If the compact reading is structurally correct, provincial consent requirements for national programs are constitutionally grounded rather than merely politically negotiated, strengthening resource-exporting provinces'' leverage. If constitutional_subordination is correct, the federal government''s formal authority is unconditional and the compact framing is a political fiction with real bargaining effects but no constitutional force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compact_vs_subordination_framing, conceptual, 'Whether Confederation is structurally a sovereign compact or a unilateral constitutional grant — the central kernel contest this reading takes one side of.').

omega_variable(
    sibling_reading_resource_sovereignty_delta,
    'How would this constraint''s classification change if the resource_sovereignty_primacy reading (grounding absolute provincial sovereignty in s.92A resource ownership) were adopted instead of the compact reading authored here?',
    'Compare the two readings'' beneficiary/victim structures directly: resource_sovereignty_primacy would ground sovereignty claims exclusively in resource-owning provinces, likely sharpening the asymmetry between resource-exporting and non-resource provinces beyond what the compact reading (which nominally applies to all provinces) implies.',
    'Under resource_sovereignty_primacy, non-resource provinces would have essentially no sovereignty claim at all, whereas under compact_federalism they retain formal (if practically weaker) residual sovereignty. The resource_sovereignty_primacy reading would likely compute as more extractive and more narrowly beneficiary-concentrated than this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_resource_sovereignty_delta, conceptual, 'Structural delta between this reading and the resource_sovereignty_primacy sibling reading of the same kernel.').

omega_variable(
    indigenous_sovereignty_incorporation,
    'Does the compact_federalism reading''s silence on Indigenous sovereignty claims reflect a genuine two-party historical settlement, or an exclusion that the compact framing has never adequately confronted?',
    'Treaty rights litigation outcomes and truth and reconciliation processes provide an ongoing empirical record; a definitive resolution would require the compact framing itself to be reconstructed as a three-order (or more) sovereignty arrangement, which has not occurred in current constitutional practice.',
    'If Indigenous sovereignty is incorporated as a third order, the entire compact_federalism kernel would need restructuring rather than mere reinterpretation — this omega marks a boundary condition on the kernel''s own scope, not just this reading''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_sovereignty_incorporation, preference, 'Whether the two-party (federal/provincial) compact framing can or should incorporate Indigenous sovereignty as a structurally independent third claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 1982, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t1982, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1982, 0.18).
narrative_ontology:measurement(prov_tr_t1990, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(prov_tr_t1998, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 1998, 0.22).
narrative_ontology:measurement(prov_tr_t2006, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2006, 0.24).
narrative_ontology:measurement(prov_tr_t2014, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2014, 0.26).
narrative_ontology:measurement(prov_tr_t2020, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(prov_tr_t2025, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(prov_be_t1982, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1982, 0.32).
narrative_ontology:measurement(prov_be_t1990, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1990, 0.36).
narrative_ontology:measurement(prov_be_t1998, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 1998, 0.4).
narrative_ontology:measurement(prov_be_t2006, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2006, 0.44).
narrative_ontology:measurement(prov_be_t2014, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2014, 0.48).
narrative_ontology:measurement(prov_be_t2020, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement(prov_be_t2025, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t1982, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1982, 0.3).
narrative_ontology:measurement(prov_su_t1990, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1990, 0.32).
narrative_ontology:measurement(prov_su_t1998, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 1998, 0.35).
narrative_ontology:measurement(prov_su_t2006, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2006, 0.38).
narrative_ontology:measurement(prov_su_t2014, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2014, 0.4).
narrative_ontology:measurement(prov_su_t2020, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(prov_su_t2025, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 2025, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__compact_federalism, 0.12).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, constitutional_subordination).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, resource_sovereignty_primacy).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the provincial_sovereignty_boundary kernel. constitutional_subordination reads the same founding moment as producing provinces with only delegated, federally-conditional authority — under that reading equalization and climate policy are federal prerogatives not subject to provincial override, and ε for the arrangement it describes would be authored much lower (near-mountain, settled constitutional order) or reframed with a different victim set (federal minorities within provinces). resource_sovereignty_primacy grounds sovereignty exclusively in s.92A resource ownership, producing a narrower and more concentrated beneficiary set than this reading's nominally universal (but practically asymmetric) provincial sovereignty claim. All three stories share the same underlying kernel text and historical record but diverge in claimed_type, beneficiary/victim structure, and ε because each reading treats a different feature of the constitutional settlement as authoritative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
