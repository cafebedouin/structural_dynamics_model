% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Fragmented Marriage Authority as Consociational Anti-Majoritarian Mechanism
 *   domain: legal/constitutional/political
 *
 * SUMMARY:
 *   This constraint instantiates the federalist/millet reading of the
 *   marriage-authority kernel: fragmented personal law jurisdiction is
 *   understood as a deliberate consociational design choice — a check against
 *   any majority faction converting numerical dominance into legislative
 *   control over family law for the whole polity. On this reading, the
 *   persistence of multiple community-administered codes and the recurring
 *   failure of uniformization proposals to pass are not institutional
 *   dysfunction but the intended operation of an anti-tyranny mechanism,
 *   comparable to Ottoman millet governance or Lijphart-style consociational
 *   power-sharing in divided societies. ε is authored low: the elite bargain
 *   that sustains fragmentation involves relatively modest active coercion of
 *   dissenters at the aggregate level and a genuine, non-fictitious
 *   coordination function (preventing majoritarian capture of an intensely
 *   salient policy domain). This is a distinct constraint from the
 *   communal_autonomy_reading (which grounds authority in religious tradition
 *   rather than elite anti-majoritarian bargaining), the secularist_reading
 *   (which treats the same pluralism as a defect awaiting a Uniform Civil
 *   Code), the gender_rights_reading (which treats the same codes as sites of
 *   intra-community rights violation to be resolved via constitutional
 *   equality litigation), and the judicial_harmonization_reading (which
 *   treats the same terrain as being incrementally unified by courts rather
 *   than preserved by legislative design). Each sibling reads the SAME
 *   underlying multi-code arrangement through a different lens and would
 *   author a different ε and a different beneficiary/victim structure; this
 *   file authors only the federalist/millet lens.
 *
 * KEY AGENTS:
 *   - minority_communities: primary beneficiary (organized/constrained) — protected from majoritarian imposition
 *   - religious_community_leaderships: agenda_setter and secondary beneficiary (organized/arbitrage) — administer and defend fragmentation
 *   - coalition_governing_elites: agenda_setter (institutional/analytical) — architects and maintainers of the consociational bargain
 *   - intra_community_dissenters: payer (powerless/trapped) — bear the cost of the bargain's stability
 *   - national_majority_electorate: excluded (organized/constrained) — structurally denied ordinary majoritarian recourse on this issue
 *   - constitutional_scholars: analytical observer — assess genuineness of the anti-tyranny function over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.22).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.18).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Fragmented Marriage Authority as Consociational Anti-Majoritarian Mechanism").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal/constitutional/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, '69be8fef-a975-4543-bdfe-8d8b5ffa64d7').
narrative_ontology:cs_kernel_codification('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', distributed).
narrative_ontology:cs_authority_grounding('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', distributed).
narrative_ontology:cs_reading_relation('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', foundational, fragmentation_is_designed_anti_tyranny_check).
narrative_ontology:cs_axiom_status(fragmentation_is_designed_anti_tyranny_check, holdable).
narrative_ontology:cs_axiom_grounding('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', fragmentation_is_designed_anti_tyranny_check, instrumental).
narrative_ontology:cs_axiom('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', secondary, legislative_paralysis_is_stability_not_failure).
narrative_ontology:cs_axiom_status(legislative_paralysis_is_stability_not_failure, holdable).
narrative_ontology:cs_axiom_grounding('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', legislative_paralysis_is_stability_not_failure, conventional).
narrative_ontology:cs_reference_frame('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', founding_era_consociational_settlement).
narrative_ontology:cs_drift_state('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', contemporary_uniform_code_debate, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('69be8fef-a975-4543-bdfe-8d8b5ffa64d7', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, religious_community_leaderships).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, coalition_governing_elites).
narrative_ontology:constraint_victim(marriage_authority__federalist_millet_reading, intra_community_dissenters).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, consociational_stability_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, anti_majoritarian_constitutional_design).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain jurisdiction over their own marriage, divorce, and succession law through personal law codes. This shields them from having a numerically dominant majority's family-law norms imposed on them via ordinary legislative majority. Exit from the arrangement as a community would mean accepting majoritarian family law; exit as an individual member is harder.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_communities, beneficiary,
    organized, generational, constrained, national).

% Administer personal law within their communities and negotiate, at the elite level, to preserve the fragmentation that keeps their jurisdiction intact. They benefit from being the recognized interlocutor for their community's family law and from the veto power fragmentation gives them over uniformization proposals.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, religious_community_leaderships, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__federalist_millet_reading, religious_community_leaderships, beneficiary).

% Maintain the multi-code system as a deliberate constitutional bargain that keeps a diverse, multi-confessional polity governable without a majority faction capturing family law wholesale. They treat legislative paralysis on uniformization as a designed safety valve, not a failure of governance capacity.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, coalition_governing_elites, agenda_setter,
    institutional, generational, analytical, national).

% Individuals within a minority community — often women seeking divorce, maintenance, or inheritance rights — who are bound to the community's personal law regime with no legislative recourse, because opening that code to reform would be read as an attack on the fragmentation bargain itself. Their individual grievance is structurally subordinated to the elite-level anti-majoritarian settlement.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, intra_community_dissenters, payer,
    powerless, biographical, trapped, local).

% Would, under ordinary majoritarian legislative process, be able to set a single family law code reflecting majority preference. The fragmented-authority arrangement is specifically designed to prevent this outcome, so the majority's preference on family law uniformity is structurally excluded from ordinary legislative channels regardless of electoral mandate.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, national_majority_electorate, excluded,
    organized, biographical, constrained, national).

% Study the arrangement as an instance of consociational power-sharing theory (Lijphart-style segmental autonomy), comparing it to Ottoman millet systems and other divided-society constitutional designs, and assess whether the anti-tyranny function is genuine or has calcified into elite cartelization.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__federalist_millet_reading, religious_community_leaderships).
narrative_ontology:fixing_cost_class(marriage_authority__federalist_millet_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents any single religious or cultural majority from using ordinary legislative majority power to impose its family law norms on minority communities, by distributing marriage/divorce/succession authority across multiple community-administered codes rather than concentrating it in one national legislature.
% TRANSFER_FUNCTION: Moves the power to set family law norms from a potential national legislative majority to community-level religious leaderships; within each community, it also moves recourse and reform leverage away from individual dissenting members toward the community's own institutional authorities.
% ABSENT_VOICES: Intra-community dissenters (frequently women within minority personal law regimes) would object that the anti-majoritarian bargain is purchased at their expense, but they are not party to the elite-level consociational negotiation — their voice enters only through litigation or gender-rights advocacy that operates outside this reading's own framework.
% DISAPPEARANCE_RATIONALE: If fragmented authority disappeared and a single uniform code were imposed overnight, minority communities would lose the primary structural guarantee against majoritarian imposition on family law, religious leaderships would lose their institutional standing as legal interlocutors, and the consociational bargain underlying the broader polity's stability would need to be renegotiated on other terms — a significant constitutional rearrangement, not a null event.
% FOUNDING_PROBLEM: In a religiously and culturally plural polity, a numerically dominant majority could use ordinary legislative process to impose its family law norms on minorities as a matter of routine majority rule, absent some structural check; fragmented marriage authority was adopted as that check, negotiated as part of the founding constitutional bargain among communal elites and framers.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and comparative consociational-democracy scholars (writing independently of any community's leadership) corroborate that the founding-era bargain was explicitly anti-majoritarian in design and cite comparable millet and consociational arrangements elsewhere. Intra-community dissenters and gender-rights litigants, from outside the benefiting elite coalition, attest that whatever the founding problem was, its present function is increasingly to shield community leaderships from internal reform pressure rather than to check national majoritarianism, which they experience as largely quiescent on this issue in the current period.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22 at interval end) because, under this reading, the primary function is a genuine coordination good — preventing majoritarian capture of a highly salient domain in a plural polity — and the aggregate coercive overhead required to sustain it is modest: no single actor need be actively suppressed at scale for the arrangement to hold, since the elite bargain is largely self-enforcing through mutual veto rather than coercive imposition. Suppression is similarly low-moderate (0.18): the mechanism operates mostly through structural non-legislation (paralysis) rather than active coercive enforcement against a target population, though the cost this imposes on intra-community dissenters is real and undercounted at the aggregate level. Theater ratio is low (0.15) because the paralysis is functionally load-bearing (it does the anti-majoritarian work), not merely performative — though it drifts slightly upward across the interval as uniformization proposals recur and are re-defeated in increasingly ritualized fashion. accessibility_collapse is moderate (0.35): for communities and elites, meaningful alternative arrangements (uniform code, judicial harmonization) remain visible and periodically proposed — this is not a foreclosed, mountain-like structure. Resistance is moderate (0.4), driven principally by secularist and gender-rights advocates who contest the arrangement's benign framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority communities and religious leaderships sit near the beneficiary end: the arrangement subsidizes their institutional standing and shields their family-law jurisdiction from majoritarian override. Coalition governing elites benefit from the stability the bargain buys the broader polity. Intra-community dissenters sit near the target end: they bear the cost of a jurisdictional shield they did not individually choose and cannot easily exit, since exit from a community's personal law regime typically means exit from the community itself. The national majority electorate is excluded rather than directly extracted from — their preference on uniformity is structurally foreclosed from ordinary legislative channels, which is a different relationship than bearing a transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure extraction (which the gender_rights_reading effectively does for the same underlying facts) by crediting the genuine coordination function: in a plural polity, unchecked majoritarian control of family law is a real risk this design addresses, not a manufactured pretext. It equally prevents mislabeling it as a pure rope with no costs, by naming intra-community dissenters as payers whose interests are structurally subordinated to the elite bargain. The founding_problem interview captures the temporal question directly: the founding problem (anti-majoritarian protection) may remain partly live at the national level while having drifted, at the community level, into a shield for leaderships against internal reform — this is exactly the contested status the R5 fields are built to surface without adjudicating it as settled fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_anti_majoritarian_function_vs_elite_cartel,
    'Is the fragmented-authority arrangement still functioning as a genuine check against majoritarian capture of family law, or has it calcified into a cartel arrangement among community leaderships that primarily shields them from internal reform pressure?',
    'Track whether uniformization proposals that would specifically increase intra-community accountability (without imposing external majoritarian norms) are treated identically to proposals that would impose majority-community norms; convergent leadership opposition to both would indicate cartelization rather than anti-majoritarian function.',
    'If the arrangement is discriminating between majoritarian-imposition threats and accountability-enhancing reform and opposing only the former, the rope classification is well-supported. If it opposes both equally, the true function is closer to tangled_rope, with community leaderships as an additional concentrated beneficiary alongside the anti-majoritarian coordination good.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_anti_majoritarian_function_vs_elite_cartel, empirical, 'Whether fragmentation still discriminates majoritarian threats from internal accountability, or blocks both uniformly.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the fragmented marriage-authority arrangement more accurately framed as an elite-bargained anti-majoritarian design (this reading) or as an organic extension of pre-existing communal religious authority that the state merely ratified (communal_autonomy_reading)?',
    'Comparative constitutional history: examine framing-era debates and drafting records for explicit anti-majoritarian rationale versus deference-to-existing-authority rationale; the two produce different beneficiary structures (elite negotiators vs. religious authorities per se) even though the resulting legal arrangement looks identical.',
    'If the historical record favors the elite-bargain framing, this reading''s ε and beneficiary set are well-grounded. If it favors the pre-existing-authority framing, the correct classification may shift toward the communal_autonomy_reading''s structure, with less emphasis on ''anti-tyranny mechanism'' and more on inherited jurisdictional deference.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the elite-bargain and pre-existing-authority framings of the same historical arrangement are actually distinguishable in the drafting record.').

omega_variable(
    paralysis_as_feature_durability,
    'Is legislative paralysis on uniformization a stable equilibrium feature of the design, or a transitional condition that will eventually resolve toward either judicial harmonization or secularist uniformization as external pressure accumulates?',
    'Longitudinal tracking of judicial incursion into personal law domains (per the judicial_harmonization_reading) and legislative uniformization attempts (per the secularist_reading) — if judicial or legislative movement accelerates, paralysis was transitional, not a stable feature.',
    'If paralysis proves durable, the rope classification and the ''stability feature not bug'' framing are vindicated over the long run. If paralysis is being eroded by judicial or legislative pressure, this reading may be describing a constraint already in the process of yielding to a sibling reading''s dynamic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paralysis_as_feature_durability, empirical, 'Whether legislative paralysis is a durable equilibrium or a transitional phase yielding to judicial or legislative resolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(marr_tr_t14, marriage_authority__federalist_millet_reading, theater_ratio, 14, 0.08).
narrative_ontology:measurement(marr_tr_t28, marriage_authority__federalist_millet_reading, theater_ratio, 28, 0.1).
narrative_ontology:measurement(marr_tr_t42, marriage_authority__federalist_millet_reading, theater_ratio, 42, 0.12).
narrative_ontology:measurement(marr_tr_t56, marriage_authority__federalist_millet_reading, theater_ratio, 56, 0.14).
narrative_ontology:measurement(marr_tr_t70, marriage_authority__federalist_millet_reading, theater_ratio, 70, 0.15).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(marr_be_t14, marriage_authority__federalist_millet_reading, base_extractiveness, 14, 0.13).
narrative_ontology:measurement(marr_be_t28, marriage_authority__federalist_millet_reading, base_extractiveness, 28, 0.16).
narrative_ontology:measurement(marr_be_t42, marriage_authority__federalist_millet_reading, base_extractiveness, 42, 0.19).
narrative_ontology:measurement(marr_be_t56, marriage_authority__federalist_millet_reading, base_extractiveness, 56, 0.21).
narrative_ontology:measurement(marr_be_t70, marriage_authority__federalist_millet_reading, base_extractiveness, 70, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_authority__federalist_millet_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language concept 'marriage authority' per the ε-invariance principle. All five describe the same underlying multi-code personal law arrangement but differ in claimed_type, ε, and beneficiary/victim structure because they adopt different framings of what the arrangement IS FOR: anti-majoritarian design (this story, ε≈0.22, rope), inherited communal jurisdiction (communal_autonomy_reading), transitional anomaly (secularist_reading, likely higher ε as extraction from women/dissenters), intra-community rights violation (gender_rights_reading, likely snare or tangled_rope), and judicially-evolving constitutional floor (judicial_harmonization_reading). Network edges link all five as one constraint family; contamination or purity findings in one reading should be checked against the others for consistency of the underlying facts even though classifications diverge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
