% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__progressive_textualist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__progressive_textualist, []).

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
 *   constraint_id: equality_clause_scope__progressive_textualist
 *   human_readable: Equality Clause Scope — Progressive Textualist Reading (Amendment-Gated Expansion)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the progressive textualist reading of the
 *   equality clause scope kernel: the founding text contains a genuine
 *   equality principle, but the population it covers expands only through the
 *   formal democratic amendment process — ratified supermajority consent —
 *   not through judicial reinterpretation of the existing text. Historically
 *   this maps onto the sequence of suffrage and citizenship amendments that
 *   extended coverage incrementally, each requiring broad political
 *   coalition-building rather than a single court's declaration. The reading
 *   occupies a middle position: it rejects the originalist claim that the
 *   scope is permanently fixed to the founding social contract, but it also
 *   rejects the universalist claim that courts may simply recognize the
 *   self-evident scope was always universal. Extraction and suppression trend
 *   downward over the interval as successive amendments have, in fact,
 *   expanded formal coverage — but both metrics remain non-trivial at the
 *   interval's end because the amendment gate itself continues to exclude
 *   populations unable to assemble supermajority support, and the theater
 *   ratio rises modestly as formal amendment activity slows relative to the
 *   rhetorical invocation of 'gradual constitutional progress' as sufficient
 *   answer to ongoing exclusion.
 *
 * KEY AGENTS:
 *   - legislative_supermajority_coalitions: agenda_setter — controls the pace and occurrence of scope expansion
 *   - groups_excluded_pending_amendment: payer — bears the cost of exclusion until ratification
 *   - minority_populations_below_amendment_threshold: payer — structurally unable to clear the threshold
 *   - enfranchised_amendment_era_groups: beneficiary — inclusion is stabilized against judicial reversal
 *   - constitutional_stability_interests: beneficiary/observer — values predictability over speed of inclusion
 *   - judiciary_seeking_expansive_interpretation: excluded — interpretive authority over scope is foreclosed
 *   - constitutional_historians: observer — assesses whether amendment-gated expansion has tracked moral consensus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__progressive_textualist, 0.42).
domain_priors:suppression_score(equality_clause_scope__progressive_textualist, 0.48).
domain_priors:theater_ratio(equality_clause_scope__progressive_textualist, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, extractiveness, 0.42).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equality_clause_scope__progressive_textualist, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__progressive_textualist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__progressive_textualist, "Equality Clause Scope — Progressive Textualist Reading (Amendment-Gated Expansion)").
narrative_ontology:topic_domain(equality_clause_scope__progressive_textualist, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(equality_clause_scope__progressive_textualist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__progressive_textualist, '95520f47-8e78-4c2e-972d-667814720d44').
narrative_ontology:cs_kernel_codification('95520f47-8e78-4c2e-972d-667814720d44', fixed_text).
narrative_ontology:cs_authority_grounding('95520f47-8e78-4c2e-972d-667814720d44', lineage).
narrative_ontology:cs_interpretation_layer_present('95520f47-8e78-4c2e-972d-667814720d44').
narrative_ontology:cs_reading_relation('95520f47-8e78-4c2e-972d-667814720d44', equality_clause_scope__restrictive_originalist, influences).
narrative_ontology:cs_reading_relation('95520f47-8e78-4c2e-972d-667814720d44', equality_clause_scope__expansive_universalist, coexists_with).
narrative_ontology:cs_axiom('95520f47-8e78-4c2e-972d-667814720d44', foundational, scope_expansion_requires_ratified_supermajority_consent).
narrative_ontology:cs_axiom_status(scope_expansion_requires_ratified_supermajority_consent, holdable).
narrative_ontology:cs_axiom_grounding('95520f47-8e78-4c2e-972d-667814720d44', scope_expansion_requires_ratified_supermajority_consent, conventional).
narrative_ontology:cs_axiom('95520f47-8e78-4c2e-972d-667814720d44', foundational, judicial_reinterpretation_is_not_a_legitimate_scope_change_mechanism).
narrative_ontology:cs_axiom_status(judicial_reinterpretation_is_not_a_legitimate_scope_change_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('95520f47-8e78-4c2e-972d-667814720d44', judicial_reinterpretation_is_not_a_legitimate_scope_change_mechanism, conventional).
narrative_ontology:cs_reference_frame('95520f47-8e78-4c2e-972d-667814720d44', founding_text_with_amendment_expansion_mechanism).
narrative_ontology:cs_drift_state('95520f47-8e78-4c2e-972d-667814720d44', post_civil_rights_amendment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('95520f47-8e78-4c2e-972d-667814720d44', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__progressive_textualist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, enfranchised_amendment_era_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, constitutional_stability_interests).
narrative_ontology:constraint_beneficiary(equality_clause_scope__progressive_textualist, legislative_supermajority_coalitions).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, groups_excluded_pending_amendment).
narrative_ontology:constraint_victim(equality_clause_scope__progressive_textualist, minority_populations_below_amendment_threshold).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, textual_supremacy_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__progressive_textualist, democratic_ratification_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control whether and when the equality clause's application scope expands, by assembling the supermajorities the amendment process requires. They set the pace of inclusion and can withhold it indefinitely if consensus does not form; their consent is the sole valid mechanism for scope change under this reading.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, legislative_supermajority_coalitions, agenda_setter,
    institutional, generational, arbitrage, national).

% Live under a legal order that recognizes the equality principle textually but denies them its application until a supermajority chooses to extend it. They bear the cost of exclusion in the interval between recognizing the principle and its ratified extension, with no judicial avenue to accelerate it.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, groups_excluded_pending_amendment, payer,
    powerless, biographical, trapped, national).

% Too numerically or politically small to ever plausibly assemble the supermajority coalition needed to expand scope in their favor. For them, the amendment gate is not a slow but eventual door — it is functionally closed, since the population size required to clear the threshold may never materialize.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, minority_populations_below_amendment_threshold, payer,
    powerless, generational, trapped, national).

% Groups whose inclusion was achieved through successfully ratified amendments (e.g., prior extensions of suffrage or citizenship) retain the stability and legitimacy the amendment process confers — their inclusion cannot be revisited by a mere shift in judicial doctrine, insulating their gains from reinterpretation in either direction.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, enfranchised_amendment_era_groups, beneficiary,
    organized, generational, mobile, national).

% Institutions and actors invested in predictable, non-judicially-volatile constitutional meaning benefit from a rule that channels scope changes through the amendment process: rights are not created or destroyed by shifting court majorities, and the settled meaning of the text is protected from oscillation.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_stability_interests, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__progressive_textualist, constitutional_stability_interests, observer).

% Courts that might otherwise read the equality principle to extend coverage through interpretation are foreclosed from doing so under this reading — their interpretive authority over scope is structurally excluded from the legitimate-change mechanism, redirecting that power to the amendment process instead.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, judiciary_seeking_expansive_interpretation, excluded,
    institutional, generational, constrained, national).

% Study the amendment record to assess whether scope expansion has actually tracked evolving moral consensus or has instead lagged, been captured, or stalled entirely for populations unable to organize supermajority support.
narrative_ontology:constraint_stakeholder(equality_clause_scope__progressive_textualist, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, legitimacy-conferring mechanism for expanding who counts within the equality principle: because expansion requires supermajority ratification rather than a single court's reinterpretation, gains are durable and harder to reverse by subsequent doctrinal shifts, and inclusion carries the imprimatur of broad democratic consent rather than judicial fiat.
% TRANSFER_FUNCTION: Moves the power to define who receives equal treatment from the judiciary (which could act faster, case by case) to legislative/ratification supermajorities (who act slower, in bulk, and only when politically feasible) — transferring the pace and scope of inclusion from excluded populations' immediate claims to the coalition-building capacity of enfranchised political actors.
% ABSENT_VOICES: Populations too small, too dispersed, or too politically toxic to ever assemble a ratifying supermajority have no seat in the amendment process and no judicial shortcut under this reading; they would argue that a principle textually present but proceduraly unreachable is equality in name only for them.
% DISAPPEARANCE_RATIONALE: If this reading (amendment-gated expansion) were replaced by the expansive universalist reading, courts could immediately extend coverage without waiting for supermajority consent — a rapid rearrangement for currently excluded groups. If replaced by the restrictive originalist reading, current amendment-achieved gains might be recharacterized as illegitimate accretions. Whether disappearance rearranges the world or leaves it unchanged depends entirely on which sibling reading fills the vacuum, which is why the verdict is contested rather than settled.
% FOUNDING_PROBLEM: The founding text asserted an equality principle while operating under a social and political order that excluded most of the population from its practical application; the problem was reconciling the abstract textual commitment with the concrete scope of who the political community was prepared to treat as covered.
% FOUNDING_PROBLEM_CORROBORATION: Legislative historians and framers of successive amendments attest that the amendment process was deliberately chosen as the legitimate expansion mechanism to prevent judicial overreach from unsettling the constitutional order. Civil rights historians and excluded populations' advocates, from outside the amendment-coalition beneficiary set, attest that the founding problem — unequal practical coverage — remains substantially live for groups unable to clear the supermajority threshold, and that the amendment gate has in practice functioned to slow or block, rather than legitimately manage, further expansion.
narrative_ontology:disappearance_verdict(equality_clause_scope__progressive_textualist, contested).
narrative_ontology:founding_problem_status(equality_clause_scope__progressive_textualist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__progressive_textualist, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__progressive_textualist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__progressive_textualist, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__progressive_textualist_tests).
:- end_tests(equality_clause_scope__progressive_textualist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the reading's coordination function is genuine — it produces durable, hard-to-reverse expansions of coverage — but the mechanism also structurally excludes any group unable to organize supermajority political power, which is a real ongoing cost imposed disproportionately on the least organized populations. Suppression (0.48) captures the active exclusion of the judicial channel as a legitimate route to scope change: this is not passive neglect but an affirmative structural choice that forecloses one avenue of relief. Accessibility collapse (0.4) is moderate rather than high because the amendment door, while difficult, is not closed in principle — coalitions have in fact formed and expanded scope repeatedly across the interval. Resistance (0.55) is elevated because groups facing exclusion under this reading have historically organized, litigated (unsuccessfully, under this reading's terms), and pursued amendment campaigns, generating real friction against the gate.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislative supermajority coalitions sit at the agenda-setting position because they alone hold the power to trigger scope change; their exit option is best characterized as arbitrage since they can selectively extend or withhold coverage as political conditions allow. Groups currently excluded pending amendment, and especially minority populations below the amendment threshold, are structural targets: trapped, because no judicial shortcut exists under this reading and their own numbers may be permanently insufficient to clear the ratification bar. Enfranchised amendment-era groups are beneficiaries of the stability the mechanism confers on their own prior inclusion — their gains cannot be judicially unwound.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling an abstract equality principle with concrete political exclusion — has been partially but not fully resolved: successive amendments have narrowed the gap for many groups, which is genuine coordination function still live. But for populations structurally incapable of assembling supermajority coalitions, the founding problem remains as live as ever, while the mechanism's own legitimacy narrative ('the amendment process is working, be patient') increasingly performs resolution it has not delivered for those groups. This is precisely the tangled_rope signature: real coordination (durable, legitimate expansion for some) coexisting with real, asymmetric, actively-maintained exclusion (permanent non-coverage for others) through the same structural mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    amendment_gate_as_legitimate_filter_or_permanent_barrier,
    'Is the supermajority amendment requirement a legitimate democratic filter that will eventually extend coverage to all deserving groups, or a permanent structural barrier for populations too small or too politically disfavored to ever assemble the required coalition?',
    'Longitudinal analysis of amendment attempts by excluded groups: track proposed amendments that failed to reach ratification thresholds over multi-generational periods, and assess whether failure correlates with population size/political organization capacity versus with the substantive merits of the claim.',
    'If the gate correlates strongly with organizing capacity rather than merit, the reading functions closer to a snare for permanently under-resourced groups even while functioning as legitimate coordination for groups that can organize; if it does not so correlate, the tangled_rope classification is more fully deserved as temporary friction rather than permanent exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_gate_as_legitimate_filter_or_permanent_barrier, empirical, 'Whether the amendment threshold is a passable-if-slow filter or a durable barrier for structurally weak groups.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the progressive textualist reading the historically dominant interpretive tradition, or is it itself a compromise position adopted primarily because it avoids the political costs of both the originalist and universalist extremes?',
    'Doctrinal history review: trace which reading dominant courts and legislatures actually invoked at each amendment moment, and whether textualist framing was invoked strategically to legitimate outcomes reached for other reasons.',
    'If the reading is primarily a legitimating compromise rather than a genuinely held interpretive commitment, its claimed_type may overstate the coordination function relative to its use as cover for managed, gradual concession.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether this reading is a sincere interpretive tradition or a strategic compromise framing.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does adopting the progressive textualist reading logically foreclose the expansive universalist reading, or can both be held by different actors within the same broader constitutional culture without contradiction?',
    'Examine whether courts and legislatures operating under this reading have simultaneously entertained universalist arguments in dicta or minority opinions without formal contradiction — if so, coexistence is empirically supported over foreclosure.',
    'If foreclosure were correct, this reading would rule out judicial recognition of universal scope categorically; the coexists_with relation instead implies these remain live, competing positions across different institutional actors (courts vs. legislatures) simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Confirms the coexists_with relation to expansive_universalist rather than a stronger foreclosure claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__progressive_textualist, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1789, equality_clause_scope__progressive_textualist, theater_ratio, 1789, 0.15).
narrative_ontology:measurement_basis(equa_tr_t1789, observed).
narrative_ontology:measurement(equa_tr_t1868, equality_clause_scope__progressive_textualist, theater_ratio, 1868, 0.18).
narrative_ontology:measurement_basis(equa_tr_t1868, observed).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__progressive_textualist, theater_ratio, 1920, 0.22).
narrative_ontology:measurement_basis(equa_tr_t1920, observed).
narrative_ontology:measurement(equa_tr_t1965, equality_clause_scope__progressive_textualist, theater_ratio, 1965, 0.24).
narrative_ontology:measurement_basis(equa_tr_t1965, observed).
narrative_ontology:measurement(equa_tr_t1990, equality_clause_scope__progressive_textualist, theater_ratio, 1990, 0.26).
narrative_ontology:measurement_basis(equa_tr_t1990, observed).
narrative_ontology:measurement(equa_tr_t2024, equality_clause_scope__progressive_textualist, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(equa_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(equa_be_t1789, equality_clause_scope__progressive_textualist, base_extractiveness, 1789, 0.65).
narrative_ontology:measurement_basis(equa_be_t1789, observed).
narrative_ontology:measurement(equa_be_t1868, equality_clause_scope__progressive_textualist, base_extractiveness, 1868, 0.58).
narrative_ontology:measurement_basis(equa_be_t1868, observed).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__progressive_textualist, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement_basis(equa_be_t1920, observed).
narrative_ontology:measurement(equa_be_t1965, equality_clause_scope__progressive_textualist, base_extractiveness, 1965, 0.46).
narrative_ontology:measurement_basis(equa_be_t1965, observed).
narrative_ontology:measurement(equa_be_t1990, equality_clause_scope__progressive_textualist, base_extractiveness, 1990, 0.44).
narrative_ontology:measurement_basis(equa_be_t1990, observed).
narrative_ontology:measurement(equa_be_t2024, equality_clause_scope__progressive_textualist, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(equa_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1789, equality_clause_scope__progressive_textualist, suppression_requirement, 1789, 0.6).
narrative_ontology:measurement_basis(equa_su_t1789, observed).
narrative_ontology:measurement(equa_su_t1868, equality_clause_scope__progressive_textualist, suppression_requirement, 1868, 0.56).
narrative_ontology:measurement_basis(equa_su_t1868, observed).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__progressive_textualist, suppression_requirement, 1920, 0.52).
narrative_ontology:measurement_basis(equa_su_t1920, observed).
narrative_ontology:measurement(equa_su_t1965, equality_clause_scope__progressive_textualist, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement_basis(equa_su_t1965, observed).
narrative_ontology:measurement(equa_su_t1990, equality_clause_scope__progressive_textualist, suppression_requirement, 1990, 0.49).
narrative_ontology:measurement_basis(equa_su_t1990, observed).
narrative_ontology:measurement(equa_su_t2024, equality_clause_scope__progressive_textualist, suppression_requirement, 2024, 0.48).
narrative_ontology:measurement_basis(equa_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__progressive_textualist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__progressive_textualist, 0.12).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__progressive_textualist, equality_clause_scope__expansive_universalist).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the equality_clause_scope kernel, each generated as a separate ε-invariant constraint per the ε-invariance principle: restrictive_originalist (scope permanently fixed to founding social contract), progressive_textualist (this story — scope expands only via ratified amendment), and expansive_universalist (scope was always universal; judicial recognition corrects historical error rather than expanding coverage). The three differ in ε, beneficiary/victim structure, and classification because they instantiate structurally distinct claims about who counts and how that count may legitimately change, despite sharing surface language about 'the equality clause.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
