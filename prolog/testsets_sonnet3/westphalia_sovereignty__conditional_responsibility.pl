% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Responsibility to Protect (R2P) Conditional Sovereignty Doctrine
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the conditional_responsibility reading of the
 *   westphalia_sovereignty kernel: sovereignty is held to be a conditional
 *   grant contingent on a state's protection of its population, such that
 *   mass atrocity commission forfeits the state's claim to territorial
 *   inviolability and licenses external adjudication and intervention. This
 *   is one reading among three sharing the kernel — absolute_non_intervention
 *   treats sovereignty as categorical regardless of internal conduct, and
 *   graded_sovereignty treats territorial authority as a scalar function of
 *   state capacity. Those are separate constraints with their own ε and
 *   stakeholder structures; this story addresses only the
 *   conditional_responsibility reading's operation. Since the 2005 UN World
 *   Summit's endorsement of the Responsibility to Protect framework, the
 *   threshold for legitimate intervention has structurally lowered, and the
 *   record of application (Libya 2011 authorized, Syria repeatedly blocked,
 *   no action against permanent-member-aligned atrocity states) shows the
 *   promised universal threshold operating selectively in practice.
 *
 * KEY AGENTS:
 *   - un_security_council_permanent_members: agenda-setters who control invocation via veto
 *   - humanitarian_intervention_coalitions: beneficiaries who gain legal cover and strategic opportunity from authorized intervention
 *   - global_governance_institutions: beneficiaries whose adjudicative mandate and relevance expand under the doctrine
 *   - atrocity_affected_populations: primary payers, invoked as the doctrine's justification but voiceless in its application
 *   - targeted_state_governments: payers who lose their strongest legal defense once atrocity is declared
 *   - international_law_scholars: analytical observers documenting the selectivity gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.61).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.52).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.61).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Responsibility to Protect (R2P) Conditional Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, '8db555ec-df8b-4c35-bdb9-66c0a0c6a91d').
narrative_ontology:cs_kernel_codification('8db555ec-df8b-4c35-bdb9-66c0a0c6a91d', distributed).
narrative_ontology:cs_authority_grounding('8db555ec-df8b-4c35-bdb9-66c0a0c6a91d', distributed).
narrative_ontology:cs_reading_relation('8db555ec-df8b-4c35-bdb9-66c0a0c6a91d', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('8db555ec-df8b-4c35-bdb9-66c0a0c6a91d', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('8db555ec-df8b-4c35-bdb9-66c0a0c6a91d', foundational, sovereignty_conditioned_on_protection_performance).
narrative_ontology:cs_axiom_status(sovereignty_conditioned_on_protection_performance, holdable).
narrative_ontology:cs_axiom_grounding('8db555ec-df8b-4c35-bdb9-66c0a0c6a91d', sovereignty_conditioned_on_protection_performance, conventional).
narrative_ontology:cs_axiom('8db555ec-df8b-4c35-bdb9-66c0a0c6a91d', foundational, mass_atrocity_commission_forfeits_territorial_inviolability).
narrative_ontology:cs_axiom_status(mass_atrocity_commission_forfeits_territorial_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('8db555ec-df8b-4c35-bdb9-66c0a0c6a91d', mass_atrocity_commission_forfeits_territorial_inviolability, deontological).
narrative_ontology:cs_reference_frame('8db555ec-df8b-4c35-bdb9-66c0a0c6a91d', post_westphalian_absolute_sovereignty).
narrative_ontology:cs_drift_state('8db555ec-df8b-4c35-bdb9-66c0a0c6a91d', post_r2p_world_summit_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8db555ec-df8b-4c35-bdb9-66c0a0c6a91d', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, un_security_council_permanent_members).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, atrocity_affected_populations).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, targeted_state_governments).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, regional_states_bordering_intervention_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, atrocity_affected_populations).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, responsibility_to_protect_norm).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, sovereignty_as_responsibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold veto power over which atrocity situations trigger authorized intervention and which do not. Invoke the doctrine selectively — against rivals or weak states, rarely against each other or allies — and can block Council action entirely regardless of atrocity severity. Their own territorial inviolability is never structurally at risk under this reading.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, un_security_council_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Ad hoc coalitions of willing states and military alliances that conduct interventions once the responsibility-to-protect threshold is declared crossed. Gain legal cover, reputational capital, and sometimes strategic or resource access from intervention; bear military and political costs but retain full exit at any point.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, agenda_setter).

% UN bodies, international courts, and NGO monitoring apparatus gain expanded adjudicative jurisdiction, funding mandates, and institutional relevance from the conditional-sovereignty framework. Their authority to declare a state's sovereignty forfeited is itself the resource the doctrine creates and that they administer.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, beneficiary,
    institutional, civilizational, analytical, global).

% Populations under regimes committing or permitting mass atrocities. The doctrine is invoked in their name, but they have no vote in whether, when, or how intervention occurs, absorb the immediate violence of both the atrocity and any subsequent military intervention, and often experience prolonged instability, occupation, or abandonment when intervention is selectively withheld.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, atrocity_affected_populations, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, atrocity_affected_populations, beneficiary).

% Governments accused of atrocity crimes lose the presumption of territorial inviolability that other states retain. They can contest the atrocity finding through diplomacy or resistance, but the doctrine structurally strips their strongest legal defense — non-intervention — the moment the threshold is declared crossed by external actors they do not control.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, targeted_state_governments, payer,
    moderate, biographical, constrained, national).

% Neighboring states absorb refugee flows, cross-border instability, and secondary economic disruption from interventions they did not initiate and often were not consulted on. They have limited standing in the adjudicative process despite bearing durable regional costs.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, regional_states_bordering_intervention_zones, payer,
    moderate, biographical, constrained, regional).

% Populations suffering comparable or worse atrocities in states aligned with a permanent Security Council member are structurally excluded from the doctrine's protective invocation — the same facts that trigger intervention elsewhere produce no adjudicative action here. They would object to the selectivity but have no forum in which the comparison is formally heard.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, non_intervened_atrocity_states_allies, excluded,
    powerless, immediate, trapped, local).

% Study the doctrine's application record, documenting the gap between its universalist justification and its selective invocation pattern; their findings shape but do not control future application.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__conditional_responsibility, diffuse).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__conditional_responsibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared normative threshold — mass atrocity commission — past which the international community can coordinate collective response to gross human rights violations that would otherwise be shielded entirely by non-intervention norms, solving the genuine problem of sovereignty being weaponized as a shield for genocide, ethnic cleansing, and crimes against humanity.
% TRANSFER_FUNCTION: Moves adjudicative authority over a state's territorial integrity from that state's exclusive control to the international community's collective (in practice, Security Council permanent members' and intervening coalitions') determination; moves military, political, and reputational costs onto affected populations, targeted governments, and border states, while moving legal cover, institutional mandate, and strategic opportunity onto intervening coalitions and governance bodies.
% ABSENT_VOICES: Atrocity-affected populations themselves have no forum in the adjudicative process that authorizes intervention on their behalf; populations suffering comparable atrocities under permanent-member allies have no venue at all in which the selectivity of application can be formally contested.
% DISAPPEARANCE_RATIONALE: If conditional sovereignty were abandoned overnight in favor of absolute non-intervention, currently-authorized and threatened interventions would lose their legal predicate, atrocity-committing regimes would regain full diplomatic shielding, and the entire apparatus of R2P monitoring, early-warning institutions, and coalition-formation practice built since the early 2000s would lose its normative foundation — a substantial rearrangement of both institutional practice and the incentive structure facing atrocity-prone regimes.
% FOUNDING_PROBLEM: The Westphalian absolute-sovereignty norm allowed states to commit or tolerate genocide and mass atrocity within their borders with near-total legal immunity from external correction, most starkly demonstrated by the international community's paralysis during the Rwandan genocide and the Srebrenica massacre.
% FOUNDING_PROBLEM_CORROBORATION: UN member states and the 2005 World Summit outcome document affirm the founding problem as live and the doctrine as its necessary remedy. Independent scholars of selective application (citing Libya vs. Syria, and the absence of invocation against permanent-member allies) and affected-population advocacy groups outside the intervening coalitions attest that the doctrine's application record diverges sharply from its universalist justification, suggesting the operative function has partially shifted toward selective geopolitical leverage rather than uniform atrocity prevention.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.61) reflects that the doctrine's coordination function — preventing atrocity-shielding — is real but is layered with asymmetric extraction: the adjudicative authority it creates is exercised selectively by powerful actors against weaker ones, generating costs (military, political, reputational) borne disproportionately by populations and states with no seat in the adjudicative process. Suppression (0.52) is moderate: the doctrine does not physically prevent targeted states from contesting findings, but the practical suppression of alternative outcomes (once atrocity is declared, non-intervention as a legal defense collapses) is substantial. Theater ratio (0.42) and its rising trajectory reflect a growing gap between the doctrine's declared universal threshold and its increasingly visible selective application — invocation theater (statements, resolutions, monitoring reports) accumulating around cases where actual intervention never follows.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (permanent members, intervention coalitions), the doctrine reads as a genuine, hard-won correction to Westphalian paralysis — a Rope solving a real coordination failure. From the payer seat (targeted governments, and especially the excluded comparison class of populations under allied atrocity regimes), the same structure reads as a Tangled Rope at best: coordination function present, but riding alongside asymmetric extraction sustained by active enforcement (veto power, selective monitoring, selective coalition formation) that the doctrine's universalist language does not disclose.
 *
 * DIRECTIONALITY LOGIC:
 *   Security Council permanent members and intervention coalitions sit near the beneficiary end: they set or exploit the threshold, bear minimal structural risk to their own sovereignty, and gain institutional or strategic capital from selective invocation. Atrocity-affected populations and targeted governments sit near the target end: the former are trapped and voiceless despite being the doctrine's nominal beneficiaries, the latter face constrained exit once atrocity is declared. Regional border states occupy an intermediate position — moderate power, constrained exit, absorbing diffuse secondary costs without being party to the adjudication.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sovereignty as an absolute shield for atrocity — remains substantively live (atrocities continue; the normative gap that motivated R2P has not closed). But the mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges signals a distinct risk: the doctrine's application record diverges from its founding justification enough that its current operative function may have partially shifted toward providing selective geopolitical legitimation rather than uniform atrocity prevention. Classifying this as tangled_rope rather than snare or rope prevents two mislabeling errors: treating it as pure extraction would erase the genuine post-Rwanda coordination achievement; treating it as pure coordination (rope) would erase the documented selectivity and the absence of any adjudicative voice for the populations it claims to protect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selective_invocation_vs_universal_norm,
    'Is the conditional-responsibility doctrine a genuinely universal normative threshold that happens to be imperfectly applied, or is the universalist language itself cover for a discretionary tool wielded by powerful states against weaker ones?',
    'Comparative case analysis across all documented mass-atrocity situations since 2005, coding for (a) atrocity severity by independent measures, (b) permanent-member alignment of the accused state, and (c) whether Security Council action was taken; a strong correlation between (b) and (c) independent of (a) would support the discretionary-tool reading.',
    'If discretionary, effective extraction is higher than the base metric suggests and the doctrine''s classification moves closer to snare for the excluded comparison class; if genuinely constrained by evidentiary rather than political criteria, the tangled_rope classification with genuine coordination function is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_invocation_vs_universal_norm, empirical, 'Whether selective application reflects genuine evidentiary constraint or geopolitical discretion.').

omega_variable(
    kernel_reading_boundary_ambiguity,
    'Where exactly does the conditional_responsibility reading end and the graded_sovereignty reading begin, given that atrocity commission and state capacity deficits are empirically correlated (fragile states are both more atrocity-prone and more likely to be targets of graded-sovereignty framing)?',
    'Analysis of intervention justifications: conditional_responsibility framings should cite atrocity acts specifically; graded_sovereignty framings should cite capacity deficits (failed-state status) independent of specific atrocity findings. Cases citing both simultaneously indicate the readings are not cleanly separable in practice even though they are separable analytically.',
    'If real-world invocation routinely blends both readings, this suggests the kernel contest is not merely three parallel readings but a single contested practice that shifts rhetorical register opportunistically — a structural finding about the kernel itself rather than about any one reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_ambiguity, conceptual, 'Whether conditional_responsibility and graded_sovereignty are cleanly distinct in application or blend in practice.').

omega_variable(
    victim_voice_structural_absence,
    'Can the doctrine''s adjudicative process be reformed to include atrocity-affected populations as a party rather than merely as an invoked justification, and would doing so change the selectivity pattern?',
    'Track proposed reforms (e.g., expanded fact-finding mandates with affected-population testimony, ICC referral pathways) and whether their adoption correlates with reduced selectivity in subsequent Security Council action.',
    'If structural voice inclusion reduces selectivity, the current absence is a fixable design flaw compatible with tangled_rope; if selectivity persists regardless, the extraction is more likely intrinsic to the veto-gated enforcement structure and harder to remedy without Security Council reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_voice_structural_absence, empirical, 'Whether affected-population voicelessness is a fixable procedural gap or intrinsic to the enforcement architecture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__conditional_responsibility, theater_ratio, 0, 0.2).
narrative_ontology:measurement(west_tr_t6, westphalia_sovereignty__conditional_responsibility, theater_ratio, 6, 0.28).
narrative_ontology:measurement(west_tr_t12, westphalia_sovereignty__conditional_responsibility, theater_ratio, 12, 0.34).
narrative_ontology:measurement(west_tr_t18, westphalia_sovereignty__conditional_responsibility, theater_ratio, 18, 0.38).
narrative_ontology:measurement(west_tr_t24, westphalia_sovereignty__conditional_responsibility, theater_ratio, 24, 0.4).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__conditional_responsibility, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(west_be_t6, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(west_be_t12, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 12, 0.53).
narrative_ontology:measurement(west_be_t18, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 18, 0.57).
narrative_ontology:measurement(west_be_t24, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 30, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(west_su_t6, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(west_su_t12, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(west_su_t18, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 18, 0.47).
narrative_ontology:measurement(west_su_t24, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the colloquial 'sovereignty vs. intervention' debate under the westphalia_sovereignty kernel. absolute_non_intervention authors a Mountain-leaning or Rope-leaning reading treating non-intervention as near-categorical; graded_sovereignty authors a distinct reading with capacity-deficit thresholds and a different victim set (capacity-deficient states generally). Each reading has its own ε: this reading's ε (0.61) reflects genuine coordination function co-present with documented selective-application extraction; the absolute_non_intervention reading would author a different, likely lower ε reflecting minimal enforcement overhead but also foreclosing the atrocity-prevention coordination function entirely. Do not average or reconcile ε across siblings — each is a separate constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
