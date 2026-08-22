% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__absolute_non_intervention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__absolute_non_intervention, []).

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
 *   constraint_id: westphalia_sovereignty__absolute_non_intervention
 *   human_readable: Absolute Non-Intervention Reading of Westphalian Sovereignty
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   This story authors the absolute non-intervention reading of the
 *   Westphalian sovereignty kernel: the claim that territorial inviolability
 *   is categorical, such that internal conduct — however severe — cannot
 *   legitimate external interference absent the state's own consent or a
 *   Security Council authorization that any permanent member can block. This
 *   is one of three structurally distinct readings of the same kernel text
 *   (UN Charter Art. 2(4), 2(7); customary Westphalian practice). The sibling
 *   readings — conditional_responsibility (sovereignty forfeited upon failure
 *   to protect) and graded_sovereignty (sovereignty as scalar capacity) — are
 *   separate constraints with their own ε values and stakeholder sets, not
 *   alternative measurements of this one. This reading's ε is assessed by its
 *   own lights: the standing arrangement under contest is the absolute-bar
 *   practice as it actually operates in Security Council diplomacy and state
 *   conduct, not the intervention regime an R2P advocate would replace it
 *   with.
 *
 * KEY AGENTS:
 *   - authoritarian_state_elites: primary beneficiary and agenda_setter — invoke the categorical bar to shield internal conduct
 *   - permanent_security_council_members: agenda_setter/beneficiary — administer the enforcement mechanism (veto) and invoke the norm selectively
 *   - military_and_security_apparatus_leadership: beneficiary, identity-locked — institutional survival depends on the shield
 *   - populations_under_authoritarian_control: primary target, trapped — bear the excluded harm this reading forecloses from remedy
 *   - ethnic_and_religious_minorities_facing_state_violence: concentrated target of the worst-case harm the categorical reading permits
 *   - humanitarian_intervention_advocates: excluded voice — argue against the categorical reading but cannot compel Security Council action
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__absolute_non_intervention, 0.72).
domain_priors:theater_ratio(westphalia_sovereignty__absolute_non_intervention, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalia_sovereignty__absolute_non_intervention, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__absolute_non_intervention, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__absolute_non_intervention, "Absolute Non-Intervention Reading of Westphalian Sovereignty").
narrative_ontology:topic_domain(westphalia_sovereignty__absolute_non_intervention, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__absolute_non_intervention).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__absolute_non_intervention, '78b15a0a-24a9-4cb6-bb40-6b781bd56478').
narrative_ontology:cs_kernel_codification('78b15a0a-24a9-4cb6-bb40-6b781bd56478', formalized).
narrative_ontology:cs_authority_grounding('78b15a0a-24a9-4cb6-bb40-6b781bd56478', extraction).
narrative_ontology:cs_interpretation_layer_present('78b15a0a-24a9-4cb6-bb40-6b781bd56478').
narrative_ontology:cs_reading_relation('78b15a0a-24a9-4cb6-bb40-6b781bd56478', westphalia_sovereignty__conditional_responsibility, forecloses).
narrative_ontology:cs_reading_relation('78b15a0a-24a9-4cb6-bb40-6b781bd56478', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('78b15a0a-24a9-4cb6-bb40-6b781bd56478', foundational, territorial_inviolability_is_categorical).
narrative_ontology:cs_axiom_status(territorial_inviolability_is_categorical, holdable).
narrative_ontology:cs_axiom_grounding('78b15a0a-24a9-4cb6-bb40-6b781bd56478', territorial_inviolability_is_categorical, conventional).
narrative_ontology:cs_axiom('78b15a0a-24a9-4cb6-bb40-6b781bd56478', foundational, internal_conduct_is_legally_irrelevant_to_intervention_legitimacy).
narrative_ontology:cs_axiom_status(internal_conduct_is_legally_irrelevant_to_intervention_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('78b15a0a-24a9-4cb6-bb40-6b781bd56478', internal_conduct_is_legally_irrelevant_to_intervention_legitimacy, conventional).
narrative_ontology:cs_reference_frame('78b15a0a-24a9-4cb6-bb40-6b781bd56478', westphalian_territorial_exclusivity).
narrative_ontology:cs_drift_state('78b15a0a-24a9-4cb6-bb40-6b781bd56478', post_r2p_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('78b15a0a-24a9-4cb6-bb40-6b781bd56478', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, permanent_security_council_members).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__absolute_non_intervention, military_and_security_apparatus_leadership).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, ethnic_and_religious_minorities_facing_state_violence).
narrative_ontology:constraint_victim(westphalia_sovereignty__absolute_non_intervention, diaspora_and_refugee_communities).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, territorial_integrity_norm).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__absolute_non_intervention, non_intervention_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern a territory and invoke the non-intervention norm to bar external scrutiny or action against internal repression, resource extraction, or violence against domestic populations. They actively cite the UN Charter's Article 2(4)/2(7) framing in diplomatic forums to block resolutions, sanctions, or intervention efforts, and face essentially no structural cost for internal conduct as long as they maintain the fiction of domestic jurisdiction.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites, agenda_setter).

% Hold veto power over Security Council authorization of intervention and invoke sovereignty norms selectively — defending it rigorously when their own conduct or that of allies is at issue, and permitting exceptions when it suits their strategic interests. They administer the very mechanism (Charter-based collective security) that the norm depends on for enforcement.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, permanent_security_council_members, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, permanent_security_council_members, beneficiary).

% Benefit directly from the norm's shielding of internal security operations from outside accountability; their institutional identity and career survival are bound to maintaining the internal-affairs framing, since loss of that shield would expose command responsibility for internal violence.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, military_and_security_apparatus_leadership, beneficiary,
    organized, biographical, identity_locked, national).

% Live under the governing regime's authority with no external recourse recognized as legitimate under this reading; atrocities, repression, and denial of rights are classified as internal matters, foreclosing UN action, foreign intervention, or binding external remedy regardless of severity. Exit is typically only through flight, which the norm does not address.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, populations_under_authoritarian_control, payer,
    powerless, immediate, trapped, local).

% Bear concentrated harm from state violence campaigns (mass detention, ethnic cleansing, genocide) that this reading of sovereignty classifies as beyond the legitimate reach of outside actors absent Security Council authorization, which the perpetrating or allied states can block.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, ethnic_and_religious_minorities_facing_state_violence, payer,
    powerless, immediate, trapped, local).

% Have fled the territory but carry ongoing costs (family exposure, statelessness, inability to return, blocked accountability processes) generated by the norm's protection of the originating regime; they lodge appeals in international forums but have no standing to trigger intervention under this reading.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, diaspora_and_refugee_communities, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__absolute_non_intervention, diaspora_and_refugee_communities, excluded).

% NGOs, human rights bodies, and coalitions of smaller states that argue mass atrocity should trigger external responsibility. They are structurally excluded from binding decision-making because Security Council authorization gates any lawful intervention and permanent members with sovereignty-protective interests can veto action.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, humanitarian_intervention_advocates, excluded,
    organized, biographical, constrained, global).

% Study the doctrinal history and practice of the non-intervention principle, document its selective invocation, and produce competing normative frameworks (including R2P) without holding enforcement power themselves.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__absolute_non_intervention, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__absolute_non_intervention, authoritarian_state_elites).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__absolute_non_intervention, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, bright-line rule preventing pretextual invasions and great-power adventurism by categorically barring external interference in internal affairs — a genuine coordination good against the historical pattern of wars of religion and dynastic intervention that Westphalia was built to end.
% TRANSFER_FUNCTION: Moves the cost of internal state violence from the international community (which bears no formal intervention obligation) onto the populations subject to that violence, while moving the benefit of unaccountable domestic authority to the governing elite and their security apparatus.
% ABSENT_VOICES: Populations experiencing mass atrocity have no standing under this reading to trigger review; humanitarian intervention advocates and affected diaspora communities raise the issue in international forums but cannot compel Security Council action when a permanent member shields the state in question.
% DISAPPEARANCE_RATIONALE: If the absolute non-intervention norm vanished overnight, the entire architecture of Charter-based collective security, non-aligned bloc diplomacy, and postcolonial territorial settlement would need renegotiation; authoritarian regimes would lose their primary legal shield against external action, and the great-power veto system would lose much of its present function.
% FOUNDING_PROBLEM: The 1648 Peace of Westphalia was built to end a century of devastating religiously-motivated intervention across European territories by establishing that rulers, not external religious or dynastic authorities, would control internal religious and political arrangements within their own borders.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars and R2P advocates attest the founding problem (preventing pretextual external war) has been substantially solved by other post-1945 mechanisms (UN Charter force prohibition, human rights treaty regimes) and that the absolute reading now primarily functions to shield internal atrocity; authoritarian state elites and several permanent Security Council members attest the founding problem remains live, citing ongoing risk of great-power intervention pretexts (e.g., contested 'humanitarian' invasions). No corroboration from outside the state-elite beneficiary set affirms that atrocity-shielding was ever the intended function — that effect is documented by scholars and victim testimony, not defended as founding purpose by anyone.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__absolute_non_intervention, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__absolute_non_intervention, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__absolute_non_intervention, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__absolute_non_intervention, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__absolute_non_intervention, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__absolute_non_intervention_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__absolute_non_intervention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__absolute_non_intervention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects that the categorical bar transfers real, severe cost — foreclosure of remedy for mass atrocity — onto trapped populations while state elites and the permanent-member enforcement structure retain full benefit of the shield; this has risen over the interval as the gap between the norm's founding rationale (preventing pretextual war) and its operative use (shielding internal atrocity, esp. post-1994 Rwanda, ongoing in Syria/Myanmar-type cases) widened. Suppression (0.72) captures that maintaining the categorical reading requires active diplomatic and veto-based enforcement — it is not self-sustaining; every attempted Security Council resolution invoking humanitarian grounds tests and reinforces the bar. Theater ratio (0.40) reflects that a substantial share of Security Council sovereignty-invocation is now performative — states citing the norm while quietly tolerating or conducting selective interventions elsewhere, producing an inconsistency the T17 trigger would flag given the rising extractiveness trend. Accessibility_collapse (0.60) and resistance (0.58) are mid-range because unlike a mountain, workable alternative doctrines (R2P, conditional sovereignty) are visible and actively argued — the categorical reading has not foreclosed the conceptual space, only the binding legal remedy.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian state elites and the security apparatus sit near the full-beneficiary end of directionality: the constraint subsidizes their unaccountable control and their exit options (arbitrage-grade diplomatic maneuvering, identity-locked institutional survival) confirm this. Populations under authoritarian control and targeted minorities sit at the full-target end: trapped exit, immediate time horizon, powerless — the categorical bar directly produces their foreclosed remedy. Permanent Security Council members are structurally ambiguous and treated as beneficiary/agenda_setter jointly — they administer the very veto mechanism that operationalizes the categorical bar, and while they are not always the perpetrating state, their own conduct receives the identical shield, which is why the norm persists through their maintenance rather than despite it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's founding problem (preventing pretextual religious/dynastic war) is genuinely dead in its original form — the Charter's own force prohibition and modern treaty-based human rights regimes now perform much of that original coordination function through other mechanisms. What persists under this reading is not the founding coordination good but a residual absolute-bar interpretation that primarily shields internal atrocity from remedy. The tangled_rope classification (rather than snare) is deliberate: the norm still performs a genuine, non-trivial coordination function — preventing great-power pretextual invasion remains a live risk, and removing the bar entirely would expose weaker states to intervention on manufactured humanitarian grounds. Both the coordination function and the asymmetric extraction are real and simultaneous, which is exactly the tangled_rope signature; classifying it as pure snare would erase the genuine war-prevention value it still provides to weaker, non-atrocity-committing states, while classifying it as pure rope would erase the documented, severe cost borne by trapped populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_purpose_vs_operative_function,
    'Is the categorical non-intervention norm still functioning as genuine war-prevention coordination, or has it been substantially captured as an atrocity shield with coordination retained only as legitimating cover?',
    'Comparative case analysis of Security Council invocations of the norm across decades: track ratio of invocations blocking genuine pretextual-invasion attempts versus invocations blocking documented-atrocity remedy attempts.',
    'If the atrocity-shielding function dominates the war-prevention function in modern invocation patterns, the classification should trend toward snare; if war-prevention remains the dominant invoked use, tangled_rope is the more accurate reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_purpose_vs_operative_function, empirical, 'Whether coordination or extraction now dominates the norm''s operative use.').

omega_variable(
    sovereignty_natural_vs_constructed,
    'Is categorical territorial inviolability a discovered feature of a stable international order, or a constructed legal convention that happens to benefit incumbent state elites and permanent Council members?',
    'Historical analysis of pre-Westphalian intervention norms and post-1945 treaty negotiation records to establish whether the categorical framing was a deliberate choice among alternatives (it was, per the historical record of Charter drafting debates) rather than an emergent natural constraint.',
    'Confirms the constraint is constructed rather than natural, supporting the tangled_rope/mandatrophy analysis over any Mountain framing that might otherwise be claimed for ''sovereignty'' as an inevitable feature of state systems.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_natural_vs_constructed, conceptual, 'Whether sovereignty-as-categorical is natural law or negotiated convention.').

omega_variable(
    reading_framing_underdetermination,
    'Does the choice to frame this constraint around the Security Council veto mechanism (institutional framing) versus around the customary international law doctrine of non-intervention (normative-text framing) change the classification?',
    'Author a parallel analysis treating the doctrine-as-text (rather than the veto-as-enforcement-mechanism) as the kernel object and compare cs_pattern outputs.',
    'The institutional framing (chosen here) foregrounds permanent-member agenda-setting power and produces tangled_rope with strong enforcement dependency; a pure-doctrine framing might understate the enforcement asymmetry and trend toward a rope classification by treating the norm as self-executing custom rather than veto-administered practice. This story adopts the institutional framing because the veto is the actual mechanism by which the categorical bar is operationalized in contested cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Alternative framing (doctrine-as-text vs institution-as-enforcer) could shift the computed classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__absolute_non_intervention, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1945, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(west_tr_t1960, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1960, 0.28).
narrative_ontology:measurement(west_tr_t1975, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1975, 0.32).
narrative_ontology:measurement(west_tr_t1994, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 1994, 0.3).
narrative_ontology:measurement(west_tr_t2005, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(west_tr_t2015, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(west_tr_t2025, westphalia_sovereignty__absolute_non_intervention, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1945, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1945, 0.45).
narrative_ontology:measurement(west_be_t1960, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(west_be_t1975, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1975, 0.55).
narrative_ontology:measurement(west_be_t1994, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 1994, 0.62).
narrative_ontology:measurement(west_be_t2005, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(west_be_t2015, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(west_be_t2025, westphalia_sovereignty__absolute_non_intervention, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1945, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1945, 0.55).
narrative_ontology:measurement(west_su_t1960, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(west_su_t1975, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(west_su_t1994, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 1994, 0.7).
narrative_ontology:measurement(west_su_t2005, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(west_su_t2015, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2015, 0.71).
narrative_ontology:measurement(west_su_t2025, westphalia_sovereignty__absolute_non_intervention, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__absolute_non_intervention, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, westphalia_sovereignty__graded_sovereignty).
narrative_ontology:affects_constraint(westphalia_sovereignty__absolute_non_intervention, un_security_council_veto_power).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the colloquial 'Westphalian sovereignty' concept per the ε-invariance principle: absolute_non_intervention (this story, tangled_rope, ε=0.68), conditional_responsibility (sovereignty as forfeitable on atrocity, expected lower ε and different victim exclusion), and graded_sovereignty (scalar capacity framing, expected intermediate structure). Each reading has its own stable ε and stakeholder set; none is a measurement of the others under a different observable. The un_security_council_veto_power constraint is the shared enforcement infrastructure all three readings contest control over.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__absolute_non_intervention, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
