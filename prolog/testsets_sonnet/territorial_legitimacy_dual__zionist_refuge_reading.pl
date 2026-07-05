% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Zionist Refuge Legitimacy Reading of Israeli Territorial Sovereignty
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   The reading treats Israel's sovereignty as flowing from the convergence
 *   of three legitimating sources — the historical fact of persecution
 *   culminating in the Holocaust, a longstanding national/religious
 *   attachment to the land, and the international procedural instrument of
 *   the 1947 UN partition resolution. Structurally, this operates as a
 *   tangled rope: it genuinely coordinates the urgent problem of providing a
 *   persecuted, stateless population with sovereign refuge (real coordination
 *   function), while simultaneously requiring active legal and military
 *   enforcement (Law of Return asymmetries, military administration of the
 *   West Bank, blockade of Gaza) that extracts land, residency rights, and
 *   political voice from a population — Palestinians displaced in 1948 and
 *   administered since 1967 — who are external to the persecution narrative
 *   the arrangement was built to redress.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.62).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Legitimacy Reading of Israeli Territorial Sovereignty").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, '197d4f5b-5da0-4351-8c5c-06e4aed05594').
narrative_ontology:cs_kernel_codification('197d4f5b-5da0-4351-8c5c-06e4aed05594', distributed).
narrative_ontology:cs_authority_grounding('197d4f5b-5da0-4351-8c5c-06e4aed05594', lineage).
narrative_ontology:cs_interpretation_layer_present('197d4f5b-5da0-4351-8c5c-06e4aed05594').
narrative_ontology:cs_reading_relation('197d4f5b-5da0-4351-8c5c-06e4aed05594', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('197d4f5b-5da0-4351-8c5c-06e4aed05594', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('197d4f5b-5da0-4351-8c5c-06e4aed05594', foundational, historical_persecution_grounds_sovereign_refuge_claim).
narrative_ontology:cs_axiom_status(historical_persecution_grounds_sovereign_refuge_claim, holdable).
narrative_ontology:cs_axiom_grounding('197d4f5b-5da0-4351-8c5c-06e4aed05594', historical_persecution_grounds_sovereign_refuge_claim, deontological).
narrative_ontology:cs_axiom('197d4f5b-5da0-4351-8c5c-06e4aed05594', foundational, un_partition_acceptance_confers_uncontested_1948_legitimacy).
narrative_ontology:cs_axiom_status(un_partition_acceptance_confers_uncontested_1948_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('197d4f5b-5da0-4351-8c5c-06e4aed05594', un_partition_acceptance_confers_uncontested_1948_legitimacy, conventional).
narrative_ontology:cs_axiom('197d4f5b-5da0-4351-8c5c-06e4aed05594', secondary, displacement_attributable_to_arab_rejection_not_founding_act).
narrative_ontology:cs_axiom_status(displacement_attributable_to_arab_rejection_not_founding_act, holdable).
narrative_ontology:cs_axiom_grounding('197d4f5b-5da0-4351-8c5c-06e4aed05594', displacement_attributable_to_arab_rejection_not_founding_act, empirically_contingent).
narrative_ontology:cs_reference_frame('197d4f5b-5da0-4351-8c5c-06e4aed05594', persecution_refuge_partition_legitimacy).
narrative_ontology:cs_drift_state('197d4f5b-5da0-4351-8c5c-06e4aed05594', post_1967_occupation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('197d4f5b-5da0-4351-8c5c-06e4aed05594', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_jewish_citizens).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, diaspora_zionist_organizations).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_occupied_territories).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_israeli_citizens).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_israeli_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers citizenship law (Law of Return), settlement policy, and military control over territory acquired in 1948 and 1967, justifying the arrangement by reference to historical persecution, the UN partition vote, and continuous security threat. Sets the terms under which land, residency, and citizenship are allocated.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Hold automatic citizenship and land access under a legal and narrative framework that treats their presence as a return from historical exile rather than a contested settlement. Their security, property, and civic rights are structured around the legitimacy claim; most face no barrier to remaining or leaving.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_jewish_citizens, beneficiary,
    organized, generational, mobile, national).

% Fund settlement, lobbying, and narrative-maintenance activity abroad, deriving continued relevance and resources from the persistence of the refuge/return framing. Not resident in the territory and bear none of the territorial costs directly.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, diaspora_zionist_organizations, beneficiary,
    organized, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, diaspora_zionist_organizations, agenda_setter).

% Displaced during and after the 1948 war; barred by Israeli law from return or property restitution. Under this reading, their displacement is framed as a consequence of Arab state rejection of the UN partition plan rather than a direct product of the founding legitimacy claim, foreclosing a right-of-return remedy.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, regional).

% Live under military administration or blockade in the West Bank and Gaza, territory held since 1967 and treated under this reading as negotiable but currently retained on security grounds. Movement, building, and political rights are constrained by the same security rationale that legitimizes continued control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_occupied_territories, payer,
    powerless, biographical, trapped, regional).

% Hold Israeli citizenship and vote, but exist inside a state whose foundational legitimacy narrative (persecution, divine promise, partition) does not include their own historical claim to the land, producing structurally unequal land-allocation and planning regimes even where formal civic rights are extended.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_israeli_citizens, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_israeli_citizens, beneficiary).

% The 1947 UN General Assembly Resolution 181 is invoked as the procedural source of legitimacy for the 1948 state but was not accepted by the surrounding Arab states or Palestinian leadership at the time, and its territorial lines were superseded by the 1948-49 armistice lines before any partition state was implemented.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, un_partition_framework, observer,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(territorial_legitimacy_dual__zionist_refuge_reading, un_partition_framework).

% States and international bodies that would press a competing legitimacy account (occupation law, right of return, self-determination) are structurally outside the domestic legal and narrative apparatus that adjudicates this reading; their resolutions carry no binding enforcement inside the territory.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_diplomatic_community, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__zionist_refuge_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__zionist_refuge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, internally coherent legal and narrative basis for a sovereign state to exist as a refuge for a historically persecuted diaspora population, coordinating citizenship, immigration (Law of Return), and defense policy around a single legitimating story.
% TRANSFER_FUNCTION: Land, residency rights, and political voice are allocated according to the persecution/promise/partition narrative; this systematically channels property, citizenship security, and territorial control toward Jewish Israeli citizens and away from Palestinian claimants whose historical presence and 1948/1967 displacement fall outside the narrative's frame.
% ABSENT_VOICES: Palestinian refugees excluded from return, and Palestinian residents of the occupied territories who never held a vote in the state administering them, would contest the framing of displacement as a consequence of Arab rejection rather than of the founding and subsequent territorial actions; they are not parties to the domestic legal processes that maintain this reading.
% DISAPPEARANCE_RATIONALE: If this specific legitimacy reading were abandoned by the Israeli state and its institutions, citizenship law, settlement policy, and diplomatic posture on 1967 territory would need to be renegotiated from a different premise; existing land allocations, military administration structures, and international recognition frameworks are built on this narrative's specific claims (uncontested 1948, negotiable 1967, security-justified control).
% FOUNDING_PROBLEM: Coordinate an urgent political and physical refuge for a population subject to genocide and centuries of persecution, using a combination of historical claim, religious/national narrative, and the available 1947 international legal instrument (UN partition) to establish sovereign statehood.
% FOUNDING_PROBLEM_CORROBORATION: Israeli state historians and much of the Jewish Israeli public attest the founding problem (persecution, statelessness) remains structurally live given continued antisemitism and regional hostility. Independent international law scholars, UN human rights bodies, and Palestinian historiography attest that whatever the founding problem's validity, its continued invocation now functions primarily to legitimate open-ended territorial control over a second population that had no role in the persecution the narrative addresses — a status-shift documented outside either benefiting party by UN Special Rapporteur reports and independent legal scholarship.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects substantial but not maximal transfer: much of the reading's coordination function (refuge for a persecuted population) is genuine, but the same legal architecture systematically channels land and rights away from Palestinian claimants. Suppression (0.62) is high because maintaining the specific 1948-uncontested/1967-negotiable/displacement-as-Arab-rejection framing requires active legal enforcement (citizenship law, military administration, settlement expansion) against a resistant party, not merely passive acceptance. Theater ratio is moderate-low (0.30): most enforcement machinery serves a real function (security, immigration administration) rather than pure performance, though its share has grown as the security rationale has been extended to territories beyond the original 1948 boundaries. Accessibility collapse (0.45) is moderate — alternative legitimacy framings (autochthony, coexistence) remain visible and actively contested internationally, they have not collapsed. Resistance (0.75) is high, reflecting sustained Palestinian, regional, and international contestation of this specific reading's claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli state institutions and Jewish Israeli citizens sit near the beneficiary end: the legitimacy narrative directly underwrites their citizenship security, land access, and political standing, and their exit options (mobile, arbitrage for diaspora organizations) are wide. Palestinian refugees and residents of the occupied territories sit near the full-target end: trapped exit options, no citizenship remedy, and the specific reading's framing of their displacement as attributable to Arab-state rejection forecloses a right-of-return claim that would otherwise attach directly to the founding events. Palestinian Israeli citizens occupy a hybrid position — formal citizenship benefits coexist with land-planning and narrative exclusion, captured via the dual role and moderate power designation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (urgent refuge from persecution) was genuinely live in 1948 and remains partially live given ongoing antisemitism, which is why founding_problem_status is authored as contested rather than dead — this is not a pure zombie mandate. However, the specific application of the security rationale to territory acquired in 1967, and to a population (Palestinians) who bore no responsibility for the persecution the mandate was built to address, is where mandatrophy risk concentrates: the founding justification is being asked to do more extractive work (indefinite territorial control) than its original coordination problem (statehood for refuge) requires. The tangled_rope classification — rather than mountain or pure rope — is intended to hold both facts simultaneously without collapsing one into the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition_status,
    'Is the 1947 UN partition resolution properly read as conferring settled international legal legitimacy on 1948 sovereignty (as this reading holds), or as one rejected, non-implemented proposal whose lines were immediately superseded by war (as the palestinian_autochthony_reading and much international law scholarship holds)?',
    'This is a committer-level disagreement about how to read a single historical-legal kernel (territorial_legitimacy_dual); it is not resolvable by additional data but is instead the specific point of divergence between this reading and its siblings, documented per the kernel/reading framework rather than adjudicated within this file.',
    'If the sibling readings'' account of the partition''s non-acceptance is treated as controlling, the ''1948 legitimacy uncontested'' premise of this reading loses its procedural anchor, which would shift this constraint''s classification toward higher extractiveness and lower legitimacy-grounding stability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_partition_status, conceptual, 'Which reading of the 1947 UN partition resolution''s legal status is authoritative — the central committer disagreement of the kernel.').

omega_variable(
    security_rationale_scope_creep,
    'Does the security justification for continued control of 1967 territories represent a stable, bounded response to genuine external threat, or has it expanded over time to cover settlement activity and permanent administration beyond what security alone would require?',
    'Comparative analysis of settlement expansion patterns against documented security incidents and international legal assessments (e.g., ICJ advisory opinions, UN Human Rights Council reporting) over the 1967-2024 interval.',
    'If security has been used to justify territorial expansion beyond defensive need, the ''security concerns justify territorial control'' premise central to this reading is weakened, and the constraint''s effective extractiveness is higher than a pure-security framing would suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_rationale_scope_creep, empirical, 'Whether the security rationale has expanded beyond its original defensive scope over the measured interval.').

omega_variable(
    displacement_causal_attribution,
    'Is Palestinian displacement in 1948 more accurately attributed primarily to Arab state and Palestinian leadership rejection of partition (this reading''s framing) or to a combination of that rejection AND direct expulsion/flight-inducing military actions documented in Israeli and international historical archives (the New Historians scholarship)?',
    'Comparative historiographical review of declassified Israeli military archives, UN relief agency records, and eyewitness testimony from 1948, weighed against the extent to which Arab rejection of partition independently explains the scale and permanence of displacement.',
    'If direct military action is found to be a substantial independent cause rather than a secondary consequence of Arab rejection, this reading''s causal attribution significantly understates the founding arrangement''s own role in producing the victim population it declares external to the legitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_causal_attribution, empirical, 'Whether 1948 displacement is fully explained by Arab rejection of partition or partly caused by direct military/expulsion action.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1967, 0.18).
narrative_ontology:measurement(terr_tr_t1980, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(terr_tr_t1995, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement(terr_be_t1980, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(terr_be_t1995, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2010, 0.56).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1948, 0.4).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(terr_su_t1980, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(terr_su_t1995, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__zionist_refuge_reading, 0.1).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language concept 'the legitimacy of Israeli/Palestinian territorial claims' (kernel: territorial_legitimacy_dual). Each reading is authored as a separate constraint with its own ε, beneficiaries/victims, and classification: zionist_refuge_reading (this file, tangled_rope), palestinian_autochthony_reading (sibling, not authored here), and two_state_coexistence_reading (sibling, not authored here). The readings are linked via affects_constraints and via cs_structure.reading_relations rather than merged into a single averaged constraint, per the ε-invariance principle — averaging across readings with different victim sets would itself misrepresent all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
