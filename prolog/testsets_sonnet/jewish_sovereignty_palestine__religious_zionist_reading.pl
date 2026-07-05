% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine Covenant Title to Eretz Yisrael (Religious Zionist Reading)
 *   domain: political_philosophy/religious_nationalism/postcolonial_theory
 *
 * SUMMARY:
 *   This story generates ONLY the religious Zionist reading of the
 *   jewish_sovereignty_palestine kernel: the claim that divine promise of
 *   Eretz Yisrael to the Jewish people grounds an inalienable territorial
 *   title, such that statehood is theological fulfillment rather than a
 *   negotiated political settlement. Under this reading, the land itself is
 *   non-negotiable — partition proposals, international legal determinations,
 *   and Palestinian claims to the same territory are not treated as competing
 *   rights to be balanced but as obstacles to a prior title already settled
 *   by divine grant. This is a structurally distinct constraint from the
 *   liberal_nationalist_reading (self-determination right,
 *   partition-compatible), the settler_colonial_reading (displacement regime
 *   independent of intent), the cultural_zionist_reading (spiritual center
 *   without political maximalism), and the post_zionist_reading (achieved
 *   statehood whose founding narrative now obstructs civic equality). Each of
 *   those is a separate constraint story with its own ε and stakeholder
 *   structure, linked here via network.affects_constraints per the
 *   ε-invariance principle — this file does not average across them or import
 *   their premises.
 *
 * KEY AGENTS:
 *   - religious_zionist_settler_movement: primary agenda-setter and beneficiary, identity-locked to the claim
 *   - palestinian_residents_of_west_bank: primary target, trapped exit, land and mobility extracted
 *   - palestinian_refugees: primary target of the non-negotiability clause specifically (return foreclosed by divine title)
 *   - state_apparatus_administering_settlement: institutional agenda-setter that enforces and absorbs diplomatic cost
 *   - religious_zionist_theologians_and_rabbinic_authorities: analytical/agenda-setting authority that interprets and transmits the claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.86).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.8).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine Covenant Title to Eretz Yisrael (Religious Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political_philosophy/religious_nationalism/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, '55f97f6a-117b-4e3f-b635-de92265ee35f').
narrative_ontology:cs_kernel_codification('55f97f6a-117b-4e3f-b635-de92265ee35f', distributed).
narrative_ontology:cs_authority_grounding('55f97f6a-117b-4e3f-b635-de92265ee35f', lineage).
narrative_ontology:cs_interpretation_layer_present('55f97f6a-117b-4e3f-b635-de92265ee35f').
narrative_ontology:cs_reading_relation('55f97f6a-117b-4e3f-b635-de92265ee35f', jewish_sovereignty_palestine__liberal_nationalist_reading, forecloses).
narrative_ontology:cs_reading_relation('55f97f6a-117b-4e3f-b635-de92265ee35f', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('55f97f6a-117b-4e3f-b635-de92265ee35f', jewish_sovereignty_palestine__cultural_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('55f97f6a-117b-4e3f-b635-de92265ee35f', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('55f97f6a-117b-4e3f-b635-de92265ee35f', foundational, divine_grant_confers_inalienable_title).
narrative_ontology:cs_axiom_status(divine_grant_confers_inalienable_title, holdable).
narrative_ontology:cs_axiom_grounding('55f97f6a-117b-4e3f-b635-de92265ee35f', divine_grant_confers_inalienable_title, theological).
narrative_ontology:cs_axiom('55f97f6a-117b-4e3f-b635-de92265ee35f', foundational, territorial_maximalism_is_theological_obligation_not_policy_choice).
narrative_ontology:cs_axiom_status(territorial_maximalism_is_theological_obligation_not_policy_choice, holdable).
narrative_ontology:cs_axiom_grounding('55f97f6a-117b-4e3f-b635-de92265ee35f', territorial_maximalism_is_theological_obligation_not_policy_choice, theological).
narrative_ontology:cs_axiom('55f97f6a-117b-4e3f-b635-de92265ee35f', secondary, partition_constitutes_covenant_breach).
narrative_ontology:cs_axiom_status(partition_constitutes_covenant_breach, holdable).
narrative_ontology:cs_axiom_grounding('55f97f6a-117b-4e3f-b635-de92265ee35f', partition_constitutes_covenant_breach, theological).
narrative_ontology:cs_reference_frame('55f97f6a-117b-4e3f-b635-de92265ee35f', biblical_covenant_grant_of_eretz_yisrael).
narrative_ontology:cs_drift_state('55f97f6a-117b-4e3f-b635-de92265ee35f', post_1967_settlement_era, gap(revival_pressure, severe, true)).
narrative_ontology:cs_created_at('55f97f6a-117b-4e3f-b635-de92265ee35f', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_settler_movement).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, covenant_community_jewish_people).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, state_apparatus_administering_settlement).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_residents_of_west_bank).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugees).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, secular_and_liberal_israeli_jews_bearing_diplomatic_and_security_costs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, secular_and_liberal_israeli_jews_bearing_diplomatic_and_security_costs).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, divine_covenant_of_eretz_yisrael).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__religious_zionist_reading, theological_necessity_of_jewish_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizes settlement expansion in the West Bank as active fulfillment of covenant, lobbies government ministries, builds outposts ahead of and sometimes against formal state authorization, and frames any territorial concession as theological betrayal. Its exit from the constraint is foreclosed by its own identity: to renounce the claim is to renounce the movement's reason for existing.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_settler_movement, agenda_setter,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_settler_movement, beneficiary).

% Named as the collective recipient of the divine promise in this reading; diaspora and Israeli Jews alike are invoked as beneficiaries of the covenant regardless of whether any individual affirms the theological claim or bears its costs. Many within this group do not seek or endorse the maximalist claim but are structurally cast as its beneficiaries by the reading itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, covenant_community_jewish_people, beneficiary,
    moderate, civilizational, constrained, global).

% Provides military protection, infrastructure, legal recognition, and citizenship pathways for settlement in territory claimed under this reading; absorbs the international legitimacy costs while enabling continued expansion; retains freedom to calibrate enforcement level against diplomatic pressure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, state_apparatus_administering_settlement, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, state_apparatus_administering_settlement, beneficiary).

% Live under a permit and land-allocation regime substantially shaped by the theological claim to inalienable title: home demolitions, land expropriation for settlement expansion, and checkpoint restrictions are justified partly by reference to the covenant claim rather than security necessity alone. Cannot exit the territory that is the subject of the claim; cannot contest the claim's premises within the administering legal system.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_residents_of_west_bank, payer,
    powerless, biographical, trapped, regional).

% Displaced in 1948 and 1967 and barred from return; the theological title claim to the whole land is invoked in religious Zionist discourse as foreclosing any right of return or land restitution, since the land itself is held non-negotiable by divine grant rather than subject to political settlement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Serve in the military units that secure settlements built on maximalist theological grounds, absorb the international isolation and internal social division the claim generates, while also nominally counted among the covenant community's beneficiaries by the reading even when they reject settlement expansion or the theological premise itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, secular_and_liberal_israeli_jews_bearing_diplomatic_and_security_costs, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, secular_and_liberal_israeli_jews_bearing_diplomatic_and_security_costs, beneficiary).

% Issue resolutions treating the settlements as illegal under international law and treat the land question as subject to negotiated partition; their determinations carry no operative force against a claim whose legitimacy is asserted as theologically prior to and independent of international legal frameworks.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_and_un_bodies, excluded,
    institutional, generational, analytical, global).

% Interpret and transmit the covenant claim through rulings, yeshiva curricula, and public teaching; adjudicate what counts as fidelity to or betrayal of the divine mandate; their own institutional authority is constituted by the claim's continued theological centrality.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_theologians_and_rabbinic_authorities, observer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, religious_zionist_theologians_and_rabbinic_authorities, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a religious and national community around a shared theological narrative that supplies meaning, continuity, and collective purpose after millennia of dispersion and persecution; mobilizes settlement, defense, and political action toward a single unifying goal.
% TRANSFER_FUNCTION: Moves land, water rights, freedom of movement, and legal security from Palestinian residents and refugees to the settler movement and the state apparatus that enables it; also moves diplomatic capital, military risk, and international standing from the broader Israeli citizenry to the maximalist settlement project.
% ABSENT_VOICES: Palestinian residents and refugees have no standing within the theological framework itself to contest the land's disposition — the claim's structure treats their presence as incidental to a title question already settled by divine grant, and international legal bodies whose determinations would ordinarily arbitrate territorial disputes are treated as theologically irrelevant.
% DISAPPEARANCE_RATIONALE: If the theological title claim were to lose its mobilizing force, settlement expansion would lose its principal ideological justification independent of security argument, negotiated partition proposals would regain viability that maximalist theology currently forecloses, and the religious Zionist movement's institutional and political weight in Israeli coalition politics would collapse.
% FOUNDING_PROBLEM: Diaspora Jewish communities faced statelessness, recurring persecution, and the absence of a secure homeland; the covenant narrative reasserted an ancient, continuous claim to a specific territory as the theological ground for return and sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist rabbinic authorities and settler movement leaders attest the covenant claim as a live, unresolved theological mandate requiring ongoing fulfillment. Secular Israeli historians, international legal scholars, and Palestinian testimony from outside the beneficiary community attest that statehood was substantially achieved in 1948 and again in 1967 by military and political means, and that the continued invocation of divine title functions primarily to justify further territorial acquisition beyond any security or historical-return rationale — a reading corroborated by UN human rights reporting and by dissenting religious authorities within Judaism itself who reject the theological necessity of settlement expansion.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.86, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very high (0.86 at interval end) because, under this specific reading, the claim's structure treats the entire territory as subject to non-negotiable divine title — there is no partition-legitimate subset of the claim, which distinguishes it sharply from the liberal_nationalist sibling. Suppression is high (0.80) and enforcement-dependent: settlement expansion, home demolition, and permit regimes require continuous active state and paramilitary enforcement to hold against Palestinian resistance and international pressure. Theater ratio is comparatively low (0.25) because the enforcement apparatus (military, legal, administrative) is substantially functional rather than performative — the extraction is real, not merely staged. Accessibility collapse (0.72) and resistance (0.88) are both high: alternatives (partition, civic equality, negotiated return) have been substantially foreclosed by decades of settlement fact-on-the-ground, yet resistance from Palestinians, international bodies, and dissenting religious authorities remains active and organized, which is why this is authored tangled_rope rather than snare — there IS a genuine coordination function (diaspora meaning-making, community cohesion, security mobilization) riding alongside the asymmetric extraction, and both are structurally present and required by the gate.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious Zionist settler movement and the theological authorities sit at the low-d beneficiary end: they set the interpretive terms, capture territorial and institutional gains, and their exit is identity-locked rather than merely constrained — renouncing the claim would dissolve the movement's reason for being. The covenant_community_jewish_people entry is named as beneficiary by the reading's own logic, but many members of that nominal class do not endorse or benefit from the maximalist claim in practice; this gap is itself flagged as an omega. Palestinian residents and refugees sit at the high-d target end: trapped exit, no standing within the theological framework to contest disposition of the land, and the specific mechanism that harms refugees (foreclosure of return) is a direct structural entailment of divine-title non-negotiability rather than an incidental side effect. Secular and liberal Israeli Jews occupy an intermediate position — nominally counted as covenant beneficiaries while actually bearing diplomatic, military, and social costs generated by a maximalism many did not choose.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Jewish statelessness and persecution — was substantially resolved by 1948 and consolidated by 1967; the religious Zionist reading's specific theological maximalism (as opposed to statehood itself) increasingly serves to justify further territorial acquisition beyond any remaining security or survival rationale. This is exactly the mismatch the R5 genealogy interview is built to surface: founding_problem_status is authored contested rather than dead outright, because state security concerns remain partly live, but the disappearance_verdict of world_rearranges combined with corroboration from outside the beneficiary set (dissenting rabbinic authorities, international legal bodies, Palestinian testimony) indicates the theological non-negotiability clause specifically has outlived the problem it was mobilized to solve and now functions to block negotiated resolution rather than to secure survival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_beneficiary_class_coherence,
    'Is ''the Jewish people'' as covenant community a coherent beneficiary class, given that large portions of that population (secular, liberal, diaspora, anti-occupation) neither endorse the theological claim nor receive its territorial gains, and instead bear its diplomatic and security costs?',
    'Survey and political data on Israeli and diaspora Jewish opinion regarding settlement expansion and theological justification for it, compared against who actually occupies settled territory and captures its economic and land value.',
    'If the beneficiary class is substantially narrower than ''the Jewish people'' as a whole, the reading''s own framing overstates its beneficiary base and the true beneficiary is closer to the organized settler movement and allied state institutions specifically, strengthening a tangled_rope-toward-snare reading rather than a rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_beneficiary_class_coherence, empirical, 'Whether the nominal covenant beneficiary class matches the actual capturing class.').

omega_variable(
    theological_naturalness_vs_construction,
    'Is the divine promise a claim about an irreducible theological fact (in the framework''s own terms, unchallengeable from outside faith commitments) or a constructed political-religious instrument whose current form and territorial scope were shaped by 20th-century nationalist movements rather than being continuous with ancient textual tradition?',
    'Comparative historical and textual analysis of how territorial maximalism in religious Zionist doctrine (particularly post-1967 messianic settlement theology) diverges from earlier rabbinic traditions that treated exile and dispersion as theologically meaningful states, not merely awaiting reversal by political settlement.',
    'If the specific maximalist, settlement-mandating form of the claim is a 20th-century theological innovation rather than a continuous ancient doctrine, this weakens the ''inalienable, always-already-settled'' framing and supports treating the constraint as constructed and contestable rather than as a fixed theological given.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_naturalness_vs_construction, conceptual, 'Whether the maximalist territorial claim is ancient-continuous or modern-constructed within the tradition.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does this reading''s non-negotiability premise genuinely foreclose the liberal_nationalist_reading''s partition-compatible self-determination claim within a single coherent framework, or can both be held by different factions of the same broad Zionist movement without contradiction?',
    'Track whether religious Zionist and liberal nationalist Zionist factions have historically formed governing coalitions that jointly administer partition-adjacent policy (e.g., disengagement plans) without one side abandoning its core premise — coexistence in practice would indicate coexists_with rather than forecloses at the political level, even if the two premises are logically incompatible at the doctrinal level.',
    'Determines whether the reading_relations edge to liberal_nationalist_reading should be authored as forecloses (doctrinal incompatibility) or coexists_with (political coalition compatibility) — this story authors forecloses at the level of the core premise (non-negotiable divine title vs. negotiable self-determination right) while acknowledging political coexistence is empirically observed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether doctrinal foreclosure and political coexistence are compatible readings of the same relation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1967, 0.12).
narrative_ontology:measurement(jewi_tr_t1980, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(jewi_tr_t2005, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(jewi_tr_t2015, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2015, 0.23).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(jewi_be_t1980, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1980, 0.63).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 1993, 0.68).
narrative_ontology:measurement(jewi_be_t2005, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2005, 0.74).
narrative_ontology:measurement(jewi_be_t2015, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2015, 0.81).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 2024, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement(jewi_su_t1980, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 1993, 0.62).
narrative_ontology:measurement(jewi_su_t2005, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(jewi_su_t2015, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, post_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five linked readings of the jewish_sovereignty_palestine kernel. Each reading is authored as a structurally distinct constraint with its own epsilon, beneficiary/victim structure, and classification, per the ε-invariance principle: the natural-language label 'Zionism' or 'Jewish sovereignty over Palestine' conflates claims with different empirical status, different victim sets, and different persistence mechanisms. religious_zionist_reading is authored here as tangled_rope with very high extractiveness (0.86) and no partition-legitimate subset of the claim. liberal_nationalist_reading (partition-compatible self-determination) and cultural_zionist_reading (non-maximalist cultural center) are expected to authore substantially lower extractiveness given their compatibility with negotiated outcomes. settler_colonial_reading is expected to converge on similar downstream facts about displacement but ground them in a different causal-structural analysis (colonial pattern vs. covenant theology) rather than theological premise, which changes the beneficiary/victim naming even where the material facts overlap. post_zionist_reading treats statehood as already achieved and the founding narrative as now obstructive, which is a different claim in time (retrospective/critical) than this reading's claim of ongoing theological mandate (prospective/mandating). All five are linked bidirectionally via affects_constraints; this file does not adjudicate between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
