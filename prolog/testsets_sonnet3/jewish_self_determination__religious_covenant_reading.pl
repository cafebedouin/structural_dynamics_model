% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__religious_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__religious_covenant_reading, []).

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
 *   constraint_id: jewish_self_determination__religious_covenant_reading
 *   human_readable: Divine Covenant as Ground of Territorial Sovereignty
 *   domain: political_philosophy/religious_nationalism/postcolonial_theory
 *
 * SUMMARY:
 *   This story instantiates one reading within the contested
 *   jewish_self_determination kernel: the claim that Jewish territorial
 *   sovereignty over the land is a divine covenantal obligation, independent
 *   of and prior to any secular political framework, international law, or
 *   negotiated settlement. Within this reading's own theological framework,
 *   the covenant is treated as an absolute, unchangeable commitment —
 *   structurally a mountain, since it is presented as emerging from a fixed
 *   transcendent order rather than human construction, and to the sincere
 *   believer it is not negotiable any more than a law of physics is
 *   negotiable. But the reading is authored here as it operates politically:
 *   a mountain claim operationalized through organized religious-Zionist
 *   institutions, state subsidy channels, and settlement infrastructure that
 *   extract land, security resources, and negotiating latitude from
 *   identifiable secular and Palestinian parties. That operational
 *   entanglement — genuine internal coordination function (unifying
 *   religious-Zionist identity and mobilization) plus asymmetric extraction
 *   from those outside or dissenting within the framework, requiring active
 *   enforcement (military protection of settlements, legal shielding of
 *   outposts, political veto pressure against withdrawal) — is why the
 *   claimed_type here is tangled_rope rather than mountain, even though the
 *   reading's own theological self-understanding is mountain-shaped. This
 *   divergence between the reading's self-presentation and its computed
 *   structural operation is exactly the kind of measurement this framework
 *   exists to take.
 *
 * KEY AGENTS:
 *   - religious_zionist_movement: primary agenda-setter and beneficiary (organized/identity_locked) — mobilizes covenant claim into settlement and political policy
 *   - settlement_enterprise: beneficiary (organized/constrained) — receives land and resources justified by the claim
 *   - secular_negotiation_framework: primary payer (institutional/constrained) — bears the cost of foreclosed compromise
 *   - palestinian_residents_of_contested_territory: primary payer (powerless/trapped) — bears expropriation and restriction with no standing in the framework
 *   - religious_law_authorities: agenda-setter/observer (institutional/identity_locked) — adjudicates covenant's practical application without power to revise its premise
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, 0.78).
domain_priors:suppression_score(jewish_self_determination__religious_covenant_reading, 0.72).
domain_priors:theater_ratio(jewish_self_determination__religious_covenant_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__religious_covenant_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__religious_covenant_reading, tangled_rope).
narrative_ontology:human_readable(jewish_self_determination__religious_covenant_reading, "Divine Covenant as Ground of Territorial Sovereignty").
narrative_ontology:topic_domain(jewish_self_determination__religious_covenant_reading, "political_philosophy/religious_nationalism/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_self_determination__religious_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__religious_covenant_reading, '074d7596-84f1-4623-bb03-389a65cf6514').
narrative_ontology:cs_kernel_codification('074d7596-84f1-4623-bb03-389a65cf6514', fixed_text).
narrative_ontology:cs_authority_grounding('074d7596-84f1-4623-bb03-389a65cf6514', lineage).
narrative_ontology:cs_interpretation_layer_present('074d7596-84f1-4623-bb03-389a65cf6514').
narrative_ontology:cs_reading_relation('074d7596-84f1-4623-bb03-389a65cf6514', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('074d7596-84f1-4623-bb03-389a65cf6514', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('074d7596-84f1-4623-bb03-389a65cf6514', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('074d7596-84f1-4623-bb03-389a65cf6514', jewish_self_determination__diasporist_reading, forecloses).
narrative_ontology:cs_axiom('074d7596-84f1-4623-bb03-389a65cf6514', foundational, divine_grant_is_prior_to_secular_sovereignty_claims).
narrative_ontology:cs_axiom_status(divine_grant_is_prior_to_secular_sovereignty_claims, holdable).
narrative_ontology:cs_axiom_grounding('074d7596-84f1-4623-bb03-389a65cf6514', divine_grant_is_prior_to_secular_sovereignty_claims, theological).
narrative_ontology:cs_axiom('074d7596-84f1-4623-bb03-389a65cf6514', foundational, territorial_retention_is_a_binding_commandment_not_a_policy_choice).
narrative_ontology:cs_axiom_status(territorial_retention_is_a_binding_commandment_not_a_policy_choice, holdable).
narrative_ontology:cs_axiom_grounding('074d7596-84f1-4623-bb03-389a65cf6514', territorial_retention_is_a_binding_commandment_not_a_policy_choice, theological).
narrative_ontology:cs_reference_frame('074d7596-84f1-4623-bb03-389a65cf6514', abrahamic_land_covenant_as_binding_grant).
narrative_ontology:cs_drift_state('074d7596-84f1-4623-bb03-389a65cf6514', post_1993_oslo_negotiation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('074d7596-84f1-4623-bb03-389a65cf6514', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__religious_covenant_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, religious_zionist_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settlement_enterprise).
narrative_ontology:constraint_beneficiary(jewish_self_determination__religious_covenant_reading, settler_yeshiva_networks).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_negotiation_framework).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, palestinian_residents_of_contested_territory).
narrative_ontology:constraint_victim(jewish_self_determination__religious_covenant_reading, secular_and_liberal_israeli_public).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, divine_promise_to_abraham).
narrative_ontology:constraint_vindicates(jewish_self_determination__religious_covenant_reading, unbroken_religious_title_to_the_land).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads Genesis and Deuteronomy covenant texts as an operative, binding land grant, not metaphor. Builds political parties, yeshivot, and settlement councils around the claim that possession of the land is a religious commandment (mitzvat yishuv ha'aretz). Sets policy agendas that make territorial withdrawal a religious violation, not merely a political concession. Its exit from the framework is foreclosed by its own theology: to abandon the covenant claim would dissolve the movement's reason for existing.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, agenda_setter,
    organized, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, religious_zionist_movement, beneficiary).

% Receives land allocation, infrastructure investment, and legal cover justified by the covenant claim. State subsidies, security details, and legal recognition of outposts flow more readily when framed as fulfillment of religious obligation. Could not easily relocate without dismantling the theological justification for its presence.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settlement_enterprise, beneficiary,
    organized, generational, constrained, regional).

% Educational institutions whose curriculum, funding, and recruitment depend on treating the covenant as literally binding. Their institutional identity and the career paths of their rabbis are constituted by the claim; abandoning it would be an act of self-dissolution, not policy revision.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, settler_yeshiva_networks, beneficiary,
    organized, civilizational, identity_locked, national).

% Israeli governments, courts, and diplomatic negotiators attempting land-for-peace or two-state frameworks bear the cost of the covenant claim: any territorial compromise can be recast domestically as a violation of divine command, mobilizing religious-nationalist veto power against ratified agreements (e.g. settler resistance to withdrawal, assassination of negotiators). The framework cannot exit the constraint because it operates inside the same polity that hosts the religious claim.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_negotiation_framework, payer,
    institutional, biographical, constrained, national).

% Live under settlement expansion, land expropriation, and movement restriction justified in part by the covenant claim's insistence that the land cannot be alienated to non-Jewish sovereignty regardless of demographic or legal facts on the ground. Have no standing within the religious framework itself and cannot negotiate against a claim structured to be non-negotiable.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, palestinian_residents_of_contested_territory, payer,
    powerless, biographical, trapped, local).

% Bears the domestic costs of a foreclosed compromise space: conscription and security burdens tied to settlement defense, diplomatic isolation, and periodic political crises when governments attempt withdrawal. Can vote and protest but cannot out-argue a claim that is theologically, not empirically, grounded.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, secular_and_liberal_israeli_public, payer,
    moderate, biographical, constrained, national).

% Many diaspora Jews reject the covenant-as-sovereignty-obligation reading, preferring pluralist or diasporist frames, but are treated within the religious covenant reading as either insufficiently observant or as a remnant awaiting ingathering. Their dissent carries little weight inside a framework that locates authority in textual and rabbinic transmission, not communal vote.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, diaspora_jewish_communities, excluded,
    moderate, generational, mobile, global).

% Rabbinic courts and halakhic decisors adjudicate what the covenant requires in specific circumstances (e.g. whether pikuach nefesh, saving life, can override land retention). Their rulings can moderate or intensify the claim's political force, but the underlying covenant premise itself is not something their office can revise without abandoning the tradition's own claimed continuity.
narrative_ontology:constraint_stakeholder(jewish_self_determination__religious_covenant_reading, religious_law_authorities, agenda_setter,
    institutional, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__religious_covenant_reading, religious_law_authorities, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__religious_covenant_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__religious_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides religious Zionism with a unifying, transcendent justification that coordinates settlement activity, political mobilization, and resource allocation around a single non-negotiable object (the land), solving the movement's internal problem of motivating sacrifice and permanence beyond ordinary political incentives.
% TRANSFER_FUNCTION: Moves land, state subsidy, security protection, and political veto power toward the religious-Zionist settlement enterprise, and moves negotiating room, physical security, and land access away from Palestinian residents and from secular Israeli institutions attempting territorial compromise.
% ABSENT_VOICES: Palestinian residents of the contested territory have no standing within the covenant framework itself — the claim is adjudicated entirely within Jewish religious sources and does not contemplate their consent as a relevant variable. Diaspora Jews who reject sovereignty-as-religious-obligation are treated as outside the tradition's authoritative voice rather than as a competing constituency.
% DISAPPEARANCE_RATIONALE: If the covenant claim's political force disappeared overnight — i.e., if territorial retention were no longer treated as religiously obligatory — settlement expansion would lose its strongest domestic justification, negotiators would face a substantially smaller veto bloc, and withdrawal or land-swap agreements would become politically survivable in ways they currently are not. Religious-Zionist institutions built around the claim would need to reconstitute their purpose entirely.
% FOUNDING_PROBLEM: After centuries of diaspora and, acutely, after the Holocaust, the covenant reading was mobilized to answer the question of why Jewish return to the land was not merely a political project of the powerful but a fulfillment of ancient, binding promise — providing a source of legitimacy that did not depend on the contingent approval of international bodies or neighboring populations.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist rabbis and settlement leaders attest the covenant remains a live, binding obligation. Secular Israeli historians, some liberal religious authorities, and international legal scholars outside the settlement movement attest that the founding problem of establishing a durable, internationally recognized state was substantially resolved by 1948/1967 through political and military means independent of the covenant claim, and that the claim now functions primarily to block negotiated territorial compromise rather than to secure survival.
narrative_ontology:disappearance_verdict(jewish_self_determination__religious_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__religious_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__religious_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__religious_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__religious_covenant_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__religious_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__religious_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__religious_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as rising over the interval (0.35 to 0.78) because the covenant claim's political mobilization intensified after 1967 with organized settlement, hardened through the 1990s Oslo-era conflicts over land-for-peace, and reached its highest political leverage in the 2010s-2020s period of settlement expansion and coalition dependence on religious-nationalist parties. Suppression tracks the same arc: enforcing the claim increasingly required active state and paramilitary resources (settlement security details, legal shielding of outposts, political penalties for coalition partners who support withdrawal). Theater ratio is moderate and rising (0.20 to 0.42): a substantial share of covenant-based political rhetoric is sincerely load-bearing for believers, but an increasing share functions as post-hoc justification for facts-on-the-ground settlement expansion that would proceed on strategic/security grounds regardless. Accessibility collapse (0.68) and resistance (0.75) are both authored high — reflecting that within the religious-Zionist framework, alternatives to the covenant claim are nearly unthinkable (near-mountain accessibility collapse for believers), while outside that framework the claim meets substantial and organized resistance (secular Israelis, international legal bodies, Palestinian civil society, and diasporist/liberal-nationalist Jewish critics) rather than quiet acceptance.
 *
 * PERSPECTIVAL GAP:
 *   From the religious_zionist_movement's seat, the claim is a mountain: an unchangeable, prior fact about the world that political frameworks must accommodate, not negotiate. From the secular_negotiation_framework and palestinian_residents seats, the same claim computes as an actively enforced extraction mechanism that removes specific parcels of land from any negotiable status regardless of demographic, legal, or security facts. The engine should compute these as genuinely different seat-level classifications from the same structural data — that divergence is not an error to be reconciled but the finding itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The religious_zionist_movement and settlement_enterprise sit near the full-beneficiary end: the claim subsidizes their political standing, resource access, and institutional purpose. secular_negotiation_framework and secular_and_liberal_israeli_public sit closer to symmetric-but-paying: they operate within the same polity and cannot exit it, but bear diffuse costs (security burden, diplomatic isolation, blocked compromise) rather than direct expropriation. palestinian_residents_of_contested_territory sit at the full-target end: trapped, powerless, and directly subject to land loss and movement restriction justified by a claim in which they have no interpretive standing whatsoever — this is the sharpest directionality asymmetry in the story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing Jewish return to the land on a legitimacy basis independent of contingent political approval — was substantively achieved through the political and military establishment of the state (1948) and its consolidation (1967, subsequent peace treaties and diplomatic recognition). The covenant claim's persistence past that point, now directed primarily at blocking negotiated withdrawal from specific contested territories rather than securing the state's existence, is the mandatrophy signature: a claim whose founding function has been substantially met continuing to operate as a live veto on a different, narrower question (which specific parcels of land, not whether a Jewish state exists). Classifying this as tangled_rope rather than dismissing it as pure snare preserves the genuine, sincerely held coordination function it still performs for religious-Zionist communal identity — it is not merely a cynical instrument, even though its political operation now extracts asymmetrically from non-participants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_as_natural_law_or_constructed_claim,
    'Is the covenant a genuine transcendent fact binding regardless of human politics (as its adherents hold), or is it a constructed religious-political claim whose ''naturalness'' is itself an artifact of the movement that benefits from treating it as immutable?',
    'This is not resolvable by empirical or historical method in the way ordinary constructed-vs-natural questions are — it is a question of theological truth that different traditions and individuals answer by faith commitment, not evidence. The closest available proxy is tracking whether religious authorities themselves have ever revised or overridden the claim under changed circumstances (pikuach nefesh rulings, historical instances of religious accommodation to political necessity), which would indicate the claim is treated as defeasible in practice even by its own tradition.',
    'If treated as genuinely immutable divine command, the constraint is a mountain at the level of individual conscience for believers, and any classification as tangled_rope describes only its political operationalization, not its truth-value. If treated as a constructed claim serving identifiable beneficiaries (the settlement enterprise, religious-nationalist parties), the false-summit signature is directly applicable: a mountain-framed claim with concentrated beneficiaries.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_as_natural_law_or_constructed_claim, conceptual, 'Whether the covenant claim is genuine transcendent fact or constructed political-religious claim serving beneficiaries — irreducible by design (FSM candidate).').

omega_variable(
    committer_structure_kernel_reading_location,
    'This constraint is one reading (religious_covenant_reading) of the contested jewish_self_determination kernel. Where exactly does its structural claim diverge from the sibling readings, and what would change if a different reading were dominant in Israeli political discourse?',
    'Compare the beneficiary/victim structure and epsilon profile of this reading against liberal_nationalist_reading, indigenous_return_reading, settler_colonial_reading, and diasporist_reading as separately authored constraint stories. The key structural divergence: this reading treats territorial retention of SPECIFIC parcels as religiously non-negotiable, which none of the secular readings do — liberal_nationalist_reading permits territorial compromise in principle (national self-determination does not require every parcel), while this reading forecloses that possibility a priori for religiously significant land.',
    'If religious_covenant_reading were displaced as the dominant public justification by liberal_nationalist_reading, territorial negotiation would become politically tractable in ways currently blocked; the settlement enterprise''s political veto power would weaken substantially since it would lose its claim to trump ordinary political horse-trading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading_location, conceptual, 'Kernel-reading structural location: this reading''s non-negotiability premise versus sibling readings that permit political compromise.').

omega_variable(
    internal_vs_effective_epsilon_gap,
    'How should the very large gap between the claim''s internal epsilon (near zero — divine command is definitionally not extractive on its own terms, it is obedience) and its effective political epsilon (high — 0.78 authored here, given contested framework status and asymmetric enforcement) be handled analytically?',
    'This gap is the central structural fact the story exists to represent, per the epsilon referent rule for kernel readings: epsilon is authored for the standing arrangement under contest, assessed by the reading''s own lights, not for an idealized internal-only frame. The 0.78 figure reflects the reading''s actual political operation (enforcement, dispossession, foreclosure of compromise) as it plays out in a contested pluralistic society, not the theological purity of the covenant claim taken alone.',
    'Authoring epsilon at the effective/political level (rather than the internal-theological level) is what allows this reading to register as tangled_rope rather than mountain in the engine''s computation — which is the correct outcome given that the claim operates through state institutions and contested territory shared with non-adherents, not within a closed religious community accountable only to itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(internal_vs_effective_epsilon_gap, conceptual, 'Resolves why epsilon is authored high despite the claim''s internal theological self-presentation as absolute and non-extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__religious_covenant_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__religious_covenant_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(jewi_tr_t1977, jewish_self_determination__religious_covenant_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__religious_covenant_reading, theater_ratio, 1993, 0.3).
narrative_ontology:measurement(jewi_tr_t2005, jewish_self_determination__religious_covenant_reading, theater_ratio, 2005, 0.34).
narrative_ontology:measurement(jewi_tr_t2015, jewish_self_determination__religious_covenant_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__religious_covenant_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(jewi_be_t1977, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1977, 0.48).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__religious_covenant_reading, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement(jewi_be_t2005, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(jewi_be_t2015, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2015, 0.71).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__religious_covenant_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(jewi_su_t1977, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1977, 0.42).
narrative_ontology:measurement(jewi_su_t1993, jewish_self_determination__religious_covenant_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(jewi_su_t2005, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(jewi_su_t2015, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__religious_covenant_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__religious_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__religious_covenant_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__religious_covenant_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the jewish_self_determination kernel, each authored as a separate constraint story per the ε-invariance principle: religious_covenant_reading (this story, tangled_rope, high effective epsilon despite near-zero internal epsilon), liberal_nationalist_reading (national self-determination parity claim), indigenous_return_reading (indigeneity/decolonization framing), settler_colonial_reading (dispossession/colonization framing), and diasporist_reading (anti-sovereignty, pluralism-first framing). Each reading has a structurally distinct beneficiary/victim configuration and a distinct epsilon because each identifies a different arrangement as the object under contest. This story's distinguishing structural contribution is non-negotiability: unlike the liberal_nationalist_reading, which permits territorial compromise as a matter of ordinary political bargaining, this reading treats specific parcels as religiously foreclosed from compromise, which is what elevates its effective extraction relative to the other readings even where beneficiary groups substantially overlap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
