% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Zionist Refuge Reading of Territorial Legitimacy (1948 Founding + Post-1967 Security Control)
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the Zionist refuge reading of the contested
 *   territorial-legitimacy kernel: a reading that treats 1948 statehood as
 *   legally and morally settled (grounded in historical persecution, a
 *   claimed continuity of Jewish presence, and UN Resolution 181's partition
 *   authorization) while treating post-1967 territory as an open, negotiable
 *   security question rather than a closed sovereignty matter. Palestinian
 *   displacement is causally attributed, within this reading, to Arab state
 *   and leadership rejection of the 1947 partition and the war that followed,
 *   rather than to an independent Israeli policy choice. This is ONE of three
 *   readings of the same kernel; the sibling readings
 *   (palestinian_autochthony_reading, two_state_coexistence_reading) are
 *   separate constraint stories with their own ε values and their own
 *   stakeholder sets, per the ε-invariance principle — this file does not
 *   average across them or hedge between them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.62).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Reading of Territorial Legitimacy (1948 Founding + Post-1967 Security Control)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, '62efbf63-0807-4b9e-a0df-5a8192b441b8').
narrative_ontology:cs_kernel_codification('62efbf63-0807-4b9e-a0df-5a8192b441b8', distributed).
narrative_ontology:cs_authority_grounding('62efbf63-0807-4b9e-a0df-5a8192b441b8', distributed).
narrative_ontology:cs_reading_relation('62efbf63-0807-4b9e-a0df-5a8192b441b8', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('62efbf63-0807-4b9e-a0df-5a8192b441b8', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('62efbf63-0807-4b9e-a0df-5a8192b441b8', foundational, un_partition_acceptance_confers_settled_legitimacy).
narrative_ontology:cs_axiom_status(un_partition_acceptance_confers_settled_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('62efbf63-0807-4b9e-a0df-5a8192b441b8', un_partition_acceptance_confers_settled_legitimacy, conventional).
narrative_ontology:cs_axiom('62efbf63-0807-4b9e-a0df-5a8192b441b8', foundational, post_1948_displacement_attributable_to_arab_rejection).
narrative_ontology:cs_axiom_status(post_1948_displacement_attributable_to_arab_rejection, holdable).
narrative_ontology:cs_axiom_grounding('62efbf63-0807-4b9e-a0df-5a8192b441b8', post_1948_displacement_attributable_to_arab_rejection, empirically_contingent).
narrative_ontology:cs_axiom('62efbf63-0807-4b9e-a0df-5a8192b441b8', secondary, ongoing_security_threat_justifies_1967_territorial_control).
narrative_ontology:cs_axiom_status(ongoing_security_threat_justifies_1967_territorial_control, holdable).
narrative_ontology:cs_axiom_grounding('62efbf63-0807-4b9e-a0df-5a8192b441b8', ongoing_security_threat_justifies_1967_territorial_control, instrumental).
narrative_ontology:cs_reference_frame('62efbf63-0807-4b9e-a0df-5a8192b441b8', un_resolution_181_partition_authorization).
narrative_ontology:cs_drift_state('62efbf63-0807-4b9e-a0df-5a8192b441b8', post_1967_security_administration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('62efbf63-0807-4b9e-a0df-5a8192b441b8', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_jewish_citizens).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora_seeking_refuge).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_of_occupied_territories).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, un_partition_plan_legitimacy).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, jewish_historical_indigeneity_claim).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, post_holocaust_refuge_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a state whose founding they regard as the resolution of two millennia of statelessness and the direct legal outcome of UN General Assembly Resolution 181. Draw citizenship, security guarantees, and national self-determination from the arrangement. Many also experience genuine existential threat perception from neighboring states and armed non-state actors, which they see as vindicating continued territorial and security control beyond the 1948 lines.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_jewish_citizens, beneficiary,
    organized, generational, constrained, national).

% Hold a standing right of return under the Law of Return regardless of prior residence, grounded in the reading's core premise that Jewish presence in the land predates and outlasts any single sovereign and that persecution elsewhere makes guaranteed refuge a moral necessity. Their claim does not depend on continuous physical occupation of any specific parcel.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora_seeking_refuge, beneficiary,
    moderate, civilizational, mobile, global).

% Administer citizenship law, military control of the West Bank, settlement policy, and the legal architecture that treats 1948 borders as settled and 1967-acquired territory as negotiable-but-currently-administered for security reasons. Justify continued control by pointing to the 1948-67 wars and subsequent rejections of negotiated settlement as evidence that territorial concession without guaranteed security is untenable.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Live under military administration, checkpoint systems, and settlement expansion in territory this reading treats as a negotiable security buffer rather than a sovereignty question resolved in their favor. Their displacement and continued restriction is framed, within this reading, as a consequence of historical Arab rejection of the 1947 partition and subsequent wars rather than as an ongoing choice by the administering power.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_residents_of_occupied_territories, payer,
    powerless, biographical, trapped, local).

% Displaced during and after the 1948 war and its aftermath; hold no right of return under Israeli law. Within this reading, their displacement is attributed to Arab state rejection of partition and the war that followed, not to any action requiring redress by the state whose founding is treated as settled and legitimate.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, regional).

% Hold Israeli citizenship and its associated protections, but experience the state's self-definition as the nation-state of the Jewish people as structurally excluding full symmetrical belonging. Benefit from civil and political rights unavailable to residents of the territories, while bearing the status asymmetry the ethno-national founding premise produces.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_citizens_of_israel, beneficiary).

% Issued the original 1947 partition resolution this reading cites as its legitimating instrument, and have since issued resolutions (242, 338, and others) treating post-1967 territory differently from 1948 territory — a distinction this reading relies on but which international bodies increasingly read as obligating withdrawal rather than permitting indefinite administration.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, un_and_international_legal_bodies, observer,
    institutional, generational, analytical, global).

% Rejected the 1947 partition plan and entered the 1948 war; this reading treats their rejection as the proximate cause of subsequent Palestinian displacement and as forfeiting standing to contest the resulting 1948 borders. They are not seats in the present-day constraint but their historical choice is load-bearing for the reading's causal account.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, arab_state_governments_1947_48, excluded,
    institutional, biographical, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally grounded, internationally sanctioned mechanism for Jewish national self-determination and physical refuge following centuries of persecution culminating in the Holocaust, resolved through a UN-authorized partition that a portion of the affected population accepted and organized a state around.
% TRANSFER_FUNCTION: Moves sovereign territorial control, security administration, and citizenship-conferring statehood to the Jewish population of the partitioned land and later-arriving diaspora, while displacing or subordinating the Arab population that did not accept or benefit from the partition outcome — with post-1967 territorial control transferring security discretion to the Israeli state at the cost of Palestinian residents' self-determination and freedom of movement.
% ABSENT_VOICES: Palestinian refugees and residents of the occupied territories are the parties most materially affected by the reading's causal attribution (displacement as consequence of Arab rejection) but are not the ones authoring that attribution; their own accounts of dispossession, largely independent of Arab state decisions, are treated within this reading as secondary to the war-causation narrative.
% DISAPPEARANCE_RATIONALE: If this legitimacy reading were to disappear as the operative framework for policy and law, Israeli territorial administration of the West Bank, the Law of Return's asymmetric citizenship structure, and the treatment of 1948 as legally closed while 1967 remains negotiable would all lose their justificatory basis, forcing renegotiation of citizenship, land administration, and return claims across the entire territory.
% FOUNDING_PROBLEM: Centuries of European and Middle Eastern antisemitism culminating in genocide left the Jewish people without secure refuge or self-determination anywhere; the UN partition plan offered an internationally sanctioned mechanism to establish one, accepted by Jewish leadership and rejected by Arab governments and the Arab Higher Committee.
% FOUNDING_PROBLEM_CORROBORATION: The core historical claim — persistent persecution culminating in the Holocaust and UN General Assembly acceptance of partition in Resolution 181 — is corroborated by international historians and the UN's own documentary record, independent of Israeli state sources. The contested element is whether that founding problem still justifies present-day territorial control beyond 1948 lines; independent human rights bodies (UN OHCHR, B'Tselem, Amnesty International) and a substantial body of international law scholarship outside both Israeli and Palestinian advocacy structures assess the post-1967 administration as exceeding what the founding refuge rationale can justify, while Israeli state and allied legal scholarship maintains security necessity as continuous with the founding rationale.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects that within its own terms this reading vindicates a genuine and internationally sanctioned founding claim (1948) while simultaneously sustaining, through the same legitimacy architecture, an asymmetric security-administration regime over 1967-acquired territory that imposes substantial ongoing costs on a population that did not consent to and does not benefit from that administration. Suppression (0.62) is authored higher than extractiveness because active enforcement infrastructure — military administration, checkpoint systems, permit regimes — is required to sustain the territorial-control component regardless of how the founding claim is scored; this is a raw structural property, not scaled by scope. Theater ratio (0.28) is moderate: security rationale is substantively operative (real threat perception, real attacks history) but an increasing share of enforcement activity outpaces demonstrable security necessity as settlement infrastructure has expanded, which the rising theater trajectory captures. Accessibility collapse (0.45) is mid-range because, within this reading's own account, real alternatives (negotiated statehood, return-based settlement) remain notionally available even though they are foreclosed in practice by the reading's own security logic. Resistance (0.78) is authored high because this reading is met with sustained, organized contestation — from Palestinian residents, international legal bodies, and human rights organizations — that the reading's own architecture must continually answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli Jewish citizens and diaspora seeking refuge sit near the beneficiary end: the founding claim directly subsidizes their citizenship, security guarantee, and standing right of return, and their exit options (constrained/mobile) reflect genuine alternatives that most choose not to exercise rather than an absence of exit. Palestinian residents of the occupied territories and 1948 refugees sit at the target end: trapped exit options, no return right, and the causal narrative of the reading itself denies them redress standing by attributing their situation to third-party (Arab state) decisions rather than to the administering power's own choices. Palestinian citizens of Israel occupy an intermediate position — real citizenship benefits coexist with structural subordination to an ethno-national founding premise that does not fully include them, warranting the dual role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (statelessness and persecution culminating in genocide) is independently corroborated and was, at the moment of 1948, substantially live — this blocks any claim that the entire arrangement is pure zombie extraction with no real founding function. But the founding_problem_status is authored as contested rather than dead or live because the reading's own logic extends the founding rationale (security necessity) to territorial administration acquired nearly two decades after the founding event, and independent human-rights and international-law assessment increasingly treats that extension as exceeding what the original founding problem can justify. This is precisely the seat-divergence the framework is built to surface: from the beneficiary seat the entire arrangement remains one continuous vindication of the founding claim; from the payer seats the founding claim's genuine legitimacy at 1948 does not transfer cleanly to legitimating open-ended control acquired later under different circumstances.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_claim_vs_extension_scope,
    'Does the moral and legal legitimacy of the 1948 founding claim (grounded in persecution, UN authorization, and partition acceptance) extend to justify indefinite security administration of territory acquired in 1967, or are these two structurally separate legitimacy questions that this reading conflates?',
    'Comparative international-law analysis of whether post-conflict security administration doctrines (as applied elsewhere) permit indefinite, non-time-bound territorial control, versus requiring negotiated resolution within a bounded timeframe; historical review of whether security rationale has been the actual operative driver of settlement expansion versus a post-hoc justification.',
    'If the two legitimacy questions are structurally separable, this reading''s extension of 1948 legitimacy to 1967 territorial control is a rhetorical move rather than a logical entailment, and the extractive component of this constraint would be authored substantially higher and independent of the founding claim''s validity. If inseparable, the security rationale carries genuine continuity with the founding problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_claim_vs_extension_scope, conceptual, 'Whether 1948 founding legitimacy logically extends to justify open-ended 1967-territory security control.').

omega_variable(
    displacement_causation_attribution,
    'Is the causal attribution of Palestinian displacement to Arab state rejection of partition (rather than to Israeli military and political action during and after 1948) an accurate historical account or a legitimating narrative that occludes documented expulsion and flight under military pressure?',
    'Historiographical review comparing this reading''s causal account against the archival record examined by both Israeli ''new historians'' (Morris, Pappé, et al.) and state-aligned historiography; assessment of the extent to which displacement resulted from war conditions generally versus specific military and political policy decisions.',
    'If the causal attribution substantially understates direct policy responsibility for displacement, the victim-group''s claim for redress standing is stronger than this reading allows, and the extractiveness score for the 1948-era component of this constraint would be authored higher than the current 0.32 initial value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_causation_attribution, empirical, 'Historical accuracy of attributing Palestinian displacement primarily to Arab state rejection versus direct Israeli policy.').

omega_variable(
    kernel_framing_alternative_readings,
    'Could this reading''s core structural elements (uncontested 1948 legitimacy, negotiable 1967 boundaries, security-justified control) be reframed as a single coherent legitimacy claim, or does it in fact bundle two distinct claims — a settled founding-refuge claim and a contested ongoing-security claim — that a more granular decomposition would separate into their own constraint stories?',
    'Apply the ε-invariance test: if the 1948 component and the post-1967 component show substantially different ε values when isolated (as the measurement trajectory here suggests, rising from 0.32 to 0.58 across the interval), that is evidence for decomposition into two linked stories rather than one.',
    'If decomposed, the 1948 component would likely classify as a rope or near-mountain (widely corroborated founding claim with minimal extraction) while the post-1967 security-administration component would likely classify independently as tangled_rope or snare with a higher, more stable ε — sharpening the analysis this single bundled story currently blurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_readings, conceptual, 'Whether this reading should itself be decomposed into a 1948-founding constraint and a 1967-security-administration constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1947, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(terr_tr_t1980, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1993, 0.28).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(terr_be_t1947, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1947, 0.32).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1967, 0.42).
narrative_ontology:measurement(terr_be_t1980, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1980, 0.48).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1993, 0.45).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2005, 0.53).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2015, 0.57).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1947, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1947, 0.35).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement(terr_su_t1980, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1980, 0.6).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1993, 0.52).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2015, 0.62).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__zionist_refuge_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the territorial_legitimacy_dual kernel. palestinian_autochthony_reading grounds Palestinian legitimacy in continuous habitation and right of return with a directly opposed causal account of 1948 displacement; two_state_coexistence_reading treats 1967 lines as a negotiated compromise rather than either a closed matter (this reading) or an unresolved injustice (the autochthony reading). All three share the same underlying territorial and demographic facts but author different ε, different beneficiary/victim sets, and different type classifications because they differ in which historical claims they treat as settled versus contested. This reading's extractiveness (0.58) is authored independently of the sibling readings' ε values, per the ε-invariance principle — no averaging or hedging across readings has occurred.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
