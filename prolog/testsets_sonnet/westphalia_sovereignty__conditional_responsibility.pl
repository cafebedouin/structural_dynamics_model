% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Responsibility to Protect (R2P) — Conditional Sovereignty Doctrine
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This story is the conditional_responsibility reading of the Westphalian
 *   sovereignty kernel: the doctrine articulated as Responsibility to Protect
 *   (R2P), adopted at the 2005 UN World Summit, holding that sovereignty is a
 *   conditional trust — states forfeit protection from external intervention
 *   when they fail to shield their own populations from genocide, war crimes,
 *   ethnic cleansing, or crimes against humanity. This is generated as ONE
 *   structurally distinct constraint, not a blend with the
 *   absolute_non_intervention or graded_sovereignty readings, which are
 *   separate stories in the same kernel family. The rise in theater_ratio and
 *   suppression_requirement around 2011 tracks the Libya intervention, widely
 *   read afterward (especially by Russia, China, and non-aligned states) as
 *   R2P used for regime change rather than population protection — a use that
 *   hardened resistance to invoking the doctrine again (visible in the
 *   subsequent Syria paralysis) even as the doctrine's institutional
 *   apparatus (monitoring bodies, special advisers, academic literature)
 *   continued to expand.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.58).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.52).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Responsibility to Protect (R2P) — Conditional Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, '41e0ff95-f7ab-4f7c-ba15-99ecb93162f0').
narrative_ontology:cs_kernel_codification('41e0ff95-f7ab-4f7c-ba15-99ecb93162f0', formalized).
narrative_ontology:cs_authority_grounding('41e0ff95-f7ab-4f7c-ba15-99ecb93162f0', extraction).
narrative_ontology:cs_interpretation_layer_present('41e0ff95-f7ab-4f7c-ba15-99ecb93162f0').
narrative_ontology:cs_reading_relation('41e0ff95-f7ab-4f7c-ba15-99ecb93162f0', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('41e0ff95-f7ab-4f7c-ba15-99ecb93162f0', westphalia_sovereignty__graded_sovereignty, coexists_with).
narrative_ontology:cs_axiom('41e0ff95-f7ab-4f7c-ba15-99ecb93162f0', foundational, protection_duty_supersedes_territorial_inviolability).
narrative_ontology:cs_axiom_status(protection_duty_supersedes_territorial_inviolability, holdable).
narrative_ontology:cs_axiom_grounding('41e0ff95-f7ab-4f7c-ba15-99ecb93162f0', protection_duty_supersedes_territorial_inviolability, deontological).
narrative_ontology:cs_axiom('41e0ff95-f7ab-4f7c-ba15-99ecb93162f0', secondary, international_community_holds_residual_adjudicative_authority).
narrative_ontology:cs_axiom_status(international_community_holds_residual_adjudicative_authority, holdable).
narrative_ontology:cs_axiom_grounding('41e0ff95-f7ab-4f7c-ba15-99ecb93162f0', international_community_holds_residual_adjudicative_authority, conventional).
narrative_ontology:cs_reference_frame('41e0ff95-f7ab-4f7c-ba15-99ecb93162f0', post_1945_charter_based_sovereign_equality).
narrative_ontology:cs_drift_state('41e0ff95-f7ab-4f7c-ba15-99ecb93162f0', post_libya_intervention_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('41e0ff95-f7ab-4f7c-ba15-99ecb93162f0', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, un_security_council_permanent_members).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, weak_state_governments).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, non_aligned_bloc_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, population_protection_supersedes_territorial_inviolability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Civilians facing mass killing, ethnic cleansing, or genocide by their own state or by armed factions within it. In principle they are the doctrine's intended beneficiaries — the promise is that the world will act when their own government will not. In practice they bear the costs when intervention is selective, delayed, or itself destructive (bombing campaigns, prolonged civil war from external arming of factions, sanctions regimes that immiserate the population the doctrine claims to protect). They cannot exit their situation; they can only wait to see whether the international community classifies their suffering as sufficient to trigger action.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, payer).

% Governments of states without great-power patrons or Security Council veto protection. They hold nominal Westphalian sovereignty but discover it is conditional the moment a humanitarian crisis inside their borders draws international attention. They can appeal to non-intervention norms, but lack the diplomatic leverage to make the appeal stick if a coalition decides to act. Exit from the constraint means acquiring a great-power patron or nuclear deterrent — most cannot.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, weak_state_governments, payer,
    moderate, biographical, constrained, national).

% Ad hoc coalitions of willing states (typically NATO members and allies) that invoke R2P to authorize or justify intervention. They select which atrocities trigger action and which do not, largely following strategic interest rather than atrocity severity alone. They bear little cost from inconsistent application and gain legitimacy narrative, basing rights, and strategic access from interventions they lead.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, agenda_setter,
    institutional, generational, arbitrage, global).

% The five veto-holding powers control whether R2P authorization is granted, and can shield their own clients or themselves from the doctrine's application by veto while applying it to rivals' clients. They administer the gate through which the doctrine passes into legitimate force, and are functionally exempt from having it applied against their own territorial conduct.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, un_security_council_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, un_security_council_permanent_members, beneficiary).

% States from the Global South, often former colonies, that argue the doctrine has been applied asymmetrically against weaker states while great powers escape scrutiny for comparable or worse conduct. Their objections are voiced in UN General Assembly debate but carry no veto weight and rarely alter Security Council outcomes; the norm-setting institutions in which R2P doctrine is elaborated are dominated by the same states that invoke it.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, non_aligned_bloc_states, excluded,
    organized, generational, constrained, global).

% The UN system, international criminal tribunals, and affiliated NGOs gain expanded mandate, funding, and adjudicative authority from the doctrine's existence — they administer atrocity determination, monitor compliance, and issue findings that trigger or justify intervention debates. Their institutional relevance and budget are tied to the doctrine remaining active and contested.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, beneficiary,
    institutional, civilizational, analytical, global).

% Academics and career diplomats who study the doctrine's application record, comparing invoked cases (Libya) against uninvoked ones (Syria's later years, Xinjiang, Yemen) to assess whether the doctrine is applied on atrocity severity or geopolitical convenience.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, legal_scholars_and_diplomats, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__conditional_responsibility, un_security_council_permanent_members).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__conditional_responsibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared international standard for when the collective security system may act inside a state's borders to stop mass atrocity, replacing ad hoc unilateral justification with a doctrine debated and elaborated through UN institutions.
% TRANSFER_FUNCTION: Moves adjudicative authority over the legitimacy of a state's internal conduct from that state's own government to the Security Council and the intervention coalitions it authorizes; moves resources, media attention, and international legal capital toward the crises great powers choose to act on and away from those they do not.
% ABSENT_VOICES: Populations actually experiencing atrocity have no seat in the Security Council debate deciding whether their situation qualifies for intervention. Non-aligned states raise the asymmetric-application objection in General Assembly forums but hold no veto and cannot compel consistent application; their objection is heard but not binding.
% DISAPPEARANCE_RATIONALE: Great powers and humanitarian institutions would say atrocity prevention loses its clearest normative and legal hook, and intervention would revert to naked unilateral justification. Non-aligned states and target governments would say little changes in practice, since the doctrine is already applied so selectively that its removal would mainly strip a legitimating vocabulary from interventions that would happen (or not happen) on strategic grounds regardless.
% FOUNDING_PROBLEM: The international community's failure to intervene in Rwanda (1994) and Srebrenica (1995), where absolute non-intervention norms were invoked to justify inaction while genocide and mass killing proceeded unimpeded, created pressure to establish a normative and legal basis for overriding sovereignty in extreme cases.
% FOUNDING_PROBLEM_CORROBORATION: UN-commissioned inquiries into the Rwanda and Srebrenica failures (independent of the coalitions that later invoked R2P) corroborate that the founding problem was real and severe. Independent legal scholars and non-aligned diplomats corroborate that the problem remains partly live but argue the doctrine's actual operation has been substantially captured by great-power strategic interest — citing Libya (2011) as invoked while comparable or worse crises (Syria post-2013, Yemen, Xinjiang) went unaddressed by the same mechanism.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, contested).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.58) reflects that adjudicative authority over a state's internal legitimacy has genuinely been transferred to Security Council permanent members and the coalitions they authorize, and that this authority is exercised asymmetrically — invoked against strategically inconvenient regimes, withheld from great powers' own clients or a permanent member's own conduct. Suppression (0.52) is moderate: the doctrine does not eliminate the non-intervention counter-norm (weak states can and do invoke it, non-aligned blocs organize resistance), but selective invocation functions as real coercive leverage against targeted states. Theater ratio (0.44) is elevated because a substantial share of R2P's institutional activity — special adviser offices, annual dialogues, doctrine elaboration — proceeds independent of whether the doctrine is actually invoked to stop an ongoing atrocity, and the Syria non-intervention record after 2013 suggests much of this activity is now performative maintenance of the doctrine's legitimacy rather than functional atrocity prevention.
 *
 * DIRECTIONALITY LOGIC:
 *   Populations under atrocity regimes are declared beneficiaries in the doctrine's own justificatory language, but the structural data places most of the realized transfer with Security Council permanent members and intervention coalitions, who gain adjudicative authority and strategic access, and with global governance institutions, who gain expanded mandate. Populations bear the downside risk of both selective inaction and destructive intervention, which is why they also carry a secondary payer role — this dual role is intentional and reflects the doctrine's genuinely contested beneficiary structure, not a modeling error.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Rwanda, Srebrenica) was real and is independently corroborated by UN inquiry outside the coalition that later benefited from the doctrine's adoption — this blocks a clean mandatrophy read (the problem was not manufactured). But founding_problem_status is authored 'contested' rather than 'dead' or 'live' because the doctrine's actual invocation record since 2011 diverges sharply from a pure protection function: Libya was invoked, Syria (with comparably severe atrocities) was not, and the asymmetry tracks great-power strategic interest more closely than atrocity severity. This is the seat-divergence the tangled_rope classification is built to hold: intervention coalitions and Security Council permanent members experience a functioning coordination mechanism they administer; non-aligned states and target populations experience a mechanism whose stated function (protection) diverges from its operating pattern (selective legitimation of great-power action).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrocity_threshold_objectivity,
    'Is the atrocity threshold that triggers R2P invocation an objective, atrocity-severity-based standard, or is it a discretionary political judgment dressed in legal language?',
    'Comparative case analysis: cross-reference atrocity severity metrics (casualty counts, UN commission of inquiry findings) against actual R2P invocation decisions across all post-2005 candidate cases, testing correlation with permanent-member strategic interest versus atrocity severity alone.',
    'If invocation correlates more strongly with strategic interest than atrocity severity, the doctrine''s coordination story (protecting populations) is substantially cover for a selective extraction/legitimation function; if it correlates primarily with severity, the tangled_rope classification should weight toward genuine coordination with incidental asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrocity_threshold_objectivity, empirical, 'Whether R2P invocation tracks atrocity severity or great-power strategic interest.').

omega_variable(
    sovereignty_as_natural_vs_constructed,
    'Is Westphalian territorial sovereignty itself a natural/foundational feature of the state system that this doctrine conditions, or is ''sovereignty'' already a constructed legal fiction whose conditionality was always latent?',
    'Historical-legal analysis of the Peace of Westphalia''s actual textual commitments versus the retrospective doctrine built on top of it; examination of how consistently ''inviolable'' sovereignty was actually honored prior to R2P''s codification.',
    'If sovereignty was never absolute in practice, R2P is better read as formalizing a pre-existing informal conditionality rather than creating a new extraction mechanism — this would lower the confidence that the doctrine itself is the extractive layer, redirecting scrutiny to the enforcement/veto mechanism instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_as_natural_vs_constructed, conceptual, 'Whether conditional sovereignty is a genuine doctrinal innovation or a formalization of pre-existing practice.').

omega_variable(
    kernel_reading_selection_pressure,
    'This story instantiates the conditional_responsibility reading; the sibling readings (absolute_non_intervention, graded_sovereignty) would classify the same underlying interventions differently. What determines which reading a given international actor adopts in a specific crisis, and does that selection itself track power position?',
    'Track which reading each Security Council permanent member and non-aligned bloc invokes across multiple crises (Libya, Syria, Myanmar, Xinjiang) and test whether reading selection correlates with whether the actor''s own client state or rival''s client state is the intervention target.',
    'If reading selection is itself strategic (powerful states invoke conditional_responsibility against rivals'' clients and absolute_non_intervention to shield their own conduct or allies), the kernel-level contest is not a genuine doctrinal disagreement but a strategic toolkit — this would be evidence for treating all three readings as simultaneously live and selectively deployed rather than as competing settled positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether kernel reading selection tracks genuine doctrinal commitment or strategic convenience.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 2001, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t2001, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2001, 0.2).
narrative_ontology:measurement(west_tr_t2005, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(west_tr_t2011, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2011, 0.3).
narrative_ontology:measurement(west_tr_t2015, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(west_tr_t2020, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2020, 0.48).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(west_be_t2001, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2001, 0.32).
narrative_ontology:measurement(west_be_t2005, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement(west_be_t2011, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2011, 0.55).
narrative_ontology:measurement(west_be_t2015, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(west_be_t2020, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement(west_be_t2024, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t2001, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2001, 0.35).
narrative_ontology:measurement(west_su_t2005, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(west_su_t2011, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2011, 0.58).
narrative_ontology:measurement(west_su_t2015, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(west_su_t2020, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(west_su_t2024, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% Three sibling readings of the westphalia_sovereignty kernel: absolute_non_intervention (sovereignty as categorical inviolability, near-zero adjudicative transfer, victims are target-state governments only), conditional_responsibility (this story — sovereignty conditioned on atrocity-prevention performance, moderate-high adjudicative transfer, victims include atrocity populations and weak states), and graded_sovereignty (sovereignty as scalar state capacity, intervention calibrated to capacity deficits rather than atrocity occurrence, victims are states classified as low-capacity regardless of atrocity). Each carries a distinct ε and distinct victim/beneficiary structure and is authored as a separate file per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__conditional_responsibility, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
