% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: The Nuclear Taboo (Constructed Normative Prohibition Reading)
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This story instantiates the nuclear-taboo reading of the
 *   total_war_possibility_space kernel: total war remains materially
 *   reachable — the weapons exist, targeting plans exist, capability has not
 *   degraded — but use has become normatively unthinkable through a
 *   constructed taboo that developed independently of the underlying material
 *   balance. The taboo is treated here as itself a constraint with
 *   coordination and extraction dimensions: it stabilizes mutual restraint
 *   (genuine coordination function) while also entrenching the relative
 *   position of existing nuclear powers and the institutional standing of
 *   norm-entrepreneur communities (asymmetric extraction). This is a distinct
 *   constraint from its siblings — the deterrence_equilibrium_reading (where
 *   restraint is explained by mutual vulnerability, not norm) and the
 *   space_contraction_reading (where total war has left the strategically
 *   thinkable altogether, not merely the normatively permissible). Each
 *   reading has a different ε, a different victim set, and different
 *   predicted fragility conditions; they are linked by
 *   network.affects_constraints, not merged.
 *
 * KEY AGENTS:
 *   - existing_nuclear_weapon_states: primary beneficiary and co-agenda-setter (institutional/arbitrage) — collects reputational and hierarchical benefit from the taboo's persistence
 *   - norm_entrepreneur_institutions: co-agenda-setter and beneficiary (organized/mobile) — collects institutional relevance from taboo maintenance
 *   - aspiring_proliferator_states: primary target (moderate/constrained) — bears sanctions and isolation costs for taboo violation
 *   - non_nuclear_states_under_extended_deterrence: secondary payer/beneficiary (moderate/constrained) — trades autonomy for shelter under a bargain partly grounded in the taboo
 *   - military_planning_establishments: excluded voice (institutional/constrained) — maintains contradicting operational reality, rarely surfaces publicly
 *   - historians_of_the_taboo: analytical observer (analytical) — documents construction and contestation of the norm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.42).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.58).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "The Nuclear Taboo (Constructed Normative Prohibition Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '88876541-9f43-4517-9c3e-16e0882a31f6').
narrative_ontology:cs_kernel_codification('88876541-9f43-4517-9c3e-16e0882a31f6', distributed).
narrative_ontology:cs_authority_grounding('88876541-9f43-4517-9c3e-16e0882a31f6', distributed).
narrative_ontology:cs_reading_relation('88876541-9f43-4517-9c3e-16e0882a31f6', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('88876541-9f43-4517-9c3e-16e0882a31f6', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('88876541-9f43-4517-9c3e-16e0882a31f6', foundational, normative_prohibition_independent_of_capability).
narrative_ontology:cs_axiom_status(normative_prohibition_independent_of_capability, holdable).
narrative_ontology:cs_axiom_grounding('88876541-9f43-4517-9c3e-16e0882a31f6', normative_prohibition_independent_of_capability, conventional).
narrative_ontology:cs_axiom('88876541-9f43-4517-9c3e-16e0882a31f6', secondary, taboo_requires_active_norm_entrepreneur_maintenance).
narrative_ontology:cs_axiom_status(taboo_requires_active_norm_entrepreneur_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('88876541-9f43-4517-9c3e-16e0882a31f6', taboo_requires_active_norm_entrepreneur_maintenance, empirically_contingent).
narrative_ontology:cs_reference_frame('88876541-9f43-4517-9c3e-16e0882a31f6', post_hiroshima_prohibition_norm).
narrative_ontology:cs_drift_state('88876541-9f43-4517-9c3e-16e0882a31f6', post_cold_war_arms_control_fatigue, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88876541-9f43-4517-9c3e-16e0882a31f6', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, existing_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_institutions).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, aspiring_proliferator_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states_under_extended_deterrence).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states_under_extended_deterrence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess weapons already and benefit from a norm that stigmatizes further acquisition and any use, which locks in their relative position at the top of the arms hierarchy while the taboo's moral language is framed as universal rather than status-preserving. They administer the non-proliferation architecture (NPT, export controls, IAEA safeguards) that operationalizes the taboo and can adjust its terms.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, existing_nuclear_weapon_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, existing_nuclear_weapon_states, agenda_setter).

% Anti-nuclear movements, arms control epistemic communities, and disarmament-focused NGOs built and continually renarrate the taboo through advocacy, scholarship, and diplomatic pressure. Their institutional relevance and funding depend on the taboo remaining a live, contested norm requiring active defense; they collect legitimacy and resources from being its custodians.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_institutions, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_institutions, agenda_setter).

% States that might otherwise acquire nuclear weapons for security reasons face sanctions, diplomatic isolation, and reputational costs enforced through the non-proliferation regime. Their exit from the taboo (withdrawing from the NPT, testing openly) triggers coordinated punitive responses; the norm is materially binding on them even though it constrains behavior rather than capability directly.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, aspiring_proliferator_states, payer,
    moderate, biographical, constrained, national).

% States sheltering under an allied nuclear umbrella accept dependence on a patron's arsenal and abstain from developing their own, trading strategic autonomy for security guarantees whose credibility rests partly on the very taboo that makes actual use appear unthinkable. If the taboo erodes, their bargain changes with it, but they have no independent lever to reinforce it.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states_under_extended_deterrence, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states_under_extended_deterrence, beneficiary).

% War-planning and targeting communities within nuclear states continue to maintain operational plans for nuclear employment and periodically argue internally that the taboo is a civilian political constraint layered atop unchanged military capability. Their institutional perspective — that the weapons remain usable tools of strategy — rarely surfaces in the public normative discourse that sustains the taboo's authority.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, military_planning_establishments, excluded,
    institutional, biographical, constrained, national).

% Scholars (Tannenwald and successors) trace how non-use since 1945 became retrospectively narrated as evidence of a taboo rather than of deterrence or lack of occasion, and document the specific historical episodes (Korea, Vietnam, Gulf War restraint debates) where the norm was invoked to foreclose options that were materially available.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, historians_of_the_taboo, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The taboo functions as a coordination device that stabilizes mutual restraint without requiring perfect verification of capability or intent — states can trust that non-use is a normative floor rather than merely a contingent calculation, reducing the need for costly continuous reassurance signaling.
% TRANSFER_FUNCTION: Moves strategic latitude and reputational standing from states seeking to acquire or use nuclear weapons to the states and institutions that already possess them or that administer the norm; a state's compliance with the taboo becomes a precondition for full standing in the international system administered by existing powers.
% ABSENT_VOICES: Military planning establishments whose operational doctrines contradict the taboo's premise are structurally excluded from the normative discourse; officials of proliferator or would-be-proliferator states who see the taboo as a great-power cartel dressed in moral language are heard mainly as norm-violators, not as parties to a negotiation.
% DISAPPEARANCE_RATIONALE: Existing nuclear states and norm entrepreneurs would say the world rearranges catastrophically if the taboo vanished — nuclear use would return to the menu of ordinary strategic options. Skeptics (including some military planners and this reading's sibling readings) would say the material deterrence structure underneath the taboo is what actually restrains use, and the taboo's disappearance would change rhetoric more than behavior. The kernel contest is precisely this disagreement.
% FOUNDING_PROBLEM: After 1945, the demonstrated destructive capacity of nuclear weapons created an urgent need to prevent normalization of their use in ordinary interstate conflict, given that deterrence alone could not be verified to hold in every crisis and a single miscalculated use could be catastrophic and irreversible.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the taboo and several retired military planners (writing outside their institutional roles) attest that non-use has held for eight decades and that explicit normative language demonstrably shaped decision-making in specific documented crises (e.g., Eisenhower administration debates over Korea, U.S. planning in the Gulf War). Skeptical corroboration exists too: declassified planning documents show live employment options were retained throughout, suggesting the founding problem was addressed at least partly by capability constraints and alliance structures rather than norm internalization alone — this is exactly the material the sibling readings claim as their own evidence.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the taboo genuinely reduces the salience of nuclear war as an ordinary policy tool (real coordination value) but also functions to lock existing nuclear powers into permanent normative high ground while denying the same latitude to aspirants — a status-preserving asymmetry. Suppression (0.58) reflects the substantial enforcement apparatus (sanctions regimes, export control cartels, diplomatic isolation of violators) required to keep the norm binding on states that retain the material capacity to defect. Theater ratio (0.31) captures a real but growing performative element: rhetorical restatements of the taboo (summit declarations, review conferences) increasingly substitute for material disarmament progress. Accessibility collapse is moderate (0.5) — unlike a mountain, alternatives to the taboo (open proliferation, explicit deterrence-only doctrine) remain conceptually available and are actively argued for by identifiable factions, which is exactly what distinguishes this reading from a natural-law framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing nuclear states and norm-entrepreneur institutions sit near the beneficiary end: the taboo's persistence preserves their relative status and institutional purpose respectively, and their exit options (arbitrage, mobile) let them shape or exit the discourse at low cost. Aspiring proliferators sit near the target end: the taboo is materially binding on them via sanctions and isolation despite their being non-signatories to its normative premises in some cases. Non-nuclear states under extended deterrence occupy a genuinely mixed position — they benefit from the security umbrella the taboo helps stabilize but pay through permanent strategic dependency; the derivation correctly places them near the midpoint rather than at either extreme.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing normalization of catastrophic, potentially civilization-ending weapons use) remains partially live — no state has used a nuclear weapon in anger since 1945, which corroborators cite as ongoing function. But the taboo also increasingly does work unrelated to that founding purpose: it now serves as a status marker distinguishing legitimate from illegitimate nuclear possession (the NPT's five-power carve-out), which was not part of the original prevention-of-use rationale. Classifying this as tangled_rope rather than pure rope or pure mountain prevents two mislabeling errors: treating the taboo as costless pure coordination (ignoring its status-preserving asymmetry) and treating it as a natural law immune to erosion (ignoring that it is a historically constructed, actively defended norm that could weaken if norm entrepreneurs exit or nuclear states stop investing in its maintenance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_vs_deterrence_causal_attribution,
    'In documented crisis decisions where nuclear use was foregone, was the operative constraint the normative taboo or the underlying material deterrence calculation (mutual vulnerability, escalation risk)?',
    'Close reading of declassified decision records (e.g., Korea 1950-53, Cuban Missile Crisis, Gulf War planning) for whether decision-makers invoked normative/taboo language as binding independent of the military assessment, or whether normative language was post-hoc rationalization of a decision already reached on capability/escalation grounds.',
    'If normative language consistently trails rather than drives the decision, this reading''s core claim (norm independent of capability) weakens and the deterrence_equilibrium_reading gains support as the better account of the same non-use record.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_causal_attribution, empirical, 'Whether historical non-use is best explained by taboo or by material deterrence.').

omega_variable(
    norm_entrepreneur_exit_fragility,
    'How much of the taboo''s continued force depends on active maintenance by identifiable norm-entrepreneur institutions, versus having become self-sustaining through diffuse socialization?',
    'Track taboo strength (measured via elite rhetoric, near-use crisis behavior, arms control participation) in periods of norm-entrepreneur institutional decline (e.g., post-Cold War arms control fatigue, funding contraction for disarmament NGOs) versus periods of high entrepreneur activity.',
    'If taboo strength tracks entrepreneur activity closely, the constraint is fragile and reversible on a biographical timescale; if it persists through entrepreneur decline, it has become more mountain-like (self-sustaining norm) than this reading''s tangled_rope framing suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(norm_entrepreneur_exit_fragility, empirical, 'Whether the taboo is entrepreneur-dependent or self-sustaining.').

omega_variable(
    asymmetric_application_as_legitimation_crisis,
    'Does the taboo''s asymmetric application (binding aspirants harshly, tolerating existing arsenals and their modernization) undermine its normative legitimacy over time, or is the asymmetry stable because it is grounded in a separate non-proliferation logic rather than the use-taboo itself?',
    'Comparative analysis of rhetoric from aspiring and existing nuclear states over time; survey evidence on whether non-nuclear-state publics and elites perceive the NPT/taboo regime as principled or as a great-power cartel.',
    'If perceived as cartel logic rather than principled prohibition, the coordination-function claim underlying the tangled_rope classification weakens, pushing the constraint toward snare; if the two logics (use-taboo vs. possession-hierarchy) are seen as genuinely separable, tangled_rope classification is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetric_application_as_legitimation_crisis, conceptual, 'Whether asymmetric enforcement erodes or is independent of the taboo''s coordination legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1962, 0.15).
narrative_ontology:measurement(tota_tr_t1975, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(tota_tr_t1995, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1995, 0.26).
narrative_ontology:measurement(tota_tr_t2010, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2010, 0.29).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2025, 0.31).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1962, 0.28).
narrative_ontology:measurement(tota_be_t1975, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1975, 0.33).
narrative_ontology:measurement(tota_be_t1995, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(tota_be_t2010, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1962, 0.48).
narrative_ontology:measurement(tota_su_t1975, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1975, 0.5).
narrative_ontology:measurement(tota_su_t1995, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(tota_su_t2010, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2010, 0.57).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, space_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the total_war_possibility_space kernel. deterrence_equilibrium_reading holds that total war remains strategically reachable but is deterred by mutual material vulnerability (MAD), assigning restraint to capability-balance rather than norm. space_contraction_reading holds that nuclear weapons removed total war from the strategically thinkable altogether — a stronger claim than mere prohibition, closer to an epistemic/cognitive constraint than a normative one. This reading (nuclear_taboo_reading) holds the middle position: capability and thinkability both remain intact, but use is normatively foreclosed by a historically constructed taboo that operates independently of the material balance. Each reading predicts different fragility conditions and would assign a different ε and victim structure to the same underlying non-use record; they are not measurement variants of one constraint but three structurally distinct claims linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
