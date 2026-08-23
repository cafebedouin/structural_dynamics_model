% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Dueling as Legitimate Honor Settlement (Composite Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   The legitimacy of dueling as the authorized mechanism for settling honor
 *   disputes among European and American elites (1600-1900) functioned as a
 *   tangled rope: it genuinely coordinated status conflicts (preventing feud,
 *   channeling violence) while extracting disproportionate costs from
 *   subordinates and commoners. The composite reading of its decline holds
 *   that cultural contraction — the cognitive unthinkability of dueling as
 *   honor satisfaction — was the dominant pathway, but it was reinforced and
 *   accelerated by state monopoly consolidation, professionalization of
 *   military and legal institutions, bourgeois redefinition of honor, and the
 *   internal contradictions of the honor code itself. No single pathway would
 *   have sufficed; their convergence overdetermined the outcome.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.78).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.82).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.87).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Dueling as Legitimate Honor Settlement (Composite Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '5d462769-d5df-4799-b586-726e6dcdcf06').
narrative_ontology:cs_kernel_codification('5d462769-d5df-4799-b586-726e6dcdcf06', distributed).
narrative_ontology:cs_authority_grounding('5d462769-d5df-4799-b586-726e6dcdcf06', practice).
narrative_ontology:cs_interpretation_layer_present('5d462769-d5df-4799-b586-726e6dcdcf06').
narrative_ontology:cs_reading_relation('5d462769-d5df-4799-b586-726e6dcdcf06', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d462769-d5df-4799-b586-726e6dcdcf06', honor_settlement_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_axiom('5d462769-d5df-4799-b586-726e6dcdcf06', foundational, decline_overdetermined_by_converging_pathways).
narrative_ontology:cs_axiom_status(decline_overdetermined_by_converging_pathways, holdable).
narrative_ontology:cs_axiom_grounding('5d462769-d5df-4799-b586-726e6dcdcf06', decline_overdetermined_by_converging_pathways, empirically_contingent).
narrative_ontology:cs_axiom('5d462769-d5df-4799-b586-726e6dcdcf06', foundational, cultural_contraction_dominates_but_requires_reinforcement).
narrative_ontology:cs_axiom_status(cultural_contraction_dominates_but_requires_reinforcement, holdable).
narrative_ontology:cs_axiom_grounding('5d462769-d5df-4799-b586-726e6dcdcf06', cultural_contraction_dominates_but_requires_reinforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('5d462769-d5df-4799-b586-726e6dcdcf06', classical_honor_settlement).
narrative_ontology:cs_drift_state('5d462769-d5df-4799-b586-726e6dcdcf06', post_enlightenment_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5d462769-d5df-4799-b586-726e6dcdcf06', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, aristocracy).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, military_officers).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, commoners).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, lower_status_men).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, honor_bound_subordinates).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, honor_requires_violent_redress).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, aristocratic_privilege_entails_dueling_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defined and enforced the codes of honor that made dueling the legitimate settlement mechanism. Benefited from a system that reserved honor satisfaction to their class and used dueling to police status boundaries. Could exit by refusing duels with social inferiors without honor loss.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, aristocracy, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, aristocracy, beneficiary).

% Adopted dueling as the institutional mechanism for officer honor disputes. The military honor code made refusal professionally fatal. Benefited from a system that converted personal courage into professional credibility. Exit meant resignation or court-martial for insubordination to the code.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, military_officers, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, military_officers, beneficiary).

% Could be challenged to duel by social superiors with no honorable refusal. Lacked standing to issue challenges themselves. The constraint extracted risk of death or injury without offering the status benefits it conferred on elites. Legal prohibition of dueling applied asymmetrically — commoners prosecuted, aristocrats often pardoned.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, commoners, payer,
    powerless, immediate, trapped, local).

% Aspirants to honor culture (merchant class, professionals, minor gentry) who adopted dueling to claim status but bore disproportionate costs. Could not access the same social forgiveness as aristocracy. Exit required abandoning honor claims entirely — social death rather than physical death.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, lower_status_men, payer,
    moderate, biographical, constrained, regional).

% Servants, junior officers, sons of aristocrats whose identity was fused to the honor code. Their self-concept made exit unthinkable — refusing a duel meant ontological collapse, not just social sanction. The constraint extracted their lives through their own internalized commitment.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, honor_bound_subordinates, payer,
    powerless, immediate, identity_locked, local).

% Initially tolerated and regulated dueling (issuing permits, setting rules) as a pressure valve for aristocratic violence. Shifted to suppression as state monopoly on violence consolidated. The constraint's legitimacy derived partly from state acquiescence; withdrawal of that acquiescence was a key material pathway in its decline.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_legal_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, state_legal_authorities, observer).

% Structurally excluded from the honor system — could not issue or accept challenges, yet bore costs (widowhood, orphaned children, social ruin of male relatives). Their honor was mediated through male relatives; the constraint's operation rendered them invisible subjects of its extraction.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, women, excluded,
    powerless, biographical, trapped, national).

% Philosophers, jurists, and reformers who attacked dueling as barbaric, irrational, and anti-civic. Provided the intellectual framework for cultural contraction. Their exit was analytical — they stood outside the honor culture and measured it against universal reason.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, enlightenment_critics, observer,
    organized, generational, analytical, continental).

% Analyze the constraint's structure, decline, and competing explanations. This reading (composite) synthesizes cultural, material, and institutional pathways without reducing to any single cause.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a bounded, ritualized mechanism for resolving status disputes among honor-claimants without escalating to feud or legal process. Solved the coordination problem of 'how to settle honor claims without destroying the social order' by channeling violence into a rule-governed duel.
% TRANSFER_FUNCTION: Transferred risk of death and injury from aristocrats and officers (who gained status from participation) to subordinates and commoners (who bore the physical costs). Transferred social legitimacy from state courts to private violence. Transferred honor capital from the challenged to the victor.
% ABSENT_VOICES: Women, who were the structural excluded — their honor was the object of male duels but they had no voice in the code. Enslaved and colonized peoples, for whom the honor system was a tool of domination. The poor, who could not afford seconds, pistols, or the social capital to make dueling a status claim rather than a death sentence.
% DISAPPEARANCE_RATIONALE: When dueling's legitimacy collapsed, the entire architecture of aristocratic honor, military officer culture, and gentlemanly status had to reorganize. Legal systems absorbed dispute resolution. New status markers (wealth, merit, bureaucratic rank) replaced honor capital. The constraint's disappearance rearranged the social order it had structured.
% FOUNDING_PROBLEM: How to contain aristocratic and military violence within a ritualized form that prevented feud, protected the state's monopoly on legitimate force, and allowed elite men to demonstrate courage without destroying each other or the social fabric.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary state builders (Frederick the Great, Napoleon's legal reformers, Victorian jurists) explicitly documented the founding problem as solved by other means — courts, police, professional armies. The honor culture's own apologists (e.g., German Burschenschaften, Southern planters) admitted the founding problem was obsolete but insisted the ritual remained binding as identity. Corroboration comes from outside the beneficiary set: state archives, military reform records, feminist historiography of honor violence.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness peaks mid-period (0.83 c. 1800) when dueling is most entrenched as elite privilege but state tolerance creates asymmetric enforcement. Suppression requirement rises as the state builds capacity to enforce its monopoly on violence (peaking 0.88 c. 1830-1850) then falls as the constraint becomes culturally unthinkable. Theater ratio rises monotonically — the performative 'satisfaction' of honor increasingly decouples from any functional dispute resolution. Accessibility collapse is near-total: once a man entered the honor culture, legal recourse, apology, and withdrawal were structurally closed. Resistance is moderate — state, church, and Enlightenment critics resisted but could not dismantle the constraint until cultural contraction took hold.
 *
 * PERSPECTIVAL GAP:
 *   From the aristocrat's seat, dueling is a rope — genuine coordination of honor disputes among equals with minimal coercion. From the subordinate's seat, it is a snare — extraction enforced by identity-lock and asymmetric law. The engine computes this divergence from the structural data; the composite reading does not adjudicate it but explains how the constraint's decline reshaped both seats simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocracy and military officers are structural beneficiaries (d ~ 0.1-0.2): they set the rules, collect status rents, and have arbitrage-grade exit (refuse inferiors, emigrate, use state protection). Commoners and lower-status men are targets (d ~ 0.8-0.9): trapped or constrained exit, bear physical costs. Honor-bound subordinates are identity-locked targets (d ~ 0.95): their self-concept fuses to the constraint, making exit ontologically impossible. State authorities shift from agenda-setter (d ~ 0.3, tolerating/regulating) to suppressor (d ~ 0.7, enforcing prohibition) as monopoly on violence consolidates. Women are excluded (d undefined): structurally invisible to the constraint's operation but bearing its externalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (containing elite violence) was solved by state institutions by 1800, but the constraint persisted for a century as mandatrophy — the ritual outlived its function because it had become identity-constitutive for the officer corps and aristocracy. The composite reading prevents mislabeling the coordination function as pure extraction (it was real, historically) and prevents mislabeling the extraction as mere coordination cost (it was asymmetric and lethal). The overdetermination thesis shows how mandatrophy interacts with cultural contraction: the constraint persisted not because it worked but because multiple reinforcing mechanisms made exit from the constraint itself structurally impossible until all pathways converged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the composite_reading a distinct structural constraint or a meta-synthesis of contraction_reading and drop_reading?',
    'Test whether the composite reading makes predictions that neither sibling makes alone — e.g., the timing of suppression intensification should correlate with state capacity growth, not just cultural discourse shifts. If the composite predicts interaction effects the siblings miss, it is structurally distinct.',
    'If the composite is not structurally distinct, it should be modeled as a network edge between the two sibling constraints rather than a separate constraint story. The engine''s contamination analysis would then propagate drift across the network.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the composite reading instantiates a separate constraint or is an analytical overlay on the sibling constraints.').

omega_variable(
    contraction_vs_material_primacy,
    'Was cultural contraction the dominant pathway, or did material/institutional changes (state monopoly, professionalization) do the causal heavy lifting with cultural change as epiphenomenon?',
    'Counterfactual historical modeling: if cultural frameworks had shifted but state capacity remained weak (e.g., no professional police, no standing army), would dueling have declined? Compare regions with similar cultural shifts but divergent state capacity (e.g., German lands vs. American frontier).',
    'If material factors are primary, the composite reading''s ''contraction edge'' is misattributed — the constraint would classify as snare (extraction maintained by state failure) rather than tangled_rope with cultural collapse. If cultural factors are primary, the tangled_rope classification holds but the decline trajectory is endogenous to the constraint''s meaning-structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contraction_vs_material_primacy, empirical, 'Causal primacy among the converging pathways in the composite reading.').

omega_variable(
    coordination_extraction_boundary_historical,
    'Did dueling ever function as genuine coordination (rope) for its participants, or was the coordination story always cover for extraction (snare)?',
    'Analyze dispute outcomes in periods of high vs. low state capacity. If duels actually prevented feud and produced stable settlements when courts were weak, coordination was real. If feud rates were unchanged and duels merely added ritual to existing violence, coordination was illusory.',
    'If coordination was always illusory, the claimed_type should be snare, not tangled_rope. The engine''s false_summit_mountain signature does not apply (not claimed as mountain), but the tangled_rope gate (requires_active_enforcement + beneficiaries + victims) would still fire — the classification would shift on the coordination dimension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_historical, empirical, 'Whether the constraint''s coordination function was genuine or performative throughout its history.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression of dueling refusal structural (legal penalties, social ostracism) or internalized (identity-locked honor culture making refusal unthinkable)?',
    'Post-exit suppression trajectory: examine men who refused duels or emigrated to non-dueling cultures. If suppression (shame, ostracism) persisted after exit, reclassify as partially internalized. Compare officer corps (identity-locked) vs. civilian aristocrats (more structural).',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression with them. This affects the directionality derivation for honor_bound_subordinates (identity_locked exit → d near 1.0) and the theater_ratio interpretation (performative maintenance of internalized code).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dueling refusal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hsl_comp_tr_t1600, honor_settlement_legitimacy__composite_reading, theater_ratio, 1600, 0.35).
narrative_ontology:measurement(hsl_comp_tr_t1650, honor_settlement_legitimacy__composite_reading, theater_ratio, 1650, 0.38).
narrative_ontology:measurement(hsl_comp_tr_t1700, honor_settlement_legitimacy__composite_reading, theater_ratio, 1700, 0.42).
narrative_ontology:measurement(hsl_comp_tr_t1750, honor_settlement_legitimacy__composite_reading, theater_ratio, 1750, 0.48).
narrative_ontology:measurement(hsl_comp_tr_t1800, honor_settlement_legitimacy__composite_reading, theater_ratio, 1800, 0.55).
narrative_ontology:measurement(hsl_comp_tr_t1830, honor_settlement_legitimacy__composite_reading, theater_ratio, 1830, 0.62).
narrative_ontology:measurement(hsl_comp_tr_t1860, honor_settlement_legitimacy__composite_reading, theater_ratio, 1860, 0.71).
narrative_ontology:measurement(hsl_comp_tr_t1900, honor_settlement_legitimacy__composite_reading, theater_ratio, 1900, 0.95).

% Extraction over time
narrative_ontology:measurement(hsl_comp_be_t1600, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1600, 0.65).
narrative_ontology:measurement(hsl_comp_be_t1650, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1650, 0.72).
narrative_ontology:measurement(hsl_comp_be_t1700, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1700, 0.78).
narrative_ontology:measurement(hsl_comp_be_t1750, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1750, 0.81).
narrative_ontology:measurement(hsl_comp_be_t1800, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1800, 0.83).
narrative_ontology:measurement(hsl_comp_be_t1830, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1830, 0.79).
narrative_ontology:measurement(hsl_comp_be_t1860, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1860, 0.65).
narrative_ontology:measurement(hsl_comp_be_t1900, honor_settlement_legitimacy__composite_reading, base_extractiveness, 1900, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hsl_comp_su_t1600, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1600, 0.55).
narrative_ontology:measurement(hsl_comp_su_t1650, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1650, 0.62).
narrative_ontology:measurement(hsl_comp_su_t1700, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1700, 0.71).
narrative_ontology:measurement(hsl_comp_su_t1750, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1750, 0.78).
narrative_ontology:measurement(hsl_comp_su_t1800, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1800, 0.85).
narrative_ontology:measurement(hsl_comp_su_t1830, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1830, 0.88).
narrative_ontology:measurement(hsl_comp_su_t1860, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1860, 0.72).
narrative_ontology:measurement(hsl_comp_su_t1900, honor_settlement_legitimacy__composite_reading, suppression_requirement, 1900, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__composite_reading, 0.08).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, state_monopoly_violence_consolidation).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, bourgeois_honor_redefinition).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__composite_reading, military_professionalization).

% DUAL FORMULATION NOTE:
% Kernel honor_settlement_legitimacy decomposes into three readings: composite (this), contraction, drop. The composite reading synthesizes cultural contraction (dominant) with material/institutional reinforcement. Contraction_reading isolates the cultural pathway; drop_reading isolates residual persistence. All three share the same referent (the standing arrangement of dueling legitimacy) but instantiate different constraints with different ε values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_settlement_legitimacy__composite_reading, organized, 0.15).
constraint_indexing:directionality_override(honor_settlement_legitimacy__composite_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
