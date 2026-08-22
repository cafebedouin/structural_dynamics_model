% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Post-1945 Normative Illegitimacy of Total War
 *   domain: international/political
 *
 * SUMMARY:
 *   This reading asserts that total war remained physically possible after
 *   1945 but became normatively illegitimate through Article 2(4) of the UN
 *   Charter, the development of international humanitarian law, and the
 *   institutional machinery (ICC, war crimes tribunals, UN enforcement
 *   actions) that codified and applied these norms. States retain the
 *   organizational and material capacity to wage wars of extermination
 *   against civilian populations, but the cost structure has shifted: openly
 *   doing so triggers diplomatic isolation, legal accountability, and
 *   coalition response. The constraint is maintained through treaty
 *   enforcement, normative socialization, and institutional adjudication.
 *   This reading DOES NOT claim that total war is physically impossible (that
 *   is the structural_contraction_reading, which attributes constraint to
 *   nuclear weapons). It DOES NOT claim that strategic culture spontaneously
 *   evolved away from total war (that is the
 *   strategic_culture_drift_reading). Rather, it claims the normative-legal
 *   framework CREATED the shift by making total war illegitimate in elite and
 *   public discourse. Extractiveness is moderate because revisionist powers
 *   are constrained but not extracted from in a direct-gain sense; the
 *   beneficiaries (civilian populations) collect protection, not payment.
 *
 * KEY AGENTS:
 *   - Global civilian populations: protected from total war through treaty-based norms; powerless but beneficiaries of the constraint
 *   - Liberal democratic states: agenda-setters and enforcers; benefit from rule-based order that constrains challenger states within manageable bounds
 *   - Revisionist powers: constrained by norms they might strategically prefer to violate; pay the cost of restricted strategy repertoire
 *   - International legal institutions: interpreters and enforcers of the normative frame; maintain legitimacy of the constraint through jurisprudence
 *   - Non-aligned and rising powers: beneficiaries of protection but also constrained; subject to rules they had less power to shape
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.28).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.42).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.28).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Post-1945 Normative Illegitimacy of Total War").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international/political").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, 'a1385128-dfbb-4a51-9779-f6e999e92e16').
narrative_ontology:cs_kernel_codification('a1385128-dfbb-4a51-9779-f6e999e92e16', formalized).
narrative_ontology:cs_authority_grounding('a1385128-dfbb-4a51-9779-f6e999e92e16', lineage).
narrative_ontology:cs_interpretation_layer_present('a1385128-dfbb-4a51-9779-f6e999e92e16').
narrative_ontology:cs_reading_relation('a1385128-dfbb-4a51-9779-f6e999e92e16', total_war_winnability_post1945__structural_contraction_reading, influences).
narrative_ontology:cs_reading_relation('a1385128-dfbb-4a51-9779-f6e999e92e16', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('a1385128-dfbb-4a51-9779-f6e999e92e16', foundational, norms_delegitimize_total_war).
narrative_ontology:cs_axiom_status(norms_delegitimize_total_war, holdable).
narrative_ontology:cs_axiom_grounding('a1385128-dfbb-4a51-9779-f6e999e92e16', norms_delegitimize_total_war, conventional).
narrative_ontology:cs_axiom('a1385128-dfbb-4a51-9779-f6e999e92e16', foundational, treaty_enforcement_sustains_aversion).
narrative_ontology:cs_axiom_status(treaty_enforcement_sustains_aversion, holdable).
narrative_ontology:cs_axiom_grounding('a1385128-dfbb-4a51-9779-f6e999e92e16', treaty_enforcement_sustains_aversion, instrumental).
narrative_ontology:cs_reference_frame('a1385128-dfbb-4a51-9779-f6e999e92e16', humanitarian_constraint_via_treaty).
narrative_ontology:cs_drift_state('a1385128-dfbb-4a51-9779-f6e999e92e16', contemporary_revisionist_challenge_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a1385128-dfbb-4a51-9779-f6e999e92e16', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, international_legal_community).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_powers_constrained_by_norms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, non_aligned_states).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, non_aligned_states).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, human_rights_primacy_doctrine).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, collective_security_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain protection from total war doctrine through international legal norms that delegitimize existential warfare against entire populations. The constraint makes total war socially and legally indefensible; states still possess the physical capability but face overwhelming diplomatic, legal, and reputational costs to deployment. Civilians cannot exit the constraint's protection—they are protected by its persistence regardless of their preference.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    powerless, generational, trapped, global).

% Established and enforce the treaty framework (UN Charter Article 2(4), Geneva Conventions, later humanitarian protocols). They set the normative standard, interpret its scope, and coordinate its enforcement through international institutions. They benefit from a rule-based system that makes their dominance durable and constrains challenger strategies within bounds they can manage.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, liberal_democratic_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Possess the physical capacity to wage total war but face near-complete normative bar to exercising it. They bear the cost of strategic constraint: total war is no longer an available option in their calculus, even when it might be militarily or economically advantageous. The constraint does not prevent their military action, but it narrows the repertoire of acceptable strategies and raises the cost of any strategy perceived as violating the norm.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_powers_constrained_by_norms, payer,
    powerful, biographical, constrained, global).

% Benefit from protection against total war (especially from great powers), but also face constraints on their own strategic freedom. They are nominally bound by the same norms but have less enforcement capacity and less ability to interpret or modify the rules. Their compliance is expected; their breach would be costly, but they lack the power to rewrite the framework.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, non_aligned_states, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, non_aligned_states, payer).

% Interpret and apply the normative framework through courts, tribunals, and investigative bodies (ICC, ICTY, UN fact-finding missions). They enforce the constraint's legitimacy through accountability mechanisms, doctrinal development, and jurisprudence. Their rulings shape what counts as total war and what violates humanitarian law.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_legal_institutions, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, international_legal_institutions, observer).

% Analyze whether total war remains strategically rational and reachable. They witness the constraint operating as a normative structure: states retain the physical and organizational capacity for total war, but the cost structure has shifted so dramatically that it is no longer a rational choice. They can articulate the constraint's operation without being subject to it.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, military_strategists_and_theorists, observer,
    analytical, biographical, analytical, global).

% Are structurally excluded from the treaty framework—they did not sign the UN Charter and are not states. Humanitarian law applies to their conduct formally, but enforcement is asymmetric and enforcement capacity against them is embedded in the constraint's operation. They are subject to its norms but not party to its creation or interpretation.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, insurgent_and_non_state_actors, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__normative_reading_drop, international_legal_institutions).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__normative_reading_drop, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of preventing mutual escalation into wars of annihilation: all states gain from an agreement that total war is off-limits because each fears being on the receiving end. The constraint creates a shared understanding that wars of survival are exceptions that trigger collective response, not normal state behavior.
% TRANSFER_FUNCTION: Moves strategic freedom from revisionist powers (who lose the option to wage total war) to global civilian populations and the international community (who gain protection and legitimacy grounds for intervention). The transfer is asymmetric: powerful states with total war capacity bear the larger constraint cost.
% ABSENT_VOICES: Rising powers that might view total war as a viable option for rapid hegemonic shift are structurally excluded from the rulemaking table. Non-state actors and populations in conflict zones are nominally protected by but not party to the system. Historians and strategic analysts who argue total war remains physically reachable and may become strategically rational again are marginalized in mainstream policy discourse.
% DISAPPEARANCE_RATIONALE: If the normative constraint on total war collapsed—if states returned to treating wars of survival as legitimate—military planning would immediately shift toward maximal force deployment against civilian populations, arms control agreements would unravel, and international institutions would either collapse or be captured by revisionist coalitions. The stability of the current state system depends on the constraint's persistence.
% FOUNDING_PROBLEM: Post-WWII, the international community faced total war as an existential threat: the Holocaust, firebombing of civilian cities, atomic weapons use. The founding problem was how to prevent great powers from ever again deploying industrial capacity for complete annihilation of adversary populations and economies.
% FOUNDING_PROBLEM_CORROBORATION: Liberal democratic states and international legal scholars attest the founding problem is live: international law and humanitarian norms are necessary to contain great-power conflict. Realist scholars and rising powers argue the founding problem is solved by nuclear deterrence and economic interdependence, not by norms. Strategic culture analysts dispute whether the problem was solved by normative internalization or by structural constraints (nuclear weapons) that happened to align with post-war treaty development.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.28 (moderate) because the constraint does impose real costs on revisionist powers—they lose access to a strategy that might otherwise be rational—but the cost is not a direct transfer of resources. Instead it is strategic constraint: the payoff from total war is reduced to near-zero by the reputational, legal, and military costs of deployment. Suppression is 0.42 because enforcement is real but not overwhelming: states have internalized the norm through decades of socialization, so formal enforcement is less visible than in pure snares. Theater is low (0.15) because the constraint's functional core (states genuinely believe total war is illegitimate) is robust; most enforcement activity is legitimate norm-maintenance, not theatrical performance. Accessibility_collapse is 0.68 because once the normative frame is understood, alternatives do collapse somewhat—a state openly advocating total war faces near-complete legitimacy loss—but the alternatives do not collapse completely: strategists can still articulate total war rationales in closed settings. Resistance is moderate (0.51) because rising powers and realist scholars actively push back against the constraint (some argue it is eroding), but the constraint persists because the beneficiary coalition (liberal democracies, international institutions, global civil society) is large and well-organized.
 *
 * PERSPECTIVAL GAP:
 *   From the liberal-democratic institutional seat, this is genuine coordination: a shared agreement to keep wars bounded saves everyone from mutual devastation. From the revisionist power seat, it is asymmetric constraint: the agreement binds them to a range of strategies that have legitimate military value, while the liberal democracies retain enforcement flexibility (they can claim necessity exceptions more credibly). From the civilian population seat, it is simple protection: norms work, but the protection is dependent on powerful states' willingness to enforce them. The engine computes these per-seat types from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Global civilian populations and liberal democracies sit at the beneficiary end (d near 0.2): they benefit from protection and rule stability. Revisionist powers sit at the target end (d near 0.8): they lose strategic options and face higher costs for norm violation. Non-aligned states sit near 0.5 (symmetric): they gain protection but also lose strategic flexibility. The asymmetry drives the classification: revisionists are more constrained than beneficiaries are empowered, but the beneficiary coalition is larger and more organized, so the constraint persists.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing total war) remains live and contested. Liberal democracies argue that vigilance is necessary: without sustained enforcement and norm development, rising powers would revert to total-war strategies. Realists argue the founding problem is solved by nuclear deterrence and economic interdependence, making the treaty framework epiphenomenal. This divergence is not mandatrophy but reading-level disagreement: the constraint cannot be mandatrophic until we resolve whether norms cause the constraint or merely name it. See the committer-frame omega for the resolution mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_vs_structural_causation,
    'Is total war illegitimate because states internalized humanitarian norms and Article 2(4), or because nuclear weapons made total war physically irrational (rendering the norms epiphenomenal)?',
    'Counterfactual analysis: would states in a non-nuclear world have adhered to the same norms? Observational study of how explicitly states justify non-use of total war in policy documents and military doctrine (norm-based vs. capability/deterrence-based justifications).',
    'If normative: the constraint is a genuine coordination solution and should classify as rope. If structural (nuclear deterrence): total war is not normatively illegitimate but physically impossible, and this reading would be misframed — the structural_contraction_reading would be correct instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_vs_structural_causation, conceptual, 'Whether norm internalization or structural constraint (nuclear weapons) is the primary driver of total war aversion.').

omega_variable(
    enforcement_asymmetry_sustainability,
    'Can the constraint persist when enforcement is asymmetric — strong against state actors, weak against rising powers and non-state actors?',
    'Track compliance drift over generational horizons; observe whether rising powers develop counter-norms (e.g., revisionist legal doctrines) or technological workarounds (e.g., cyber-warfare framed as below total-war threshold). Monitor enforcement selectivity in ICC/UN proceedings.',
    'If enforcement remains effectively symmetric and rising powers internalize the norm: rope persists. If enforcement degrades against rising powers or revisionist coalitions capture the definition: the constraint might weaken to piton (theater without real normative effect) or collapse entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_sustainability, empirical, 'Whether asymmetric enforcement undermines the normative constraint''s long-term stability.').

omega_variable(
    humanitarian_law_expansion_scope,
    'As humanitarian law expands to cover economic siege, information warfare, and climate-modifying weapons, does the constraint sharpen (becoming more restrictive) or blur (becoming less enforceable)?',
    'Doctrinal analysis of humanitarian law development; observe whether new categories of prohibited conduct strengthen or dilute the core total-war norm. Track state compliance with expanded protections (sieges, non-combatant targeting via information).',
    'If clarifying and strengthening: the constraint becomes more robust. If expanding so broadly that compliance becomes ambiguous: the constraint risks theater (states claim compliance while violating spirit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_law_expansion_scope, empirical, 'Whether expansion of humanitarian law scope strengthens or dilutes the total-war constraint.').

omega_variable(
    reading_committer_kernel_contest,
    'This constraint is one of three readings of the kernel ''total_war_winnability_post1945'': is this reading (normative_reading_drop) the correct account, or should one of the sibling readings (structural_contraction_reading or strategic_culture_drift_reading) replace it?',
    'Examine the causal chain in declassified military doctrine, policy statements, and strategic theory: (1) Does state non-use of total war follow from internalized norms (normative_reading_drop)? (2) Does it follow from nuclear deterrence making total war irrational regardless of norms (structural_contraction_reading)? (3) Does it follow from elite belief-system evolution independent of structure or formal law (strategic_culture_drift_reading)? Distinguish by observing which justification states cite for constraint adherence.',
    'Each reading produces a different constraint: normative_reading produces ε=0.28 (coordination cost of mutual constraint); structural_contraction produces ε≈0.0 (not a constraint, just naming the inevitable); strategic_culture produces ε medium-high (piton, maintained by theater/inertia). The classification path diverges sharply. This omega marks the reading-level underdetermination — a single observable kernel admitted three distinct readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_kernel_contest, conceptual, 'Which of three competing readings of the total_war_winnability_post1945 kernel is structurally correct: the normative reading (norms delegitimized it), the structural reading (nukes made it impossible), or the culture-drift reading (elites abandoned it via belief change)?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(tota_tr_t0, observed).
narrative_ontology:measurement(tota_tr_t10, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(tota_tr_t10, observed).
narrative_ontology:measurement(tota_tr_t20, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(tota_tr_t20, observed).
narrative_ontology:measurement(tota_tr_t30, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(tota_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(tota_be_t0, observed).
narrative_ontology:measurement(tota_be_t10, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 10, 0.31).
narrative_ontology:measurement_basis(tota_be_t10, observed).
narrative_ontology:measurement(tota_be_t20, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(tota_be_t20, observed).
narrative_ontology:measurement(tota_be_t30, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(tota_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(tota_su_t0, observed).
narrative_ontology:measurement(tota_su_t10, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(tota_su_t10, observed).
narrative_ontology:measurement(tota_su_t20, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 20, 0.43).
narrative_ontology:measurement_basis(tota_su_t20, observed).
narrative_ontology:measurement(tota_su_t30, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 30, 0.42).
narrative_ontology:measurement_basis(tota_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__normative_reading_drop, 0.12).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__strategic_culture_drift_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'total_war_winnability_post1945', which decomposes into three structurally distinct constraints depending on causal attribution: (1) normative_reading_drop (this file) — total war became illegitimate through treaty and norm development; (2) structural_contraction_reading — total war became impossible through nuclear weapons; (3) strategic_culture_drift_reading — elites abandoned total war via ideational evolution. Each reading has different ε (extractiveness), different beneficiaries, and different persistence mechanisms. The readings coexist as live positions in contemporary IR theory. See reading_relations in cs_structure for the logical structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
