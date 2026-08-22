% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__freedom_of_movement_reading, []).

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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Freedom of Movement Reading: Borders as Presumptively Illegitimate Restrictions
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_of_movement_reading of the
 *   border_legitimacy kernel. The reading asserts that freedom of movement is
 *   a human right (UDHR Art. 13, ICCPR Art. 12) and borders are presumptively
 *   illegitimate restrictions requiring specific, proportionate
 *   justification. The structural delta from the kernel's other readings is
 *   that current citizens (displaced workers, welfare beneficiaries) enter
 *   the victim set — they bear the distributive and fiscal costs of the
 *   reading's realization. Border enforcement is coded as extractive: it
 *   extracts mobility from migrants and compliance from citizens, sustaining
 *   a global apartheid of birthplace privilege. The claimed_type is
 *   tangled_rope because the reading performs genuine coordination (universal
 *   baseline for mobility claims) AND asymmetric extraction (citizens pay
 *   fiscal/distributive costs; enforcement personnel bear identity costs).
 *   The engine will compute per-seat classifications from the stakeholder
 *   surface.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.82).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.78).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Freedom of Movement Reading: Borders as Presumptively Illegitimate Restrictions").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, 'b95697e5-9a13-47f7-9821-056e77c8e0e4').
narrative_ontology:cs_kernel_codification('b95697e5-9a13-47f7-9821-056e77c8e0e4', formalized).
narrative_ontology:cs_authority_grounding('b95697e5-9a13-47f7-9821-056e77c8e0e4', lineage).
narrative_ontology:cs_interpretation_layer_present('b95697e5-9a13-47f7-9821-056e77c8e0e4').
narrative_ontology:cs_reading_relation('b95697e5-9a13-47f7-9821-056e77c8e0e4', border_legitimacy__sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('b95697e5-9a13-47f7-9821-056e77c8e0e4', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('b95697e5-9a13-47f7-9821-056e77c8e0e4', foundational, freedom_of_movement_as_inalienable_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_as_inalienable_right, holdable).
narrative_ontology:cs_axiom_grounding('b95697e5-9a13-47f7-9821-056e77c8e0e4', freedom_of_movement_as_inalienable_right, deontological).
narrative_ontology:cs_axiom('b95697e5-9a13-47f7-9821-056e77c8e0e4', foundational, state_exclusion_requires_proportionate_justification).
narrative_ontology:cs_axiom_status(state_exclusion_requires_proportionate_justification, holdable).
narrative_ontology:cs_axiom_grounding('b95697e5-9a13-47f7-9821-056e77c8e0e4', state_exclusion_requires_proportionate_justification, instrumental).
narrative_ontology:cs_reference_frame('b95697e5-9a13-47f7-9821-056e77c8e0e4', postwar_human_rights_consensus).
narrative_ontology:cs_drift_state('b95697e5-9a13-47f7-9821-056e77c8e0e4', contemporary_border_externalization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b95697e5-9a13-47f7-9821-056e77c8e0e4', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, migrants_asylum_seekers).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, transnational_corporations_labor_mobility).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, human_rights_advocacy_orgs).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_native_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_state_beneficiaries).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, border_enforcement_personnel).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, human_rights_universality).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, freedom_of_movement_as_jus_cogens).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, border_presumption_of_illegitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals seeking to cross borders for survival, family reunification, or economic opportunity. Their movement is constrained by border enforcement apparatuses. The freedom_of_movement reading treats their exclusion as the primary injustice; they benefit directly from the reading's normative force but lack structural power to enforce it.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, migrants_asylum_seekers, beneficiary,
    powerless, biographical, trapped, global).

% Corporate actors who benefit from flexible global labor markets and supply chains. They advocate for open borders instrumentally — not for human rights per se but for capital's freedom to move labor. Their structural position is paradoxical: they are beneficiaries of the reading's policy implications but do not bear its moral commitments.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, transnational_corporations_labor_mobility, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__freedom_of_movement_reading, transnational_corporations_labor_mobility, agenda_setter).

% NGOs, legal clinics, and advocacy networks that litigate, document, and campaign for migrant rights. They are the primary articulators and enforcers of this reading in international fora. They collect status, funding, and institutional legitimacy from the reading's ascendancy.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, human_rights_advocacy_orgs, agenda_setter,
    organized, generational, mobile, global).

% Citizens in receiving countries whose wages or employment prospects are depressed by unrestricted labor inflow. They bear the distributive costs of the reading's policy realization. Their exit from the constraint is constrained: they cannot leave the labor market, and political representation is mediated by parties that may not represent their interests.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_native_workers, payer,
    moderate, biographical, constrained, national).

% Citizens dependent on tax-financed public services (healthcare, education, housing) that face fiscal strain from non-contributory population growth. They bear the fiscal externalities of open borders. Their exit is constrained by the same political mediation as displaced workers.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, welfare_state_beneficiaries, payer,
    moderate, biographical, constrained, national).

% Agents of the state tasked with enforcing border laws that this reading declares illegitimate. They experience the reading as a delegitimation of their professional identity and mission. Their exit is identity-locked: leaving the role means abandoning a self-concept constituted through border defense.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, border_enforcement_personnel, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__freedom_of_movement_reading, border_enforcement_personnel, excluded).

% State apparatuses that claim monopoly on border control. They are the primary targets of the reading's normative challenge. They respond with enforcement intensification (externalization, deterrence, criminalization) which the reading measures as extractive suppression.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, sovereign_states_as_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Scholars who analyze the border legitimacy kernel from outside the contest. They do not bear costs or collect rents from any reading; they map the structural relationships between readings.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, political_theorists_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading coordinates a global normative framework that treats human mobility as a default entitlement rather than a state-granted privilege. It solves the coordination problem of competing exclusionary claims by establishing a universal baseline: the burden of justification falls on the restrictor, not the mover.
% TRANSFER_FUNCTION: Transfers the right to exclude from states to individuals: states lose the unilateral power to deny entry; migrants gain a claim-right to cross borders unless a specific, proportionate justification overrides. The transfer is normative (legal/moral authority), not material — but material consequences follow (remittances, labor market effects, fiscal flows).
% ABSENT_VOICES: Future generations who will inherit the demographic and institutional consequences of open borders; stateless persons who fall outside both state protection and human rights enforcement; indigenous peoples whose territorial sovereignty claims are neither state sovereignty nor individual mobility rights.
% DISAPPEARANCE_RATIONALE: If the freedom_of_movement reading vanished overnight, states would revert to unrestricted exclusionary sovereignty. Border enforcement would intensify without normative constraint; migrant deaths at borders would increase; the international protection regime (refugee law, non-refoulement) would collapse. The world would rearrange toward hardened sovereignty.
% FOUNDING_PROBLEM: The post-WWII revelation that state sovereignty over borders enabled genocide, ethnic cleansing, and the trapping of populations in death zones. The founding problem was: how to prevent states from using border control as a weapon against their own populations and others?
% FOUNDING_PROBLEM_CORROBORATION: The UDHR drafting history (Morsink 1999) corroborates that Article 13 was a direct response to Nazi exit bans and Soviet emigration restrictions. Contemporary restrictionists argue the founding problem is solved — the genocidal regimes are gone — and the reading now serves migration management interests. The corroboration is split: the historical record supports the founding narrative; the present functional analysis disputes its continued relevance.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__freedom_of_movement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__freedom_of_movement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the reading demands transfer of exclusionary authority from states to individuals — a massive reallocation of normative and material power. Suppression (0.78) is high because the reading's realization requires dismantling the entire border enforcement apparatus, which resists fiercely. Theater ratio (0.25) is moderate: the human rights machinery (UNHCR, treaty bodies, strategic litigation) has real coordination function but increasingly performs ritual compliance while material borders harden. Accessibility collapse (0.45) is moderate: alternatives (guest worker programs, regional free movement, humanitarian corridors) exist but are treated as concessions, not rights. Resistance (0.68) is high: states, publics, and enforcement agencies actively resist the reading's demands.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (displaced workers, welfare beneficiaries, border personnel) experience this reading as a snare: a normative framework that extracts from them while claiming universal benefit. The beneficiary seats (migrants, corporations, advocates) experience it as a rope: a coordination mechanism that solves a genuine collective action problem (statelessness, rightlessness). The agenda-setter seats diverge: states experience it as extraction of their sovereign prerogative; advocacy orgs experience it as their mission. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Migrants are full beneficiaries (d ~ 0.0) — the reading subsidizes their claims. Corporate actors are beneficiaries with arbitrage exit (d ~ 0.1) — they capture gains without bearing moral costs. Advocacy orgs are agenda-setters with mobile exit (d ~ 0.15) — they administer the reading. Displaced workers and welfare beneficiaries are payers with constrained exit (d ~ 0.7-0.8) — they bear costs they cannot easily escape. Border personnel are payers with identity_locked exit (d ~ 0.85) — the reading attacks their professional self-concept. States are agenda-setters targeted by the reading (d ~ 0.9) — they lose authority. Observers are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The reading's founding problem (preventing states from weaponizing borders) was live in 1948. By 2025, the genocidal regimes that motivated it are gone, but new border regimes (externalization, deterrence, militarization) have emerged that the reading's framers did not anticipate. The mandatrophy is contested: the reading's proponents argue the problem has mutated, not disappeared; critics argue the reading now serves as cover for capital's labor mobility demands. The engine's mandatrophy detection will hinge on the founding_problem_status x disappearance_verdict mismatch: status=contested + verdict=world_rearranges = active contestation, not capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_right_vs_constructed_entitlement,
    'Is freedom of movement a pre-political natural right (mountain-like) or a constructed entitlement that requires active institutional maintenance (rope/tangled_rope)?',
    'Compare the reading''s operation in failed states (where no institution enforces it) vs. functional states. If the right evaporates without enforcement, it is constructed; if it persists as a moral claim even under total suppression, it has mountain-like features.',
    'If natural right, the reading''s high extractiveness/suppression metrics reflect the cost of realizing a mountain, not the reading''s own extractive nature. If constructed, the metrics accurately describe the reading as a tangled_rope. This ambiguity is the FSM trigger for false_summit_mountain if the reading were claimed as mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_right_vs_constructed_entitlement, conceptual, 'Whether the reading''s normative claim has mountain-like natural-law status or is a human institution.').

omega_variable(
    coordination_extraction_boundary_migration,
    'Can the coordination function (universal mobility baseline) be separated from the extraction function (fiscal/distributive costs on citizens)?',
    'Examine regional free movement zones (EU, ECOWAS, Mercosur) where coordination exists without global open borders. If coordination scales without the global extraction profile, the functions are separable.',
    'If separable, the tangled_rope classification is correct: the reading bundles a genuine coordination good with an extractive transfer. If inseparable, the reading may be a scaffold (transitional coordination toward a world where extraction is internalized) or a snare (coordination as cover for extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary_migration, empirical, 'Whether the reading''s coordination and extraction components are structurally separable in practice.').

omega_variable(
    committer_structure_kernel_reading,
    'How does the freedom_of_movement_reading structurally relate to the sovereignty_reading and humanitarian_obligation_reading within the border_legitimacy kernel?',
    'Map the logical space: does any single institutional framework (constitution, treaty regime, customary law) accommodate more than one reading simultaneously? The answer determines whether readings are mutually exclusive foreclosures or coexisting positions.',
    'If forecloses relations dominate, the kernel is a site of zero-sum constitutional conflict. If coexists_with dominates, the kernel is a pluralist field where multiple readings operate in different domains. This determines the cs_structure classification of the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Structural relationships between the three declared readings of the border_legitimacy kernel.').

omega_variable(
    victim_set_boundary_current_citizens,
    'Do displaced native workers and welfare beneficiaries genuinely enter the victim set of this reading, or are their costs externalities of a just arrangement?',
    'Distinguish between: (a) costs that are necessary incidents of realizing a just right (like taxation for public goods) vs. (b) costs that indicate the right itself is misspecified or overbroad. The distinction turns on whether the reading contains internal limits (proportionality, non-regression) that would bound citizen costs.',
    'If (a), the victim declaration is a category error — these are not victims of the constraint but bearers of its just costs. If (b), the victim declaration is correct and the reading is extractive toward citizens. This determines whether the tangled_rope classification holds or the reading is a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary_current_citizens, conceptual, 'Whether citizen costs constitute victimhood under the reading or are externalities of justice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(bord_tr_t1965, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(bord_tr_t1980, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(bord_tr_t1995, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(bord_tr_t2010, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(bord_tr_t2025, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(bord_be_t1965, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement(bord_be_t1980, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(bord_be_t1995, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(bord_be_t2010, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2010, 0.74).
narrative_ontology:measurement(bord_be_t2025, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2025, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1948, 0.3).
narrative_ontology:measurement(bord_su_t1965, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1965, 0.42).
narrative_ontology:measurement(bord_su_t1980, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(bord_su_t1995, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(bord_su_t2010, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(bord_su_t2025, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(border_legitimacy__freedom_of_movement_reading, 0.12).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__humanitarian_obligation_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, refugee_protection_regime).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, global_labor_mobility_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the border_legitimacy kernel. The kernel decomposes because the label 'border legitimacy' conflates structurally distinct claims with different ε values: sovereignty_reading (ε ≈ 0.15, mountain/tangled_rope depending on enforcement), humanitarian_obligation_reading (ε ≈ 0.45, rope/tangled_rope), freedom_of_movement_reading (ε ≈ 0.82, tangled_rope/snare). The ε values differ by a wide margin because each reading demands a different transfer of authority and bears different enforcement costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, organized, 0.85).
constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, moderate, 0.75).
constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
