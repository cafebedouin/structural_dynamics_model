% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Border Enforcement Regime (Freedom-of-Movement Reading)
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This constraint story represents the freedom-of-movement reading of the
 *   contested kernel 'border_legitimacy.' It assesses the actually existing
 *   border enforcement regime — visa regimes, physical barriers, detention,
 *   deportation, surveillance — from the perspective that freedom of movement
 *   is a human right and borders are presumptively illegitimate restrictions.
 *   The regime claims to coordinate security, welfare, and social cohesion;
 *   this reading sees those claims as theater covering a global caste system
 *   that allocates life chances by birthplace. Extraction is high and rising:
 *   the regime transfers opportunity from the global poor to a narrow
 *   enforcement coalition while imposing diffuse costs on domestic workers
 *   and welfare recipients. Suppression is near-total: alternatives (open
 *   borders, regional freedom of movement, migration compacts) are actively
 *   crushed. Theater ratio is high and growing: security theater,
 *   humanitarian gesturing, and 'managed migration' rhetoric expand while
 *   enforcement intensifies.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.82).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.91).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Enforcement Regime (Freedom-of-Movement Reading)").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, '13f9227b-6dd9-4870-bdc5-b98cfe175659').
narrative_ontology:cs_kernel_codification('13f9227b-6dd9-4870-bdc5-b98cfe175659', distributed).
narrative_ontology:cs_authority_grounding('13f9227b-6dd9-4870-bdc5-b98cfe175659', lineage).
narrative_ontology:cs_interpretation_layer_present('13f9227b-6dd9-4870-bdc5-b98cfe175659').
narrative_ontology:cs_reading_relation('13f9227b-6dd9-4870-bdc5-b98cfe175659', border_legitimacy__sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('13f9227b-6dd9-4870-bdc5-b98cfe175659', border_legitimacy__humanitarian_obligation_reading, coexists_with).
narrative_ontology:cs_axiom('13f9227b-6dd9-4870-bdc5-b98cfe175659', foundational, freedom_of_movement_is_human_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_is_human_right, holdable).
narrative_ontology:cs_axiom_grounding('13f9227b-6dd9-4870-bdc5-b98cfe175659', freedom_of_movement_is_human_right, deontological).
narrative_ontology:cs_axiom('13f9227b-6dd9-4870-bdc5-b98cfe175659', foundational, borders_presumptively_illegitimate).
narrative_ontology:cs_axiom_status(borders_presumptively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('13f9227b-6dd9-4870-bdc5-b98cfe175659', borders_presumptively_illegitimate, deontological).
narrative_ontology:cs_reference_frame('13f9227b-6dd9-4870-bdc5-b98cfe175659', universal_human_mobility).
narrative_ontology:cs_drift_state('13f9227b-6dd9-4870-bdc5-b98cfe175659', contemporary_enforcement_intensification, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('13f9227b-6dd9-4870-bdc5-b98cfe175659', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, state_border_apparatus).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, domestic_labor_protection_interests).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, security_contracting_complex).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, would_be_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, welfare_recipients).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, mixed_status_families).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, freedom_of_movement_as_human_right).
narrative_ontology:constraint_vindicates(border_legitimacy__freedom_of_movement_reading, borders_presumptively_illegitimate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces border controls: visa regimes, detention, deportation, surveillance. Justifies the regime as necessary for sovereignty, security, and welfare protection. Collects budgetary resources, institutional authority, and political capital from enforcement. Can shift policy but faces domestic political constraints.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, state_border_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Blocked from moving by visa regimes, physical barriers, and legal prohibitions. Bear the full cost of denied opportunity: lost wages, separated families, exposure to danger in origin countries. No legal pathway exists for most; irregular migration carries lethal risk. Their voices are structurally absent from the sovereign decision-making that determines their mobility.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, would_be_migrants, payer,
    powerless, biographical, trapped, global).

% Experience wage pressure and job displacement attributed to immigration, but also bear the costs of a closed system: reduced economic dynamism, higher prices for goods and services, and the fiscal burden of enforcement. Their interests are invoked to justify restriction while the enforcement apparatus captures the rents. Exit from the national labor market is costly and incomplete.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_domestic_workers, payer,
    moderate, biographical, constrained, national).

% Framed as threatened by migrant access to public benefits, they are politically mobilized to support border enforcement. In practice, enforcement spending diverts resources from social provision, and the closed system reduces the tax base that funds welfare. They have no meaningful exit from the national welfare system and no voice in migration policy.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, welfare_recipients, payer,
    powerless, biographical, trapped, national).

% Citizen and non-citizen family members split by enforcement: deportations, visa denials, bars to reentry. The constraint directly severs kinship bonds. Their identity as family makes exit from the constraint unthinkable — they cannot 'choose' to abandon relatives. Bear psychological, economic, and civic costs.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, mixed_status_families, payer,
    powerless, biographical, identity_locked, national).

% Unions and professional associations that lobby for restricted labor mobility to protect wage floors and bargaining power. Gain concentrated benefits from reduced competition. Have political access and exit options (capital mobility, political influence). Their gain is real but narrower than the enforcement apparatus's.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, domestic_labor_protection_interests, beneficiary,
    organized, biographical, mobile, national).

% Private detention, surveillance, and border-technology firms that contract with states. Extract direct revenue from enforcement spending. Influence policy through lobbying and revolving doors. Operate globally; can shift contracts across jurisdictions. Their interest is the perpetuation and intensification of enforcement regardless of migratory pressure.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, security_contracting_complex, beneficiary,
    powerful, generational, arbitrage, global).

% Provide protection and assistance to migrants but are barred from shaping the regime that creates the need. Their mandate is limited to mitigation, not structural change. They witness the harm but cannot alter the enforcement architecture. Funding depends on state donors, creating a structural conflict.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, humanitarian_organizations, excluded,
    moderate, biographical, constrained, global).

% Treaty bodies, special rapporteurs, and courts that monitor compliance with human rights obligations. They document the gap between freedom-of-movement norms and state practice. Their findings carry normative weight but no enforcement power. They see the full structure but cannot compel compliance.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, international_legal_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The regime claims to coordinate territorial integrity, national security, welfare sustainability, and social cohesion by controlling who enters and remains. From this reading's perspective, these are post-hoc justifications; the actual coordination achieved is the allocation of life chances by birthplace — a global caste system.
% TRANSFER_FUNCTION: Moves freedom of movement and economic opportunity from would-be migrants (and the global poor) to the state border apparatus and its contractors, mediated by the political consent of protected domestic interests. Transfers fiscal resources from social provision to enforcement. Transfers risk from capital to labor by immobilizing the latter.
% ABSENT_VOICES: Would-be migrants — the primary subjects of the constraint — are structurally excluded from the sovereign forums where border policy is made. Future generations who will inherit the climate and demographic consequences of closed borders. Stateless persons who fall outside any sovereign protection. Their absence is not accidental; the regime's legitimacy depends on their silence.
% DISAPPEARANCE_RATIONALE: If border enforcement vanished overnight, global labor markets would reorganize within months: wages would converge toward productivity, remittance flows would dwarf current aid, urbanization patterns would shift, and the fiscal architecture of the welfare state would face immediate stress. The nation-state system as a territorial monopoly on movement would dissolve. The rearrangement would be chaotic, violent in places, and profoundly redistributive — but the world would not stay the same.
% FOUNDING_PROBLEM: The Westphalian settlement (1648) established territorial sovereignty as the organizing principle of international order, requiring states to control borders to monopolize violence, collect taxes, and define the political community. The post-1948 human rights framework attempted to limit this but preserved the sovereign right to exclude. The constraint was built to solve the problem of political order in a world of competing territorial claims.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the Westphalian order (Krasner, Philpott) and the drafting history of the UDHR (Morsink) corroborate that border control was foundational to the sovereign state system. Migration studies (Carens, Betts) and climate mobility literature document that the founding problem — territorial political order — is now contested by globalization, climate displacement, and the demographic collapse of closed societies. No external corroboration supports the claim that the founding problem remains live in its original form; the arrangement persists as rent extraction.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
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
 *   Extractiveness (0.82) reflects the massive transfer of lifetime earnings and life chances from would-be migrants to the enforcement coalition, plus the fiscal diversion from social provision to border spending. Suppression (0.91) reflects the physical, legal, and bureaucratic walls that make irregular migration lethal and legal pathways nearly nonexistent for most. Theater ratio (0.68) captures the expanding gap between the regime's stated purposes (security, order, fairness) and its actual operation (rent extraction, labor control, political mobilization). Accessibility collapse (0.78) measures how completely the imaginary of 'legal migration' collapses for the global poor once the system's actual operation is understood. Resistance (0.73) is high: migrants resist through irregular movement, legal challenges, and political organizing; citizens resist through sanctuary movements and policy advocacy — but the regime's institutional power dwarfs this resistance.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute per-seat types from the structural data. The agenda-setter seat (state apparatus) experiences the constraint as coordination-with-extraction (tangled_rope from its view: it solves the real problem of territorial control while extracting rents). The payer seats (migrants, displaced workers, families) experience it as pure extraction (snare). The beneficiary seats (labor interests, contractors) experience it as rope (they get coordination benefit with minimal cost). The observer seats see the full extractive structure. This divergence is the measurement — not an error to resolve.
 *
 * DIRECTIONALITY LOGIC:
 *   The state border apparatus is the structural beneficiary (d ~ 0.1): it sets the rules, collects the budget, and wields the coercive power. Would-be migrants are full targets (d ~ 0.95): trapped, powerless, bearing the full extractive weight. Displaced domestic workers and welfare recipients are secondary targets (d ~ 0.7): they bear diffuse costs and are politically mobilized as supporters, but their material interests are harmed. Mixed-status families are identity-locked targets (d ~ 0.85): kinship bonds make exit unthinkable. Domestic labor interests and security contractors are beneficiaries with arbitrage-grade exit (d ~ 0.2-0.3). Humanitarian organizations are excluded observers. International legal observers are analytical seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (territorial political order) is contested, not dead — but the current regime extracts far beyond what that problem requires. The mandate has atrophied into rent extraction: enforcement spending exceeds any plausible security or welfare justification; the theater ratio confirms performative maintenance. The constraint persists because the enforcement coalition captures the gains, the victims are trapped or identity-locked, and the excluded voices cannot enter the sovereign forum. This is not a piton (inertial remnant) — it is actively intensifying. Mandatrophy is unresolved: the arrangement has outgrown its founding function but no successor coordination exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_kernel_reading,
    'How does the classification change if this constraint is understood as one reading of a contested kernel rather than a standalone claim?',
    'Compare the ε, victim set, and computed type across all three declared readings of the border_legitimacy kernel. The engine''s cross-reading contamination analysis will reveal whether the kernel itself carries structural extractiveness that partitions across readings.',
    'If the kernel has irreducible extractiveness that all readings inherit, then even the freedom-of-movement reading cannot classify the constraint as mountain — the kernel''s structure bounds the minimum extraction. If readings are fully independent, each reading''s ε stands alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Whether kernel-level structure constrains reading-level classification.').

omega_variable(
    borders_natural_vs_constructed,
    'Are borders and their enforcement a natural feature of political order (mountain-like) or a constructed constraint that benefits identifiable agents?',
    'Historical analysis of stateless societies, pre-Westphalian mobility, and the emergence of passport regimes. Counterfactual: would a world without border enforcement reorganize into a stable order or collapse into violence?',
    'If borders are constructed and extractive, the snare classification holds. If they are necessary for any political order (even a just one), the constraint may be a tangled_rope — genuine coordination with asymmetric extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(borders_natural_vs_constructed, conceptual, 'Natural-law vs. constructed status of border enforcement.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (laws, walls, visas) or internalized (national identity, fear of other, legitimacy of sovereign exclusion)?',
    'Post-exit suppression trajectory: if migrants who succeed in crossing still carry internalized barriers (precarity, stigma, legal limbo), suppression is partially internalized. Survey experiments on attitudes toward open borders across populations.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This raises the computed extraction for identity-locked seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in border enforcement.').

omega_variable(
    beneficiary_structure_ambiguity,
    'Do domestic workers and welfare recipients genuinely benefit from border restrictions, or are they co-opted into a coalition that extracts from them as well?',
    'Economic analysis of wage effects, fiscal impacts, and dynamic growth effects of immigration. Political economy of enforcement spending vs. social spending. Coalition stability under varying economic conditions.',
    'If domestic workers are net victims, the victim set expands and the snare classification strengthens. If they are net beneficiaries, the constraint has a genuine coordination function for them — tangled_rope from their seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether protected domestic interests are genuine beneficiaries or co-opted victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bol_fom_tr_t1948, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1948, 0.35).
narrative_ontology:measurement(bol_fom_tr_t1960, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1960, 0.42).
narrative_ontology:measurement(bol_fom_tr_t1975, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1975, 0.48).
narrative_ontology:measurement(bol_fom_tr_t1990, border_legitimacy__freedom_of_movement_reading, theater_ratio, 1990, 0.55).
narrative_ontology:measurement(bol_fom_tr_t2001, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2001, 0.62).
narrative_ontology:measurement(bol_fom_tr_t2015, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2015, 0.66).
narrative_ontology:measurement(bol_fom_tr_t2024, border_legitimacy__freedom_of_movement_reading, theater_ratio, 2024, 0.68).

% Extraction over time
narrative_ontology:measurement(bol_fom_be_t1948, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(bol_fom_be_t1960, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1960, 0.52).
narrative_ontology:measurement(bol_fom_be_t1975, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(bol_fom_be_t1990, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(bol_fom_be_t2001, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2001, 0.72).
narrative_ontology:measurement(bol_fom_be_t2015, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2015, 0.78).
narrative_ontology:measurement(bol_fom_be_t2024, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(bol_fom_su_t1948, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement(bol_fom_su_t1960, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1960, 0.71).
narrative_ontology:measurement(bol_fom_su_t1975, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1975, 0.76).
narrative_ontology:measurement(bol_fom_su_t1990, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 1990, 0.82).
narrative_ontology:measurement(bol_fom_su_t2001, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2001, 0.87).
narrative_ontology:measurement(bol_fom_su_t2015, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2015, 0.89).
narrative_ontology:measurement(bol_fom_su_t2024, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 2024, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_legitimacy__freedom_of_movement_reading, 0.12).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, migration_regime).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, refugee_protection).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, labor_mobility).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, climate_displacement_governance).

% DUAL FORMULATION NOTE:
% This constraint is the freedom_of_movement_reading of the border_legitimacy kernel. It decomposes the single label 'border legitimacy' into a structurally distinct claim with ε=0.82, victim set including would-be migrants and domestic citizens, and claimed type snare. The sovereignty_reading (ε~0.15, mountain/tangled_rope from state seat) and humanitarian_obligation_reading (ε~0.45, tangled_rope) are separate constraint stories linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, institutional, 0.15).
constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, powerless, 0.95).
constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, moderate, 0.7).
constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, organized, 0.25).
constraint_indexing:directionality_override(border_legitimacy__freedom_of_movement_reading, powerful, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
