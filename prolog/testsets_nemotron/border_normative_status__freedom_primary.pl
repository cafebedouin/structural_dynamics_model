% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Border Control as Illegitimate Restriction on Freedom of Movement
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom_primary reading of the
 *   border_normative_status kernel: freedom of movement is a fundamental
 *   human right that borders impermissibly restrict; exclusion requires
 *   extraordinary justification. The standing arrangement under contest is
 *   the global border regime — visa systems, detention apparatus,
 *   extraterritorial enforcement, and the sovereignty doctrine that
 *   legitimizes them. From this reading's lights, the regime is a snare: it
 *   extracts life chances from the globally poor and displaced to subsidize
 *   the political stability and economic rents of wealthy states. The
 *   coordination story (order, security, self-determination) is cover; the
 *   regime's persistence depends on suppressing exits (no right to leave, no
 *   right to enter) and alternatives (open borders, free movement regimes,
 *   regional integration beyond the EU model).
 *
 * KEY AGENTS:
 *   - excluded_migrants: Primary target (powerless/trapped) — bears extraction
 *   - asylum_seekers: Primary target (powerless/trapped) — bears extraction
 *   - stateless_persons: Primary target (powerless/trapped) — bears extraction
 *   - climate_displaced_persons: Primary target (powerless/trapped) — bears extraction
 *   - displaced_domestic_workers: Secondary beneficiary (moderate/constrained) — incidental benefit
 *   - border_enforcement_agencies: Agenda-setter + beneficiary (institutional/arbitrage) — administers and profits
 *   - sovereign_state_apparatus: Agenda-setter (institutional/arbitrage) — sets rules, extracts legitimacy
 *   - human_rights_observers: Observer (organized/analytical) — sees full structure
 *   - migrant_rights_organizations: Excluded (organized/constrained) — would object if present
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.92).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.88).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.92).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Border Control as Illegitimate Restriction on Freedom of Movement").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, 'a01f0121-1941-49be-897e-24afa88eec8d').
narrative_ontology:cs_kernel_codification('a01f0121-1941-49be-897e-24afa88eec8d', distributed).
narrative_ontology:cs_authority_grounding('a01f0121-1941-49be-897e-24afa88eec8d', distributed).
narrative_ontology:cs_reading_relation('a01f0121-1941-49be-897e-24afa88eec8d', border_normative_status__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('a01f0121-1941-49be-897e-24afa88eec8d', border_normative_status__qualified_sovereignty, coexists_with).
narrative_ontology:cs_axiom('a01f0121-1941-49be-897e-24afa88eec8d', foundational, freedom_of_movement_fundamental_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('a01f0121-1941-49be-897e-24afa88eec8d', freedom_of_movement_fundamental_right, deontological).
narrative_ontology:cs_axiom('a01f0121-1941-49be-897e-24afa88eec8d', foundational, border_exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(border_exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('a01f0121-1941-49be-897e-24afa88eec8d', border_exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('a01f0121-1941-49be-897e-24afa88eec8d', universal_freedom_of_movement).
narrative_ontology:cs_drift_state('a01f0121-1941-49be-897e-24afa88eec8d', contemporary_border_regime, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a01f0121-1941-49be-897e-24afa88eec8d', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, displaced_domestic_workers).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, border_enforcement_agencies).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, sovereign_state_apparatus).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, stateless_persons).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, climate_displaced_persons).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, freedom_of_movement_as_fundamental_right).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, universal_human_dignity).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, equality_of_moral_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the full weight of border enforcement — detention, deportation, family separation, denied access to labor markets and asylum systems. Their exclusion is the constraint's direct operation. No viable exit: cannot choose where to be born, cannot access legal pathways that states collectively deny, and face lethal consequences for unauthorized movement.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Fleeing persecution or violence, they encounter border regimes that criminalize their arrival, detain them indefinitely, or return them to danger (non-refoulement violations). The constraint operates on them as both extraction (denied protection) and suppression (active deterrence). Exit options are structurally nonexistent — they cannot 'choose' not to flee.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Denied nationality by any state, they fall through the cracks of a system that allocates rights through state membership. Border controls render them permanently rightless — unable to enter, unable to leave, unable to claim protection. The constraint's extraction is total and intergenerational.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, stateless_persons, payer,
    powerless, generational, trapped, global).

% Displaced by environmental collapse they did not cause, they face border regimes with no legal category for their movement. The constraint extracts their survival chances while offering no coordination function for them. Exit is structurally blocked — no state recognizes climate displacement as grounds for entry.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, climate_displaced_persons, payer,
    powerless, biographical, trapped, global).

% Domestic workers in sectors exposed to migrant labor competition who benefit from border restrictions that reduce labor supply and protect wage floors. Their benefit is real but diffuse — they are not the agenda-setters of border policy, and their political power is constrained by broader economic forces. Exit from this beneficiary position is possible (solidarity with migrants, union organizing) but structurally discouraged.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers, beneficiary,
    moderate, biographical, constrained, national).

% Administer and enforce border controls; their budgets, personnel, and institutional mandate expand with enforcement intensity. They benefit materially and institutionally from the constraint's persistence. However, they are also constrained by legal frameworks, political oversight, and operational realities — they cannot simply 'choose' to stop enforcing without systemic change.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, border_enforcement_agencies, beneficiary).

% The collective state machinery that claims legitimate authority over territorial exclusion. It sets the rules, controls the enforcement apparatus, and extracts legitimacy from the border regime. Its 'exit' from this arrangement would require reconstituting the international order — theoretically possible (open borders advocacy, world federalism) but practically arbitrage-grade: only achievable through coordinated systemic transformation.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, sovereign_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Document, litigate, and advocate against border violations. They do not bear the constraint's costs nor collect its benefits directly. Their analytical position reveals the full structural asymmetry: a global regime that extracts from the most vulnerable to subsidize the political and economic interests of the powerful.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, human_rights_observers, observer,
    organized, generational, analytical, global).

% Organizations led by or accountable to migrant communities that would reject the border regime's legitimacy if given voice in its design. They are structurally excluded from the authoritative forums where border policy is made (state sovereignty, UN system, trade agreements). Their exclusion is not accidental — the constraint's persistence depends on their absence from the agenda-setting table.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, migrant_rights_organizations, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint claims to coordinate collective self-determination, resource allocation, and security through territorial boundaries. In practice, it coordinates the global allocation of life chances by birthplace — a lottery that sorts humans into radically unequal opportunity structures.
% TRANSFER_FUNCTION: Transfers freedom of movement, access to labor markets, political participation, and physical safety from excluded migrants (who lose these) to sovereign states and their favored domestic constituencies (who gatekeep them). The transfer is enforced through detention, deportation, visa regimes, carrier sanctions, and extraterritorial bordering.
% ABSENT_VOICES: Excluded migrants, asylum seekers, stateless persons, and climate-displaced persons are the primary absent voices — they are the constraint's direct targets but have no vote in the states that exclude them. Migrant-led organizations are excluded from authoritative governance forums. Future generations who will inherit a climate-disrupted world with hardened borders are structurally unrepresented.
% DISAPPEARANCE_RATIONALE: If border enforcement vanished overnight, global labor markets would reorganize around willingness to move rather than permission to enter; remittance flows would surge; states would lose a primary tool of population control and resource hoarding; the international order premised on territorial sovereignty would face existential pressure. The world would rearrange profoundly — which is why the constraint persists.
% FOUNDING_PROBLEM: The Westphalian state system needed a mechanism to define political membership, control territory, and allocate collective resources among bounded communities. Borders solved the coordination problem of 'who belongs to which polity' in a world of competing sovereign claims.
% FOUNDING_PROBLEM_CORROBORATION: State sovereignty advocates (sovereignty_primary reading) attest the founding problem remains live: borders are necessary for democratic self-governance and welfare provision. Freedom-of-movement advocates (this reading) and migration scholars attest the founding problem has been superseded by globalization, human rights law, and climate change — the coordination function now serves extraction. No neutral arbiter exists; the dispute is structural.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.92, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is extremely high (0.92) because the constraint allocates the most fundamental life chances — where you can live, work, seek safety, participate politically — by an accident of birth, and enforces this allocation through violence. Suppression is very high (0.88) because the regime actively prevents both exit (exit visas, carrier sanctions, extraterritorial interception) and entry (visa walls, detention, pushbacks), and criminalizes solidarity (humanitarian aid prosecutions). Theater ratio is low (0.15) because the enforcement machinery is real and functional — the constraint does what it claims to do (exclude), it just claims to do it for reasons (security, order) that mask the actual function (rent extraction, political control). Accessibility collapse is high (0.75) because once you understand the border regime as a global apartheid system, alternatives (open borders, freedom of movement) appear both morally necessary and practically unimaginable within the current order. Resistance is substantial (0.65) — migrant caravans, sanctuary movements, legal challenges, direct action — but meets overwhelming structural power.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign state apparatus seat, the border regime appears as legitimate coordination (mountain/rope) — the foundational infrastructure of political order. From the excluded migrant seat, it appears as totalizing extraction with no exit (snare). The engine computes this divergence from the structural data: same constraint, opposite classifications. The freedom_primary reading asserts the migrant seat's classification is the truth; the sovereignty_primary reading asserts the state seat's classification is the truth. This perspectival gap IS the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded migrants, asylum seekers, stateless persons, and climate-displaced persons are the primary victims — they bear the constraint's full extractive weight with zero exit options (trapped). Displaced domestic workers are incidental beneficiaries — they gain marginal wage protection from labor market segmentation, but they do not control the constraint and their benefit is precarious. Border enforcement agencies and the sovereign state apparatus are the agenda-setters who administer the regime and extract institutional and material benefits from it. Their directionality is near-zero (full beneficiary) because the constraint subsidizes their existence. Human rights observers occupy the analytical seat. Migrant rights organizations are excluded — their voice would change the constraint's legitimacy calculus, which is precisely why they are kept out.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defining political membership in a world of sovereign states) has been superseded by the reality that borders now primarily allocate global inequality rather than coordinate collective self-determination. The constraint persists through mandatrophy: its original coordination function has atrophied, but the extraction function has intensified. The classification as snare (not piton) is deliberate — this is not a degraded institution maintained by inertia. It is an actively enforced, highly extractive regime that concentrates benefits on identifiable agenda-setters (states, enforcement agencies) while suppressing alternatives. The high theater ratio would indicate piton; the low theater ratio here indicates active, functional extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Does the border regime perform any genuine coordination function that would persist without its extractive core, or is the coordination story entirely cover for global apartheid?',
    'Counterfactual analysis: if borders were opened but states retained authority over domestic welfare, policing, and democratic procedure, would coordination collapse? Historical analogs: EU free movement (coordination persists), 19th century open borders (coordination persisted).',
    'If genuine coordination exists, the constraint is tangled_rope (hybrid) not pure snare. If coordination is entirely cover, snare classification holds. This is the central empirical/conceptual dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether the border regime''s coordination function is separable from its extraction function').

omega_variable(
    extraordinary_justification_threshold,
    'What would constitute ''extraordinary justification'' for exclusion under this reading, and is the threshold ever met in practice?',
    'Normative theory: identify conditions (pandemic containment, imminent invasion, genocide prevention) where even freedom_primary advocates might accept temporary restriction. Empirical survey: has any historical border enforcement met this standard?',
    'If the threshold is never met, the reading collapses into absolute open borders (no legitimate exclusion ever). If occasionally met, the reading admits a residual coordination function — tangled_rope territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_justification_threshold, conceptual, 'The operational content of ''extraordinary justification'' in the freedom_primary reading').

omega_variable(
    displaced_domestic_worker_benefit_structure,
    'Do displaced domestic workers genuinely benefit from border restrictions, or is their apparent benefit a divide-and-rule strategy that ultimately harms their class position?',
    'Political economy analysis: compare wage and organizing outcomes in sectors with high vs. low migrant competition, controlling for union density, labor law, and capital mobility. Test whether border enforcement correlates with improved or degraded conditions for the domestic working class over the long term.',
    'If the benefit is real and durable, displaced_domestic_workers are genuine beneficiaries (tangled_rope element). If the benefit is illusory or temporary, they are co-victims of a regime that extracts from all workers — the constraint becomes a more diffuse snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_domestic_worker_benefit_structure, empirical, 'Whether domestic workers'' apparent benefit from border restrictions is real or ideological').

omega_variable(
    committer_frame_kernel_contest,
    'This constraint is one reading (freedom_primary) of the contested kernel border_normative_status. How does the structural classification change across the kernel''s sibling readings?',
    'Generate the sibling constraint stories (sovereignty_primary, qualified_sovereignty) with their own ε, beneficiaries, victims, and claimed types. Compare the engine''s computed per-seat classifications across the three readings.',
    'If sovereignty_primary computes as mountain/rope from the state seat and snare from the migrant seat, the kernel contest IS the perspectival gap. If qualified_sovereignty computes as tangled_rope from all seats, it occupies a distinct structural position. The kernel''s structure is the distribution of computed types across its readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_contest, conceptual, 'Commitment-system frame: this reading''s structural position within the border_normative_status kernel family').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1948, border_normative_status__freedom_primary, theater_ratio, 1948, 0.05).
narrative_ontology:measurement(bord_tr_t1975, border_normative_status__freedom_primary, theater_ratio, 1975, 0.08).
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__freedom_primary, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(bord_tr_t2001, border_normative_status__freedom_primary, theater_ratio, 2001, 0.12).
narrative_ontology:measurement(bord_tr_t2015, border_normative_status__freedom_primary, theater_ratio, 2015, 0.14).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__freedom_primary, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(bord_be_t1948, border_normative_status__freedom_primary, base_extractiveness, 1948, 0.65).
narrative_ontology:measurement(bord_be_t1975, border_normative_status__freedom_primary, base_extractiveness, 1975, 0.72).
narrative_ontology:measurement(bord_be_t1990, border_normative_status__freedom_primary, base_extractiveness, 1990, 0.78).
narrative_ontology:measurement(bord_be_t2001, border_normative_status__freedom_primary, base_extractiveness, 2001, 0.85).
narrative_ontology:measurement(bord_be_t2015, border_normative_status__freedom_primary, base_extractiveness, 2015, 0.89).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__freedom_primary, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1948, border_normative_status__freedom_primary, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(bord_su_t1975, border_normative_status__freedom_primary, suppression_requirement, 1975, 0.62).
narrative_ontology:measurement(bord_su_t1990, border_normative_status__freedom_primary, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(bord_su_t2001, border_normative_status__freedom_primary, suppression_requirement, 2001, 0.8).
narrative_ontology:measurement(bord_su_t2015, border_normative_status__freedom_primary, suppression_requirement, 2015, 0.85).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__freedom_primary, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_normative_status__freedom_primary, 0.12).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, global_migration_regime).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, international_refugee_law).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, climate_displacement_governance).

% DUAL FORMULATION NOTE:
% This story is the freedom_primary reading of the border_normative_status kernel. The kernel decomposes into three constraint stories with divergent ε values: freedom_primary (ε=0.92, snare), qualified_sovereignty (ε≈0.45, tangled_rope), sovereignty_primary (ε≈0.15, rope/mountain from state seat). The ε-invariance principle requires separate stories because the same label ('border control') covers structurally distinct claims with different extraction profiles, different victim sets, and different empirical statuses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_normative_status__freedom_primary, moderate, 0.35).
constraint_indexing:directionality_override(border_normative_status__freedom_primary, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
