% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: Sovereign Absolute Border Exclusion
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story captures the sovereignty-primary reading of the
 *   contested kernel 'border_control_legitimacy'. The reading asserts that
 *   territorial sovereignty entails absolute discretion to exclude
 *   non-citizens and that border control is constitutive of statehood itself.
 *   The standing arrangement under contest is the global regime of sovereign
 *   border control, assessed from the sovereignty-primary perspective: the
 *   arrangement extracts freedom of movement from non-citizens and transfers
 *   political autonomy to states. The claim is that this is a mountain — a
 *   natural, unchangeable feature of political order — but the authored
 *   metrics describe a highly extractive, actively enforced regime with
 *   substantial suppression and rising theater. The divergence between claim
 *   and metrics is the measurement target.
 *
 * KEY AGENTS:
 *   - sovereign_states: Primary agenda-setter and beneficiary (institutional/arbitrage) — sets and enforces border rules, collects political autonomy rents
 *   - excluded_migrants: Primary victim/payer (powerless/trapped) — bears the full cost of exclusion, denied entry, detained, deported
 *   - citizens: Secondary beneficiary/payer (organized/constrained) — gains political community definition but pays enforcement costs and moral complicity
 *   - international_human_rights_bodies: Observer (institutional/analytical) — monitors, critiques, but lacks enforcement power
 *   - neighboring_states: Excluded (powerful/constrained) — affected by migration externalities but not party to the sovereignty claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.85).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.9).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.85).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, mountain).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Sovereign Absolute Border Exclusion").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).
domain_priors:emerges_naturally(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, '099d8b7f-57a4-456e-a606-48072b1d83d5').
narrative_ontology:cs_kernel_codification('099d8b7f-57a4-456e-a606-48072b1d83d5', formalized).
narrative_ontology:cs_authority_grounding('099d8b7f-57a4-456e-a606-48072b1d83d5', lineage).
narrative_ontology:cs_interpretation_layer_present('099d8b7f-57a4-456e-a606-48072b1d83d5').
narrative_ontology:cs_reading_relation('099d8b7f-57a4-456e-a606-48072b1d83d5', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('099d8b7f-57a4-456e-a606-48072b1d83d5', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('099d8b7f-57a4-456e-a606-48072b1d83d5', foundational, absolute_exclusion_discretion).
narrative_ontology:cs_axiom_status(absolute_exclusion_discretion, holdable).
narrative_ontology:cs_axiom_grounding('099d8b7f-57a4-456e-a606-48072b1d83d5', absolute_exclusion_discretion, conventional).
narrative_ontology:cs_axiom('099d8b7f-57a4-456e-a606-48072b1d83d5', foundational, border_control_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(border_control_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('099d8b7f-57a4-456e-a606-48072b1d83d5', border_control_constitutive_of_statehood, conventional).
narrative_ontology:cs_reference_frame('099d8b7f-57a4-456e-a606-48072b1d83d5', westphalian_sovereignty).
narrative_ontology:cs_drift_state('099d8b7f-57a4-456e-a606-48072b1d83d5', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('099d8b7f-57a4-456e-a606-48072b1d83d5', '2026-07-30T14:22:10Z').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, sovereign_states).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_migrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizens).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, citizens).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, statehood_constitutive_exclusion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces border rules through domestic law and international treaties. Collects political autonomy, control over membership, and the ability to externalize enforcement costs. Can exit the constraint only by ceding sovereignty (e.g., EU free movement), which is structurally difficult.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, sovereign_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, sovereign_states, beneficiary).

% Denied entry, detained, deported, or forced into irregular status. Bears the full cost of the constraint: lost life-chances, family separation, physical danger. Exit options are nearly nonexistent — cannot change the constraint, cannot easily change identity as non-citizen, and irregular migration is criminalized.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Gains defined political community, welfare state sustainability, and cultural continuity from border control. Pays through tax-funded enforcement, moral complicity in exclusion, and reduced labor market flexibility. Exit is constrained — emigration possible but costly, and the constraint follows them as citizens of a state that enforces borders.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizens, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, citizens, payer).

% Monitors state compliance with human rights treaties, issues reports, and brings cases. Does not collect rents nor bear enforcement costs. Has analytical exit (can change mandate) but no structural power to alter the constraint.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Receives migration externalities (refugee flows, remittances, brain drain) from other states' border policies. Not a party to the sovereignty claim that justifies exclusion. Could retaliate or cooperate but is structurally excluded from the legitimacy discourse.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, neighboring_states, excluded,
    powerful, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__sovereignty_primary, sovereign_states).
narrative_ontology:fixing_cost_class(border_control_legitimacy__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines and maintains political community by controlling membership and territory; solves the problem of who belongs to the self-governing unit and under what terms.
% TRANSFER_FUNCTION: Moves the right to enter, remain, and access territory from non-citizens to states; transfers security, welfare, and political autonomy to citizens at the cost of migrants' freedom of movement and life-chances.
% ABSENT_VOICES: Migrants themselves (especially those never able to reach the border), future generations who inherit the border regime, stateless persons who fall through the cracks of the citizenship system, and the global poor for whom borders are the primary barrier to opportunity.
% DISAPPEARANCE_RATIONALE: If absolute sovereign border control vanished overnight, global migration would surge dramatically, labor markets would reorganize, welfare states would face immediate pressure, and the Westphalian state system would need to renegotiate the basis of political membership. The world would not stay the same.
% FOUNDING_PROBLEM: The need for political communities to control their membership and territory in order to achieve self-determination, provide public goods, and maintain cultural coherence — a problem identified in the Westphalian settlement and reinforced by 19th-20th century nation-state building.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists in the communitarian/statist tradition (e.g., Walzer, Miller) attest the problem is live. Cosmopolitan theorists (e.g., Carens, Benhabib) and human rights bodies attest it is dead or outweighed by freedom of movement. The corroboration is split across ideological lines.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, ExtMetricName, E),
    domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(border_control_legitimacy__sovereignty_primary),
    narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint transfers the full value of mobility and life-chances from migrants to states. Suppression is very high (0.9) because enforcement requires walls, detention, deportation machinery, and legal bars — the constraint cannot persist without active coercion. Theater ratio is moderate (0.3): the sovereignty discourse performs naturalness, but the enforcement apparatus is substantive. Accessibility collapse is high (0.8) for migrants — alternatives (irregular migration, asylum) are dangerous and narrowing. Resistance is substantial (0.7) from migrants, advocates, and some international bodies. The measurement series shows rising extractiveness and suppression over the interval, consistent with border externalization and deterrence policies.
 *
 * PERSPECTIVAL GAP:
 *   From the sovereign state seat, the constraint appears as coordination (defining the political community) with minimal extraction — the natural order. From the excluded migrant seat, it appears as pure extraction (snare) enforced by violence. The engine computes this divergence from the structural data; the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states are structural beneficiaries (d near 0.0): they collect political autonomy, control membership, and externalize costs. Excluded migrants are structural targets (d near 1.0): they bear the full extractive weight, have trapped exit options, and are identity-locked into the category 'non-citizen'. Citizens sit near symmetric (d ~0.5): they gain political community but pay enforcement costs and bear moral injury. International bodies are analytical observers (d=0.5). Neighboring states are excluded — they experience spillovers but cannot participate in the sovereignty claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (defining political community through territorial control) may be live, dead, or contested. If dead (global mobility as norm), the constraint persists as piton/zombie. If live, it remains tangled_rope (coordination + extraction). The corridor analysis will detect mandatrophy via founding_problem_status x disappearance_verdict mismatch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the absolute discretion to exclude non-citizens a genuine natural law of political order, or a constructed constraint that benefits identifiable agents (states)?',
    'Historical analysis of the emergence of border control as a universal state practice; comparative study of pre-Westphalian political forms; examination of whether any functional political community has existed without exclusionary border control.',
    'If natural law, the constraint is a genuine mountain; if constructed, it is a false summit and should reclassify as tangled_rope or snare via FSM.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Natural-law vs. constructed status of sovereign border exclusion').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of migrants structural (walls, detention, legal bars) or internalized (migrants accepting exclusion as legitimate, self-deportation, deterrence)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., open borders policy), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for excluded migrants').

omega_variable(
    kernel_reading_identity,
    'How does this reading''s classification change if the kernel ''border_control_legitimacy'' is reframed from sovereignty-primary to freedom-of-movement-primary?',
    'Compare ε, beneficiary/victim sets, and directionality across the three declared readings of the kernel; map structural deltas.',
    'If the sibling readings produce substantially different classifications, the kernel is a site of genuine structural contestation, not merely interpretive disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-kernel framing under-determination across sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcl_sp_tr_t0, border_control_legitimacy__sovereignty_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(bcl_sp_tr_t6, border_control_legitimacy__sovereignty_primary, theater_ratio, 6, 0.22).
narrative_ontology:measurement(bcl_sp_tr_t12, border_control_legitimacy__sovereignty_primary, theater_ratio, 12, 0.25).
narrative_ontology:measurement(bcl_sp_tr_t18, border_control_legitimacy__sovereignty_primary, theater_ratio, 18, 0.27).
narrative_ontology:measurement(bcl_sp_tr_t24, border_control_legitimacy__sovereignty_primary, theater_ratio, 24, 0.29).
narrative_ontology:measurement(bcl_sp_tr_t30, border_control_legitimacy__sovereignty_primary, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(bcl_sp_be_t0, border_control_legitimacy__sovereignty_primary, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(bcl_sp_be_t6, border_control_legitimacy__sovereignty_primary, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(bcl_sp_be_t12, border_control_legitimacy__sovereignty_primary, base_extractiveness, 12, 0.81).
narrative_ontology:measurement(bcl_sp_be_t18, border_control_legitimacy__sovereignty_primary, base_extractiveness, 18, 0.83).
narrative_ontology:measurement(bcl_sp_be_t24, border_control_legitimacy__sovereignty_primary, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(bcl_sp_be_t30, border_control_legitimacy__sovereignty_primary, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(bcl_sp_su_t0, border_control_legitimacy__sovereignty_primary, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(bcl_sp_su_t6, border_control_legitimacy__sovereignty_primary, suppression_requirement, 6, 0.82).
narrative_ontology:measurement(bcl_sp_su_t12, border_control_legitimacy__sovereignty_primary, suppression_requirement, 12, 0.85).
narrative_ontology:measurement(bcl_sp_su_t18, border_control_legitimacy__sovereignty_primary, suppression_requirement, 18, 0.87).
narrative_ontology:measurement(bcl_sp_su_t24, border_control_legitimacy__sovereignty_primary, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(bcl_sp_su_t30, border_control_legitimacy__sovereignty_primary, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__sovereignty_primary, 0.08).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% Kernel 'border_control_legitimacy' decomposes into three readings with distinct ε and beneficiary/victim structures. This reading (sovereignty_primary) has ε=0.85, beneficiaries={sovereign_states}, victims={excluded_migrants}. The freedom_of_movement_primary reading has ε≈0.1 (mountain of human rights), beneficiaries={migrants, humanity}, victims={none}. The jurisdictional_sovereignty reading has ε≈0.4 (tangled_rope), beneficiaries={states, employers}, victims={irregular_migrants}. The ε-invariance principle requires separate stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
