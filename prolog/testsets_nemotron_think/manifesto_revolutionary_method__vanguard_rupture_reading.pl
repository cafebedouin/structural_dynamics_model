% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__vanguard_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__vanguard_rupture_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__vanguard_rupture_reading
 *   human_readable: Vanguard Party Seizure of State Power as Revolutionary Method
 *   domain: political_philosophy/revolutionary_theory
 *
 * SUMMARY:
 *   This constraint story models the Leninist-Trotskyist-Stalinist reading of
 *   revolutionary method: that a disciplined vanguard party must seize state
 *   power, smash the bourgeois state machine, and establish a dictatorship of
 *   the proletariat as a transitional form. The reading claims this is the
 *   only historically effective path to socialism. In practice, the
 *   constraint created a highly centralized party-state that suppressed
 *   political pluralism, subordinated autonomous worker organizations, and
 *   generated a new ruling stratum (party cadres and planning apparatus). The
 *   claimed_type is tangled_rope because the arrangement genuinely
 *   coordinates revolutionary seizure and initial socialist construction
 *   (coordination function) while simultaneously extracting autonomy and
 *   surplus from rival socialists and the working class itself (asymmetric
 *   extraction), and requires continuous active enforcement to maintain the
 *   party's monopoly.
 *
 * KEY AGENTS:
 *   - party_cadres: Primary agenda_setter (institutional/identity_locked) — sets and enforces the revolutionary method
 *   - state_planning_apparatus: Primary beneficiary (institutional/constrained) — administers the extracted surplus
 *   - political_pluralists: Primary victim/payer (moderate/trapped) — bear suppression costs
 *   - autonomous_worker_organizations: Primary victim/payer (organized/trapped) — lose autonomous coordination capacity
 *   - proletariat: Dual beneficiary/payer (organized/identity_locked) — receive social gains but lose political autonomy
 *   - revolutionary_theorists: Observer (analytical/analytical) — analyze from outside the constraint
 *   - rival_socialist_factions: Excluded (moderate/trapped) — would object but are structurally silenced
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68).
domain_priors:suppression_score(manifesto_revolutionary_method__vanguard_rupture_reading, 0.82).
domain_priors:theater_ratio(manifesto_revolutionary_method__vanguard_rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__vanguard_rupture_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__vanguard_rupture_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__vanguard_rupture_reading, "Vanguard Party Seizure of State Power as Revolutionary Method").
narrative_ontology:topic_domain(manifesto_revolutionary_method__vanguard_rupture_reading, "political_philosophy/revolutionary_theory").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__vanguard_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__vanguard_rupture_reading, 'f61340ea-b113-4eed-8f5d-4cd5dfed4492').
narrative_ontology:cs_kernel_codification('f61340ea-b113-4eed-8f5d-4cd5dfed4492', distributed).
narrative_ontology:cs_authority_grounding('f61340ea-b113-4eed-8f5d-4cd5dfed4492', lineage).
narrative_ontology:cs_interpretation_layer_present('f61340ea-b113-4eed-8f5d-4cd5dfed4492').
narrative_ontology:cs_reading_relation('f61340ea-b113-4eed-8f5d-4cd5dfed4492', manifesto_revolutionary_method__democratic_gradualism_reading, forecloses).
narrative_ontology:cs_reading_relation('f61340ea-b113-4eed-8f5d-4cd5dfed4492', manifesto_revolutionary_method__council_communist_reading, forecloses).
narrative_ontology:cs_axiom('f61340ea-b113-4eed-8f5d-4cd5dfed4492', foundational, vanguard_party_necessary_for_revolution).
narrative_ontology:cs_axiom_status(vanguard_party_necessary_for_revolution, holdable).
narrative_ontology:cs_axiom_grounding('f61340ea-b113-4eed-8f5d-4cd5dfed4492', vanguard_party_necessary_for_revolution, instrumental).
narrative_ontology:cs_axiom('f61340ea-b113-4eed-8f5d-4cd5dfed4492', foundational, dictatorship_of_proletariat_transitional).
narrative_ontology:cs_axiom_status(dictatorship_of_proletariat_transitional, holdable).
narrative_ontology:cs_axiom_grounding('f61340ea-b113-4eed-8f5d-4cd5dfed4492', dictatorship_of_proletariat_transitional, instrumental).
narrative_ontology:cs_reference_frame('f61340ea-b113-4eed-8f5d-4cd5dfed4492', leninist_vanguardism).
narrative_ontology:cs_drift_state('f61340ea-b113-4eed-8f5d-4cd5dfed4492', post_soviet_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f61340ea-b113-4eed-8f5d-4cd5dfed4492', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__vanguard_rupture_reading, proletariat).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__vanguard_rupture_reading, proletariat).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__vanguard_rupture_reading, vanguardism).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__vanguard_rupture_reading, democratic_centralism).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__vanguard_rupture_reading, dictatorship_of_the_proletariat).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lead the vanguard party, seize state power, and direct the transitional state apparatus. They derive authority, material privileges, and control over the political agenda from their positions. Exit from the party structure entails loss of identity, status, and access to power.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, party_cadres, agenda_setter,
    institutional, generational, identity_locked, national).

% Administer the planned economy and social services, controlling resource allocation and production targets. They benefit from secure positions, status, and the ability to direct economic activity. Exit is constrained by the integration of their expertise into the party-state structure.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, state_planning_apparatus, beneficiary,
    institutional, generational, constrained, national).

% Advocate for multi-party democracy, civil liberties, and political competition. They are systematically suppressed — banned, imprisoned, exiled, or forced underground — because their existence challenges the party's monopoly on power. Their exit options are eliminated by state repression.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, political_pluralists, payer,
    moderate, biographical, trapped, national).

% Independent unions, factory committees, and soviets not controlled by the party. They are either co-opted into the party's transmission belts or dismantled. Their autonomous coordination capacity is extracted to serve the party's central plan.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, autonomous_worker_organizations, payer,
    organized, biographical, trapped, national).

% The class in whose name the dictatorship is exercised. They receive social gains (literacy, healthcare, industrial employment) but lose political autonomy, independent organization, and the right to dissent. Their class identity is fused with the party's claim to represent them, making exit from the arrangement conceptually unavailable.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, proletariat, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__vanguard_rupture_reading, proletariat, payer).

% Analyze the historical outcomes of vanguard-led revolutions, debate the method's validity, and produce the intellectual ammunition for both defenders and critics. They neither collect extraction nor bear its direct costs.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, revolutionary_theorists, observer,
    analytical, civilizational, analytical, global).

% Council communists, left communists, democratic socialists, and anarchists who offer alternative revolutionary methods. They are marginalized, purged, or forced into exile by the vanguard party's suppression of political competition within the workers' movement.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__vanguard_rupture_reading, rival_socialist_factions, excluded,
    moderate, biographical, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Overthrowing capitalist state power and coordinating the transition to socialism through a centralized revolutionary leadership that can act decisively in civil war, economic collapse, and hostile international encirclement.
% TRANSFER_FUNCTION: Moves political power and economic control from bourgeois institutions to a party-state apparatus; extracts autonomy from rival socialist currents and independent worker organizations to concentrate decision-making in the vanguard; transfers surplus from the peasantry and working class to fund industrialization and military defense.
% ABSENT_VOICES: Anarchists, council communists, left-communists, and democratic socialists who were suppressed or exiled; future generations who inherit the institutional structure without having consented to it; peasants subjected to forced collectivization who had no organized representation in the party.
% DISAPPEARANCE_RATIONALE: If the vanguard party seizure model disappeared overnight, revolutionary movements would have to adopt councilist, gradualist, or other coordination forms; the 20th-century state socialist bloc would not have existed in its form; the global communist movement would have fragmented into distinct traditions much earlier.
% FOUNDING_PROBLEM: The problem of achieving proletarian revolution in conditions of bourgeois military dominance, ideological hegemony, and economic power; the need for a disciplined organization to overcome capitalist state repression and coordinate the working class across heterogeneous conditions.
% FOUNDING_PROBLEM_CORROBORATION: Lenin and the Comintern attest the problem was live and required the vanguard form; council communists (Pannekoek, Mattick) and later democratic socialists attest the problem was misdiagnosed and the solution created new domination; historical outcomes in Russia, China, Cuba corroborate both the initial revolutionary success and the subsequent entrenchment of party privilege.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__vanguard_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__vanguard_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__vanguard_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__vanguard_rupture_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(manifesto_revolutionary_method__vanguard_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(manifesto_revolutionary_method__vanguard_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the party-state extracts surplus labor, political autonomy, and organizational independence from the working class and rival socialists, redirecting them to party priorities. Suppression (0.82) is very high because the constraint's persistence depends on banning opposition parties, censoring dissent, controlling unions, and preventing independent political organization. Theater_ratio (0.42) reflects the gap between the transitional rhetoric (withering away of the state) and the permanent institutional reality. Accessibility_collapse (0.73) is high because once the vanguard party seizes power, alternative revolutionary methods (councils, gradualism) are structurally foreclosed — not just discouraged but made organizationally impossible. Resistance (0.61) is significant: from Kronstadt to Prague Spring to Solidarity, the constraint has faced repeated challenges from within its own claimed constituency.
 *
 * PERSPECTIVAL GAP:
 *   From the party_cadres seat, the constraint appears as necessary coordination (rope-like) — the only way to defeat counterrevolution and build socialism. From the political_pluralists and autonomous_worker_organizations seats, it appears as pure extraction (snare-like) — their organizations are destroyed, their members imprisoned. From the proletariat seat, it is experienced as a tangled_rope: real material gains (literacy, healthcare, industrial employment) coupled with loss of political voice and exit. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Party_cadres and state_planning_apparatus are structural beneficiaries (d near 0.0) — they collect the constraint's gains (power, resources, status). Political_pluralists and autonomous_worker_organizations are structural targets (d near 1.0) — they bear the extraction and have trapped exit. Proletariat sits near symmetric (d ~0.5) — they receive real benefits but pay with autonomy, and their identity_lock makes exit conceptually blocked. Rival_socialist_factions are excluded (d undefined) — their suppression is the enforcement mechanism itself, not a byproduct.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (overcoming bourgeois dominance) was live in 1917 but becomes contested thereafter. The arrangement persists long after the conditions that justified it (civil war, encirclement, backwardness) have changed. The sunset clause (withering away of the state) is perpetually deferred. This is a classic mandatrophy case: a transitional scaffold that hardened into a tangled_rope because the party that administers it benefits from its continuation and the victims are too suppressed to force its dissolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vanguard_necessity_vs_power_grab,
    'Is the vanguard party form a genuine coordination necessity for revolutionary success, or a rationalization for party power?',
    'Counterfactual historical analysis: could revolutions have succeeded and defended themselves without vanguard party dictatorship? Compare outcomes in Russia (vanguard), Spain 1936 (councilist), Chile 1970 (gradualist).',
    'If coordination necessity, the extraction is the price of successful revolution (tangled_rope). If power grab, the coordination story is cover and the constraint is a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vanguard_necessity_vs_power_grab, conceptual, 'Whether the vanguard form is structurally necessary or contingently chosen.').

omega_variable(
    transitional_permanence,
    'Is the dictatorship of the proletariat genuinely transitional, or does the party-state have a structural tendency to reproduce itself?',
    'Longitudinal study of all vanguard-led revolutions: measure duration of ''transitional'' phase, rate of power transfer to non-party organs, and correlation between party entrenchment and economic development.',
    'If transitional, the constraint is a scaffold (despite lacking formal sunset). If permanent, it is a tangled_rope or snare with no transitional character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_permanence, empirical, 'Whether the transitional claim is empirically vindicated or falsified by historical trajectory.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of political pluralists and autonomous workers primarily structural (state repression) or internalized (ideological hegemony, identity fusion)?',
    'Post-exit suppression trajectory: in post-Soviet states, did former dissidents remain suppressed after state repression lifted? Did worker self-organization emerge spontaneously or require new cultural formation?',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint survives its own enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for interpersonal/institutional constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__vanguard_rupture_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mrm_vanguard_tr_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1917, 0.2).
narrative_ontology:measurement(mrm_vanguard_tr_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1921, 0.3).
narrative_ontology:measurement(mrm_vanguard_tr_t1928, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1928, 0.38).
narrative_ontology:measurement(mrm_vanguard_tr_t1953, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1953, 0.45).
narrative_ontology:measurement(mrm_vanguard_tr_t1991, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 1991, 0.55).
narrative_ontology:measurement(mrm_vanguard_tr_t2024, manifesto_revolutionary_method__vanguard_rupture_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(mrm_vanguard_be_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1917, 0.45).
narrative_ontology:measurement(mrm_vanguard_be_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1921, 0.58).
narrative_ontology:measurement(mrm_vanguard_be_t1928, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1928, 0.67).
narrative_ontology:measurement(mrm_vanguard_be_t1953, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1953, 0.72).
narrative_ontology:measurement(mrm_vanguard_be_t1991, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 1991, 0.65).
narrative_ontology:measurement(mrm_vanguard_be_t2024, manifesto_revolutionary_method__vanguard_rupture_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mrm_vanguard_su_t1917, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1917, 0.6).
narrative_ontology:measurement(mrm_vanguard_su_t1921, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1921, 0.75).
narrative_ontology:measurement(mrm_vanguard_su_t1928, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1928, 0.85).
narrative_ontology:measurement(mrm_vanguard_su_t1953, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1953, 0.88).
narrative_ontology:measurement(mrm_vanguard_su_t1991, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 1991, 0.7).
narrative_ontology:measurement(mrm_vanguard_su_t2024, manifesto_revolutionary_method__vanguard_rupture_reading, suppression_requirement, 2024, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__vanguard_rupture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(manifesto_revolutionary_method__vanguard_rupture_reading, 0.1).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__democratic_gradualism_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__vanguard_rupture_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single label 'revolutionary method' into three structurally distinct constraints with different ε values: vanguard_rupture (high extraction, high suppression), democratic_gradualism (low extraction, low suppression), council_communist (moderate extraction, variable suppression). They are linked because each reading cites the same kernel (Marx/Englen revolutionary theory) but instantiates different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(manifesto_revolutionary_method__vanguard_rupture_reading, institutional, 0.1).
constraint_indexing:directionality_override(manifesto_revolutionary_method__vanguard_rupture_reading, organized, 0.55).
constraint_indexing:directionality_override(manifesto_revolutionary_method__vanguard_rupture_reading, moderate, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
