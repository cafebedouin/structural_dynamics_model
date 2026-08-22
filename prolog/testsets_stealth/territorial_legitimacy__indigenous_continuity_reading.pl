% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy via Continuous Indigenous Habitation (1948 as Nakba)
 *   domain: political theory/international law/territorial sovereignty
 *
 * SUMMARY:
 *   The standing arrangement under contest is the post-1948 territorial
 *   settlement in historic Palestine: a state consolidated through the
 *   displacement of roughly three-quarters of a million Palestinians,
 *   extended since 1967 over the remaining territories through military
 *   occupation and settlement. This story instantiates ONE reading of the
 *   territorial_legitimacy kernel — the indigenous_continuity_reading, under
 *   which legitimate sovereignty flows from continuous habitation and
 *   anti-colonial self-determination, and 1948 is a catastrophe (Nakba)
 *   rather than a ratified partition. Per the ε-referent rule, ε is authored
 *   for the standing arrangement as this reading sees it — settler-colonial
 *   dispossession — and NOT for the decolonized arrangement this reading
 *   endorses; the endorsed alternative appears nowhere in the metrics. The
 *   sibling readings (partition_reading, security_necessity_reading) are
 *   separate constraints in separate files, linked via
 *   network.affects_constraints; the contest between readings is carried in
 *   omega variables, not folded into this story's classification. KEY AGENTS
 *   (by structural relationship): - palestinian_refugee_diaspora: Primary
 *   target (powerless/trapped) — bears the settlement's core costs: land,
 *   homes, nationality, return - palestinians_in_occupied_territories:
 *   Primary target (powerless/trapped) — lives under direct military
 *   administration and blockade - palestinian_citizens_of_israel: Secondary
 *   target (moderate/constrained) — formal citizenship inside the state
 *   constituted, on this reading, as the instrument of their dispossession -
 *   israeli_state_apparatus: Agenda-setter and principal collector
 *   (institutional/arbitrage) — administers the settlement and receives its
 *   gains - israeli_jewish_settler_society: Principal beneficiary
 *   (powerful/constrained) — receives land, housing, and preferential
 *   citizenship - great_power_patron: Secondary beneficiary and enforcement
 *   enabler (institutional/arbitrage) - international_legal_bodies:
 *   Analytical observer (institutional/analytical) — sees the full structure,
 *   commands no enforcement
 *
 * KEY AGENTS:
 *   - palestinian_refugee_diaspora: primary target (powerless/trapped)
 *   - palestinians_in_occupied_territories: primary target (powerless/trapped)
 *   - palestinian_citizens_of_israel: secondary target (moderate/constrained)
 *   - israeli_state_apparatus: agenda-setter and principal collector (institutional/arbitrage)
 *   - israeli_jewish_settler_society: principal beneficiary (powerful/constrained)
 *   - great_power_patron: secondary beneficiary and enforcement enabler (institutional/arbitrage)
 *   - international_legal_bodies: analytical observer (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.94).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.93).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.94).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.93).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy via Continuous Indigenous Habitation (1948 as Nakba)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political theory/international law/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, '58bf66b5-7d68-4745-b167-464e1c421ad2').
narrative_ontology:cs_kernel_codification('58bf66b5-7d68-4745-b167-464e1c421ad2', formalized).
narrative_ontology:cs_authority_grounding('58bf66b5-7d68-4745-b167-464e1c421ad2', distributed).
narrative_ontology:cs_reading_relation('58bf66b5-7d68-4745-b167-464e1c421ad2', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('58bf66b5-7d68-4745-b167-464e1c421ad2', territorial_legitimacy__security_necessity_reading, forecloses).
narrative_ontology:cs_axiom('58bf66b5-7d68-4745-b167-464e1c421ad2', foundational, indigenous_habitation_confers_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_habitation_confers_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('58bf66b5-7d68-4745-b167-464e1c421ad2', indigenous_habitation_confers_sovereignty, deontological).
narrative_ontology:cs_axiom('58bf66b5-7d68-4745-b167-464e1c421ad2', foundational, mass_displacement_voids_successor_title).
narrative_ontology:cs_axiom_status(mass_displacement_voids_successor_title, holdable).
narrative_ontology:cs_axiom_grounding('58bf66b5-7d68-4745-b167-464e1c421ad2', mass_displacement_voids_successor_title, deontological).
narrative_ontology:cs_axiom('58bf66b5-7d68-4745-b167-464e1c421ad2', secondary, right_of_return_individual_and_inalienable).
narrative_ontology:cs_axiom_status(right_of_return_individual_and_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('58bf66b5-7d68-4745-b167-464e1c421ad2', right_of_return_individual_and_inalienable, conventional).
narrative_ontology:cs_reference_frame('58bf66b5-7d68-4745-b167-464e1c421ad2', pre_1948_indigenous_majority_palestine).
narrative_ontology:cs_drift_state('58bf66b5-7d68-4745-b167-464e1c421ad2', post_2024_gaza_war, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('58bf66b5-7d68-4745-b167-464e1c421ad2', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, israeli_jewish_settler_society).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinians_in_occupied_territories).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, great_power_patron).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Descendants of the roughly 750,000 Palestinians displaced in 1948, registered with UNRWA across Lebanon, Syria, Jordan, the West Bank, and Gaza. Many retain deeds and keys to homes now inside Israel. They hold no state; host states range from partial integration to legal exclusion; physical entry to former homes is barred. Exit in practice means permanent resettlement elsewhere and abandonment of the return claim.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, global).

% Live under Israeli military administration in the West Bank — area-based jurisdictional fragmentation, settlement expansion, and a permit regime governing movement, building, and water — and under blockade in Gaza. Market access, travel, and civic life run through controls they do not administer. Exit means emigration through restrictive permit channels, severing residence from any future claim.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinians_in_occupied_territories, payer,
    powerless, biographical, trapped, regional).

% Hold Israeli citizenship and vote, but sit inside a state whose land-planning, immigration law (the Law of Return does not extend to them), and budget allocation treat their localities unequally. Emigration is legally available but cuts them off from family networks and from the only polity where they hold formal standing.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).

% Administers the settlement: the Law of Return, custodianship of absentee property, settlement administration, military government in the occupied territories, and diplomatic defense of the territorial status quo abroad. Land, housing stock, water allocations, tax base, and sovereign authority over the territory flow to it. Its position lets it redraw facts on the ground, shift alliances, and reframe the dispute faster than external pressure accumulates.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, regional).

% Receives subsidized housing, preferential land access, military protection, and full citizenship within the settlement. Materially many could emigrate — large numbers hold foreign passports — but home, army service, family graves, and national belonging are bound up with staying, and departure reads socially as betrayal. They also bear conscription and war risk, the settlement's principal costs to them.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_jewish_settler_society, beneficiary,
    powerful, biographical, constrained, national).

% Supplies military aid, funding, and diplomatic cover — including Security Council vetoes shielding the settlement from binding enforcement. In return it receives strategic cooperation, intelligence sharing, and domestic political returns. It can redirect its posture at low cost to itself, which makes its support a chosen input rather than a structural necessity.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, great_power_patron, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, great_power_patron, agenda_setter).

% Issue advisory opinions, register treaty obligations, and record violations bearing on the legitimacy of the territorial settlement. They adjudicate claims but command no enforcement of their own; their effect runs entirely through state compliance and Great Power filtering.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_legal_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__indigenous_continuity_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_legitimacy__indigenous_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves, for the population incorporated within it, the problems of defense, administration, citizenship, and infrastructure at state scale — one army, one legal system, one currency, one infrastructure grid. The displaced and occupied Palestinian population sits outside the coordinated unit.
% TRANSFER_FUNCTION: Moves land titles, housing, water shares, and sovereign authority from the displaced and occupied Palestinian population to the state and the population it incorporates; moves the displaced themselves into permanent exile or subordinate residency.
% ABSENT_VOICES: The refugees were absent from every forum that fixed the map: no Palestinian delegation sat as a party in the 1947 partition deliberations, none signed the armistice lines that became the de facto borders, and the final-status questions — return, Jerusalem, settlements — were deferred out of the Oslo architecture precisely where refugee representatives had the most at stake. Host-state refugee communities additionally lack civic voice inside the states that host them.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight — sovereignty dissolved, return opened — the regional order rearranges: the Egypt–Jordan peace architecture, water-sharing regimes, arms balances, and alliance structures built around the conflict lose their anchor; the incorporated population confronts the return of the displaced onto land now densely built over; and every state that positioned itself relative to the dispute repositions.
% FOUNDING_PROBLEM: After the Holocaust and the collapse of the British Mandate, a stateless and recently exterminated minority sought sovereign refuge and statehood in its historic-religious homeland against regional opposition.
% FOUNDING_PROBLEM_CORROBORATION: No one outside the settlement's beneficiaries attests that the founding problem required permanent displacement: the New Historian archival corpus (writing from inside Israeli state archives) corroborates that the 1948 displacement was systematic rather than incidental; UNRWA registration and host-state records corroborate the scale and continuity of displacement across generations; the refugees' own institutions attest the problem they were left holding. The benefiting parties attest the founding problem remains live as persistent existential threat; outside corroborators attest the refuge problem was resolved for the incorporated population by the early 1950s while the displacement it produced remains unresolved.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.94, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near ceiling (0.94 at interval end) because, on this reading's lights, the settlement's core operation is the transfer of land, homes, water, and sovereignty away from the indigenous population and the permanence of that transfer. Suppression (0.93) is high because persistence depends on actively barring return, administering occupied populations by force, and blockading Gaza — suppression is authored as a raw structural property and is deliberately unscaled; the engine owns any contextual scaling of extractiveness only. Theater is moderate (0.30) and peaks during negotiation episodes: the Oslo-era spike (0.45 in 1993) reflects process substituting for change, while the enforcement core stays brutally functional. Accessibility_collapse (0.62) is below mountain levels because discursive and institutional alternatives persist (the sibling readings remain live internationally) even though the physical alternatives — return, shared sovereignty — have been militarily foreclosed on the ground for decades. Resistance (0.78) reflects sustained multi-generational, multi-front opposition. CYCLICAL PATTERN: the series oscillates rather than drifting monotonically — war and repression (1948, 1967, 1982, 2000, 2024) alternate with liberalization episodes (1950s consolidation-with-citizenship, 1993 Oslo, 2005 disengagement) during which measured pressure eases while consolidation continues underneath. The oscillation is itself part of the maintenance mechanism: each liberalization episode lowered external pressure while facts on the ground accumulated — an intermittent-reinforcement structure, not noise. All three tracked metrics share one time grid (ten points); the base_properties scalars reflect the 2024 endpoint. Claim/metric independence: claimed_type is authored from this reading's structural assessment (pure extraction with the coordination-and-democracy story as cover); the metrics are authored descriptively; neither was tuned to the other or to a predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   From the trapped payer seats the settlement presents as dispossession administered by force; from the agenda-setter and beneficiary seats the identical structure presents as legitimate statehood providing defense, services, and belonging. The engine computes per-seat classifications from power, exit, and directionality — a snare-shaped experience at the trapped seats alongside a rope-shaped experience at the arbitrage seat is the expected divergence, not an inconsistency. The great-power patron adds a third vantage: it experiences the settlement as a cheap, high-return alliance input and can revise that judgment unilaterally.
 *
 * DIRECTIONALITY LOGIC:
 *   The three victim declarations place the Palestinian seats near the full-target end of d; trapped exit amplifies effective extraction for the diaspora and the occupied territories, while the citizens-of-Israel seat's constrained-but-real exit moderates theirs slightly. The state apparatus sits near the beneficiary end with arbitrage-grade exit damping its exposure further. Settler society is a beneficiary whose conscription and war risk keep d slightly above zero rather than at the floor. The patron collects alliance rents while paying little — near-beneficiary despite its enforcement-enabling role. International legal bodies take the analytical seat: no directional stake, full structural visibility.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sovereign refuge for a persecuted minority — and the settlement's present function — maintaining the displacement that refuge was built upon — have diverged, and the parties dispute whether the founding problem is still live. Because status is authored contested rather than dead, the mismatch consumer does not fire a mechanical zombie flag; but the corroboration asymmetry (archival historians writing from inside the beneficiary society attesting systematic displacement) keeps the capture question permanently open. Declaring victims and active enforcement blocks the mislabel this reading most fears — presenting the settlement as pure coordination with the displaced as regrettable collateral — while the omega on attachment-versus-coercion persistence blocks the opposite error of denying any coordination value to the incorporated population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_delta,
    'This constraint instantiates the indigenous_continuity_reading of the territorial_legitimacy kernel; what structural delta would the partition_reading or the security_necessity_reading introduce if instantiated instead?',
    'Generate the sibling stories and compare beneficiary/victim sets, epsilon, and computed types. The declared foreclosure edges predict no single framework holds both readings; cross-story comparison locates the disagreement in the source-of-legitimacy variable itself.',
    'Under the partition_reading, the 1948 lines become the legitimacy anchor and the return demand becomes revisionist rather than restorative; under the security_necessity_reading, post-1967 control becomes defensive rather than extractive. Both siblings invert this reading''s directionality assignments and victim sets wholesale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_delta, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    coercion_vs_attachment_persistence,
    'Does the settlement persist purely through coercion and suppression, or also through genuine coordination value and identity attachment experienced by the incorporated population?',
    'Counterfactual enforcement-removal analysis plus longitudinal measurement of emigration intention, service satisfaction, and identity indicators among the incorporated population; comparison with societies that dissolved comparable settlements voluntarily.',
    'If attachment contributes materially to persistence, the settlement carries a hybrid coordination component even under this reading''s lights and the pure-extraction claim weakens; if coercion dominates, the snare classification is stable across seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_attachment_persistence, empirical, 'Whether persistence rests on coercion alone or coercion plus genuine attachment.').

omega_variable(
    return_insistence_identity_lock,
    'Is the refugee population''s insistence on return structural (legal entitlement plus host-state exclusion) or identity-fused (steadfastness as constitutive commitment)?',
    'Post-settlement trajectories from comparable protracted-displacement cases; preference surveys offering credible implementation guarantees with compensation-versus-return options.',
    'If identity-fused, the refugee seat''s exit condition sits nearer identity_locked than trapped, amplifying effective extraction; if structural, removing host-state barriers would reveal mobile exit and lower the measured lock.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(return_insistence_identity_lock, empirical, 'Structural versus internalized character of the return commitment.').

omega_variable(
    historiographic_epsilon_stability,
    'Do contested historiographies of 1948 (planned transfer versus wartime flight) change the epsilon authored for the standing arrangement?',
    'Assessment of the archival record convergence (New Historian corpus, state archives, captured documents) applying the epsilon-invariance test to the label ''Nakba'': if the observable used to evaluate displacement changes epsilon, the label covers two constraints and decomposes.',
    'If displacement were shown substantially non-systematic, this reading''s foundational axiom loses its factual anchor and the reading collapses toward the partition_reading; the reading''s epsilon is indexed to systematic dispossession being historically accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historiographic_epsilon_stability, empirical, 'Historiographic contingency of the reading''s factual anchor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement_basis(terr_tr_t1948, observed).
narrative_ontology:measurement(terr_tr_t1956, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1956, 0.22).
narrative_ontology:measurement_basis(terr_tr_t1956, observed).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1967, 0.24).
narrative_ontology:measurement_basis(terr_tr_t1967, observed).
narrative_ontology:measurement(terr_tr_t1973, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1973, 0.26).
narrative_ontology:measurement_basis(terr_tr_t1973, observed).
narrative_ontology:measurement(terr_tr_t1982, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1982, 0.28).
narrative_ontology:measurement_basis(terr_tr_t1982, observed).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1993, 0.45).
narrative_ontology:measurement_basis(terr_tr_t1993, observed).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement_basis(terr_tr_t2000, observed).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement_basis(terr_tr_t2005, observed).
narrative_ontology:measurement(terr_tr_t2018, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2018, 0.35).
narrative_ontology:measurement_basis(terr_tr_t2018, observed).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2024, 0.3).
narrative_ontology:measurement_basis(terr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1948, 0.9).
narrative_ontology:measurement_basis(terr_be_t1948, observed).
narrative_ontology:measurement(terr_be_t1956, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1956, 0.86).
narrative_ontology:measurement_basis(terr_be_t1956, observed).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1967, 0.91).
narrative_ontology:measurement_basis(terr_be_t1967, observed).
narrative_ontology:measurement(terr_be_t1973, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1973, 0.89).
narrative_ontology:measurement_basis(terr_be_t1973, observed).
narrative_ontology:measurement(terr_be_t1982, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1982, 0.87).
narrative_ontology:measurement_basis(terr_be_t1982, observed).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1993, 0.78).
narrative_ontology:measurement_basis(terr_be_t1993, observed).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement_basis(terr_be_t2000, observed).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2005, 0.86).
narrative_ontology:measurement_basis(terr_be_t2005, observed).
narrative_ontology:measurement(terr_be_t2018, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2018, 0.9).
narrative_ontology:measurement_basis(terr_be_t2018, observed).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2024, 0.94).
narrative_ontology:measurement_basis(terr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1948, 0.85).
narrative_ontology:measurement_basis(terr_su_t1948, observed).
narrative_ontology:measurement(terr_su_t1956, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1956, 0.82).
narrative_ontology:measurement_basis(terr_su_t1956, observed).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1967, 0.88).
narrative_ontology:measurement_basis(terr_su_t1967, observed).
narrative_ontology:measurement(terr_su_t1973, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1973, 0.86).
narrative_ontology:measurement_basis(terr_su_t1973, observed).
narrative_ontology:measurement(terr_su_t1982, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1982, 0.84).
narrative_ontology:measurement_basis(terr_su_t1982, observed).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1993, 0.72).
narrative_ontology:measurement_basis(terr_su_t1993, observed).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement_basis(terr_su_t2000, observed).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2005, 0.85).
narrative_ontology:measurement_basis(terr_su_t2005, observed).
narrative_ontology:measurement(terr_su_t2018, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2018, 0.87).
narrative_ontology:measurement_basis(terr_su_t2018, observed).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2024, 0.93).
narrative_ontology:measurement_basis(terr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, resource_allocation).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the legitimacy of Israel/Palestine' conflates three structurally distinct claims about the SOURCE of territorial legitimacy. Decomposed per the epsilon-invariance principle into a three-member constraint family: this story (indigenous continuity), territorial_legitimacy__partition_reading (international legal partition and recognition), and territorial_legitimacy__security_necessity_reading (defensive necessity and strategic depth). Each member carries its own epsilon, beneficiary/victim structure, and classification. Downstream citation structure: the partition reading is routinely cited as evidence by the security reading; this reading cites the same instrument corpus (UNGA Resolution 194, Fourth Geneva Convention, ICJ advisory jurisprudence) against both. All members link one another via affects_constraints; orphaning any member would break contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
