% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Second Amendment Militia-Conditioned Boundary Reading
 *   domain: constitutional_law/firearms_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the militia_conditioned_reading of the
 *   second_amendment_boundary kernel. The reading holds that the Second
 *   Amendment's prefatory clause ('A well regulated Militia, being necessary
 *   to the security of a free State') grammatically and teleologically bounds
 *   the operative clause ('the right of the people to keep and bear Arms,
 *   shall not be infringed') to a collective defense context — specifically,
 *   the preservation of state-organized militia capacity. Consequently,
 *   comprehensive firearms regulation is presumptively constitutional,
 *   subject only to means-end scrutiny. The constraint's coordination
 *   function is public safety through democratic regulation; its extraction
 *   falls on gun owners whose possession is restricted (collectors,
 *   self-defense claimants in high-regulation jurisdictions). The measurement
 *   series traces the reading's operational life from founding (low
 *   extraction, minimal theater) through the Miller-era regulatory consensus
 *   (peak extraction, rising theater) to its doctrinal displacement by
 *   Heller/Bruen (declining extraction, peak theater as the militia rationale
 *   becomes performative cover for a rejected framework).
 *
 * KEY AGENTS:
 *   - state_legislatures: Primary agenda_setter (enact regulation) / beneficiary (regulatory authority validated) — institutional / generational / arbitrage / national
 *   - federal_courts: Agenda_setter (interpretive authority) — institutional / generational / analytical / national
 *   - restricted_gun_owners: Primary payer (bear regulatory costs) — organized / biographical / constrained / national
 *   - collectors: Payer (possession bans, registration) — moderate / biographical / constrained / national
 *   - self_defense_claimants_high_regulation_jurisdictions: Payer (carry restrictions, waiting periods) — moderate / immediate / constrained / regional
 *   - public_safety_advocates: Beneficiary (regulatory tools enabled) — organized / biographical / mobile / national
 *   - law_enforcement_agencies: Beneficiary (regulatory enforcement tools) — institutional / generational / arbitrage / national
 *   - legal_scholars_militia_view: Observer (analytical seat) — analytical / civilizational / analytical / universal
 *   - legal_scholars_individual_right_view: Excluded (would object but reading's framework excludes them) — analytical / civilizational / analytical / universal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.68).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.72).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Second Amendment Militia-Conditioned Boundary Reading").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '1d746ce8-1e57-404b-a2e4-a17747565dfb').
narrative_ontology:cs_kernel_codification('1d746ce8-1e57-404b-a2e4-a17747565dfb', fixed_text).
narrative_ontology:cs_authority_grounding('1d746ce8-1e57-404b-a2e4-a17747565dfb', lineage).
narrative_ontology:cs_interpretation_layer_present('1d746ce8-1e57-404b-a2e4-a17747565dfb').
narrative_ontology:cs_reading_relation('1d746ce8-1e57-404b-a2e4-a17747565dfb', second_amendment_boundary__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('1d746ce8-1e57-404b-a2e4-a17747565dfb', second_amendment_boundary__insurrectionist_reading, forecloses).
narrative_ontology:cs_axiom('1d746ce8-1e57-404b-a2e4-a17747565dfb', foundational, prefatory_clause_bounds_operative_clause).
narrative_ontology:cs_axiom_status(prefatory_clause_bounds_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('1d746ce8-1e57-404b-a2e4-a17747565dfb', prefatory_clause_bounds_operative_clause, conventional).
narrative_ontology:cs_axiom('1d746ce8-1e57-404b-a2e4-a17747565dfb', foundational, collective_defense_only_purpose).
narrative_ontology:cs_axiom_status(collective_defense_only_purpose, holdable).
narrative_ontology:cs_axiom_grounding('1d746ce8-1e57-404b-a2e4-a17747565dfb', collective_defense_only_purpose, conventional).
narrative_ontology:cs_axiom('1d746ce8-1e57-404b-a2e4-a17747565dfb', secondary, state_regulatory_authority_presumed_legitimate).
narrative_ontology:cs_axiom_status(state_regulatory_authority_presumed_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('1d746ce8-1e57-404b-a2e4-a17747565dfb', state_regulatory_authority_presumed_legitimate, conventional).
narrative_ontology:cs_reference_frame('1d746ce8-1e57-404b-a2e4-a17747565dfb', founding_era_militia_constitutionalism).
narrative_ontology:cs_drift_state('1d746ce8-1e57-404b-a2e4-a17747565dfb', post_bruen_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('1d746ce8-1e57-404b-a2e4-a17747565dfb', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, restricted_gun_owners).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, collectors).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants_high_regulation_jurisdictions).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, collective_defense_constitutionalism).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, state_police_power_primacy).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, prefatory_clause_interpretive_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact firearms regulations under the militia-conditioned reading's constitutional authorization. Their regulatory authority is validated by this reading; they benefit from the interpretive framework that presumes regulatory legitimacy. Exit is arbitrage-grade — they can shift regulatory strategies across jurisdictions and policy domains.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_legislatures, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, state_legislatures, beneficiary).

% Adjudicate Second Amendment challenges. Under this reading, courts apply means-end scrutiny to regulations (intermediate or rational basis), deferring to legislative judgments about militia/public safety. Their interpretive authority is the enforcement mechanism. Exit is analytical — they interpret, not bear, the constraint's costs.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Bear the costs of comprehensive regulation (licensing, registration, bans, waiting periods) enabled by this reading. Organized through advocacy groups (NRA, GOA, SAF) but exit is constrained — cannot easily avoid jurisdiction-specific restrictions without relocating or surrendering protected interests. The reading's framework treats their burden as presumptively legitimate.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, restricted_gun_owners, payer,
    organized, biographical, constrained, national).

% Face possession bans, registration requirements, and transfer restrictions on categories of firearms deemed outside militia utility (e.g., 'dangerous and unusual weapons'). Their specialized interest lacks the political weight of broader gun owner coalitions; exit is constrained by the specificity of their collecting focus and jurisdiction.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, collectors, payer,
    moderate, biographical, constrained, national).

% In jurisdictions with restrictive carry laws (may-issue, good-cause requirements) enabled by this reading, bear the cost of being unable to carry for self-defense. Time horizon is immediate (threat response); exit is constrained by residency, employment, and family ties. Regional scope reflects patchwork jurisdiction.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants_high_regulation_jurisdictions, payer,
    moderate, immediate, constrained, regional).

% Gain regulatory tools (background checks, waiting periods, assault weapon bans, red flag laws) whose constitutional legitimacy this reading secures. Organized through advocacy coalitions; exit is mobile — can shift focus to other policy domains if this reading loses force.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, public_safety_advocates, beneficiary,
    organized, biographical, mobile, national).

% Receive enforcement authority and regulatory frameworks (tracing, dealer licensing, prohibited possessor databases) that this reading constitutionally permits. Institutional power and generational horizon; arbitrage exit through intergovernmental cooperation and federal partnership programs.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, law_enforcement_agencies, beneficiary,
    institutional, generational, arbitrage, national).

% Analytical seat defending the militia-conditioned interpretation through historical linguistics, founding-era sources, and structural constitutional argument. Neither collects nor pays; their role is interpretive contestation within the academy and amicus practice.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, legal_scholars_militia_view, observer,
    analytical, civilizational, analytical, universal).

% Would object to this reading's exclusion of individual self-defense from the Amendment's core. Under this reading's framework, their position is structurally excluded — the prefatory clause's bounding function logically forecloses the individual-right premise. They operate from outside the reading's interpretive community.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, legal_scholars_individual_right_view, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables democratic regulation of firearms for public safety by establishing a constitutional boundary that presumes regulatory legitimacy — solving the collective action problem of balancing individual arms possession against community security through legislative means-end scrutiny rather than judicial invalidation.
% TRANSFER_FUNCTION: Moves regulatory authority from gun owners (who would prefer minimal restriction) to state legislatures and law enforcement (who gain presumptive authority to restrict possession, carry, and transfer), with the transfer mediated by federal courts applying deferential scrutiny.
% ABSENT_VOICES: The excluded seat (legal_scholars_individual_right_view) and the insurrectionist_reading proponents — both would argue the right protects individual self-defense or anti-tyranny capacity unconnected to militia service. They are absent because this reading's interpretive framework defines them out of the constitutional conversation; they re-enter only when the reading loses authoritative force (post-Heller).
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight (as it effectively did in Heller/Bruen), the regulatory presumption flips: restrictions must now satisfy text-history-tradition analysis rather than means-end scrutiny. The firearms regulatory landscape reorganizes — carry regimes shift from may-issue to shall-issue, bans face strict scrutiny, the regulatory floor rises. The world rearranges because arrangements (licensing schemes, possession bans, carry restrictions) depended on this reading's constitutional authorization.
% FOUNDING_PROBLEM: The founding problem was the Anti-Federalist fear that the new federal government could disarm state militias by prohibiting individual arms possession, leaving states defenseless against federal overreach. The Second Amendment was adopted to guarantee that the federal government could not destroy the militia institution by disarming the citizenry from which militias were drawn.
% FOUNDING_PROBLEM_CORROBORATION: The militia institution as founded (universal citizen enrollment, state-organized, federalism check) has been corroborated as dead by: (1) the Militia Act of 1903 (Dick Act) federalizing the militia into the National Guard; (2) the end of universal enrollment; (3) Supreme Court acknowledgment in Heller (Scalia, dissenting in part) and academic consensus (e.g., Cornell, Waldman) that the founding-era militia no longer exists. No corroboration from outside the beneficiary set (state_legislatures, public_safety_advocates) sustains the founding problem as live.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness peaks during the Miller-to-Heller era (1939-2008) when the militia-conditioned reading was the controlling doctrinal framework and federal/state regulation expanded substantially. Theater_ratio rises throughout as the militia justification becomes increasingly decoupled from actual militia organization (the 'well regulated Militia' exists only as a legal fiction). Suppression_requirement tracks enforcement intensity: low at founding (universal militia enrollment made regulation uncontroversial), rising with NFA/GCA/AWB enforcement machinery, declining after Heller as the reading loses authoritative force. The reading's claimed_type is tangled_rope: genuine coordination function (public safety regulation enabled) + asymmetric extraction (gun owners bear costs) + active enforcement (regulatory apparatus). The engine will compute per-seat χ from the declared beneficiaries (state_legislatures, public_safety_advocates, law_enforcement) and victims (restricted_gun_owners, collectors, self_defense_claimants) with their distinct power/exit/scope profiles.
 *
 * PERSPECTIVAL GAP:
 *   From the state_legislatures/federal_courts seat (agenda_setter, institutional power, arbitrage exit), the constraint appears as legitimate coordination — the constitutional boundary that enables democratic regulation. From the restricted_gun_owners/collectors/self_defense_claimants seats (payer, organized/moderate power, constrained exit), the same structure operates as enforced extraction — regulatory costs imposed without militia-service reciprocity. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: state_legislatures (regulatory authority validated), public_safety_advocates (policy tools enabled), law_enforcement_agencies (enforcement tools). These agents have institutional/organized power, generational/biographical horizons, arbitrage/mobile exit, national scope → derived d near beneficiary end (low χ). Victims declared: restricted_gun_owners, collectors, self_defense_claimants_high_regulation_jurisdictions. These agents have organized/moderate power, biographical/immediate horizons, constrained exit, national/regional scope → derived d near target end (high χ). The excluded seat (legal_scholars_individual_right_view) has analytical power but no structural position in this reading's framework — their exclusion is the reading's boundary-maintenance mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (founding_problem) was ensuring state militia capacity against federal disarmament — a live problem in 1791. By the late 20th century, the militia institution had atrophied (National Guard federalized, universal enrollment abandoned) while the reading persisted as the doctrinal basis for comprehensive regulation. The mismatch (founding_problem_status = dead, disappearance_verdict = world_rearranges) flags mandatrophy: the arrangement persists after its founding problem vanished, extracting from gun owners without the coordination reciprocity (militia service) that originally justified it. The theater_ratio trajectory (0.05 → 0.62) tracks this decoupling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of the contested second_amendment_boundary kernel, and does it instantiate a distinct ε from its siblings?',
    'Compare beneficiary/victim structures and metric profiles across individual_right_reading, insurrectionist_reading, and this militia_conditioned_reading; distinct ε values confirm separate constraints per DP-001.',
    'If ε values converge, the kernel label masks a single constraint; if they diverge, the three readings are properly decomposed into three constraint stories linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment to the ε-invariance decomposition of the Second Amendment boundary kernel.').

omega_variable(
    prefatory_operative_relationship,
    'Does the prefatory clause (''well regulated Militia'') grammatically and logically bound the operative clause (''keep and bear Arms''), or does it merely announce a purpose?',
    'Founding-era linguistic corpus analysis, Madison''s drafting history, state ratification convention records, and early judicial commentary (pre-1860).',
    'If prefatory bounds operative, this reading''s ε reflects genuine coordination (public safety regulation); if merely purposive, the reading''s coordination claim is cover for regulatory extraction, shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_operative_relationship, empirical, 'Whether the militia preamble structurally limits the right''s scope.').

omega_variable(
    regulatory_capture_of_militia_concept,
    'Has the ''well regulated Militia'' concept been captured by state regulatory apparatus to justify restrictions beyond any historical militia function?',
    'Trace the evolution of ''militia'' from founding-era universal citizen enrollment to modern selective regulatory triggers (e.g., ''dangerous and unusual weapons'' test, ''longstanding prohibitions'' doctrine); measure gap between historical militia scope and contemporary regulatory reach.',
    'If captured, the coordination function is substantially performative — theater_ratio understates extraction; if not, the reading''s coordination claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_of_militia_concept, empirical, 'Whether the militia concept functions as genuine coordination boundary or regulatory cover.').

omega_variable(
    victim_set_boundary,
    'Are all restricted gun owners (collectors, self-defense claimants in high-regulation jurisdictions) equally positioned as payers, or do sub-classes experience qualitatively different extraction?',
    'Disaggregate the victim set by restriction type (possession bans, carry restrictions, registration, waiting periods) and jurisdiction; measure χ per sub-class using directionality derivation from exit_options and spatial_scope.',
    'If sub-classes diverge (e.g., collectors face prohibitive exit while self-defense claimants retain mobile exit), the single ''restricted_gun_owners'' stakeholder masks seat divergence the engine would compute separately.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_boundary, empirical, 'Granularity of the victim/payer structure within the restricted gun owner population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_militia_reading_tr_t1791, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(sa_militia_reading_tr_t1820, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1820, 0.08).
narrative_ontology:measurement(sa_militia_reading_tr_t1868, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1868, 0.12).
narrative_ontology:measurement(sa_militia_reading_tr_t1934, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1934, 0.25).
narrative_ontology:measurement(sa_militia_reading_tr_t1939, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1939, 0.31).
narrative_ontology:measurement(sa_militia_reading_tr_t1968, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1968, 0.38).
narrative_ontology:measurement(sa_militia_reading_tr_t1994, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 1994, 0.45).
narrative_ontology:measurement(sa_militia_reading_tr_t2004, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2004, 0.48).
narrative_ontology:measurement(sa_militia_reading_tr_t2008, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2008, 0.52).
narrative_ontology:measurement(sa_militia_reading_tr_t2022, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2022, 0.58).
narrative_ontology:measurement(sa_militia_reading_tr_t2024, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 2024, 0.62).

% Extraction over time
narrative_ontology:measurement(sa_militia_reading_be_t1791, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1791, 0.15).
narrative_ontology:measurement(sa_militia_reading_be_t1820, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1820, 0.18).
narrative_ontology:measurement(sa_militia_reading_be_t1868, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1868, 0.22).
narrative_ontology:measurement(sa_militia_reading_be_t1934, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1934, 0.45).
narrative_ontology:measurement(sa_militia_reading_be_t1939, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1939, 0.52).
narrative_ontology:measurement(sa_militia_reading_be_t1968, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1968, 0.61).
narrative_ontology:measurement(sa_militia_reading_be_t1994, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 1994, 0.68).
narrative_ontology:measurement(sa_militia_reading_be_t2004, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2004, 0.65).
narrative_ontology:measurement(sa_militia_reading_be_t2008, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2008, 0.58).
narrative_ontology:measurement(sa_militia_reading_be_t2022, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2022, 0.42).
narrative_ontology:measurement(sa_militia_reading_be_t2024, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sa_militia_reading_su_t1791, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1791, 0.1).
narrative_ontology:measurement(sa_militia_reading_su_t1820, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1820, 0.12).
narrative_ontology:measurement(sa_militia_reading_su_t1868, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1868, 0.18).
narrative_ontology:measurement(sa_militia_reading_su_t1934, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1934, 0.55).
narrative_ontology:measurement(sa_militia_reading_su_t1939, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1939, 0.62).
narrative_ontology:measurement(sa_militia_reading_su_t1968, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1968, 0.71).
narrative_ontology:measurement(sa_militia_reading_su_t1994, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 1994, 0.78).
narrative_ontology:measurement(sa_militia_reading_su_t2004, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2004, 0.75).
narrative_ontology:measurement(sa_militia_reading_su_t2008, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2008, 0.68).
narrative_ontology:measurement(sa_militia_reading_su_t2022, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement(sa_militia_reading_su_t2024, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__militia_conditioned_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, national_firearms_act_regulatory_framework).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, gun_control_act_regulatory_framework).

% DUAL FORMULATION NOTE:
% This constraint story decomposes the 'Second Amendment scope' natural-language concept into three ε-invariant constraints linked as a kernel family. The militia_conditioned_reading (this story) has ε ≈ 0.68 (substantial extraction on gun owners via regulatory enablement). The individual_right_reading has lower ε on gun owners but higher ε on regulatory authorities (depriving them of presumptive legitimacy). The insurrectionist_reading has distinct victim/beneficiary structures (government as target, resistance-capable citizens as beneficiaries). All three linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_boundary__militia_conditioned_reading, institutional, 0.15).
constraint_indexing:directionality_override(second_amendment_boundary__militia_conditioned_reading, organized, 0.75).
constraint_indexing:directionality_override(second_amendment_boundary__militia_conditioned_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
