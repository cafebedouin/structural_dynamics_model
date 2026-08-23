% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Absolute Exclusion Discretion as Constitutive of Statehood (Sovereignty-Primary Reading)
 *   domain: political philosophy/international law/migration
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_primary reading of the
 *   border_control_legitimacy kernel: the claim that territorial sovereignty
 *   entails absolute discretion to exclude non-citizens and that border
 *   control is constitutive of statehood itself. The epsilon referent is the
 *   standing arrangement under contest — the actually-operating exclusion
 *   regime of the contemporary interstate system — assessed by this reading's
 *   own lights, which is why epsilon sits at 0.58 rather than the higher
 *   value a freedom_of_movement_primary authoring would assign to the same
 *   arrangement. Sibling readings are separate constraint files linked
 *   through the network block. The claim/metrics gap is deliberate: the
 *   reading CLAIMS mountain (constitutive, natural, self-executing) while the
 *   authored metrics describe a substantially extractive, actively enforced
 *   arrangement with identifiable beneficiaries and bearers — the divergence
 *   is the false-summit measurement the corpus exists to take. KEY AGENTS (by
 *   structural relationship): - territorial_states: Agenda-setter
 *   (institutional/arbitrage) — administers exclusion, collects fees and
 *   political credit - citizen_majorities: Primary beneficiary
 *   (organized/constrained) — protected membership, funds enforcement -
 *   border_enforcement_industry: Concentrated fiscal beneficiary
 *   (institutional/arbitrage) — appropriations scale with enforcement
 *   intensity - employers_of_undocumented_labor: Beneficiary via precarity
 *   premium (powerful/arbitrage) - smuggling_networks: Shadow-market
 *   beneficiary (organized/arbitrage) - excluded_migrants: Primary target
 *   (powerless/trapped) — bears route mortality, detention, deportation -
 *   asylum_seekers: Target at the doctrine's conceded edge
 *   (powerless/trapped) - undocumented_resident_workers: Target inside the
 *   line (powerless/trapped) - migrant_sending_states: Weak counterparty
 *   (organized/constrained) - international_human_rights_bodies: Analytical
 *   observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.58).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.68).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, mountain).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Absolute Exclusion Discretion as Constitutive of Statehood (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political philosophy/international law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).
domain_priors:emerges_naturally(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, 'd65e9072-d5d0-4624-bc27-68fb04a9f125').
narrative_ontology:cs_kernel_codification('d65e9072-d5d0-4624-bc27-68fb04a9f125', formalized).
narrative_ontology:cs_authority_grounding('d65e9072-d5d0-4624-bc27-68fb04a9f125', lineage).
narrative_ontology:cs_interpretation_layer_present('d65e9072-d5d0-4624-bc27-68fb04a9f125').
narrative_ontology:cs_reading_relation('d65e9072-d5d0-4624-bc27-68fb04a9f125', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('d65e9072-d5d0-4624-bc27-68fb04a9f125', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('d65e9072-d5d0-4624-bc27-68fb04a9f125', foundational, absolute_exclusion_discretion_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(absolute_exclusion_discretion_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('d65e9072-d5d0-4624-bc27-68fb04a9f125', absolute_exclusion_discretion_constitutive_of_statehood, conventional).
narrative_ontology:cs_axiom('d65e9072-d5d0-4624-bc27-68fb04a9f125', foundational, political_community_self_determination_requires_membership_control).
narrative_ontology:cs_axiom_status(political_community_self_determination_requires_membership_control, holdable).
narrative_ontology:cs_axiom_grounding('d65e9072-d5d0-4624-bc27-68fb04a9f125', political_community_self_determination_requires_membership_control, deontological).
narrative_ontology:cs_axiom('d65e9072-d5d0-4624-bc27-68fb04a9f125', secondary, human_rights_obligations_external_to_sovereign_authority).
narrative_ontology:cs_axiom_status(human_rights_obligations_external_to_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('d65e9072-d5d0-4624-bc27-68fb04a9f125', human_rights_obligations_external_to_sovereign_authority, conventional).
narrative_ontology:cs_reference_frame('d65e9072-d5d0-4624-bc27-68fb04a9f125', westphalian_absolute_exclusion_baseline).
narrative_ontology:cs_drift_state('d65e9072-d5d0-4624-bc27-68fb04a9f125', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d65e9072-d5d0-4624-bc27-68fb04a9f125', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_majorities).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, employers_of_undocumented_labor).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, smuggling_networks).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, undocumented_resident_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, territorial_states).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, migrant_sending_states).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, migrant_sending_states).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, un_charter_domestic_jurisdiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim and exercise final authority over who may enter, stay, and work in their territory. Maintain visa regimes, patrol forces, detention and deportation systems, and diplomatic defenses of discretionary exclusion. Justify each tightening as defense of statehood and public order. Collect visa fees, fines, and the political credit that restrictionist positioning earns. Can adjust policy, strike bilateral labor deals, outsource enforcement to neighbors, or sign human-rights instruments selectively — adjustment, not exit.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, territorial_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, territorial_states, beneficiary).

% Hold membership in protected political communities: preferential labor markets, public services, and a bounded demos whose votes set entry rules. Fund enforcement through taxes and supply the electoral majorities that reward restriction. Permanent emigration would surrender the membership advantages, so most stay and press for tighter rather than looser boundaries.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_majorities, beneficiary,
    organized, biographical, constrained, national).

% Border agencies, detention operators, surveillance and barrier contractors, and the consultancies that serve them. Budgets scale with enforcement intensity; each tightening cycle expands contracts, staffing, and technology procurement. The industry organizes to defend and enlarge those appropriations, supplying threat assessments and pilot programs that shape the next round of policy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, border_enforcement_industry, beneficiary,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, border_enforcement_industry, agenda_setter).

% Agriculture, construction, care, and hospitality firms that hire workers whose presence the entry regime renders precarious. Precarity suppresses wages, discourages complaints, and stabilizes a compliant workforce. The firms can relocate, automate, or switch hiring channels as suits them.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, employers_of_undocumented_labor, beneficiary,
    powerful, biographical, arbitrage, national).

% Route brokers, document forgers, and transport operators whose market exists because lawful channels are closed. They charge passage fees that rise with every enforcement surge and can reroute, rebrand, or shift corridors faster than patrols adapt.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, smuggling_networks, beneficiary,
    organized, immediate, arbitrage, continental).

% People who seek entry for work, family, or safety and find no lawful channel open to them. They finance dangerous crossings, absorb detention and deportation, endure route deaths in the thousands annually, and remain ineligible to vote anywhere their fate is decided. Remaining home, moving somewhere equally poor, or risking the crossing are the available choices.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% People fleeing persecution who reach a frontier and request protection. A narrow legal foothold — non-refoulement — is the one opening the discretion doctrine concedes, and it is narrowing: interception at sea, safe-third-country shunts, and procedural hurdles convert the right to ask into a gauntlet. They cannot return home and cannot choose where to ask.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, asylum_seekers, payer,
    powerless, biographical, trapped, global).

% People already inside the territory without status, produced by the gap between labor demand and legal channels. Deportability disciplines their wages and working conditions; reporting abuse invites removal. Years of residence, local ties, and sometimes citizen children bind them in place.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, undocumented_resident_workers, payer,
    powerless, biographical, trapped, national).

% Governments of origin countries whose nationals are turned back or expelled. They negotiate labor agreements from weakness, absorb returnee shocks, depend on remittances that require some mobility to persist, and lose skilled workers through the few channels that do open. Diplomatic protest is their main lever.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, migrant_sending_states, payer,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, migrant_sending_states, beneficiary).

% UNHCR, human-rights treaty bodies, and regional courts that publish findings, issue judgments, and press states on refoulement, detention conditions, and family unity. They command no enforcement force; their influence runs through documented findings, conditional funding, and occasional court orders that states comply with partially or delay.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__sovereignty_primary, border_enforcement_industry).
narrative_ontology:fixing_cost_class(border_control_legitimacy__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains bounded political communities: screens who enters, protects member-preferential labor markets and public-goods regimes from unmanaged inflows, and preserves a demos able to govern itself and set its own membership rules. Security screening, document systems, and orderly processing are solved once, centrally, per state.
% TRANSFER_FUNCTION: Moves life chances, labor, and physical safety away from would-be entrants — denied visas, intercepted crossings, detention, deportation, or years of deportable precarity — toward member populations and intermediary collectors (enforcement contractors, employers of undocumented labor, smuggling networks); moves fiscal resources from taxpayers to the enforcement apparatus.
% ABSENT_VOICES: Excluded migrants themselves: the people the rule most consequentially governs hold no vote, no seat, and no standing in any polity that decides their fate; their objections arrive only through NGO proxies, UNHCR statements, and origin-state diplomacy. Future generations denied entry by today's closures are absent twice over. The public consent that legitimates the arrangement is consent collected exclusively from those it protects.
% DISAPPEARANCE_RATIONALE: If absolute exclusion discretion vanished overnight, labor markets would reorganize around freer movement, welfare-state coalitions would rebuild around new fiscal equilibria, the enforcement industry's revenue base would collapse, remittance corridors and origin-country labor markets would shift, and citizenship would lose its boundary meaning — the interstate system's membership architecture is arranged around this discretion.
% FOUNDING_PROBLEM: After two world wars and imperial collapse, the new interstate order needed to stabilize states' exclusive authority over territory and membership: the UN Charter's domestic-jurisdiction guarantee and the refugee regime's statelessness categories were built to secure sovereign control of who belongs where, so governments could reconstruct welfare states and political communities without interference.
% FOUNDING_PROBLEM_CORROBORATION: State parties and their legal advisers attest the founding problem as live and the discretion as constitutive. Outside the beneficiary set, UNHCR and human-rights treaty bodies, European Court of Human Rights jurisprudence on interception and pushback, and historical scholarship on the Westphalian synthesis attest instead that the absolute-discretion formulation is a constructed doctrine already substantially limited in practice — corroborating the contested status from seats that collect nothing from the arrangement's operation.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.58: by this reading's own lights the arrangement is legitimate authority, so epsilon is authored below what a rights-based reading would assign — but the descriptive record (route mortality, detention economies, deportability wage suppression, closed lawful channels) forces substantial extraction even on sympathetic accounting. Suppression 0.68 reflects the closing of lawful alternatives for most would-be movers; it is unscaled structural coercion concentrated on the powerless seats. Theater 0.34: screening and processing are real functions, but a growing share of activity is symbolic — barriers that route traffic rather than stop it, election-timed surges, externalization agreements that displace rather than reduce movement. Accessibility collapse 0.58: alternatives do not vanish (irregular channels, onward movement, staying put persist), but lawful-option collapse for the poor is near-total. Resistance 0.62: advocacy networks, sanctuary practices, judicial pushback, and the smuggling economy itself. All three temporal series share one nine-point grid (decades 1945–2025); the trajectory is secular hardening, not cyclical — election-cycle oscillation exists but averages out at decade resolution, so no intermittent-reinforcement mechanism is claimed.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter and citizen seats the arrangement computes as genuine coordination: a bounded community managing membership, funded by consented taxation. From the excluded-migrant and undocumented-worker seats the same structure computes as enforced extraction with no exit and no voice. The enforcement-industry seat experiences it as revenue. The engine derives these divergent per-seat classifications from the structural data; this story's mountain claim belongs to the reading's authoring seat and does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for citizen_majorities, border_enforcement_industry, employers_of_undocumented_labor, and smuggling_networks; victim declarations drive high d for excluded_migrants, asylum_seekers, and undocumented_resident_workers, with trapped exit pushing them toward the full-target end. territorial_states carries an agenda_setter role with a secondary beneficiary position — it sets the rules and collects fees and political credit. migrant_sending_states sit ambiguously (remittance gains against returnee losses) and are left to structural derivation rather than override. No directionality overrides are authored: the beneficiary/victim plus exit data already produce the correct ordering, and the shared power atoms across mixed seats (organized covers citizens, smugglers, and sending states alike) would make atom-keyed overrides smear across genuinely different positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification apparatus prevents two opposite mislabels. Reading the arrangement as pure coordination would launder route mortality, the detention economy, and deportability wage suppression behind the membership-protection story; the victim declarations and the enforcement requirement block that. Reading it as pure extraction would erase the genuine function — bounded political communities do solve real membership, security, and public-goods problems, and majorities consent to the arrangement; the beneficiary declarations and consent structure block that. The mountain claim is where the reading overreaches: constitutiveness language converts a policy choice into natural law, and the false-summit signature (mountain plus declared beneficiaries) exists precisely to catch that conversion. Mandatrophy is not resolved: the founding problem (stable state governance of membership) remains contested-live, so no sunset or obsolescence verdict is authored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_constructed_doctrine,
    'Is discretionary exclusion a constitutive, natural attribute of statehood, as this reading claims, or a constructed legal doctrine whose persistence serves identifiable beneficiaries?',
    'Comparative-historical analysis: polities without membership-closure doctrines, pre-Westphalian mobility norms, and free-movement zones (EU internal borders). If bounded-community functioning survives without absolute discretion, the constitutiveness claim fails.',
    'If constructed, the mountain claim is a false summit: the false_summit_mountain signature reclassifies toward tangled_rope, and the enforcement apparatus reads as defended rent rather than natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_constructed_doctrine, empirical, 'Natural-law vs constructed-doctrine status of exclusion discretion (FSM ambiguity).').

omega_variable(
    consent_scope_legitimacy,
    'Does majority consent inside the bordered demos legitimately ground a rule whose primary bearers — the excluded — are barred from the consenting body by design?',
    'Normative analysis of consent theory applied to non-member governance, plus revealed-preference evidence: how origin-country populations rank legal-channel access against current arrangements under hypothetical enfranchisement.',
    'If the consent scope is invalid, the coordination-function half of the justification collapses and the arrangement''s effective extraction rises sharply; if valid, the reading''s legitimacy claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_scope_legitimacy, conceptual, 'Whether demos-bounded consent can legitimate governance of the excluded.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression bearing on would-be migrants structural (patrols, visa walls, route mortality) or internalized (learned illegality, self-exclusion, diminished sense of entitlement to move)?',
    'Post-liberalization trajectory studies: mobility behavior and self-perception after barriers drop (EU 2004 enlargement cohorts). Persistence of avoidance patterns after legal channels open indicates internalization.',
    'Internalized suppression travels with the target after any reform, raising effective suppression above the structural measure and slowing decay of the arrangement''s coercive grip.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression mechanism for excluded populations.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the sovereignty_primary reading of kernel border_control_legitimacy — which structural elements would sibling readings change, and where exactly is the disagreement located?',
    'Not resolvable by data alone: the readings partition at the axiom level. freedom_of_movement_primary negates the exclusion-authority premise outright; jurisdictional_sovereignty keeps the authority but strips absoluteness and adds balancing. Compare the three stories'' victim sets and epsilon values.',
    'Under freedom_of_movement_primary the victim set expands to all blocked movers and epsilon rises well above this story''s value; under jurisdictional_sovereignty the discretion scope narrows and epsilon falls. Classification of the standing arrangement is reading-indexed, not topic-fixed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading among three of the border-legitimacy kernel.').

omega_variable(
    enforcement_apparatus_capture_direction,
    'Does the enforcement industry shape restriction policy (capture driving intensification) or merely respond to policy set for other reasons?',
    'Lobbying-disclosure and appropriations-timing analysis: whether budget expansions precede threat assessments or follow contractor campaigns; comparison of jurisdictions with and without private detention industries.',
    'Capture would mean part of the measured intensification is rent-seeking layered onto a coordination function, raising effective extraction and accelerating the theater_ratio climb.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_apparatus_capture_direction, empirical, 'Direction of causation between enforcement industry growth and policy intensification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t0, border_control_legitimacy__sovereignty_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t0, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t10, border_control_legitimacy__sovereignty_primary, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t10, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t20, border_control_legitimacy__sovereignty_primary, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t20, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t30, border_control_legitimacy__sovereignty_primary, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t30, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t40, border_control_legitimacy__sovereignty_primary, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t40, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t50, border_control_legitimacy__sovereignty_primary, theater_ratio, 50, 0.26).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t50, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t60, border_control_legitimacy__sovereignty_primary, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t60, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t70, border_control_legitimacy__sovereignty_primary, theater_ratio, 70, 0.31).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t70, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_tr_t80, border_control_legitimacy__sovereignty_primary, theater_ratio, 80, 0.34).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(bcl_sovereignty_primary_be_t0, border_control_legitimacy__sovereignty_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t0, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t10, border_control_legitimacy__sovereignty_primary, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t10, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t20, border_control_legitimacy__sovereignty_primary, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t20, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t30, border_control_legitimacy__sovereignty_primary, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t30, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t40, border_control_legitimacy__sovereignty_primary, base_extractiveness, 40, 0.51).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t40, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t50, border_control_legitimacy__sovereignty_primary, base_extractiveness, 50, 0.53).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t50, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t60, border_control_legitimacy__sovereignty_primary, base_extractiveness, 60, 0.55).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t60, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t70, border_control_legitimacy__sovereignty_primary, base_extractiveness, 70, 0.57).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t70, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_be_t80, border_control_legitimacy__sovereignty_primary, base_extractiveness, 80, 0.58).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(bcl_sovereignty_primary_su_t0, border_control_legitimacy__sovereignty_primary, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t0, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t10, border_control_legitimacy__sovereignty_primary, suppression_requirement, 10, 0.32).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t10, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t20, border_control_legitimacy__sovereignty_primary, suppression_requirement, 20, 0.36).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t20, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t30, border_control_legitimacy__sovereignty_primary, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t30, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t40, border_control_legitimacy__sovereignty_primary, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t40, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t50, border_control_legitimacy__sovereignty_primary, suppression_requirement, 50, 0.52).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t50, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t60, border_control_legitimacy__sovereignty_primary, suppression_requirement, 60, 0.57).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t60, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t70, border_control_legitimacy__sovereignty_primary, suppression_requirement, 70, 0.63).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t70, observed).
narrative_ontology:measurement(bcl_sovereignty_primary_su_t80, border_control_legitimacy__sovereignty_primary, suppression_requirement, 80, 0.68).
narrative_ontology:measurement_basis(bcl_sovereignty_primary_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, identity_coordination).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, jurisdictional_sovereignty).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, non_refoulement_asylum_regime).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'border control legitimacy' decomposes into three readings of one kernel, per the epsilon-invariance principle — each reading authors a different epsilon over the same referent arrangement. This story (sovereignty_primary) links both siblings. Upstream/downstream: the sovereignty doctrine historically grounds the enforcement apparatus that freedom_of_movement_primary contests and that jurisdictional_sovereignty moderates. The third edge records dependence on the non-refoulement asylum regime — the one carve-out this reading concedes, treated here as an external limit rather than a constitutive element, which is exactly the structural delta separating this reading from its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
