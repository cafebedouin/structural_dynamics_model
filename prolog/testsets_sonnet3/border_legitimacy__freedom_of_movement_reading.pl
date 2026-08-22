% ============================================================================
% CONSTRAINT STORY: border_legitimacy__freedom_of_movement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: border_legitimacy__freedom_of_movement_reading
 *   human_readable: Border Enforcement as Illegitimate Restriction on Freedom of Movement
 *   domain: political_philosophy/migration_studies/international_law
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested border-legitimacy
 *   kernel: the freedom-of-movement reading, which treats freedom of movement
 *   as a human right and border restriction as presumptively illegitimate.
 *   Under this reading, current border enforcement is not a neutral sovereign
 *   prerogative but an extractive arrangement — it transfers opportunity,
 *   wages, and safety from excluded non-citizens to incumbent citizens and
 *   the enforcement apparatus, while the border's exclusionary function is
 *   enforced through detention, surveillance, and deportation infrastructure.
 *   This is a distinct constraint from the sovereignty_reading (which treats
 *   territorial exclusion as a legitimate exercise of state authority,
 *   near-mountain in that reading's own terms) and the
 *   humanitarian_obligation_reading (which draws a narrower line admitting
 *   only those fleeing persecution or disaster). Each reading has its own ε,
 *   its own beneficiary/victim structure, and its own classification; they
 *   are linked, not merged.
 *
 * KEY AGENTS:
 *   - would_be_migrants: primary target (powerless/trapped) — bears foreclosed opportunity and physical risk
 *   - displaced_persons_denied_entry: primary target (powerless/trapped) — bears direct exclusion costs
 *   - receiving_state_incumbent_workers: beneficiary (organized/mobile) — protected labor market
 *   - border_enforcement_industry: beneficiary (institutional/arbitrage) — profits from enforcement volume
 *   - receiving_state_governments: agenda_setter (institutional/analytical) — administers and could relax the restriction
 *   - human_rights_bodies_and_migration_scholars: analytical observer — supplies the normative case for this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.81).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.78).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Enforcement as Illegitimate Restriction on Freedom of Movement").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, 'b519b385-fce2-4d23-9f98-0d0598c5288c').
narrative_ontology:cs_kernel_codification('b519b385-fce2-4d23-9f98-0d0598c5288c', distributed).
narrative_ontology:cs_authority_grounding('b519b385-fce2-4d23-9f98-0d0598c5288c', distributed).
narrative_ontology:cs_reading_relation('b519b385-fce2-4d23-9f98-0d0598c5288c', border_legitimacy__sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('b519b385-fce2-4d23-9f98-0d0598c5288c', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('b519b385-fce2-4d23-9f98-0d0598c5288c', foundational, movement_is_a_universal_human_right).
narrative_ontology:cs_axiom_status(movement_is_a_universal_human_right, holdable).
narrative_ontology:cs_axiom_grounding('b519b385-fce2-4d23-9f98-0d0598c5288c', movement_is_a_universal_human_right, deontological).
narrative_ontology:cs_axiom('b519b385-fce2-4d23-9f98-0d0598c5288c', foundational, burden_of_justification_falls_on_the_excluding_state).
narrative_ontology:cs_axiom_status(burden_of_justification_falls_on_the_excluding_state, holdable).
narrative_ontology:cs_axiom_grounding('b519b385-fce2-4d23-9f98-0d0598c5288c', burden_of_justification_falls_on_the_excluding_state, deontological).
narrative_ontology:cs_reference_frame('b519b385-fce2-4d23-9f98-0d0598c5288c', universal_right_to_relocate).
narrative_ontology:cs_drift_state('b519b385-fce2-4d23-9f98-0d0598c5288c', contemporary_migration_governance_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b519b385-fce2-4d23-9f98-0d0598c5288c', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, receiving_state_incumbent_workers).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, citizenship_premium_holders).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, would_be_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_persons_denied_entry).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, informal_undocumented_workers).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, family_members_separated_by_border).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek to relocate for work, safety, or family reunification but are barred by passport-based entry regimes, visa quotas, and physical border enforcement. On this reading their movement is a right whose exercise is blocked by an arrangement with no principled justification beyond incumbent advantage. Exit from the constraint means either not moving, moving illegally at high risk, or waiting years in quota systems.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, would_be_migrants, payer,
    powerless, biographical, trapped, global).

% Turned back, detained, or routed into indefinite processing by border control regimes that this reading treats as presumptively illegitimate. They bear direct physical and material costs of exclusion — detention, refoulement risk, prolonged transit limbo.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_persons_denied_entry, payer,
    powerless, immediate, trapped, global).

% Having crossed despite the restriction, they live and work without legal status, unable to invoke labor protections or exit the underground economy without triggering enforcement against themselves. The border's afterlife inside the territory is what traps them.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, informal_undocumented_workers, payer,
    powerless, biographical, trapped, national).

% Kept apart from spouses, children, or parents by visa sponsorship backlogs and entry denial. The restriction converts what would be a private family decision into a multi-year administrative gauntlet.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, family_members_separated_by_border, payer,
    powerless, generational, trapped, global).

% Benefit from reduced labor market competition and preserved wage floors in sectors exposed to migrant labor. Their political support sustains restriction as protective; on this reading that protection is extracted from excluded workers' foreclosed opportunity.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, receiving_state_incumbent_workers, beneficiary,
    organized, biographical, mobile, national).

% Contractors, surveillance technology vendors, and detention operators whose revenue depends directly on the volume and intensity of enforcement. They lobby to maintain and expand the restriction regardless of its humanitarian cost.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, border_enforcement_industry, beneficiary,
    institutional, generational, arbitrage, national).

% Citizens of wealthy states who hold a birthright entitlement to residence, labor market access, and social provision unavailable to non-citizens purely by accident of birth. On this reading their citizenship functions as an inherited rent the border exists to protect.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, citizenship_premium_holders, beneficiary,
    moderate, civilizational, arbitrage, global).

% Set visa policy, staff and fund border enforcement, and adjudicate entry claims. They administer the restriction and could, in principle, open the border unilaterally; this reading treats their continued enforcement as the maintained mechanism of extraction rather than a neutral administrative function.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, receiving_state_governments, agenda_setter,
    institutional, generational, analytical, national).

% Document detention conditions, refoulement, and family separation, and articulate the freedom-of-movement claim as a matter of human rights law and moral philosophy. They have no enforcement power but supply the normative framework this reading draws on.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, human_rights_bodies_and_migration_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__freedom_of_movement_reading, diffuse).
narrative_ontology:fixing_cost_class(border_legitimacy__freedom_of_movement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a residual coordination function this reading concedes only narrowly: some border administration (identity verification, epidemic screening, criminal-record checks) solves genuine information problems for receiving communities. The reading holds that this thin function does not require categorical exclusion by nationality.
% TRANSFER_FUNCTION: The arrangement moves opportunity, wage premiums, and social provision from would-be migrants (who bear foreclosed income, family separation, and physical risk) to incumbent citizens and the enforcement apparatus that materially profits from maintaining the restriction.
% ABSENT_VOICES: Would-be migrants and stateless persons have no vote, no standing, and no representation in the states whose border policy determines their life prospects — they are structurally excluded from the demos that authors the restriction against them.
% DISAPPEARANCE_RATIONALE: If nationality-based entry restriction vanished overnight, global labor markets would reallocate substantially, wage differentials between rich and poor countries would compress, detention and enforcement industries would lose their core function, and hundreds of millions of currently-excluded people would exercise relocation options presently foreclosed to them.
% FOUNDING_PROBLEM: Modern passport and visa regimes were built to manage wartime security concerns, control labor supply during industrialization, and consolidate the nation-state's claim to a bounded, governable population.
% FOUNDING_PROBLEM_CORROBORATION: Migration historians and international law scholars (a source outside the beneficiary set) document that passport controls were a 20th-century innovation tied to wartime exigency and labor protectionism rather than an ancient or natural feature of sovereignty; incumbent-worker unions and enforcement contractors, by contrast, assert the restriction addresses a live and permanent problem of resource and labor-market management — the corroborating outside account undercuts, but does not fully resolve, that claim.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored high (0.81 at interval end) because, on this reading's own terms, the restriction transfers real value (labor market access, wages, family unity, safety) from excluded non-citizens to incumbents and enforcement contractors, with no principled justification the reading accepts as legitimate. Suppression is high (0.78) because the restriction is maintained through active coercive infrastructure — detention, deportation, surveillance — not through voluntary compliance. Theater ratio (0.42) reflects that a meaningful share of enforcement activity (identity checks unrelated to nationality exclusion, symbolic deterrence measures) is performative relative to any legitimate residual function this reading concedes. Accessibility collapse (0.6) is moderate rather than near-total because informal and irregular migration routes persist despite enforcement, meaning alternatives to compliance are suppressed but not eliminated. Resistance (0.72) is high, reflecting active migrant-rights advocacy, sanctuary movements, and litigation challenging enforcement practices.
 *
 * DIRECTIONALITY LOGIC:
 *   Would-be migrants, displaced persons, undocumented workers, and separated families are declared victims because the restriction's entire function, on this reading, is to block their movement — they carry the reading's derived high directionality toward full-target. Incumbent workers, the enforcement industry, and citizenship-premium holders are declared beneficiaries because they retain or gain value (wage protection, contract revenue, birthright entitlement) precisely because the restriction excludes others — low directionality, subsidized position. Receiving-state governments are the agenda_setter: they administer the enforcement machinery and could relax it, which is why this reading treats their continued enforcement, not an external constraint, as the operative extractive mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview surfaces a status = contested verdict deliberately: labor historians corroborate (from outside the beneficiary set) that modern passport/visa regimes are a 20th-century administrative innovation tied to wartime and labor-protectionist motives, not an eternal feature of political order — while incumbent beneficiaries assert the problem (resource and labor-market management) remains fully live. This reading does not resolve that dispute; it registers that the disappearance_verdict (world_rearranges) and founding_problem_status (contested) together flag a possible capture/zombie pattern worth downstream scrutiny, without treating the founding narrative as self-validating.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_border_legitimacy,
    'Which reading of the border_legitimacy kernel is structurally correct: freedom_of_movement_reading (this story), sovereignty_reading, or humanitarian_obligation_reading — and is the disagreement resolvable or a genuine normative fork?',
    'No empirical resolution mechanism exists; the disagreement is normative/conceptual. Comparative political philosophy and international law scholarship can clarify the structure of the disagreement (e.g., whether sovereignty claims can survive scrutiny once decoupled from historical conquest and arbitrary birthplace allocation) but cannot adjudicate it definitively. Track institutional and treaty-body drift (e.g., expanding vs. contracting refugee/migrant rights jurisprudence) as an indicator of which reading is gaining practical ground.',
    'If the sovereignty_reading is treated as structurally correct instead, victim status for excluded migrants dissolves, ε for the same enforcement apparatus drops sharply, and the constraint reclassifies toward mountain-or-rope (legitimate exercise of self-governance) rather than snare. If the humanitarian_obligation_reading is adopted, only the subset of persecution/disaster-fleeing migrants remains in the victim set, substantially lowering ε relative to this reading''s economic-migrant-inclusive scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_border_legitimacy, conceptual, 'Which kernel reading is structurally authoritative, and what the disagreement''s location is.').

omega_variable(
    sibling_reading_structural_delta,
    'What specific structural element do the three readings differ on, precisely?',
    'Textual and doctrinal analysis: the disagreement is located in whether the burden of justification for exclusion falls on the state (this reading, humanitarian_obligation_reading for the persecution subset) or on the migrant (sovereignty_reading), and in how broadly the class of migrants entitled to a justification-shifting claim is drawn (universal for this reading, persecution/disaster-limited for humanitarian_obligation_reading, empty for sovereignty_reading).',
    'Locating the disagreement in burden-of-proof allocation and claimant-class scope, rather than in disputed facts about migration''s economic or social effects, clarifies that the three readings are not resolvable by better empirical data on migration outcomes alone — they rest on different foundational premises about who owes whom a justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Precise location of the structural disagreement among sibling readings.').

omega_variable(
    enforcement_apparatus_capture,
    'Has the border enforcement industry''s profit motive independently entrenched restriction beyond what any of the three normative readings would separately justify, turning a contested normative question into a captured administrative reality?',
    'Track lobbying expenditure, contract renewal patterns, and enforcement-budget growth rates against migration-flow and public-opinion trends; divergence (enforcement growing while flows and public support for restriction are flat or declining) would indicate capture independent of the underlying normative dispute.',
    'If capture is established, part of the measured extraction is attributable not to any coherent border-legitimacy position but to rent-seeking by the enforcement apparatus itself — a distinct extractive layer riding on top of whichever normative reading currently holds institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_apparatus_capture, empirical, 'Whether enforcement-industry rent-seeking has independently entrenched restriction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__freedom_of_movement_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bord_tr_t8, border_legitimacy__freedom_of_movement_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(bord_tr_t16, border_legitimacy__freedom_of_movement_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(bord_tr_t24, border_legitimacy__freedom_of_movement_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(bord_tr_t32, border_legitimacy__freedom_of_movement_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(bord_tr_t40, border_legitimacy__freedom_of_movement_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(bord_be_t8, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(bord_be_t16, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(bord_be_t24, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 24, 0.73).
narrative_ontology:measurement(bord_be_t32, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 32, 0.78).
narrative_ontology:measurement(bord_be_t40, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bord_su_t8, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(bord_su_t16, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(bord_su_t24, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(bord_su_t32, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(bord_su_t40, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__freedom_of_movement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling stories decomposing the natural-language concept 'border legitimacy' per the ε-invariance principle. sovereignty_reading treats the same standing enforcement arrangement as a legitimate exercise of territorial self-governance (low ε, likely mountain-or-rope from its own seat, no victim set for excluded migrants). humanitarian_obligation_reading treats it as legitimate for economic migrants but illegitimate for persecution/disaster claimants (intermediate ε, narrower victim set limited to wrongly-denied asylum seekers). This story (freedom_of_movement_reading) authors the widest victim set and the highest ε, treating the restriction as presumptively illegitimate across the board. All three share the same underlying kernel — the standing border-enforcement arrangement — and must be read together, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
