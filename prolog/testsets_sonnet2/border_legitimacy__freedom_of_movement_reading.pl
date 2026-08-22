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
 *   This story instantiates the freedom-of-movement reading of the contested
 *   border-legitimacy kernel: the claim that freedom of movement is a
 *   fundamental human right and that border restrictions are presumptively
 *   illegitimate barriers to its exercise, justifiable (if at all) only by a
 *   very high burden of proof the ordinary immigration-control apparatus does
 *   not meet. Under this reading, the standing arrangement — routine visa
 *   denial, physical and administrative border enforcement, deportation
 *   infrastructure — functions as an extraction mechanism that converts an
 *   accident of birth (citizenship in a wealthy state) into a durable,
 *   enforced economic and physical-safety premium. This is emphatically NOT
 *   the sovereignty reading (which holds exclusion is a legitimate exercise
 *   of territorial self-governance) or the humanitarian-obligation reading
 *   (which draws a bright line between persecution-based admission and
 *   general economic migration); those are separate constraints with their
 *   own ε values, filed separately and linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - would_be_migrants: primary target (powerless/trapped) — bears the direct cost of exclusion
 *   - displaced_workers_abroad: primary target (powerless/trapped) — bears the wage-arbitrage cost of exclusion
 *   - citizenship_premium_holders: primary beneficiary (powerful/arbitrage) — collects the birthright premium border enforcement protects
 *   - border_enforcement_apparatus: agenda-setter (institutional/arbitrage) — administers and depends on the restriction
 *   - receiving_state_electorates: excluded from this reading's normative weighting despite having political voice generally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__freedom_of_movement_reading, 0.78).
domain_priors:suppression_score(border_legitimacy__freedom_of_movement_reading, 0.82).
domain_priors:theater_ratio(border_legitimacy__freedom_of_movement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(border_legitimacy__freedom_of_movement_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__freedom_of_movement_reading, snare).
narrative_ontology:human_readable(border_legitimacy__freedom_of_movement_reading, "Border Enforcement as Illegitimate Restriction on Freedom of Movement").
narrative_ontology:topic_domain(border_legitimacy__freedom_of_movement_reading, "political_philosophy/migration_studies/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__freedom_of_movement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__freedom_of_movement_reading, 'b4481a5e-4d1d-4cb9-b27c-2e801c416478').
narrative_ontology:cs_kernel_codification('b4481a5e-4d1d-4cb9-b27c-2e801c416478', distributed).
narrative_ontology:cs_authority_grounding('b4481a5e-4d1d-4cb9-b27c-2e801c416478', distributed).
narrative_ontology:cs_reading_relation('b4481a5e-4d1d-4cb9-b27c-2e801c416478', border_legitimacy__sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('b4481a5e-4d1d-4cb9-b27c-2e801c416478', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('b4481a5e-4d1d-4cb9-b27c-2e801c416478', foundational, movement_is_a_universal_human_right).
narrative_ontology:cs_axiom_status(movement_is_a_universal_human_right, holdable).
narrative_ontology:cs_axiom_grounding('b4481a5e-4d1d-4cb9-b27c-2e801c416478', movement_is_a_universal_human_right, deontological).
narrative_ontology:cs_axiom('b4481a5e-4d1d-4cb9-b27c-2e801c416478', foundational, territorial_birthright_confers_no_special_exclusion_privilege).
narrative_ontology:cs_axiom_status(territorial_birthright_confers_no_special_exclusion_privilege, holdable).
narrative_ontology:cs_axiom_grounding('b4481a5e-4d1d-4cb9-b27c-2e801c416478', territorial_birthright_confers_no_special_exclusion_privilege, deontological).
narrative_ontology:cs_reference_frame('b4481a5e-4d1d-4cb9-b27c-2e801c416478', universal_right_to_free_movement).
narrative_ontology:cs_drift_state('b4481a5e-4d1d-4cb9-b27c-2e801c416478', post_cold_war_globalization_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b4481a5e-4d1d-4cb9-b27c-2e801c416478', '').
narrative_ontology:cs_kernel_id(border_legitimacy__freedom_of_movement_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, receiving_state_labor_incumbents).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, border_enforcement_apparatus).
narrative_ontology:constraint_beneficiary(border_legitimacy__freedom_of_movement_reading, citizenship_premium_holders).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, would_be_migrants).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, displaced_workers_abroad).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, refugee_populations_in_transit).
narrative_ontology:constraint_victim(border_legitimacy__freedom_of_movement_reading, diaspora_separated_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek to relocate for work, safety, or family reunification but are turned back at borders, denied visas, or forced into irregular and dangerous crossing routes. Under this reading, their exclusion is a direct rights violation, not a lawful administrative act; the border machinery is the thing standing between them and a right they are entitled to exercise.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, would_be_migrants, payer,
    powerless, biographical, trapped, global).

% Live in labor markets with depressed wages or no work at all, unable to sell their labor where it is most valued because passport-based exclusion forecloses the option. Border enforcement is read here as an extraction mechanism that locks global wage arbitrage in favor of citizens of wealthy states.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, displaced_workers_abroad, payer,
    powerless, biographical, trapped, global).

% Held in transit camps, pushed back at sea, or detained at frontier zones while awaiting adjudication that a general right to move would render largely unnecessary. Their situation is treated, under this reading, as evidence of the border regime's ongoing harm rather than a narrower question of asylum procedure.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, refugee_populations_in_transit, payer,
    powerless, immediate, trapped, global).

% Live divided across borders by visa quotas and family-reunification backlogs, bearing years of separation as a direct cost of a border system this reading holds has no legitimate basis for restricting their reunification.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, diaspora_separated_families, payer,
    powerless, biographical, constrained, global).

% Domestic workers whose wages and job security are, on this reading, artificially propped up by the exclusion of willing foreign labor. They benefit from suppressed labor-market competition even though they rarely frame their position this way.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, receiving_state_labor_incumbents, beneficiary,
    organized, biographical, constrained, national).

% Border and immigration agencies design, fund, and enforce the exclusion regime: visa systems, physical barriers, detention infrastructure, deportation pipelines. They administer the very restriction this reading holds is presumptively illegitimate, and their institutional survival depends on the restriction's continuation.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, border_enforcement_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Citizens of wealthy states hold a 'birthright lottery' premium — access to high-wage labor markets, social services, and political voice purely by accident of birth. Border restriction is what converts this accident into an enforceable, durable advantage; they can travel freely themselves while the same freedom is denied to others.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, citizenship_premium_holders, beneficiary,
    powerful, civilizational, arbitrage, global).

% Voters in destination countries who would object that open movement threatens wage levels, welfare-system solvency, and cultural cohesion are treated, under this reading, as expressing preferences that cannot override a prior human right — their electoral voice is present in politics generally but is not admitted as a legitimate veto within this reading's normative frame.
narrative_ontology:constraint_stakeholder(border_legitimacy__freedom_of_movement_reading, receiving_state_electorates, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__freedom_of_movement_reading, citizenship_premium_holders).
narrative_ontology:fixing_cost_class(border_legitimacy__freedom_of_movement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is no genuine coordination problem this reading recognizes the border regime as solving for the people it restricts — from this reading's vantage, the only 'coordination' occurring is among incumbent citizens and enforcement institutions to preserve exclusive access to a territory and its labor market.
% TRANSFER_FUNCTION: Moves economic opportunity, physical safety, and family proximity away from non-citizens and toward incumbent citizens and the institutions that police the boundary; the citizenship premium is transferred from an accident of birth into an enforced, ongoing extraction from everyone born outside the line.
% ABSENT_VOICES: Would-be migrants themselves are almost never parties to the domestic political processes that set the restrictions binding them — they have no vote, no standing, and no seat in the legislatures whose enforcement apparatus determines their fate.
% DISAPPEARANCE_RATIONALE: If border restrictions vanished overnight under this reading's premise, labor markets would reorganize globally within a generation, wage differentials between rich and poor countries would compress sharply, diaspora families would reunite, and the enforcement apparatus itself would lose its function and funding rationale entirely.
% FOUNDING_PROBLEM: States built border control regimes to manage territorial sovereignty, national security, and orderly labor-market administration in an era of nation-state consolidation.
% FOUNDING_PROBLEM_CORROBORATION: Enforcement agencies and receiving-state governments attest the founding problem (security, orderly administration) remains live. Independent economists studying global wage convergence, human rights bodies documenting migrant deaths at borders, and philosophers of open borders (writing from outside the enforcement and incumbent-labor beneficiary set) attest that the security rationale has become a pretext for what functions, under this reading, as an extraction of the citizenship premium.
narrative_ontology:disappearance_verdict(border_legitimacy__freedom_of_movement_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__freedom_of_movement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__freedom_of_movement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__freedom_of_movement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__freedom_of_movement_reading, 0.78, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.78) because, from this reading's own premises, the entire border-restriction apparatus lacks a legitimating coordination function for the people it restricts — it is pure boundary-maintenance in service of an unearned advantage. Suppression is authored even higher (0.82) because persistence depends on physical barriers, detention, deportation, and increasingly militarized frontier enforcement, not on migrant consent or the absence of alternatives migrants would prefer. Theater ratio is moderate (0.40) — some enforcement genuinely tracks security and public-health functions this reading does not deny exist, but a rising share of enforcement activity (reflected in the temporal series) polices ordinary economic migration that this reading holds carries no legitimate restriction basis at all. Accessibility collapse is moderate (0.50), reflecting that the restriction is total for the powerless target populations but that political and legal channels for contesting individual cases remain formally open, however ineffective. Resistance is authored high (0.72): this reading treats the substantial global migrant-rights movement, asylum litigation, and cross-border labor organizing as genuine resistance to an unjust arrangement, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Would-be migrants, displaced workers, refugees in transit, and separated families are declared victims because border enforcement's costs land on them directly and they have essentially no exit — trapped by the very border the constraint is about. Citizenship premium holders and the enforcement apparatus are declared beneficiaries: the former collect an unearned, enforced advantage; the latter administers the machinery and depends institutionally on its continuation. Receiving-state labor incumbents are a secondary, less concentrated beneficiary class — they benefit from suppressed labor competition without personally operating the enforcement machinery, which is why their power is authored as organized rather than institutional.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (orderly territorial administration, security) is authored as contested rather than flatly dead, because this reading does not claim border administration solves no problem — it claims the specific restriction on ordinary movement is disproportionate to whatever problem remains live, and that the apparatus has outgrown any legitimating function into a rent-preserving structure. The mismatch the engine should register is: founding_problem_status=contested paired with disappearance_verdict=world_rearranges — a classic capture-flag pattern where an institution that claims a live founding purpose nonetheless administers an arrangement whose removal would fundamentally reorganize who has access to what.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    movement_as_human_right_vs_policy_choice,
    'Is freedom of international movement a genuine human right on par with freedom of internal movement or speech, or is it a policy preference this reading elevates to rights-language to gain rhetorical leverage?',
    'Examination of whether international human rights instruments (e.g., UDHR Art. 13) that guarantee the right to leave any country but not the right to enter any country support a symmetric reading, and whether customary international law has moved toward recognizing a broader entry right.',
    'If movement is a genuine, symmetric right, the classification of border enforcement as extractive is strongly supported. If it is asymmetric by design (a right to exit, not to enter), the sovereignty reading''s legitimacy claim is substantially strengthened and this reading''s ε may be overstated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(movement_as_human_right_vs_policy_choice, conceptual, 'Whether the normative premise of a symmetric right to movement is well-founded or reading-specific.').

omega_variable(
    kernel_reading_location_of_disagreement,
    'Where exactly do the three border-legitimacy readings (freedom_of_movement, sovereignty, humanitarian_obligation) disagree — is it about the existence of a right to enter, the strength of the state''s countervailing interest, or the category of migrant to whom obligations attach?',
    'Structural decomposition of each reading''s foundational axioms (see cs_structure.axioms across the three sibling files) to identify whether the disagreement is a single foreclosing premise or several independently variable parameters.',
    'If the disagreement reduces to a single foreclosing axiom, the readings may not all be simultaneously coherent as coexisting positions within one legal system, which would change how sovereignty_reading and freedom_of_movement_reading interact in the network. If it is a matter of independently variable strength-of-interest weighting, all three readings can coexist as live positions across different jurisdictions and courts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_disagreement, conceptual, 'Locating the precise structural point of disagreement among the three kernel readings.').

omega_variable(
    welfare_system_solvency_tradeoff,
    'Does open movement under this reading''s prescription threaten the fiscal solvency of destination-state welfare systems, and if so, does that threat constitute a legitimate countervailing interest this reading underweights?',
    'Empirical modeling of fiscal impact of substantially open immigration on welfare-state solvency in high-benefit destination countries, compared against labor-market and tax-revenue gains from increased immigration.',
    'If fiscal threat is severe and well-evidenced, this reading''s treatment of receiving_state_electorates as an excluded, non-legitimate voice becomes harder to sustain and ε may be overstated by neglecting a real coordination cost. If fiscal impact is neutral or positive, the extraction framing is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_system_solvency_tradeoff, empirical, 'Whether displaced fiscal/welfare concerns constitute a legitimate coordination interest this reading discounts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__freedom_of_movement_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__freedom_of_movement_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(bord_tr_t8, border_legitimacy__freedom_of_movement_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(bord_tr_t16, border_legitimacy__freedom_of_movement_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(bord_tr_t24, border_legitimacy__freedom_of_movement_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(bord_tr_t32, border_legitimacy__freedom_of_movement_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(bord_tr_t40, border_legitimacy__freedom_of_movement_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bord_be_t8, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(bord_be_t16, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(bord_be_t24, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(bord_be_t32, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 32, 0.75).
narrative_ontology:measurement(bord_be_t40, border_legitimacy__freedom_of_movement_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(bord_su_t8, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(bord_su_t16, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(bord_su_t24, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(bord_su_t32, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement(bord_su_t40, border_legitimacy__freedom_of_movement_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__sovereignty_reading).
narrative_ontology:affects_constraint(border_legitimacy__freedom_of_movement_reading, border_legitimacy__humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'border legitimacy' per the ε-invariance principle. freedom_of_movement_reading authors high ε for the standing restriction regime (presumptively illegitimate, extractive). sovereignty_reading authors low ε for the same standing regime (a legitimate exercise of territorial self-governance). humanitarian_obligation_reading authors a split ε structure that depends on migrant category (persecution-based vs. economic). All three share the kernel_id border_legitimacy and must remain cross-linked via affects_constraints; none averages or hedges across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
