% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Border Closure Apparatus (Freedom-of-Movement Reading)
 *   domain: political philosophy/international law/migration
 *
 * SUMMARY:
 *   This story instantiates the freedom-of-movement-primary reading of the
 *   border_control_legitimacy kernel: freedom of movement is treated as a
 *   fundamental human right that territorial sovereignty does not override,
 *   so exclusion at the threshold — as opposed to jurisdictional regulation
 *   of rights once present — is treated as structurally illegitimate. Under
 *   this reading, the border closure apparatus is not a legitimate exercise
 *   of an inherent sovereign power but an extraction and coercion structure
 *   that manufactures a victim class (displaced persons, migrant workers,
 *   asylum seekers, separated families) to protect concentrated domestic
 *   beneficiaries (incumbent labor blocs, the enforcement industry, nativist
 *   political coalitions). This is a DIFFERENT constraint from the
 *   sovereignty_primary reading (which holds exclusion is constitutive of
 *   statehood and would classify the same apparatus as legitimate
 *   coordination) and from the jurisdictional_sovereignty reading (which
 *   holds sovereignty is real but limited to in-territory regulation,
 *   balancing protection against labor needs and consent). Each reading has
 *   its own epsilon, its own beneficiary/victim structure, and its own type —
 *   they are linked as siblings in the same kernel contest, not merged into
 *   one constraint.
 *
 * KEY AGENTS:
 *   - displaced_persons: primary target (powerless/trapped) — bears exclusion and interdiction risk
 *   - undocumented_migrant_workers: primary target (powerless/trapped) — bears deportability and wage suppression
 *   - border_enforcement_agencies: agenda_setter (institutional/arbitrage) — administers the apparatus this reading holds illegitimate
 *   - nativist_political_coalitions: beneficiary/agenda_setter (powerful/arbitrage) — mobilizes and profits politically from closure
 *   - international_human_rights_bodies: analytical observer (institutional/analytical) — documents rights violations without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.71).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.86).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.71).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, snare).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Border Closure Apparatus (Freedom-of-Movement Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political philosophy/international law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, 'bce31707-23f8-493c-98f7-8c8f534766b0').
narrative_ontology:cs_kernel_codification('bce31707-23f8-493c-98f7-8c8f534766b0', distributed).
narrative_ontology:cs_authority_grounding('bce31707-23f8-493c-98f7-8c8f534766b0', distributed).
narrative_ontology:cs_reading_relation('bce31707-23f8-493c-98f7-8c8f534766b0', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_reading_relation('bce31707-23f8-493c-98f7-8c8f534766b0', border_control_legitimacy__jurisdictional_sovereignty, influences).
narrative_ontology:cs_axiom('bce31707-23f8-493c-98f7-8c8f534766b0', foundational, movement_right_prior_to_sovereign_authority).
narrative_ontology:cs_axiom_status(movement_right_prior_to_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('bce31707-23f8-493c-98f7-8c8f534766b0', movement_right_prior_to_sovereign_authority, deontological).
narrative_ontology:cs_axiom('bce31707-23f8-493c-98f7-8c8f534766b0', secondary, exclusion_authority_not_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(exclusion_authority_not_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('bce31707-23f8-493c-98f7-8c8f534766b0', exclusion_authority_not_constitutive_of_statehood, conventional).
narrative_ontology:cs_reference_frame('bce31707-23f8-493c-98f7-8c8f534766b0', universal_movement_right_pre_political).
narrative_ontology:cs_drift_state('bce31707-23f8-493c-98f7-8c8f534766b0', contemporary_securitized_border_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('bce31707-23f8-493c-98f7-8c8f534766b0', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, receiving_state_incumbent_labor_blocs).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_industry).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, nativist_political_coalitions).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_persons).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, undocumented_migrant_workers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, transnational_families).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, receiving_state_general_public).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, receiving_state_general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fleeing conflict, persecution, or economic collapse, they seek entry into a territory where they could rebuild. Under the closure regime they are intercepted, detained, pushed back, or forced into irregular and dangerous crossing routes. Their movement is treated as an act requiring state permission rather than an exercise of a right they already hold.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_persons, payer,
    powerless, biographical, trapped, global).

% Already inside the territory, performing labor the receiving economy depends on, but classified as unauthorized presence. Deportable at any time, they cannot access legal protections, cannot organize openly, and absorb the enforcement risk that lets employers pay below-market wages.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, undocumented_migrant_workers, payer,
    powerless, biographical, trapped, national).

% Entitled under this reading to enter and have their claims heard as an extension of the underlying right of movement, they instead face externalized processing, offshore detention, or metering policies that convert a rights claim into a queue subject to administrative discretion.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Split across the border by visa denial, deportation, or entry bans, they bear indefinite separation. Reunification depends on discretionary state permission rather than being treated as a corollary of the family members' movement right.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, transnational_families, payer,
    powerless, generational, constrained, global).

% Domestic labor constituencies who benefit from restricted labor supply competition. They lobby to maintain closure because it protects wage floors and employment share in sectors exposed to migrant labor, regardless of the human-rights framing of movement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, receiving_state_incumbent_labor_blocs, beneficiary,
    organized, biographical, mobile, national).

% Contractors, surveillance-technology vendors, and detention operators whose revenue scales directly with enforcement intensity. They have a direct financial stake in border closure persisting and intensifying, independent of any security or sovereignty justification.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_industry, beneficiary,
    organized, generational, arbitrage, national).

% Political actors who mobilize electoral support around exclusion, framing closure as protecting national identity and order. They set enforcement policy and control legislative and administrative levers that determine border regime intensity.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, nativist_political_coalitions, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__freedom_of_movement_primary, nativist_political_coalitions, agenda_setter).

% State bodies that administer interdiction, detention, and deportation. They implement and justify the closure regime as sovereign prerogative, controlling the operational apparatus that this reading holds to be structurally illegitimate.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_agencies, agenda_setter,
    institutional, biographical, arbitrage, national).

% Benefits from perceived order and reduced short-term labor-market disruption but also bears the fiscal cost of enforcement infrastructure and the moral and social costs of a system this reading holds to violate a fundamental right on their behalf.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, receiving_state_general_public, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__freedom_of_movement_primary, receiving_state_general_public, payer).

% Monitor state compliance with movement-related human rights obligations, issue findings and recommendations, but lack direct enforcement power over sovereign border administration.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__freedom_of_movement_primary, border_enforcement_industry).
narrative_ontology:fixing_cost_class(border_control_legitimacy__freedom_of_movement_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: There is a genuine coordination residue: states do need some jurisdictional mechanism to allocate services, register presence, and manage rights-bearing status once people are within a territory. This reading holds that function is real but does not require or justify exclusion at the threshold.
% TRANSFER_FUNCTION: The closure apparatus transfers physical safety, labor bargaining power, family unity, and legal personhood away from displaced persons, migrant workers, and asylum seekers, and transfers wage protection, enforcement revenue, and political capital to incumbent labor blocs, the enforcement industry, and nativist coalitions.
% ABSENT_VOICES: Displaced persons, asylum seekers, and migrant workers are structurally excluded from the domestic political processes that set closure policy in the receiving state — they have no vote, no standing, and often no legal representation in the proceedings that determine whether they may enter or remain.
% DISAPPEARANCE_RATIONALE: If border closure authority were withdrawn overnight under this reading, interdiction and deportation infrastructure would lose its legal basis, labor markets would reorganize around open movement, enforcement-industry revenue would collapse, and incumbent labor blocs and nativist coalitions would lose a primary policy lever — a substantial rearrangement of political economy and rights enforcement.
% FOUNDING_PROBLEM: Historically framed as solving the problem of controlling entry to protect national resources, security, and social cohesion — the classical sovereignty justification for exclusion.
% FOUNDING_PROBLEM_CORROBORATION: States and nativist coalitions attest the founding problem (security, resource protection) remains live. International human rights bodies, refugee law scholars, and migrant advocacy organizations — parties outside the enforcement beneficiary set — attest that the empirical security rationale is weak relative to the human cost, and that the 'problem' as currently framed is a constructed justification for maintaining an extraction and exclusion apparatus rather than a genuine, still-live necessity.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.71 at interval end) because, under this reading, the entire coercive apparatus of interdiction, detention, and deportation extracts safety, labor value, and family unity from people who under this reading already hold the right being denied. Suppression is authored even higher (0.86) because persistence depends on continuously suppressing the exit option that the right itself is supposed to guarantee — the suppression is the mechanism, not a side effect. Theater ratio (0.42) reflects that a meaningful share of enforcement activity (security screening framed as necessary) has become performative relative to its stated security rationale, while resistance (0.74) reflects the sustained legal, political, and direct-action resistance mounted by migrants, advocates, and rights bodies against the regime. Accessibility collapse (0.58) is only moderate because, unlike a mountain, alternative arrangements (open movement regimes, regional free-movement zones) are demonstrably viable and exist elsewhere — the collapse is enforced, not natural.
 *
 * PERSPECTIVAL GAP:
 *   From the enforcement agency and nativist-coalition seats, closure reads as a legitimate exercise of protective authority — a rope or tangled rope coordinating security and labor-market stability. From the displaced-person and migrant-worker seats, under this reading's premises, the identical structure is a snare: a rights violation dressed as sovereign prerogative, sustained by suppression of an exit that should not require permission at all. The engine computes both seat-level readings from the same structural data; the divergence itself is the analytical product this reading is built to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Displaced persons, undocumented workers, asylum seekers, and separated families are declared victims with trapped exit options — under this reading their directionality sits at the full-target end because the constraint's entire function, from their position, is to deny them the right the reading holds they already possess. Incumbent labor blocs, the enforcement industry, and nativist coalitions are declared beneficiaries with mobile-to-arbitrage exit — they collect wage protection, contract revenue, or political capital and face no equivalent risk, placing them near the full-beneficiary end. Border enforcement agencies occupy an intermediate institutional position: they administer the apparatus (agenda_setter) but do not personally capture rents from it in the way the enforcement industry does — captured mainly through institutional continuity and mission identity rather than direct financial gain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting national resources and security through entry control) is contested precisely because the reading holds the problem was never a legitimate basis for the current apparatus's scope. Even if some version of the founding problem remains live (states do have interests in managing settlement, service provision, and security screening), this reading holds the apparatus has vastly outgrown that residual function, now serving primarily to protect concentrated domestic rents (labor protection, enforcement contracts, political mobilization) rather than the narrower, more defensible in-territory regulatory function the jurisdictional_sovereignty sibling reading would preserve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    movement_right_universality_scope,
    'Is freedom of movement a universal individual right binding on all states regardless of consent, or a right whose scope is itself determined by the very sovereign jurisdictional authority this reading denies exists prior to it?',
    'Resolution would require settling a foundational question in political philosophy and international law about whether human rights claims can bind state territorial authority without state consent — likely unresolvable by empirical means alone, though the trajectory of customary international law and treaty ratification patterns provides partial evidence.',
    'If the right is genuinely universal and prior to sovereignty, this reading''s classification of the closure apparatus as a snare is well-grounded. If the right''s scope is itself a product of sovereign jurisdictional determination, the jurisdictional_sovereignty reading''s more balanced framing may better describe the actual normative structure, and this reading overstates the illegitimacy of jurisdictional border administration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(movement_right_universality_scope, conceptual, 'Whether freedom of movement is prior to and independent of sovereign authority, or itself a product of jurisdictional determination — the foundational fault line between this reading and its jurisdictional_sovereignty sibling.').

omega_variable(
    kernel_reading_selection_evidence,
    'What observable political, legal, and normative signals would distinguish which of the three sibling readings (freedom_of_movement_primary, jurisdictional_sovereignty, sovereignty_primary) best describes the actual operative legitimacy structure of any given state''s border regime?',
    'Comparative analysis of state practice, judicial reasoning in asylum and deportation cases, ratification and derogation patterns for movement-related human rights instruments, and the arguments states themselves offer when defending or restricting border closure.',
    'If state practice and legal reasoning consistently track jurisdictional_sovereignty''s balancing framework rather than either pole, this reading (freedom_of_movement_primary) would be shown to be a normatively aspirational or minority position rather than a description of the dominant operative legitimacy claim — reclassifying it as a contested normative program rather than an accurate structural account of the current regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'Whether this reading describes the dominant operative legitimacy framework or a minority normative position relative to its siblings.').

omega_variable(
    enforcement_apparatus_delegitimization_mechanism,
    'Does adopting this reading actually delegitimize the specific institutional enforcement apparatus (agencies, detention infrastructure, interdiction operations), or only the normative justification offered for it, leaving the apparatus itself materially unchanged absent political or legal action?',
    'Track whether jurisdictions that formally adopt movement-rights-primary legal frameworks (e.g., regional free-movement zones, expansive asylum jurisprudence) show measurable reductions in enforcement apparatus scale, budget, or activity relative to jurisdictions that do not.',
    'If normative delegitimization under this reading does not translate into material apparatus reduction, the classification captures a legitimacy critique without capturing an operative causal mechanism — suggesting the apparatus''s persistence is driven by the beneficiary structure (labor protection, enforcement industry rents, political mobilization) independent of which reading is normatively ascendant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_apparatus_delegitimization_mechanism, empirical, 'Whether normative delegitimization under this reading has observable material effects on enforcement apparatus scale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement(bord_tr_t8, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 8, 0.28).
narrative_ontology:measurement(bord_tr_t16, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 16, 0.33).
narrative_ontology:measurement(bord_tr_t24, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 24, 0.37).
narrative_ontology:measurement(bord_tr_t32, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 32, 0.4).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(bord_be_t8, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 8, 0.55).
narrative_ontology:measurement(bord_be_t16, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(bord_be_t24, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(bord_be_t32, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 40, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(bord_su_t8, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 8, 0.69).
narrative_ontology:measurement(bord_su_t16, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(bord_su_t24, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(bord_su_t32, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 32, 0.83).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 40, 0.86).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(bord_grid_01, border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse(class), 0, 0.45).
narrative_ontology:measurement(bord_grid_02, border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse(class), 40, 0.66).
narrative_ontology:measurement(bord_grid_03, border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse(individual), 0, 0.4).
narrative_ontology:measurement(bord_grid_04, border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse(individual), 40, 0.62).
narrative_ontology:measurement(bord_grid_05, border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse(organizational), 0, 0.3).
narrative_ontology:measurement(bord_grid_06, border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse(organizational), 40, 0.5).
narrative_ontology:measurement(bord_grid_07, border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse(structural), 0, 0.5).
narrative_ontology:measurement(bord_grid_08, border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse(structural), 40, 0.7).
narrative_ontology:measurement(bord_grid_09, border_control_legitimacy__freedom_of_movement_primary, resistance(class), 0, 0.3).
narrative_ontology:measurement(bord_grid_10, border_control_legitimacy__freedom_of_movement_primary, resistance(class), 40, 0.52).
narrative_ontology:measurement(bord_grid_11, border_control_legitimacy__freedom_of_movement_primary, resistance(individual), 0, 0.2).
narrative_ontology:measurement(bord_grid_12, border_control_legitimacy__freedom_of_movement_primary, resistance(individual), 40, 0.35).
narrative_ontology:measurement(bord_grid_13, border_control_legitimacy__freedom_of_movement_primary, resistance(organizational), 0, 0.35).
narrative_ontology:measurement(bord_grid_14, border_control_legitimacy__freedom_of_movement_primary, resistance(organizational), 40, 0.58).
narrative_ontology:measurement(bord_grid_15, border_control_legitimacy__freedom_of_movement_primary, resistance(structural), 0, 0.25).
narrative_ontology:measurement(bord_grid_16, border_control_legitimacy__freedom_of_movement_primary, resistance(structural), 40, 0.48).
narrative_ontology:measurement(bord_grid_17, border_control_legitimacy__freedom_of_movement_primary, stakes_inflation(class), 0, 0.4).
narrative_ontology:measurement(bord_grid_18, border_control_legitimacy__freedom_of_movement_primary, stakes_inflation(class), 40, 0.68).
narrative_ontology:measurement(bord_grid_19, border_control_legitimacy__freedom_of_movement_primary, stakes_inflation(individual), 0, 0.45).
narrative_ontology:measurement(bord_grid_20, border_control_legitimacy__freedom_of_movement_primary, stakes_inflation(individual), 40, 0.78).
narrative_ontology:measurement(bord_grid_21, border_control_legitimacy__freedom_of_movement_primary, stakes_inflation(organizational), 0, 0.35).
narrative_ontology:measurement(bord_grid_22, border_control_legitimacy__freedom_of_movement_primary, stakes_inflation(organizational), 40, 0.6).
narrative_ontology:measurement(bord_grid_23, border_control_legitimacy__freedom_of_movement_primary, stakes_inflation(structural), 0, 0.3).
narrative_ontology:measurement(bord_grid_24, border_control_legitimacy__freedom_of_movement_primary, stakes_inflation(structural), 40, 0.55).
narrative_ontology:measurement(bord_grid_25, border_control_legitimacy__freedom_of_movement_primary, suppression(class), 0, 0.45).
narrative_ontology:measurement(bord_grid_26, border_control_legitimacy__freedom_of_movement_primary, suppression(class), 40, 0.75).
narrative_ontology:measurement(bord_grid_27, border_control_legitimacy__freedom_of_movement_primary, suppression(individual), 0, 0.5).
narrative_ontology:measurement(bord_grid_28, border_control_legitimacy__freedom_of_movement_primary, suppression(individual), 40, 0.88).
narrative_ontology:measurement(bord_grid_29, border_control_legitimacy__freedom_of_movement_primary, suppression(organizational), 0, 0.4).
narrative_ontology:measurement(bord_grid_30, border_control_legitimacy__freedom_of_movement_primary, suppression(organizational), 40, 0.7).
narrative_ontology:measurement(bord_grid_31, border_control_legitimacy__freedom_of_movement_primary, suppression(structural), 0, 0.55).
narrative_ontology:measurement(bord_grid_32, border_control_legitimacy__freedom_of_movement_primary, suppression(structural), 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the border_control_legitimacy kernel. freedom_of_movement_primary (this story) classifies the border closure apparatus as substantially extractive (snare-leaning) because it holds movement is a prior right sovereignty cannot override. jurisdictional_sovereignty classifies a narrower apparatus (in-territory rights regulation, balanced against genuine protection and labor considerations) with lower extraction and a real coordination function — closer to tangled_rope. sovereignty_primary classifies the identical physical enforcement structure as legitimate coordination constitutive of statehood — closer to rope or mountain from that reading's premises. The three stories share no epsilon value; each is ε-invariant on its own terms and linked here for contamination/family analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
