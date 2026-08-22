% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Geneva Conventions Protective Scope — Universal Rights Reading (Common Article 3 + IHRL Floor)
 *   domain: international_humanitarian_law/legal_theory/armed_conflict
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested Geneva Conventions
 *   protective-scope kernel: the universal rights reading, which holds that
 *   Common Article 3 plus applicable international human rights law
 *   establishes a status-independent floor covering every person affected by
 *   armed conflict, regardless of whether they qualify as a privileged
 *   combatant under Article 4. This is distinct from the state-centric
 *   reading (protection conditioned on meeting combatant criteria) and the
 *   hybrid proportionality reading (protection scaled by conflict-type
 *   classification) — those are separate constraints, not alternative
 *   measurements of this one. Under this reading, the September 11 attacks
 *   and subsequent 'unlawful enemy combatant' detention doctrine sharpened
 *   the practical stakes: state actors sought to argue captured non-state
 *   fighters fell outside Geneva protection entirely, while human rights
 *   bodies and much of customary IHL scholarship held that Common Article 3
 *   applied regardless. The rising extractiveness trajectory reflects the
 *   accumulating body of jurisprudence, monitoring practice, and customary
 *   law crystallization that has hardened this floor against state assertions
 *   of a status-based carve-out.
 *
 * KEY AGENTS:
 *   - state_military_operational_command: primary target — bears constrained operational flexibility
 *   - counterterrorism_intelligence_services: primary target — bears foreclosed interrogation/detention latitude
 *   - non_state_armed_group_members: primary beneficiary — gains guaranteed treatment floor despite unlawful status
 *   - civilian_populations: primary beneficiary — protected regardless of status ambiguity
 *   - human_rights_monitoring_bodies: analytical/beneficiary — gains expanded jurisdictional standing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.62).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.48).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Geneva Conventions Protective Scope — Universal Rights Reading (Common Article 3 + IHRL Floor)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international_humanitarian_law/legal_theory/armed_conflict").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, '74a1f575-d81b-4582-b042-164b0c34eafe').
narrative_ontology:cs_kernel_codification('74a1f575-d81b-4582-b042-164b0c34eafe', fixed_text).
narrative_ontology:cs_authority_grounding('74a1f575-d81b-4582-b042-164b0c34eafe', practice).
narrative_ontology:cs_interpretation_layer_present('74a1f575-d81b-4582-b042-164b0c34eafe').
narrative_ontology:cs_reading_relation('74a1f575-d81b-4582-b042-164b0c34eafe', geneva_conventions_protective_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('74a1f575-d81b-4582-b042-164b0c34eafe', geneva_conventions_protective_scope__hybrid_proportionality_reading, influences).
narrative_ontology:cs_axiom('74a1f575-d81b-4582-b042-164b0c34eafe', foundational, protection_is_status_independent).
narrative_ontology:cs_axiom_status(protection_is_status_independent, holdable).
narrative_ontology:cs_axiom_grounding('74a1f575-d81b-4582-b042-164b0c34eafe', protection_is_status_independent, deontological).
narrative_ontology:cs_axiom('74a1f575-d81b-4582-b042-164b0c34eafe', foundational, common_article_3_constitutes_customary_universal_floor).
narrative_ontology:cs_axiom_status(common_article_3_constitutes_customary_universal_floor, holdable).
narrative_ontology:cs_axiom_grounding('74a1f575-d81b-4582-b042-164b0c34eafe', common_article_3_constitutes_customary_universal_floor, conventional).
narrative_ontology:cs_reference_frame('74a1f575-d81b-4582-b042-164b0c34eafe', id_1949_geneva_drafting_compromise).
narrative_ontology:cs_drift_state('74a1f575-d81b-4582-b042-164b0c34eafe', post_9_11_detention_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('74a1f575-d81b-4582-b042-164b0c34eafe', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_group_members).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, detained_persons_of_unclear_status).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, human_rights_monitoring_bodies).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_command).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, counterterrorism_intelligence_services).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Plans and executes targeting, detention, and interrogation decisions under this reading's requirement that every person affected by the conflict — including unprivileged belligerents, fighters who do not meet Article 4 criteria, and persons of ambiguous status — receives the Common Article 3 floor plus applicable human rights law. Cannot legally treat captured non-state fighters as unprotected; must extend judicial guarantees, humane treatment standards, and non-derogable protections it would otherwise reserve for POWs. Exit from the constraint would require formal withdrawal from treaty obligations or successful reinterpretation through state practice, both politically costly and slow.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_operational_command, payer,
    institutional, immediate, constrained, global).

% Conducts detention and interrogation of captured non-state actors for intelligence value. Under this reading, enhanced interrogation techniques, indefinite detention without judicial process, and status-based exclusion from Geneva protections are foreclosed regardless of how the detainee is classified. Operates under legal risk of prosecution or civil liability if practices are found to violate the universal floor; exit only through political reclassification of the legal regime itself.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, counterterrorism_intelligence_services, payer,
    institutional, immediate, constrained, national).

% Fighters for insurgent, militia, or non-state armed groups who fail Article 4 combatant criteria (no distinctive sign, no responsible command, no compliance with laws of war). Under the state-centric reading they would fall outside treaty protection entirely; under this reading they retain humane treatment guarantees, due process rights, and protection against summary execution or torture regardless of their combatant status. They cannot personally invoke or enforce this floor — protection depends entirely on third-party compliance and monitoring.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_group_members, beneficiary,
    powerless, immediate, trapped, global).

% Non-combatants caught in conflict zones, including those who provide ambiguous support to armed groups (a farmer suspected of harboring fighters, a family member of a combatant). Under this reading their protected status does not evaporate at the margin of doubt — the human rights floor covers them even where combatant-status classification is contested. They have no capacity to compel enforcement themselves; protection is entirely a function of state and monitor compliance.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    powerless, biographical, trapped, regional).

% Individuals captured in conflict whose combatant status is disputed or undetermined — the population the state-centric reading would leave in a protection gap. This reading resolves the gap by extending the Common Article 3 floor automatically rather than requiring status adjudication first. They remain physically detained regardless of the legal classification; the reading changes what treatment they are entitled to, not whether they are held.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, detained_persons_of_unclear_status, beneficiary,
    powerless, biographical, trapped, national).

% ICRC delegations, UN human rights mechanisms, and NGOs that monitor detention and targeting practices. This reading expands their jurisdictional mandate — every person affected by conflict falls within their monitoring remit, not only status-qualified combatants. They gain standing to investigate and report on treatment of any detainee or affected person, which is itself a resource and legitimacy gain even though they bear none of the operational costs.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, human_rights_monitoring_bodies, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__universal_rights_reading, human_rights_monitoring_bodies, observer).

% Allied states conducting joint operations who may hold the hybrid or state-centric reading as a matter of internal doctrine. Their operational planning assumptions are disrupted when a coalition partner unilaterally adopts the universal reading, but they are not parties to the interpretive dispute in any formal sense — they simply absorb the operational friction of interoperability gaps without a seat at the table where the reading is settled.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, coalition_partner_states, excluded,
    institutional, immediate, constrained, national).

% Bodies such as the ICC and ad hoc tribunals that adjudicate individual criminal responsibility for violations of the laws of war. They interpret the scope question when prosecuting or declining to prosecute alleged violations against persons of contested status, effectively selecting among the competing readings case by case without formally endorsing one as universally binding.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_criminal_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__universal_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, status-independent baseline of humane treatment so that no category of person affected by armed conflict falls into a protection vacuum created by contested combatant-status determinations — solving the coordination problem of what floor applies when classification itself is disputed or delayed.
% TRANSFER_FUNCTION: Moves operational flexibility and interrogation/detention discretion away from state military and intelligence services and toward guaranteed treatment standards for non-state fighters, civilians, and status-ambiguous detainees; the cost is borne in intelligence yield, detention efficiency, and battlefield targeting latitude.
% ABSENT_VOICES: Coalition partner states operating under different doctrinal readings are not parties to the interpretive settlement but must absorb interoperability costs; captured persons themselves have no direct voice in how the scope question is resolved despite being the named beneficiaries — their protection is entirely third-party administered.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned in favor of the state-centric reading, unprivileged belligerents and status-ambiguous detainees would lose their claim to Common Article 3 and human rights law protections; interrogation and detention practices currently foreclosed would become legally available; monitoring bodies would lose jurisdictional standing over large detainee populations. Practice on the ground would change immediately and substantially.
% FOUNDING_PROBLEM: The 1949 Conventions and their Additional Protocols left a gap: irregular fighters, civilians of ambiguous allegiance, and persons whose status could not be quickly or reliably determined risked falling outside any treaty protection, particularly as conflicts shifted from state-vs-state wars toward non-international and asymmetric conflicts against non-state armed groups.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's own commentary and customary IHL study attest the gap is real and treat Common Article 3 as a customary floor independent of status determination. State military and intelligence services, corroborated by some allied government legal counsel, attest the founding problem is overstated relative to operational costs and that Article 4 status criteria remain functionally necessary to distinguish lawful combatants from criminal actors; this corroboration comes from a party with a direct stake in narrowing scope, so it should be read as a directly interested position, not a neutral outside check.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects a substantial and rising restriction on state military operational flexibility — this reading forecloses targeting and detention practices the state-centric reading would permit. It is lower than a pure snare would score because the restriction serves a genuine, independently defensible coordination function (closing a protection gap that would otherwise strand ambiguous-status persons). Suppression (0.48) is moderate: the floor is enforced through treaty ratification, customary law status, and international monitoring/prosecution rather than direct coercive machinery, but it does constrain state discretion against the state's own preference. Theater ratio (0.28) is moderate-low; most enforcement activity (ICRC visits, tribunal adjudication) is functionally substantive rather than performative, though some compliance reporting by states under scrutiny trends toward box-ticking. Accessibility collapse (0.4) is moderate — states retain the practical option of asserting the state-centric reading unilaterally, so alternatives have not fully collapsed; resistance (0.72) is high because military and intelligence institutions actively contest this reading's application in doctrine, litigation, and policy.
 *
 * PERSPECTIVAL GAP:
 *   From the state military/intelligence seat, this reading computes as extractive: a coordination structure (uniform treatment standards) has been captured by a maximalist interpretation that imposes costs asymmetrically on operational actors without corresponding benefit to them. From the beneficiary and monitoring seats, the same structure computes as coordination succeeding exactly as intended — closing a gap that would otherwise leave persons unprotected. The tangled_rope classification holds both readings simultaneously: genuine coordination function (closing the status-ambiguity protection gap) plus asymmetric extraction (operational cost concentrated on state military/intelligence actors) sustained by active enforcement (treaty mechanisms, customary law, tribunal jurisdiction).
 *
 * DIRECTIONALITY LOGIC:
 *   Non-state armed group members, civilian populations, and status-ambiguous detainees are structural beneficiaries — the reading extends a treatment floor to them that would otherwise be contingent or absent, placing their directionality near the full-beneficiary end despite their trapped exit options (they cannot personally invoke the protection; it is administered on their behalf). State military command and intelligence services are structural targets — the reading directly restricts their operational discretion, placing them near the full-target end. Human rights monitoring bodies benefit indirectly (expanded mandate, no operational cost), consistent with organized power and arbitrage-grade exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — persons falling into a protection gap due to contested status determinations — remains partially live: irregular and asymmetric conflicts continue to generate status-ambiguous captures. This forecloses treating the constraint as pure mandatrophy (a structure whose founding problem has vanished but which persists anyway). However, the reading's application has expanded beyond the original gap-filling function into a more general floor that some state actors argue now exceeds what the founding problem required — hence 'contested' rather than 'live' status for the founding problem, and the rising extraction trajectory documenting that expansion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_crystallization_status,
    'Has the universal rights reading crystallized into binding customary international law, or does it remain a contested interpretation that individual states may permissibly reject through persistent objection?',
    'Systematic review of state practice and opinio juris across the post-2001 period: track whether states asserting the state-centric or hybrid reading are treated by international tribunals and the broader state community as violating custom, or as exercising a legitimate interpretive option.',
    'If crystallized as custom, the reading''s extraction on state military operations is a settled, non-negotiable feature of IHL; if still contested, the high resistance metric reflects a live doctrinal fight whose outcome is not yet determined, and the extraction figure may overstate the reading''s current binding force on non-ratifying or persistently objecting states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_crystallization_status, empirical, 'Whether the universal reading has achieved customary law status or remains a contested interpretation.').

omega_variable(
    kernel_framing_which_reading_is_the_gap_filler,
    'Is the universal rights reading properly understood as CLOSING a gap the treaty text left open (a corrective, coordination-restoring reading), or as an EXPANSIVE reinterpretation that displaces the treaty''s own status-based architecture (a reading that itself creates the asymmetry it is measured for)?',
    'Textual and drafting-history analysis of the 1949 Conventions and 1977 Protocols: examine whether the drafters intended Article 4 criteria as an exhaustive gate or as one operative category alongside an independent Common Article 3 floor understood to already be universal.',
    'If the universal reading is the gap-filling, textually intended reading, its extraction on state military operations reflects legitimate coordination cost, not overreach — pushing this constraint toward rope. If it is better understood as an expansive judicial/advocacy reinterpretation beyond original design, the extraction reflects genuine asymmetric imposition on state actors who did not consent to that scope at ratification — supporting the tangled_rope classification as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_which_reading_is_the_gap_filler, conceptual, 'Whether this reading restores or displaces the treaty''s original interpretive architecture — a framing question that could shift the classification toward rope if resolved one way.').

omega_variable(
    beneficiary_capacity_to_invoke,
    'Given that the named beneficiaries (non-state fighters, ambiguous-status detainees) have no direct capacity to invoke or enforce this protection themselves, is the ''beneficiary'' designation structurally accurate, or does the real beneficiary-in-practice reduce to the monitoring/advocacy institutions that invoke the floor on their behalf?',
    'Case-level review of who actually initiates and sustains enforcement actions under the universal reading — track the proportion of successful invocations initiated by detainee counsel/self-advocacy versus ICRC, UN mechanisms, and NGO litigation.',
    'If enforcement is near-entirely third-party initiated, the practical beneficiary structure resembles the monitoring bodies more than the nominal detainee/civilian beneficiaries, which would sharpen rather than soften the tangled_rope reading (extraction from states funds/enables an institutional advocacy function, with the named human beneficiaries as the justifying but largely passive class).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capacity_to_invoke, empirical, 'Whether the nominal human beneficiaries or the monitoring institutions are the practical beneficiaries of enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1977, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1977, 0.14).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(gene_tr_t2006, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2006, 0.24).
narrative_ontology:measurement(gene_tr_t2015, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(gene_be_t1977, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(gene_be_t2006, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2006, 0.58).
narrative_ontology:measurement(gene_be_t2015, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2015, 0.6).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1949, 0.2).
narrative_ontology:measurement(gene_su_t1977, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1977, 0.28).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2001, 0.34).
narrative_ontology:measurement(gene_su_t2006, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2006, 0.42).
narrative_ontology:measurement(gene_su_t2015, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2015, 0.46).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__universal_rights_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, hybrid_proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the geneva_conventions_protective_scope kernel. state_centric_reading authors a lower ε on state military operations (protection conditioned on Article 4 criteria, so state discretion over unprivileged belligerents is preserved rather than restricted) and a correspondingly different beneficiary/victim structure (state military as a relative beneficiary of scope limitation, unprivileged belligerents as excluded from protection rather than as named beneficiaries). hybrid_proportionality_reading authors an intermediate ε that varies by conflict classification. All three readings share the same underlying treaty text and factual conflict scenarios but instantiate structurally distinct constraints with different victim sets and different extraction profiles — per the ε-invariance principle, these are three files, not one file measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__universal_rights_reading, institutional, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
