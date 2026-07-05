% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member-State Sovereignty Reading of Free Movement (Welfare Capacity Bounding)
 *   domain: political economy / federalism / migration policy / welfare state theory
 *
 * SUMMARY:
 *   This constraint instantiates the member-sovereignty reading of the EU
 *   free-movement kernel: national governments and the ECJ's post-Dano
 *   jurisprudence permit member states to condition access to social benefits
 *   and residence for economically inactive migrants on a 'sufficient
 *   resources' or 'genuine link' test, and to derogate from expansive
 *   equal-treatment obligations to protect the fiscal sustainability of
 *   national welfare institutions and domestic labor markets. The reading
 *   treats free movement as bounded, not absolute — a coordination device
 *   that stops short of fiscal or labor-market harmonization. Its
 *   coordination function (protecting welfare state solvency and incumbent
 *   labor conditions) is genuine, but it operates by actively excluding and
 *   restricting a specific population — economically inactive migrants and,
 *   secondarily, sending-state labor emigrants whose home economies lose
 *   working-age population without full corresponding benefit capture — which
 *   is why it is authored as tangled_rope rather than rope. This is one of
 *   three sibling readings of the same kernel; the integration_reading and
 *   welfare_coordination_reading are separate constraint stories.
 *
 * KEY AGENTS:
 *   - receiving_state_welfare_systems: institutional beneficiary — retains fiscal boundary control, protected against unfunded liability expansion
 *   - receiving_state_incumbent_workers: organized beneficiary — protected from wage/benefit competition pressure attributed to unrestricted inflows
 *   - national_governments_border_authority: agenda_setter — administers sufficient-resources tests, derogations, and exclusion mechanisms
 *   - economically_inactive_migrants: powerless victim — trapped by residence/benefit conditionality, restricted mobility despite formal free-movement rights
 *   - sending_state_labor_emigrants: moderate-power victim — restricted labor market access in destination states, face conditionality on arrival
 *   - sending_state_economies: institutional victim — bear brain drain and remittance-dependency costs without full labor market access compensation
 *   - ecj_and_commission: observer/analytical — adjudicates boundary of exclusion authority, mediates between kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.58).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.62).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member-State Sovereignty Reading of Free Movement (Welfare Capacity Bounding)").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political economy / federalism / migration policy / welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, '9e5f861f-84d8-4fae-a717-c11802316202').
narrative_ontology:cs_kernel_codification('9e5f861f-84d8-4fae-a717-c11802316202', formalized).
narrative_ontology:cs_authority_grounding('9e5f861f-84d8-4fae-a717-c11802316202', distributed).
narrative_ontology:cs_reading_relation('9e5f861f-84d8-4fae-a717-c11802316202', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e5f861f-84d8-4fae-a717-c11802316202', federation_membership_kernel__welfare_coordination_reading, influences).
narrative_ontology:cs_axiom('9e5f861f-84d8-4fae-a717-c11802316202', foundational, welfare_solidarity_requires_bounded_membership).
narrative_ontology:cs_axiom_status(welfare_solidarity_requires_bounded_membership, holdable).
narrative_ontology:cs_axiom_grounding('9e5f861f-84d8-4fae-a717-c11802316202', welfare_solidarity_requires_bounded_membership, conventional).
narrative_ontology:cs_axiom('9e5f861f-84d8-4fae-a717-c11802316202', foundational, member_states_retain_default_exclusion_competence).
narrative_ontology:cs_axiom_status(member_states_retain_default_exclusion_competence, holdable).
narrative_ontology:cs_axiom_grounding('9e5f861f-84d8-4fae-a717-c11802316202', member_states_retain_default_exclusion_competence, conventional).
narrative_ontology:cs_reference_frame('9e5f861f-84d8-4fae-a717-c11802316202', post_maastricht_conditional_citizenship_settlement).
narrative_ontology:cs_drift_state('9e5f861f-84d8-4fae-a717-c11802316202', post_dano_enforcement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9e5f861f-84d8-4fae-a717-c11802316202', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_systems).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_state_incumbent_workers).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, national_governments_border_authority).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_labor_emigrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_state_economies).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, national_solidarity_bounded_membership_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_kernel__member_sovereignty_reading, subsidiarity_in_welfare_design).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National social insurance and benefit systems are shielded from unconditional claims by economically inactive EU migrants through sufficient-resources and genuine-link tests. They do not administer the exclusion directly but their fiscal exposure is the justification the bounding mechanism claims to serve, and they are the intended net gainer of reduced liability.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_welfare_systems, beneficiary,
    institutional, generational, analytical, national).

% Domestic labor unions and worker constituencies benefit from reduced competitive pressure when inactive migrants and some categories of job-seeking migrants face conditional access. They organize politically to defend and extend exclusion authority, framing it as protection of domestic wage floors and job security.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_state_incumbent_workers, beneficiary,
    organized, biographical, constrained, national).

% Sets and administers the 'sufficient resources,' 'genuine link,' and habitual-residence tests that determine which migrants can access benefits and residence rights. Litigates before the ECJ to defend the scope of national derogation authority and negotiates within the Council to preserve unanimity requirements on social policy harmonization. Retains substantial discretion over enforcement intensity.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, national_governments_border_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% EU citizens who have moved to another member state without securing employment or sufficient independent resources face residence permit conditionality, benefit exclusion, and in some cases expulsion procedures despite holding formal free-movement rights. Their exit is trapped in the specific sense that returning to the home state may mean losing acquired social insurance contributions or facing worse economic conditions than those that prompted emigration.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, biographical, trapped, national).

% Workers from lower-wage member states who move to seek employment face conditionality on arrival (proof of active job search, time limits on job-seeker benefit access, habitual-residence tests) that narrows the practical scope of their formal free-movement rights. They can relocate to other member states or return home, but each destination applies similar conditionality, and returning often means forfeiting accrued mobility gains.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_labor_emigrants, payer,
    moderate, biographical, constrained, continental).

% Lower-income member states lose working-age, often skilled, population to outward migration while this reading's exclusion architecture limits reciprocal social transfer capture (e.g., unemployment benefit portability, pension coordination gaps) that might otherwise offset the demographic and fiscal cost of emigration. They cannot exit the arrangement without exiting the federation itself.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_state_economies, payer,
    institutional, generational, trapped, continental).

% Adjudicate the scope of the sufficient-resources and genuine-link tests case by case (Dano, Alimanovic, Commission v. UK) and mediate between the competing kernel readings through jurisprudence and infringement proceedings. They do not benefit from or pay into the arrangement directly but their rulings determine which reading of the kernel currently governs practice.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, ecj_and_commission, observer,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_kernel__member_sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_kernel__member_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bounds free movement so that national welfare states and labor markets are not exposed to unconditional claims from residents who have not contributed to or established a genuine link with the host system, allowing member states to sustain distinct welfare designs without harmonizing fiscal exposure across the federation.
% TRANSFER_FUNCTION: Moves the cost of adjustment away from receiving-state welfare systems and incumbent labor markets and onto economically inactive migrants (who lose benefit access and residence security) and sending-state economies (who lose population and receive reduced compensating transfers), while national governments retain administrative discretion over where the line is drawn.
% ABSENT_VOICES: Economically inactive migrants themselves are rarely represented in the Council negotiations or ECJ proceedings that set the scope of exclusion authority; migrant advocacy NGOs raise the issue but lack a formal seat. Sending-state governments participate in Council negotiations but with less leverage than large receiving states, since population outflow is diffuse and politically less salient domestically than inflow is for receiving states.
% DISAPPEARANCE_RATIONALE: If member-state exclusion authority disappeared overnight and free movement became fully unconditional, receiving-state welfare systems would face immediate exposure to broader benefit claims, incumbent-worker political coalitions would mobilize against the change, and the negotiated balance underlying continued EU enlargement and single-market cohesion would be destabilized — this is a live, structurally load-bearing arrangement, not a residual formality.
% FOUNDING_PROBLEM: Early European integration faced the prospect that unconditional free movement combined with divergent national welfare generosity could trigger fiscally destabilizing 'welfare magnet' effects and undermine political support for continued integration among receiving-state electorates.
% FOUNDING_PROBLEM_CORROBORATION: National governments and receiving-state courts attest the problem remains live, citing ongoing fiscal exposure concerns. Independent empirical research commissioned by the European Commission itself (e.g., 2013 ICF GHK study on the impact of mobile EU citizens on national social security systems) found benefit-tourism effects substantially smaller than the political narrative asserts, and academic labor economists studying post-Dano case law argue the exclusion apparatus has expanded beyond what the empirical fiscal-risk evidence would justify — corroboration from outside the beneficiary set exists and points toward the 'dead problem, live enforcement' reading, though it is contested rather than settled.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that the bounding mechanism transfers real costs onto economically inactive migrants and sending-state emigrants/economies while protecting a concentrated beneficiary set (receiving-state welfare systems and incumbent labor markets) — this is asymmetric, not diffuse, extraction. Suppression (0.62) is authored higher than extractiveness because the exclusion apparatus (residence permits, sufficient-resources tests, benefit-conditionality bureaucracy) requires continuous administrative enforcement and has hardened over the measured interval following the Dano/Alimanovic jurisprudential line. Theater ratio is moderate-low (0.28): the welfare-protection function is substantively real (member states do face genuine fiscal exposure risk), but a growing share of enforcement activity serves labor-market protectionism rather than fiscal solvency per se. Accessibility collapse is moderate (0.5) — some alternative pathways (worker status acquisition, sufficient means demonstration) remain open, distinguishing this from a pure snare. Resistance is high (0.7) because sending states, migrant advocacy networks, and the Commission itself actively contest the scope of exclusion authority in ongoing litigation and political negotiation.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (national governments), this reading is coordination: a necessary boundary preventing free movement from destabilizing welfare state solvency, which is the whole point of retaining sovereign competence over social policy. From the payer/victim seat (economically inactive migrants, sending-state emigrants), the same rules operate as enforced exclusion that formally exists within a 'free movement' framework while substantively restricting it. The engine should compute divergent seat-level types from this single set of structural facts — the claim of coordination and the fact of extraction are not mutually exclusive, which is precisely why this is authored as tangled_rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state welfare systems and incumbent workers sit near the beneficiary end: they retain fiscal insulation and reduced labor-supply competition without bearing the exclusion's direct costs. National governments as agenda-setters occupy the enforcement seat, deriving legitimacy and reduced fiscal exposure from the bounding function. Economically inactive migrants are the clearest target — trapped by conditionality with few practical mobility alternatives once excluded from benefit access. Sending-state labor emigrants face constrained (not trapped) exit — they can seek work in other member states or return home, but face conditionality wherever they land under this reading's logic. Sending-state economies, as institutional victims, experience the aggregate brain-drain effect: mobile skilled labor departs under free movement's formal promise while this reading's restrictions reduce the corresponding social transfer or reintegration benefits they might otherwise negotiate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — early-1990s fear that unconditional free movement would trigger 'welfare tourism' and race-to-the-bottom fiscal competition among member states — remains genuinely contested rather than resolved or dead. Empirical benefit-tourism studies (e.g., EU Commission's own commissioned research) have found the phenomenon smaller than the political narrative suggests, yet the exclusion apparatus has intensified rather than relaxed over the measured interval, which is the signature this framework is built to catch: a coordination rationale whose empirical predicate has weakened while its enforcement machinery hardens. Classifying this as tangled_rope (not scaffold) is deliberate — there is no sunset clause; this bounding is asserted as a permanent structural feature of EU federalism, not a transitional measure pending fuller integration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_protectionism_ambiguity,
    'Is member-state exclusion authority a genuine defense of fiscally bounded solidarity institutions, or a protectionist device that uses welfare-capacity language to shield incumbent labor markets from competition?',
    'Compare exclusion patterns against actual fiscal strain data: if exclusions correlate with documented welfare system stress (benefit tourism case volumes, dependency ratios) rather than with labor market competition pressure (unemployment cycles, sectoral wage pressure), the welfare-capacity reading is supported.',
    'If protectionist, the reading functions as cover for incumbent-worker extraction dressed as solidarity defense, raising effective suppression above the authored value; if genuine, the bounding is a legitimate coordination limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_protectionism_ambiguity, conceptual, 'Whether sovereignty-based exclusion is welfare defense or labor protectionism in disguise.').

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly does the member_sovereignty_reading diverge from the integration_reading and welfare_coordination_reading on the same kernel (the scope of Article 45 TFEU / Directive 2004/38 free movement rights)?',
    'This constraint instantiates the reading in which member states retain unilateral authority to determine ''sufficient resources'' and ''genuine link'' tests for economically inactive migrants, narrowing ECJ-expansive case law (Dano, Alimanovic line) rather than the earlier mobility-maximizing line (Grzelczyk, Collins). The sibling integration_reading instantiates the ECJ-expansive line; welfare_coordination_reading instantiates the coordination-not-harmonization compromise (Regulation 883/2004 logic). The disagreement is located precisely in who adjudicates ''sufficient resources'' and whether exclusion is exceptional or default.',
    'If the sibling integration_reading''s structural premise were adopted instead, economically_inactive_migrants would move from victim to beneficiary/neutral, and the constraint''s coordination function would be reframed as citizenship completion rather than fiscal bounding — a different ε and a different type entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Locates the structural disagreement between sibling kernel readings in the adjudication of sufficient-resources tests.').

omega_variable(
    brain_drain_measurement_uncertainty,
    'How much of sending-state brain drain and labor emigrant hardship is attributable to this specific bounding reading versus pre-existing structural asymmetries between member state economies?',
    'Longitudinal comparison of emigration flows and remittance-dependency data before and after exclusion-authority rulings (e.g., post-Dano) in comparable sending states.',
    'If the bounding reading is a marginal contributor, victim severity for sending_state_economies is overstated; if it is a major contributor, the tangled_rope classification understates extraction directed at sending states.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(brain_drain_measurement_uncertainty, empirical, 'Uncertainty about causal attribution of brain drain to this reading versus background economic asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t4, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(fede_tr_t8, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(fede_tr_t12, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(fede_tr_t16, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(fede_tr_t24, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fede_be_t4, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(fede_be_t8, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(fede_be_t12, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(fede_be_t16, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(fede_be_t24, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fede_su_t4, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(fede_su_t8, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(fede_su_t12, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement(fede_su_t16, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(fede_su_t24, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_kernel__member_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the federation_membership_kernel (kernel_id: federation_membership_kernel). The integration_reading treats free movement as an expansively-interpreted fundamental right with negligible authorized exclusion, producing a much lower ε and a rope-leaning classification centered on mobile workers as beneficiaries. The welfare_coordination_reading occupies a middle position — anti-social-dumping enforcement without broad exclusion authority — producing an intermediate ε. This member_sovereignty_reading authorizes the widest exclusion scope and therefore carries the highest ε and the clearest victim set (economically inactive migrants, sending-state emigrants and economies). Do not average across these three; each is ε-invariant on its own reading and the three are linked here for contamination/network analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_kernel__member_sovereignty_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
