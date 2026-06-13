% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country, Two Systems: Autonomy-Primacy Reading (Treaty-Guaranteed Civil Liberties)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The 1984 Sino-British Joint Declaration and the 1997 Hong Kong Basic Law
 *   establish 'One Country, Two Systems' as the framework for Hong Kong's
 *   status after handover. The autonomy-primacy reading interprets this
 *   framework to mean that Hong Kong retains substantive autonomy backed by
 *   treaty, with civil liberties and judicial independence serving as
 *   structural limits on mainland interference. Mainland intervention that
 *   violates these boundaries constitutes breach of the treaty commitment.
 *   This reading is one of three coherent interpretations of the contested
 *   kernel; the other two (sovereignty-primacy and balanced-coexistence) are
 *   instantiated in separate constraint stories linked by
 *   network.affects_constraints. The autonomy-primacy reading claims mountain
 *   status (the structure emerges from the treaty text and international law
 *   principle of territorial autonomy within sovereign states) while
 *   declaring beneficiaries and low-to-moderate extraction metrics. This
 *   combination triggers false-summit analysis: either the reading is
 *   genuinely structural (beneficiaries are incidental) or the beneficiaries
 *   have shaped the interpretation (it is constructively extracted and should
 *   be tangled_rope). The omegas document this irreducible ambiguity. The
 *   time series shows extractiveness and suppression rising sharply in years
 *   9-15 (corresponding to 2019-2025 in real time: pro-democracy protests,
 *   National Security Law implementation, judicial independence cases) then
 *   stabilizing, indicating the reading is under sustained pressure but has
 *   not been formally abandoned by Hong Kong institutions.
 *
 * KEY AGENTS:
 *   - Hong Kong residents: beneficiaries of autonomy-primacy reading; exercise civil liberties; retain meaningful life-planning autonomy under this interpretation
 *   - Hong Kong judiciary: agenda-setter; enforces the autonomy-primacy reading through judicial review; institutional identity fused with the reading's capacity to say 'no'
 *   - Hong Kong executive: payer; operates under judicial review; faces irreconcilable conflict if mainland directives override local law
 *   - PRC central authority: payer under this reading; structurally constrained by treaty commitment if the reading is binding; trapped between sovereignty doctrine and treaty obligation
 *   - International rule-of-law system: beneficiary; benefits from treaty precedent and Hong Kong's autonomy functioning as rule-of-law anchor
 *   - Democratic reform advocates: excluded; would expand autonomy-primacy to include democratic governance but lack institutional standing
 *   - Mainland security authorities: payer; cannot unilaterally invoke national security under the autonomy-primacy reading without breaching treaty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.35).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.28).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.19).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.19).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, mountain).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country, Two Systems: Autonomy-Primacy Reading (Treaty-Guaranteed Civil Liberties)").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional/political").

domain_priors:emerges_naturally(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '541a26f4-1c48-4ce1-b79b-84afa55f5ef5').
narrative_ontology:cs_kernel_codification('541a26f4-1c48-4ce1-b79b-84afa55f5ef5', fixed_text).
narrative_ontology:cs_authority_grounding('541a26f4-1c48-4ce1-b79b-84afa55f5ef5', lineage).
narrative_ontology:cs_interpretation_layer_present('541a26f4-1c48-4ce1-b79b-84afa55f5ef5').
narrative_ontology:cs_reading_relation('541a26f4-1c48-4ce1-b79b-84afa55f5ef5', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('541a26f4-1c48-4ce1-b79b-84afa55f5ef5', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('541a26f4-1c48-4ce1-b79b-84afa55f5ef5', foundational, territorial_autonomy_structural).
narrative_ontology:cs_axiom_status(territorial_autonomy_structural, holdable).
narrative_ontology:cs_axiom_grounding('541a26f4-1c48-4ce1-b79b-84afa55f5ef5', territorial_autonomy_structural, deontological).
narrative_ontology:cs_axiom('541a26f4-1c48-4ce1-b79b-84afa55f5ef5', foundational, treaty_supremacy_over_unilateral_sovereignty).
narrative_ontology:cs_axiom_status(treaty_supremacy_over_unilateral_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('541a26f4-1c48-4ce1-b79b-84afa55f5ef5', treaty_supremacy_over_unilateral_sovereignty, conventional).
narrative_ontology:cs_reference_frame('541a26f4-1c48-4ce1-b79b-84afa55f5ef5', treaty_mandated_two_system_autonomy).
narrative_ontology:cs_drift_state('541a26f4-1c48-4ce1-b79b-84afa55f5ef5', contemporary_national_security_pressures, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('541a26f4-1c48-4ce1-b79b-84afa55f5ef5', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_rule_of_law_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_executive).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_authority).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, mainland_security_authorities).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, treaty_supremacy_doctrine).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, judicial_independence_as_structural_necessity).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__autonomy_primacy_reading, civil_liberties_inalienability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy civil liberties protections (freedom of speech, assembly, press) guaranteed by the Joint Declaration and Basic Law; exercise judicial review through local courts; retain meaningful autonomy in self-governance. Under this reading, these protections are not delegated revocable permissions but structural rights anchored in treaty commitment. Exit options are constrained by geographic and economic proximity to mainland.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents, beneficiary,
    moderate, biographical, constrained, regional).

% Exercises judicial review power to interpret the Basic Law and enforce civil liberties; operates under the autonomy-primacy reading as an independent arbiter of the boundary between local autonomy and mainland sovereignty. The institutional identity is constituted through the capacity to say 'no' to executive overreach. Exit from this role would dissolve the constraint entirely.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, agenda_setter,
    institutional, generational, identity_locked, regional).

% Operates under judicial review; cannot unilaterally override civil liberties or invoke national security without justification reviewable by courts; answers to Hong Kong law first. Under the autonomy-primacy reading, mainland directives that conflict with local law or treaty obligations create irreconcilable institutional conflict, forcing a choice between legal fidelity and political pressure.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_executive, payer,
    institutional, biographical, constrained, regional).

% Under the autonomy-primacy reading, is structurally constrained by the treaty commitment: direct intervention in Hong Kong civil liberties, security decisions, or judicial independence constitutes breach of the Joint Declaration and Basic Law. The constraint traps the authority between maintaining sovereignty doctrine (which it asserts supersedes the treaty) and honoring the treaty it signed.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_authority, payer,
    powerful, generational, trapped, national).

% Under the autonomy-primacy reading, the constraint vindicates treaty enforcement and the international legal standing of civil liberties guarantees. The rule-of-law system benefits structurally from Hong Kong's autonomy functioning as a precedent for treaty credibility. Does not enforce directly but provides legitimacy backing.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_rule_of_law_system, beneficiary,
    institutional, civilizational, analytical, global).

% Would expand the autonomy-primacy reading to include democratic governance as a required civil liberty; currently excluded from formal decision-making over the boundary between local autonomy and mainland authority. Their voices push for a wider reading of what 'autonomy' entails but are structurally marginal to the treaty-based institutional framework.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, democratic_reform_advocates, excluded,
    organized, biographical, constrained, regional).

% Under the autonomy-primacy reading, cannot unilaterally invoke national security to override Hong Kong's judicial independence without breaching the treaty framework. The constraint forces a negotiation between security doctrine and legal obligation, creating persistent institutional tension and a trapped position if both claims are held simultaneously.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, mainland_security_authorities, payer,
    powerful, generational, trapped, national).

% UN bodies, treaty monitoring committees, and international legal scholars assess whether the autonomy-primacy reading is being honored. They have no enforcement power but carry epistemic authority; their findings shape international treatment of Hong Kong status and PRC treaty compliance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_treaty_monitors, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_residents).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Separates governance authority between Hong Kong's local autonomy (civil liberties, judicial independence, economic policy, most administrative functions) and PRC sovereignty (foreign affairs, defense, currency). The reading solves the coordination problem of how a unitary state can accommodate a legally distinct system with different legal traditions, requiring clear boundaries and institutional mechanisms to prevent unilateral override.
% TRANSFER_FUNCTION: Transfers to the PRC central authority: formal sovereignty, foreign policy control, and national security authority. Transfers to Hong Kong residents and institutions: substantive autonomy, judicial independence, and civil liberties protections guaranteed by treaty and internationally enforceable.
% ABSENT_VOICES: Democratic majorities in Hong Kong seeking expanded political autonomy (franchise reform, universal suffrage) are excluded from formal renegotiation of the autonomy boundary; mainland popular constituencies asserting national security authority over Hong Kong's internal affairs are excluded from the treaty interpretation frame; international human rights organs (outside treaty monitoring) lack institutional standing in Hong Kong's constitutional system.
% DISAPPEARANCE_RATIONALE: If the autonomy-primacy reading were abandoned and replaced by the sovereignty-primacy reading, Hong Kong's institutional structure would rearrange: judicial independence would become revocable, civil liberties would lose treaty backing, and executive-mainland coordination would replace court-mediated boundary maintenance. The international legal status of the treaty commitment would shift from binding constraint to declarative aspiration.
% FOUNDING_PROBLEM: How to reintegrate Hong Kong into the PRC after 156 years of colonial separation while preserving its distinct legal system, market economy, and civil liberties; how to honor both China's sovereignty and Hong Kong's institutional autonomy; how to bridge common-law and civil-law traditions within a unitary state framework.
% FOUNDING_PROBLEM_CORROBORATION: The autonomy-primacy reading is corroborated by the Joint Declaration's treaty text itself (filed at the UN), international law scholars who specialize in treaty interpretation (e.g., Benny Tai's pre-2020 academic work on constitutional autonomy), and Hong Kong's judiciary in landmark decisions pre-2020 (e.g., HKSAR v Ng Ka Ling on Basic Law interpretation rights). Corroboration from OUTSIDE the benefiting parties: international human rights monitoring bodies (UN Office of the High Commissioner) and treaty law specialists from non-aligned states have affirmed the autonomy-primacy reading as the most coherent with the treaty text. The PRC central authority disputes this reading, asserting sovereignty-primacy as the only legitimate frame, so the founding problem's resolution is genuinely contested across the institutional divide.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(one_country_two_systems_framework__autonomy_primacy_reading),
    narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The autonomy-primacy reading claims mountain status because it asserts that territorial autonomy within a sovereign state is a structural principle emerging from the treaty text and international law, not a constructed arrangement. The metrics reflect this: extractiveness is low-to-moderate (0.35) because the reading does not describe one party systematically extracting from others — rather, it allocates governance authority along a functional boundary (local autonomy vs. national sovereignty). Suppression is low (0.28) because the reading relies on law and institutional design (judicial review, treaty text) rather than coercive enforcement. Theater ratio is very low (0.19) because the reading's core function is real — boundary maintenance between two systems — not performative. Accessibility_collapse is moderate (0.62) because alternatives to the autonomy-primacy reading exist and are institutionally plausible (the sibling readings are coherent). Resistance is high (0.71) because the reading faces sustained institutional challenge from the mainland authority, which asserts a different reading; the reading persists because Hong Kong courts and residents defend it. The measurement series track the period 1997-2024: early years (t=0-9) show low extractiveness and suppression as the reading is relatively uncontested; years 9-15 (2019-2025) show sharp rises as pro-democracy movements and National Security Law implementation create acute conflict between the readings; years 15-27 show stabilization as institutions adapt to the new constraint structure under pressure but the reading is not formally abandoned. One shared time grid ensures every metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   From the Hong Kong judiciary's seat, the autonomy-primacy reading is a mountain: it is binding, enforced by their own authority and the treaty text, and supports their institutional role. From the PRC central authority's seat, the same reading is a snare: it constrains mainland action, is enforced against PRC interest, and persists by external pressure and Hong Kong institutional resistance. From the international rule-of-law system's seat, the reading is a mountain: it is a structural principle that validates treaty supremacy and international law. These divergences are structural, not perceptual differences about the same constraint — they reflect real differences in how the constraint binds each actor. The engine computes these divergences by applying directionality (beneficiary vs. target status) to each seat's power atom and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Hong Kong residents, judiciary, international rule-of-law system) sit at low d, indicating the reading subsidizes their autonomy and legal standing. Payers (PRC central authority, mainland security authorities, Hong Kong executive under pressure) sit at high d, indicating constraint on their freedom of action. This is not coercive extraction (the reading does not extract wealth or resources from payers) but institutional constraint: the reading allocates power downward to Hong Kong and outward to international treaty monitoring, limiting mainland unilateral control. The metrics show that this constraint operates with low suppression (0.28) and low theater (0.19), indicating the boundary is maintained through law and institutional design rather than coercive enforcement or performative display.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy-primacy reading does not currently exhibit mandatrophy because the founding problem remains live (treaty integration while preserving autonomy is an ongoing task) and the mandate (maintain boundaries, enforce treaty commitments, protect civil liberties) is still actively pursued by Hong Kong institutions. However, the measurement series showing pressure buildup in years 9-15 suggests preconditions for mandatrophy are forming: if mainland sovereignty-primacy reading becomes institutionally dominant (in courts, executive, security apparatus) while the autonomy-primacy reading persists as theatrical compliance (courts issue rulings that are ignored, residents assert civil liberties that are increasingly restricted), then mandatrophy would set in. The declaration of base_properties.mandatrophy_resolved: false is appropriate — the constraint is not yet resolved as mandatrophic, but the empirical conditions that could trigger mandatrophy (institutional capture of the judiciary, degradation of civil liberties protection, abandonment of treaty enforcement) are under active observation in the measurements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_framing,
    'Is the autonomy-primacy reading a natural law (emerging from the treaty text and international law principles independently of who benefits) or a constructed interpretation strategically advantaging Hong Kong residents and treaty-monitoring institutions?',
    'Comparative legal analysis: does the autonomy-primacy reading emerge from the treaty''s plain language, treaty interpretation doctrine, and the structural logic of two-systems coexistence? Or is it one of several coherent readings that happens to align with the preferences of benefiting parties? Examination of pre-1997 international legal scholarship and PRC negotiating positions would show whether the reading was always implicit or emerged as a claim after institutional conflict.',
    'If natural (textually grounded, independent of beneficiary preference), the constraint is defensible as mountain-type. If constructed, it is a false summit: the beneficiaries have shaped the interpretation to their advantage and the constraint should be reclassified as tangled_rope (genuine coordination function + asymmetric extraction toward Hong Kong residents and international legitimacy).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_framing, conceptual, 'Whether the autonomy-primacy reading is treaty-intrinsic or strategically constructed.').

omega_variable(
    treaty_enforcement_mechanism_gap,
    'What enforces the treaty commitment if the PRC central authority has ultimate sovereignty? Without a supranational arbiter with enforcement power over the PRC, is the treaty-guarantee enforceable or merely declarative?',
    'Empirical test: Does the PRC modify its behavior in response to international treaty monitoring bodies? Do Hong Kong courts successfully enjoin executive acts that breach autonomy boundaries? Are there mechanisms (e.g., international arbitration, UN Security Council review, economic sanctions) that enforce the treaty against a state actor that chooses to violate it?',
    'If treaty enforcement is actually dependent on international coordination and PRC voluntary compliance, the constraint''s enforcement mechanism is weaker than the autonomy-primacy reading assumes. This would lower the accessibility_collapse and resistance metrics — the reading assumes enforcement that may not exist in practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_enforcement_mechanism_gap, empirical, 'Whether the treaty-guarantee has actual enforcement teeth or is backed by reputational cost only.').

omega_variable(
    judicial_independence_under_pressure,
    'Can Hong Kong courts maintain judicial independence under sustained political and institutional pressure from the mainland if the constraint''s legitimacy depends on that independence?',
    'Longitudinal study of judicial behavior: track whether court rulings that constrain executive power or mainland-aligned actors persist, how often judges face retaliation or pressure, and whether the rate of pro-government rulings increases under pressure. Compare Hong Kong to other common-law jurisdictions facing authoritarian pressure (e.g., Myanmar post-coup, Venezuela).',
    'If judicial independence degrades under pressure, the autonomy-primacy reading loses its institutional anchor — courts cannot enforce the boundary if they are captured or intimidated. This would expose the reading as dependent on an unstable institutional condition and shift classification toward snare or degraded_piton (performative autonomy maintained by theater, not function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_under_pressure, empirical, 'Whether judicial independence is structurally robust or contingent on unstable political conditions.').

omega_variable(
    reading_contest_as_constraint_property,
    'Is the existence of competing readings (autonomy-primacy vs. sovereignty-primacy vs. balanced-coexistence) itself an irreducible uncertainty about what the constraint IS, such that the constraint''s type cannot be determined from the reading alone?',
    'Meta-level framing: The three readings are not errors or future clarifications; they represent different parties'' coherent institutional commitments to the same kernel (the Joint Declaration and Basic Law). The constraint EXISTS IN THE CONTEST. No amount of legal analysis or institutional evolution resolves which reading is ''true'' — only political power resolves which reading''s institutions persist. The ε-invariance principle holds within each reading (each reading has its own stable ε), but the constraint''s type ACROSS readings is indeterminate by construction.',
    'This omega documents that the autonomy-primacy reading is ONE reading of a fundamentally contested kernel. The story is not falsified by the existence of sibling readings; rather, the existence of siblings is evidence that the kernel is politically unstable and institutionally contested. The constraint-family structure (three linked stories) is the appropriate model, not fusion into one constraint with a measurement-dependent ε.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_as_constraint_property, conceptual, 'The constraint is one reading of a permanently contested kernel; no reading-external resolution exists.').

omega_variable(
    beneficiary_claim_on_natural_law_status,
    'Does the fact that Hong Kong residents and the international rule-of-law system are declared as beneficiaries undermine the mountain claim? Are these genuine beneficiaries or vindicated propositions?',
    'Clarification: Hong Kong residents benefit from civil liberties protections, but the autonomy-primacy reading asserts those protections are NATURAL (treaty-guaranteed, inalienable) not contingent on beneficiary preference. The international rule-of-law system benefits from Hong Kong''s status as a precedent for treaty enforcement, but the reading asserts that treaty supremacy is a structural principle, not a system created for the rule-of-law''s benefit. The beneficiaries are real (they do receive gains), but the reading claims the gains flow from a natural/structural principle, not from constructed extraction. This is the false-summit candidate case: the constraint has real beneficiaries but asserts natural-law status. The omegas document the ambiguity.',
    'The FSM (false summit mountain) signature may fire if the engine detects beneficiary presence + low-extraction metrics + mountain claim. The declared omegas provide the analytical grounding for FSM''s hypothesis: the reading is either a genuine structural principle (in which case beneficiary-presence is incidental) or a constructed interpretation whose beneficiaries have shaped it (in which case it should be reclassified as tangled_rope). The corpus measurement will distinguish these cases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_claim_on_natural_law_status, conceptual, 'Whether declared beneficiaries indicate false summit or genuine natural law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(one__tr_t0, observed).
narrative_ontology:measurement(one__tr_t3, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 3, 0.11).
narrative_ontology:measurement_basis(one__tr_t3, observed).
narrative_ontology:measurement(one__tr_t6, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement_basis(one__tr_t6, observed).
narrative_ontology:measurement(one__tr_t9, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 9, 0.15).
narrative_ontology:measurement_basis(one__tr_t9, observed).
narrative_ontology:measurement(one__tr_t12, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement_basis(one__tr_t12, observed).
narrative_ontology:measurement(one__tr_t15, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(one__tr_t15, observed).
narrative_ontology:measurement(one__tr_t18, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 18, 0.19).
narrative_ontology:measurement_basis(one__tr_t18, observed).
narrative_ontology:measurement(one__tr_t21, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 21, 0.2).
narrative_ontology:measurement_basis(one__tr_t21, observed).
narrative_ontology:measurement(one__tr_t27, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 27, 0.19).
narrative_ontology:measurement_basis(one__tr_t27, observed).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(one__be_t0, observed).
narrative_ontology:measurement(one__be_t3, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 3, 0.22).
narrative_ontology:measurement_basis(one__be_t3, observed).
narrative_ontology:measurement(one__be_t6, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 6, 0.26).
narrative_ontology:measurement_basis(one__be_t6, observed).
narrative_ontology:measurement(one__be_t9, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 9, 0.31).
narrative_ontology:measurement_basis(one__be_t9, observed).
narrative_ontology:measurement(one__be_t12, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 12, 0.33).
narrative_ontology:measurement_basis(one__be_t12, observed).
narrative_ontology:measurement(one__be_t15, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 15, 0.34).
narrative_ontology:measurement_basis(one__be_t15, observed).
narrative_ontology:measurement(one__be_t18, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 18, 0.35).
narrative_ontology:measurement_basis(one__be_t18, observed).
narrative_ontology:measurement(one__be_t21, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 21, 0.36).
narrative_ontology:measurement_basis(one__be_t21, observed).
narrative_ontology:measurement(one__be_t27, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 27, 0.35).
narrative_ontology:measurement_basis(one__be_t27, observed).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(one__su_t0, observed).
narrative_ontology:measurement(one__su_t3, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 3, 0.15).
narrative_ontology:measurement_basis(one__su_t3, observed).
narrative_ontology:measurement(one__su_t6, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 6, 0.18).
narrative_ontology:measurement_basis(one__su_t6, observed).
narrative_ontology:measurement(one__su_t9, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 9, 0.21).
narrative_ontology:measurement_basis(one__su_t9, observed).
narrative_ontology:measurement(one__su_t12, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 12, 0.25).
narrative_ontology:measurement_basis(one__su_t12, observed).
narrative_ontology:measurement(one__su_t15, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 15, 0.27).
narrative_ontology:measurement_basis(one__su_t15, observed).
narrative_ontology:measurement(one__su_t18, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 18, 0.28).
narrative_ontology:measurement_basis(one__su_t18, observed).
narrative_ontology:measurement(one__su_t21, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 21, 0.29).
narrative_ontology:measurement_basis(one__su_t21, observed).
narrative_ontology:measurement(one__su_t27, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 27, 0.28).
narrative_ontology:measurement_basis(one__su_t27, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework__balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% The one_country_two_systems_framework is a contested kernel with three institutionally live readings. Each reading instantiates a different constraint with a different ε, beneficiary structure, and institutional backing. The autonomy-primacy reading (this story) asserts treaty supremacy and territorial autonomy. The sovereignty-primacy reading asserts PRC sovereign authority as supreme. The balanced-coexistence reading asserts neither absolute and seeks political accommodation. All three readings interpret the same kernel text (Joint Declaration, Basic Law) but produce incompatible institutions and classifications. The readings coexist as institutional commitments held by different parties; none logically forecloses another within any single party's framework, though each reading influences the others by creating pressure on institutional boundaries. Each story carries its own ε-invariant metrics and structural data. Cross-reading comparison (via the affects_constraints links and the reading_relations array in cs_structure) enables the corpus to measure how contested kernels function as constraint families.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
