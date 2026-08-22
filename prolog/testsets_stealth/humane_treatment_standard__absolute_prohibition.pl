% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Common Article 3 Absolute Prohibition Reading — Non-Derogable Humane Treatment Floor
 *   domain: international_humanitarian_law/state_security/human_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading — absolute_prohibition — of the
 *   contested kernel humane_treatment_standard. Under this reading, Common
 *   Article 3's minimum standards are non-derogable: detainees enter the full
 *   rights-holder set, state interrogation methods are constrained
 *   absolutely, and no security exception permits crossing the threshold. The
 *   sibling readings (contextual_necessity, proportionality_balancing) are
 *   separate constraints in separate files; nothing about them is averaged
 *   into this one. Per the kernel-reading epsilon rule, the referent of
 *   extractiveness is the STANDING ARRANGEMENT UNDER CONTEST — actual state
 *   detention and interrogation practice operating under the Common Article 3
 *   regime — assessed by this reading's own lights: a regime whose absolutist
 *   text coexists with systematic violation, consent-gated enforcement, and a
 *   growing compliance-performance layer. KEY AGENTS (by structural
 *   relationship):
 *
 * KEY AGENTS:
 *   - high_contracting_states: Agenda setter (institutional/constrained) — administers the conventions, runs the detention estate, controls self-reporting and domestic prosecution
 *   - captured_combatants_and_detainees: Protected class and residual payer (powerless/trapped) — receives the floor's protection when it holds, absorbs violations when it fails
 *   - tortured_or_degraded_detainees: Primary victim (powerless/trapped) — crossed-threshold casualties of the enforcement gap
 *   - disappeared_and_inaccessible_detainees: Primary victim (powerless/trapped) — held beyond every verification channel the standards depend on
 *   - humanitarian_access_mandate_holders: Institutional beneficiary (organized/constrained) — collects access rights and standing from the norm's existence
 *   - belligerents_relying_on_reciprocity: Secondary beneficiary and payer (organized/constrained) — compliance rests on expected reciprocity they did not bargain for
 *   - national_interrogation_services: Formal payer with arbitrage exit (institutional/arbitrage) — methods foreclosed on paper, routed around in practice
 *   - international_prosecutorial_bodies: Analytical observer (institutional/analytical) — adjudicates after the fact behind consent gates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.65).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.62).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.65).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Common Article 3 Absolute Prohibition Reading — Non-Derogable Humane Treatment Floor").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/state_security/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '15c0b46b-4b2d-421a-88c4-4e080046beef').
narrative_ontology:cs_kernel_codification('15c0b46b-4b2d-421a-88c4-4e080046beef', fixed_text).
narrative_ontology:cs_authority_grounding('15c0b46b-4b2d-421a-88c4-4e080046beef', lineage).
narrative_ontology:cs_interpretation_layer_present('15c0b46b-4b2d-421a-88c4-4e080046beef').
narrative_ontology:cs_reading_relation('15c0b46b-4b2d-421a-88c4-4e080046beef', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('15c0b46b-4b2d-421a-88c4-4e080046beef', humane_treatment_standard__proportionality_balancing, forecloses).
narrative_ontology:cs_axiom('15c0b46b-4b2d-421a-88c4-4e080046beef', foundational, torture_prohibition_admits_no_exceptions).
narrative_ontology:cs_axiom_status(torture_prohibition_admits_no_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('15c0b46b-4b2d-421a-88c4-4e080046beef', torture_prohibition_admits_no_exceptions, deontological).
narrative_ontology:cs_axiom('15c0b46b-4b2d-421a-88c4-4e080046beef', foundational, detainee_full_rights_holder_status).
narrative_ontology:cs_axiom_status(detainee_full_rights_holder_status, holdable).
narrative_ontology:cs_axiom_grounding('15c0b46b-4b2d-421a-88c4-4e080046beef', detainee_full_rights_holder_status, deontological).
narrative_ontology:cs_reference_frame('15c0b46b-4b2d-421a-88c4-4e080046beef', non_derogable_absolute_floor).
narrative_ontology:cs_drift_state('15c0b46b-4b2d-421a-88c4-4e080046beef', contemporary_counterterrorism_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('15c0b46b-4b2d-421a-88c4-4e080046beef', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, captured_combatants_and_detainees).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, belligerents_relying_on_reciprocity).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, humanitarian_access_mandate_holders).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, tortured_or_degraded_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, disappeared_and_inaccessible_detainees).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, captured_combatants_and_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, belligerents_relying_on_reciprocity).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, national_interrogation_services).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, non_derogability_doctrine).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, hors_de_combat_protection_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ratify and administer the Geneva Conventions, operate the detention facilities the standards govern, staff the treaty bodies, write the self-reports, and decide whether to prosecute their own personnel. Formal denunciation of the Conventions exists on paper but carries civilizational-membership costs no state has been willing to pay, so exit is nominal rather than real.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, high_contracting_states, agenda_setter,
    institutional, generational, constrained, global).

% Persons hors de combat in someone else's custody. When the treatment floor holds they receive food, medical care, contact with families, and protection from coercive interrogation. When it fails they absorb beatings, stress positions, sexual violence, and starvation. They cannot leave custody, cannot choose their captor, and their remedies depend entirely on institutions they do not control.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, captured_combatants_and_detainees, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__absolute_prohibition, captured_combatants_and_detainees, beneficiary).

% The subset of detainees whose treatment crossed the threshold this reading declares uncrossable. Evidence is destroyed, proceedings are closed, amnesties are traded, and testimony surfaces years later through archives, memoirs, or exhumations. Remedy, when it comes at all, comes long after the harm.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, tortured_or_degraded_detainees, payer,
    powerless, immediate, trapped, global).

% Held in undisclosed facilities with no notification to families, no registration, and no delegate visits. The minimum standards nominally apply to them, but every verification channel the standards rely on has been severed. Their families often lack even confirmation of death.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, disappeared_and_inaccessible_detainees, payer,
    powerless, immediate, trapped, regional).

% ICRC-style delegations collect access rights, registration privileges, and institutional standing directly from the existence of the standards. They are bound by a confidentiality model in which public denunciation forfeits access, and losing access punishes detainees more than the organization, so walking away is not a live option.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, humanitarian_access_mandate_holders, beneficiary,
    organized, generational, constrained, global).

% Non-state armed groups and smaller states whose compliance posture rests on expecting their own captured people to be covered by the same floor. They had no seat drafting the text, yet they bear prosecution exposure for violations and lose the reciprocity shield when they defect.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, belligerents_relying_on_reciprocity, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__absolute_prohibition, belligerents_relying_on_reciprocity, payer).

% Military and intelligence interrogation directorates whose coercive method repertoire this reading forecloses entirely. They bear the capability cost on paper and respond with offshore sites, rendition, proxy detention, and definitional lobbying — routes around the standards' reach that no detainee possesses.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, national_interrogation_services, payer,
    institutional, biographical, arbitrage, global).

% ICC chambers, universal-jurisdiction prosecutors, and UN commissions of inquiry. They adjudicate violations after the fact, but their reach is gated by state consent, Security Council referral politics, and the practical difficulty of proving threshold-crossing inside closed facilities.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_prosecutorial_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__absolute_prohibition, national_interrogation_services).
narrative_ontology:fixing_cost_class(humane_treatment_standard__absolute_prohibition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives all parties to a non-international armed conflict — including unrecognized non-state groups — a common minimum-treatment floor they can adopt without negotiating with or recognizing each other, enabling reciprocity expectations, humanitarian access, and predictable prisoner handling between enemies.
% TRANSFER_FUNCTION: Moves coercive interrogation latitude from state security services into a protected legal status for persons in custody; moves reciprocity assurance and reputational standing to adhering parties; in violation episodes, moves bodily integrity and sometimes life from detainees into the interrogating service's intelligence product.
% ABSENT_VOICES: Detainees — the class the standards exist to protect — hold no seat in treaty diplomacy, no vote in enforcement design, and usually no voice in their own cases. Families of the disappeared and victims under historical amnesties are likewise outside the room. They are in custody, in mass graves, or in exile.
% DISAPPEARANCE_RATIONALE: Overnight removal would strip internal conflicts of their only shared treatment floor: the humanitarian access architecture collapses, reciprocity expectations unwind, interrogation escalates toward whatever each captor's incentives permit, and the legal line between lawful custody and disappearance loses its anchor.
% FOUNDING_PROBLEM: Mid-nineteenth-century battlefield observation and the mid-twentieth-century record of internal conflicts showed persons hors de combat tortured and executed with no binding floor that non-state parties would honor. Common Article 3 (1949) was built to extend a minimum humanitarian floor to civil wars, binding all parties without requiring recognition or reciprocity guarantees.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: UN commissions of inquiry and the Special Rapporteur on torture document ongoing detention abuse across current conflicts; forensic exhumation programs and conflict-monitoring organizations independently attest both the founding problem's persistence and the enforcement gap. No source outside the beneficiary set attests that the problem is solved.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is tangled_rope because BOTH halves are structurally present. The coordination half is real: a treatment floor adoptable without mutual recognition solves a genuine collective-action problem in civil wars, and the access/reciprocity architecture demonstrably functions in conflicts where both sides hold captives. The extraction half is equally real: detainees bear the costs of the enforcement gap through the same structure that coordinates their captors, powerful states defect cheaply while demanding compliance from weaker parties, and the absolutist text is maintained partly as performance over a practice that regularly crosses it. Hence requires_active_enforcement: true, with named beneficiaries and victims. Metric rationale: extractiveness 0.65 reflects the standing arrangement as this reading sees it — grave, recurring threshold-crossings with rare remedy, tempered by real protection delivered in conventional-custody settings. Suppression 0.62 is authored as a RAW structural property (the prohibition is deliberately maximally coercive toward violators and permits no alternative framework) and is NOT scaled by power or scope — only extractiveness is scaled downstream. Accessibility_collapse 0.35 is low because the forbidden alternative remains practically reachable via denial, offshore sites, and definitional narrowing; resistance 0.6 is high because the sibling readings ARE the organized resistance — security establishments and major powers contest the absolute reading continuously. Theater_ratio 0.48 reflects a compliance-performance layer (manuals, training, periodic reports, condemnation statements) that has grown faster than verified protection. The measurement series run on ONE shared grid (t = 0,6,12,18,24,30) with every tracked metric authored at every point; the extractiveness arc rises to a mid-series peak corresponding to the offshore-detention era and partially recedes under prosecutorial and documentary pressure, while theater and enforcement machinery ratchet monotonically upward.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the high_contracting_states seat the arrangement is a coordination achievement those states built, administer, and legitimately pride themselves on — rope-flavored. From the detained_persons seats the same arrangement is a promise that fails precisely when it is most needed, enforced by institutions the detainee cannot reach — snare-flavored at the violation margin. From the national_interrogation_services seat the arrangement is a foreclosing constraint whose bite is blunted by arbitrage — the constraint reads as costly on paper and porous in practice. The engine computes this divergence; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for captured_combatants_and_detainees (when the floor holds), belligerents_relying_on_reciprocity, and humanitarian_access_mandate_holders; victim declarations drive high directionality for tortured_or_degraded_detainees and disappeared_and_inaccessible_detainees, whose trapped exit pins them near the full-target end. Two structural wrinkles deserve note. First, captured_combatants_and_detainees carry a dual declaration (payer with secondary beneficiary): their position alternates with the floor's integrity, so their derived d sits intermediate with high variance rather than at either pole. Second, national_interrogation_services are declared payers (the reading forecloses their methods) but their arbitrage-grade exit — offshore sites, rendition, proxy detention — damps the extraction the constraint can actually impose on them, pulling their effective position back toward the beneficiary end despite formal targeting. That damping is itself diagnostic: the enforcement gap is visible in the directionality arithmetic, not just in the violation record.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Mountain-by-rhetoric: the reading's own language ('non-derogable', 'no circumstances') invites natural-law treatment, but the constraint is constructed treaty law that requires active enforcement, meets organized resistance, and leaves the forbidden alternative practically accessible — it is not a mountain, and declaring beneficiaries on a naturality claim would be a false summit. Snare-by-violation-record: the documented torture record invites a coordination-as-cover reading, but the reciprocity and access functions are demonstrably real and load-bearing — participants are net beneficiaries in exactly the conflicts the founding problem targeted. Tangled rope preserves both halves and locates the pathology where it belongs: not in the coordination function, but in the asymmetric enforcement that lets the strongest parties defect cheapest. The founding problem is live (corroborated externally), so no mandatrophy is declared; the rising theater_ratio is watched as the leading indicator of a future drift toward piton if performance continues substituting for protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the absolute_prohibition reading of kernel humane_treatment_standard; how would the classification shift if the sibling readings were instantiated instead?',
    'Author and compile the sibling stories (humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing) and compare seat sets, epsilon, and computed type across the family.',
    'Under contextual_necessity the detainee rights-holder set shrinks to a derogable baseline and a security-carve-out seat gains agenda power; under proportionality_balancing a balancing-tribunal seat enters and epsilon becomes threshold-dependent rather than absolute. Family comparison isolates how much of this story''s extraction profile is reading-indexed versus kernel-stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one reading of a contested kernel; sibling deltas deferred to sibling files.').

omega_variable(
    enforcement_gap_design_or_defection,
    'Is the measured extraction attributable to state defection that the norm condemns, or to the norm''s self-enforcement design (consent-based jurisdiction, state self-reporting, confidentiality-bound monitoring)?',
    'Compare violation and remedy rates across conflicts with and without external enforcement presence — ICC referrals, ad hoc tribunals, peacekeeping detention monitoring — holding conflict severity roughly constant.',
    'Design attribution raises effective extraction and pushes the review toward the snare boundary; defection attribution keeps the tangled_rope reading with the remediation locus at enforcement capacity rather than norm design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_design_or_defection, empirical, 'Whether the enforcement gap is a design property of the constraint or a behavior of its subjects.').

omega_variable(
    reciprocity_asymmetry_durability,
    'Does the coordination function survive conflicts in which one party holds no captives — suicide-insurgent campaigns, siege warfare — removing the reciprocity leverage the floor historically ran on?',
    'Compare treatment-floor adherence indicators between symmetric-captive conflicts and asymmetric no-captive conflicts, controlling for monitoring access.',
    'If the floor collapses without reciprocity, the coordination half is thinner than the tangled_rope claim requires and the extraction share rises; if it holds via internalized professional norms, the coordination function is more robust than the reciprocity mechanism alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_asymmetry_durability, empirical, 'Durability of the coordination half under asymmetric-capture conditions.').

omega_variable(
    customary_law_naturalization,
    'Has the absolute prohibition hardened into customary international law binding all states erga omnes — moving it from constructed treaty commitment toward something approaching a political natural law?',
    'Track citations by national courts of non-party states, General Assembly voting patterns, and the military manuals of states outside the treaty framework.',
    'Customary status would raise accessibility_collapse (fewer exits for would-be defectors) and strengthen the mountain-aspiration reading of the text, without changing its constructed origin or its enforcement dependence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_naturalization, conceptual, 'Whether the prohibition''s basis has shifted from treaty consent to asserted universality.').

omega_variable(
    theater_measurement_artifact,
    'Is the rising theater_ratio genuine Goodhart drift — compliance performance substituting for protection — or a measurement artifact of expanding reporting obligations?',
    'Correlate theater indicators (report volume, training hours, condemnation statements) with independent outcome measures: documented violation rates, humanitarian access denials, disappeared-person counts.',
    'An artifact finding would lower theater_ratio and soften the piton-drift concern; a genuine-drift finding confirms the leading indicator flagged in the mandatrophy analysis and warrants lifecycle-transition monitoring.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theater_measurement_artifact, empirical, 'Attribution of the theater trend: drift versus instrumentation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(humane_treatment_absolute_tr_t0, humane_treatment_standard__absolute_prohibition, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t0, observed).
narrative_ontology:measurement(humane_treatment_absolute_tr_t6, humane_treatment_standard__absolute_prohibition, theater_ratio, 6, 0.3).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t6, observed).
narrative_ontology:measurement(humane_treatment_absolute_tr_t12, humane_treatment_standard__absolute_prohibition, theater_ratio, 12, 0.36).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t12, observed).
narrative_ontology:measurement(humane_treatment_absolute_tr_t18, humane_treatment_standard__absolute_prohibition, theater_ratio, 18, 0.42).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t18, observed).
narrative_ontology:measurement(humane_treatment_absolute_tr_t24, humane_treatment_standard__absolute_prohibition, theater_ratio, 24, 0.46).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t24, observed).
narrative_ontology:measurement(humane_treatment_absolute_tr_t30, humane_treatment_standard__absolute_prohibition, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(humane_treatment_absolute_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(humane_treatment_absolute_be_t0, humane_treatment_standard__absolute_prohibition, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t0, observed).
narrative_ontology:measurement(humane_treatment_absolute_be_t6, humane_treatment_standard__absolute_prohibition, base_extractiveness, 6, 0.58).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t6, observed).
narrative_ontology:measurement(humane_treatment_absolute_be_t12, humane_treatment_standard__absolute_prohibition, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t12, observed).
narrative_ontology:measurement(humane_treatment_absolute_be_t18, humane_treatment_standard__absolute_prohibition, base_extractiveness, 18, 0.72).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t18, observed).
narrative_ontology:measurement(humane_treatment_absolute_be_t24, humane_treatment_standard__absolute_prohibition, base_extractiveness, 24, 0.69).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t24, observed).
narrative_ontology:measurement(humane_treatment_absolute_be_t30, humane_treatment_standard__absolute_prohibition, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(humane_treatment_absolute_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(humane_treatment_absolute_su_t0, humane_treatment_standard__absolute_prohibition, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t0, observed).
narrative_ontology:measurement(humane_treatment_absolute_su_t6, humane_treatment_standard__absolute_prohibition, suppression_requirement, 6, 0.44).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t6, observed).
narrative_ontology:measurement(humane_treatment_absolute_su_t12, humane_treatment_standard__absolute_prohibition, suppression_requirement, 12, 0.5).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t12, observed).
narrative_ontology:measurement(humane_treatment_absolute_su_t18, humane_treatment_standard__absolute_prohibition, suppression_requirement, 18, 0.56).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t18, observed).
narrative_ontology:measurement(humane_treatment_absolute_su_t24, humane_treatment_standard__absolute_prohibition, suppression_requirement, 24, 0.6).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t24, observed).
narrative_ontology:measurement(humane_treatment_absolute_su_t30, humane_treatment_standard__absolute_prohibition, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(humane_treatment_absolute_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Common Article 3 humane treatment standard' decomposes, per the epsilon-invariance principle, into three structurally distinct claims that share one fixed text. This file is the absolute_prohibition member: threshold uncrossable, detainees as full rights-holders, epsilon authored over the standing arrangement as seen from that absolutist seat. The contextual_necessity sibling authors a different victim set (only treatment below its security-adjusted baseline counts) and a different epsilon; the proportionality_balancing sibling makes epsilon threshold-dependent. The upstream member is this one in doctrinal prestige (the text's plain words favor it) while the siblings command more operative state practice — the citation traffic runs in both directions, which is why all three files carry mutual links. Measuring the standard one way (text-as-written) yields near-zero tolerated extraction; measuring it another way (practice-as-permitted) yields substantial extraction; that observable-dependent spread is the signature that these are distinct constraints, not one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
