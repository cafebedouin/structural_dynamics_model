% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions 1949 — Humanitarian Ceiling Reading (Common Article 3 / Absolute Minimums)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the humanitarian-ceiling reading of the 1949
 *   Geneva Conventions kernel: the claim that humanitarian minimums
 *   (especially Common Article 3 and the core protections for civilians and
 *   detainees) bind state actors absolutely, irrespective of adversary
 *   reciprocity or the irregular status of the opposing force. This is one of
 *   three structurally distinct readings of the same kernel text. The
 *   conditional-reciprocity reading treats the same text as a reciprocal
 *   bargain that degrades under non-compliance; the security-maximization
 *   reading treats it as aspirational and subordinate to operational
 *   necessity. Each reading is authored as its own constraint story with its
 *   own epsilon, because the humanitarian-ceiling reading's extraction
 *   profile (moderate epsilon, high suppression of security-necessity
 *   arguments, asymmetric burden on state militaries) is not commensurable
 *   with the other readings' profiles — averaging across them would
 *   misrepresent all three.
 *
 * KEY AGENTS:
 *   - civilian_populations_in_conflict_zones: primary beneficiary (powerless/trapped) — receives protection regardless of any party's conduct
 *   - captured_irregular_combatants: primary beneficiary (powerless/trapped) — receives Common Article 3 floor despite lacking formal POW status
 *   - state_military_operational_commanders: primary payer (institutional/constrained) — bears the cost of unconditional restraint
 *   - national_security_establishments: secondary payer/excluded voice (institutional/constrained) — necessity arguments structurally discounted
 *   - icrc_and_monitoring_bodies: analytical observer and institutional beneficiary (organized/analytical) — derives mandate from the ceiling's non-derogability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.42).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.72).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 — Humanitarian Ceiling Reading (Common Article 3 / Absolute Minimums)").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, '75665fbf-5cb1-4962-8f85-02540be23178').
narrative_ontology:cs_kernel_codification('75665fbf-5cb1-4962-8f85-02540be23178', fixed_text).
narrative_ontology:cs_authority_grounding('75665fbf-5cb1-4962-8f85-02540be23178', lineage).
narrative_ontology:cs_interpretation_layer_present('75665fbf-5cb1-4962-8f85-02540be23178').
narrative_ontology:cs_reading_relation('75665fbf-5cb1-4962-8f85-02540be23178', geneva_conventions_1949__conditional_reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('75665fbf-5cb1-4962-8f85-02540be23178', geneva_conventions_1949__security_maximization_reading, forecloses).
narrative_ontology:cs_axiom('75665fbf-5cb1-4962-8f85-02540be23178', foundational, humanitarian_minimums_bind_unconditionally).
narrative_ontology:cs_axiom_status(humanitarian_minimums_bind_unconditionally, holdable).
narrative_ontology:cs_axiom_grounding('75665fbf-5cb1-4962-8f85-02540be23178', humanitarian_minimums_bind_unconditionally, deontological).
narrative_ontology:cs_axiom('75665fbf-5cb1-4962-8f85-02540be23178', foundational, adversary_noncompliance_does_not_license_degradation).
narrative_ontology:cs_axiom_status(adversary_noncompliance_does_not_license_degradation, holdable).
narrative_ontology:cs_axiom_grounding('75665fbf-5cb1-4962-8f85-02540be23178', adversary_noncompliance_does_not_license_degradation, deontological).
narrative_ontology:cs_reference_frame('75665fbf-5cb1-4962-8f85-02540be23178', post_1949_absolute_floor_framework).
narrative_ontology:cs_drift_state('75665fbf-5cb1-4962-8f85-02540be23178', post_9_11_asymmetric_warfare_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('75665fbf-5cb1-4962-8f85-02540be23178', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, captured_irregular_combatants).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, prisoners_of_war).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, icrc_and_monitoring_bodies).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_military_operational_commanders).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, counterinsurgency_forces).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, national_security_establishments).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, human_dignity_is_non_derogable).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, humanitarian_minimums_bind_absolutely).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live inside active or contested combat zones with no ability to relocate or negotiate their own protection. Under this reading, they retain protection against targeting, collective punishment, and reprisal regardless of whether the opposing irregular force observes any convention obligation itself. Their protection does not depend on anything they or their side did.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, immediate, trapped, regional).

% Detained fighters who do not qualify for formal POW status because they fought outside the uniformed, hierarchically-commanded, distinctive-insignia criteria. Under this reading they still receive Common Article 3 minimums — no torture, no summary execution, humane treatment — purely as a floor attached to their humanity, not to their side's compliance record.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, captured_irregular_combatants, beneficiary,
    powerless, immediate, trapped, national).

% Captured state-military personnel who receive full POW protections. Their treatment is meant to be entirely insulated from whether their own government or the detaining power's adversaries comply with the conventions elsewhere in the same conflict.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, prisoners_of_war, beneficiary,
    powerless, immediate, trapped, national).

% Must extend humanitarian treatment to detainees and restraint toward civilians even when the opposing irregular force is documented executing prisoners, using human shields, or feigning civilian status. They bear the operational and force-protection cost of a floor that does not lift even under provable, repeated adversary violation. Their exit is constrained by treaty ratification, domestic military law, and international accountability exposure — they cannot simply opt out without legal and reputational consequence.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_military_operational_commanders, payer,
    institutional, biographical, constrained, national).

% Field units operating against non-uniformed adversaries in ambiguous combat environments bear the practical tactical cost of the ceiling: they must apply humane-treatment and detention rules to persons whose combatant status is contested and who may not extend equivalent treatment to captured state personnel. The asymmetry lands hardest at this level, in real time, under fire.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, counterinsurgency_forces, payer,
    organized, immediate, constrained, regional).

% Ministries and intelligence services that argue security necessity should modulate obligations in asymmetric conflict. Under this reading their necessity arguments are explicitly not a valid basis for lowering the floor; their institutional preference for conditioning protections on adversary conduct is treated as exactly the rationale the ceiling exists to foreclose, so their voice is structurally discounted in the reading's own framework even though they remain bound by and must resource compliance.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, national_security_establishments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__humanitarian_ceiling_reading, national_security_establishments, excluded).

% Serve as guardians and monitors of the ceiling reading, conducting detention visits and reporting on compliance. Their institutional mandate and authority derive from the conventions being treated as absolute and non-derogable; a conditional-reciprocity or security-maximization reading would substantially reduce their monitoring mandate and moral authority.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, icrc_and_monitoring_bodies, beneficiary,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__humanitarian_ceiling_reading, icrc_and_monitoring_bodies, observer).

% Command non-state or irregular forces and are not formally party to the conventions in the way states are, yet their conduct is repeatedly cited (by rival readings) as the reason the ceiling should flex. Under this reading their compliance or non-compliance is irrelevant to whether the ceiling applies to the state actor — but their own troops' protections under Common Article 3 depend on the same floor holding, without their voice shaping how the reading is defended in state legal argument.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_force_commanders, excluded,
    moderate, immediate, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single non-negotiable floor of humane treatment that state militaries commit to in advance, so that no operational commander, government, or conflict phase can argue its way down from that floor by pointing to an adversary's misconduct — this removes the floor itself from the space of tactical or political bargaining.
% TRANSFER_FUNCTION: Moves the cost of restraint from the protected population (civilians, detainees, captured irregulars) onto the state military apparatus — operational flexibility, tactical latitude, and reciprocity leverage are given up by the state so that treatment floors are guaranteed to those who cannot negotiate for themselves.
% ABSENT_VOICES: Irregular force commanders and the populations under their control are not parties to treaty negotiation or interpretation, yet their forces' conduct is the recurring justification rival readings use to argue for degradation; their absence from the interpretive room means the ceiling's defense is argued entirely by states and humanitarian bodies, not by the irregular actors whose behavior is cited against it.
% DISAPPEARANCE_RATIONALE: If the humanitarian-ceiling reading disappeared and the conditional-reciprocity or security-maximization reading prevailed instead, detention practices, targeting rules, and prisoner treatment would shift immediately toward conduct-conditioned protections — commanders would gain formal latitude to degrade treatment in response to adversary non-compliance, and the ICRC's monitoring mandate would lose its non-derogable anchor. Civilian and detainee populations in asymmetric conflicts would face materially different legal protection depending on their adversary's battlefield conduct rather than their own status.
% FOUNDING_PROBLEM: The 1949 conventions, following the Second World War's mass atrocities against POWs and civilians, were built to prevent the recurrence of a world where treatment of the defenseless was set by battlefield reciprocity and could collapse entirely once one side broke the reciprocal bargain.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC and international humanitarian law scholarship (largely outside any single belligerent's chain of command) attest the founding problem remains live — atrocities against detainees and civilians in ongoing asymmetric conflicts are the direct empirical referent. State military and security establishments, who bear the compliance cost, contest whether the absolute-ceiling framing still matches the operational reality of conflicts against non-state actors who do not reciprocate; this contestation is documented in military legal doctrine debates and is not merely self-serving, since career military lawyers on multiple sides raise it independently of political leadership.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).
:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42 rather than low or high: the ceiling reading imposes real, non-trivial costs on state militaries (constrained tactical latitude, detention obligations toward non-reciprocating adversaries) but the transfer serves an identifiable, non-appropriative coordination function — preventing a race-to-the-bottom collapse of humane treatment norms. This is not zero-sum extraction toward a concentrated beneficiary; it redistributes restraint costs toward diffuse protected populations. Suppression is authored high (0.72) because the defining structural move of this reading is the explicit foreclosure of security-necessity and non-reciprocity as valid grounds for degrading the floor — that is a suppression of an entire class of counter-argument, not merely an incidental byproduct. Resistance is correspondingly high (0.68): military and security establishments actively contest the ceiling's absoluteness in doctrine, in domestic legislative debate, and in battlefield practice. Accessibility collapse sits at the midpoint (0.5) because although the ceiling forecloses the necessity argument doctrinally, states retain de facto capacity to violate and absorb the political/legal cost — the alternative is suppressed as legitimate argument but not physically foreclosed as behavior.
 *
 * DIRECTIONALITY LOGIC:
 *   Civilian populations, captured irregulars, and POWs are declared beneficiaries because the reading's entire structural point is to make their protection independent of their own or their adversary's conduct — this is about as clean a beneficiary declaration as exists, since the protection is unconditional by design. State military commanders and counterinsurgency forces are victims/payers because they absorb the tactical and legal cost of a floor that does not lower even against a documented non-compliant adversary — this is the asymmetric burden the expected structural delta specifies. National security establishments are payers with an excluded secondary role: they fund and staff compliance but their preferred interpretive move (conditioning protection on adversary conduct) is exactly what this reading forecloses, so their institutional voice carries less interpretive weight than their financial/operational stake would predict elsewhere.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — atrocities against the defenseless enabled by reciprocity-collapse — remains empirically live per ICRC and IHL scholarship, which sits outside the beneficiary set (protected populations do not author legal doctrine; the corroboration comes from monitoring bodies and legal scholarship). This blocks a mandatrophy verdict: the arrangement is not a vestigial ceremony persisting past its function, because the underlying problem (belligerents targeting or mistreating detainees/civilians when reciprocity collapses) recurs in every asymmetric conflict cited. The contest is not over whether the problem exists but over whether an unconditional-ceiling response is the right-shaped answer to it — that is a live doctrinal dispute, not evidence of obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_exception_scope_ambiguity,
    'Does the humanitarian-ceiling reading''s foreclosure of security-necessity arguments hold even in extreme force-protection scenarios (e.g., credible imminent mass-casualty threat from a detainee), or does customary practice carve an unacknowledged exception that the doctrinal text denies?',
    'Systematic review of state practice and military tribunal rulings for de facto necessity carve-outs versus the formally declared absolute standard; a persistent practice-doctrine gap would indicate the ceiling is aspirationally absolute but operationally conditional.',
    'If a durable practice-level exception exists, the ceiling reading''s suppression score is overstated relative to actual operational constraint, and the reading functions more as a strong presumption than a true absolute floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_exception_scope_ambiguity, empirical, 'Whether the ceiling''s absoluteness holds in extreme operational practice or is a doctrinal fiction with quiet exceptions.').

omega_variable(
    sibling_reading_foreclosure_location,
    'Where precisely does the humanitarian-ceiling reading''s core premise (unconditional floor) structurally conflict with the conditional-reciprocity reading''s core premise (conduct-conditioned floor) — is this a logical foreclosure (both cannot be simultaneously true of the same legal text) or a coexisting interpretive dispute resolvable by different tribunals reaching different holdings?',
    'Doctrinal analysis of whether international courts and domestic military tribunals have treated these as mutually exclusive holdings (one prevails, one is overruled) versus parallel doctrines applied by different fora without formal contradiction.',
    'If genuinely foreclosing, adoption of this reading by a controlling tribunal would render the conditional-reciprocity reading legally untenable in that jurisdiction; if merely coexisting, both readings persist as live positions across different fora indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_location, conceptual, 'Whether this reading logically forecloses the conditional-reciprocity reading or merely coexists with it across different legal fora.').

omega_variable(
    asymmetric_burden_naturalness,
    'Is the asymmetric burden placed on state militaries (versus non-state/irregular forces who are not formal treaty parties) a defensible structural feature of state sovereignty and treaty-making capacity, or is it an artifact of an outdated state-centric framework that under-regulates non-state violence?',
    'Comparative analysis of Additional Protocol II and customary IHL developments extending obligations to non-state armed groups; track whether enforcement mechanisms against irregular forces have matured to closer parity with state obligations.',
    'If the framework has meaningfully evolved to bind non-state actors comparably, the asymmetric-burden critique weakens; if not, the ceiling reading''s cost allocation remains structurally one-sided by design rather than necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetric_burden_naturalness, conceptual, 'Whether the state-only burden allocation is a principled sovereignty feature or an unaddressed structural gap.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.15).
narrative_ontology:measurement(gene_tr_t1965, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1965, 0.18).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2001, 0.25).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2010, 0.27).
narrative_ontology:measurement(gene_tr_t2025, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.28).
narrative_ontology:measurement(gene_be_t1965, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1965, 0.3).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1980, 0.33).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2001, 0.38).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(gene_be_t2025, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.55).
narrative_ontology:measurement(gene_su_t1965, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1980, 0.63).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2001, 0.68).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement(gene_su_t2025, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__humanitarian_ceiling_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposed from the natural-language 'Geneva Conventions' kernel per the epsilon-invariance principle. Each reading assigns a different epsilon and beneficiary/victim structure to the same underlying text: humanitarian_ceiling_reading (this story, epsilon=0.42, moderate extraction with high suppression of necessity arguments), conditional_reciprocity_reading (expected lower baseline extraction with conduct-conditioned protections), and security_maximization_reading (expected highest extraction from protected populations, since protections are treated as suspensible). The three are linked bidirectionally via affects_constraints and are not to be averaged or treated as observational variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
