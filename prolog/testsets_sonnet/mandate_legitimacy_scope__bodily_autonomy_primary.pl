% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__bodily_autonomy_primary, []).

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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: State-Compelled Medical Intervention as Bodily Integrity Violation
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This story instantiates the bodily_autonomy_primary reading of the
 *   mandate_legitimacy_scope kernel: medical intervention without informed
 *   consent violates fundamental bodily integrity irrespective of collective
 *   benefit, meaning the presence of a mandate itself — not its
 *   proportionality, not the severity of the underlying disease — is what
 *   determines the constraint's structure. Under this reading, the moment a
 *   state compels a medical intervention through conditioning employment,
 *   education, or mobility, an unvaccinated-coerced victim class is created
 *   by the mandate's existence, and the state occupies the structural
 *   position of rights violator rather than protector. This is a distinct
 *   constraint from the proportionality_reading (where legitimacy is a
 *   function of disease severity and less-restrictive alternatives) and the
 *   public_health_primary reading (where the state's coercive authority is
 *   treated as legitimate when protecting vulnerable populations). Those are
 *   different constraints, authored separately, with different ε values and
 *   different victim sets — they are not alternative measurements of this
 *   one.
 *
 * KEY AGENTS:
 *   - state_public_health_apparatus: agenda_setter (institutional/analytical) — designs and enforces the mandate
 *   - unvaccinated_coerced_individuals: primary target (powerless/trapped) — bears the bodily cost directly
 *   - medically_contraindicated_individuals: secondary target (powerless/trapped) — bears cost despite documented medical basis for refusal
 *   - vaccine_manufacturers: institutional beneficiary (institutional/arbitrage) — receives guaranteed demand
 *   - medical_ethics_bodies: analytical observer — articulates the consent doctrine baseline the mandate departs from
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.72).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.68).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.72).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "State-Compelled Medical Intervention as Bodily Integrity Violation").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, '6ee4e330-0231-4c86-bfe1-2444b96656c3').
narrative_ontology:cs_kernel_codification('6ee4e330-0231-4c86-bfe1-2444b96656c3', distributed).
narrative_ontology:cs_authority_grounding('6ee4e330-0231-4c86-bfe1-2444b96656c3', distributed).
narrative_ontology:cs_reading_relation('6ee4e330-0231-4c86-bfe1-2444b96656c3', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('6ee4e330-0231-4c86-bfe1-2444b96656c3', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('6ee4e330-0231-4c86-bfe1-2444b96656c3', foundational, consent_is_threshold_not_balanceable).
narrative_ontology:cs_axiom_status(consent_is_threshold_not_balanceable, holdable).
narrative_ontology:cs_axiom_grounding('6ee4e330-0231-4c86-bfe1-2444b96656c3', consent_is_threshold_not_balanceable, deontological).
narrative_ontology:cs_axiom('6ee4e330-0231-4c86-bfe1-2444b96656c3', secondary, collective_benefit_magnitude_cannot_license_nonconsensual_intervention).
narrative_ontology:cs_axiom_status(collective_benefit_magnitude_cannot_license_nonconsensual_intervention, holdable).
narrative_ontology:cs_axiom_grounding('6ee4e330-0231-4c86-bfe1-2444b96656c3', collective_benefit_magnitude_cannot_license_nonconsensual_intervention, deontological).
narrative_ontology:cs_reference_frame('6ee4e330-0231-4c86-bfe1-2444b96656c3', nuremberg_helsinki_informed_consent_doctrine).
narrative_ontology:cs_drift_state('6ee4e330-0231-4c86-bfe1-2444b96656c3', post_pandemic_mandate_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6ee4e330-0231-4c86-bfe1-2444b96656c3', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_apparatus).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, vaccine_manufacturers).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, vaccinated_majority_population).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, medically_contraindicated_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, religious_and_conscience_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces vaccination mandates as a condition of employment, school attendance, travel, or public participation. Frames the mandate as protecting collective health and justifies coercive mechanisms (fines, exclusion, termination) as proportionate. Collects compliance and reduced transmission as institutional success, and bears none of the bodily cost itself.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Face loss of employment, education access, or freedom of movement unless they submit to a medical intervention they have not consented to on the merits. Their bodily tissue is the direct site of the intervention; the cost is borne physically and irreversibly by them alone, regardless of whether the collective benefit materializes for them personally.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, biographical, trapped, national).

% Have a genuine medical reason the intervention is unsafe for them specifically, yet exemption processes are narrow, bureaucratic, and frequently rejected. They are structurally indistinguishable from ordinary refusers in the enforcement apparatus and absorb the same coercive consequences despite a documented medical basis for refusal.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medically_contraindicated_individuals, payer,
    powerless, biographical, trapped, national).

% Object on grounds the state does not weigh as equivalent to medical contraindication. Exemption is discretionary and increasingly withdrawn by policy revision; their only alternatives are compliance against conscience or accepting exclusion from employment, schooling, or public life.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, religious_and_conscience_objectors, payer,
    powerless, biographical, constrained, national).

% Already compliant, they benefit from reduced disease prevalence and unrestricted movement through public and institutional life. They bear none of the bodily-integrity cost the mandate imposes on refusers and often support the mandate's continuation as protecting their own interests.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, vaccinated_majority_population, beneficiary,
    organized, biographical, mobile, national).

% Receive guaranteed demand and liability shielding once a product becomes a mandated condition of participation in public life. Their revenue is decoupled from ordinary market persuasion because the state's coercive apparatus substitutes for voluntary uptake.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, vaccine_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Articulate informed consent doctrine as the baseline of medical ethics since Nuremberg and Helsinki. They document the tension between mandate policy and the doctrine but have no enforcement power over the state's coercive mechanisms.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medical_ethics_bodies, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized under this reading at the level of the individual body: bodily integrity is treated as a threshold right that collective-benefit coordination cannot override, so the arrangement does not solve a coordination problem for the coerced party — it imposes a cost that coordination cannot justify.
% TRANSFER_FUNCTION: Moves bodily autonomy and physical self-determination from the coerced individual to the state's chosen public-health objective, and moves guaranteed demand from the general public to vaccine manufacturers, in exchange for continued participation in employment, education, and public life.
% ABSENT_VOICES: Individuals harmed by rare adverse events, and objectors whose contraindication or conscience claims are rejected by narrow exemption criteria, are not represented in the policy-setting process; their objections are treated as noncompliance rather than as claims requiring adjudication.
% DISAPPEARANCE_RATIONALE: If mandate enforcement disappeared overnight, coerced individuals would regain employment, educational, and mobility access previously conditioned on compliance; the state's compliance-measurement apparatus would lose its object; manufacturer demand would revert to voluntary market dynamics — the arrangement is load-bearing for both sides, not incidental.
% FOUNDING_PROBLEM: Historically, the felt problem was containing transmissible disease in populations where voluntary uptake was judged insufficient to reach protective thresholds.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies attest the problem remains live wherever coverage gaps persist. Independent bioethicists and civil liberties organizations outside the mandate-enforcing institutions attest that, under this reading, the transmission-containment problem does not license overriding individual consent regardless of whether the problem is live — the objection is to the mechanism, not merely to whether the disease threat persists.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises across the interval (0.35 to 0.72) as mandate enforcement mechanisms mature from voluntary encouragement to conditioned access — the theater ratio stays comparatively low (0.25) because the enforcement apparatus is functionally real, not performative: exclusion from employment and schooling is a substantive cost, not a symbolic one. Suppression climbs sharply in the early-to-mid interval (0.30 to 0.68) as exemption pathways narrow and enforcement mechanisms (termination, exclusion) become standardized. Under this reading, resistance is high (0.78) because the claim being violated — bodily integrity — is treated as a threshold right rather than a balanceable interest, so any coercive mechanism meets principled objection independent of the disease context.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus's seat, the mandate is a coordination achievement solving a collective action problem. From the coerced individual's seat, under this reading, no coordination story can license an intervention on their body without their consent — the two seats do not converge on a shared description of the same event, and the engine is expected to compute divergent per-seat types from the identical structural data rather than reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and vaccine manufacturers sit near the full-beneficiary end: the state collects compliance and measurable transmission reduction: manufacturers collect guaranteed demand decoupled from voluntary persuasion. Coerced individuals and medically contraindicated individuals sit near the full-target end: trapped exit options, direct and irreversible bodily cost, no meaningful alternative once the mandate attaches to employment or schooling. The vaccinated majority sits closer to the beneficiary end but did not choose the coercive mechanism itself and is not treated as an agenda_setter — their benefit is incidental to the mandate's enforcement structure, not a cause of it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmission containment) may remain live in an epidemiological sense while the arrangement's legitimacy, under this reading, was never contingent on the founding problem's persistence — it was contingent on consent, which the mandate structurally forecloses. This decouples founding-problem status from legitimacy in a way the other readings do not share; it is the structural marker that distinguishes this reading from proportionality_reading, where founding-problem severity is exactly what would resolve the legitimacy question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bodily_autonomy_absolutism_vs_balancing,
    'Is bodily integrity a threshold right that cannot be outweighed by any collective benefit magnitude, or is it one interest among others subject to a balancing/proportionality test?',
    'This is fundamentally a normative/jurisprudential question, not an empirical one — resolution would require examining which framework a given legal or ethical tradition has adopted (e.g., strict scrutiny vs. rational basis review of bodily intervention), and whether that framework treats consent violations as categorically different from other rights infringements.',
    'If bodily autonomy is absolute, this reading''s classification as the dominant lens is correct and the mandate is properly classified as extraction regardless of disease severity. If balancing is the correct frame, this reading collapses into a limiting case of proportionality_reading rather than standing as an independent claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bodily_autonomy_absolutism_vs_balancing, preference, 'Whether bodily integrity functions as an absolute threshold or a balanceable interest — the foundational fork between this reading and its siblings.').

omega_variable(
    reading_selection_evidentiary_basis,
    'What in the case record or policy record signals that bodily_autonomy_primary is the operative reading rather than proportionality_reading or public_health_primary?',
    'Examine whether exemption processes for medical contraindication and conscience objection were narrow/discretionary (suggesting the state did not treat consent as threshold) versus whether the mandate design included proportionality review mechanisms (disease severity thresholds, sunset provisions, less-restrictive-alternative requirements) that would evidence the proportionality_reading instead.',
    'If the mandate design shows genuine proportionality mechanisms, this story''s high ε and victim-set framing would be contested by the same facts under a sibling reading — the choice of reading is doing significant classificatory work independent of the underlying facts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, conceptual, 'Whether the facts underdetermine which kernel reading is the correct lens, and what evidence would discriminate between them.').

omega_variable(
    manufacturer_beneficiary_directness,
    'Is vaccine manufacturer profit under a mandate regime a direct causal consequence of the coercive mechanism, or would comparable demand have arisen from voluntary public health campaigns absent the mandate?',
    'Compare uptake and revenue trajectories in comparable populations with mandate versus purely voluntary/incentive-based policy regimes.',
    'If demand would be comparable without the mandate, manufacturer beneficiary status is weaker than authored and the extraction is more purely a state-individual dynamic; if mandate-driven demand is substantially higher, the manufacturer beneficiary role is well-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturer_beneficiary_directness, empirical, 'Whether manufacturer benefit is causally tied to the coercive mechanism specifically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mand_tr_t4, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 4, 0.13).
narrative_ontology:measurement(mand_tr_t8, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 8, 0.17).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 12, 0.2).
narrative_ontology:measurement(mand_tr_t16, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 16, 0.22).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 20, 0.24).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 24, 0.25).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(mand_be_t4, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(mand_be_t8, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(mand_be_t16, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 16, 0.69).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 20, 0.71).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(mand_su_t4, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(mand_su_t8, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(mand_su_t16, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__bodily_autonomy_primary, 0.1).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the mandate_legitimacy_scope kernel. bodily_autonomy_primary (this story) treats mandate presence itself as dispositive and installs the state as rights violator with a high, mandate-driven ε. public_health_primary treats the same coercive mechanism as legitimate when necessary to protect vulnerable populations, producing a substantially lower ε and a coordination-dominant classification. proportionality_reading conditions legitimacy on disease severity, safety/efficacy data, and availability of less restrictive alternatives, producing an ε that varies with those inputs rather than with mandate presence alone. All three share the same underlying policy mechanism but diverge in which normative premise adjudicates legitimacy — per the ε-invariance principle, they are authored as three separate constraints rather than one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
