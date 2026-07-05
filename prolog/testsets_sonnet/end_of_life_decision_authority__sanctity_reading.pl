% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity-of-Life Prohibition on Intentional Life-Ending
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This story instantiates the sanctity-of-life reading of the end-of-life
 *   decision authority kernel: human life carries intrinsic value independent
 *   of what the individual wills for it, so intentional life-ending — whether
 *   physician-assisted or self-directed with medical cooperation — violates
 *   that value regardless of consent, competence, or suffering severity.
 *   Under this reading, the physician role is healer-only (relieve suffering,
 *   never end life), and the availability of a legal hastened-death option is
 *   treated as itself creating a class of pressured-vulnerable patients — so
 *   the victim set here is patients denied the option they seek, while the
 *   vulnerability-protection beneficiary class is patients hypothetically
 *   protected from an option that does not exist. This is a distinct
 *   constraint from the autonomy_reading (sovereign individual authority over
 *   one's death) and the vulnerability_protection_reading (distributed
 *   institutional checkpoints permitting regulated access) — those are
 *   separate stories with separate ε values, not alternate measurements of
 *   this one. Per the ε-invariance principle, decomposing the kernel into
 *   three readings avoids collapsing structurally distinct claims into a
 *   single averaged constraint.
 *
 * KEY AGENTS:
 *   - terminally_ill_patients_seeking_hastened_death: primary target (powerless/trapped) — bears the extraction of enforced continuation
 *   - patients_with_intractable_suffering: primary target (powerless/trapped) — suffering externalized into clinical management
 *   - family_caregivers_bearing_prolonged_dying: secondary payer (moderate/constrained) — absorbs duration cost
 *   - religious_and_disability_advocacy_institutions: primary beneficiary/agenda_setter (organized/arbitrage) — moral and institutional standing from persistence
 *   - palliative_care_establishment: institutional beneficiary (institutional/arbitrage) — sanctioned alternative channel
 *   - physicians: agenda_setter/payer (institutional/constrained) — enforces at the bedside, bears licensure risk
 *   - legislatures_and_courts: agenda_setter (institutional/analytical) — periodically revisits under pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.42).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.58).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity-of-Life Prohibition on Intentional Life-Ending").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '57e14c92-4afa-4f31-bfd3-bada1d1e3095').
narrative_ontology:cs_kernel_codification('57e14c92-4afa-4f31-bfd3-bada1d1e3095', distributed).
narrative_ontology:cs_authority_grounding('57e14c92-4afa-4f31-bfd3-bada1d1e3095', lineage).
narrative_ontology:cs_interpretation_layer_present('57e14c92-4afa-4f31-bfd3-bada1d1e3095').
narrative_ontology:cs_reading_relation('57e14c92-4afa-4f31-bfd3-bada1d1e3095', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('57e14c92-4afa-4f31-bfd3-bada1d1e3095', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('57e14c92-4afa-4f31-bfd3-bada1d1e3095', foundational, life_value_independent_of_will).
narrative_ontology:cs_axiom_status(life_value_independent_of_will, holdable).
narrative_ontology:cs_axiom_grounding('57e14c92-4afa-4f31-bfd3-bada1d1e3095', life_value_independent_of_will, deontological).
narrative_ontology:cs_axiom('57e14c92-4afa-4f31-bfd3-bada1d1e3095', secondary, physician_role_categorically_healer_only).
narrative_ontology:cs_axiom_status(physician_role_categorically_healer_only, holdable).
narrative_ontology:cs_axiom_grounding('57e14c92-4afa-4f31-bfd3-bada1d1e3095', physician_role_categorically_healer_only, conventional).
narrative_ontology:cs_reference_frame('57e14c92-4afa-4f31-bfd3-bada1d1e3095', medical_oath_non_maleficence_tradition).
narrative_ontology:cs_drift_state('57e14c92-4afa-4f31-bfd3-bada1d1e3095', contemporary_assisted_dying_legalization_wave, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('57e14c92-4afa-4f31-bfd3-bada1d1e3095', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, religious_and_disability_advocacy_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, vulnerable_patients_at_risk_of_coercion).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, palliative_care_establishment).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients_seeking_hastened_death).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, patients_with_intractable_suffering).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, family_caregivers_bearing_prolonged_dying).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, physicians).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, intrinsic_human_dignity_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, life_as_inviolable_good).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces a diagnosis with a foreseeable, often painful trajectory and wants control over its timing and manner. Under this reading, no physician or institution may lawfully assist in ending their life regardless of competence, persistence of request, or documented suffering. Exit is limited to enduring the disease course, refusing treatment/nutrition (which the reading permits as 'allowing to die' rather than 'killing'), or seeking jurisdictions where assistance is legal — an option foreclosed to those without resources to travel.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients_seeking_hastened_death, payer,
    powerless, immediate, trapped, national).

% Experiences suffering that maximal palliative intervention does not fully resolve. The constraint holds that no degree of subjective suffering licenses intentional termination, so their distress is treated as a problem for palliative medicine and psychological support to manage, not as grounds for ending life. Their suffering is externalized — reframed as a treatable clinical state rather than a legitimate basis for the person's own judgment about their life.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, patients_with_intractable_suffering, payer,
    powerless, immediate, trapped, national).

% Provides sustained physical, financial, and emotional care through a dying process that the constraint prevents from being shortened by request. Bears the extended cost of the prohibition without being the party whose life is at stake; can withdraw from caregiving but cannot alter the legal terms of the dying process itself.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, family_caregivers_bearing_prolonged_dying, payer,
    moderate, biographical, constrained, national).

% Lobbies for and defends statutory and professional prohibitions on assisted dying, framing the ban as protection of inherent human worth and of disabled/elderly people against devaluation. Gains moral and institutional standing from the prohibition's persistence and shapes legislative and medical-board language that entrenches it. Faces no direct cost from the constraint's operation.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, religious_and_disability_advocacy_institutions, beneficiary,
    organized, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, religious_and_disability_advocacy_institutions, agenda_setter).

% Operates as the sanctioned alternative to hastened death, receiving referrals, funding, and professional legitimacy premised on the claim that adequate palliation makes intentional life-ending unnecessary. The prohibition channels patients toward this institution's services and forecloses a competing option that would reduce demand for prolonged terminal care.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, palliative_care_establishment, beneficiary,
    institutional, generational, arbitrage, national).

% Includes elderly, disabled, or dependent patients who might otherwise face subtle family, financial, or institutional pressure to request death if the option existed. The prohibition removes that pathway entirely, which this reading treats as protective even though the same patients bear the cost of having no legal option if their own considered wish is to die.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, vulnerable_patients_at_risk_of_coercion, beneficiary,
    powerless, immediate, trapped, national).

% Bound by professional codes and often criminal statute to a healer-only role: physicians may relieve suffering and withhold or withdraw treatment but may not intentionally end life. Enforces the boundary at the bedside, sometimes against a competent patient's explicit and repeated request, and carries the discretion to interpret 'double effect' sedation while bearing licensure and legal risk if that line is judged to have been crossed.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, physicians, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, physicians, payer).

% Writes and adjudicates the statutory prohibition, periodically revisiting it under pressure from right-to-die litigation and advocacy on both sides. Can alter the constraint's scope but faces sustained organized resistance from beneficiary institutions when doing so.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, bright-line professional and legal norm — physicians heal and relieve suffering but never intentionally kill — that protects against diagnostic error, coerced consent, and the normalization of ending vulnerable lives under social or economic pressure.
% TRANSFER_FUNCTION: Moves the burden of a fixed dying timeline from the state/institutions (which bear no liability for facilitating death) onto the dying individual and their caregivers, who absorb the duration, suffering, and cost of a process they cannot legally shorten by request.
% ABSENT_VOICES: Terminally ill patients who have already died without their preferred timing are permanently absent from the debate; competent patients currently requesting hastened death are present but structurally unable to act on the request within this reading, and their testimony is frequently characterized by advocacy institutions as evidence of treatable depression rather than settled judgment.
% DISAPPEARANCE_RATIONALE: If the sanctity prohibition vanished overnight, physicians could legally act on hastened-death requests, palliative care referral patterns and funding models would shift, disability and religious advocacy organizations would lose a central campaign object, and family caregiving trajectories for a subset of terminal patients would shorten substantially — the legal, clinical, and advocacy landscape would reorganize around the newly available option.
% FOUNDING_PROBLEM: Historically built to prevent physicians and institutions from being agents of death — guarding against medical killing under duress, eugenic abuse, and the erosion of trust in the healer role, especially in the shadow of documented 20th-century medical atrocities.
% FOUNDING_PROBLEM_CORROBORATION: Religious and disability-rights organizations attest the founding problem remains fully live, citing ongoing reports of pressured or ambiguous-consent cases in jurisdictions that have legalized assistance. Independent bioethicists, patient-autonomy researchers, and courts in jurisdictions that have introduced regulated assisted-dying frameworks attest that the categorical prohibition addresses a narrower harm than it once did and that procedural safeguards (competency review, waiting periods, multi-physician sign-off) can address the coercion risk without a blanket ban — this corroboration comes from outside the beneficiary set and is contested by it.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).
:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) rather than extreme because the coordination function — protecting against coerced or erroneous medical killing — is genuine and not fabricated; the extraction lies in categorically denying the option to the subset of patients whose competent, persistent, informed request is not coercion-driven. Suppression (0.58) is substantial because the prohibition is backed by criminal law and professional licensure sanction, not merely social pressure. Theater ratio is comparatively low (0.28) because enforcement is largely functional (criminal prosecution risk, licensing board review) rather than symbolic, though it drifts upward slightly as advocacy messaging increasingly emphasizes dignity rhetoric over the narrower coercion-prevention rationale that originally justified the rule. Accessibility collapse (0.61) reflects that once the categorical prohibition is understood, the only 'legal' alternatives (refusing treatment, palliative sedation under double-effect doctrine, cross-border travel) are narrow and unevenly available. Resistance (0.55) reflects sustained right-to-die litigation and advocacy contesting the prohibition.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seat (advocacy institutions, palliative establishment), the prohibition reads as principled protection of inherent worth and a bulwark against a slippery slope toward devaluing disabled and dependent lives. From the payer seat (terminally ill patients, intractable-suffering patients), the identical structure reads as an enforced continuation of suffering imposed by parties who do not bear its cost. Physicians occupy a split position: agenda_setter in enforcing the healer-only boundary, but payer in bearing the professional and legal risk of interpreting where relief of suffering ends and intentional ending begins (the double-effect doctrine).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (religious/disability institutions, palliative establishment) are organized and institutional with arbitrage-grade exit — they set and defend the rule without bearing its costs, so directionality sits near the full-beneficiary end. Victims (terminally ill and intractable-suffering patients) are powerless and trapped — no meaningful exit exists once diagnosed, so directionality sits near the full-target end. The 'vulnerable_patients_at_risk_of_coercion' group is deliberately dual-natured: this reading counts them as beneficiaries of the prohibition's protective function, but they are structurally identical in power/exit to the victim groups — the reading's own logic is what assigns them to the beneficiary side, which is exactly the interpretive commitment this reading makes and the sibling readings dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing medical killing under duress or eugenic pressure — remains genuinely live in some form (documented coercion risk exists), which is why this is authored as tangled_rope rather than pure snare: there is a real coordination function protecting against a real harm. But the categorical (rather than procedural/case-by-case) form of the prohibition extends well beyond what the coercion-prevention rationale requires, and that extension is where the extraction lives. The mandatrophy question is whether a categorical ban is still doing coercion-prevention work that a regulated, checkpoint-based system (the vulnerability_protection_reading) could not do equally well while returning agency to the genuinely non-coerced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_versus_procedural_coercion_prevention,
    'Does preventing coercion of vulnerable patients require a categorical ban on intentional life-ending, or can a procedural/checkpoint system achieve equivalent protection while permitting genuinely voluntary requests?',
    'Comparative outcomes research across jurisdictions with categorical bans versus regulated assisted-dying frameworks with competency review, waiting periods, and multi-physician sign-off — specifically measuring documented coercion incidents under each regime.',
    'If procedural safeguards achieve equivalent coercion prevention, the categorical form of this constraint is extracting beyond its coordination function''s requirements, supporting reclassification toward a more purely extractive reading; if categorical bans are demonstrably necessary, the tangled_rope classification with a large coordination component is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_versus_procedural_coercion_prevention, empirical, 'Whether the categorical form of the prohibition is doing more work than the underlying coercion-prevention rationale justifies.').

omega_variable(
    intrinsic_value_versus_constructed_doctrine,
    'Is ''intrinsic value independent of individual will'' a discoverable moral fact this constraint merely enforces, or a constructed doctrine that happens to align with the institutional interests of religious and palliative-care beneficiaries?',
    'This is likely irreducible by empirical means — it is a foundational metaethical dispute. Partial evidence: cross-cultural and historical variation in how societies have treated intentional life-ending suggests the doctrine is not universally held, which weighs against pure naturalness, though this does not settle the underlying moral question.',
    'If constructed, the beneficiary institutions'' role in shaping and defending the doctrine is better read as interest-aligned advocacy rather than neutral moral discovery, strengthening the tangled_rope reading; if genuinely discoverable, the constraint''s coordination function is more robust than the extraction framing suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_value_versus_constructed_doctrine, conceptual, 'Whether the sanctity claim is a moral discovery or a doctrine whose persistence is partly explained by institutional benefit.').

omega_variable(
    sibling_reading_foreclosure_scope,
    'Does the sanctity_reading''s foreclosure of autonomy_reading hold at the level of individual conscience, or only at the level of institutional/legal policy?',
    'Distinguish personal moral commitment (an individual can privately hold sanctity views while supporting legal autonomy frameworks for others) from institutional policy (a single jurisdiction''s law cannot simultaneously enact both premises as governing rules). Legal philosophy analysis of pluralist frameworks that permit conscientious objection alongside legalized assistance would test whether partial coexistence is possible.',
    'If foreclosure is only at the policy level, the relationship to autonomy_reading might be better modeled as coexists_with in pluralist legal systems with conscience exemptions, rather than a pure forecloses relation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_scope, conceptual, 'Whether the forecloses relation to autonomy_reading holds only at the collective-policy level or also at the individual level.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(end__tr_t8, end_of_life_decision_authority__sanctity_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(end__tr_t16, end_of_life_decision_authority__sanctity_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(end__tr_t24, end_of_life_decision_authority__sanctity_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(end__tr_t32, end_of_life_decision_authority__sanctity_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__sanctity_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(end__be_t8, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(end__be_t16, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(end__be_t24, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(end__be_t32, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(end__su_t8, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 8, 0.51).
narrative_ontology:measurement(end__su_t16, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(end__su_t24, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(end__su_t32, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 32, 0.57).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the end_of_life_decision_authority kernel, decomposed per the ε-invariance principle because the natural-language concept 'who has authority over ending life' resolves into structurally distinct claims with different ε values, different victim sets, and different physician-role framings. sanctity_reading (this story) treats life's value as independent of individual will and categorically prohibits intentional ending; autonomy_reading treats competent individual will as sovereign; vulnerability_protection_reading distributes authority across institutional checkpoints. sanctity_reading forecloses autonomy_reading's core premise (no single legal framework can hold both) and influences vulnerability_protection_reading by supplying the coercion-risk rationale that shapes checkpoint design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
