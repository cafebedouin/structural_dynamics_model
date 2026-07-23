% ============================================================================
% CONSTRAINT STORY: authority_vacuum_incommensurability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_authority_vacuum_incommensurability, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: authority_vacuum_incommensurability
 *   human_readable: Post-Theological Authority Vacuum in Moral Dispute Adjudication
 *   domain: philosophical/social
 *
 * SUMMARY:
 *   MacIntyre's After Virtue diagnosis: modern moral argument (abortion, just
 *   war, euthanasia) proceeds as if premises were shared and a conclusion
 *   could be forced, but the historical institution that once terminated such
 *   disputes by appeal to a fixed interpretive body (Catholic magisterial
 *   authority reading scripture through Aquinas/Augustine) has dissolved.
 *   What remains is a set of incommensurable first-principle frameworks
 *   (autonomy-rights liberalism, Kantian universalizability, sanctity-of-life
 *   traditionalism) that can argue past one another indefinitely because no
 *   institution's ruling is recognized by all sides as dispositive. The
 *   observable is structural: presence or absence of a body whose ruling ends
 *   the argument (contrast historical Church rulings with PCUSA's explicit
 *   declination to rule on Iraq war morality). This story treats the vacuum
 *   itself — not any one framework's substantive content — as the constraint
 *   under analysis; the personhood-boundary kernel and its three readings
 *   (autonomy, golden-rule-consistency, personhood-continuity) are downstream
 *   siblings that would each be authored as separate constraint stories per
 *   the ε-invariance principle, since each reading produces a different
 *   beneficiary/victim structure on the abortion question specifically.
 *
 * KEY AGENTS:
 *   - individual_moral_autonomy: primary beneficiary (moderate/mobile) — gains from the absence of a binding terminus
 *   - social_coordination_capacity: primary victim (powerless/trapped, non-agent) — bears the cost of perpetual re-litigation
 *   - denominational_and_civic_institutions: agenda-setters without terminating power — administer rulings that do not bind
 *   - competing_first_principle_frameworks: organized beneficiaries of stalemate, also payers of permanent contestation
 *   - historical_catholic_magisterium: excluded incumbent — structurally present in genealogy, absent from present adjudication
 *   - moral_philosophers_and_ethicists: analytical observers who diagnose without resolving
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(authority_vacuum_incommensurability, 0.44).
domain_priors:suppression_score(authority_vacuum_incommensurability, 0.28).
domain_priors:theater_ratio(authority_vacuum_incommensurability, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(authority_vacuum_incommensurability, extractiveness, 0.44).
narrative_ontology:constraint_metric(authority_vacuum_incommensurability, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(authority_vacuum_incommensurability, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(authority_vacuum_incommensurability, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(authority_vacuum_incommensurability, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(authority_vacuum_incommensurability, tangled_rope).
narrative_ontology:human_readable(authority_vacuum_incommensurability, "Post-Theological Authority Vacuum in Moral Dispute Adjudication").
narrative_ontology:topic_domain(authority_vacuum_incommensurability, "philosophical/social").

domain_priors:requires_active_enforcement(authority_vacuum_incommensurability).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(authority_vacuum_incommensurability, '3dab3b6f-644d-4bdf-9bc6-6c065ddee814').
narrative_ontology:cs_kernel_codification('3dab3b6f-644d-4bdf-9bc6-6c065ddee814', distributed).
narrative_ontology:cs_authority_grounding('3dab3b6f-644d-4bdf-9bc6-6c065ddee814', distributed).
narrative_ontology:cs_reference_frame('3dab3b6f-644d-4bdf-9bc6-6c065ddee814', magisterial_doctrinal_authority).
narrative_ontology:cs_drift_state('3dab3b6f-644d-4bdf-9bc6-6c065ddee814', post_theological_pluralist_modernity, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('3dab3b6f-644d-4bdf-9bc6-6c065ddee814', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(authority_vacuum_incommensurability, individual_moral_autonomy).
narrative_ontology:constraint_victim(authority_vacuum_incommensurability, social_coordination_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(authority_vacuum_incommensurability, competing_first_principle_frameworks).
narrative_ontology:constraint_victim(authority_vacuum_incommensurability, competing_first_principle_frameworks).
narrative_ontology:constraint_vindicates(authority_vacuum_incommensurability, moral_pluralism_is_the_correct_metaethical_description).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Modern moral agents are no longer bound to accept a single magisterial ruling on contested questions (abortion, just war, sexuality). They select among competing frameworks — autonomy-rights liberalism, Kantian universalizability, sanctity-of-life traditionalism — and can exit any single tradition without institutional penalty, forming their own premises and switching frameworks as convenient.
narrative_ontology:constraint_stakeholder(authority_vacuum_incommensurability, individual_moral_autonomy, beneficiary,
    moderate, biographical, mobile, national).

% The capacity of a polity to reach binding, terminable answers on morally loaded policy questions (abortion law, war justification, end-of-life policy) has no institutional backstop. Legislatures, courts, and denominations (e.g. PCUSA declining to rule on the Iraq war) each speak but none terminates the dispute, so the same arguments recur in every legislative cycle, culture-war flashpoint, and generation, at a real cost to durable policy and to the possibility of moral consensus.
narrative_ontology:constraint_stakeholder(authority_vacuum_incommensurability, social_coordination_capacity, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(authority_vacuum_incommensurability, social_coordination_capacity).

% Bodies like PCUSA, the Catholic magisterium, national courts, and bioethics commissions still issue statements and rulings on contested moral questions, but none commands cross-institutional recognition as terminating. Each administers its own internal procedure for producing a ruling while lacking the power to bind rival institutions or dissenting citizens — agenda-setting without adjudicative finality.
narrative_ontology:constraint_stakeholder(authority_vacuum_incommensurability, denominational_and_civic_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(authority_vacuum_incommensurability, denominational_and_civic_institutions, excluded).

% Autonomy-rights liberalism, Kantian universalizability, and sanctity-of-life traditionalism each survive and even flourish in the vacuum — no rival framework can be forced to concede defeat. Each camp benefits from the absence of an adjudicator that could rule it wrong, but each also pays the cost of permanent contestation: no framework converts its intellectual victories into settled social fact.
narrative_ontology:constraint_stakeholder(authority_vacuum_incommensurability, competing_first_principle_frameworks, beneficiary,
    organized, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(authority_vacuum_incommensurability, competing_first_principle_frameworks, payer).

% The historical institution whose scriptural-plus-Thomistic interpretive chain once terminated moral arguments for a large civilizational population has lost the practical capacity to bind non-adherents; its rulings remain doctrinally coherent internally but no longer function as a shared adjudicating authority for the broader polity. It is structurally present in the genealogy but absent from the current adjudication.
narrative_ontology:constraint_stakeholder(authority_vacuum_incommensurability, historical_catholic_magisterium, excluded,
    institutional, civilizational, identity_locked, global).

% Diagnose the incommensurability (MacIntyre's own project) without themselves being able to resolve it — their analytical work documents the vacuum but does not fill it; the profession itself has fragmented along the same first-principle lines it studies.
narrative_ontology:constraint_stakeholder(authority_vacuum_incommensurability, moral_philosophers_and_ethicists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(authority_vacuum_incommensurability, diffuse).
narrative_ontology:fixing_cost_class(authority_vacuum_incommensurability, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: When it existed, the terminating authority solved a genuine coordination problem: it converted endless first-principle disagreement into a single binding answer that legislatures, courts, and individuals could act on without re-litigating foundational premises each time.
% TRANSFER_FUNCTION: The vacuum transfers the burden of premise-selection and dispute-resolution from a central authority onto every individual disputant and downstream institution; it moves the cost of unresolved argument from a one-time adjudicating body onto recurring, diffuse social and legislative cycles.
% ABSENT_VOICES: The historical magisterium and any successor claiming comparable terminating authority are structurally excluded from the current adjudication — not silenced by force, but simply no longer recognized as binding by rival frameworks or by the state. Those who would want a genuine terminus (rather than perpetual contestation) have no seat that can deliver one.
% DISAPPEARANCE_RATIONALE: If the vacuum were somehow filled overnight by a new universally-recognized adjudicating authority, the beneficiaries of pluralism (individual autonomy, competing frameworks) would experience a major rearrangement — loss of exit and arbitrage options. Those bearing the coordination cost would experience relief. Whether the world 'rearranges' or 'stays the same' therefore depends on which seat is asked; there is no neutral answer, which is itself evidence of the incommensurability the constraint names.
% FOUNDING_PROBLEM: Pre-modern polities needed a way to terminate first-principle moral disputes (on personhood, war, sexuality) so that law, policy, and shared social life could proceed without perpetual re-litigation of ultimate premises; the Catholic magisterium (and analogous authorities elsewhere) filled that functional slot for centuries.
% FOUNDING_PROBLEM_CORROBORATION: MacIntyre's own diagnosis in After Virtue, written from outside any single framework's beneficiary interest, attests that the coordination problem (terminating incommensurable moral arguments) remains live and unsolved in secular modernity; legislative gridlock on abortion, euthanasia, and just-war questions across multiple jurisdictions corroborates the persistence of the problem independent of any single tradition's self-interested account.
narrative_ontology:disappearance_verdict(authority_vacuum_incommensurability, contested).
narrative_ontology:founding_problem_status(authority_vacuum_incommensurability, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(authority_vacuum_incommensurability, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(authority_vacuum_incommensurability, 'none', 1).
narrative_ontology:epsilon_provenance(authority_vacuum_incommensurability, 0.44, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(authority_vacuum_incommensurability_tests).
:- end_tests(authority_vacuum_incommensurability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.44) and rising over the interval: the vacuum does not extract wealth or labor, but it extracts settled coordination capacity — each unresolved cycle of the abortion/war/euthanasia debates consumes legislative, judicial, and civic energy that a terminating authority would have conserved. Suppression is comparatively low and slightly falling (0.28, down from 0.35): the vacuum is not maintained by coercion; if anything, coercive enforcement of any single framework has become harder over time as pluralism entrenches, which is precisely why the constraint persists structurally rather than by force. Theater ratio rises sharply (0.15 to 0.52) because as genuine adjudicative capacity erodes, institutions substitute performative rulings (statements, resolutions, commission reports) that resemble termination but bind no one — the PCUSA declination is the paradigm case of an institution being honest about this rather than performing it, which makes it a diagnostic anomaly against the rising-theater trend. Accessibility collapse is low (0.35): alternative frameworks remain fully visible and adoptable, which is the opposite of collapse. Resistance is high (0.71): every framework actively resists ceding ground to rivals, which is exactly what keeps the vacuum stable rather than resolving into a new authority.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of individual_moral_autonomy the vacuum looks like liberation — a rope, even: genuine coordination-free pluralism with no coercive overhead. From the seat of social_coordination_capacity (and the institutions trying to legislate durably) the same structure looks like an unfillable tangled extraction: society pays repeatedly for arguments that never terminate, and the payment recurs every generation because nothing forecloses it. The engine should compute these as different per-seat types from the same structural data — that divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual_moral_autonomy is declared beneficiary because the absence of a terminating authority is precisely what preserves the space for framework-switching and premise-selection; its exit options are mobile because no single tradition can compel adherence. Social_coordination_capacity is declared victim and modeled as a non-agent (a capacity, not an actor) with trapped exit and powerless standing — it cannot exit the vacuum because it is the substrate the vacuum operates on, not a party that chooses. Competing_first_principle_frameworks carry organized power with arbitrage exit: each tradition can retreat into its own institutional space when a dispute goes against it, never fully conceding. Historical_catholic_magisterium is identity_locked rather than mobile or trapped — its interpretive commitments are constitutive of what the institution is, so it cannot simply adopt a rival framework without ceasing to be itself; it is excluded from the present adjudication not by force but by the erosion of cross-institutional recognition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (terminating first-principle disputes so social life can proceed) is authored as live, not dead — legislatures still gridlock on exactly these questions, so this is not a case of a mandate persisting after its function disappeared. What has happened instead is that the institution capable of performing the function has dissolved while the need for the function persists, which is the mirror image of ordinary mandatrophy (function-gone-institution-persists). Framing this as tangled_rope rather than snare or piton matters: there IS a genuine coordination function that termination-capable authority used to provide (a rope-like function), but its absence now falls asymmetrically on those who need durable collective answers (victims) while benefiting those who prefer permanent optionality (beneficiaries) — and the arrangement (institutions still issuing non-binding rulings) requires active maintenance (drafting, deliberating, publishing statements) even though it cannot deliver the terminating function it gestures at. That combination — real coordination history, present asymmetric cost-bearing, and active but hollow institutional maintenance — is what places this in tangled_rope territory rather than a clean mountain (it is not natural law) or clean piton (it is not merely inertial; frameworks actively compete for the vacated authority space).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vacuum_is_permanent_or_transitional,
    'Is the post-magisterial authority vacuum a permanent feature of pluralist modernity, or a transitional state pending the emergence of some new terminating authority (a secular equivalent, a global juridical body, a technocratic consensus mechanism)?',
    'Longitudinal tracking of whether any institution (international courts, transnational bioethics bodies, AI-mediated consensus mechanisms) begins to acquire cross-framework terminating recognition over multiple generations.',
    'If permanent, this constraint is correctly classified as a stable tangled_rope with no scaffold trajectory. If transitional, it should instead be modeled with a sunset expectation and re-evaluated as a scaffold once a successor authority stabilizes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vacuum_is_permanent_or_transitional, conceptual, 'Whether the vacuum is a stable equilibrium or an interim state.').

omega_variable(
    coordination_cost_measurement_validity,
    'Can the cost social_coordination_capacity bears from unresolved moral disputes actually be measured (legislative cycles consumed, litigation volume, social polarization indices), or is this cost inherently diffuse and unmeasurable, making the extractiveness score partly speculative?',
    'Comparative institutional analysis: measure legislative time and judicial resources consumed by recurring moral-framework disputes (abortion law revisited across decades) versus jurisdictions with a stronger single-framework consensus, controlling for other polarization drivers.',
    'If the cost is measurable and substantial, the extractiveness trajectory is empirically grounded; if the cost cannot be isolated from other polarization drivers, the extractiveness score should be treated as a lower-confidence estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_measurement_validity, empirical, 'Whether the diffuse coordination cost can be empirically isolated.').

omega_variable(
    kernel_framing_underdetermination,
    'Should this constraint be framed around the general authority-vacuum structure (any morally loaded question lacking a terminating adjudicator), or specifically around the personhood_boundary_kernel (the abortion-specific contest among autonomy, golden-rule-consistency, and personhood-continuity readings)? The two framings produce different beneficiary/victim structures: the general framing names diffuse social_coordination_capacity as victim, while the kernel-specific framing would name the embryo (under the personhood_continuity_reading) or the mother (under the autonomy_reading) as the concrete rights-bearing party at stake.',
    'Decompose per the ε-invariance principle: this story stays at the general authority-vacuum level; the three personhood_boundary_kernel readings (autonomy_reading, golden_rule_consistency_reading, personhood_continuity_reading) should each be authored as separate sibling constraint stories with their own ε, beneficiaries, and victims, linked via network.affects_constraints.',
    'Conflating the general vacuum with any one kernel reading would either dilute the reading''s specific victim claim or falsely universalize the vacuum''s diffuse-cost structure onto a dispute with a concrete rights-holder at stake. Keeping them separate preserves both the general diagnosis and the reading-specific structural claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether to model at the general vacuum level or decompose into kernel-specific readings; this story deliberately stays general.').

omega_variable(
    magisterium_natural_or_constructed_authority,
    'Was the historical Catholic magisterium''s terminating authority itself a genuine mountain-like fixed point (a correctly discovered moral order), or a constructed authority that benefited the institution wielding it, later exposed as constructed once alternative frameworks proved viable?',
    'Historical and comparative religious-authority analysis: examine whether comparable terminating authorities arose independently in other traditions absent institutional self-interest, versus evidence that magisterial rulings tracked institutional power preservation.',
    'If genuinely natural-law-like, its dissolution is a pure loss of correctly-functioning coordination infrastructure. If substantially constructed and self-serving, its dissolution is partly a correction, and the current vacuum''s cost is partially offset by removal of an extractive predecessor arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(magisterium_natural_or_constructed_authority, conceptual, 'Whether the dissolved authority was itself extractive, complicating the cost accounting of its absence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(authority_vacuum_incommensurability, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auth_tr_t0, authority_vacuum_incommensurability, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(auth_tr_t0, observed).
narrative_ontology:measurement(auth_tr_t10, authority_vacuum_incommensurability, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(auth_tr_t10, observed).
narrative_ontology:measurement(auth_tr_t20, authority_vacuum_incommensurability, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(auth_tr_t20, observed).
narrative_ontology:measurement(auth_tr_t30, authority_vacuum_incommensurability, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(auth_tr_t30, observed).
narrative_ontology:measurement(auth_tr_t45, authority_vacuum_incommensurability, theater_ratio, 45, 0.47).
narrative_ontology:measurement_basis(auth_tr_t45, observed).
narrative_ontology:measurement(auth_tr_t60, authority_vacuum_incommensurability, theater_ratio, 60, 0.52).
narrative_ontology:measurement_basis(auth_tr_t60, projected).

% Extraction over time
narrative_ontology:measurement(auth_be_t0, authority_vacuum_incommensurability, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(auth_be_t0, observed).
narrative_ontology:measurement(auth_be_t10, authority_vacuum_incommensurability, base_extractiveness, 10, 0.24).
narrative_ontology:measurement_basis(auth_be_t10, observed).
narrative_ontology:measurement(auth_be_t20, authority_vacuum_incommensurability, base_extractiveness, 20, 0.31).
narrative_ontology:measurement_basis(auth_be_t20, observed).
narrative_ontology:measurement(auth_be_t30, authority_vacuum_incommensurability, base_extractiveness, 30, 0.37).
narrative_ontology:measurement_basis(auth_be_t30, observed).
narrative_ontology:measurement(auth_be_t45, authority_vacuum_incommensurability, base_extractiveness, 45, 0.41).
narrative_ontology:measurement_basis(auth_be_t45, observed).
narrative_ontology:measurement(auth_be_t60, authority_vacuum_incommensurability, base_extractiveness, 60, 0.44).
narrative_ontology:measurement_basis(auth_be_t60, projected).

% Suppression requirement over time
narrative_ontology:measurement(auth_su_t0, authority_vacuum_incommensurability, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(auth_su_t0, observed).
narrative_ontology:measurement(auth_su_t10, authority_vacuum_incommensurability, suppression_requirement, 10, 0.32).
narrative_ontology:measurement_basis(auth_su_t10, observed).
narrative_ontology:measurement(auth_su_t20, authority_vacuum_incommensurability, suppression_requirement, 20, 0.3).
narrative_ontology:measurement_basis(auth_su_t20, observed).
narrative_ontology:measurement(auth_su_t30, authority_vacuum_incommensurability, suppression_requirement, 30, 0.29).
narrative_ontology:measurement_basis(auth_su_t30, observed).
narrative_ontology:measurement(auth_su_t45, authority_vacuum_incommensurability, suppression_requirement, 45, 0.28).
narrative_ontology:measurement_basis(auth_su_t45, observed).
narrative_ontology:measurement(auth_su_t60, authority_vacuum_incommensurability, suppression_requirement, 60, 0.28).
narrative_ontology:measurement_basis(auth_su_t60, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(authority_vacuum_incommensurability, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(authority_vacuum_incommensurability, 0.12).
narrative_ontology:affects_constraint(authority_vacuum_incommensurability, personhood_boundary_kernel_autonomy_reading).
narrative_ontology:affects_constraint(authority_vacuum_incommensurability, personhood_boundary_kernel_golden_rule_consistency_reading).
narrative_ontology:affects_constraint(authority_vacuum_incommensurability, personhood_boundary_kernel_personhood_continuity_reading).

% DUAL FORMULATION NOTE:
% This story models the general structural condition (absence of any terminating adjudicator for first-principle moral disputes) as a single tangled_rope constraint with stable ε across all contested domains it governs (abortion, just war, euthanasia). The personhood_boundary_kernel and its three sibling readings are downstream, domain-specific constraints: each reading of the personhood question produces a distinct beneficiary/victim structure (mother-as-sole-rights-holder vs. conditional-constraint vs. embryo-as-rights-holder) and should be authored as separate constraint_story files per the ε-invariance principle, each linking back to this constraint via their own network.affects_constraints. This story is the upstream, more general member of that family; the kernel readings are downstream and more contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
