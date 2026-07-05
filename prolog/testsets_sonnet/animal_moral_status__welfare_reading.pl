% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__welfare_reading, []).

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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare Regulation Reading of the Moral Status Kernel
 *   domain: applied_ethics/legal_philosophy/animal_studies
 *
 * SUMMARY:
 *   This constraint instantiates the welfare reading of the animal moral
 *   status kernel: animals are sentient beings whose suffering matters
 *   morally and should be minimized, but whose use by humans (for food,
 *   research, labor, or entertainment) remains permissible provided cruelty
 *   is avoided. This reading is distinct from the property reading (animals
 *   as resources with no independent moral standing) and the abolitionist
 *   reading (property status itself as the violation, with all use
 *   perpetuating victimization). The welfare reading occupies a middle
 *   structural position: it grants animals moral patienthood (unlike the
 *   property reading) but preserves the legitimacy of use (unlike the
 *   abolitionist reading). Over time, welfare regulation has increasingly
 *   formalized into certification regimes and audited standards whose theater
 *   ratio has risen as compliance documentation and labeling have grown
 *   relative to measurable reductions in animal suffering.
 *
 * KEY AGENTS:
 *   - welfare_certification_organizations: institutional agenda-setter and beneficiary — administers standards, derives legitimacy and funding
 *   - regulated_animal_use_industries: powerful beneficiary/payer — absorbs compliance cost in exchange for continued permission to use animals
 *   - consumers_seeking_ethical_reassurance: organized beneficiary — purchases moral resolution via labeling
 *   - animals_in_regulated_use_systems: powerless, trapped payer — bears residual suffering the standard tolerates as acceptable
 *   - abolitionist_advocates: excluded voice — argues welfare reform entrenches rather than dismantles the underlying property status
 *   - legislators_and_regulators: institutional observer/agenda-setter — enacts and revises the statutory floor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.42).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.55).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare Regulation Reading of the Moral Status Kernel").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/legal_philosophy/animal_studies").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, '11490326-7ce3-4268-80bc-ebac4de2def4').
narrative_ontology:cs_kernel_codification('11490326-7ce3-4268-80bc-ebac4de2def4', distributed).
narrative_ontology:cs_authority_grounding('11490326-7ce3-4268-80bc-ebac4de2def4', distributed).
narrative_ontology:cs_reading_relation('11490326-7ce3-4268-80bc-ebac4de2def4', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('11490326-7ce3-4268-80bc-ebac4de2def4', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('11490326-7ce3-4268-80bc-ebac4de2def4', foundational, sentience_grounds_moral_patienthood).
narrative_ontology:cs_axiom_status(sentience_grounds_moral_patienthood, holdable).
narrative_ontology:cs_axiom_grounding('11490326-7ce3-4268-80bc-ebac4de2def4', sentience_grounds_moral_patienthood, empirically_contingent).
narrative_ontology:cs_axiom('11490326-7ce3-4268-80bc-ebac4de2def4', foundational, use_permissible_if_suffering_minimized).
narrative_ontology:cs_axiom_status(use_permissible_if_suffering_minimized, holdable).
narrative_ontology:cs_axiom_grounding('11490326-7ce3-4268-80bc-ebac4de2def4', use_permissible_if_suffering_minimized, instrumental).
narrative_ontology:cs_reference_frame('11490326-7ce3-4268-80bc-ebac4de2def4', sentience_based_use_permission_framework).
narrative_ontology:cs_drift_state('11490326-7ce3-4268-80bc-ebac4de2def4', contemporary_certification_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('11490326-7ce3-4268-80bc-ebac4de2def4', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, welfare_certification_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, consumers_seeking_ethical_reassurance).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_in_regulated_use_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, regulated_animal_use_industries).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, cruelty_is_wrong_use_is_permissible).
narrative_ontology:constraint_vindicates(animal_moral_status__welfare_reading, suffering_minimization_within_use_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and administer welfare standards (cage sizes, stunning methods, transport durations), certify compliant operations, and derive funding, membership dues, and public legitimacy from being the recognized arbiter of 'humane' treatment. Their institutional survival depends on the use-with-limits framework remaining the operative moral settlement rather than being displaced by an abolitionist framework that would render their certification work obsolete.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_certification_organizations, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, welfare_certification_organizations, beneficiary).

% Meat, dairy, egg, fur, laboratory, and entertainment industries absorb the compliance costs of welfare standards (marginally higher than unregulated operation) in exchange for continued legal and social permission to use animals as resources. They lobby to keep standards achievable and can relocate production to lower-standard jurisdictions if requirements tighten, giving them real exit leverage the animals themselves lack.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_animal_use_industries, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, regulated_animal_use_industries, payer).

% Purchase welfare-labeled products (cage-free, humanely raised, cruelty-free) to resolve moral discomfort about animal use without changing consumption patterns. The welfare label converts an unresolved ethical question into a completed transaction; consumers can choose among labels or ignore them entirely.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, consumers_seeking_ethical_reassurance, beneficiary,
    organized, immediate, mobile, national).

% Bear the residual suffering the welfare standard tolerates as acceptable: confinement, transport stress, painful procedures without anesthesia where deemed impractical, and death by design, all within a framework that improves conditions at the margin but never questions whether they may be used, killed, or owned at all. They cannot exit, cannot be consulted, and their interests are represented only through the same institutions that also serve human beneficiaries.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_in_regulated_use_systems, payer,
    powerless, immediate, trapped, national).

% Argue that welfare reform legitimizes and entrenches the property status it should be dismantling, making animal use more publicly palatable and thus more durable rather than moving toward abolition. Their position is treated within welfare-reading institutions as extreme or impractical and is largely absent from standard-setting processes, which are dominated by industry and welfare-organization negotiation.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    moderate, generational, constrained, national).

% Enact and revise animal welfare statutes, balancing industry lobbying, welfare-organization advocacy, and public sentiment. They adjudicate the boundary between permissible use and prohibited cruelty without addressing whether the use/cruelty distinction itself is coherent, and their statutes give the whole framework its enforcement teeth.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, legislators_and_regulators, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, legislators_and_regulators, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__welfare_reading, welfare_certification_organizations).
narrative_ontology:fixing_cost_class(animal_moral_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legally enforceable standard for what counts as acceptable versus cruel treatment of animals used for food, research, and labor, allowing industries, consumers, and regulators to coordinate around a stable definition rather than each actor litigating the boundary case by case.
% TRANSFER_FUNCTION: Moves moral legitimacy and consumer trust toward regulated industries and certifying welfare organizations, in exchange for marginal reductions in the intensity (not the existence) of suffering imposed on animals within use systems; residual suffering is absorbed entirely by the animals, who receive no share of the legitimacy or revenue the arrangement generates.
% ABSENT_VOICES: Animals themselves cannot articulate objection by definition, and abolitionist advocates who would challenge the use/cruelty distinction itself are structurally marginalized from standard-setting bodies dominated by industry and welfare-organization negotiation.
% DISAPPEARANCE_RATIONALE: If welfare regulation vanished overnight, industries would face no binding floor on treatment methods, welfare-certification organizations would lose their institutional function and funding model, consumer-facing ethical labels would become meaningless, and the legal and commercial infrastructure built around 'humane' use claims would need to be rebuilt from either a pure property framework or a rights framework — a substantial rearrangement, not a null change.
% FOUNDING_PROBLEM: Industrial-scale animal use (factory farming, vivisection, fur production) generated documented suffering severe enough to provoke sustained public outcry and reputational risk to the industries involved; welfare regulation was built to reduce the most visible and politically costly forms of cruelty while preserving the underlying permission to use animals as resources.
% FOUNDING_PROBLEM_CORROBORATION: Regulated industries and welfare-certification organizations attest the founding problem is being actively addressed through evolving standards. Independent animal behavior scientists and abolitionist scholars publishing outside industry-funded venues attest that documented suffering persists largely unabated under 'humane' certification (e.g. peer-reviewed audits of certified slaughter and confinement practices), and that the founding problem has been managed for public perception rather than resolved for the animals — corroboration from outside the beneficiary set exists but is contested by the certifying bodies themselves.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).
:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) because the welfare reading genuinely reduces certain forms of gratuitous cruelty relative to unregulated use, but leaves the core extractive relationship (use, confinement, killing) intact — the reduction is real but bounded by design, not incidental. Suppression (0.55) reflects that alternatives to the use/welfare framework (full abolition, or unregulated property treatment) are actively foreclosed by the legal and institutional apparatus built around the welfare settlement; the framework does not merely coordinate, it also forecloses the abolitionist alternative from serious policy consideration. Theater ratio starts moderate and rises to 0.48 over the interval, reflecting the historical trajectory in which welfare certification has increasingly emphasized labeling, audits, and compliance documentation relative to substantive suffering reduction — a Goodhart-style drift where the proxy (certification compliance) has partially displaced the target (actual suffering minimization). Accessibility collapse (0.5) and resistance (0.45) are authored at moderate levels appropriate to a contested tangled rope: alternatives (full abolition, stricter property-only regimes) remain visible and actively argued, and the framework meets real organized resistance from both directions (industry pushing for looser standards, abolitionists pushing for the framework's dissolution).
 *
 * PERSPECTIVAL GAP:
 *   From the welfare-certification organization's seat, the arrangement is genuine moral progress: a coordination mechanism that channels humane concern into enforceable standards. From the animal's structural position (represented only by proxy, never directly), the same arrangement is extraction with a comfort layer: suffering continues, use continues, ownership continues, and the marginal improvements in method do not touch the underlying claim that animals may be used as means. The engine should register this asymmetry as seat divergence rather than resolve it — the welfare organization computes closer to rope/tangled_rope from its enforcement seat while the animal-payer seat computes closer to snare-adjacent extraction, because the beneficiary relationship (welfare orgs, industry, consumers) and the victim relationship (the animals) run through the identical certification structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare-certification organizations and regulated industries sit near the beneficiary end of directionality: the constraint subsidizes their legitimacy, revenue, and social license respectively, and both retain meaningful exit or adaptation options (organizations can shift standards; industries can relocate production). Consumers benefit from resolved moral discomfort with mobile exit (can switch labels or products). Animals sit at the full-target end: trapped exit options, no voice in standard-setting, and the suffering the standard tolerates is suffering they alone bear with no compensating benefit flowing back to them. This is a textbook tangled rope directionality profile — genuine coordination benefit flowing to the human-side stakeholders, extraction flowing to the non-human payer, through the identical structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (visible, politically costly cruelty in industrial animal use) is contested as live vs. dead: welfare organizations and industries treat the problem as an ongoing, iteratively-solved engineering question (better standards, better audits), while independent behavioral science and abolitionist scholarship argue the founding problem was never actually about animal suffering per se but about managing public tolerance for animal use, and that the mandate has drifted from suffering-reduction toward legitimacy-production. This divergence is exactly the mandatrophy signal the framework is built to surface: a status=contested reading paired with a disappearance_verdict of world_rearranges indicates the arrangement has real stakeholders whose founding claim is disputed by outside parties (independent auditors, abolitionist scholars) rather than settled — the classification should not collapse this into either a clean rope (coordination triumphant) or a clean snare (pure capture), but preserve it as a tangled rope where both readings are simultaneously true of different seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reform_legitimation_effect,
    'Does incremental welfare reform reduce net animal suffering over time, or does it primarily extend the social and legal durability of animal-use industries by making use appear more morally resolved than it is?',
    'Longitudinal comparison of aggregate animal use volumes and per-animal suffering indicators (confinement density, procedure pain scores, transport duration) in jurisdictions with strong welfare regimes versus jurisdictions with weak or absent welfare regulation, controlling for overall demand growth.',
    'If welfare regulation is shown to entrench rather than reduce net suffering (by expanding total use while marginally improving per-unit treatment), the tangled_rope classification is strongly supported and the extraction component should be weighted more heavily than the coordination component. If it is shown to meaningfully reduce net suffering without expanding use, the constraint moves closer to a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_legitimation_effect, empirical, 'Whether welfare reform is net suffering-reducing or primarily legitimation-producing.').

omega_variable(
    sentience_threshold_arbitrariness,
    'Is the welfare reading''s boundary between ''acceptable use with minimized suffering'' and ''cruelty'' a principled moral distinction, or an administratively convenient line that shifts to accommodate whatever level of suffering current industrial practice requires?',
    'Historical trace of how welfare standards have moved (e.g. changes in permitted confinement, slaughter methods, pain management requirements) relative to changes in industrial practice and cost pressure, to see whether standards lead or follow economic feasibility.',
    'If standards are shown to track economic feasibility for industry rather than an independent account of what suffering is tolerable, the welfare reading functions as a rationalization layer over the property reading rather than a genuinely distinct moral position — this would support reclassifying the reading''s coordination function as substantially theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_threshold_arbitrariness, conceptual, 'Whether the cruelty/acceptable-use boundary is principled or economically contingent.').

omega_variable(
    animal_representation_adequacy,
    'Can any human-administered institution (welfare organization, regulator, court) adequately represent animal interests given that animals cannot articulate consent, objection, or preference in the standard-setting process?',
    'This is likely irresolvable empirically; it depends on contested philosophical commitments about proxy representation and moral standing that the framework should flag rather than attempt to settle.',
    'If proxy representation is inherently inadequate, every welfare standard set without direct animal input carries an irreducible legitimacy gap, which should be reflected in a persistently elevated suppression score regardless of how humane the specific standards become.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(animal_representation_adequacy, preference, 'Whether proxy representation of animal interests can ever be structurally adequate.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(anim_tr_t8, animal_moral_status__welfare_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(anim_tr_t16, animal_moral_status__welfare_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__welfare_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(anim_tr_t32, animal_moral_status__welfare_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__welfare_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(anim_be_t8, animal_moral_status__welfare_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(anim_be_t16, animal_moral_status__welfare_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__welfare_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(anim_be_t32, animal_moral_status__welfare_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__welfare_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(anim_su_t8, animal_moral_status__welfare_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(anim_su_t16, animal_moral_status__welfare_reading, suppression_requirement, 16, 0.47).
narrative_ontology:measurement(anim_su_t24, animal_moral_status__welfare_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(anim_su_t32, animal_moral_status__welfare_reading, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__welfare_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the animal_moral_status kernel, each authored as a separate ε-invariant constraint per the decomposition principle. property_reading treats animals as pure resources with no independent moral standing (expected low suppression of the use claim itself, since no moral tension is acknowledged). abolitionist_reading treats property status itself as the violation, placing all use in the victim-generating category regardless of treatment standard (expected high extractiveness across the entire use system). welfare_reading (this file) occupies the structural middle: it grants moral patienthood but preserves use, producing a tangled_rope profile where genuine coordination benefit (legitimacy, revenue, consumer comfort) and genuine extraction (residual animal suffering) run through the identical certification apparatus. The three readings are linked via network edges rather than merged, since their ε values, victim sets, and claimed types differ substantially and would violate ε-invariance if collapsed into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
