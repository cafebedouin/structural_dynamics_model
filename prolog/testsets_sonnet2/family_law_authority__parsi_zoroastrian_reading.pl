% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Endogamous Marriage Governance
 *   domain: religious_governance/family_law
 *
 * SUMMARY:
 *   This story instantiates the Parsi Zoroastrian reading of the
 *   family_law_authority kernel: marriage as a community-preserving
 *   institution whose religious validity and membership consequences are
 *   governed by priestly ritual authority and panchayat trust administration,
 *   rather than by state contract law, ecclesiastical sacrament, dharmic
 *   samskara, or Quranic contract. The reading's distinctive structural delta
 *   is the endogamy requirement — loss of community status (and,
 *   historically, gender-asymmetric loss for women versus men) upon marrying
 *   outside the faith — combined with priestly control over ritual validity
 *   (Ashirvad, navjote) and an explicit small-community-preservation logic
 *   that treats demographic survival as the coordinating rationale.
 *   Extraction is measured for the standing arrangement as it operates today:
 *   a bounded, resource-controlling trust structure that transfers costs onto
 *   intermarrying members and their children while the priesthood and
 *   trustees retain interpretive and administrative control.
 *
 * KEY AGENTS:
 *   - zoroastrian_priesthood: ritual authority setting membership boundaries
 *   - parsi_panchayat_trustees: administer excludable communal resources
 *   - intermarrying_parsi_women: bear the sharpest structural cost
 *   - children_of_intermarriage: excluded without having made the choice
 *   - reformist_parsi_advocates: contest the rule from inside the community, largely unheeded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.58).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.62).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Endogamous Marriage Governance").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "religious_governance/family_law").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, 'a9f41e29-5fbd-4878-8e1c-9803479f4e68').
narrative_ontology:cs_kernel_codification('a9f41e29-5fbd-4878-8e1c-9803479f4e68', distributed).
narrative_ontology:cs_authority_grounding('a9f41e29-5fbd-4878-8e1c-9803479f4e68', lineage).
narrative_ontology:cs_interpretation_layer_present('a9f41e29-5fbd-4878-8e1c-9803479f4e68').
narrative_ontology:cs_reading_relation('a9f41e29-5fbd-4878-8e1c-9803479f4e68', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9f41e29-5fbd-4878-8e1c-9803479f4e68', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9f41e29-5fbd-4878-8e1c-9803479f4e68', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9f41e29-5fbd-4878-8e1c-9803479f4e68', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('a9f41e29-5fbd-4878-8e1c-9803479f4e68', foundational, community_membership_transmitted_through_endogamous_lineage).
narrative_ontology:cs_axiom_status(community_membership_transmitted_through_endogamous_lineage, holdable).
narrative_ontology:cs_axiom_grounding('a9f41e29-5fbd-4878-8e1c-9803479f4e68', community_membership_transmitted_through_endogamous_lineage, conventional).
narrative_ontology:cs_axiom('a9f41e29-5fbd-4878-8e1c-9803479f4e68', foundational, priestly_ritual_authority_determines_religious_marriage_validity).
narrative_ontology:cs_axiom_status(priestly_ritual_authority_determines_religious_marriage_validity, holdable).
narrative_ontology:cs_axiom_grounding('a9f41e29-5fbd-4878-8e1c-9803479f4e68', priestly_ritual_authority_determines_religious_marriage_validity, conventional).
narrative_ontology:cs_axiom('a9f41e29-5fbd-4878-8e1c-9803479f4e68', secondary, small_population_survival_justifies_boundary_enforcement).
narrative_ontology:cs_axiom_status(small_population_survival_justifies_boundary_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('a9f41e29-5fbd-4878-8e1c-9803479f4e68', small_population_survival_justifies_boundary_enforcement, instrumental).
narrative_ontology:cs_reference_frame('a9f41e29-5fbd-4878-8e1c-9803479f4e68', post_migration_diaspora_endogamy_covenant).
narrative_ontology:cs_drift_state('a9f41e29-5fbd-4878-8e1c-9803479f4e68', contemporary_demographic_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a9f41e29-5fbd-4878-8e1c-9803479f4e68', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_panchayat_trustees).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, endogamous_community_members).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, intermarrying_parsi_women).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, children_of_intermarriage).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_men_marrying_out_partially_shielded).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_men_marrying_out_partially_shielded).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, community_survival_through_bloodline_preservation).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, ritual_purity_of_navjote_initiation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls ritual validity of marriage (Ashirvad ceremony) and navjote initiation into the faith. Determines which unions and which children are recognized as religiously Zoroastrian. Frames endogamy as necessary for preserving a demographically shrinking community and its purity of practice; this framing is also the basis of the priesthood's continued relevance and income from ritual fees.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, zoroastrian_priesthood, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Administer community trusts, housing colonies (baugs), charitable funds, and Tower of Silence access, all restricted to those recognized as community members through endogamous lineage or male-line descent. Litigate to defend the endogamy rule in civil courts. Control admission to the material benefits that make community membership valuable.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_panchayat_trustees, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, parsi_panchayat_trustees, beneficiary).

% Marry within the community and retain full access to trust housing, charitable funds, religious ceremonies, and social standing. Benefit from a bounded marriage market with clear rules and from the collective identity-preservation project, without personally bearing the exclusion costs imposed on intermarrying members.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, endogamous_community_members, beneficiary,
    moderate, generational, constrained, national).

% A Parsi woman who marries outside the community is historically deemed to have left the faith and loses access to community trusts, colony housing, fire temple entry, and Tower of Silence rites for herself; children of such unions are traditionally denied navjote initiation. She cannot un-marry to regain status once married out, and litigation over this asymmetry (compared to men marrying out) has spanned decades without full resolution in every community trust.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, intermarrying_parsi_women, payer,
    powerless, biographical, trapped, national).

% Born to a Parsi parent (especially a mother) and a non-Parsi parent, they are frequently denied navjote initiation and therefore religious and community membership regardless of upbringing or desire to belong. They had no part in the marriage decision that determines their exclusion and cannot appeal to any process they control.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, children_of_intermarriage, payer,
    powerless, biographical, trapped, national).

% A Parsi man who marries outside the community traditionally retains his own community status and trust access under patrilineal custom, while his non-Parsi wife and children face exclusion or contested status. He bears less direct cost than an intermarrying woman but still faces community disapproval and contributes to demographic decline that the endogamy rule claims to be preventing.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_men_marrying_out_partially_shielded, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, parsi_men_marrying_out_partially_shielded, beneficiary).

% Organized groups and individual litigants who argue the endogamy rule accelerates the very community extinction it claims to prevent, and that gender-asymmetric enforcement is an equal-protection violation. They petition trusts and courts but their voice is structurally minority within panchayat governance and is frequently outvoted or excluded from trustee decision-making.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, reformist_parsi_advocates, excluded,
    organized, generational, constrained, national).

% Adjudicate disputes over trust access and religious status where Parsi personal law intersects with constitutional equal-protection guarantees, generally deferring to religious community self-governance over marriage and membership while occasionally ruling narrowly on specific trust-access disputes.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, indian_civil_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(family_law_authority__parsi_zoroastrian_reading, parsi_panchayat_trustees).
narrative_ontology:fixing_cost_class(family_law_authority__parsi_zoroastrian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a small, demographically shrinking religious-ethnic community (fewer than ~60,000 in India) with a bright-line rule for who counts as a member, coordinating access to scarce communal resources (trust housing, charitable funds, funerary rites, ritual spaces) and preserving a distinct set of religious practices believed to require lineage continuity.
% TRANSFER_FUNCTION: Moves the burden of community-boundary maintenance onto individuals who marry outside the community and their children — primarily women and their offspring under patrilineal custom — transferring continued access to trust wealth, ritual participation, and social belonging to those who marry endogamously.
% ABSENT_VOICES: Intermarried women, their children, and reformist advocates raise objections in petitions and litigation but hold few or no seats on panchayat boards of trustees, which are typically composed of members elected from within the endogamous community itself.
% DISAPPEARANCE_RATIONALE: If the endogamy requirement and its enforcement through trust and ritual gatekeeping vanished, community trust funds and colony housing would become open to a much larger population of mixed-heritage descendants, priestly authority over who counts as Zoroastrian would weaken substantially, and the community's self-conception as a closed, ethnically bounded faith would be forced to confront reconstitution around religious practice rather than lineage.
% FOUNDING_PROBLEM: Following migration from Persia to the Indian subcontinent roughly a millennium ago, a small refugee community sought to preserve a distinct religious and ethnic identity against assimilation into a much larger surrounding Hindu and later Muslim population, using strict endogamy as a defense against demographic and cultural dissolution.
% FOUNDING_PROBLEM_CORROBORATION: Panchayat trustees and priesthood attest the founding problem remains live, citing demographic decline as vindication. Demographers studying the community from outside its institutions attest the opposite: population geneticists and sociologists note the endogamy rule now accelerates population collapse rather than preventing it, since it excludes children of mixed marriages who would otherwise sustain community numbers — an assessment corroborated by independent demographic studies commissioned by parties outside the trust structure, not by the trustees themselves.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a real but partial transfer: intermarrying members and their children lose access to trust wealth and ritual participation, but the community also provides a genuine coordination good (bounded resource pool, cultural continuity, funerary and ritual infrastructure) to those who remain within its rules. Suppression (0.62) is substantial because exit from status-loss is not available after the fact — a woman who marries out cannot undo that status loss — and the mechanism is enforced through trust litigation and priestly non-recognition. Theater ratio (0.3) is moderate: some panchayat rhetoric about demographic survival outpaces the actual efficacy of endogamy as a survival strategy (which demographers argue accelerates decline), but real material and ritual functions persist. Accessibility collapse (0.65) is high because once inside the framework, alternatives (recognition by other denominations, civil-only marriage recognition for community purposes) are largely foreclosed by panchayat and priestly non-recognition. Resistance (0.55) reflects active internal reformist contestation without full success.
 *
 * PERSPECTIVAL GAP:
 *   From the priesthood and trustee seats, this is Rope or even Mountain-adjacent: an ancient, functionally necessary boundary-maintenance practice that any small community would need to survive. From the seat of an intermarried woman or her excluded child, the same rule computes as an enforced, asymmetric extraction of status and material access for a decision (whom to marry, or being born) that carries costs disproportionate to any coordination benefit they personally receive. The engine's per-seat computation should surface this divergence rather than resolve it toward either reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Priesthood and trustees sit at the low end of directionality: they administer the rule, collect deference and resources through it, and face no personal exclusion risk (d near beneficiary end, arbitrage exit — they can reinterpret or selectively enforce). Endogamous members are moderate beneficiaries with constrained exit (they retain benefits contingent on continued compliance). Intermarrying women and their children sit at the high end of directionality: trapped exit, powerless position, and the clearest transfer of costs onto them — the classic tangled-rope victim profile. Parsi men marrying out are given a directionality override reflecting their partially shielded position under patrilineal custom, differentiating them from women in the identical nominal 'intermarried' category despite the derivation chain treating exit/victim status similarly absent the override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (demographic and cultural survival against assimilation) is genuinely contested as still live: the community is far smaller and more endangered today than a century ago, giving the priesthood and trustees a plausible ongoing-function claim. But independent demographic analysis corroborates that the endogamy mechanism itself, as currently enforced, worsens rather than solves the founding problem by excluding descendants who would otherwise sustain community numbers. This is the mandatrophy signature: an arrangement whose stated founding problem is real, but whose specific enforcement mechanism has become counterproductive to that very problem while continuing to be defended in the founding problem's name — which is why tangled_rope (genuine coordination function plus asymmetric extraction requiring active enforcement) rather than pure snare is the structurally accurate claim, and why founding_problem_status is authored as contested rather than dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogamy_survival_efficacy,
    'Does strict endogamy actually preserve the Parsi community, or does it accelerate the demographic collapse it claims to prevent by excluding children of intermarriage who would otherwise sustain population numbers?',
    'Comparative demographic modeling of communities that relaxed versus maintained strict endogamy/patrilineal recognition rules, tracking population trajectories over multiple generations; existing demographic studies of the Parsi population already point toward this resolution.',
    'If endogamy is shown to worsen the founding problem, the coordination-function claim weakens substantially and the arrangement looks more like pure extraction dressed in survival rhetoric — pushing the classification toward snare. If endogamy is shown to meaningfully slow assimilation-driven cultural loss independent of raw population count, the coordination claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_survival_efficacy, empirical, 'Whether the endogamy mechanism serves or undermines its stated demographic-survival purpose.').

omega_variable(
    gender_asymmetric_enforcement_origin,
    'Is the asymmetric treatment of intermarrying women versus men a core religious requirement or a patrilineal customary accretion separable from Zoroastrian doctrine itself?',
    'Textual and historical analysis of Zoroastrian scripture and early practice versus colonial-era and modern panchayat trust deed language, to determine whether the asymmetry is doctrinally grounded or administratively constructed.',
    'If the asymmetry is a customary/administrative accretion rather than doctrinal requirement, its persistence via priestly and trustee enforcement looks more like discretionary extraction than genuine religious-law fidelity, strengthening the tangled_rope (or even snare) reading for the gender-differentiated victim group specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_asymmetric_enforcement_origin, conceptual, 'Whether gender-asymmetric status loss is doctrinal or customary in origin.').

omega_variable(
    kernel_reading_boundary_location,
    'Where precisely does the parsi_zoroastrian_reading''s distinguishing claim sit relative to the muslim_shariat_reading and hindu_dharmashastra_reading — is it the endogamy requirement alone, or the combination of endogamy with priestly ritual-validity gatekeeping?',
    'Structural comparison across the readings: does either sibling reading condition religious marriage validity on community-of-origin in a similarly strict, trust-access-forfeiting way? If not, the combination (not endogamy alone) is the distinguishing structural element.',
    'Clarifies which structural element should be treated as the delta when comparing readings for network/family purposes; affects how reading_relations edges should be interpreted downstream.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Precise location of this reading''s distinguishing structural claim relative to siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(fami_tr_t60, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement(fami_tr_t80, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 80, 0.28).
narrative_ontology:measurement(fami_tr_t100, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fami_be_t20, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(fami_be_t40, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(fami_be_t60, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(fami_be_t80, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(fami_be_t100, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fami_su_t20, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(fami_su_t40, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(fami_su_t60, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(fami_su_t80, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement(fami_su_t100, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__parsi_zoroastrian_reading, 0.1).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five sibling readings of the family_law_authority kernel, decomposed per the ε-invariance principle: each religious/legal tradition's marriage-governance claim is structurally distinct (different authority source, different membership consequences, different victim sets) and therefore carries its own ε rather than being one constraint measured five ways. The parsi_zoroastrian_reading is distinguished from all siblings by its explicit endogamy-with-status-forfeiture mechanism tied to small-population survival logic, a feature none of the other four readings share in the same form.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
