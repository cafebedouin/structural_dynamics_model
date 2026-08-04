% ============================================================================
% CONSTRAINT STORY: dignity_kernel__imago_dei_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignity_kernel__imago_dei_reading, []).

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
 *   constraint_id: dignity_kernel__imago_dei_reading
 *   human_readable: Imago Dei Reading of the Dignity Kernel (AI Subordination & Anti-Enhancement Doctrine)
 *   domain: theological_ethics/technology_governance/philosophical_anthropology
 *
 * SUMMARY:
 *   This story instantiates the imago Dei reading of the contested dignity
 *   kernel: dignity is the inviolable image of the Triune God, equal in all
 *   persons prior to any demonstrated capability. This reading grounds a real
 *   coordination function — an unconditional floor protecting the disabled,
 *   dying, and cognitively diminished from utilitarian instrumentalization —
 *   but as institutionalized into technology governance it also functions to
 *   categorically bar AI personhood consideration and cognitive/biological
 *   enhancement, producing an identifiable victim class among researchers,
 *   patients, and disabled persons who would benefit from capability-altering
 *   technologies. The autonomy_rights_reading and posthumanist_reading are
 *   separate constraints (separate files) instantiating the same kernel
 *   differently; this file does not adjudicate between them, per Rule 1.
 *
 * KEY AGENTS:
 *   - magisterial_authorities: Primary agenda-setter (institutional/arbitrage) — articulates and enforces the doctrine
 *   - vulnerable_persons_protected_from_instrumentalization: Primary beneficiary (powerless/trapped) — genuinely protected by the capability-independent floor
 *   - cognitively_disabled_persons_denied_enhancement_access: Primary payer (powerless/trapped) — denied enhancement access the doctrine forecloses
 *   - researchers_in_enhancement_and_ai_personhood_fields: Secondary payer (moderate/constrained) — faces funding and licensing obstruction
 *   - philosophical_ethicists_observing_the_kernel_contest: Analytical observer — tracks the reading's competition with sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignity_kernel__imago_dei_reading, 0.28).
domain_priors:suppression_score(dignity_kernel__imago_dei_reading, 0.42).
domain_priors:theater_ratio(dignity_kernel__imago_dei_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignity_kernel__imago_dei_reading, tangled_rope).
narrative_ontology:human_readable(dignity_kernel__imago_dei_reading, "Imago Dei Reading of the Dignity Kernel (AI Subordination & Anti-Enhancement Doctrine)").
narrative_ontology:topic_domain(dignity_kernel__imago_dei_reading, "theological_ethics/technology_governance/philosophical_anthropology").

domain_priors:requires_active_enforcement(dignity_kernel__imago_dei_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignity_kernel__imago_dei_reading, '2f813204-8952-44e3-af98-91fc1cba03d3').
narrative_ontology:cs_kernel_codification('2f813204-8952-44e3-af98-91fc1cba03d3', fixed_text).
narrative_ontology:cs_authority_grounding('2f813204-8952-44e3-af98-91fc1cba03d3', lineage).
narrative_ontology:cs_interpretation_layer_present('2f813204-8952-44e3-af98-91fc1cba03d3').
narrative_ontology:cs_reading_relation('2f813204-8952-44e3-af98-91fc1cba03d3', dignity_kernel__autonomy_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f813204-8952-44e3-af98-91fc1cba03d3', dignity_kernel__posthumanist_reading, forecloses).
narrative_ontology:cs_axiom('2f813204-8952-44e3-af98-91fc1cba03d3', foundational, dignity_grounded_in_divine_image_not_capability).
narrative_ontology:cs_axiom_status(dignity_grounded_in_divine_image_not_capability, holdable).
narrative_ontology:cs_axiom_grounding('2f813204-8952-44e3-af98-91fc1cba03d3', dignity_grounded_in_divine_image_not_capability, theological).
narrative_ontology:cs_axiom('2f813204-8952-44e3-af98-91fc1cba03d3', foundational, human_nature_fixed_by_created_order).
narrative_ontology:cs_axiom_status(human_nature_fixed_by_created_order, holdable).
narrative_ontology:cs_axiom_grounding('2f813204-8952-44e3-af98-91fc1cba03d3', human_nature_fixed_by_created_order, theological).
narrative_ontology:cs_reference_frame('2f813204-8952-44e3-af98-91fc1cba03d3', patristic_conciliar_anthropology).
narrative_ontology:cs_drift_state('2f813204-8952-44e3-af98-91fc1cba03d3', contemporary_biotechnology_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f813204-8952-44e3-af98-91fc1cba03d3', '').
narrative_ontology:cs_kernel_id(dignity_kernel__imago_dei_reading, dignity_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, magisterial_authorities).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, traditional_bioethics_institutions).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, vulnerable_persons_protected_from_instrumentalization).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, cognitively_disabled_persons_denied_enhancement_access).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, terminally_ill_patients_seeking_experimental_augmentation).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, researchers_in_enhancement_and_ai_personhood_fields).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, persons_subjected_to_technocratic_reduction).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, persons_subjected_to_technocratic_reduction).
narrative_ontology:constraint_vindicates(dignity_kernel__imago_dei_reading, equal_dignity_prior_to_capability).
narrative_ontology:constraint_vindicates(dignity_kernel__imago_dei_reading, created_order_doctrine).
narrative_ontology:constraint_vindicates(dignity_kernel__imago_dei_reading, human_person_as_non_instrumentalizable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Church bodies, theological ethics commissions, and allied bioethics councils articulate and enforce the doctrine that dignity is grounded in the imago Dei and is equal in all persons prior to capability. They issue guidance restricting AI personhood claims, cognitive enhancement, and germline modification, and their institutional standing is substantially built on being the authoritative interpreters of this claim.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, magisterial_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Hospital ethics boards, licensing bodies, and review panels that draw legitimacy and funding from applying imago-Dei-grounded dignity criteria to gatekeep enhancement technologies and AI deployment. They benefit from the doctrine's stability as an adjudicating standard even when it forecloses options patients or researchers want.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, traditional_bioethics_institutions, beneficiary,
    institutional, generational, constrained, national).

% Persons at risk of being reduced to data points, labor inputs, or subjects of non-consensual technological experimentation (severely disabled individuals, the very old, the unborn, the comatose) are shielded by a doctrine that insists their dignity does not depend on demonstrated capability. For this group the constraint functions as genuine protection against instrumentalization, not extraction.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, vulnerable_persons_protected_from_instrumentalization, beneficiary,
    powerless, biographical, trapped, local).

% Individuals who might benefit from cognitive enhancement technologies to gain function they currently lack are denied access because the doctrine treats any alteration of the given cognitive endowment as a violation of created order. Their exit options are essentially nil — they cannot access enhancement without leaving jurisdictions or communities that enforce the doctrine, if such exit is even physically possible.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, cognitively_disabled_persons_denied_enhancement_access, payer,
    powerless, biographical, trapped, local).

% Patients facing death from degenerative or terminal conditions who might extend or improve life through neural interfaces, life-extension therapies, or AI-augmented care are barred where institutions enforcing the doctrine classify such measures as illegitimate transgressions of the created human form. Their remaining option is medical tourism to jurisdictions with different governing readings, available only to those with resources.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, terminally_ill_patients_seeking_experimental_augmentation, payer,
    powerless, immediate, constrained, national).

% Scientists and technologists pursuing cognitive enhancement, life extension, or frameworks for AI moral status face funding denial, licensing obstruction, and reputational sanction from institutions applying the doctrine. They can relocate research programs to more permissive jurisdictions, but at high biographical and career cost.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, researchers_in_enhancement_and_ai_personhood_fields, payer,
    moderate, biographical, constrained, global).

% Ordinary persons whose labor, health, and cognition are increasingly mediated by AI systems and metrics. The doctrine simultaneously protects them (by insisting they cannot be reduced to their measurable output) and constrains them (by foreclosing enhancement options that might improve their position relative to systems already reshaping their lives), producing a genuinely mixed position.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, persons_subjected_to_technocratic_reduction, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__imago_dei_reading, persons_subjected_to_technocratic_reduction, beneficiary).

% Developers building increasingly capable AI systems, and any future claim of AI moral status, are categorically excluded from the dignity the doctrine confers — AI is fixed as tool, never candidate for personhood, regardless of demonstrated capability. Their perspective (that capability-based criteria might warrant reconsideration) has no standing within this reading's framework by design.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, ai_systems_and_developers, excluded,
    organized, generational, constrained, global).

% Scholars tracking how the imago Dei reading, autonomy/rights reading, and posthumanist reading compete for governance authority over emerging biotechnology and AI law, without institutional stake in any single reading's victory.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, philosophical_ethicists_observing_the_kernel_contest, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignity_kernel__imago_dei_reading, diffuse).
narrative_ontology:fixing_cost_class(dignity_kernel__imago_dei_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, capability-independent floor for human worth that protects the severely disabled, the dying, the unborn, and the cognitively diminished from being valued only for demonstrated function — solving the real problem of instrumentalization and utilitarian triage that pure capability-based dignity frameworks are vulnerable to.
% TRANSFER_FUNCTION: Moves adjudicating authority over biotechnology, AI governance, and enhancement research toward magisterial and allied bioethics institutions, and moves access to enhancement, AI-personhood consideration, and experimental augmentation away from researchers, terminally ill patients, and disabled persons who might benefit from capability-altering technologies.
% ABSENT_VOICES: Cognitively disabled persons who might personally prefer enhancement access are rarely consulted as a constituency distinct from their institutional guardians; AI systems (and any future claim to AI moral status) have no voice in a framework that forecloses their candidacy by definition; posthumanist and autonomy-rights ethicists are present in academic discourse but largely absent from the ecclesial and clinical bodies that operationalize this reading.
% DISAPPEARANCE_RATIONALE: If the imago Dei reading disappeared overnight, protective effects for the profoundly disabled and non-autonomous would need to be re-derived from another source (autonomy-based frameworks struggle to ground the dignity of those who lack or have never had rational agency), while enhancement research, AI personhood debate, and experimental augmentation access would likely accelerate substantially. Beneficiary institutions dispute that anything essential would be lost; payer groups dispute that anything essential would remain protected by a purely capability-based alternative — hence contested rather than a clean rearrangement.
% FOUNDING_PROBLEM: How to ground human worth against ancient and recurring practices (infanticide, slavery, discarding the incapacitated) that value persons only by demonstrated utility or capability — the doctrine was built to secure an unconditional floor beneath which no human being's worth could fall regardless of function.
% FOUNDING_PROBLEM_CORROBORATION: Disability rights advocates and palliative care ethicists outside the magisterial institutions corroborate that the capability-independent floor still does protective work against utilitarian triage in clinical settings. Bioethicists and technology researchers outside the beneficiary institutions attest that the same doctrine, applied to enhancement and AI governance, now functions primarily to preserve institutional interpretive authority over emerging technology rather than to solve any active instrumentalization threat in those domains — the founding problem is live in one application and largely resolved-or-displaced in the other.
narrative_ontology:disappearance_verdict(dignity_kernel__imago_dei_reading, contested).
narrative_ontology:founding_problem_status(dignity_kernel__imago_dei_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignity_kernel__imago_dei_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignity_kernel__imago_dei_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dignity_kernel__imago_dei_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignity_kernel__imago_dei_reading_tests).
:- end_tests(dignity_kernel__imago_dei_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.28 at interval end) because the doctrine's primary operation in clinical protection contexts is genuinely coordinative and low-cost; the extraction accrues specifically in the technology-governance extension, where the same doctrine is used to foreclose enhancement and AI-personhood inquiry without a correspondingly active instrumentalization threat in those domains. Suppression is moderate (0.42) and rising over the interval as enhancement and AI-personhood research matures and institutional resistance to it correspondingly hardens — the suppression_requirement trajectory tracks the tightening of licensing and funding gatekeeping as the technologies become more feasible and threaten the doctrine's foreclosure claim more directly. Theater ratio is low (0.2), reflecting that most enforcement is substantive doctrinal application rather than empty performance.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the doctrine is coherent and singular: dignity is dignity regardless of application domain. From the payer seats in the enhancement/AI domain, the doctrine looks like an institutional interpretive monopoly extended opportunistically beyond its original protective scope. The engine computes these as different seat-level classifications from the same structural data; the divergence is not an error in the story.
 *
 * DIRECTIONALITY LOGIC:
 *   Magisterial authorities and traditional bioethics institutions sit near the beneficiary end: they administer the standard and derive institutional legitimacy from being its interpreters. Vulnerable persons protected from instrumentalization also sit toward the beneficiary end, but through a different mechanism — they receive a real protective good, not institutional rent. Cognitively disabled persons denied enhancement, terminally ill patients, and researchers sit toward the target end: the same doctrine that protects one group's baseline worth forecloses another group's access to capability-altering options, with limited or no exit. Persons subjected to technocratic reduction occupy a genuinely mixed position — protected in one respect, constrained in another — which is why they carry both payer and beneficiary roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (grounding worth against purely capability-based triage) remains live in clinical and disability-rights contexts — corroborated by advocates outside the beneficiary institutions — which is why this reading is not simply a snare. But its extension into AI governance and enhancement policy shows the founding problem substantially displaced: the doctrine there functions less to prevent an active instrumentalization threat and more to preserve magisterial interpretive authority over a domain the tradition did not originally address. This bifurcation is exactly why the story is authored as tangled_rope rather than either rope (would ignore the enhancement-domain extraction) or snare (would ignore the genuine clinical protective function) — the coordination and extraction functions are structurally distinct halves of the same enforced standard.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clinical_protection_vs_governance_extension_separability,
    'Is the imago Dei reading''s protective function in clinical/disability contexts structurally separable from its foreclosure function in AI-governance and enhancement contexts, or are they the same underlying commitment applied consistently?',
    'Compare doctrinal reasoning across the two domains: if the same theological argument (equal dignity prior to capability) generates both the protective conclusion and the foreclosure conclusion through the same inferential steps, they are one commitment; if the foreclosure conclusion requires additional premises not present in the protective argument, the domains are separable and the extraction is a distinct addition.',
    'If separable, the extraction in the enhancement/AI domain is an opportunistic extension riding on the legitimacy of the clinical protective function, strengthening the tangled_rope classification. If inseparable, the doctrine is a single coherent commitment and the apparent extraction is simply the doctrine''s consistent application, which would push the classification toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clinical_protection_vs_governance_extension_separability, conceptual, 'Whether clinical protection and technology foreclosure are one commitment or two.').

omega_variable(
    natural_law_vs_constructed_institutional_claim,
    'Is the imago Dei grounding of dignity a claim about the actual constitution of reality (a theological mountain, true independent of any institution''s assertion of it) or a constructed doctrinal claim whose persistence depends on the institutions that benefit from being its authoritative interpreters?',
    'Examine whether the doctrine''s protective effects would persist if magisterial interpretive authority were removed and the claim were adjudicated by an unaffiliated ethics body — persistence without the beneficiary institutions would support the natural-law reading; dependence on those institutions'' continued authority would support the constructed reading.',
    'If natural-law, the tangled_rope classification is itself contestable at the deepest level — the doctrine may be closer to a mountain whose institutional stewards happen to also benefit, rather than a constructed extraction mechanism. If constructed, the institutional beneficiary structure is doing more work than the theological claim itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_institutional_claim, preference, 'Whether the imago Dei claim is discovered or institutionally constructed.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that dignity_kernel supports at least three coherent readings (imago_dei, autonomy_rights, posthumanist) that produce structurally different victim sets and different verdicts on AI personhood and enhancement, what determines which reading a given governance body adopts, and is that selection itself contestable on grounds internal to any one reading?',
    'Track which reading is operative in specific governance bodies (national bioethics commissions, AI regulatory frameworks) and whether their selection is argued on theological, legal, or purely political grounds.',
    'If reading-selection is arbitrary or power-determined rather than argued, all three readings'' claims to being the ''correct'' account of dignity are weakened equally, and the kernel itself (rather than any single reading) becomes the object of contest that governance frameworks must resolve procedurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'How governance bodies select among the three dignity_kernel readings and whether that selection is principled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignity_kernel__imago_dei_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignity_kernel__imago_dei_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dign_tr_t8, dignity_kernel__imago_dei_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(dign_tr_t16, dignity_kernel__imago_dei_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(dign_tr_t24, dignity_kernel__imago_dei_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(dign_tr_t32, dignity_kernel__imago_dei_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(dign_tr_t40, dignity_kernel__imago_dei_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignity_kernel__imago_dei_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(dign_be_t8, dignity_kernel__imago_dei_reading, base_extractiveness, 8, 0.16).
narrative_ontology:measurement(dign_be_t16, dignity_kernel__imago_dei_reading, base_extractiveness, 16, 0.19).
narrative_ontology:measurement(dign_be_t24, dignity_kernel__imago_dei_reading, base_extractiveness, 24, 0.22).
narrative_ontology:measurement(dign_be_t32, dignity_kernel__imago_dei_reading, base_extractiveness, 32, 0.25).
narrative_ontology:measurement(dign_be_t40, dignity_kernel__imago_dei_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignity_kernel__imago_dei_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(dign_su_t8, dignity_kernel__imago_dei_reading, suppression_requirement, 8, 0.29).
narrative_ontology:measurement(dign_su_t16, dignity_kernel__imago_dei_reading, suppression_requirement, 16, 0.33).
narrative_ontology:measurement(dign_su_t24, dignity_kernel__imago_dei_reading, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(dign_su_t32, dignity_kernel__imago_dei_reading, suppression_requirement, 32, 0.39).
narrative_ontology:measurement(dign_su_t40, dignity_kernel__imago_dei_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignity_kernel__imago_dei_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(dignity_kernel__imago_dei_reading, 0.1).
narrative_ontology:affects_constraint(dignity_kernel__imago_dei_reading, autonomy_rights_reading).
narrative_ontology:affects_constraint(dignity_kernel__imago_dei_reading, posthumanist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of dignity_kernel. autonomy_rights_reading grounds dignity in rationality/autonomy/rights rather than divine image, producing a different (narrower) protective floor for non-rational persons but a more permissive stance toward AI personhood claims grounded in demonstrated rational capacity. posthumanist_reading rejects a fixed human nature altogether, treating enhancement and superintelligence as continuous with flourishing, and has no categorical bar on AI moral status. Each reading is authored as a separate constraint story with its own ε, victim set, and classification per the ε-invariance principle; this file's imago_dei_reading forecloses posthumanist_reading's core premise (see cs_structure.reading_relations) while coexisting with autonomy_rights_reading in ongoing public and legal discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
