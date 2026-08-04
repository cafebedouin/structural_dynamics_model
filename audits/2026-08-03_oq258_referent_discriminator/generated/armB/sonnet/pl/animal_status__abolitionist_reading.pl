% ============================================================================
% CONSTRAINT STORY: animal_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__abolitionist_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: animal_status__abolitionist_reading
 *   human_readable: Animal Rights: Abolitionist Reading (Inherent Value Precludes Instrumental Use)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This story instantiates the abolitionist reading of the animal_status
 *   kernel: a categorical claim that animals hold rights grounded in inherent
 *   value that no degree of welfare improvement can satisfy, because the
 *   wrong is located in instrumental use as such. This is deliberately NOT a
 *   story about whether animal welfare regulation is adequate (that is the
 *   welfare_reading, a sibling constraint) nor about whether animals are
 *   property with no independent standing (the property_reading, another
 *   sibling). Those are different constraints with different ε values,
 *   different victim sets, and different beneficiary structures — they are
 *   linked here via network only, never merged into this story's
 *   classification. Under this reading, extractiveness is measured as maximal
 *   for every category of instrumental use (farming, research, entertainment,
 *   labor) because the reading treats zero instrumental use as the only
 *   non-extractive baseline; any use at all registers as extraction from the
 *   animal's inherent-value standing, not merely from its welfare interests.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__abolitionist_reading, 0.91).
domain_priors:suppression_score(animal_status__abolitionist_reading, 0.72).
domain_priors:theater_ratio(animal_status__abolitionist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, extractiveness, 0.91).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(animal_status__abolitionist_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__abolitionist_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__abolitionist_reading, "Animal Rights: Abolitionist Reading (Inherent Value Precludes Instrumental Use)").
narrative_ontology:topic_domain(animal_status__abolitionist_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__abolitionist_reading, '009b972d-21b5-425e-a97f-c6175f396094').
narrative_ontology:cs_kernel_codification('009b972d-21b5-425e-a97f-c6175f396094', distributed).
narrative_ontology:cs_authority_grounding('009b972d-21b5-425e-a97f-c6175f396094', distributed).
narrative_ontology:cs_reading_relation('009b972d-21b5-425e-a97f-c6175f396094', animal_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('009b972d-21b5-425e-a97f-c6175f396094', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('009b972d-21b5-425e-a97f-c6175f396094', foundational, inherent_value_precludes_instrumentalization).
narrative_ontology:cs_axiom_status(inherent_value_precludes_instrumentalization, holdable).
narrative_ontology:cs_axiom_grounding('009b972d-21b5-425e-a97f-c6175f396094', inherent_value_precludes_instrumentalization, deontological).
narrative_ontology:cs_axiom('009b972d-21b5-425e-a97f-c6175f396094', secondary, welfare_reform_constitutes_legitimation_not_progress).
narrative_ontology:cs_axiom_status(welfare_reform_constitutes_legitimation_not_progress, holdable).
narrative_ontology:cs_axiom_grounding('009b972d-21b5-425e-a97f-c6175f396094', welfare_reform_constitutes_legitimation_not_progress, instrumental).
narrative_ontology:cs_reference_frame('009b972d-21b5-425e-a97f-c6175f396094', property_based_animal_law_tradition).
narrative_ontology:cs_drift_state('009b972d-21b5-425e-a97f-c6175f396094', contemporary_rights_litigation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('009b972d-21b5-425e-a97f-c6175f396094', '').
narrative_ontology:cs_kernel_id(animal_status__abolitionist_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, animal_rights_advocacy_organizations).
narrative_ontology:constraint_beneficiary(animal_status__abolitionist_reading, plant_based_industry_actors).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, livestock_dependent_communities).
narrative_ontology:constraint_victim(animal_status__abolitionist_reading, biomedical_research_institutions).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, sentience_grounds_moral_status).
narrative_ontology:constraint_vindicates(animal_status__abolitionist_reading, instrumentalization_is_categorically_wrong).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under the current property/welfare regime this reading contests, farmed animals are bred, confined, and killed as a matter of course. The abolitionist reading names them as the direct victims of any instrumental use whatsoever — not merely of cruel treatment within use, but of use itself. They have no voice in the arrangement and cannot exit it; their situation only changes if the reading's prohibition is adopted and enforced.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, farmed_animals, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(animal_status__abolitionist_reading, farmed_animals).

% Used in biomedical and product-safety research under current regulatory frameworks. Under this reading, any experimental use is a rights violation regardless of the welfare protocols governing it, since the wrong is located in instrumentalization itself, not in the manner of treatment.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, laboratory_animals, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_non_agent(animal_status__abolitionist_reading, laboratory_animals).

% Sets and litigates the abolitionist position: pursues legal personhood claims, drafts model legislation banning categories of use, and organizes public campaigns arguing that welfare reform is a legitimating half-measure that should be rejected rather than pursued. Gains institutional standing, funding, and moral authority from advancing a bright-line prohibition rather than incremental regulation.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, animal_rights_advocacy_organizations, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status__abolitionist_reading, animal_rights_advocacy_organizations, beneficiary).

% Commercial producers of animal-product substitutes benefit materially from a moral and legal environment that treats any instrumental use of animals as impermissible, since this widens the addressable market and legitimizes public subsidy or preference for their products. They are not the moral drivers of the reading but capture a downstream commercial benefit from its adoption.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, plant_based_industry_actors, beneficiary,
    organized, biographical, mobile, global).

% Ranchers, pastoralists, and rural economies whose livelihoods depend on animal agriculture. Under this reading their entire economic basis is not merely regulated but morally illegitimate in its totality — there is no welfare-compliant version of their work that satisfies the prohibition. Exit means abandoning generational occupations and land use with limited retraining or transition support offered by advocates of the reading.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, livestock_dependent_communities, payer,
    moderate, biographical, constrained, regional).

% Universities, pharmaceutical companies, and regulatory-testing bodies whose methodologies rely on animal models. This reading treats their entire research paradigm as rights-violating, not merely as requiring stronger welfare safeguards, threatening funding models, regulatory approval pathways, and decades of institutional investment in animal-model science.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, biomedical_research_institutions, payer,
    institutional, generational, constrained, global).

% Organizations pursuing incremental improvements — cage-free standards, humane slaughter requirements, reduced testing — are structurally excluded from legitimacy within this reading, which characterizes their work as complicity that entrenches instrumental use by making it more palatable. They would object that incremental gains reduce actual suffering now, but this reading forecloses that framing as a category error.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, welfare_reform_advocates, excluded,
    organized, biographical, constrained, global).

% Adjudicate personhood and standing claims brought under this reading's framework — habeas corpus petitions for chimpanzees, rights-based challenges to confinement statutes. They evaluate the doctrinal coherence of extending rights-bearer status to non-human animals without being parties who gain or lose from the outcome.
narrative_ontology:constraint_stakeholder(animal_status__abolitionist_reading, legal_scholars_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__abolitionist_reading, diffuse).
narrative_ontology:fixing_cost_class(animal_status__abolitionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a moral and legal movement around a single bright-line principle — inherent value precludes instrumental use — allowing diverse advocacy efforts (litigation, legislation, consumer campaigns) to converge on a shared, non-negotiable standard rather than fragmenting across incremental welfare metrics.
% TRANSFER_FUNCTION: If adopted, the reading transfers economic activity, land use, and institutional practice away from animal agriculture, biomedical animal research, and related industries toward substitute industries and toward the standing/legitimacy of rights-based advocacy organizations; the moral costs of continued use are reassigned entirely onto the humans who use animals, with no residual instrumental use treated as permissible.
% ABSENT_VOICES: Livestock-dependent communities and biomedical researchers are treated as morally compromised parties within the reading's own framework rather than as stakeholders to be negotiated with; welfare reform advocates are present in public discourse but are excluded from legitimacy within this reading's internal logic, which characterizes their compromises as obstacles rather than progress.
% DISAPPEARANCE_RATIONALE: If the abolitionist reading vanished as a live position tomorrow, the property and welfare readings would continue to govern actual practice largely unchanged in the near term — animal agriculture and research would proceed under existing regulatory regimes. But its disappearance would remove the doctrinal pressure that currently shapes welfare-reform ceilings, legal personhood litigation, and long-horizon investment decisions in cultivated-protein alternatives; whether the world 'rearranges' depends on whether one credits the reading's causal influence on institutions that have not yet adopted it, which is itself contested between advocates and opponents.
% FOUNDING_PROBLEM: The historical treatment of animals as property permitted, without moral remainder, forms of confinement, breeding, and killing whose scale and severity welfare regulation was argued to leave fundamentally intact — the abolitionist reading was built to name and prohibit the use itself, not merely its manner.
% FOUNDING_PROBLEM_CORROBORATION: Animal law scholars outside the advocacy movement (e.g., in comparative jurisprudence on legal personhood) corroborate that the property-based treatment of sentient beings remains largely intact in most jurisdictions despite welfare reforms, supporting the claim that the founding problem persists. However, welfare economists and agricultural policy researchers — also outside the advocacy movement — dispute whether prohibition rather than reform is the tractable remedy, so corroboration exists for the problem's persistence but not for the reading's proposed solution.
narrative_ontology:disappearance_verdict(animal_status__abolitionist_reading, contested).
narrative_ontology:founding_problem_status(animal_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__abolitionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__abolitionist_reading, 0.91, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.91 because under this reading's own premises, essentially the entire current human-animal economic relationship constitutes extraction — there is no welfare-compliant residue that escapes the prohibition. Suppression is authored at 0.72, reflecting that the reading's adoption would require active legal and economic suppression of currently legal industries (livestock, biomedical testing) to hold; it is not self-enforcing. Accessibility_collapse is authored lower (0.35) because, unlike a mountain, alternatives to full prohibition (welfare reform, property-based regulation) remain widely practiced and institutionally entrenched — the abolitionist position has not collapsed its rivals, it contests them. Resistance is authored high (0.88) because livestock communities, biomedical institutions, and welfare-reform advocates all actively contest the reading's premises and its practical implications.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting advocacy seat, the reading appears as principled coordination around a correct moral claim. From the livestock-community and biomedical-institution payer seats, the same reading appears as an absolutist demand that offers no negotiated path and characterizes their entire livelihood as illegitimate. The engine computes these as structurally different experiences of the same declared structure; the claim (tangled_rope) does not resolve which seat's experience is 'correct' — it names that both are structurally coherent outputs of the same data.
 *
 * DIRECTIONALITY LOGIC:
 *   Farmed and laboratory animals are named as full victims with trapped exit — under this reading there is no exit available to them from instrumental use short of the reading's adoption and enforcement, and as non-agents they cannot advocate for themselves. Advocacy organizations and plant-based industry actors are beneficiaries: the former gain moral and institutional standing from advancing a bright-line claim, the latter gain material market advantage. Livestock-dependent communities and biomedical institutions are victims of a different kind — not physically confined, but facing wholesale delegitimation of their economic and professional existence with constrained exit given sunk investment and lack of transition support within the reading's own framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than snare) reflects that the reading does perform a genuine coordination function — it unifies a fragmented advocacy movement around a single doctrinal standard, enabling coordinated litigation and legislative strategy that would otherwise dissipate across incompatible welfare benchmarks. But it also names concrete victims (farmed/laboratory animals under the status quo, and displaced human industries under a prohibition regime) and requires active enforcement to hold against contestation from property and welfare readings. This prevents mislabeling the reading as pure extraction (it does solve a real coordination problem for the movement) while also refusing to launder it as a costless rope — the reading's structural rejection of welfare reform as 'legitimation' is itself a move that forecloses incremental victim relief in favor of doctrinal purity, which is exactly the kind of asymmetric cost the tangled_rope classification is built to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sentience_threshold_for_rights_bearing,
    'Does inherent value sufficient to ground rights against instrumental use track sentience, sapience, or some other criterion, and where does that criterion draw the line across the animal kingdom?',
    'Convergence in comparative cognition and neuroscience research on markers of morally relevant sentience, combined with philosophical argument about which markers are sufficient rather than merely necessary for rights-bearing status.',
    'A narrow criterion (e.g., higher-order consciousness) would shrink the victim set to a subset of currently used animals; a broad criterion (any nociception) would expand it to include most or all currently farmed and researched species, changing which use categories register as extractive under this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sentience_threshold_for_rights_bearing, conceptual, 'Unresolved boundary of which animals the abolitionist reading''s rights claim actually covers.').

omega_variable(
    welfare_reform_as_legitimation_or_harm_reduction,
    'Does pursuing welfare reform under the current property/welfare regime causally entrench instrumental use (legitimation effect) or causally reduce net suffering while prohibition remains politically unattainable (harm-reduction effect)?',
    'Longitudinal comparative study of jurisdictions that pursued welfare reform versus prohibition-focused advocacy, tracking downstream consumption patterns, industry investment, and public attitude shifts over multi-decade windows.',
    'If welfare reform demonstrably entrenches use, the abolitionist reading''s rejection of incrementalism is vindicated as strategically sound rather than merely doctrinally pure. If welfare reform demonstrably reduces suffering without entrenching the underlying practice, the reading''s categorical rejection of it becomes harder to justify even on its own terms, strengthening the sibling welfare_reading''s practical claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_reform_as_legitimation_or_harm_reduction, empirical, 'Whether rejecting welfare reform as legitimation is strategically correct or self-defeating.').

omega_variable(
    committer_framing_alternative_kernel_location,
    'Is the kernel more accurately located at ''moral status of animals'' (as framed here, generating three readings by degree of protection) or at ''the legitimacy of any human-animal hierarchy claim'' (a broader framing that would fold in questions of ecological interdependence and non-Western cosmologies of animal personhood not captured by the rights-based framework at all)?',
    'Comparative analysis of whether non-rights-based frameworks (e.g., relational or ecological accounts of animal standing found in some Indigenous legal traditions) produce classifications this three-reading kernel structure cannot represent, which would indicate the kernel itself is under-specified rather than merely contested among three readings.',
    'If a fourth framing exists that is not reducible to a point on the property-welfare-abolition spectrum, this kernel decomposition is incomplete and a fourth sibling constraint should be authored rather than treating the current three readings as exhaustive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_alternative_kernel_location, conceptual, 'Whether the three-reading kernel structure (property/welfare/abolition) exhausts the live framings of animal moral status or omits a structurally distinct alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__abolitionist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__abolitionist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(anim_tr_t8, animal_status__abolitionist_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(anim_tr_t16, animal_status__abolitionist_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(anim_tr_t24, animal_status__abolitionist_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(anim_tr_t32, animal_status__abolitionist_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(anim_tr_t40, animal_status__abolitionist_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__abolitionist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(anim_be_t8, animal_status__abolitionist_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(anim_be_t16, animal_status__abolitionist_reading, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(anim_be_t24, animal_status__abolitionist_reading, base_extractiveness, 24, 0.85).
narrative_ontology:measurement(anim_be_t32, animal_status__abolitionist_reading, base_extractiveness, 32, 0.89).
narrative_ontology:measurement(anim_be_t40, animal_status__abolitionist_reading, base_extractiveness, 40, 0.91).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__abolitionist_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(anim_su_t8, animal_status__abolitionist_reading, suppression_requirement, 8, 0.5).
narrative_ontology:measurement(anim_su_t16, animal_status__abolitionist_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(anim_su_t24, animal_status__abolitionist_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement(anim_su_t32, animal_status__abolitionist_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(anim_su_t40, animal_status__abolitionist_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__abolitionist_reading, identity_coordination).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__welfare_reading).
narrative_ontology:affects_constraint(animal_status__abolitionist_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the animal_status kernel. The property_reading treats animals as legal objects with no independent standing (ownership unrestricted except by welfare statute) — its core premise is directly foreclosed by this reading's core premise (inherent value grounding rights), since no single legal framework can simultaneously hold that animals both lack independent moral standing and possess rights-grounding inherent value. The welfare_reading treats animals as sentient beings whose interests constrain but do not prohibit use — this reading coexists with it in public discourse (different advocacy factions hold each as live positions) but structurally influences it by rejecting its incrementalist reforms as legitimation, creating downstream pressure on the welfare_reading's legitimacy claims without logically foreclosing them. Each reading carries its own ε, its own beneficiary/victim structure, and its own classification; they are not measurement variants of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
