% ============================================================================
% CONSTRAINT STORY: animal_moral_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: animal_moral_status__welfare_reading
 *   human_readable: Animal Welfare Regulation of Permissible Use
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This story instantiates the WELFARE reading of the contested
 *   animal-moral-status kernel: animals are sentient beings whose suffering
 *   matters morally, but use itself is permissible provided cruelty
 *   (gratuitously severe or unnecessary suffering) is prevented. This is
 *   structurally distinct from the property reading (no independent moral
 *   standing) and the abolitionist reading (use itself is the violation,
 *   regardless of method). Under this reading, animals enter the victim set
 *   only for the residual suffering that persists within 'humane' regulated
 *   use, not for use as such — the constraint governs methods, not the
 *   underlying use-relationship. The coordination function is real: it lets
 *   industries, regulators, welfare organizations, and consumers operate
 *   around a stable, administrable line rather than relitigating animal
 *   ontology at every transaction. The extraction is that this same
 *   coordination function stabilizes and legitimizes a scale of continued use
 *   whose aggregate suffering the framework was never designed to eliminate,
 *   only to bound.
 *
 * KEY AGENTS:
 *   - welfare_certification_organizations: agenda_setter/beneficiary (organized/arbitrage) — draws legitimacy and funding from occupying the welfare middle ground
 *   - regulated_animal_use_industries: beneficiary/payer (powerful/mobile) — buys legal and reputational cover at modest compliance cost
 *   - consuming_public: beneficiary (organized/mobile) — receives moral reassurance without confronting the underlying use question
 *   - animals_under_regulated_use: payer (powerless/trapped) — bears the suffering that remains legally permitted
 *   - abolitionist_advocates: excluded (moderate/constrained) — reject the use/cruelty distinction as incoherent, structurally shut out of standard-setting
 *   - regulatory_agencies: observer/agenda_setter (institutional/analytical) — writes and enforces the specific line between cruelty and permitted use
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__welfare_reading, 0.48).
domain_priors:suppression_score(animal_moral_status__welfare_reading, 0.42).
domain_priors:theater_ratio(animal_moral_status__welfare_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(animal_moral_status__welfare_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_moral_status__welfare_reading, "Animal Welfare Regulation of Permissible Use").
narrative_ontology:topic_domain(animal_moral_status__welfare_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__welfare_reading, '1afb9204-1a33-49e0-94a6-a83396f3ead7').
narrative_ontology:cs_kernel_codification('1afb9204-1a33-49e0-94a6-a83396f3ead7', distributed).
narrative_ontology:cs_authority_grounding('1afb9204-1a33-49e0-94a6-a83396f3ead7', practice).
narrative_ontology:cs_interpretation_layer_present('1afb9204-1a33-49e0-94a6-a83396f3ead7').
narrative_ontology:cs_reading_relation('1afb9204-1a33-49e0-94a6-a83396f3ead7', animal_moral_status__property_reading, influences).
narrative_ontology:cs_reading_relation('1afb9204-1a33-49e0-94a6-a83396f3ead7', animal_moral_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_axiom('1afb9204-1a33-49e0-94a6-a83396f3ead7', foundational, suffering_matters_but_use_is_permissible).
narrative_ontology:cs_axiom_status(suffering_matters_but_use_is_permissible, holdable).
narrative_ontology:cs_axiom_grounding('1afb9204-1a33-49e0-94a6-a83396f3ead7', suffering_matters_but_use_is_permissible, deontological).
narrative_ontology:cs_axiom('1afb9204-1a33-49e0-94a6-a83396f3ead7', foundational, cruelty_wrong_use_not_wrong).
narrative_ontology:cs_axiom_status(cruelty_wrong_use_not_wrong, holdable).
narrative_ontology:cs_axiom_grounding('1afb9204-1a33-49e0-94a6-a83396f3ead7', cruelty_wrong_use_not_wrong, conventional).
narrative_ontology:cs_reference_frame('1afb9204-1a33-49e0-94a6-a83396f3ead7', sentience_based_graduated_moral_status).
narrative_ontology:cs_drift_state('1afb9204-1a33-49e0-94a6-a83396f3ead7', contemporary_industrial_scale_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1afb9204-1a33-49e0-94a6-a83396f3ead7', '').
narrative_ontology:cs_kernel_id(animal_moral_status__welfare_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, welfare_certification_organizations).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, regulated_animal_use_industries).
narrative_ontology:constraint_beneficiary(animal_moral_status__welfare_reading, consuming_public).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, animals_under_regulated_use).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_moral_status__welfare_reading, regulated_animal_use_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft, audit, and certify humane-handling standards for industries that use animals. Draw funding, legitimacy, and institutional relevance from the existence of a middle position between cruelty and abolition. Their organizational survival depends on the welfare frame remaining the dominant public settlement rather than being displaced by either the property frame or the abolitionist frame.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, welfare_certification_organizations, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, welfare_certification_organizations, beneficiary).

% Pay modest compliance costs (approved slaughter methods, cage-size minimums, transport rules) in exchange for legal and reputational cover that stabilizes their right to continue using animals commercially. The welfare label lets them market products as humanely produced while retaining the underlying practice; where compliance costs rise, they lobby to soften standards rather than exit the practice.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulated_animal_use_industries, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, regulated_animal_use_industries, payer).

% Receives moral reassurance that consumption of animal products or services occurs under a regime that prohibits gratuitous cruelty. Rarely inspects whether certified practices meaningfully reduce suffering; the welfare frame lets consumption continue without the cognitive burden of confronting the abolitionist argument or the raw property argument directly.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, consuming_public, beneficiary,
    organized, biographical, mobile, national).

% Bear the suffering that remains legally permitted under 'humane' standards — confinement, separation from offspring, transport stress, methods of killing deemed acceptable rather than eliminated. Cannot exit, cannot be represented directly in the standard-setting process, and their welfare is defined and measured entirely by human proxies with their own interests in the outcome.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, animals_under_regulated_use, payer,
    powerless, immediate, trapped, national).

% Argue that welfare standards legitimize and entrench the very use-relationship that is the actual harm, and that 'humane use' is definitionally incoherent for a being with interests in its own life and liberty. Largely excluded from standard-setting bodies, which are dominated by welfare organizations and industry representatives; treated as fringe rather than as holders of a coherent competing reading.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, abolitionist_advocates, excluded,
    moderate, generational, constrained, national).

% Some producers and traditionalist interests view even minimal welfare regulation as an illegitimate constraint on ownership rights; they are marginalized in the public conversation by the welfare settlement's occupation of the 'reasonable middle,' though they retain influence in specific deregulatory pushes.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, property_frame_stakeholders, excluded,
    moderate, biographical, constrained, national).

% Write and enforce the specific welfare rules (stunning methods, space allowances, inspection regimes), adjudicating between industry lobbying and welfare-organization pressure. Their enforcement capacity and political will determine whether the standards are substantive or largely nominal.
narrative_ontology:constraint_stakeholder(animal_moral_status__welfare_reading, regulatory_agencies, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__welfare_reading, regulatory_agencies, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_moral_status__welfare_reading, regulated_animal_use_industries).
narrative_ontology:fixing_cost_class(animal_moral_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, administrable line between prohibited cruelty and permitted use, allowing industries, regulators, consumers, and moderate advocates to coordinate around a stable practical settlement rather than litigating the underlying moral status of animals in every transaction.
% TRANSFER_FUNCTION: Moves reputational and legal legitimacy to regulated industries and certifying organizations, and moves reassurance to consumers, in exchange for animals bearing whatever suffering remains within the certified methods — the transfer is legitimacy and continuity of practice, purchased at the animals' continued exposure to confinement, separation, and killing.
% ABSENT_VOICES: Animals themselves have no means of representation in standard-setting. Abolitionist advocates who reject the use/cruelty distinction as incoherent are structurally excluded from the certifying and regulatory bodies that write the rules they contest.
% DISAPPEARANCE_RATIONALE: If the welfare reading collapsed overnight, the settlement would fracture toward one of its siblings: either a property-frame retrenchment (no meaningful use restrictions) or an abolitionist reframing (use itself contested) — both of which would reorganize industries, certification bodies, and consumer practice substantially. The welfare frame is doing real coordinating work, not merely reporting a fact.
% FOUNDING_PROBLEM: Documented cases of gratuitous, uncontrolled cruelty in animal agriculture, transport, and research created public and political pressure for some legal floor on treatment, without unsettling the broader legal and economic structure of animal use.
% FOUNDING_PROBLEM_CORROBORATION: Welfare organizations and regulators attest the founding problem (gratuitous cruelty) remains live and that standards continue to reduce measurable suffering. Independent animal-behavior researchers and abolitionist scholars, from outside the certifying and regulated-industry seats, attest that the coordination has increasingly become a legitimacy mechanism for a scale of use whose aggregate suffering exceeds what the framework was originally built to address.
narrative_ontology:disappearance_verdict(animal_moral_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(animal_moral_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__welfare_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__welfare_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__welfare_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-to-moderate (0.48 at interval end) because the welfare reading's own metric of success — reduction of gratuitous cruelty relative to an unregulated baseline — is genuinely achieved in many certified contexts; this is not a pure extraction story. But extraction is non-trivial and rising because the certifying and regulatory apparatus increasingly serves a legitimation function for expanding scale of use (industrial confinement, high-throughput slaughter) that a 'suffering minimization' standard, honestly applied, would constrain further than it currently does. Theater ratio rises over the interval (0.22 to 0.40) reflecting increasing gap between certification labeling and audited practice as industries professionalize compliance-as-marketing. Suppression is moderate (0.42): animals cannot exit or contest the standard, but the suppression is diffuse rather than actively coercive — it operates through definitional control (what counts as 'necessary' suffering) rather than direct force against a resisting party.
 *
 * PERSPECTIVAL GAP:
 *   From the certifying organization and regulated-industry seats, this is coordination: a workable, enforceable floor against real cruelty that stabilizes an otherwise contested practice. From the animal seat — computed structurally, since animals cannot self-report — the same arrangement is extraction: legally permitted suffering continues, bounded only by what remains profitable and administrable to prohibit. The engine's per-seat computation should reflect this asymmetry without requiring either seat's account to be discounted.
 *
 * DIRECTIONALITY LOGIC:
 *   Welfare certification organizations and regulated industries sit near the beneficiary end: they collect legitimacy, market access, and reduced legal exposure, and can exit or renegotiate standards through lobbying (arbitrage/mobile exit). The consuming public benefits similarly through moral reassurance with low cost. Animals sit at the full-target end: trapped, powerless, immediate time horizon, and definitionally excluded from the negotiation that sets the boundary of their own permitted suffering — directionality here is maximal not because of an override but because the structural derivation from victim declaration plus trapped exit already produces it honestly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unchecked, unregulated cruelty — was real and the welfare reading substantially addressed it relative to a no-regulation baseline; classifying this as pure snare would erase that genuine coordination achievement. But classifying it as pure rope would erase the fact that the same standard-setting apparatus now does legitimation work for a scale and intensity of use that gratuitous-cruelty prevention alone does not justify. Tangled rope holds both: real coordination (a workable floor against clear cruelty, verified by outside animal-behavior science) and asymmetric extraction (the beneficiaries of the settlement are not the party bearing its residual costs), with active enforcement required to hold the line where it currently sits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reading_kernel_position,
    'This constraint instantiates the welfare reading of the animal_moral_status kernel (animals as sentient beings whose suffering matters but whose use is permissible). The property reading (animals as resources with no independent moral standing) and the abolitionist reading (use itself, not just cruelty, is the violation) are separate constraints, not measurement variants of this one. What would change if the sibling readings were adopted instead?',
    'Compare victim-set composition and ε across the three sibling stories: property_reading should show ε near zero for animals (no moral standing = no extraction registered against them structurally); abolitionist_reading should place ALL regulated use — not just cruelty-adjacent methods — in the extraction set, driving ε substantially higher than this reading''s 0.48.',
    'Confirms these are three distinct constraints sharing a kernel rather than one constraint measured three ways; each requires its own ε, beneficiaries, and victims per the ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(welfare_reading_kernel_position, conceptual, 'This story is one reading (welfare) of a three-reading kernel; sibling readings are separate constraint files.').

omega_variable(
    humane_use_coherence,
    'Is ''humane use'' a coherent, stable category, or does it collapse under scrutiny into either the property reading (use without a genuine welfare floor) or the abolitionist reading (any nontrivial confinement/killing is itself the violation)?',
    'Track whether welfare standards, over time and under industry lobbying pressure, drift toward minimal compliance thresholds (property-reading direction) versus tightening toward increasingly restrictive definitions of necessary suffering (abolitionist-adjacent direction). The rising theater_ratio and base_extractiveness trend in this story''s measurements is one data point suggesting drift toward the property-reading direction.',
    'If the category proves unstable and drifts systematically toward minimal compliance, that would support reclassifying this constraint''s trajectory from tangled_rope toward snare over time (a T17-style extraction accumulation signal); if it holds stable or tightens, it supports the tangled_rope classification as a genuine, durable middle position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humane_use_coherence, empirical, 'Whether the welfare/cruelty line is stable or drifts toward minimal-compliance capture over time.').

omega_variable(
    animal_representation_proxy_problem,
    'Animals cannot directly report their interests or contest the standards set on their behalf; all welfare metrics are human-authored proxies (behavioral indicators, physiological stress markers). How much does proxy-measurement error understate the actual extraction (χ) borne by animals under regulated use?',
    'Cross-check certified welfare outcomes against independent, non-industry-funded ethological research on stress and suffering indicators in certified vs. uncertified operations.',
    'If proxy measures systematically understate suffering (a documented risk in welfare science, e.g. under-detection of chronic stress vs. acute distress), the authored extractiveness of 0.48 is a floor, not a ceiling, on the true value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(animal_representation_proxy_problem, empirical, 'Proxy-measurement uncertainty in assessing animal suffering under certified welfare standards.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__welfare_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__welfare_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(anim_tr_t8, animal_moral_status__welfare_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(anim_tr_t16, animal_moral_status__welfare_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(anim_tr_t24, animal_moral_status__welfare_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(anim_tr_t32, animal_moral_status__welfare_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__welfare_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__welfare_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(anim_be_t8, animal_moral_status__welfare_reading, base_extractiveness, 8, 0.39).
narrative_ontology:measurement(anim_be_t16, animal_moral_status__welfare_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(anim_be_t24, animal_moral_status__welfare_reading, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(anim_be_t32, animal_moral_status__welfare_reading, base_extractiveness, 32, 0.47).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__welfare_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__welfare_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(anim_su_t8, animal_moral_status__welfare_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(anim_su_t16, animal_moral_status__welfare_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(anim_su_t24, animal_moral_status__welfare_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement(anim_su_t32, animal_moral_status__welfare_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__welfare_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__property_reading).
narrative_ontology:affects_constraint(animal_moral_status__welfare_reading, animal_moral_status__abolitionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language 'animal moral status' kernel per the ε-invariance principle. property_reading places animals wholly outside the victim set (no independent moral standing; low ε registered as extraction against animals specifically). abolitionist_reading places all regulated use, not merely cruelty, in the extraction set, driving ε substantially higher and reclassifying the same institutional apparatus as closer to pure snare from that reading's premises. welfare_reading (this story) occupies the contested middle: real coordination function (administrable floor against gratuitous cruelty) plus asymmetric extraction (residual permitted suffering borne entirely by animals, legitimation captured by industry and certifying bodies) — hence tangled_rope. All three share the same underlying institutional facts (slaughterhouses, farms, laboratories, certification bodies) but instantiate structurally distinct constraints because each reading's premises produce a different victim set and a different ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
