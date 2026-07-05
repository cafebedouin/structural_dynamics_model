% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Personhood Limited to Born Humans with Cognitive Capacity (Restrictive Anthropocentric Reading)
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint instantiates the restrictive anthropocentric reading of
 *   the legal personhood boundary kernel: legal personhood attaches only to
 *   born humans possessing cognitive capacity, excluding fetuses, ecosystems,
 *   and artificial systems from the class of rights-bearers regardless of
 *   their developmental trajectory or functional sophistication. This reading
 *   is one of three competing instantiations of the same underlying kernel
 *   (the personhood boundary); the developmental-potentiality reading and the
 *   functional-capacity reading are separate constraint stories with their
 *   own ε values, beneficiary/victim sets, and classifications — they are not
 *   alternate measurements of this constraint, they are structurally distinct
 *   constraints that happen to share a contested textual/doctrinal kernel.
 *
 * KEY AGENTS:
 *   - pregnant_persons: primary beneficiary (moderate/constrained) — retains bodily autonomy because fetus is not a competing rights-holder
 *   - constitutional_courts: agenda-setter (institutional/analytical) — administers and could redraw the boundary
 *   - extractive_industry_operators and ai_development_firms: secondary beneficiaries (powerful/arbitrage) — avoid personhood-based liability for ecological and AI harms
 *   - fetal_rights_advocates and environmental_personhood_movements: primary payers (organized-moderate/constrained) — foreclosed from asserting personhood claims on behalf of excluded classes
 *   - future_generations_affected_by_ecological_harm: diffuse payer (powerless/trapped) — bears long-horizon costs with no standing today
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.42).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.55).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Personhood Limited to Born Humans with Cognitive Capacity (Restrictive Anthropocentric Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, 'e5ecee04-20c2-4748-8ffe-24e644fcf3ec').
narrative_ontology:cs_kernel_codification('e5ecee04-20c2-4748-8ffe-24e644fcf3ec', distributed).
narrative_ontology:cs_authority_grounding('e5ecee04-20c2-4748-8ffe-24e644fcf3ec', lineage).
narrative_ontology:cs_interpretation_layer_present('e5ecee04-20c2-4748-8ffe-24e644fcf3ec').
narrative_ontology:cs_reading_relation('e5ecee04-20c2-4748-8ffe-24e644fcf3ec', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('e5ecee04-20c2-4748-8ffe-24e644fcf3ec', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('e5ecee04-20c2-4748-8ffe-24e644fcf3ec', foundational, birth_as_bright_line_for_legal_status).
narrative_ontology:cs_axiom_status(birth_as_bright_line_for_legal_status, holdable).
narrative_ontology:cs_axiom_grounding('e5ecee04-20c2-4748-8ffe-24e644fcf3ec', birth_as_bright_line_for_legal_status, conventional).
narrative_ontology:cs_axiom('e5ecee04-20c2-4748-8ffe-24e644fcf3ec', foundational, bodily_autonomy_precludes_competing_fetal_personhood).
narrative_ontology:cs_axiom_status(bodily_autonomy_precludes_competing_fetal_personhood, holdable).
narrative_ontology:cs_axiom_grounding('e5ecee04-20c2-4748-8ffe-24e644fcf3ec', bodily_autonomy_precludes_competing_fetal_personhood, deontological).
narrative_ontology:cs_reference_frame('e5ecee04-20c2-4748-8ffe-24e644fcf3ec', born_alive_common_law_threshold).
narrative_ontology:cs_drift_state('e5ecee04-20c2-4748-8ffe-24e644fcf3ec', contemporary_bioethics_and_ai_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e5ecee04-20c2-4748-8ffe-24e644fcf3ec', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_healthcare_providers).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, extractive_industry_operators).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_development_firms).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_rights_advocates).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_personhood_movements).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, future_generations_affected_by_ecological_harm).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, bodily_autonomy_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, born_alive_rule).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain full legal personhood and decisional authority over their own bodies throughout pregnancy because the fetus is not a competing rights-holder under this reading. Their exit from unwanted state intervention in reproductive decisions is protected precisely because the boundary excludes the fetus from the class of persons whose interests could override theirs.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    moderate, biographical, constrained, national).

% Practice medicine, including abortion and fertility procedures, without criminal or civil liability keyed to fetal personhood. They also litigate and lobby to keep the boundary drawn at birth, since a shift to a developmental-potentiality standard would criminalize core parts of their practice.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_healthcare_providers, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_healthcare_providers, agenda_setter).

% Adjudicate and enforce the born-alive boundary in case law, drawing on due process and equal protection doctrine. They administer the line and could, in principle, redraw it — the boundary persists because a stable majority of sitting courts continues to ratify it, not because it is logically compelled.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Site mines, pipelines, and industrial facilities without needing to litigate against ecosystems as rights-holders, because rivers, forests, and species have no standing to sue in their own name under this reading. Environmental review remains procedural rather than adjudicative of a competing person's rights.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, extractive_industry_operators, beneficiary,
    powerful, generational, arbitrage, national).

% Deploy, modify, and decommission AI systems without confronting personhood claims on behalf of those systems, since cognitive capacity alone (absent being a born human) does not qualify an entity for legal personhood under this reading. This forecloses AI liability, AI rights, and AI standing litigation that would otherwise slow deployment.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ai_development_firms, beneficiary,
    powerful, generational, arbitrage, global).

% Argue the boundary arbitrarily excludes a class of human life with a continuous developmental trajectory from legal protection, and bear the cost of losing standing to bring wrongful-death, inheritance, or personhood claims on behalf of the unborn. Their exit option is confined to further litigation and legislative advocacy within a system that keeps ruling against the premise.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_rights_advocates, payer,
    organized, generational, constrained, national).

% Seek legal standing for rivers, forests, and ecosystems (as achieved in some other jurisdictions) and are foreclosed by a reading that reserves personhood for born humans. They bear the cost of ecological harms that could otherwise be litigated directly on behalf of the harmed entity rather than through diluted procedural review.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, environmental_personhood_movements, payer,
    moderate, civilizational, constrained, national).

% Inherit ecological and infrastructural consequences of decisions made without a rights-holder able to represent long-horizon environmental interests in court today. They have no voice in current proceedings and no legal standing of their own under this reading.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, future_generations_affected_by_ecological_harm, payer,
    powerless, civilizational, trapped, global).

% Study the doctrinal history and consequences of the born-alive boundary, comparing it against developmental-potentiality and functional-capacity readings, without a stake in which reading prevails.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_philosophy_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__restrictive_anthropocentric_reading, diffuse).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__restrictive_anthropocentric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, administrable line for who counts as a rights-bearer under law — birth plus cognitive capacity — so that courts, hospitals, and regulators have a workable threshold instead of adjudicating personhood case-by-case on contested metaphysical grounds.
% TRANSFER_FUNCTION: Moves decisional authority and legal protection toward pregnant persons, reproductive healthcare providers, extractive industry operators, and AI developers, and away from fetal-rights claimants, environmental-standing advocates, and diffuse future generations who cannot assert personhood-based claims under this boundary.
% ABSENT_VOICES: Fetuses, ecosystems, and future generations have no direct voice in the courts that draw this line — they are represented, if at all, by advocacy organizations whose standing is itself contested. Non-human cognitively sophisticated animals and advanced AI systems are also structurally absent from the personhood conversation despite functional-capacity arguments being raised on their behalf in adjacent readings.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned overnight in favor of a developmental-potentiality or functional-capacity standard, reproductive medicine, environmental permitting, and AI deployment would all face immediate new liability exposure and litigation from newly-recognized rights-holders (fetuses, ecosystems, or capable non-human/artificial systems) — the arrangement is load-bearing for large sectors of current practice, not a free-floating description of reality.
% FOUNDING_PROBLEM: Common law and constitutional doctrine needed a stable, administrable threshold for legal personhood that could be applied uniformly across contexts (inheritance, criminal law, civil rights) without requiring courts to resolve contested questions about the moral status of fetuses, animals, or non-human systems on a case-by-case basis.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional courts and mainstream legal scholarship attest the born-alive threshold remains a live, functioning doctrinal solution to the administrability problem. Fetal-rights and environmental-personhood advocates, writing from outside the beneficiary set, corroborate that the underlying problem (how to allocate rights among human, potential-human, and non-human interests) is very much unresolved rather than settled — they argue the current line is a contested policy choice presented as a stable resolution, not evidence the founding problem was actually solved.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).
:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) because the boundary does confer a real coordination benefit (administrable rights threshold) alongside a real, if diffuse, cost shifted onto excluded classes — this is not a pure land-grab, it is a genuine hybrid. Suppression is moderate-to-high (0.55) and rising over the interval as courts more explicitly foreclose fetal-personhood and ecosystem-standing litigation through precedent, hardening what began as an interpretive default into an actively defended line. Resistance is high (0.72) because fetal-rights and environmental-personhood movements mount sustained, organized legal and political challenges to the boundary — this is a heavily contested doctrinal line, not a settled natural fact, which is reflected in low-to-moderate accessibility_collapse (0.5): alternative framings remain live and litigated, not foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   From the pregnant-persons and healthcare-provider seats, this reading is protective coordination — it prevents a competing rights claim from overriding their autonomy. From the fetal-rights and environmental-personhood seats, the identical doctrinal line is extractive foreclosure — it prevents their preferred rights claims from ever reaching a court on the merits. The engine computes these as different seat-level classifications from the same structural facts; neither seat is wrong about its own position, and the divergence itself is the analytically interesting output.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant persons, reproductive healthcare providers, extractive industry operators, and AI developers are structural beneficiaries: the boundary either protects their decisional autonomy directly or shields them from personhood-based liability claims that a broader reading would create. Fetal-rights advocates and environmental-personhood movements are structural targets: the boundary is precisely what forecloses their preferred legal theories from prevailing. Future generations sit furthest toward the target end (powerless, trapped, civilizational horizon) because they cannot even organize to advocate within the current system — their exclusion is total and structurally locked in.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing an administrable threshold for personhood — remains genuinely live (courts still need SOME threshold), which cuts against treating this as pure mandatrophy. But the specific placement of the threshold at birth-plus-cognitive-capacity is a contested policy choice dressed in the language of doctrinal necessity; the coordination function (having a threshold at all) does not by itself justify this particular threshold over the sibling readings. Classifying as tangled_rope rather than mountain or rope prevents both overclaiming (treating the boundary as natural law) and underclaiming (treating it as pure extraction with no coordination value) — it is a real coordination device that also produces asymmetric winners and losers along an axis the courts actively defend.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personhood_boundary_kernel_reading_choice,
    'Is the born-alive-plus-cognitive-capacity threshold the uniquely correct reading of personhood, or one of several defensible readings of an irreducibly contested kernel (alongside developmental-potentiality and functional-capacity readings)?',
    'No empirical resolution mechanism exists — this is a foundational normative/conceptual dispute about the criteria for moral and legal status, not a factual question the courts or science can settle definitively. Track shifts in comparative jurisprudence (e.g., jurisdictions granting river/ecosystem personhood, fetal personhood statutes, AI legal-status litigation) as evidence of which reading is gaining or losing ground, without treating any shift as proof of correctness.',
    'If the developmental-potentiality reading were adopted instead, fetuses would enter the victim set of THIS reading as beneficiaries of the alternative, and reproductive healthcare providers would shift from beneficiary to payer. If the functional-capacity reading were adopted, ecosystems and sufficiently sophisticated AI systems could gain standing, converting extractive_industry_operators and ai_development_firms from beneficiaries to payers under that sibling constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(personhood_boundary_kernel_reading_choice, conceptual, 'Whether this reading is the correct resolution of the personhood kernel or one contested reading among several structurally distinct siblings.').

omega_variable(
    beneficiary_capture_of_administrability_rationale,
    'Is the administrability rationale (courts need a workable threshold) doing genuine coordination work, or is it a post-hoc justification that happens to track where power already sits (protecting established medical, industrial, and technology practices from new liability)?',
    'Examine whether courts adopting this reading engage with functional-capacity or developmental-potentiality alternatives on their administrability merits, or dismiss them primarily via appeals to settled precedent and practical disruption to existing industries.',
    'If administrability is primarily a shield for incumbent interests, the tangled_rope classification understates extraction and the constraint drifts closer to snare; if administrability reflects genuine institutional constraint, the tangled_rope classification''s coordination component is well-founded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_administrability_rationale, conceptual, 'Whether the coordination rationale is genuine or a cover story for protecting incumbent beneficiaries.').

omega_variable(
    future_generations_standing_gap,
    'Can the exclusion of future generations from personhood-based environmental standing be remedied within this reading (e.g., through guardian ad litem mechanisms or statutory environmental rights) without abandoning the born-human-cognitive-capacity threshold itself?',
    'Track jurisdictions that have created statutory or common-law representation mechanisms for future generations or the environment without extending personhood per se, and assess whether these function as adequate substitutes.',
    'If adequate substitute mechanisms exist, the victim status of future_generations_affected_by_ecological_harm is overstated; if no such mechanism functions in practice, the exclusion is a genuine and currently unremedied structural cost of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_generations_standing_gap, empirical, 'Whether alternative representation mechanisms mitigate the exclusion of future generations from standing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 40, 0.19).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__functional_capacity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints instantiating readings of the shared legal_personhood_boundary kernel. Each reading has a distinct ε, distinct beneficiary/victim sets, and is evaluated independently per the ε-invariance principle — they are not the same constraint measured differently. The restrictive_anthropocentric_reading (this story) minimizes extraction from pregnant persons, industry, and AI developers while shifting cost onto fetal-rights and environmental-personhood claimants; the developmental_potentiality_reading inverts much of this beneficiary/victim structure; the functional_capacity_reading introduces a wholly different victim/beneficiary axis organized around cognitive capacity rather than species or birth status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
