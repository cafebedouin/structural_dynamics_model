% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Equality as Universal Principle Requiring Iterative Expansion
 *   domain: constitutional_law / political_philosophy
 *
 * SUMMARY:
 *   This constraint is the universalist reading of the contested kernel 'all
 *   men created equal.' The reading treats equality as a universal principle
 *   whose scope expands iteratively over time, regardless of founder intent.
 *   It coordinates the problem of how a restrictive founding can legitimately
 *   evolve to include marginalized groups by invoking a universal meaning
 *   allegedly latent in the founding language. It extracts costs from the
 *   institutional establishment and from descendants of original
 *   beneficiaries (slaveholders, male monopolies), while benefiting groups
 *   claiming inclusion through expanded equal status. The constraint is
 *   claimed as tangled_rope because it genuinely solves the coordination
 *   problem of how restrictive founding can acknowledge marginalized claims
 *   (coordination function) while simultaneously imposing expansion costs on
 *   those who benefit from restriction (extraction function).
 *   Institutionally, it requires continuous active enforcement—courts
 *   striking down restrictions, legislatures passing civil-rights
 *   legislation, executives enforcing equal protection—making the enforcement
 *   real and the extraction measurable, not performative.
 *
 * KEY AGENTS:
 *   - Marginalized groups claiming inclusion (beneficiaries) — enslaved people, women, religious minorities, post-colonial immigrants, LGBTQ+ persons — invoke universal language to demand equal status, gaining argumentative and legal standing through the universalist reading
 *   - Constitutional establishment and descendant beneficiaries (payers) — institutions defending the Constitution, interests that benefited from original restrictions — bear expansion costs (litigation, legitimacy strain, redistributed privileges)
 *   - Originalist legal tradition (secondary payer) — scholars and judges committed to bounded-intent interpretation bear the burden of defending why universal language was not universally applied
 *   - Federal constitutional authority (agenda setter) — courts, legislatures, executives enforce expansions and recognize new claim-groups, managing the legitimacy strain and coordination burden
 *   - Philosophical universalist tradition (observer) — philosophers outside the system provide the interpretive frame that universal principles cannot be bounded by historical intent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.58).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.42).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, tangled_rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Equality as Universal Principle Requiring Iterative Expansion").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law / political_philosophy").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, '3a285639-78e0-46ba-99ab-4c717f34578a').
narrative_ontology:cs_kernel_codification('3a285639-78e0-46ba-99ab-4c717f34578a', fixed_text).
narrative_ontology:cs_authority_grounding('3a285639-78e0-46ba-99ab-4c717f34578a', lineage).
narrative_ontology:cs_interpretation_layer_present('3a285639-78e0-46ba-99ab-4c717f34578a').
narrative_ontology:cs_reading_relation('3a285639-78e0-46ba-99ab-4c717f34578a', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a285639-78e0-46ba-99ab-4c717f34578a', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('3a285639-78e0-46ba-99ab-4c717f34578a', foundational, universal_principle_transcends_intent).
narrative_ontology:cs_axiom_status(universal_principle_transcends_intent, holdable).
narrative_ontology:cs_axiom_grounding('3a285639-78e0-46ba-99ab-4c717f34578a', universal_principle_transcends_intent, deontological).
narrative_ontology:cs_axiom('3a285639-78e0-46ba-99ab-4c717f34578a', foundational, meaning_revealed_through_application).
narrative_ontology:cs_axiom_status(meaning_revealed_through_application, holdable).
narrative_ontology:cs_axiom_grounding('3a285639-78e0-46ba-99ab-4c717f34578a', meaning_revealed_through_application, deontological).
narrative_ontology:cs_reference_frame('3a285639-78e0-46ba-99ab-4c717f34578a', universal_equality_evolving_scope).
narrative_ontology:cs_drift_state('3a285639-78e0-46ba-99ab-4c717f34578a', contemporary_post_civil_rights_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3a285639-78e0-46ba-99ab-4c717f34578a', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, groups_denied_equal_status_historically).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, constitutional_establishment_bearing_expansion_cost).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the constraint benefits marginalized groups (shifting their directionality toward low extraction) while imposing measurable costs on the establishment (shifting their directionality toward high extraction). Suppression is lower (0.42) because the universalist claim has achieved significant institutionalization — marginalized groups have legal standing to press claims, courts recognize them, and the constraint is defended in mainstream jurisprudence rather than suppressed or hidden. Accessibility to alternatives is moderate (0.68) because groups denied equal status can theoretically appeal to universal language, yet that appeal requires institutional recognition — one cannot simply opt out of needing equal status. Resistance is high (0.71) because originalist and conservative interests mount substantial resistance to each expansion, defending the restricted reading through sophisticated textual arguments and institutional power. Theater is moderate (0.28) and rising because after approximately 120 units (mid-20th century), as the universalist reading becomes institutionally mainstream, institutions increasingly perform universalism rhetorically while enforcement patterns remain selective — civil rights are affirmed while implementation lags, equality is celebrated while material gaps persist. The measurement series show base_extractiveness rising from 0.35 (t=0, founding) to a peak of 0.63 (t=160, late 20th century civil-rights maturity) then stabilizing at 0.58, suggesting the most disruptive expansion phases have passed and the constraint has shifted from acute conflict to managed institutionalization. Suppression_requirement falls over the same period (0.68 to 0.40) as the reading gains institutional legitimacy and no longer needs defensive suppression — the cost of defending universalism becomes institutional rather than coercive.
 *
 * PERSPECTIVAL GAP:
 *   The federalist constitutional authority and originalist tradition compute the constraint very differently. From the authority's seat, the universalist reading is a legitimate constitutional evolution—the system functioning as intended, recognizing universal principles as understanding improves. From the originalist seat, the same structure is an illegitimate judicial rewriting of a fixed text, an extraction of interpretive authority by courts claiming to discover principles the founders explicitly rejected or left ambiguous. From the marginalized-group seat, the constraint is a real, hard-won expansion of protection, with benefits accruing through legal standing and reduced institutional exclusion. From the descendant-beneficiary seat (slaveholders' heirs, male professional monopolies), the same constraint is a loss of inherited privilege and social hierarchy. The engine computes per-seat classification from power, exit, and beneficiary/victim declarations: high-power originalists with constrained exit (trapped in the interpretive tradition) facing a universalist claim that erodes their authority compute high directionality (d near 1.0, targets of extraction); powerless marginalized groups with constrained exit but gaining legal standing through the claim compute lower d (0.3–0.5, beneficiaries-with-cost). These divergences are structural, not perspectival opinion — the constraint operates asymmetrically across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized groups claiming inclusion have d values near the beneficiary end (0.1–0.3) because the universalist reading directly benefits them: it gives them argumentative standing, institutional recognition, and potential rights-protection. Their only cost is the constraint's continued requirement that they continually press claims and defend inclusion—a coordination cost, not extraction. Originalists and the constitutional establishment have d values near the target end (0.6–0.9) because the constraint erodes their interpretive authority, requires them to defend their bounded reading against universal language, and imposes institutional burden through endless litigation and expansion. Descendant beneficiaries have even higher d (0.75–0.95) because they bear material loss—slaveholders' descendants lose property claims, male associations lose monopolies, religious establishments lose privileges. The federalist authority sits near symmetric (d = 0.5) because it both benefits (maintains legitimacy by appearing responsive to justice claims) and pays (bears institutional burden, legitimacy strain from performing universalism while enforcement remains selective). These directionality assignments derive from the beneficiary/victim declarations and exit-option modulation: beneficiaries with constrained exit get lower d; victims with constrained exit get higher d; those with mobile exit (powerful descendant interests) face modulation that raises d slightly (trapped extraction on the powerful). The overrides here are necessary because raw power atoms alone would misclassify: powerful originalists with institutional position would appear low-d without recognizing that the universalist reading specifically targets and erodes their authority.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (universal language incompatible with restrictive practice) is contested between readings but has clear material content: the Constitution says 'all' but the practice restricts to 'some.' The universalist reading resolves this by treating the contradiction as a genuine mandate for expansion. The originalist reading resolves it by treating the universal language as rhetorical while restricting scope through founder intent. The tangled_rope classification is correct because the constraint genuinely solves the coordination problem (how to evolve a restrictive founding to include marginalized groups) while simultaneously imposing asymmetric extraction (costs to the establishment, benefits to included groups). The constraint cannot be classified as pure rope (symmetric coordination with no victims) because the expansion is contested and imposed against originalist and conservative resistance—the benefited groups do not universally prefer this arrangement; they prefer it because they benefit from it. The constraint cannot be classified as snare (pure extraction with no real coordination function) because the universalist reading solves an authentic coordination problem: without it, the system faces a delegitimizing contradiction that undermines the entire constitutional project. The mandatrophy question is whether the founding problem persists or has been solved by sufficient expansion. The contested status indicates the readings disagree: universalists say the founding problem is solved only through continual expansion (live problem as long as any group lacks equal status); originalists say the problem is resolved by recognizing that universal language never meant to expand beyond the founders' intent (dead problem once we stop misreading). The constraint's persistence therefore depends on whether marginal expansion continues to occur—if equality expansion halts and stabilizes at some fixed set of included groups, the originalists' reading of a resolved contradiction would gain plausibility, and the constraint might shift toward piton (performed universalism masking stable restriction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_constructed_intent,
    'Is the universal principle latent in the founding text a genuine universal moral truth, or a retrospective construction imposed on founders who intended restriction?',
    'Textual analysis of founding-era documents and founders'' correspondence; philosophical argument about whether universal principles can be ''discovered'' in language or only ''constructed'' through interpretation.',
    'If the principle is genuinely latent (discovered), the universalist reading is a mountain-adjacent natural-law constraint. If constructed, it is a contestable interpretation riding on institutionalized power — the reading would shift toward snare (pure extraction by judicial authority rewriting the Constitution).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_vs_constructed_intent, conceptual, 'Whether universality is discovered in or imposed on the founding text.').

omega_variable(
    expansion_costs_vs_justice_benefits,
    'Are the institutional and social costs of continual equality expansion (litigation burden, legitimacy strain, disrupted privileges) proportionate to the benefits accruing to included groups, or does the constraint extract from the establishment without commensurate gain?',
    'Economic and social analysis of rights-expansion outcomes: do included groups experience durable material and political gains, or ephemeral symbolic recognition followed by structural exclusion? Longitudinal measurement of equality in practice post-recognition.',
    'If benefits exceed costs, the constraint is genuine coordination (rope-like tangled rope). If costs exceed benefits for included groups, the constraint is performative—recognition without material change, theater riding on legitimacy extraction from the constitutional system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_costs_vs_justice_benefits, empirical, 'Whether expansion benefits justify its institutional costs.').

omega_variable(
    reading_foreclosure_under_originalism,
    'Does the universalist reading logically foreclose the originalist reading within the same constitutional framework, or can both readings coexist as competing but internally coherent hermeneutics?',
    'Philosophical analysis of whether a framework can simultaneously affirm that (a) the Constitution''s language is universal, and (b) founder intent limits the scope of application. Can both be true in one system, or does affirming (a) invalidate (b)?',
    'If foreclosure is real, the reading_relations should mark ''forecloses'' rather than ''coexists_with''. If both are coherent, they coexist as irreducible readings of an ambiguous kernel. This affects the diagnosis of false summits: does universalist ascendency constitute a genuine alternative discovery, or a contestable rewriting?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_under_originalism, conceptual, 'Logical relationship between universalist and originalist readings in a single constitutional framework.').

omega_variable(
    identity_fusion_institutional_defense,
    'Do institutional actors (courts, legislatures, executives) defending the universalist reading do so from genuine commitment to universal equality, or from identity fusion with the institutional role itself and institutional need to appear dynamic and responsive?',
    'Comparative institutional behavior analysis: do institutions defend universalist expansion equally vigorously regardless of the claim-group''s power and salience, or selectively when doing so garners legitimacy? Post-recognition behavior: do institutions maintain the expanded equality or allow it to erode through non-enforcement or resource starvation?',
    'If behavior is selective or post-recognition erosion occurs, the institutional defense is partly theater — the constraint extracts legitimacy from appearing universal while suppressing actual universality through enforcement selectivity. This would raise the theater_ratio and suggest the constraint is a piton (performing universalism while actual enforcement patterns remain restrictive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_institutional_defense, empirical, 'Whether institutional defense of universalism is commitment-based or identity-fusion theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t0, all_men_created_equal__universalist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(all__tr_t40, all_men_created_equal__universalist_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement(all__tr_t80, all_men_created_equal__universalist_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(all__tr_t120, all_men_created_equal__universalist_reading, theater_ratio, 120, 0.24).
narrative_ontology:measurement(all__tr_t160, all_men_created_equal__universalist_reading, theater_ratio, 160, 0.3).
narrative_ontology:measurement(all__tr_t200, all_men_created_equal__universalist_reading, theater_ratio, 200, 0.28).
narrative_ontology:measurement(all__tr_t250, all_men_created_equal__universalist_reading, theater_ratio, 250, 0.28).

% Extraction over time
narrative_ontology:measurement(all__be_t0, all_men_created_equal__universalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(all__be_t40, all_men_created_equal__universalist_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(all__be_t80, all_men_created_equal__universalist_reading, base_extractiveness, 80, 0.52).
narrative_ontology:measurement(all__be_t120, all_men_created_equal__universalist_reading, base_extractiveness, 120, 0.58).
narrative_ontology:measurement(all__be_t160, all_men_created_equal__universalist_reading, base_extractiveness, 160, 0.63).
narrative_ontology:measurement(all__be_t200, all_men_created_equal__universalist_reading, base_extractiveness, 200, 0.58).
narrative_ontology:measurement(all__be_t250, all_men_created_equal__universalist_reading, base_extractiveness, 250, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t0, all_men_created_equal__universalist_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(all__su_t40, all_men_created_equal__universalist_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(all__su_t80, all_men_created_equal__universalist_reading, suppression_requirement, 80, 0.54).
narrative_ontology:measurement(all__su_t120, all_men_created_equal__universalist_reading, suppression_requirement, 120, 0.45).
narrative_ontology:measurement(all__su_t160, all_men_created_equal__universalist_reading, suppression_requirement, 160, 0.4).
narrative_ontology:measurement(all__su_t200, all_men_created_equal__universalist_reading, suppression_requirement, 200, 0.42).
narrative_ontology:measurement(all__su_t250, all_men_created_equal__universalist_reading, suppression_requirement, 250, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(all_men_created_equal__universalist_reading, 0.12).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__originalist_reading).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, all_men_created_equal__textualist_paradox_reading).

% DUAL FORMULATION NOTE:
% The kernel 'all men created equal' instantiates three structurally distinct constraints corresponding to three incompatible readings of what the universal language means and how it should govern practice. Each reading produces a different epsilon value because the structural relationship between universal language and restrictive founding differs fundamentally. The universalist reading (this constraint) treats the gap as a mandate for expansion (moderate extractiveness from institution-bearing costs); the originalist reading treats it as resolved by fixing scope to founder intent (low extractiveness, minimal institutional burden); the textualist reading treats it as an irreconcilable performative contradiction (high extractiveness from institutional legitimacy erosion). These are not the same constraint viewed from different angles — they are distinct constraints whose existence depends on which reading of the kernel is adopted. Each has its own epsilon, its own beneficiary/victim structure, and its own lifecycle. They are linked here because determining which reading is structurally true is the epistemic problem the corpus is designed to illuminate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(all_men_created_equal__universalist_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
