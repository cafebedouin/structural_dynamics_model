% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent as Adaptive Evolutionary Framework
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the evolutionary-framework reading of the common
 *   law precedent kernel: precedent is treated as a living, adaptive
 *   framework that legitimately yields to contemporary normative evolution,
 *   with overruling normalized as a corrective mechanism rather than a
 *   rupture. This is one of three structurally distinct readings of the same
 *   underlying kernel (the practice and doctrine of stare decisis) and is
 *   authored as a self-contained constraint with its own epsilon,
 *   beneficiary/victim structure, and classification — not averaged against
 *   the sibling readings (strict_stare_decisis, pluralist_balancing), which
 *   are separate constraint files.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: institutional agenda-setter empowered to treat precedent as revisable
 *   - reform_oriented_litigators: organized beneficiaries who build strategy around doctrinal openness
 *   - marginalized_groups_underserved_by_old_precedent: powerless beneficiaries with no exit but the framework itself
 *   - parties_relying_on_settled_precedent and long_term_contractual_and_property_interests: payers who bear retroactive doctrinal risk
 *   - originalist_and_strict_stare_decisis_adherents: excluded voice structurally cast as the position being corrected
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.42).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.28).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.42).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent as Adaptive Evolutionary Framework").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal_theory/jurisprudence/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, 'ea6506d2-e9ac-4f44-955c-abea777422c0').
narrative_ontology:cs_kernel_codification('ea6506d2-e9ac-4f44-955c-abea777422c0', distributed).
narrative_ontology:cs_authority_grounding('ea6506d2-e9ac-4f44-955c-abea777422c0', practice).
narrative_ontology:cs_interpretation_layer_present('ea6506d2-e9ac-4f44-955c-abea777422c0').
narrative_ontology:cs_reading_relation('ea6506d2-e9ac-4f44-955c-abea777422c0', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('ea6506d2-e9ac-4f44-955c-abea777422c0', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('ea6506d2-e9ac-4f44-955c-abea777422c0', foundational, precedent_revisability_as_legitimate_correction).
narrative_ontology:cs_axiom_status(precedent_revisability_as_legitimate_correction, holdable).
narrative_ontology:cs_axiom_grounding('ea6506d2-e9ac-4f44-955c-abea777422c0', precedent_revisability_as_legitimate_correction, conventional).
narrative_ontology:cs_axiom('ea6506d2-e9ac-4f44-955c-abea777422c0', foundational, contemporary_normative_consensus_as_valid_interpretive_input).
narrative_ontology:cs_axiom_status(contemporary_normative_consensus_as_valid_interpretive_input, holdable).
narrative_ontology:cs_axiom_grounding('ea6506d2-e9ac-4f44-955c-abea777422c0', contemporary_normative_consensus_as_valid_interpretive_input, instrumental).
narrative_ontology:cs_reference_frame('ea6506d2-e9ac-4f44-955c-abea777422c0', corrective_common_law_tradition).
narrative_ontology:cs_drift_state('ea6506d2-e9ac-4f44-955c-abea777422c0', contemporary_appellate_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ea6506d2-e9ac-4f44-955c-abea777422c0', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, rights_claimants_seeking_novel_relief).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, reform_oriented_litigators).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, marginalized_groups_underserved_by_old_precedent).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, parties_relying_on_settled_precedent).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, long_term_contractual_and_property_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, trial_courts_and_lower_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides when precedent should be revisited in light of changed social understanding, empirical learning, or moral consensus shift. Holds the authority to overrule prior decisions and frames overruling as principled correction rather than instability. Its own institutional legitimacy and doctrinal legacy are enhanced by successful, well-received reinterpretations.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Bring claims that existing precedent forecloses under a rigid reading but that an evolutionary reading opens to reconsideration. Depend entirely on courts being willing to treat old rulings as revisable in light of new social facts; without that openness they have no forum at all.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, rights_claimants_seeking_novel_relief, beneficiary,
    moderate, biographical, constrained, national).

% Build long-term litigation strategies around persuading courts to abandon precedent they view as outdated. Their professional and organizational value depends on the framework's normalization of overruling as legitimate; they actively shape doctrine by selecting test cases and advancing new normative arguments.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, reform_oriented_litigators, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, reform_oriented_litigators, agenda_setter).

% Historically excluded from legal protection under earlier precedent regimes. Have no exit from the legal system itself and depend on the evolutionary reading to obtain recognition that a rigid stare decisis regime would deny them for generations. Cannot litigate their way around the framework; they can only hope courts exercise the discretion the framework grants.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, marginalized_groups_underserved_by_old_precedent, beneficiary,
    powerless, generational, trapped, national).

% Structured contracts, business arrangements, and personal affairs around the expectation that established precedent would hold. Bear the cost when courts reinterpret governing rules retroactively or announce doctrinal shifts that undercut settled expectations they had no opportunity to renegotiate around.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, parties_relying_on_settled_precedent, payer,
    moderate, biographical, constrained, national).

% Institutional holders of property, financial, and contractual interests structured over decades under a given precedent regime. Substantial capital is at stake when the framework's normalization of overruling introduces the risk that governing rules can change mid-course; they cannot fully hedge against a judiciary empowered to revisit settled doctrine.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, long_term_contractual_and_property_interests, payer,
    powerful, civilizational, constrained, national).

% Must apply precedent day to day and absorb the uncertainty created when higher courts signal openness to reinterpretation. Bear the administrative cost of relitigating settled questions and issuing rulings that may be overtaken by appellate reinterpretation before final resolution.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, trial_courts_and_lower_judiciary, observer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, trial_courts_and_lower_judiciary, payer).

% Would argue that treating precedent as merely provisional undermines rule-of-law predictability and substitutes judicial preference for settled law. Are present in the broader legal discourse but structurally disfavored within this reading's own framework, which treats their position as the thing being corrected rather than a live constraint on legitimate adjudication.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, originalist_and_strict_stare_decisis_adherents, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__evolutionary_framework, diffuse).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__evolutionary_framework, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Precedent under this reading solves the problem of legal knowledge accumulation while remaining responsive to demonstrated error and social change: courts do not have to relitigate first principles in every case, but the system retains a legitimate, non-revolutionary mechanism for correcting rulings that later prove unjust, unworkable, or empirically mistaken.
% TRANSFER_FUNCTION: Moves normative authority from the text of prior rulings and the parties who relied on them toward the contemporary judiciary's assessment of present social consensus; correspondingly moves risk from claimants seeking new interpretations onto parties who structured their affairs around the old rule.
% ABSENT_VOICES: Adherents of strict stare decisis and those with vested reliance interests in settled doctrine are present in legal debate generally but are structurally positioned as the objection being overcome within this reading, not as a constraint the reading treats as binding on itself.
% DISAPPEARANCE_RATIONALE: If the evolutionary reading were abandoned in favor of strict stare decisis, entire categories of contemporary claims (civil rights expansions, tort doctrine modernization, family law reform) would lose their principal doctrinal pathway; reform litigation strategy, judicial legitimacy narratives, and settled-party risk calculus would all reorganize substantially.
% FOUNDING_PROBLEM: Rigid adherence to precedent produced demonstrably unjust or empirically outdated rules (segregation-era doctrine, common law disabilities, outdated scientific assumptions embedded in tort and family law) that a purely backward-looking system had no legitimate internal mechanism to correct.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative-law scholars outside the reform litigation community broadly corroborate that historical overruling episodes (e.g., desegregation-era doctrine, workers' rights expansions) addressed genuine injustice; however, scholars aligned with reliance-interest and rule-of-law traditions dispute whether the founding problem remains as pressing today as it was historically, or whether the framework has become a general-purpose tool for doctrinal preference substitution beyond its original corrective scope.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).
:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate 0.42: the framework redistributes normative authority and reliance-interest risk toward settled parties, but this is a genuine byproduct of a real coordination function (correcting demonstrated legal error) rather than pure rent extraction, so extraction sits well below snare territory. Suppression is comparatively low (0.28) because the framework does not physically or economically trap anyone into compliance — it operates through legitimacy and argument, and losing parties retain avenues (legislative override, further appeal, dissent-building for future reversal). Resistance is moderately high (0.58) because reliance-interest holders and stare decisis traditionalists actively contest the framework's legitimacy in scholarship, briefs, and confirmation politics. Theater ratio is low-to-moderate and rising slowly (0.12 to 0.22) reflecting some accumulation of rhetorical overruling-as-correction framing without corresponding substantive change in outcomes over time — a mild drift worth watching but not dominant.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (rights claimants, reform litigators, marginalized groups) sit near the subsidized end because the framework's discretion is precisely what gives them access to relief a rigid regime would deny. Payers (settled-precedent reliant parties, long-term capital holders) sit toward the target end because they bear the cost of doctrinal instability they did not choose and cannot fully hedge against, despite holding substantial nominal power in the case of institutional capital holders. The judiciary is the agenda-setter whose analytical exit option reflects that it adjudicates the framework rather than being subject to it in the way private parties are.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework's founding problem (correcting demonstrably unjust or outdated precedent) is authored as contested rather than dead or fully live: some historical instances (segregation-era doctrine) are broadly agreed to have needed correction, but critics argue the mechanism has since generalized into a tool for ordinary doctrinal preference-substitution beyond emergency correction. This is exactly the ambiguity the mandatrophy question is designed to surface — the reading should not be treated as either purely legitimate perpetual reform or as a captured mechanism without examining whether court-by-court overruling still tracks genuine normative error versus routine doctrinal churn.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    correction_versus_preference_substitution,
    'When courts overrule precedent under the evolutionary framework, are they correcting demonstrated legal or empirical error, or substituting a new normative preference under the cover of principled correction?',
    'Comparative study of overruling opinions coded for whether they cite new empirical evidence, changed factual predicates, or purely evolving moral/political consensus, cross-checked against dissenting opinions'' characterizations at the time.',
    'If overruling tracks demonstrated error, the framework functions closer to a genuine correction mechanism (rope-like); if it tracks judicial composition and political preference shifts, the framework functions closer to an extraction mechanism transferring authority from settled law to whichever judiciary currently sits (tangled_rope-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correction_versus_preference_substitution, empirical, 'Whether overruling under this reading tracks genuine error correction or preference substitution.').

omega_variable(
    reading_selection_and_case_composition,
    'Is the evolutionary framework''s dominance in a given era a function of which cases reach appellate courts, or an independent judicial commitment to the reading itself?',
    'Track litigation funding patterns and case selection by reform-oriented litigators against baseline rates of similar claims that do not reach appellate review.',
    'If dominance is driven by strategic case selection by organized litigators, part of the framework''s apparent normative pull is itself a product of resourced advocacy rather than pure judicial philosophy, which would shift some of the coordination credit toward strategic extraction by organized reform interests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_and_case_composition, conceptual, 'Whether the framework''s prevalence reflects judicial philosophy or strategic litigation shaping.').

omega_variable(
    reliance_interest_measurement_horizon,
    'Over what time horizon should the cost imposed on settled-precedent-reliant parties be measured — immediate disruption, or long-run system-wide gains from corrected doctrine?',
    'Longitudinal economic and legal-outcome studies tracking affected industries and rights-holders across multiple decades following major overruling episodes.',
    'A short horizon makes the framework look more extractive toward settled parties; a long horizon may reveal net systemic benefit that offsets the transitional cost, changing the extraction assessment substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliance_interest_measurement_horizon, conceptual, 'The time horizon problem in assessing cost to reliance-interest holders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.12).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 10, 0.14).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 20, 0.16).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 30, 0.18).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 40, 0.19).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 50, 0.21).
narrative_ontology:measurement(comm_tr_t60, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(comm_be_t60, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 60, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(common_law_precedent_corpus__evolutionary_framework, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_law_precedent_corpus__evolutionary_framework, 0.12).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the single natural-language concept of 'common law precedent doctrine' per the epsilon-invariance principle. Each reading (evolutionary_framework, pluralist_balancing, strict_stare_decisis) carries its own epsilon, beneficiary/victim structure, and classification because measuring the doctrine's extractiveness from each reading's own normative commitments yields materially different values. The evolutionary_framework reading (this story) authors the lowest rigidity and highest discretion; strict_stare_decisis authors the highest rigidity and lowest discretion; pluralist_balancing sits between. All three are linked bidirectionally via affects_constraints to preserve the family structure for contamination and drift propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_law_precedent_corpus__evolutionary_framework, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
