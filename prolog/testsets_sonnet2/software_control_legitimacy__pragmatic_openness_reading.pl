% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__pragmatic_openness_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__pragmatic_openness_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: software_control_legitimacy__pragmatic_openness_reading
 *   human_readable: Pragmatic Openness Reading of Software Control Legitimacy
 *   domain: software_engineering/political_economy_of_technology
 *
 * SUMMARY:
 *   This constraint represents the pragmatic-openness reading of the
 *   contested software-control-legitimacy kernel: the claim that open-source
 *   and proprietary development are methodologically distinct but equally
 *   legitimate strategies, with open source's empirical strength (peer
 *   review, distributed defect-finding, transparency) treated as a quality
 *   argument rather than an ethical mandate. Unlike the
 *   freedom_imperative_reading (which holds proprietary restriction ethically
 *   illegitimate) or the property_rights_reading (which grounds legitimacy in
 *   creator authority), this reading brackets the ethics question entirely
 *   and evaluates methodology on engineering outcomes. It shares structural
 *   ground with the commons_reading in accepting plural legitimate
 *   arrangements, but does not require negotiated collective governance — it
 *   accepts market-mediated coexistence.
 *
 * KEY AGENTS:
 *   - open_source_contributors: distributed peer-review labor, mobile exit, organized
 *   - software_users: choose between models on merit, mobile exit
 *   - commercial_software_vendors: proprietary licensors, institutional power, treated as legitimate under this reading
 *   - enterprise_adopters: select stacks by empirical fit
 *   - free_software_advocates: excluded ethical objection that restriction itself is illegitimate
 *   - software_engineering_researchers: analytical observers measuring empirical quality claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__pragmatic_openness_reading, 0.18).
domain_priors:suppression_score(software_control_legitimacy__pragmatic_openness_reading, 0.12).
domain_priors:theater_ratio(software_control_legitimacy__pragmatic_openness_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(software_control_legitimacy__pragmatic_openness_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__pragmatic_openness_reading, rope).
narrative_ontology:human_readable(software_control_legitimacy__pragmatic_openness_reading, "Pragmatic Openness Reading of Software Control Legitimacy").
narrative_ontology:topic_domain(software_control_legitimacy__pragmatic_openness_reading, "software_engineering/political_economy_of_technology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__pragmatic_openness_reading, 'b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7').
narrative_ontology:cs_kernel_codification('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7', distributed).
narrative_ontology:cs_authority_grounding('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7', practice).
narrative_ontology:cs_interpretation_layer_present('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7').
narrative_ontology:cs_reading_relation('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7', software_control_legitimacy__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7', software_control_legitimacy__property_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7', foundational, methodology_choice_is_ethically_neutral).
narrative_ontology:cs_axiom_status(methodology_choice_is_ethically_neutral, holdable).
narrative_ontology:cs_axiom_grounding('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7', methodology_choice_is_ethically_neutral, instrumental).
narrative_ontology:cs_axiom('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7', secondary, quality_outcomes_determine_methodology_merit).
narrative_ontology:cs_axiom_status(quality_outcomes_determine_methodology_merit, holdable).
narrative_ontology:cs_axiom_grounding('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7', quality_outcomes_determine_methodology_merit, empirically_contingent).
narrative_ontology:cs_reference_frame('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7', methodology_pluralism_baseline).
narrative_ontology:cs_drift_state('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7', post_open_core_commercialization_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('b7eefd6e-aa4e-4612-a0f8-b239bf10b3e7', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, open_source_contributors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, software_users).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, commercial_software_vendors).
narrative_ontology:constraint_beneficiary(software_control_legitimacy__pragmatic_openness_reading, enterprise_adopters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Contribute code under permissive or copyleft licenses, gaining reputation, peer review, and collaborative improvement of shared codebases. Can freely choose to contribute to proprietary-adjacent projects, dual-license their own work, or move between projects; no single license regime binds them.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, open_source_contributors, beneficiary,
    organized, generational, mobile, global).

% Choose between open-source and proprietary software based on price, support, feature set, and trust. Under this reading, both categories of tool are legitimate offerings competing on merit; users are not locked into either paradigm by principle, only by ordinary switching costs.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_users, beneficiary,
    moderate, biographical, mobile, global).

% Choose proprietary licensing to fund development, protect competitive advantage, and sustain commercial operations. This reading treats their choice as methodologically equivalent to open development, not as an ethical compromise requiring justification.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, commercial_software_vendors, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__pragmatic_openness_reading, commercial_software_vendors, agenda_setter).

% Select software stacks — open or proprietary — based on total cost of ownership, support guarantees, and technical fit. Benefit from a marketplace where both models compete rather than one being deemed illegitimate, widening the pool of viable options.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, enterprise_adopters, beneficiary,
    powerful, biographical, mobile, global).

% Hold that proprietary restriction is an ethical wrong, not a neutral methodology choice, and that this reading's 'both are legitimate' framing launders a rights violation as a preference. Their objection is present in public discourse but is treated by this reading as one stakeholder view among several rather than a dispositive ethical claim.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, free_software_advocates, excluded,
    organized, civilizational, constrained, global).

% Study empirical outcomes — defect rates, security response times, contributor retention — across open and proprietary projects to evaluate which methodological claims about quality actually hold, without taking a position on the ethics of restriction itself.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__pragmatic_openness_reading, software_engineering_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared framework in which developers, vendors, and users can select a development methodology — distributed peer review versus centralized controlled release — based on empirical fit to a project's needs, without either model needing to be delegitimized to justify the other's existence.
% TRANSFER_FUNCTION: Under this reading nothing is extracted from a victim class; the arrangement redistributes attention and adoption toward whichever methodology produces better empirical outcomes (code quality, security response, maintenance velocity) for a given context, moving reputation and market share between competing projects rather than moving value from a payer to a collector.
% ABSENT_VOICES: Free software advocates who hold that proprietary restriction is categorically illegitimate are present in public debate but are structurally treated by this reading as advancing one preference among several, not as identifying a rights violation the reading is obligated to redress.
% DISAPPEARANCE_RATIONALE: If this pragmatic-openness framing vanished, the underlying practices (open and proprietary development) would continue, but public discourse would lose a widely-used justification that stabilizes coexistence; some parties expect the freedom_imperative or property_rights readings would simply absorb the resulting normative vacuum, others expect little practical change since methodology choice is already made on technical/business grounds independent of the framing.
% FOUNDING_PROBLEM: Early open-source advocacy (particularly from the free software movement) framed proprietary licensing as ethically illegitimate, which created friction with commercial software ecosystems; the pragmatic-openness framing was built to let engineering organizations and mixed communities adopt open methods for quality reasons without committing to a rejection of proprietary business models.
% FOUNDING_PROBLEM_CORROBORATION: Empirical software-engineering literature on defect density and time-to-patch across open and closed codebases (produced by academic researchers outside both the open-source movement and proprietary vendors) continues to be cited by neither side exclusively, and both open-source foundations and proprietary vendor trade associations independently invoke methodology-neutral framing in standards bodies, suggesting the practical coordination problem the reading addresses remains active rather than resolved.
narrative_ontology:disappearance_verdict(software_control_legitimacy__pragmatic_openness_reading, contested).
narrative_ontology:founding_problem_status(software_control_legitimacy__pragmatic_openness_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__pragmatic_openness_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__pragmatic_openness_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__pragmatic_openness_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__pragmatic_openness_reading_tests).
:- end_tests(software_control_legitimacy__pragmatic_openness_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because this reading, by construction, declares no victim class: both proprietary and open models are held legitimate, so no party's participation is coerced or structurally extracted from purely by virtue of the methodology chosen. Suppression is low (0.12) because neither model forecloses the other — developers and firms can and do move between licensing regimes. Theater ratio is low and only mildly rising (0.10 to 0.15) reflecting modest performative use of 'open source community' branding by some commercial vendors (open-core marketing) without substantial functional peer review. Accessibility collapse is low (0.2) since alternatives to either model remain fully available; resistance is moderate-low (0.25), driven mainly by free software advocates who reject the neutrality framing itself.
 *
 * PERSPECTIVAL GAP:
 *   From the software_engineering_researchers seat, methodology choice is an empirical question resolved by measurable outcomes. From the free_software_advocates seat, treating restriction as 'merely' a methodology choice already forecloses the ethical claim they are making — the engine should register this reading's low extraction figure as the reading's own internal accounting, not as a settlement of the underlying contest.
 *
 * DIRECTIONALITY LOGIC:
 *   All four named beneficiary groups sit near the low-directionality (beneficiary) end: contributors gain reputation and improved shared code, users gain market choice, vendors gain a legitimated commercial path, enterprises gain a wider option set. No victim group is declared because this reading's defining structural feature is the absence of an extraction target — that is precisely the delta that distinguishes it from the freedom_imperative_reading (which would name proprietary vendors as extracting from locked-in users) and the property_rights_reading (which would potentially name unauthorized copiers as violators, inverting the victim polarity).
 *
 * MANDATROPHY ANALYSIS:
 *   Because this reading declares no victims and low suppression, mandatrophy risk is low: there is no coercive apparatus whose original justification could have outlived its function, since the reading does not depend on active enforcement to maintain the coexistence it describes. The primary risk is not mandatrophy but omission — treating the coexistence claim as descriptively complete when it may itself be functioning as ideological cover for the ethical dispute the freedom_imperative_reading identifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    neutrality_framing_as_cover,
    'Does the pragmatic-openness reading''s ''both models are legitimate'' framing constitute a genuinely neutral empirical assessment, or does it function as an ideological move that forecloses the freedom_imperative_reading''s ethical claim by definitional fiat?',
    'Track whether pragmatic-openness framing is disproportionately invoked by parties with a commercial interest in proprietary licensing remaining unchallenged (commercial vendors, trade associations) versus by neutral third parties (academic researchers with no stake in either licensing model).',
    'If the framing''s usage is concentrated among proprietary-interested parties, this reading''s low-extraction self-assessment should be treated as reading-indexed rather than as evidence resolving the underlying kernel contest; the freedom_imperative_reading''s victim set may be understated by comparison.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutrality_framing_as_cover, conceptual, 'Whether treating methodology as ethically neutral is itself a contestable ethical move.').

omega_variable(
    empirical_quality_claim_contestability,
    'Is the claim that open source produces empirically better software through peer review robustly supported across project types, or does it hold only for specific project profiles (widely-used infrastructure) while failing for others (niche or under-resourced projects)?',
    'Meta-analysis of defect density, time-to-patch, and maintenance longevity studies segmented by project size, contributor count, and commercial backing.',
    'If the quality advantage is contingent rather than general, the reading''s coordination_function claim (methodology chosen for empirical fit) is weaker than stated, and the reading''s low theater_ratio may be understated for marketing-driven ''open-core'' projects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_quality_claim_contestability, empirical, 'Whether the open-source quality advantage generalizes or is context-dependent.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that all four kernel readings describe the same underlying software-control practices, what determines which reading a given institutional actor adopts, and is that selection itself strategic (vendors preferring pragmatic-openness or property-rights framings, advocates preferring freedom-imperative framing)?',
    'Survey which reading is invoked by which class of institutional actor (vendor trade press, foundation governance documents, advocacy literature) and correlate with each actor''s structural position (beneficiary vs. potential victim under alternative readings).',
    'If reading selection correlates strongly with structural interest, the pragmatic-openness reading''s claim to be the ''engineering-neutral'' default is undermined, and the kernel contest is better modeled as strategic framing competition rather than genuine interpretive plurality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether kernel-reading choice tracks structural interest rather than neutral interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__pragmatic_openness_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t5, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(soft_tr_t10, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(soft_tr_t15, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 15, 0.13).
narrative_ontology:measurement(soft_tr_t20, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(soft_tr_t25, software_control_legitimacy__pragmatic_openness_reading, theater_ratio, 25, 0.15).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(soft_be_t5, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(soft_be_t10, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 10, 0.16).
narrative_ontology:measurement(soft_be_t15, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(soft_be_t20, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(soft_be_t25, software_control_legitimacy__pragmatic_openness_reading, base_extractiveness, 25, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(software_control_legitimacy__pragmatic_openness_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__pragmatic_openness_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(software_control_legitimacy__pragmatic_openness_reading, 0.12).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__freedom_imperative_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__pragmatic_openness_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposed from the natural-language 'software control legitimacy' concept per the epsilon-invariance principle. Each reading (pragmatic_openness, freedom_imperative, property_rights, commons) is authored as a separate constraint with its own epsilon, beneficiary/victim structure, and claimed type, because measuring 'software control' through the lens of each reading yields a materially different extraction figure and a different victim set. This reading yields the lowest epsilon of the four because it is the only reading that structurally declares no victim class.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
