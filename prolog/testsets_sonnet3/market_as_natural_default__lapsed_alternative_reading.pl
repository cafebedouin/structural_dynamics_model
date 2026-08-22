% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__lapsed_alternative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__lapsed_alternative_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: market_as_natural_default__lapsed_alternative_reading
 *   human_readable: Market Allocation as Presumed Default (Lapsed-Memory Reading)
 *   domain: political_economy/ideology_studies/economic_history
 *
 * SUMMARY:
 *   This story instantiates the 'lapsed alternative' reading of the
 *   market_as_natural_default kernel: the presumption that market allocation
 *   is the obvious institutional default is treated as a byproduct of
 *   historical forgetting rather than any active project of closure or
 *   extraction. Guild economies, commons management regimes, mutual-aid
 *   networks, and mid-century planning experiments once existed as contested,
 *   comparably legible alternatives; as those practices declined and
 *   curricular attention drifted, the market default came to be experienced
 *   as simply 'how things work,' not as the winner of an ongoing contest.
 *   This reading is deliberately distinct from the
 *   beneficiary_maintained_reading (which holds that incumbents actively
 *   defend the naturalization post-hoc) and the hybrid_amnesia_reading (which
 *   holds that initial lapse created the conditions later captured by
 *   beneficiaries). Each reading is authored as its own constraint with its
 *   own ε; this one keeps ε low (≤0.15) because, on its own terms, there is
 *   no active suppression apparatus and no identifiable class extracting rent
 *   from the amnesia itself.
 *
 * KEY AGENTS:
 *   - incumbent_market_participants: incidental beneficiary (organized/mobile) — gains from the drift without maintaining it
 *   - economic_historians: analytical observer (analytical/analytical) — holds the recovery mechanism
 *   - general_public: bears narrowed imagination (moderate/constrained) — absorbs the default without extraction
 *   - curriculum_and_media_institutions: agenda_setter by inertia (institutional/constrained) — shapes emphasis through drift, not design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__lapsed_alternative_reading, 0.1).
domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, 0.08).
domain_priors:theater_ratio(market_as_natural_default__lapsed_alternative_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__lapsed_alternative_reading, mountain).
narrative_ontology:human_readable(market_as_natural_default__lapsed_alternative_reading, "Market Allocation as Presumed Default (Lapsed-Memory Reading)").
narrative_ontology:topic_domain(market_as_natural_default__lapsed_alternative_reading, "political_economy/ideology_studies/economic_history").

domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__lapsed_alternative_reading, '7ade4be2-284c-4a91-955c-5c0fb9428bda').
narrative_ontology:cs_kernel_codification('7ade4be2-284c-4a91-955c-5c0fb9428bda', distributed).
narrative_ontology:cs_authority_grounding('7ade4be2-284c-4a91-955c-5c0fb9428bda', diffuse_epistemic).
narrative_ontology:cs_reading_relation('7ade4be2-284c-4a91-955c-5c0fb9428bda', market_as_natural_default__beneficiary_maintained_reading, coexists_with).
narrative_ontology:cs_reading_relation('7ade4be2-284c-4a91-955c-5c0fb9428bda', market_as_natural_default__hybrid_amnesia_reading, influences).
narrative_ontology:cs_axiom('7ade4be2-284c-4a91-955c-5c0fb9428bda', foundational, naturalization_is_unmaintained_drift).
narrative_ontology:cs_axiom_status(naturalization_is_unmaintained_drift, holdable).
narrative_ontology:cs_axiom_grounding('7ade4be2-284c-4a91-955c-5c0fb9428bda', naturalization_is_unmaintained_drift, empirically_contingent).
narrative_ontology:cs_axiom('7ade4be2-284c-4a91-955c-5c0fb9428bda', foundational, no_identifiable_extraction_from_amnesia_itself).
narrative_ontology:cs_axiom_status(no_identifiable_extraction_from_amnesia_itself, holdable).
narrative_ontology:cs_axiom_grounding('7ade4be2-284c-4a91-955c-5c0fb9428bda', no_identifiable_extraction_from_amnesia_itself, empirically_contingent).
narrative_ontology:cs_reference_frame('7ade4be2-284c-4a91-955c-5c0fb9428bda', plural_allocation_systems_contested_pre_lapse).
narrative_ontology:cs_drift_state('7ade4be2-284c-4a91-955c-5c0fb9428bda', contemporary_curricular_consensus, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ade4be2-284c-4a91-955c-5c0fb9428bda', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__lapsed_alternative_reading, incumbent_market_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(market_as_natural_default__lapsed_alternative_reading, general_public).
narrative_ontology:constraint_vindicates(market_as_natural_default__lapsed_alternative_reading, market_allocation_as_default_institutional_form).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate firms, hold capital, and transact within existing market institutions. Benefit incidentally from the fact that market allocation is treated as the obvious default rather than one historical option among several — this lowers the cost of legitimating their position, but they did not engineer the forgetting and would not need to actively defend it if historians revived the alternatives. Their advantage is a byproduct of collective amnesia, not a maintained project.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, incumbent_market_participants, beneficiary,
    organized, generational, mobile, national).

% Study guild systems, commons management, mutual-aid economies, planned allocation experiments, and other historical arrangements that once competed with market allocation. Their scholarship is the mechanism by which the lapsed alternatives could be recovered; nothing structurally prevents this research, but public attention and curricular emphasis have drifted away from it over generations.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% Encounter market allocation as simply 'how things work,' absorbing the presumption through schooling, media, and everyday transaction without ever being presented the historical menu of alternatives as live options. The cost borne is narrowed imagination, not extracted rent — no party is collecting from their unfamiliarity with the alternatives.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, general_public, payer,
    moderate, biographical, constrained, national).

% Set the default content of economic education and popular economic narrative. Their choices about what to include or omit are shaped by inherited convention and institutional inertia — a path-dependent drift, not a coordinated suppression campaign. They could, in principle, reintroduce alternative-allocation history without resistance from any organized opponent.
narrative_ontology:constraint_stakeholder(market_as_natural_default__lapsed_alternative_reading, curriculum_and_media_institutions, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Presenting market allocation as the default reduces the cognitive and coordination cost of everyday economic decision-making — nobody has to relitigate the choice-of-system question before every transaction.
% TRANSFER_FUNCTION: Nothing is transferred in this reading: there is no rent flow from a payer class to a beneficiary class. What moves is attention and curricular emphasis, drifting away from historical alternatives over successive generations, with no agent capturing value from that drift.
% ABSENT_VOICES: Advocates of guild, commons, or planned-allocation traditions are not in the room, but not because they were excluded by force — the relevant historical record simply receded from common curricula and popular narrative as those traditions ceased to be practiced or contested political options.
% DISAPPEARANCE_RATIONALE: If the presumption of market-as-default vanished overnight, historians and educators dispute whether much would actually change in the near term: the alternatives are not suppressed, only unfamiliar, so recovery would depend on renewed research and curricular attention rather than removal of any active barrier. Incumbent participants would see no material change since their position was never actively defended by this naturalization.
% FOUNDING_PROBLEM: Early market theorists and educators needed a simplified teaching heuristic and a workable default institutional description as competing historical allocation systems (guilds, commons regimes, wartime planning, mutual aid networks) declined in practice and were not renewed as live curricular options.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians outside any incumbent beneficiary group attest that the narrowing is a genuine artifact of curricular drift and declining practice of the alternatives, not an actively maintained exclusion; no corroborating source has identified an organized incumbent effort to prevent the alternatives' historical study, though some historians argue the amnesia is convenient for incumbents even if unintended.
narrative_ontology:disappearance_verdict(market_as_natural_default__lapsed_alternative_reading, contested).
narrative_ontology:founding_problem_status(market_as_natural_default__lapsed_alternative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__lapsed_alternative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__lapsed_alternative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__lapsed_alternative_reading, 0.1, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__lapsed_alternative_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, ExtMetricName, E),
    domain_priors:suppression_score(market_as_natural_default__lapsed_alternative_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(market_as_natural_default__lapsed_alternative_reading),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(market_as_natural_default__lapsed_alternative_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(market_as_natural_default__lapsed_alternative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.10 at interval end) because this reading denies any transfer mechanism — nothing is collected from the general public's unfamiliarity with historical alternatives. Suppression is low (0.08) because no active barrier prevents recovery of the alternatives; the accessibility_collapse figure (0.6) reflects that the alternatives have become practically obscure through disuse and curricular neglect even though no one is actively blocking their rediscovery — collapse-by-neglect, not collapse-by-suppression. Resistance is low (0.15) because there is no entrenched actor mobilizing against historical recovery efforts; where resistance exists it is institutional inertia, not opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the incumbent seat, market default is simply unremarkable common sense — there is nothing to defend. From the historian's analytical seat, the same default is visible as one contingent institutional path among several that receded from view. Neither seat experiences active suppression; the divergence is in salience, not in coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent market participants are declared beneficiaries because they gain from reduced legitimation cost, but the derivation should place them only modestly toward the beneficiary end of directionality — their gain is incidental and would not motivate active defense if the historical record were revived, consistent with the reading's no-identifiable-beneficiary-class thesis at the extraction level even though a benefiting group exists nominally. The general public bears the cost of narrowed imagination but this is not extraction in the classical sense — hence a payer role with low authored extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview is central here: the original 'problem' curricula and market theorists solved (a simplifying default amid declining alternative practices) has drifted rather than resolved or been repurposed for extraction. Because no party is shown actively re-purposing the mandate for rent capture, this reading resists being classified as mandatrophy in the tangled_rope or snare sense — it is a case of institutional drift without capture, which is exactly the structural delta this reading is meant to isolate from its siblings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lapse_vs_maintenance_ambiguity,
    'Is the persistence of market-as-default genuinely explained by unmaintained historical forgetting, or does apparent ''lapse'' conceal low-visibility maintenance activity (subtle curricular gatekeeping, funding patterns favoring market-friendly economic history) that would make this actually the beneficiary_maintained_reading in disguise?',
    'Archival and institutional-funding analysis of economics curricula and economic-history publishing over the relevant period: does funding or editorial selection systematically disfavor alternative-allocation scholarship, or does its decline track independent factors (decline of the practices themselves, general historiographical fashion)?',
    'If low-visibility maintenance is found, this constraint should be reclassified toward the beneficiary_maintained_reading or hybrid_amnesia_reading, with correspondingly higher extractiveness and suppression; if no such maintenance is found, the lapsed_alternative_reading''s low-ε profile is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lapse_vs_maintenance_ambiguity, empirical, 'Whether the apparent absence of active maintenance is real or merely undetected.').

omega_variable(
    kernel_reading_boundary_location,
    'Where exactly does the lapsed_alternative_reading''s premise stop being distinguishable from the hybrid_amnesia_reading''s premise, given that both agree an initial lapse occurred?',
    'Track whether any agent''s directionality shifts toward extraction over the story''s interval (this constraint''s own measurements show extractiveness rising only marginally, 0.06 to 0.10, over 60 time units) — a reading in which that rise accelerates and a beneficiary class becomes identifiable would have crossed into hybrid_amnesia_reading territory.',
    'Clarifies the boundary condition between two sibling readings sharing a genealogical premise (lapse) but diverging on whether capture subsequently occurs; without this, the two readings risk being indistinguishable at low ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'The structural boundary between the pure-lapse reading and the hybrid lapse-then-capture reading.').

omega_variable(
    recoverability_of_alternatives,
    'Are the historical alternatives (guild systems, commons regimes, mutual-aid economies, planning experiments) genuinely recoverable through historical research and renewed curricular attention, or has enough institutional and material infrastructure for those practices disappeared that ''recovery'' would require reconstruction rather than mere remembering?',
    'Comparative case study of jurisdictions or movements that have attempted to revive commons-based or mutual-aid allocation mechanisms in the present, assessing whether historical memory alone was sufficient or whether new institutional infrastructure had to be built from scratch.',
    'If recovery requires substantial reconstruction rather than mere memory-retrieval, the accessibility_collapse figure authored here (0.6) understates the practical difficulty, which would push this reading''s profile closer to a mountain-like structural claim rather than a reversible ideational drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recoverability_of_alternatives, empirical, 'Whether ''lapsed memory'' alternatives are practically recoverable or only theoretically so.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__lapsed_alternative_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mark_tr_t12, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 12, 0.07).
narrative_ontology:measurement(mark_tr_t24, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(mark_tr_t36, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 36, 0.09).
narrative_ontology:measurement(mark_tr_t48, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 48, 0.11).
narrative_ontology:measurement(mark_tr_t60, market_as_natural_default__lapsed_alternative_reading, theater_ratio, 60, 0.12).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(mark_be_t12, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 12, 0.07).
narrative_ontology:measurement(mark_be_t24, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 24, 0.08).
narrative_ontology:measurement(mark_be_t36, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 36, 0.09).
narrative_ontology:measurement(mark_be_t48, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 48, 0.1).
narrative_ontology:measurement(mark_be_t60, market_as_natural_default__lapsed_alternative_reading, base_extractiveness, 60, 0.1).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(market_as_natural_default__lapsed_alternative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__lapsed_alternative_reading, information_standard).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__lapsed_alternative_reading, 0.03).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__beneficiary_maintained_reading).
narrative_ontology:affects_constraint(market_as_natural_default__lapsed_alternative_reading, market_as_natural_default__hybrid_amnesia_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the market_as_natural_default kernel, decomposed per the ε-invariance principle because the natural-language claim ('market dominance is naturalized') conflates structurally distinct genealogies with very different extraction profiles. This reading (lapsed_alternative_reading) authors ε ≤ 0.15 and no identifiable capturing beneficiary class. beneficiary_maintained_reading authors substantially higher ε and suppression, reflecting active post-hoc defense by incumbents. hybrid_amnesia_reading authors a rising ε trajectory reflecting a two-phase process: initial lapse followed by beneficiary capture of the resulting vacuum. All three should be read as competing genealogical hypotheses about the same surface phenomenon, not as three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
