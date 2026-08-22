% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Scope — Three-Category Framework with Categorical Limiting Principles
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   Since 1937, federal commerce power has operated under a doctrine that
 *   permits regulation of channels of interstate commerce, instrumentalities
 *   and persons/things in commerce, and activities substantially affecting
 *   commerce — while purporting to cabin this power against becoming a
 *   general federal police power through categorical limiting principles. In
 *   practice, courts have applied the economic/noneconomic distinction
 *   inconsistently: aggregation theories sustained federal reach over
 *   homegrown wheat and marijuana cultivation, while similar reasoning was
 *   rejected for gun possession near schools and civil remedies for
 *   gender-motivated violence. The doctrine allocates enormous stakes — which
 *   sovereign gets to regulate what — through categories that are themselves
 *   contestable in application.
 *
 * KEY AGENTS:
 *   - federal_regulatory_agencies: institutional beneficiary/agenda_setter — collects expanded jurisdiction within the economic sphere
 *   - state_governments_retaining_traditional_authority: institutional beneficiary — retains family/criminal/education law authority via the noneconomic exclusion
 *   - national_market_participants: powerful beneficiary — gains uniform federal floor, can arbitrage jurisdiction
 *   - local_noneconomic_conduct_regulated_states: powerless payer — trapped by unpredictable recategorization
 *   - criminal_defendants_under_federalized_statutes: powerless payer — bears consequences of jurisdictional-hook manipulability
 *   - federal_judiciary: institutional agenda_setter — administers and polices the categorical line
 *   - constitutional_theorists_and_lower_courts: excluded moderate-power observer — documents doctrinal instability without deciding authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.48).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.42).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.48).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Scope — Three-Category Framework with Categorical Limiting Principles").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, '8cc1f081-20a9-4def-8ce7-9190009aba9f').
narrative_ontology:cs_kernel_codification('8cc1f081-20a9-4def-8ce7-9190009aba9f', formalized).
narrative_ontology:cs_authority_grounding('8cc1f081-20a9-4def-8ce7-9190009aba9f', lineage).
narrative_ontology:cs_interpretation_layer_present('8cc1f081-20a9-4def-8ce7-9190009aba9f').
narrative_ontology:cs_reading_relation('8cc1f081-20a9-4def-8ce7-9190009aba9f', commerce_clause_scope__narrow_originalist, influences).
narrative_ontology:cs_reading_relation('8cc1f081-20a9-4def-8ce7-9190009aba9f', commerce_clause_scope__broad_effects_test, influences).
narrative_ontology:cs_axiom('8cc1f081-20a9-4def-8ce7-9190009aba9f', foundational, categorical_limits_are_judicially_enforceable).
narrative_ontology:cs_axiom_status(categorical_limits_are_judicially_enforceable, holdable).
narrative_ontology:cs_axiom_grounding('8cc1f081-20a9-4def-8ce7-9190009aba9f', categorical_limits_are_judicially_enforceable, conventional).
narrative_ontology:cs_axiom('8cc1f081-20a9-4def-8ce7-9190009aba9f', foundational, economic_activity_admits_aggregation_noneconomic_does_not).
narrative_ontology:cs_axiom_status(economic_activity_admits_aggregation_noneconomic_does_not, holdable).
narrative_ontology:cs_axiom_grounding('8cc1f081-20a9-4def-8ce7-9190009aba9f', economic_activity_admits_aggregation_noneconomic_does_not, empirically_contingent).
narrative_ontology:cs_reference_frame('8cc1f081-20a9-4def-8ce7-9190009aba9f', post_1937_jurisdictional_rebalancing).
narrative_ontology:cs_drift_state('8cc1f081-20a9-4def-8ce7-9190009aba9f', post_lopez_morrison_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8cc1f081-20a9-4def-8ce7-9190009aba9f', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_regulatory_agencies).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_governments_retaining_traditional_authority).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, national_market_participants).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, local_noneconomic_conduct_regulated_states).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, criminal_defendants_under_federalized_statutes).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, litigants_facing_unpredictable_categorical_line_drawing).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, enumerated_powers_doctrine).
narrative_ontology:constraint_vindicates(commerce_clause_scope__intermediate_channels, dual_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and enforce statutes reaching economic activity substantially affecting interstate commerce, including aggregated small-scale conduct. Benefit from the broad economic-activity prong while accepting doctrinal constraint at the categorical boundary; litigate to keep contested statutes inside the economic characterization.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_regulatory_agencies, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__intermediate_channels, federal_regulatory_agencies, agenda_setter).

% Retain primary regulatory authority over family law, general criminal law, and education because the noneconomic/jurisdictional-element limits exclude federal reach absent an interstate nexus. Depend on courts continuing to enforce the economic/noneconomic line to preserve this domain; lose ground whenever a court characterizes disputed conduct as economic.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, state_governments_retaining_traditional_authority, beneficiary,
    institutional, generational, constrained, national).

% Businesses operating across state lines benefit from a uniform federal regulatory floor over genuinely interstate economic conduct, reducing the cost of navigating fifty divergent state regimes. Can often structure operations to invoke or avoid federal jurisdiction depending on which regime is more favorable.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, national_market_participants, beneficiary,
    powerful, biographical, mobile, national).

% Individuals and small local actors whose conduct is noneconomic but gets swept into federal statutes when courts stretch the jurisdictional-element or channels/instrumentalities categories to reach it. They cannot predict in advance which local conduct will be recharacterized as within federal reach, and have no forum to contest the categorization until prosecuted or regulated.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, local_noneconomic_conduct_regulated_states, payer,
    powerless, biographical, trapped, local).

% Face federal prosecution for conduct (e.g., possession, local violence, or local trafficking) that Congress reached by attaching a jurisdictional hook or asserting an aggregation theory. Bear the practical cost of the doctrine's manipulability: whether their conduct is 'economic' or has the requisite nexus is frequently outcome-determinative and unpredictable ex ante.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, criminal_defendants_under_federalized_statutes, payer,
    powerless, immediate, trapped, local).

% Parties challenging or defending federal statutes bear the transaction cost of litigating under a framework whose central categories (economic vs. noneconomic, substantial vs. attenuated effect) lack a stable, non-manipulable test. Repeated relitigation of the boundary itself, rather than settled application, consumes resources on all sides.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, litigants_facing_unpredictable_categorical_line_drawing, payer,
    moderate, biographical, constrained, national).

% Administers the three-category framework and its limiting principles, deciding case by case whether conduct is channel, instrumentality, or substantially-affecting economic activity, and whether aggregation or a jurisdictional element applies. Holds the discretion that determines which conduct falls to state or federal authority, but is bound by its own precedent chain and by the categorical structure it must keep coherent.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Academics and lower courts who observe that the economic/noneconomic distinction and the attenuated-causal-chain limit are not self-applying rules but contestable characterizations that can be manipulated by framing. They are not the deciding authority and their critiques of doctrinal incoherence rarely change outcomes in a given case, only accumulate as commentary.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__intermediate_channels, constitutional_theorists_and_lower_courts, excluded,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_scope__intermediate_channels, diffuse).
narrative_ontology:fixing_cost_class(commerce_clause_scope__intermediate_channels, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates regulatory jurisdiction between federal and state governments by sorting conduct into channels of commerce, instrumentalities/persons/things in commerce, and activities substantially affecting commerce — while reserving noneconomic local conduct (family law, general criminal law, education) to the states absent a jurisdictional nexus. Solves the genuine problem of a national economy requiring some uniform floor without collapsing all governance into a single federal sovereign.
% TRANSFER_FUNCTION: Moves regulatory authority (and the practical consequences of being regulated — federal criminal exposure, federal civil liability, compliance costs) from local/state jurisdiction to federal jurisdiction whenever conduct is characterized as economic or as possessing a qualifying nexus; moves it back to the states when conduct is characterized as noneconomic and nexus-free. The categorization act itself is the transfer mechanism.
% ABSENT_VOICES: Constitutional theorists and lower courts who have long identified that the economic/noneconomic distinction is not a natural kind but a judicially constructed and manipulable line are not the deciding authority; their critiques surface in dissents and scholarship but do not constrain the framework's application in individual cases. Defendants swept in by aggregation or jurisdictional-hook theories rarely have standing to challenge the doctrinal structure itself, only its application to their case.
% DISAPPEARANCE_RATIONALE: If this three-category framework with its limiting principles disappeared, federal power would either collapse to the narrow originalist reading (dramatically shrinking federal regulatory reach into economic and social life) or expand to the broad effects-test reading (eliminating meaningful state-reserved domains). Either direction would immediately reallocate enormous swaths of regulatory and prosecutorial authority between federal and state governments — this is not a background fact but the operative boundary of the federal system.
% FOUNDING_PROBLEM: The doctrine was built to reconcile two commitments in tension: a national economy that requires some uniform federal floor to function, and a constitutional structure of enumerated federal powers that was supposed to leave a meaningful residuum of authority to the states. The three-category test with categorical limits was constructed to prevent the Necessary and Proper/Commerce Clause combination from becoming a general federal police power.
% FOUNDING_PROBLEM_CORROBORATION: The framework's own architects (the judiciary) attest the limiting principles remain necessary to prevent commerce power from becoming unlimited. Independent constitutional scholars outside the federal government's own interest in expansive jurisdiction have documented, across decades of commentary, that the economic/noneconomic line has been applied inconsistently case to case (compare cases sustaining aggregation for wheat and marijuana cultivation against cases striking gun-possession and gender-violence statutes), suggesting the categorical limits function as much as post-hoc rationalization for results reached on other grounds as they do as a predictable, non-manipulable rule. This corroboration comes from academic commentary and dissenting judicial opinions, not from any party who benefits from either an expansive or a narrow reading.
narrative_ontology:disappearance_verdict(commerce_clause_scope__intermediate_channels, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__intermediate_channels, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__intermediate_channels, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_scope__intermediate_channels, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__intermediate_channels, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at medium (0.48) because the framework genuinely constrains federal reach in a meaningful fraction of cases (unlike the broad_effects_test sibling) while still permitting substantial federal intrusion into what would, under narrow_originalist, remain exclusively state or private conduct. Suppression is moderate (0.42): the doctrine does not suppress dissenting readings through coercion so much as through structural asymmetry — only the federal judiciary can authoritatively resolve where the line falls, and litigants bear the cost of that resolution being unpredictable case to case. Theater ratio rises over the interval (0.15 to 0.38) because the categorical limiting principles increasingly function as post-hoc rationalization vocabulary applied to preserve or reach a desired result, rather than as an ex ante predictive test — the vocabulary of 'economic,' 'jurisdictional element,' and 'attenuated chain' persists and is cited, but its application has become more clearly outcome-driven over time, per the accumulating scholarly record.
 *
 * PERSPECTIVAL GAP:
 *   From the federal agency and national market participant seats, this doctrine reads as a workable, real accommodation — a Rope solving the genuine problem of national economic coordination while respecting some state prerogative. From the seat of a defendant or locally-regulated party swept in by an aggregation or jurisdictional-hook theory, the same doctrine reads as an enforced, unpredictable extraction of state or personal autonomy dressed in the vocabulary of principled limitation. The engine should register this seat divergence directly from the structural data: the categorical limits function as real constraint from some angles and as manipulable cover from others, and both readings can be simultaneously true of the same doctrinal text depending on which case and which era is examined.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulatory agencies and national market participants sit near the beneficiary end: they collect expanded, more predictable jurisdiction within the economic sphere and can often structure their conduct or advocacy to land inside the favorable category. State governments retaining traditional authority also benefit, but their benefit is conditional and precarious — it depends on courts continuing to enforce the noneconomic exclusion, which the doctrine itself does not guarantee will hold. Local noneconomic actors and criminal defendants sit near the target end: they are trapped (no realistic ability to litigate the categorical boundary proactively) and bear the cost when the line is drawn against them, often without notice that their conduct was at doctrinal risk. Litigants generally bear a diffuse, symmetric-leaning cost from doctrinal unpredictability itself, independent of which side ultimately wins.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing commerce power from becoming a general federal police power while still permitting a functional national economic floor — remains genuinely live in the sense that the underlying federalism tension has not disappeared; it is not a case of an obsolete mandate being defended purely by inertia. However, the specific categorical apparatus (economic/noneconomic distinction, jurisdictional element, no-attenuated-chains rule) shows signs of having drifted from a predictive test toward a post-hoc justificatory vocabulary, per the rising theater_ratio series and the documented inconsistency across cases with structurally similar aggregation logic. This is best classified as tangled_rope rather than snare or piton: the coordination function (allocating jurisdiction between two levels of government in a national economy) is real and necessary, the beneficiary/victim asymmetry is real and identifiable, and active enforcement (ongoing judicial administration of the line) is required to sustain it — but the coordination is not a cover story; it addresses an actual structural problem that narrow_originalist would leave unsolved and broad_effects_test would dissolve by eliminating the boundary altogether.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_noneconomic_line_stability,
    'Is the economic/noneconomic distinction a real, non-manipulable jurisprudential category, or is it a post-hoc label applied to reach results driven by other (unstated) considerations?',
    'Systematic empirical coding of commerce clause cases across decades, testing whether the economic/noneconomic characterization correlates more strongly with case outcome or with independently identifiable features of the conduct itself (interstate movement, market participation, etc.) that would predict the characterization ex ante.',
    'If the line is shown to be predictively unstable (correlates with outcome, not with independent conduct features), the doctrine''s claimed coordination function collapses toward pure discretion dressed as principle, pushing the classification toward snare; if the line is shown to track independently identifiable conduct features reliably, the tangled_rope classification with genuine coordination is strongly supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_noneconomic_line_stability, empirical, 'Whether the central limiting distinction is a real test or a manipulable label.').

omega_variable(
    kernel_reading_selection,
    'Is the intermediate_channels reading the doctrinally dominant/controlling reading of the commerce_clause_scope kernel, or is it one contested reading among the narrow_originalist and broad_effects_test alternatives that could displace it with a shift in judicial composition?',
    'Track the plurality/majority holdings across the relevant case line and whether categorical limiting principles have been reaffirmed, narrowed, or abandoned in subsequent controlling opinions.',
    'If intermediate_channels remains the controlling reading, this story''s structural data describes the operative constraint on federal power. If a doctrinal shift toward either sibling reading occurs, the beneficiary/victim structure and extractiveness values authored here would need to be re-derived for the newly-controlling reading rather than amended in place — per the ε-invariance principle, a shift in controlling reading is a shift to a different constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Which kernel reading currently controls and how stable that control is.').

omega_variable(
    aggregation_scope_manipulability,
    'Does the rule confining aggregation to economic activity provide a genuinely stable limiting principle, or can ''economic activity'' be defined broadly enough after the fact to admit whatever aggregation result is desired?',
    'Comparative analysis of aggregation holdings (e.g., wheat cultivation, drug cultivation for personal use) against non-aggregation holdings (e.g., gun possession, gender-violence civil remedies) to identify whether a coherent, articulable principle distinguishes the two lines beyond the labeled conclusion itself.',
    'Confirms or undermines the coordination-function claim underlying the tangled_rope classification; a finding of manipulability without a coherent distinguishing principle would support reclassifying this as closer to a snare wearing coordination-language cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_scope_manipulability, empirical, 'Whether the aggregation-for-economic-activity-only rule is a real constraint or an empty formalism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 1937, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__intermediate_channels, theater_ratio, 1937, 0.15).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1964, commerce_clause_scope__intermediate_channels, theater_ratio, 1964, 0.2).
narrative_ontology:measurement_basis(comm_tr_t1964, observed).
narrative_ontology:measurement(comm_tr_t1985, commerce_clause_scope__intermediate_channels, theater_ratio, 1985, 0.28).
narrative_ontology:measurement_basis(comm_tr_t1985, observed).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__intermediate_channels, theater_ratio, 1995, 0.34).
narrative_ontology:measurement_basis(comm_tr_t1995, observed).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_scope__intermediate_channels, theater_ratio, 2005, 0.36).
narrative_ontology:measurement_basis(comm_tr_t2005, observed).
narrative_ontology:measurement(comm_tr_t2015, commerce_clause_scope__intermediate_channels, theater_ratio, 2015, 0.37).
narrative_ontology:measurement_basis(comm_tr_t2015, observed).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_scope__intermediate_channels, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(comm_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__intermediate_channels, base_extractiveness, 1937, 0.3).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1964, commerce_clause_scope__intermediate_channels, base_extractiveness, 1964, 0.38).
narrative_ontology:measurement_basis(comm_be_t1964, observed).
narrative_ontology:measurement(comm_be_t1985, commerce_clause_scope__intermediate_channels, base_extractiveness, 1985, 0.44).
narrative_ontology:measurement_basis(comm_be_t1985, observed).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__intermediate_channels, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement_basis(comm_be_t1995, observed).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_scope__intermediate_channels, base_extractiveness, 2005, 0.46).
narrative_ontology:measurement_basis(comm_be_t2005, observed).
narrative_ontology:measurement(comm_be_t2015, commerce_clause_scope__intermediate_channels, base_extractiveness, 2015, 0.47).
narrative_ontology:measurement_basis(comm_be_t2015, observed).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_scope__intermediate_channels, base_extractiveness, 2025, 0.48).
narrative_ontology:measurement_basis(comm_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(commerce_clause_scope__intermediate_channels, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__intermediate_channels, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the commerce_clause_scope kernel. commerce_clause_scope__narrow_originalist authors the reading confining federal power to facilitating interstate trade (minimal federal reach, maximal state autonomy, low federal extractiveness). commerce_clause_scope__broad_effects_test authors the reading permitting federal reach to any economic activity with aggregate substantial effects (maximal federal reach, minimal state autonomy, high federal extractiveness). This story (intermediate_channels) sits structurally between them: moderate federal extractiveness (0.48), a moderate and unstable victim set, and a beneficiary structure that splits between federal and state institutions depending on how conduct is categorized. Each reading has a distinct ε and distinct beneficiary/victim structure by design — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
