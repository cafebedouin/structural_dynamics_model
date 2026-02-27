% ============================================================================
% CONSTRAINT STORY: legal_formalism_overhang
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_formalism_overhang, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legal_formalism_overhang
 *   human_readable: The Ghost of Statutes Past: Legal Formalism Overhang
 *   domain: political/legal/social
 *
 * SUMMARY:
 *   Legal formalism — the doctrine that law means what its text literally
 *   says, interpreted according to original intent or plain language —
 *   creates a structural constraint when the social, technological, or
 *   economic context that gave the statute meaning has vanished. A statute
 *   regulating horse-drawn carriages becomes a formalist trap when applied to
 *   automobiles; a tax code written for industrial manufacturing creates
 *   extraction barriers for digital service providers; labor laws designed
 *   for factory floors become outdated constraints on remote work. The
 *   constraint exhibits all six DR types from different perspectives: pure
 *   coordination for courts (rope), temporary problem for reformers
 *   (scaffold), degraded ritual for the profession itself (piton), natural
 *   law for the formalist philosopher (false mountain), mixed
 *   extraction-coordination for entrepreneurs (tangled rope), and pure
 *   extraction for innovators with no exit (snare). The constraint's
 *   theater_ratio (0.68) reflects that statutory formalism increasingly
 *   performs its own legitimacy rather than delivers predictability — as
 *   contexts diverge further from original intent, formalist judges must
 *   strain to reach plausible conclusions, producing theater: the appearance
 *   of being bound by text while actually making discretionary choices.
 *
 * KEY AGENTS:
 *   - Innovators and Adaptive Firms: Primary victim (powerless/trapped) — face compliance costs and legal risk with no exit; bear full extraction
 *   - Adaptive Entrepreneurs: Secondary victim (moderate/constrained) — some workaround capacity; experience mixed coordination benefit and extraction
 *   - Legal Profession and Courts: Primary beneficiary (institutional/arbitrage) — experience formalism as pure coordination mechanism enabling predictability and professional legitimacy; can arbitrage between interpretative schools
 *   - Reform Coalition: Organized agents (organized/constrained) — legislators, statutory modernization movements, adaptive jurisprudence scholars; see constraint as temporary with clear sunset
 *   - Institution of Statutory Interpretation: Institutional actor (institutional/arbitrage) — maintains formalist ritual through professional inertia; perceives own process as degraded (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent interpretative methodology as inherent feature of law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_formalism_overhang, 0.38).
domain_priors:suppression_score(legal_formalism_overhang, 0.52).
domain_priors:theater_ratio(legal_formalism_overhang, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_formalism_overhang, extractiveness, 0.38).
narrative_ontology:constraint_metric(legal_formalism_overhang, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(legal_formalism_overhang, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_formalism_overhang, tangled_rope).
narrative_ontology:human_readable(legal_formalism_overhang, "The Ghost of Statutes Past: Legal Formalism Overhang").
narrative_ontology:topic_domain(legal_formalism_overhang, "political/legal/social").

domain_priors:requires_active_enforcement(legal_formalism_overhang).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_formalism_overhang, legal_practitioners).
narrative_ontology:constraint_beneficiary(legal_formalism_overhang, institutional_status_quo).
narrative_ontology:constraint_victim(legal_formalism_overhang, adaptive_innovation).
narrative_ontology:constraint_victim(legal_formalism_overhang, resource_efficiency).
narrative_ontology:constraint_victim(legal_formalism_overhang, social_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTRAINED INNOVATOR (SNARE) — Individuals or organizations attempting adaptive innovation face legal compliance costs with no escape. The formalism creates a compliance tax on new practices. Cannot exit without massive legal risk or relocation. Maximum experienced extraction.
constraint_indexing:constraint_classification(legal_formalism_overhang, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADAPTIVE ENTREPRENEUR (TANGLED ROPE) — Some capacity to navigate or adapt to formalism through expertise, legal workarounds, or selective enforcement; also benefits from the coordination function of stable legal structures. Mixed: extraction through compliance burden, but coordination benefit from legal stability.
constraint_indexing:constraint_classification(legal_formalism_overhang, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LEGAL PROFESSION AND COURTS (ROPE) — Institutional beneficiaries who experience the constraint as pure coordination. Stable legal texts enable precedent, predictability, and professional legitimacy. Can arbitrage between jurisdictions and interpretative schools. Net beneficiary; extraction runs toward this agent.
constraint_indexing:constraint_classification(legal_formalism_overhang, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM COALITION (SCAFFOLD) — Organized actors (legislators, reform movements, advocacy groups) perceive the formalism as a temporary coordination failure with a clear sunset: legislative modernization, recodification, and statutory interpretation reform. Low effective extraction because this coalition has agency and sees an explicit exit path (sunset legislation, statutory amendment).
constraint_indexing:constraint_classification(legal_formalism_overhang, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATUTORY INTERPRETATION RITUAL (PITON) — The formalist methodology (plain language reading, original intent, strict construction) persists through professional and institutional inertia long after its rationale has degraded. Courts maintain the ritual despite its low functional verification of actual legislative intent or social utility. Theater ratio reflects that formalist interpretation is largely performative — judges often reach predetermined conclusions justified by formalist rhetoric rather than driven by text.
constraint_indexing:constraint_classification(legal_formalism_overhang, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some formalism in legal interpretation is inherent to the rule of law: written statutes must have relatively stable meaning across time, or predictability collapses. This perspective sees the formalist overhang as an immutable property of any text-based legal system. However, the structural data contradicts the mountain classification — the engine will identify this as a false summit, revealing that the 'inherent to law' framing naturalizes what is actually a contingent institutional arrangement about interpretation methodology.
constraint_indexing:constraint_classification(legal_formalism_overhang, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_formalism_overhang_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legal_formalism_overhang, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legal_formalism_overhang, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(legal_formalism_overhang, TR),
    TR >= 0.70.

:- end_tests(legal_formalism_overhang_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The legal formalism overhang extracts through compliance burden and innovation delay, but the extraction is not total — most economies remain functional despite formalist drag. The value reflects that formalism raises barriers without completely blocking adaptation. Many innovators find workarounds (regulatory arbitrage, jurisdictional shopping, interpretative expansion). Suppression (0.52): Moderate. Barriers exist but are not total: statutory amendment is possible (though slow), purposive interpretation can stretch formalism's boundaries, and jurisdictional competition creates pressure for reform. International trade and federal structures create exit options for some actors. Theater ratio (0.68): High. As contexts diverge from original statutory intent, formalist interpretation increasingly requires rhetorical stretching to reach defensible conclusions. Courts apply formalist methodology while making purposive choices and justifying them retroactively. The gap between formalist rhetoric and formalist reasoning widens as obsolescence increases — classic Goodhart drift where the metric (adherence to text) substitutes for the goal (predictable, just law).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a wide perspectival gap between institutional beneficiaries and powerless victims. The legal profession sees coordination — formalist methodology enables precedent, predictability, and professional legitimacy. The constrained innovator sees pure extraction — compliance burden with no escape. The reform coalition sees a temporary problem being solved — statutory modernization is underway. The profession sees its own interpretation ritual as increasingly performative (piton). The analytical observer risks naturalizing formalism as an inherent feature of any rule-of-law system, but the structural data reveals it as a contingent choice about interpretation methodology. The perspectives split along lines of institutional power and exit options: those with institutional leverage or options to exit see coordination or temporality; those trapped see extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this constraint is structured by structural position relative to the formalism itself. Legal professionals and courts benefit from formalism's predictability and institutional legitimacy — their d value (0.05-0.15) produces negative or low f(d), revealing they experience the constraint as coordination, not extraction. Innovators and constrained firms are targets of compliance requirements with no exit — their d value (0.80-0.95) produces high f(d), revealing maximum experienced extraction. Organized reform coalitions occupy intermediate d (0.40-0.50) reflecting that they have some exit options (legislative amendment, jurisdictional exit) but are still constrained by formalism's institutional inertia. The beneficiary/victim asymmetry is fundamental: legal stability benefits institutional actors; formalist rigor becomes extraction for those attempting adaptive innovation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification resolves the mandatrophy by identifying both coordination and asymmetric extraction functions. Formalism provides genuine coordination benefit: stable text, precedent, predictability for legal institutions (rope function). Simultaneously, formalism creates extraction barriers for innovators adapting to changed contexts (snare function). The hybrid emerges because the coordination mechanism (literal text stability) becomes extractive precisely when the context changes — the same text that enabled coordination in the original context now enforces compliance in a radically different context. This is the structural signature of tangled rope: one mechanism serving two incompatible functions. Reform (Scaffold perspective) sees this as temporary — statutory modernization, recodification, and purposive interpretation will gradually replace pure formalism, creating a sunset for the extractive layer while preserving the coordination layer through updated statutes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_tractability,
    'Is legislative intent for historical statutes recoverable with sufficient reliability to justify formalist interpretation methods?',
    'Comparative analysis of formalist vs purposive interpretation outcomes; historical documentation studies of legislative intent for 50+ statutes; correlation between formalist methodology and actual legislative purpose',
    'If original intent is recoverable: formalism provides genuine coordination function. If not recoverable: formalism is performative theater masking discretionary choice, increasing extraction classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_intent_tractability, empirical, 'Whether legislative intent can be recovered reliably enough to justify formalist interpretation').

omega_variable(
    statutory_obsolescence_threshold,
    'What social/technological change threshold triggers statutory obsolescence severe enough to justify adaptive interpretation or repeal?',
    'Historical case studies of repealed statutes; sociological analysis of technology adoption curves vs legal modernization timelines; identification of specific context changes that made formalist reading harmful',
    'If threshold < 10 years: many statutes become extractive traps quickly, snare classification strengthens. If threshold > 30 years: formalism persists as coordinating force longer, rope perspective dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statutory_obsolescence_threshold, conceptual, 'Timeline threshold for when statutes become sufficiently obsolete to require modernization').

omega_variable(
    interpretive_discretion_containment,
    'Does formalist methodology actually constrain judicial discretion, or does it provide rhetorical cover for discretionary choices already made?',
    'Comparative study of formalist vs purposive jurisdictions; analysis of judicial outcomes when formalist logic would produce absurd results; identification of cases where formalist rhetoric diverges from formalist conclusion',
    'If formalism constrains discretion: theater ratio lower, rope coordination benefit stronger. If formalism masks discretion: theater ratio higher, piton classification confirmed, extraction increases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_discretion_containment, empirical, 'Whether formalist methodology actually constrains judicial discretion or provides rhetorical cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_formalism_overhang, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legform_tr_t0, legal_formalism_overhang, theater_ratio, 0, 0.42).
narrative_ontology:measurement(legform_tr_t25, legal_formalism_overhang, theater_ratio, 25, 0.58).
narrative_ontology:measurement(legform_tr_t50, legal_formalism_overhang, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(legform_be_t0, legal_formalism_overhang, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(legform_be_t25, legal_formalism_overhang, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(legform_be_t50, legal_formalism_overhang, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_formalism_overhang, information_standard).
narrative_ontology:affects_constraint(legal_formalism_overhang, regulatory_arbitrage).
narrative_ontology:affects_constraint(legal_formalism_overhang, jurisdictional_exit_capacity).
narrative_ontology:affects_constraint(legal_formalism_overhang, statutory_obsolescence_lag).

% DUAL FORMULATION NOTE:
% Legal formalism overhang is upstream of specific statutory obsolescence problems but represents a distinct structural constraint about interpretation methodology. Specific obsolete statutes (horse-and-buggy laws, pre-digital tax codes) are downstream manifestations. The formalism itself — the methodology of interpretation — is the constraint. Decomposition tracks how formalist methodology creates extraction barriers differently in different statutory domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
