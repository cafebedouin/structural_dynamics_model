% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__thinkability_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: IP Category Emergence: Ownable Expression as Conceptual Coherence (1710)
 *   domain: legal_philosophy/intellectual_property/jurisprudence
 *
 * SUMMARY:
 *   In 1710, English common law articulated a new legal category: 'copy
 *   right,' the ownable interest in a published text, distinct from the
 *   physical printing block (property in goods) or the crown's monopoly grant
 *   (prerogative). Before 1710, disputes over printing rights existed —
 *   stationers claimed control, authors occasionally claimed compensation,
 *   the Crown granted monopolies — but the legal system lacked a coherent
 *   vocabulary to express these claims as a coherent kind of ownership. The
 *   Statute of Anne codified 'copy right' as a distinct property interest
 *   tied to authorship and publication, making 'ownable expression' thinkable
 *   as a category. This is the thinkability reading: the constraint is the
 *   emergence of this conceptual space, not the first entry of a particular
 *   actor (author or stationer) into rights-holding, nor the enforcement
 *   machinery protecting those rights, but the conceptual coherence that made
 *   'copy right' a discussable legal object. The claim/metric independence
 *   rule applies: the constraint is CLAIMED as a mountain (a natural fact of
 *   legal concept formation — expressing abstract ownership becomes thinkable
 *   once the category exists) while the authored metrics describe measurable
 *   extraction (the category benefits stationers, suppresses alternative
 *   framings, carries theater ratio as the concept is invoked to justify
 *   institutional interests). The engine measures that divergence; we do not
 *   reconcile the claim to the metrics.
 *
 * KEY AGENTS:
 *   - common_law_jurists: gain conceptual tools to articulate IP disputes; institutional power but analytical motivation
 *   - stationer_guild_members: gain new legal grounding for their monopoly; organized power, strong interest in preserving the category once emerged
 *   - crown_authority: controls the moment of codification (Statute of Anne); institutional power, prerogative interest
 *   - authors: nominally granted rights but excluded from institutional control; moderate power, trapped by the category's definition
 *   - reading_public: observers of the emerging category; powerless, constrained exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.31).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.19).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.19).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, mountain).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "IP Category Emergence: Ownable Expression as Conceptual Coherence (1710)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/jurisprudence").

domain_priors:emerges_naturally(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, 'bdcb3257-3909-4283-87bc-55a4d22ac647').
narrative_ontology:cs_kernel_codification('bdcb3257-3909-4283-87bc-55a4d22ac647', formalized).
narrative_ontology:cs_authority_grounding('bdcb3257-3909-4283-87bc-55a4d22ac647', lineage).
narrative_ontology:cs_interpretation_layer_present('bdcb3257-3909-4283-87bc-55a4d22ac647').
narrative_ontology:cs_reading_relation('bdcb3257-3909-4283-87bc-55a4d22ac647', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('bdcb3257-3909-4283-87bc-55a4d22ac647', ip_category_emergence__synchronic_diachronic_seam, coexists_with).
narrative_ontology:cs_axiom('bdcb3257-3909-4283-87bc-55a4d22ac647', foundational, conceptual_coherence_precedes_enforcement).
narrative_ontology:cs_axiom_status(conceptual_coherence_precedes_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('bdcb3257-3909-4283-87bc-55a4d22ac647', conceptual_coherence_precedes_enforcement, deontological).
narrative_ontology:cs_axiom('bdcb3257-3909-4283-87bc-55a4d22ac647', secondary, ownership_of_intangibles_requires_epistemic_frame).
narrative_ontology:cs_axiom_status(ownership_of_intangibles_requires_epistemic_frame, holdable).
narrative_ontology:cs_axiom_grounding('bdcb3257-3909-4283-87bc-55a4d22ac647', ownership_of_intangibles_requires_epistemic_frame, empirically_contingent).
narrative_ontology:cs_reference_frame('bdcb3257-3909-4283-87bc-55a4d22ac647', pre_1710_vocational_monopoly_frame).
narrative_ontology:cs_drift_state('bdcb3257-3909-4283-87bc-55a4d22ac647', post_1710_property_rights_frame, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('bdcb3257-3909-4283-87bc-55a4d22ac647', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, common_law_jurists).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, stationer_guild_members).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, ExtMetricName, E),
    domain_priors:suppression_score(ip_category_emergence__thinkability_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(ip_category_emergence__thinkability_reading),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.31 at 1710, rising slightly by 1750) because the constraint measures conceptual emergence, not extraction from a particular agent to another, but stationers and jurists do benefit institutionally from the coherence the category provides. Suppression is similarly modest (0.19 at 1710) because the constraint does not require active coercive enforcement — suppression takes the form of epistemic closure (the new vocabulary makes pre-1710 framings difficult to articulate). Theater ratio is very low (0.08) because the constraint's function is primarily conceptual, not performative; the category exists to enable reasoning, not to dress up another mechanism. Accessibility collapse is high (0.72) because once the 'copy right' category is thinkable, alternatives are difficult to recover — the legal system has committed to this framework for reasoning about text ownership. Resistance is moderate (0.41) because some actors (authors, the reading public) resist aspects of how the category crystallizes, but the category itself gains rapid institutional acceptance. The measurement series run on one shared time grid (1650, 1680, 1710, 1730, 1750) so every metric is authored at every time point.
 *
 * PERSPECTIVAL GAP:
 *   From the jurist's seat, the constraint is the discovery and articulation of a natural legal category — 'ownable expression' emerges because it names a real phenomenon (the value of the text, independent of the paper). From the stationer's seat, the constraint is an institutional win: their monopoly, previously vulnerable to being revoked as royal whim, now grounds itself in property law, making it harder to overturn. From the author's seat, the constraint is a nominal victory without institutional substance: they are granted rights on paper but excluded from the machinery that enforces them. These perspectival gaps should compute as different directionalities and possibly different type classifications at different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Common-law jurists are near the beneficiary end (d ~0.25) because they gain conceptual coherence without bearing costs; the category exists to enable their reasoning. Stationers are also beneficiaries (d ~0.30) because their monopoly gains new legal grounding. Authors are complex: they are nominally granted rights by the Statute but are excluded from institutional control, so their directionality sits between symmetric (0.5) and target (1.0), placing them at d ~0.65 — they benefit from the category's existence (they can now claim ownership) but are suppressed within it (the category's institutional embodiment vests control in stationers). The reading public are observers, not stakeholders in the emergence event itself — they experience downstream effects but do not participate in setting the category.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (pre-1710 chaos in articulating text ownership) is live at 1710 — the Statute is a direct response to dispute. By 1750, the category has become normalized; the institutional machinery of copyright protection is well-established. The question is whether the category persists because it solves a live coordination problem (jurists need the vocabulary, the market needs property rights in intangibles) or because institutions are invested in maintaining it (stationers and the Crown benefit from the monopoly, authors are locked in). The theater_ratio rising from 0.02 to 0.10 suggests increasing performative use: early on, the category is a genuine conceptual tool; over time, it becomes a justification for policies that serve institutional interests. This pattern is consistent with mandatrophy: the category's original function (conceptual clarity) is intact, but its institutional embodiment (the monopoly) accumulates extraction. The constraint is not mandatrophic in the strict sense — the category continues to enable genuine reasoning — but it is under pressure toward that diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    thinkability_vs_first_holding,
    'Is the emergence of ''copy right'' as a thinkable category a SEPARATE event from the emergence of authorship as a legitimate rights-holder? Or is thinkability merely the epistemic mirror of the first holding entering the legal system?',
    'M4/M5 collapse test: examine whether the two readings (thinkability_reading and first_holding_reading) produce different classifications of the constraint when directionality and scope are held constant. If they compute to the same type, the readings are temporal framings of one structural change; if they compute differently, thinkability is structurally independent.',
    'If independent: two constraints, two constraint IDs, linked via network.affects_constraints. If collapsed: one constraint with two readings, unified under a single story with kernel_context documenting the interpretive choice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(thinkability_vs_first_holding, conceptual, 'Whether category emergence and first-holding are structurally distinct events or temporal framings of the same structural change.').

omega_variable(
    natural_law_vs_constructed_category,
    'Is ''ownable expression'' a fact of nature that jurists discovered in 1710, or a constructed legal category that jurists invented in response to economic and institutional pressures?',
    'Trace whether ''copy right'' vocabulary appears in pre-1710 statutes, cases, or jurist writing without the 1710 legal-reform event. If the concept pre-existed the reform, it was discovered/formalized; if it appears only after, it was constructed.',
    'If discovered: the constraint is a genuine mountain (natural legal reality), and the ''beneficiaries'' declaration triggers FSM evaluation. If constructed: the beneficiaries are real institutional actors who benefit from the category''s emergence, and the constraint may reclassify to tangled_rope (invented category serving institutional interests).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_category, empirical, 'Whether ''ownable expression'' is a pre-existing natural category or an invented legal construct.').

omega_variable(
    suppression_mechanism_epistemic_vs_institutional,
    'Is the ''suppression'' measured here institutional suppression (the Crown and Stationers actively barring alternative framings, preventing dissent from the ''copy right'' category) or epistemic suppression (the category, once thinkable, crowds out pre-1710 vocabulary, making alternatives difficult to articulate)?',
    'Historical record of who resisted the ''copy right'' framing and whether resistance was legally suppressed (institutional) or simply rendered incoherent by the new vocabulary (epistemic).',
    'If institutional: suppression is a structural feature of the constraint''s enforcement; if epistemic: suppression is an emergent feature of conceptual coherence, not coercion. The latter is lower-stakes and fits the mountain profile better.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_epistemic_vs_institutional, conceptual, 'Whether suppression of alternative framings is enforced institutionally or emerges from epistemic coherence.').

omega_variable(
    scope_of_coherence,
    'Is the emergence of ''copy right'' coherence a feature of English common law specifically, or does it represent a global/universal threshold in legal concept formation that multiple jurisdictions crossed independently?',
    'Compare IP emergence dates and vocabulary in civil-law jurisdictions, Scots law, and colonial legal systems. If multiple systems invented ''ownable expression'' independently, it is a universal conceptual threshold; if unique to England, it is a jurisdictional category.',
    'If universal: the constraint is closer to a mountain (crossing a threshold of conceptual coherence that any legal system would eventually reach). If jurisdictional: the constraint is more constructed (England made a choice; other systems made different choices). Spatial scope assessment depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_coherence, empirical, 'Whether IP category emergence is jurisdiction-specific or universal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1650, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1650, ip_category_emergence__thinkability_reading, theater_ratio, 1650, 0.02).
narrative_ontology:measurement(ip_c_tr_t1680, ip_category_emergence__thinkability_reading, theater_ratio, 1680, 0.04).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.08).
narrative_ontology:measurement(ip_c_tr_t1730, ip_category_emergence__thinkability_reading, theater_ratio, 1730, 0.09).
narrative_ontology:measurement(ip_c_tr_t1750, ip_category_emergence__thinkability_reading, theater_ratio, 1750, 0.1).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1650, ip_category_emergence__thinkability_reading, base_extractiveness, 1650, 0.12).
narrative_ontology:measurement(ip_c_be_t1680, ip_category_emergence__thinkability_reading, base_extractiveness, 1680, 0.18).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.31).
narrative_ontology:measurement(ip_c_be_t1730, ip_category_emergence__thinkability_reading, base_extractiveness, 1730, 0.35).
narrative_ontology:measurement(ip_c_be_t1750, ip_category_emergence__thinkability_reading, base_extractiveness, 1750, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1650, ip_category_emergence__thinkability_reading, suppression_requirement, 1650, 0.08).
narrative_ontology:measurement(ip_c_su_t1680, ip_category_emergence__thinkability_reading, suppression_requirement, 1680, 0.12).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.19).
narrative_ontology:measurement(ip_c_su_t1730, ip_category_emergence__thinkability_reading, suppression_requirement, 1730, 0.2).
narrative_ontology:measurement(ip_c_su_t1750, ip_category_emergence__thinkability_reading, suppression_requirement, 1750, 0.21).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__thinkability_reading, 0.03).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% The kernel ip_category_emergence decomposes into three structurally distinct constraints. The thinkability_reading treats the constraint as the emergence of conceptual coherence for 'copy right' as a distinct category — a natural-law or logically-necessary threshold that legal reasoning must cross. The first_holding_reading treats the constraint as the institutional moment when authors (or stationers) enter the rights-holder set — an event in the distribution of power. The synchronic_diachronic_seam reading disputes whether these are one event viewed from two angles (temporal framing artifact) or two independent structural changes. Each reading has a different ε, beneficiary/victim structure, and type classification. Thinkability emphasizes the conceptual space (low extraction, high accessibility collapse, mountain claim); first-holding emphasizes power redistribution (higher extraction, suppression of prior claimants). The ε-invariance principle requires separate constraint stories because the core claim differs: 'thinkability' measures conceptual emergence while 'first-holding' measures institutional change. Linking them via network.affects_constraints preserves the kernel unity while respecting structural independence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
