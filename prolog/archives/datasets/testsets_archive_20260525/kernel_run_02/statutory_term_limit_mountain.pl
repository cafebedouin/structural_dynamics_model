% ============================================================================
% CONSTRAINT STORY: statutory_term_limit_mountain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_term_limit_mountain, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: statutory_term_limit_mountain
 *   human_readable: Statutory Term Limit as Natural Law of Intellectual Property
 *   domain: legal_history/intellectual_property
 *
 * SUMMARY:
 *   The Statute of Anne (1710) established the first statutory copyright
 *   regime with an explicit term limit, transforming copyright from a
 *   monopoly grant to a time-limited incentive structure. This constraint
 *   story examines whether the statutory term limit represents a natural law
 *   of IP coherence or a constructed institutional arrangement that has been
 *   naturalized as inevitable. The structure of copyright law — the
 *   fundamental tension between property incentives (which naturally
 *   gravitate toward perpetuity) and competitive access (which requires
 *   boundaries) — creates an apparent logical ceiling beyond which indefinite
 *   copyright becomes indistinguishable from feudal exclusion. Yet the
 *   observed history shows systematic extension of copyright terms (British
 *   extensions in 1814, 1877; US extensions in 1976, 1998) whenever powerful
 *   interests lobbied, suggesting the mountain appearance may be a false
 *   summit: naturalized policy masquerading as natural law. The constraint is
 *   the binding of all copyright regimes to some form of term limit, not any
 *   specific duration. The term limit itself — whether 14 years,
 *   life-plus-70, or some other measure — is structurally variable. What
 *   appears immutable is the principle that copyright cannot coherently be
 *   perpetual without re-creating the monopoly condition the statute was
 *   designed to escape. This is the analytical mountain. The institutional
 *   and powerless perspectives, however, diverge sharply on whether the term
 *   limit functions as an immutable boundary or as a negotiable coordination
 *   mechanism.
 *
 * KEY AGENTS:
 *   - Statute of Anne Parliament (1710): Institutional actor (institutional/arbitrage) — crafted the original term limit to solve the Stationers' Company monopoly problem; created the boundary condition that defined modern copyright law
 *   - Publishing Industry & Rights-Holders: Powerful institutional actors (powerful/mobile, institutional/arbitrage) — benefit from coordination mechanisms but have successfully lobbied for term extensions; experience the limit as negotiable
 *   - Archival Commons & Libraries: Powerless institutional actors (powerless/trapped) — depend on public domain access; experience term limits as ineffective against systematic extension lobbying
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the term limit as a structural necessity emergent from the logical coherence of copyright law; risks naturalizing contingent policy extensions as inevitable
 *   - Comparative Legal Traditions: Regional variation (institutional/constrained) — some IP regimes (trade secrets, author's rights traditions) depart from strict term limits; test whether alternatives are coherent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_term_limit_mountain, 0.18).
domain_priors:suppression_score(statutory_term_limit_mountain, 0.04).
domain_priors:theater_ratio(statutory_term_limit_mountain, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_term_limit_mountain, extractiveness, 0.18).
narrative_ontology:constraint_metric(statutory_term_limit_mountain, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(statutory_term_limit_mountain, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_term_limit_mountain, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(statutory_term_limit_mountain, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_term_limit_mountain, mountain).
narrative_ontology:human_readable(statutory_term_limit_mountain, "Statutory Term Limit as Natural Law of Intellectual Property").
narrative_ontology:topic_domain(statutory_term_limit_mountain, "legal_history/intellectual_property").

domain_priors:emerges_naturally(statutory_term_limit_mountain).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(statutory_term_limit_mountain, formalized).
narrative_ontology:cs_authority_grounding(statutory_term_limit_mountain, lineage).
narrative_ontology:cs_interpretation_layer_present(statutory_term_limit_mountain).
narrative_ontology:cs_reading_relation(statutory_term_limit_mountain, copyright_natural_law_reading, forecloses).
narrative_ontology:cs_reading_relation(statutory_term_limit_mountain, perpetual_copyright_variant, forecloses).
narrative_ontology:cs_axiom(statutory_term_limit_mountain, foundational, copyright_must_have_temporal_boundary).
narrative_ontology:cs_axiom_status(copyright_must_have_temporal_boundary, holdable).
narrative_ontology:cs_axiom_grounding(statutory_term_limit_mountain, copyright_must_have_temporal_boundary, deontological).
narrative_ontology:cs_axiom(statutory_term_limit_mountain, foundational, monopoly_perpetuity_creates_feudal_exclusion).
narrative_ontology:cs_axiom_status(monopoly_perpetuity_creates_feudal_exclusion, holdable).
narrative_ontology:cs_axiom_grounding(statutory_term_limit_mountain, monopoly_perpetuity_creates_feudal_exclusion, empirically_contingent).
narrative_ontology:cs_axiom(statutory_term_limit_mountain, secondary, authors_deserve_limited_exclusive_rights).
narrative_ontology:cs_axiom_status(authors_deserve_limited_exclusive_rights, holdable).
narrative_ontology:cs_axiom_grounding(statutory_term_limit_mountain, authors_deserve_limited_exclusive_rights, deontological).
narrative_ontology:cs_reference_frame(statutory_term_limit_mountain, statute_of_anne_monopoly_prevention).
narrative_ontology:cs_drift_state(statutory_term_limit_mountain, contemporary_copyright_extension_equilibrium, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOGICAL STRUCTURE (MOUNTAIN) — From a civilizational perspective spanning multiple legal traditions, statutory term limits represent an irreducible constraint on the coherence of IP law itself. The fundamental tension between property perpetuity and monopoly prevention creates a logical ceiling beyond which indefinite copyright becomes indistinguishable from feudal land monopoly. No jurisdictional framing escapes this logic. The term limit is not a policy choice but a structural necessity — the mathematical boundary between two incoherent regimes.
constraint_indexing:constraint_classification(statutory_term_limit_mountain, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: CONSTITUTIONAL AUTHORITY (MOUNTAIN) — From the standpoint of constitutional legality (e.g., US Constitution Article I, Section 8: copyright protection for 'limited times'), the term limit emerges as an immutable boundary condition on legislative power itself. No legislature can coherently authorize perpetual copyright and claim fidelity to the constitutional grant. The limit is not discovered empirically — it is a logical consequence of the constitutional text. Resistance to this boundary is effectively zero across established legal systems.
constraint_indexing:constraint_classification(statutory_term_limit_mountain, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLISHING INDUSTRY (ROPE) — From the standpoint of commercial publishers with access to extension mechanisms, statutory term limits function as a coordination device: they create a predictable renewal cadence, enable estate planning, and establish clear ownership transition windows. The constraint solves a genuine collective-action problem (preventing tragedy-of-the-commons from unlimited derivative claims) while providing publishers with arbitrage opportunities (copyright extension lobbying, estate licensing). For this powerful institutional actor, the term limit is experienced as a negotiable coordination mechanism, not an immutable boundary.
constraint_indexing:constraint_classification(statutory_term_limit_mountain, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: ARCHIVAL COMMONS (SNARE) — From the standpoint of libraries, archives, and the epistemic commons that depend on public domain access, statutory term limits represent insufficient protection against extraction. Even with nominal term limits (95 years for works-for-hire in the US), the effective duration of copyright exceeds the institutional memory of most cultural organizations. The commons bears the full cost of monopoly during the term while having no exit mechanism. Multiple term extensions (Sonny Bono Act) demonstrate that the term limit is not enforced against powerful interests. For this powerless agent, the constraint is a snare: the term limit appears immutable (mountain) until confronted with extension mechanisms available exclusively to rights-holders.
constraint_indexing:constraint_classification(statutory_term_limit_mountain, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / STATUTE OF ANNE READING (MOUNTAIN) — The Statute of Anne (1710) established the term limit (14 years, renewable once) as the boundary condition that transformed copyright from a monopoly grant (Stationers' Company model) into a time-limited incentive structure. This reading treats the term limit as a natural law of copyright law itself: once you abandon the monopoly model, you cannot coherently sustain indefinite copyright without re-creating the condition (total exclusion) the statute was designed to escape. The Statute codified a structural insight, not an arbitrary policy choice. From this perspective, all subsequent copyright expansions are false summits — naturalized policy as legal necessity.
constraint_indexing:constraint_classification(statutory_term_limit_mountain, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_term_limit_mountain_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(statutory_term_limit_mountain, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statutory_term_limit_mountain, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(statutory_term_limit_mountain, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(statutory_term_limit_mountain, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(statutory_term_limit_mountain, ExtMetricName, E),
    domain_priors:suppression_score(statutory_term_limit_mountain, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(statutory_term_limit_mountain),
    narrative_ontology:constraint_metric(statutory_term_limit_mountain, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(statutory_term_limit_mountain, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(statutory_term_limit_mountain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low, consistent with mountain classification. The term limit itself, as a binding constraint on all copyright regimes, represents minimal extraction — it is a boundary condition, not a wealth transfer. The measurement shows slight rise from 1710 (0.08) to 1910 (0.20), then slight decline to 2010 (0.18). This modest trajectory reflects the gradual expansion of copyright terms without fundamental structural change: term extensions add extraction at the margins but do not alter the core constraint that copyright must have some temporal boundary. Theater ratio (0.15): Very low, consistent with natural law signature. The statutory term limit requires minimal performative apparatus — it is stated plainly in statute and enforced through straightforward duration calculations. The slight rise to 0.18 in the early 20th century reflects increasing rhetoric around copyright extension debates, then moderation as the extended term became normalized. The theater never rises significantly because the constraint itself is not contestable within the framework of copyright law — no major actor argues for perpetual copyright on principle, only for longer specific terms. Suppression (0.04): Minimal. The term limit, as a boundary condition, does not require coercion to maintain. It is enforced through straightforward legal procedures (expiration date) and accepted across all major legal traditions. The low suppression value reflects that the constraint is not maintained through force but through the logical necessity of the copyright framework itself.
 *
 * PERSPECTIVAL GAP:
 *   The critical perspectival gap divides the analytical observer (who sees mountain: a structural necessity) from the archival commons (who sees snare: an insufficient protection against extraction). The publishing industry and constitutional authority both see mountain but from different grounding: the publishing industry experiences the term limit as a coordination mechanism (rope characteristics) that enables renewal planning, while constitutional authority sees it as a logical boundary on legislative power. The divergence between rope experience (powerful actors) and snare experience (powerless actors) reveals the false summit structure: the term limit appears immutable (mountain) to those without exit options, but negotiable (rope) to those with lobbying capacity and extension mechanisms. The Statute of Anne reading (analytical observer) risks naturalizing what is actually a constructed institutional arrangement that has proven elastic over time. This perspectival gap is the primary diagnostic signal that the mountain classification may be a false summit — the term limit is a natural law only from the vantage point of those who cannot change it.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint declares no explicit beneficiaries or victims in the base_properties because the mountain classification (natural law) does not require structural relationship declarations in the classical formulation. However, the false summit engine may trigger if we treat the constraint as having implicit beneficiaries (publishing industry benefits from term limits that prevent perpetual monopoly while enabling strategic extensions; archival commons is the implicit victim). The analytical perspective does not derive directionality through the standard power-exit-benefit chain because analytical positions are observers of structure, not agents within it. The institutional perspectives (publishing industry, constitutional authority) occupy different structural relationships: the publishing industry derives d ≈ 0.30-0.40 (beneficiary with arbitrage options), while constitutional authority derives d ≈ 0.10 (neutral enforcer of boundary). The powerless archival commons derives d ≈ 0.85 (victim with no exit). These derived d values, if computed explicitly, would show that the effective extraction χ experienced by powerless agents is substantially higher than by powerful agents, despite the same base ε. This is the asymmetry that reveals the false summit: a constraint that appears as mountain (immutable boundary) to all observers masks different effective extraction rates based on structural position.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC CANDIDACY FOR FALSE SUMMIT: This constraint presents as a mountain (natural law of copyright coherence) across the analytical observer's civilizational perspective. The base metrics support this: ε ≤ 0.25, suppression ≤ 0.05, accessibility_collapse ≥ 0.85, resistance ≤ 0.15, emerges_naturally = true. All mountain gates fire. However, three structural signals suggest this may be a false summit — a constraint naturalized as necessary that actually represents contingent institutional arrangements: (1) The publishing industry perspective reveals rope characteristics (coordination mechanism with beneficiaries and clear exit options), suggesting that what the analytical observer sees as natural law, powerful actors experience as negotiable policy. (2) The archival commons perspective reveals snare characteristics (high suppression of alternatives, no exit options, systematic extraction through term extensions), suggesting the constraint functions asymmetrically — mountain to the powerless, rope to the powerful. (3) The historical measurements show consistent term extensions (1814, 1877, 1976, 1998) despite the mountain classification, suggesting the natural law appearance is maintained through institutional rhetoric rather than logical necessity. The false summit detection engine would require an explicit beneficiary declaration to trigger. If we were to declare 'publishing_industry' as a beneficiary (because the term limit structure enables strategic licensing and extension lobbying without re-creating Stationers' monopoly), the FSM candidate would activate: mountain metrics + beneficiary presence → reclassification signal to tangled_rope or rope, depending on whether the archival commons counts as a victim. RECOMMENDATION: This constraint story is structured to support false summit diagnostic analysis. The mountain classification is deliberately preserved to show how a constraint can appear natural-lawlike while actually instantiating constructed institutional arrangements. The five omegas document the irreducible uncertainties that, if resolved empirically, would reclassify the constraint. The perspectival gap between rope (powerful) and snare (powerless) is the primary diagnostic signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_statute,
    'Is the statutory term limit a natural law of copyright coherence, or a constructed institutional arrangement that has accrued legitimacy through repetition?',
    'Comparative legal history across IP regimes that departed from term limits (e.g., German author''s rights traditions, trade secret perpetuity); analysis of whether term-limit-free regimes produced coherent law or encountered logical contradictions that forced re-adoption of limits',
    'If natural law: classification stands as mountain across all perspectives. If constructed: classification should be tangled_rope (hybrid coordination + extraction disguised as natural law); false summit engine would reclassify. The Statute of Anne would be a constructed solution to a real coordination problem, not a discovered natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_statute, conceptual, 'Whether term limit is natural law or constructed institutional arrangement').

omega_variable(
    thinkable_vs_held_distinction,
    'Did the Statute of Anne ''make thinkable'' a new conceptual space (time-limited monopoly as coherent category) or ''first hold'' an already-occupied position (authors'' rights as natural property)?',
    'Close analysis of statutory language, preamble framing, and parliamentary debate; comparison with prior legal concepts of authorship (common law, guild structures); examination of whether contemporaries perceived the statute as innovation vs recovery',
    'If thinkable: the term limit opened a new conceptual possibility; copyright as we know it is posterior to the statute; no pre-statutory IP regime had the logical structure of time-limited exclusive rights. If held: the statute articulated something already latent in common law or natural property rights; the term limit is a rediscovery of pre-existing legal principle. The distinction affects whether we understand copyright as a constructed legal fiction or as a natural-law discovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(thinkable_vs_held_distinction, conceptual, 'Statute of Anne as conceptual innovation (''thinkable'') vs. articulation of latent position (''held'')').

omega_variable(
    monopoly_prevention_necessity,
    'Is the term limit strictly necessary to prevent the re-emergence of Stationers'' Company-style monopoly, or is monopoly prevention achievable through alternative mechanisms (compulsory licensing, fair-use doctrine, derivative-works restrictions)?',
    'Experimental policy analysis of regimes with weak term limits but strong compulsory licensing (some European models); examination of whether modern competition law can substitute for term limits in preventing IP monopoly abuse; historical analysis of whether copyright monopolies have re-emerged in modern extensions without triggering intervention',
    'If necessary: term limit is a structural requirement, supporting mountain classification. If alternative mechanisms suffice: term limit is one policy tool among several; classification should shift toward rope (coordination mechanism) or tangled_rope (hybrid with extraction). This would expose the false summit: the mountain appearance masks contingent policy choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopoly_prevention_necessity, empirical, 'Whether term limit is structurally necessary or substitutable').

omega_variable(
    sunset_enforcement_asymmetry,
    'Why does the term limit functionally operate as a soft sunset (easily extended for powerful interests) rather than a hard boundary (truly immutable)?',
    'Political economy analysis of copyright extension campaigns; comparative study of enforceability of term limits across jurisdictions with different institutional structures; examination of whether any copyright term limit has actually expired without extension when rights-holders lobbied for extension',
    'If term limit is truly immutable: mountain classification holds. If extensions are systematic and unopposed: the mountain is a false summit — the limit functions as coordination rhetoric for the publishing industry while remaining negotiable for powerful actors. The powerless (archival commons) experience it as mountain; the powerful experience it as rope. This perspectival gap is the diagnostic signal of a naturalized constraint that should be classified as tangled_rope with FSM candidate status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_enforcement_asymmetry, empirical, 'Enforcement asymmetry between term-limit rhetoric and extension practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_term_limit_mountain, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stl_theater_1710, statutory_term_limit_mountain, theater_ratio, 0, 0.08).
narrative_ontology:measurement(stl_theater_1810, statutory_term_limit_mountain, theater_ratio, 100, 0.12).
narrative_ontology:measurement(stl_theater_1910, statutory_term_limit_mountain, theater_ratio, 200, 0.18).
narrative_ontology:measurement(stl_theater_2010, statutory_term_limit_mountain, theater_ratio, 300, 0.15).

% Extraction over time
narrative_ontology:measurement(stl_extract_1710, statutory_term_limit_mountain, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(stl_extract_1810, statutory_term_limit_mountain, base_extractiveness, 100, 0.15).
narrative_ontology:measurement(stl_extract_1910, statutory_term_limit_mountain, base_extractiveness, 200, 0.2).
narrative_ontology:measurement(stl_extract_2010, statutory_term_limit_mountain, base_extractiveness, 300, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statutory_term_limit_mountain, resource_allocation).
narrative_ontology:affects_constraint(statutory_term_limit_mountain, copyright_extension_equilibrium).
narrative_ontology:affects_constraint(statutory_term_limit_mountain, public_domain_access_barrier).
narrative_ontology:affects_constraint(statutory_term_limit_mountain, authorial_incentive_necessity).

% DUAL FORMULATION NOTE:
% The statutory term limit is composed of three structurally distinct constraints: (1) term_limit_as_monopoly_prevention (mountain: logical boundary preventing re-creation of Stationers' monopoly), (2) term_limit_as_negotiable_extension_mechanism (rope: publishing industry coordination tool), (3) term_limit_as_insufficient_public_access (snare: archival commons extraction). Each has different ε. Decomposition is recommended for detailed analysis of which reading is operant in specific historical contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
