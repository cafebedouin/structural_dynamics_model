% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__progressive_synthesis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__progressive_synthesis, []).

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
 *   constraint_id: quran_9_5_scope__progressive_synthesis
 *   human_readable: Qur'an 9:5 Scope Constraint (Progressive Synthesis Reading)
 *   domain: islamic_jurisprudence/hermeneutics/political_theology
 *
 * SUMMARY:
 *   Qur'an 9:5, known as 'Ayat al-Sayf' (Verse of the Sword), states: 'So
 *   when the sacred months have passed, then kill the polytheists wherever
 *   you find them.' This verse has generated centuries of Islamic juridical
 *   debate regarding its scope, temporality, and normativity. The progressive
 *   synthesis reading instantiates the position that Verse 9:5 represents a
 *   time-bound, historically-specific political directive addressed to
 *   7th-century Arabian Islamic polity in response to particular Meccan
 *   polytheist opposition, not an eternal legal command binding contemporary
 *   Islamic communities. This reading further claims that the Qur'anic
 *   ethical trajectory — moving from early surahs emphasizing tolerance and
 *   coexistence toward later surahs addressing specific governance contexts —
 *   demonstrates that literal application of harsh provisions violates the
 *   text's own internal hermeneutical arc. The progressive synthesis reading
 *   thus removes Verse 9:5 entirely from the active normative constraint
 *   space: neither polytheists nor Muslims remain bound by its directive in
 *   contemporary contexts. The structural consequence: authority structures
 *   claiming the verse's ongoing binding force lose legitimacy;
 *   secular-pluralist and moderate Islamic frameworks gain institutional
 *   leverage; textualist interpretation methodologies face a methodological
 *   crisis. This constraint demonstrates how a single scriptural verse can be
 *   structurally decomposed into multiple constraints with different ε
 *   values, different beneficiary structures, and different temporal
 *   horizons, depending on which reading's premises are adopted.
 *
 * KEY AGENTS:
 *   - Literalist Authority Structures (textualist scholars, traditional jurisprudence schools): Primary victim (powerless/trapped) — experience the reading as existential threat to hermeneutical methodology; no exit option within textualist framework
 *   - Conservative Islamic Legal Scholarship (traditional fiqh institutions): Secondary victim (moderate/constrained) — benefit from clarity function of deterministic textual rules, but bear extraction cost as reading undermines literalist authority claims
 *   - Progressive Islamic Scholarship Movement: Primary beneficiary (institutional/arbitrage) — gains methodological authority to reconcile Islamic tradition with pluralist governance; can arbitrage between literalist and secular interpretive communities
 *   - Pluralist Governance Framework Advocates (secular states, human rights bodies, interfaith dialogue movements): Secondary beneficiary (organized/mobile) — use the reading to legitimate displacement of literalist religious authority from governance; see constraint as scaffolding toward complete secularization
 *   - Classical Jurisprudential Institutions (formal madhabs): Institutional actor (institutional/constrained) — maintain formal authority through inertia while methodological legitimacy erodes; exhibit piton characteristics
 *   - Analytical Observer (hermeneutical limits perspective): Civilizational view (analytical/analytical) — risks naturalizing what is a contingent institutional arrangement (authority structures grounded in interpretive frameworks) as an immutable hermeneutical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__progressive_synthesis, 0.35).
domain_priors:suppression_score(quran_9_5_scope__progressive_synthesis, 0.62).
domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, extractiveness, 0.35).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(quran_9_5_scope__progressive_synthesis, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__progressive_synthesis, tangled_rope).
narrative_ontology:human_readable(quran_9_5_scope__progressive_synthesis, "Qur'an 9:5 Scope Constraint (Progressive Synthesis Reading)").
narrative_ontology:topic_domain(quran_9_5_scope__progressive_synthesis, "islamic_jurisprudence/hermeneutics/political_theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__progressive_synthesis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__progressive_synthesis, 'f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c').
narrative_ontology:cs_kernel_codification('f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c', fixed_text).
narrative_ontology:cs_authority_grounding('f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c', lineage).
narrative_ontology:cs_interpretation_layer_present('f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c').
narrative_ontology:cs_reading_relation('f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c', quran_9_5_scope__abrogating_universal, forecloses).
narrative_ontology:cs_reading_relation('f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c', quran_9_5_scope__contextual_defensive, coexists_with).
narrative_ontology:cs_axiom('f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c', foundational, ethical_trajectory_overrides_literal).
narrative_ontology:cs_axiom_status(ethical_trajectory_overrides_literal, holdable).
narrative_ontology:cs_axiom_grounding('f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c', ethical_trajectory_overrides_literal, deontological).
narrative_ontology:cs_axiom('f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c', foundational, contextual_supersession_principle).
narrative_ontology:cs_axiom_status(contextual_supersession_principle, holdable).
narrative_ontology:cs_axiom_grounding('f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c', contextual_supersession_principle, empirically_contingent).
narrative_ontology:cs_reference_frame('f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c', classical_literalist_authority).
narrative_ontology:cs_drift_state('f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c', contemporary_pluralist_institutional_environment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f16df5bc-1d31-4e6e-8e64-f8c9244a7b3c', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__progressive_synthesis, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, secular_pluralist_frameworks).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__progressive_synthesis, progressive_islamic_scholarship).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, textualist_authority_structures).
narrative_ontology:constraint_victim(quran_9_5_scope__progressive_synthesis, literalist_interpretation_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITERALIST AUTHORITY STRUCTURE (SNARE) — Textualist interpretation traditions experience the progressive synthesis reading as an existential threat to their hermeneutical authority. They cannot exit or reframe without abandoning the foundational claim that the text's surface meaning is normatively binding. No alternative reading framework available within their epistemic position. Trapped by identity fusion with literalist methodology.
constraint_indexing:constraint_classification(quran_9_5_scope__progressive_synthesis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSERVATIVE ISLAMIC LEGAL SCHOLARSHIP (TANGLED ROPE) — Traditional jurisprudence schools benefit from the authority-stabilizing function of deterministic textual rules (coordination: enables predictable legal rulings across contexts), but also bear extraction cost as the progressive reading constrains their authority claims. Can invoke classical 'naskh' (abrogation) doctrine and 'specificity' (khusus) arguments, providing some exit option, but at cost of engaging contested methodological territory.
constraint_indexing:constraint_classification(quran_9_5_scope__progressive_synthesis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROGRESSIVE ISLAMIC SCHOLARSHIP MOVEMENT (ROPE) — Experiences the constraint as a coordination mechanism solving the problem of reconciling Islamic textual authority with pluralist governance frameworks. Primary beneficiary. Can arbitrage between different interpretation communities; has institutional platforms (academic journals, theological seminaries) enabling exit from literalist authority. Net beneficiary of the constraint.
constraint_indexing:constraint_classification(quran_9_5_scope__progressive_synthesis, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLURALIST GOVERNANCE FRAMEWORK ADVOCATES (SCAFFOLD) — Organized actors (secular states, international human rights bodies, inter-faith dialogue movements) see the progressive synthesis reading as temporary scaffolding toward a complete secularization or compartmentalization of religious authority. See the constraint as dissolving through institutional change: as pluralist norms mature institutionally, the binding force of literalist scripture interpretation loses grip on governance legitimacy. Exit path: institutionalization of secular legal authority and religious autonomy separation.
constraint_indexing:constraint_classification(quran_9_5_scope__progressive_synthesis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: CLASSICAL JURISPRUDENTIAL INSTITUTIONS (PITON) — Formal Islamic legal schools (Hanafi, Maliki, Shafi'i, Hanbali) and their institutional embodiments maintain interpretive authority through institutional inertia even as their methodological foundations are contested. The theater of classical jurisprudence (formal opinions, legal reasoning rituals) persists while the underlying authority claim erodes. Degraded constraint: the institution performs legal function but the constraint's binding force derives increasingly from custom rather than methodological legitimacy.
constraint_indexing:constraint_classification(quran_9_5_scope__progressive_synthesis, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / HERMENEUTICAL LIMITS VIEW (MOUNTAIN) — From a civilizational perspective, the constraint appears immutable: no interpretive framework can simultaneously hold (a) that a specific historical text binds all readers across time, and (b) that contextual reading supersedes literal meaning. This appears to be an irreducible logical limit on hermeneutics itself. However, the structural data reveals beneficiary interests grounding the apparent immutability. The engine detects this as a false summit.
constraint_indexing:constraint_classification(quran_9_5_scope__progressive_synthesis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__progressive_synthesis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quran_9_5_scope__progressive_synthesis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quran_9_5_scope__progressive_synthesis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(quran_9_5_scope__progressive_synthesis, TR),
    TR >= 0.70.

:- end_tests(quran_9_5_scope__progressive_synthesis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The progressive synthesis reading does coordinate a genuine governance problem — how to reconcile classical Islamic textual authority with contemporary pluralist state structure — but also extracts by displacing literalist authority claims without offering internally-grounded Islamic replacement. The extraction is not maximal (0.72) because the reading operates within recognizable Islamic hermeneutical tradition (can invoke Mu'tazilite rationalism, maqasid (purposes) methodology). But extraction rises from t0 (0.18) to t50 (0.35) as the reading gains institutional foothold and literalist communities experience authority loss. Suppression (0.62): Moderate-high. Rising over the interval from 0.48 to 0.62. Suppression mechanisms include: dominance of progressive-sympathetic academic institutions in Quranic studies, publication barriers against literalist scholarship in secular academic venues, institutional incentives favoring pluralist interpretations in pluralist-governance contexts, social suppression of literalist voice in interfaith dialogue. Suppression increases as the reading becomes institutionally entrenched. Theater ratio (0.68): High and rising (0.45 → 0.68). The reading exhibits high performative content: scholarly apparatus emphasizing method (historical-critical reading, ethical trajectory analysis) creates impression of textual inevitability while the actual reading rests on normative commitments (preferring ethical consistency over literal meaning) that are not textually determined. Theater increases as the reading becomes institutionalized — more ritual, more formal scholarly performance, less direct hermeneutical engagement with textualist objections.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence from identical scriptural base. The textualist sees a constraint that binds them (the verse remains normatively authoritative) and that their methodological commitments prevent them from escaping (Snare). The progressive sees a constraint dissolving (the verse exits normative space; beneficiary status secured). The classical institution sees its own authority degrading (Piton). The pluralist governance advocate sees a temporary scaffold (Scaffold) — the constraint's function is to mediate toward eventual complete secularization of Islamic authority. The conservative scholar sees mixed coordination and extraction (Tangled Rope) — benefit from clarity, cost from authority loss. The analytical observer sees an apparent immutable hermeneutical law (Mountain) but the structural data reveals a false summit: beneficiaries exist (progressive scholars, pluralist states) whose interests align with naturalizing the 'ethical supersession' principle as inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for this reading: Progressive scholarship (institutional/arbitrage/beneficiary) derives d ≈ 0.15 → f(d) ≈ -0.01, producing low or slightly negative χ (they experience the constraint as beneficial coordination). Textualist communities (powerless/trapped/victim) derive d ≈ 0.95 → f(d) ≈ 1.42, producing high χ (they experience maximal extraction). Conservative scholars (moderate/constrained/split victim-beneficiary) derive d ≈ 0.60 → f(d) ≈ 0.95, producing moderate-high χ. Pluralist governance (organized/mobile/beneficiary) derives d ≈ 0.35 → f(d) ≈ 0.25, producing low-moderate χ (benefit with some external pressure). The analytical observer (analytical/analytical) derives canonical d ≈ 0.72 → f(d) ≈ 1.15, showing analytical observer chi — can see the structure but risks being captured by beneficiary framing. The scope modifier σ(global) = 1.2 amplifies all χ values moderately, reflecting that this constraint operates across all Muslim-majority and Muslim-diaspora communities globally.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutical_framework_determination,
    'Is the progressive synthesis reading a legitimate internal Islamic hermeneutical development, or an externally-imposed secularization framework disguised as Islamic scholarship?',
    'Historical genealogy of progressive interpretation methods; tracing back to classical Islamic theological traditions (Mu''tazilite rationalism, Ash''arite contextualism) vs. modern secular hermeneutics influence; analysis of authority claims within Islamic intellectual lineage',
    'If internal development: the constraint is Tangled Rope (coordination + extraction within a coherent tradition). If externally imposed: the constraint is Snare (extraction masquerading as legitimate scholarship). Classification hinge on whether the reading''s authority grounding is ''lineage'' (internal) or ''extraction'' (imposed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_framework_determination, conceptual, 'Whether progressive hermeneutics is internal Islamic development or external secularization imposition').

omega_variable(
    ethical_trajectory_empirical_basis,
    'What is the empirical foundation for the claim that the Qur''an exhibits a progressive ethical trajectory from early surahs (tolerance) to late surahs (specificity) that invalidates literalist application of harsh provisions?',
    'Systematic literary and historical analysis of chronological progression in Qur''anic ethics; comparison against alternative periodization models (thematic rather than chronological); assessment of whether ethical progression is evident or a posterior reading imposed onto the text',
    'If trajectory is textually evident: the constraint is legitimately Tangled Rope (readers genuinely disagree on how to weight progression vs. literal text). If trajectory is imposed pattern: the constraint is extractive Snare (the reading manufactures authority it claims to discover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_trajectory_empirical_basis, empirical, 'Empirical status of Qur''anic ethical trajectory claim').

omega_variable(
    authority_displacement_mechanism,
    'When the progressive synthesis reading displaces literalist authority, what new legitimacy structure grounds Islamic legal authority in governance contexts?',
    'Analysis of institutional outcomes in Muslim-majority pluralist states (Tunisia, Indonesia, Turkey); assessment of whether displaced literalism is replaced by scholarly consensus, popular sovereignty, rights-based constitutionalism, or hybrid models; measurement of whether new authority structure is stable or transitional',
    'If stable replacement exists: constraint is Tangled Rope or Scaffold (coordination mechanism with institutional endpoint). If no stable replacement: constraint is Snare (authority extraction without legitimate substitute). Determines whether scaffold sunset is real or aspirational.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_displacement_mechanism, empirical, 'Whether authority displacement by progressive reading creates stable alternative legitimacy structure').

omega_variable(
    textual_essentialism_forelosure,
    'Does the progressive synthesis reading logically foreclose the textualist position (literalism is categorically invalid for all classical Islamic texts) or merely coexist with it as alternative interpretive frameworks?',
    'Formal logical analysis: can a single reader hold both ''Verse 9:5 is not binding'' (progressive) AND ''this verse contains binding normative content'' (textualist) without internal contradiction? Or does progressive reading''s core claim logically rule out the textualist core claim in any single coherent framework?',
    'If foreclosure: the reading_relations should declare ''forecloses'' toward textualist sibling. If coexistence: declare ''coexists_with''. Determines whether the kernel admits genuine pluralism or contains latent exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_essentialism_forelosure, conceptual, 'Whether progressive reading logically forecloses textualism or coexists with it').

omega_variable(
    ethical_supersession_normativity,
    'On what grounds does the progressive synthesis reading claim that ethical trajectory supersedes literal text? What normative principle justifies preferring one interpretive layer over another?',
    'Articulation of the meta-hermeneutical principle (e.g., ''intention over letter,'' ''purpose (maqasid) over form,'' ''rationalist interpretation over traditionalist''). Assessment of whether this principle is internally Islamic (grounded in classical Islamic theology) or externally imported. Comparison across Islamic and Christian hermeneutical traditions to identify whether supersession principle is tradition-specific or transcultural.',
    'If principle is internal: constraint shows legitimate intra-Islamic methodological pluralism (Tangled Rope). If principle is external: constraint shows replacement of Islamic authority with secular epistemology (Snare). Determines whether beneficiary classification is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_supersession_normativity, conceptual, 'Normative grounds for preferring ethical trajectory over literal text').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__progressive_synthesis, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(q95ps_theater_t0, quran_9_5_scope__progressive_synthesis, theater_ratio, 0, 0.45).
narrative_ontology:measurement(q95ps_theater_t25, quran_9_5_scope__progressive_synthesis, theater_ratio, 25, 0.62).
narrative_ontology:measurement(q95ps_theater_t50, quran_9_5_scope__progressive_synthesis, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(q95ps_extract_t0, quran_9_5_scope__progressive_synthesis, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(q95ps_extract_t25, quran_9_5_scope__progressive_synthesis, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(q95ps_extract_t50, quran_9_5_scope__progressive_synthesis, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(q95ps_supp_t0, quran_9_5_scope__progressive_synthesis, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(q95ps_supp_t25, quran_9_5_scope__progressive_synthesis, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(q95ps_supp_t50, quran_9_5_scope__progressive_synthesis, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__progressive_synthesis, identity_coordination).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, islamic_authority_secularization_pluralist_states).
narrative_ontology:affects_constraint(quran_9_5_scope__progressive_synthesis, textualist_hermeneutics_institutional_authority).

% DUAL FORMULATION NOTE:
% The Qur'an 9:5 kernel admits three structurally distinct readings, each instantiating a different constraint with different ε values. The progressive_synthesis reading (this file) claims ε=0.35 (Tangled Rope) and removes the verse from normative space. The abrogating_universal reading claims ε≈0.70+ (Snare or Scaffold depending on perspective) and reinstates the verse's binding force universally. The contextual_defensive reading claims ε≈0.25 (Rope or Mountain) and localizes the verse to historical context. Each reading produces different beneficiary structures, different suppression mechanisms, and different terminal institutional outcomes. They are not observational variants of a single constraint; they are genuinely different constraints grounded in different hermeneutical premises.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
