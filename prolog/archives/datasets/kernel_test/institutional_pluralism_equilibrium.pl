% ============================================================================
% CONSTRAINT STORY: institutional_pluralism_equilibrium
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_pluralism_equilibrium, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
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
 *   constraint_id: institutional_pluralism_equilibrium
 *   human_readable: Institutional Pluralism Equilibrium in Islamic Jurisprudence
 *   domain: legal_philosophy/institutional_history/islamic_jurisprudence
 *
 * SUMMARY:
 *   The institutional pluralism equilibrium in Islamic jurisprudence
 *   represents a constraint where the legitimacy of legal authority is
 *   grounded in the coexistence of multiple valid madhab (jurisprudential
 *   school) structures. This reading instantiates a specific interpretation
 *   of how Islamic legal authority has been organized: that the classical
 *   four schools (Hanafi, Maliki, Shafi'i, Hanbali) constitute a permanent
 *   institutional solution to the problem of jurisprudential reasoning in the
 *   absence of centralized legislative authority. The constraint exhibits
 *   genuine coordination functions (madhab structures organize training,
 *   transmit knowledge, stabilize legal doctrine) alongside asymmetric
 *   extraction (institutional gatekeeping of interpretive authority,
 *   suppression of heterodox reasoning, control of scholarly legitimacy). The
 *   equilibrium is maintained through institutional enforcement: scholars
 *   must work within madhab frameworks to claim jurisprudential authority;
 *   heterodox interpreters are delegitimized; secondary scholars are
 *   constrained by madhab consensus. The theater ratio (0.48) reflects that
 *   the claim of 'equal validity' across schools is partially performative —
 *   while the schools' methodologies differ legitimately, the institutional
 *   fiction that all represent equally viable permanent solutions obscures
 *   substantive methodological disagreements and the institutional interests
 *   maintaining the boundary.
 *
 * KEY AGENTS:
 *   - Madhab Institutional Frameworks (Hanafi, Maliki, Shafi'i, Hanbali): Primary beneficiaries (institutional/arbitrage) — derive authority, institutional stability, and scholarly transmission from the pluralism equilibrium
 *   - Established Scholarly Lineages: Primary beneficiaries (institutional/constrained) — gain legitimacy, network access, and methodological inheritance through madhab affiliation
 *   - Heterodox Interpreters and Individual Reasoners: Primary victims (powerless/identity_locked) — face institutional delegitimation, exclusion from authority networks, loss of scholarly standing; structurally mobile but identity-locked to Islamic scholarly tradition
 *   - Secondary-Level Scholars: Secondary victims (moderate/constrained) — experience mixed coordination benefits (training, networks) and extraction costs (limited discretion, constrained innovation)
 *   - Reform Movements (Salafi, Modernist): Secondary actors (organized/constrained) — attempted to bypass madhab structures; experienced extraction costs (institutional opposition) alongside coordination benefits (unified reform platforms)
 *   - Analytical Observer: Sees potential false summit — risks naturalizing contingent institutional arrangements as immutable jurisprudential necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_pluralism_equilibrium, 0.38).
domain_priors:suppression_score(institutional_pluralism_equilibrium, 0.52).
domain_priors:theater_ratio(institutional_pluralism_equilibrium, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_pluralism_equilibrium, extractiveness, 0.38).
narrative_ontology:constraint_metric(institutional_pluralism_equilibrium, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(institutional_pluralism_equilibrium, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_pluralism_equilibrium, tangled_rope).
narrative_ontology:human_readable(institutional_pluralism_equilibrium, "Institutional Pluralism Equilibrium in Islamic Jurisprudence").
narrative_ontology:topic_domain(institutional_pluralism_equilibrium, "legal_philosophy/institutional_history/islamic_jurisprudence").

domain_priors:requires_active_enforcement(institutional_pluralism_equilibrium).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(institutional_pluralism_equilibrium, '076fedf1-885b-40b1-bff5-7f008fb435bb').
narrative_ontology:cs_created_at('076fedf1-885b-40b1-bff5-7f008fb435bb', '').
narrative_ontology:cs_kernel_codification('076fedf1-885b-40b1-bff5-7f008fb435bb', fixed_text).
narrative_ontology:cs_authority_grounding('076fedf1-885b-40b1-bff5-7f008fb435bb', lineage).
narrative_ontology:cs_interpretation_layer_present('076fedf1-885b-40b1-bff5-7f008fb435bb').
narrative_ontology:cs_kernel_id(institutional_pluralism_equilibrium, jurisprudential_method_kernel).
narrative_ontology:cs_axiom('076fedf1-885b-40b1-bff5-7f008fb435bb', foundational, ijtihaad_methodologically_closed).
narrative_ontology:cs_axiom_status(ijtihaad_methodologically_closed, holdable).
narrative_ontology:cs_axiom_grounding('076fedf1-885b-40b1-bff5-7f008fb435bb', ijtihaad_methodologically_closed, conventional).
narrative_ontology:cs_axiom('076fedf1-885b-40b1-bff5-7f008fb435bb', foundational, madhab_equality_methodologically_binding).
narrative_ontology:cs_axiom_status(madhab_equality_methodologically_binding, holdable).
narrative_ontology:cs_axiom_grounding('076fedf1-885b-40b1-bff5-7f008fb435bb', madhab_equality_methodologically_binding, conventional).
narrative_ontology:cs_reference_frame('076fedf1-885b-40b1-bff5-7f008fb435bb', madhab_methodological_pluralism).
narrative_ontology:cs_drift_state('076fedf1-885b-40b1-bff5-7f008fb435bb', contemporary_nation_state_legal_systems, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_pluralism_equilibrium, madhab_institutional_frameworks).
narrative_ontology:constraint_beneficiary(institutional_pluralism_equilibrium, established_scholarly_lineages).
narrative_ontology:constraint_victim(institutional_pluralism_equilibrium, heterodox_legal_interpretation).
narrative_ontology:constraint_victim(institutional_pluralism_equilibrium, individual_reasoners_outside_madhhabs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HETERODOX INTERPRETER (SNARE) — An individual jurist or scholar attempting ijtihaad outside the madhab institutional framework faces institutional delegitimation, exclusion from scholarly networks, and loss of community standing. Exit is structurally mobile (they have intellectual capacity, access to sources) but identity-locked: their authority claim depends on recognition within the Islamic scholarly tradition, which the madhab-bound institutions control. Suppression manifests as epistemic authority gatekeeping and social sanction.
constraint_indexing:constraint_classification(institutional_pluralism_equilibrium, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: SECONDARY-LEVEL SCHOLAR (TANGLED ROPE) — Scholars embedded within madhab structures gain genuine coordination benefits (mentorship networks, methodological training, access to juridical inheritance) while experiencing asymmetric extraction (limited discretion in interpretation, subordination to madhab consensus, career vulnerability if deviating). Exit is constrained by professional costs and reputational damage, but conceivable.
constraint_indexing:constraint_classification(institutional_pluralism_equilibrium, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MADHAB INSTITUTIONAL FRAMEWORK (ROPE) — The Hanafi, Maliki, Shafi'i, and Hanbali schools experience the constraint as a pure coordination mechanism. The pluralism equilibrium stabilizes legal authority, enables scholarly transmission across generations, organizes jurisprudential training, and provides institutional home for Islamic legal reasoning. These institutions have near-complete arbitrage options — they can shift doctrine, modify practices, or invoke historical flexibility without loss of core institutional identity. Effective extraction is minimal; the constraint serves coordination.
constraint_indexing:constraint_classification(institutional_pluralism_equilibrium, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM MOVEMENT (TANGLED ROPE) — Late 19th/early 20th century Salafi and modernist reformers attempted to bypass madhab structures through direct engagement with sources (Quran, Hadith, earliest jurisprudential reasoning). These movements experienced genuine coordination benefits (they unified disparate reform initiatives under coherent jurisprudential logic) while facing extraction costs (institutional opposition, accusations of bid'ah, loss of traditional scholarly legitimacy). Their exit option is constrained by the need to maintain Islamic legal authority and community acceptance — they cannot simply abandon the tradition.
constraint_indexing:constraint_classification(institutional_pluralism_equilibrium, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: INSTITUTIONAL PLURALISM THEATER (PITON) — From the longest historical horizon, the madhab pluralism appears substantially performative. The institutional fiction that all four schools are equally valid, that their differences are methodological refinements rather than substantive contradictions, and that jurisprudential closure occurred in the 10th-11th centuries requires continuous maintenance. The theater persists because the institutions derive legitimacy from it, yet the functional reasoning within each madhab often proceeds as if other schools' valid conclusions do not constitute binding alternatives. Theater ratio reflects this: the constraint maintains its ritual form while core coordination function has partially atrophied into institutional territoriality.
constraint_indexing:constraint_classification(institutional_pluralism_equilibrium, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical perspective, institutional pluralism appears as an immutable natural law of Islamic jurisprudence: the irreducibility of ijtihaad combined with the absence of centralized legislative authority logically necessitates multiple valid schools. Methodological diversity is inherent to any legal tradition without supreme lawmaking authority. This perspective risks naturalizing a contingent institutional arrangement — the specific madhab structures and their particular equilibrium — as a law of reasoning itself.
constraint_indexing:constraint_classification(institutional_pluralism_equilibrium, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_pluralism_equilibrium_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_pluralism_equilibrium, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_pluralism_equilibrium, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_pluralism_equilibrium, TR),
    TR >= 0.70.

:- end_tests(institutional_pluralism_equilibrium_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts through institutional gatekeeping of interpretive authority and suppression of heterodox ijtihaad, but the extraction is not maximal because madhab structures genuinely coordinate jurisprudential training, scholarly transmission, and legal stability. The beneficiaries (madhab institutions) derive real value from coordination, not solely from exclusion. The heterodox victims experience identity-locked suppression rather than purely material extraction. Suppression (0.52): Moderate-high. Institutional barriers to heterodox interpretation are substantial — social sanction, network exclusion, loss of scholarly legitimacy — but not total. Individual scholars can and do work outside madhab frameworks, though at high cost. The suppression mechanism is enforcement of institutional boundaries through authority gatekeeping. Theater ratio (0.48): Moderate. The institutional claim that all four madhabs are equally valid methodological approaches is partially true (they do differ in method) and partially performative (the claim obscures substantive doctrinal differences and treats institutional equilibrium as if it were logical necessity). The theater has increased slightly from 0.35 to 0.48 as modernization has required more explicit justification of the madhab system's continued relevance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same institutional structure produces radically different classifications depending on structural position. The madhab institutions see pure coordination (Rope) — they are solving the genuine problem of organizing jurisprudential authority without centralized legislation. The established scholars see coordination with benefits (Rope/Tangled Rope) — they gain from the institutional inheritance and methodological training. The heterodox interpreters see pure extraction (Snare) — they are excluded from authority networks and face identity-level suppression. The reform movements see extraction mixed with coordination benefits (Tangled Rope) — they attempted alternative coordination but faced institutional opposition. The institutional theater itself appears degraded/performative (Piton) — the claim of permanent methodological pluralism requires continuous maintenance despite changed historical conditions. The analytical observer risks seeing an immutable natural law (Mountain) — that methodological pluralism is inherent to Islamic jurisprudence absent centralized legislation — but the structural data reveals this as a false summit: the specific madhab institutional structures and their particular equilibrium are contingent historical arrangements, not logical necessities.
 *
 * DIRECTIONALITY LOGIC:
 *   The derived directionality values reflect each agent's structural position within the constraint. Madhab institutions as beneficiaries with arbitrage options experience low d → negative effective extraction (they experience it as pure coordination). Heterodox interpreters as victims with identity-locked exits experience high d → high f(d) (they bear maximum extraction but experience it as loss of identity/legitimacy, not primarily material loss). Secondary scholars experience moderate d (they are constrained victims-cum-beneficiaries) → moderate chi. The reform movements, attempting to bypass madhab structures while remaining within Islamic jurisprudential tradition, experience constrained exit options; their d values are moderate-high. The institutional piton perspective derives from the theater gate rather than from high chi — the constraint is performative institutional maintenance rather than purely extractive.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijtihaad_closure_status,
    'Was jurisprudential reasoning (ijtihaad) genuinely closed at the institutional level in the 10th-11th centuries, or is ''closure'' itself a retrospective institutional narrative?',
    'Comparative textual analysis of classical period versus post-Ghazali jurisprudential practice; examination of whether scholars actually treated ijtihaad as closed or merely worked within established madhab frameworks',
    'If genuinely closed: the institutional pluralism reading holds; multiple valid schools are the permanent institutional structure. If narrative: the constraint is institutional gatekeeping disguised as natural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijtihaad_closure_status, empirical, 'Whether ijtihaad closure was actual institutional fact or retrospective narration').

omega_variable(
    methodological_versus_substantive_pluralism,
    'Do the madhabs differ only in jurisprudential method, or do they instantiate fundamentally different substantive visions of Islamic law?',
    'Systematic comparison of madhab conclusions across 50+ major juridical questions; analysis of whether differences can be reduced to methodological variation or require substantive normative divergence',
    'If methodological: institutional pluralism is a stable equilibrium of valid alternative approaches. If substantive: the schools represent competing legal visions; claiming equal validity is institutional theater masking jurisdictional conflict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(methodological_versus_substantive_pluralism, conceptual, 'Scope of madhab divergence: methodological vs substantive').

omega_variable(
    heterodox_ijtihaad_suppression_mechanism,
    'Is the suppression of heterodox interpretation (individual ijtihaad outside madhabs) a structural feature of Islamic jurisprudence or a contingent institutional interest?',
    'Textual examination of juridical foundations for madhab authority; comparison with non-madhab-bound traditions (informal jurisprudential practice, mufti networks, hadith scholars); analysis of whether suppression derives from Islamic legal theory or institutional self-preservation',
    'If structural: heterodox interpretation is genuinely foreclosed by Islamic jurisprudential logic. If institutional: suppression is extractive gatekeeping, and the constraint is a snare from the heterodox perspective, not a mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(heterodox_ijtihaad_suppression_mechanism, conceptual, 'Whether heterodox suppression is jurisprudentially necessary or institutionally contingent').

omega_variable(
    madhab_institutional_flexibility,
    'Can madhab structures accommodate substantial doctrinal innovation without institutional collapse, or does innovation require exit from the madhab framework?',
    'Historical case studies of major madhab doctrinal shifts (Ottoman legal reform, contemporary fatwa adaptations); analysis of whether these occurred within madhab structures or required new institutional frameworks (fatwa bureaus, reform movements, state legal codes)',
    'If flexible: madhab institutional structures are genuine coordination mechanisms with real innovation capacity. If rigid: the constraint is institutional gatekeeping preventing doctrinal evolution; the rope classification is inaccurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(madhab_institutional_flexibility, empirical, 'Madhab institutional capacity for doctrinal change').

omega_variable(
    modern_state_law_interaction,
    'Does the emergence of nation-state legal systems and constitutional frameworks represent a genuine bifurcation of Islamic jurisprudence, or do madhab structures retain authority within their modified institutional contexts?',
    'Analysis of modern Islamic states'' jurisprudential authority structures (Egypt, Malaysia, Saudi Arabia); examination of whether madhab jurisprudence persists as binding authority or becomes cultural-historical reference',
    'If bifurcation: the institutional pluralism equilibrium described in this constraint is post-20th-century specific. If retention: madhab authority has adapted to coexist with state legal systems; the constraint operates across plural legal domains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(modern_state_law_interaction, empirical, 'Status of madhab authority in modern state legal systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_pluralism_equilibrium, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ipe_tr_t0, institutional_pluralism_equilibrium, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ipe_tr_t4, institutional_pluralism_equilibrium, theater_ratio, 4, 0.42).
narrative_ontology:measurement(ipe_tr_t8, institutional_pluralism_equilibrium, theater_ratio, 8, 0.48).

% Extraction over time
narrative_ontology:measurement(ipe_be_t0, institutional_pluralism_equilibrium, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ipe_be_t4, institutional_pluralism_equilibrium, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(ipe_be_t8, institutional_pluralism_equilibrium, base_extractiveness, 8, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_pluralism_equilibrium, identity_coordination).
narrative_ontology:affects_constraint(institutional_pluralism_equilibrium, islamic_legal_authority_transmission).
narrative_ontology:affects_constraint(institutional_pluralism_equilibrium, hadith_chain_epistemology).

% DUAL FORMULATION NOTE:
% The institutional pluralism equilibrium is one reading of how Islamic jurisprudential authority operates. Sibling readings (reform movements, heterodox interpretations, state legal system integration) would constitute separate constraint stories with different epsilon values and different beneficiary/victim structures. This story treats madhab pluralism as functionally stable and institutionally enforced; alternative readings would challenge either the stability (showing madhab decline) or the enforcement (showing that suppression is not absolute).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_pluralism_equilibrium, institutional, 0.08).
constraint_indexing:directionality_override(institutional_pluralism_equilibrium, powerless, 0.91).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
