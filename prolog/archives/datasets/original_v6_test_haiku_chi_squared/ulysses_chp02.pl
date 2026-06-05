% ============================================================================
% CONSTRAINT STORY: ulysses_chp02
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ulysses_chp02, []).

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
 *   constraint_id: ulysses_chp02
 *   human_readable: The Nightmare of History (Dalkey School)
 *   domain: economic/social/political
 *
 * SUMMARY:
 *   Stephen Dedalus arrives at Dalkey School in 1902 as a sensitive boy
 *   immersed in Catholic teaching and emerging Irish consciousness. The
 *   school, a Catholic institution in British-occupied Ireland, presents
 *   itself as an educational space designed to form character and transmit
 *   civilization. But for Stephen, the school becomes a site of profound
 *   extraction: the curriculum enforces a 'nightmare of history' — a
 *   colonially-mediated understanding of Irish identity, Catholic authority,
 *   and the boy's own consciousness that denies him intellectual autonomy.
 *   The constraint is not primarily economic (though fees extract resources)
 *   nor purely ideological (though ideology is its mechanism) — it is the
 *   systematic suppression of indigenous intellectual formation through the
 *   apparatus of education. Stephen's torment at Dalkey crystallizes the
 *   structural problem: the school promises liberation through knowledge but
 *   delivers subjection through doctrine. The 'nightmare' is not that history
 *   is painful, but that the curriculum forces boys to internalize a version
 *   of history written by their colonizers and their colonizers' priests. By
 *   the constraint's end (Stephen's rejection of the school), extractiveness
 *   has risen from 0.52 to 0.68, and theater from 0.48 to 0.65 — the
 *   performative gap between the school's stated purpose and its actual
 *   function has widened.
 *
 * KEY AGENTS:
 *   - Stephen Dedalus: Primary victim (powerless/trapped) — bears the full burden of the nightmare; has no structural exit except complete rejection of the educational pathway
 *   - Catholic Ecclesiastical Hierarchy: Primary beneficiary (institutional/arbitrage) — maintains doctrinal control over Irish intellectual formation; can reallocate resources or shift strategy
 *   - British Colonial Administrative State: Secondary beneficiary (institutional/arbitrage) — indirectly controls Irish consciousness through curriculum; ensures compliance through institutional mediation
 *   - Irish Middle-Class Parent: Constrained hybrid (moderate/constrained) — wants son's social mobility but victimized by the constraint's content; cannot exit without losing standing
 *   - Irish Literary-Nationalist Movement: Organized resistance (organized/constrained) — recognizes the school as a temporary problem; building alternative intellectual pathways through cultural reclamation
 *   - Imperial Educational Doctrine: Performative authority (institutional/arbitrage) — the school's stated purpose has degraded into theater; maintained through inertia rather than function
 *   - Irish Intellectual Autonomy: Structural victim (powerless/trapped) — abstract collective that cannot exit; the entire Irish intellectual tradition is trapped within a colonial curriculum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ulysses_chp02, 0.68).
domain_priors:suppression_score(ulysses_chp02, 0.72).
domain_priors:theater_ratio(ulysses_chp02, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ulysses_chp02, extractiveness, 0.68).
narrative_ontology:constraint_metric(ulysses_chp02, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ulysses_chp02, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ulysses_chp02, snare).
narrative_ontology:human_readable(ulysses_chp02, "The Nightmare of History (Dalkey School)").
narrative_ontology:topic_domain(ulysses_chp02, "economic/social/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ulysses_chp02, catholic_ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(ulysses_chp02, colonial_british_educational_establishment).
narrative_ontology:constraint_victim(ulysses_chp02, stephen_dedalus).
narrative_ontology:constraint_victim(ulysses_chp02, schoolboy_consciousness).
narrative_ontology:constraint_victim(ulysses_chp02, irish_intellectual_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STEPHEN DEDALUS (SNARE) — Powerless student with no exit option except total rejection of education itself. Bears the full burden of the 'nightmare of history': forced to internalize colonial authority, Catholic doctrine, and the weight of an inescapable past. Cannot walk away without social and economic devastation. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.65.
constraint_indexing:constraint_classification(ulysses_chp02, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: IRISH INTELLECTUAL AUTONOMY (SNARE) — Abstract collective victim (like field epistemic reliability in the verification bottleneck). The Irish intellectual tradition is trapped within a colonial curriculum designed to suppress native intellectual formation. The 'nightmare of history' is a mechanism that prevents the colonized from thinking freely about their own past. d≈0.96, f(d)≈1.42, σ=1.1 → χ≈0.68.
constraint_indexing:constraint_classification(ulysses_chp02, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CATHOLIC CHURCH (ROPE) — Institutional beneficiary with arbitrage options (can reallocate educational resources, shift curricula, exit if politically advantageous). Experiences the school as a coordination mechanism: maintaining doctrinal unity, controlling narrative transmission, ensuring generational compliance. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.07. Net beneficiary; extraction is negative because the constraint subsidizes church authority.
constraint_indexing:constraint_classification(ulysses_chp02, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: BRITISH COLONIAL STATE (ROPE) — Institutional beneficiary via indirect control through educational curriculum. Experiences the school as coordination of colonial subjection: teaching English history as universal history, naturalizing British political structures, ensuring Irish compliance through intellectual formation. d≈0.05, f(d)≈-0.12, σ=1.1 → χ≈-0.08. Net beneficiary; extraction is subsidized by colonial administrative capacity.
constraint_indexing:constraint_classification(ulysses_chp02, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: IRISH MIDDLE-CLASS PARENT (TANGLED ROPE) — Moderate power, constrained exit. Wants son educated for social mobility but recognizes the curriculum as colonial subjection. Benefits from credential pathway (son becomes educated) but victimized by the constraint's content (son's consciousness is trapped in 'nightmare of history'). Cannot exit without losing social standing; cannot fully embrace the system without moral compromise. d≈0.58, f(d)≈0.72, σ=1.0 → χ≈0.49.
constraint_indexing:constraint_classification(ulysses_chp02, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: IRISH LITERARY-NATIONALIST MOVEMENT (SCAFFOLD) — Organized agents (Yeats, Lady Gregory, nationalist educators) see the school as a temporary institutional problem with a sunset: Irish-language education, indigenous curriculum, and literary revival are building alternative intellectual pathways. The 'nightmare of history' can be rewritten through cultural reclamation. d≈0.42, f(d)≈0.41, σ=1.1 → χ≈0.19. Low effective extraction because the movement has agency, recognizes the constraint as contingent, and sees an exit path through cultural reconstruction.
constraint_indexing:constraint_classification(ulysses_chp02, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: IMPERIAL EDUCATIONAL DOCTRINE (PITON) — The school's proclaimed purpose (to educate, to form character, to transmit civilization) is substantially performative by 1904. The system persists through institutional inertia and lack of alternatives, not because it functions. Teachers enforce an ideology they themselves doubt. The 'nightmare' is maintained because the colonial apparatus has not yet built a replacement. theater_ratio=0.65 approaches the piton gate (≥0.70); if theater rises to 0.72, the classification locks as degraded. d≈0.06, f(d)≈-0.11, σ=1.1 → χ≈-0.04.
constraint_indexing:constraint_classification(ulysses_chp02, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the school at Dalkey is a structurally typical mechanism of colonial subjection: education designed to prevent indigenous intellectual formation while appearing to transmit universal culture. The 'nightmare of history' is not a regrettable pedagogical failure but a structural feature of colonialism itself. This perspective confirms the snare classification across all scales: local teaching, national curriculum, global imperial hierarchy. d≈0.74, f(d)≈1.15, σ=1.1 → χ≈0.57.
constraint_indexing:constraint_classification(ulysses_chp02, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ulysses_chp02_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ulysses_chp02, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ulysses_chp02, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ulysses_chp02, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ulysses_chp02, TR),
    TR >= 0.70.

:- end_tests(ulysses_chp02_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The school extracts intellectual autonomy from Stephen through three mechanisms: (1) Imposed historical narrative that denies Irish agency (the 'nightmare' is colonially-written history); (2) Suppression of indigenous intellectual traditions (Irish language, native philosophy, local history are absent or denigrated); (3) Institutional channeling of ambition toward conformity (credential pathway requires acceptance of the nightmare). The 0.68 value reflects that this is not maximum extraction (0.92+) — some boys benefit from credentials, some teachers perform their duties with genuine commitment, and Stephen does achieve eventual clarity about the constraint. But extraction is severe because it operates on consciousness itself, not merely on resources. Suppression (0.72): High. Barriers to intellectual autonomy include: Catholic doctrinal authority (nonrefusable without family rupture), British imperial curriculum (presented as universal standard), lack of alternative educational institutions (in 1902 Ireland, Dalkey is one of few options for middle-class boys), career consequences of rejection (abandoning education means abandoning social mobility), and psychological internalization (boys absorb the constraint's logic into their own self-understanding). Theater ratio (0.65): Moderate-high. The school claims to educate and form character, but teachers themselves recognize the futility — Haines and other instructors understand they are teaching boys who 'lack innocence,' who are already morally compromised by the system they're meant to enter. The performative content is high because the school's real function (social reproduction through doctrinal compliance) is masked by its stated purpose (character formation through humanistic education). The gap widens over the interval as Stephen's clarity reveals the theater: what appears to be education is actually subjection.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates the full spectrum of DR classifications across perspectives. Stephen sees pure extraction (Snare) — the system traps him in a nightmare he did not choose and cannot escape without radical rejection. The Church sees coordination (Rope) — the school solves the problem of maintaining doctrinal unity and generational compliance. The British colonial state sees coordination (Rope) — the school ensures Irish intellectual compliance through institution-mediated consent. The Irish middle-class parent sees mixed extraction and coordination (Tangled Rope) — the school enables his son's advancement but compromises his son's consciousness. The literary-nationalist movement sees a temporary problem with an exit path (Scaffold) — cultural reclamation is building alternatives. The imperial doctrine sees its own degradation (Piton) — the school's stated purpose has atrophied, leaving only the ritual. The analytical observer sees the snare confirmed across all scales: the 'nightmare of history' is not a local failure but a structural feature of colonialism itself. The perspectival gap is widest between Stephen (Snare) and the Church (Rope): they inhabit the same constraint but experience it with opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Stephen Dedalus: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. No exit option except total rejection; bears the full psychological burden of the constraint. Catholic Church: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Can reallocate educational resources, shift doctrinal priorities, or exit the education business without existential threat. British Colonial State: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net beneficiary. Controls the institutional apparatus; can change curriculum, create alternatives, or shift colonial strategy. Irish Middle-Class Parent: Victim + beneficiary + constrained → d≈0.58, f(d)≈0.72. Mixed extraction. Trapped between desire for son's advancement (benefits from school) and recognition of constraint's extractiveness (victimized by curriculum). Cannot exit cleanly without social cost. Literary-Nationalist Movement: Organized + constrained → d≈0.42, f(d)≈0.41. Lower effective extraction. Has agency, recognizes constraint as contingent, can build alternatives. Irish Intellectual Autonomy: Victim + trapped → d≈0.96, f(d)≈1.42. Maximum extraction. Abstract collective cannot organize, cannot exit, bears permanent cost of intellectual subjection.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves potential mandatrophy (mislabeling pure extraction as coordination) by confronting the beneficiary/victim structure directly. The school's stated function (coordination: educating boys, forming character, transmitting culture) is genuine from the Church's and State's perspectives — they do achieve their coordination goals (doctrinal unity, colonial compliance). But the actual cost of this coordination falls entirely on Stephen and on Irish intellectual autonomy — the 'nightmare of history' is precisely the mechanism that makes the coordination functional. From Stephen's perspective, there is no coordination benefit at all: he gains credentials but loses intellectual autonomy, a net loss of (d≈0.92) magnitude. The mandatrophy is resolved by recognizing that Rope (from the beneficiary's view) and Snare (from the victim's view) are not contradictory but complementary readings of a single extractive mechanism. The Church experiences coordination; Stephen experiences subjection. Both are correct. The constraint is snare because the extractiveness (0.68) and suppression (0.72) exceed the thresholds for pure coordination — the 'coordination' achieved by the Church is purchased with Stephen's intellectual autonomy. This is the definition of Tangled Rope at minimum severity, Snare at maximum severity. The analytical perspective confirms: the school is a snare of colonialism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nightmare_content_specificity,
    'Is the ''nightmare of history'' a specific Catholic/colonial doctrine imposed by the school, or a universal human burden that the school merely fails to alleviate?',
    'Textual analysis: compare curriculum content (what is actually taught) against Stephen''s internal experience (what he internalizes); contrast with alternative curricula (Montessori, Irish nationalist schools) to isolate the extractive content from the human condition',
    'If imposed doctrine: snare classification holds (extraction is deliberate). If universal burden: school appears as Piton (failing to solve an inherent problem) rather than Snare (actively creating victimization).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nightmare_content_specificity, empirical, 'Whether the nightmare is imposed doctrine or universal condition').

omega_variable(
    stephen_agency_trajectory,
    'Does Stephen''s eventual rejection of the school represent successful exit or merely delayed compliance with a deeper layer of the constraint?',
    'Longitudinal textual analysis: does Stephen''s ''Non serviam'' declaration actually free him from the nightmare, or does it trap him in reactive opposition? Compare his later choices (self-exile, artistic aspiration) against the constraint''s continued hold on his consciousness.',
    'If exit is successful: snare classification weakens (victim achieves escape velocity). If delayed compliance: snare classification strengthens (even rejection becomes a form of internalization; the nightmare persists through opposition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stephen_agency_trajectory, conceptual, 'Whether Stephen achieves genuine exit or internalized opposition').

omega_variable(
    colonial_educational_extractiveness_variance,
    'How much of the extractiveness (0.68) is specific to Catholic-colonial coupling, and how much is structural to all systems of credential-mediated social control?',
    'Comparative institutional analysis: contrast Dalkey school against contemporary English public schools, German gymnasiums, and post-colonial alternative schools; measure the variance in suppressiveness and theater_ratio across these systems',
    'If variance is low (all credential systems extract similarly): the constraint is a universal feature of education, not specific to colonialism. If variance is high (Dalkey is exceptionally extractive): the snare classification is strengthened as a deliberate colonial mechanism rather than a generic institutional problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_educational_extractiveness_variance, empirical, 'Extractiveness variance across educational systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ulysses_chp02, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ulysses_tr_t0, ulysses_chp02, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ulysses_tr_t2, ulysses_chp02, theater_ratio, 2, 0.57).
narrative_ontology:measurement(ulysses_tr_t4, ulysses_chp02, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(ulysses_be_t0, ulysses_chp02, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(ulysses_be_t2, ulysses_chp02, base_extractiveness, 2, 0.6).
narrative_ontology:measurement(ulysses_be_t4, ulysses_chp02, base_extractiveness, 4, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ulysses_chp02, enforcement_mechanism).
narrative_ontology:affects_constraint(ulysses_chp02, irish_linguistic_suppression).
narrative_ontology:affects_constraint(ulysses_chp02, colonial_cultural_hierarchy).
narrative_ontology:affects_constraint(ulysses_chp02, catholic_clerical_authority_ireland).

% DUAL FORMULATION NOTE:
% The nightmare of history at Dalkey School is downstream of broader colonial educational doctrine but represents a distinct structural constraint operating at the institutional scale. The constraint family includes: (1) colonial_cultural_hierarchy (ε≈0.35, Tangled Rope) — the overarching imperial framework; (2) ulysses_school_1904 (ε≈0.68, Snare) — the specific school mechanism; (3) irish_linguistic_suppression (ε≈0.45, Snare) — the curriculum's exclusion of Irish language. Each has distinct ε values reflecting empirical specificity. The school constraint is downstream (affected by) the broader hierarchy but upstream (affecting) the linguistic suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
