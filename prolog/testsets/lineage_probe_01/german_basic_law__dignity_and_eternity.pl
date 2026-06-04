% ============================================================================
% CONSTRAINT STORY: german_basic_law__dignity_and_eternity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_german_basic_law__dignity_and_eternity, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: german_basic_law__dignity_and_eternity
 *   human_readable: German Basic Law Article 1 & 79(3): Inviolable Human Dignity and Unamendable Core
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Basic Law (Grundgesetz) of 1949 embeds a structural principle that
 *   transcends ordinary constitutional amendment: Article 1 declares human
 *   dignity inviolable, and Article 79(3) makes this core — along with
 *   federalism and democratic principle — unamendable. This constraint
 *   represents a post-Nazi decision to lock the constitutional order against
 *   future majorities that might overturn dignitarian protections. The
 *   unamendable floor is not presented as a natural law but as a deliberate
 *   self-binding commitment. However, the phenomenology of the constraint
 *   varies dramatically across perspectives: from the dignity-holder's view,
 *   it appears as an absolute law of the constitutional order; from the
 *   would-be authoritarian reformer's view, it appears as extractive
 *   suppression of political possibility; from the supermajority's view, it
 *   appears as a coordination mechanism protecting against future erosion.
 *   This constraint is a kernel reading — one interpretation of the contested
 *   German constitutional project. It competes with sibling readings
 *   emphasizing amendment history (the Basic Law as a record of managed
 *   reform), the basic rights catalog (Article 1 within a broader rights
 *   framework), federal construction (Länder autonomy eternity-protected),
 *   and militant democracy (the state's right to defend itself against
 *   democracy's enemies). The dignity-and-eternity reading claims that the
 *   core is truly beyond politics, that no amendment process — however
 *   supermajority — can touch it.
 *
 * KEY AGENTS:
 *   - Dignity Holders: Any person claiming constitutional protection under Article 1. Universally beneficiaries of the constraint; trapped by its unamendability but protected by it.
 *   - The Bundesverfassungsgericht (Constitutional Court): Institutional custodian tasked with enforcing Article 79(3). Experiences the constraint as mandatory and non-negotiable; enforces it against any attempted amendment.
 *   - Constitutional Supermajorities: Parliamentary coalitions with 2/3 majorities can amend Articles 1–78 but cannot touch Articles 79(3) core. Experience the constraint as rope (credible self-binding for temporal protection).
 *   - Would-Be Authoritarian Reformers: Political forces seeking to overturn dignitarian protections or suppress dissent. Experience the constraint as snare (foreclosed political possibility with high suppression).
 *   - The Analytical Observer: Civilizational perspective evaluating whether the constraint is truly natural law or a contingent political choice embedded in a 78-year-old constitutional text.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(german_basic_law__dignity_and_eternity, 0.08).
domain_priors:suppression_score(german_basic_law__dignity_and_eternity, 0.02).
domain_priors:theater_ratio(german_basic_law__dignity_and_eternity, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(german_basic_law__dignity_and_eternity, extractiveness, 0.08).
narrative_ontology:constraint_metric(german_basic_law__dignity_and_eternity, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(german_basic_law__dignity_and_eternity, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(german_basic_law__dignity_and_eternity, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(german_basic_law__dignity_and_eternity, resistance, 0.04).

% --- Constraint claim ---
narrative_ontology:constraint_claim(german_basic_law__dignity_and_eternity, mountain).
narrative_ontology:human_readable(german_basic_law__dignity_and_eternity, "German Basic Law Article 1 & 79(3): Inviolable Human Dignity and Unamendable Core").
narrative_ontology:topic_domain(german_basic_law__dignity_and_eternity, "constitutional/political").

domain_priors:emerges_naturally(german_basic_law__dignity_and_eternity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(german_basic_law__dignity_and_eternity, '66c86c46-4bcd-409c-aec0-994219338d1c').
narrative_ontology:cs_kernel_codification('66c86c46-4bcd-409c-aec0-994219338d1c', fixed_text).
narrative_ontology:cs_authority_grounding('66c86c46-4bcd-409c-aec0-994219338d1c', lineage).
narrative_ontology:cs_interpretation_layer_present('66c86c46-4bcd-409c-aec0-994219338d1c').
narrative_ontology:cs_reading_relation('66c86c46-4bcd-409c-aec0-994219338d1c', german_basic_law__amendment_history, coexists_with).
narrative_ontology:cs_reading_relation('66c86c46-4bcd-409c-aec0-994219338d1c', german_basic_law__basic_rights_catalog, influences).
narrative_ontology:cs_reading_relation('66c86c46-4bcd-409c-aec0-994219338d1c', german_basic_law__federal_construction, coexists_with).
narrative_ontology:cs_reading_relation('66c86c46-4bcd-409c-aec0-994219338d1c', german_basic_law__militant_democracy, coexists_with).
narrative_ontology:cs_axiom('66c86c46-4bcd-409c-aec0-994219338d1c', foundational, dignity_categorically_inviolable).
narrative_ontology:cs_axiom_status(dignity_categorically_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('66c86c46-4bcd-409c-aec0-994219338d1c', dignity_categorically_inviolable, deontological).
narrative_ontology:cs_axiom('66c86c46-4bcd-409c-aec0-994219338d1c', secondary, supermajority_cannot_reach_core).
narrative_ontology:cs_axiom_status(supermajority_cannot_reach_core, holdable).
narrative_ontology:cs_axiom_grounding('66c86c46-4bcd-409c-aec0-994219338d1c', supermajority_cannot_reach_core, conventional).
narrative_ontology:cs_created_at('66c86c46-4bcd-409c-aec0-994219338d1c', '').
narrative_ontology:cs_kernel_id(german_basic_law__dignity_and_eternity, german_basic_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(german_basic_law__dignity_and_eternity, dignity_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIGNITY HOLDER (MOUNTAIN) — From the position of any person claiming constitutional protection of human dignity, Article 1 under Article 79(3) appears as an immutable natural law of the constitutional order itself. No political majority can reach this core; no amendment process can suspend it. The dignity-holder's protection is absolute, unchangeable, and independent of any temporal political calculation. This is the mountain from below.
constraint_indexing:constraint_classification(german_basic_law__dignity_and_eternity, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: ANALYTICAL OBSERVER (MOUNTAIN) — From a structural analysis perspective, Article 79(3) embeds a self-binding commitment: the constitutional framers locked future constitutional politics out of the core by declaring the dignity clause unamendable. This is not a claim of natural law but a claim of structural law — a political choice to create unchangeability through constitutional form. The immutability is real and enforced through the constitutional text itself. Accessibility collapse is near-total: no political process can revise the core; resistance is near-zero because the constraint operates through formal prohibition, not suppression of alternatives.
constraint_indexing:constraint_classification(german_basic_law__dignity_and_eternity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 3: CONSTITUTIONAL COURT (MOUNTAIN) — From the vantage of the Bundesverfassungsgericht, Article 79(3) is both a constraint and an authority. The court is tasked with enforcing the unamendable core — it must reject any amendment that touches the dignity clause or federalism or democratic principle. This enforcement is not discretionary; it is mandatory. The court experiences the constraint as an absolute boundary that structures all its future decisions. The constraint has zero extractiveness toward the court (the court is not a victim) and zero suppression (the constraint operates openly through the constitutional text). This is a mountain where the institutional custodian has perfect clarity about its immutability.
constraint_indexing:constraint_classification(german_basic_law__dignity_and_eternity, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL SUPERMAJORITY (ROPE) — From the position of a supermajority attempting constitutional reform, Article 79(3) is experienced as a genuine coordination mechanism. The eternity clause solves a coordination problem: it credibly commits the polity against future majorities eroding the dignitarian core. A supermajority can undertake many constitutional reforms (Articles 79(1)–(2) allow amendment with 2/3 votes), but not core amendments. This constraint is experienced as rope from this perspective because the supermajority benefits from the same protection it cannot revise — the eternity clause protects against the next constitutional moment when they become the minority. Low extractiveness, low suppression. The constraint coordinates across temporal boundaries by making credible commitments to future generations.
constraint_indexing:constraint_classification(german_basic_law__dignity_and_eternity, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: WOULD-BE AUTHORITARIAN REFORMER (SNARE) — From the position of a political force seeking to overturn dignitarian protections — to revoke citizenship based on ethnicity, to suppress dissent, to instrumentalize the state for ideological ends — the constraint is experienced as pure extraction with high suppression. The reformer cannot exit this constraint through amendment (Article 79(3) forecloses this path); cannot suppress the constraint's operation (it is embedded in the constitutional text and the court's mandatory enforcement); cannot coordinate around it (no supermajority vote reaches the core). The extractiveness is severe because it represents the indefinite foreclosure of a particular political project. This is snare from the position of the actor whose ambitions the constraint targets.
constraint_indexing:constraint_classification(german_basic_law__dignity_and_eternity, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(german_basic_law__dignity_and_eternity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(german_basic_law__dignity_and_eternity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(german_basic_law__dignity_and_eternity, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(german_basic_law__dignity_and_eternity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(german_basic_law__dignity_and_eternity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(german_basic_law__dignity_and_eternity, ExtMetricName, E),
    domain_priors:suppression_score(german_basic_law__dignity_and_eternity, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(german_basic_law__dignity_and_eternity),
    narrative_ontology:constraint_metric(german_basic_law__dignity_and_eternity, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(german_basic_law__dignity_and_eternity, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(german_basic_law__dignity_and_eternity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.08): Minimal. The constraint exhibits almost no extraction in the classical sense — no agent extracts resources, power, or value from dignity protection. The dignity clause benefits all and harms none directly. The low value reflects that this is a protective constraint (its function is to shield, not to extract). The small non-zero value (0.08 rather than 0.00) accounts for interpretive drag: the Constitutional Court's interpretations of dignity content have evolved over 78 years, and competing interpretations exist about whether social rights, asylum, or privacy constitute dignity. This minimal interpretive friction prevents a full 0.00 reading. SUPPRESSION (0.02): Near-zero. The constraint is enforced openly through the constitutional text and the court's transparent jurisprudence. No alternatives are hidden; the supermajority amendment pathway is explicit in Article 79(1)–(2), and the exception for the core is explicit in Article 79(3). The constraint operates through formal prohibition, not concealment. The tiny non-zero value (0.02) accounts for the fact that some political actors may not fully understand the constitutional barrier (illiteracy about constitutional law is a real social phenomenon), but this is external to the constraint's operation, not intrinsic to it. THEATER_RATIO (0.05): Minimal performativity. The constraint operates through direct constitutional text and mandatory court enforcement — there is no theatrical ritual maintaining the dignity clause. The tiny value (0.05) accounts for formal ceremonial aspects of constitutional oath-taking and court proceedings, but these are peripheral. This is one of the lowest theater ratios possible: the constraint's function is entirely structural, not performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural data produces radically different classifications across perspectives. The dignity holder and the analytical observer both classify as mountain, but from opposite directions: the dignity holder sees an unchangeable protection from below; the observer sees an unchangeable structural barrier. The constitutional court also classifies as mountain but experiences the constraint as an obligation (they must enforce it) rather than a threat. The supermajority classifies as rope because they benefit from the same binding they cannot revise — the eternity clause protects them against future degradation just as it protects the dignity holder. The would-be reformer classifies as snare because the constraint forecloses their political project entirely. The perspectival gap reveals that 'immutability' is experienced differently depending on whether you benefit from (rope), are protected by (mountain), must enforce (mountain), or are blocked by (snare) the constraint. The gap also reveals the false-summit risk: if this constraint is analyzed as a natural law (mountain from all perspectives), it naturalizes what is actually a post-Nazi political choice. The difference between rock and wall — between a natural law and a deliberate self-binding — is visible only from certain perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d-value is derived from the agent's structural relationship to the dignity protection. DIGNITY HOLDERS: structurally mobile (they can emigrate, can organize politically) but benefit from unlimited protection (d ≈ 0.10, beneficiary with mobile exit). The engine computes low d, and low chi results. CONSTITUTIONAL COURT: institutional (d ≈ 0.00, neutral custodian with no beneficiary/victim status) — d approaches institutional canonical value (~0.00). SUPERMAJORITY: institutional (d ≈ 0.20, slight beneficiary status) — the 2/3 coalition benefits from the binding against future degradation. WOULD-BE REFORMER: powerful (d ≈ 0.95, complete target) — the constraint entirely forecloses this actor's political project. The reformer is trapped (cannot exit the constitutional order) and has no arbitrage options (cannot amend around the barrier). ANALYTICAL OBSERVER: analytical (d ≈ 0.73, canonical analytical value) — no beneficiary/victim relationship, only structural observation. The directionality computation correctly produces the perspectival gap without requiring manual override.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy by being classified as a genuine mountain across multiple perspectives rather than as a disguised rope or snare. The dignity clause appears as a mountain because it truly is unamendable (zero degrees of freedom within the amendment framework), not because it has been misclassified. The false-summit risk is documented in the omegas: if the dignity clause benefits specific political coalitions and naturalizes those benefits as universal law, the constraint may be a snare disguised as a mountain. The mandatrophy is resolved by maintaining the mountain classification while documenting the false-summit risk as an omega variable. The constraint avoids the typical extraction-hiding strategy (presenting snare as rope) because dignity protection is genuinely non-extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_self_binding,
    'Is Article 79(3) a claim of natural law (dignity is inherently inviolable) or a political self-binding commitment (the polity has chosen to lock itself into dignitarian protection)?',
    'Genealogical and textual analysis: examine the constitutional framers'' debates (Parlamentarischer Rat, 1948–1949) to determine whether they grounded the unamendable core in natural law theory or in deliberate political choice. Compare the Basic Law''s approach to dignity with other post-Hobbesian constitutional traditions (US Bill of Rights, Universal Declaration, Canadian Charter) to identify whether the unamendable floor is a universal feature or a contingent choice specific to the German historical moment (post-Nazi reckoning).',
    'If natural law: the classification remains mountain regardless of future political circumstances — the constraint is truly unchangeable because it reflects necessary truths about human dignity. If self-binding: the classification is contingent — a future constitutional moment could in principle undertake total revision by discarding the Basic Law itself and adopting a new fundamental law. The distinction determines whether a successor constitution could legally abandon the dignity core.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_self_binding, conceptual, 'Whether the unamendable core is grounded in natural law or political self-binding').

omega_variable(
    total_revision_threshold,
    'Does Article 79(3) block only amendment within the framework of the Basic Law, or does it also prohibit total constitutional revision and replacement?',
    'Constitutional jurisprudence analysis: examine Bundesverfassungsgericht holdings on whether total constitutional replacement (adoption of a new fundamental law) would be barred by the same structural principles that bar Article 79(3) amendments. Test case: could a future majority adopt an entirely new constitution that lacks a dignity clause? Are there unwritten limits (the identity theory of the constitutional order) that survive total revision?',
    'If Article 79(3) blocks only amendments: a future supermajority could in principle discard the Basic Law entirely and adopt a new constitution without the dignity clause. The constraint''s immutability extends only to the amended-law pathway, not the total-revision pathway. If unwritten limits extend beyond Article 79(3): the dignity core is truly immutable — even total constitutional replacement is constrained by the constitutional identity that preceded the text. This would elevate the constraint from structural law (80-year-old self-binding commitment) to something approaching natural law (the foundational principle that survives constitutional death).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(total_revision_threshold, conceptual, 'Scope of Article 79(3) prohibition: amendment-only or total-revision-inclusive').

omega_variable(
    dignity_content_contestation,
    'Is the content of ''human dignity'' fixed, or does dignity''s legal meaning evolve through constitutional court interpretation?',
    'Jurisprudential mapping: catalog Bundesverfassungsgericht decisions expanding or contracting the scope of Article 1 protections (privacy, autonomy, social dignity, personality rights, asylum claims, bioethical boundaries). Identify whether the court''s expansions represent evolution of a fixed concept or reinterpretation of an inherently contested principle. Compare with originalist and living-constitution interpretive traditions.',
    'If dignity is fixed: the unamendable core has a definite, unchanging content — Article 79(3) locks in a specific normative commitment. If dignity evolves: the eternity clause locks in a principle whose content is internally contestable — the amendment process is barred, but the meaning-making process (constitutional interpretation) continues indefinitely. The former supports a stronger mountain classification; the latter complicates it by showing the unamendable core as a textual anchor for interpretive drift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dignity_content_contestation, conceptual, 'Whether dignity''s legal content is fixed or evolves through interpretation').

omega_variable(
    false_summit_candidate,
    'Does the dignity clause benefit identifiable groups (constitutional scholars, human rights organizations, immigration advocates, Holocaust memorial constituencies) in ways that naturalize what is actually a contested political commitment?',
    'Institutional beneficiary analysis: identify which actors and constituencies benefit from the dignity clause''s constitutional primacy. Examine whether the clause naturalizes protections that some political coalitions actively oppose (strict asylum limitations that conflict with dignity rhetoric, austerity that conflicts with social dignity, security measures that conflict with privacy rights). If beneficiaries exist and the clause naturalizes their preferences as natural law, false summit detection fires.',
    'If beneficiaries identified: the mountain classification is vulnerable to false-summit reclassification — the constraint may appear natural but actually reflects the interests of specific political coalitions. If no beneficiaries can be identified: the mountain holds — the dignity clause is genuinely post-ideological, or ideological only in the sense that all constitutional commitments are ideological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_candidate, empirical, 'Whether dignity clause benefits identifiable political coalitions, triggering false-summit detection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(german_basic_law__dignity_and_eternity, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gblaw_dignity_theater_t0, german_basic_law__dignity_and_eternity, theater_ratio, 0, 0.02).
narrative_ontology:measurement(gblaw_dignity_theater_t40, german_basic_law__dignity_and_eternity, theater_ratio, 40, 0.04).
narrative_ontology:measurement(gblaw_dignity_theater_t78, german_basic_law__dignity_and_eternity, theater_ratio, 78, 0.05).

% Extraction over time
narrative_ontology:measurement(gblaw_dignity_extract_t0, german_basic_law__dignity_and_eternity, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(gblaw_dignity_extract_t40, german_basic_law__dignity_and_eternity, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(gblaw_dignity_extract_t78, german_basic_law__dignity_and_eternity, base_extractiveness, 78, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(german_basic_law__dignity_and_eternity, enforcement_mechanism).
narrative_ontology:affects_constraint(german_basic_law__dignity_and_eternity, german_basic_law__amendment_history).
narrative_ontology:affects_constraint(german_basic_law__dignity_and_eternity, german_basic_law__basic_rights_catalog).
narrative_ontology:affects_constraint(german_basic_law__dignity_and_eternity, german_basic_law__federal_construction).
narrative_ontology:affects_constraint(german_basic_law__dignity_and_eternity, german_basic_law__militant_democracy).

% DUAL FORMULATION NOTE:
% The German Basic Law is a contested kernel with five structurally distinct readings. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and perspectival gaps. The dignity_and_eternity reading treats Article 79(3) as the constitutive core; the amendment_history reading treats the text as a cumulative record of amendments; the basic_rights_catalog reading distributes protection across multiple articles; the federal_construction reading emphasizes the Länder as co-constitutional actors; the militant_democracy reading emphasizes the state's defensive mechanisms. These are not five aspects of one constraint — they are five distinct constraints sharing a kernel (the Basic Law text) but differing in which clause, which history, which principle they take as foundational.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
