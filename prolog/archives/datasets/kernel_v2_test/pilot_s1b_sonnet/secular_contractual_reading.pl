% ============================================================================
% CONSTRAINT STORY: secular_contractual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secular_contractual_reading, []).

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
 *   constraint_id: secular_contractual_reading
 *   human_readable: Secular Contractual Marriage Under State Law
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   The secular contractual reading treats marriage as a voluntary civil
 *   contract between autonomous individuals, validated solely by state
 *   registration under general civil law. This framework emerged in
 *   post-Enlightenment legal systems as an alternative to ecclesiastical
 *   jurisdiction, and in religiously plural states (notably India under the
 *   Special Marriage Act, 1954) as a neutral coordination mechanism for
 *   interfaith unions and secular citizens. The secular framework's core
 *   structural feature is jurisdictional displacement: religious
 *   solemnization becomes optional ceremony rather than legal necessity. The
 *   constraint coordinates where religious frameworks cannot — interfaith
 *   couples, gender-symmetric partnerships, non-religious unions — while
 *   enabling exit from religious marriage norms. The framework's modest
 *   extractiveness (0.25) and suppression (0.40) reflect coordination
 *   overhead (registration requirements, procedural compliance, age
 *   restrictions) rather than asymmetric rent extraction. Theater ratio
 *   (0.35) has risen modestly over the 50-year interval as bureaucratic
 *   requirements have accumulated, but the core coordination function remains
 *   intact. This reading is ONE of five sibling readings of the
 *   family_law_authority kernel; it coexists with
 *   hindu_dharmashastra_reading, muslim_shariat_reading,
 *   christian_canonical_reading, and parsi_zoroastrian_reading under the same
 *   legal system, producing a jurisdictional menu rather than a unified
 *   authority structure.
 *
 * KEY AGENTS:
 *   - Interfaith Couples: Primary beneficiaries (moderate/mobile) — secular framework solves recognition problem religious systems cannot accommodate
 *   - Gender-Symmetric Partners: Primary beneficiaries (moderate/mobile) — enables equal legal standing unavailable under asymmetric religious codes
 *   - Secular Citizens: Primary beneficiaries (moderate/mobile) — provides marriage option without religious affiliation requirement
 *   - State Legal System: Coordinating institution (institutional/constrained) — administers registration, enforces civil consequences, maintains records
 *   - Religious Community Leadership: Mixed position (institutional/constrained) — experiences both coordination (interfaith cases resolved) and extraction (jurisdictional loss, authority made optional)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees secular framework as coordination solution to legal pluralism in diverse polities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secular_contractual_reading, 0.25).
domain_priors:suppression_score(secular_contractual_reading, 0.4).
domain_priors:theater_ratio(secular_contractual_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secular_contractual_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(secular_contractual_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(secular_contractual_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secular_contractual_reading, rope).
narrative_ontology:human_readable(secular_contractual_reading, "Secular Contractual Marriage Under State Law").
narrative_ontology:topic_domain(secular_contractual_reading, "comparative_law/political_theory/religious_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secular_contractual_reading, 'ccdceeae-8973-47c8-9ee8-05c55051c90b').
narrative_ontology:cs_kernel_codification('ccdceeae-8973-47c8-9ee8-05c55051c90b', formalized).
narrative_ontology:cs_authority_grounding('ccdceeae-8973-47c8-9ee8-05c55051c90b', extraction).
narrative_ontology:cs_interpretation_layer_present('ccdceeae-8973-47c8-9ee8-05c55051c90b').
narrative_ontology:cs_reading_relation('ccdceeae-8973-47c8-9ee8-05c55051c90b', secular_contractual_reading__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccdceeae-8973-47c8-9ee8-05c55051c90b', secular_contractual_reading__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccdceeae-8973-47c8-9ee8-05c55051c90b', secular_contractual_reading__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('ccdceeae-8973-47c8-9ee8-05c55051c90b', secular_contractual_reading__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_axiom('ccdceeae-8973-47c8-9ee8-05c55051c90b', foundational, individual_autonomy_in_contract).
narrative_ontology:cs_axiom_status(individual_autonomy_in_contract, holdable).
narrative_ontology:cs_axiom_grounding('ccdceeae-8973-47c8-9ee8-05c55051c90b', individual_autonomy_in_contract, deontological).
narrative_ontology:cs_axiom('ccdceeae-8973-47c8-9ee8-05c55051c90b', foundational, state_monopoly_on_civil_validity).
narrative_ontology:cs_axiom_status(state_monopoly_on_civil_validity, holdable).
narrative_ontology:cs_axiom_grounding('ccdceeae-8973-47c8-9ee8-05c55051c90b', state_monopoly_on_civil_validity, conventional).
narrative_ontology:cs_reference_frame('ccdceeae-8973-47c8-9ee8-05c55051c90b', westphalian_civil_sovereignty).
narrative_ontology:cs_drift_state('ccdceeae-8973-47c8-9ee8-05c55051c90b', contemporary_india_post_shah_bano, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ccdceeae-8973-47c8-9ee8-05c55051c90b', '').
narrative_ontology:cs_kernel_id(secular_contractual_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secular_contractual_reading, interfaith_couples).
narrative_ontology:constraint_beneficiary(secular_contractual_reading, gender_symmetric_partners).
narrative_ontology:constraint_beneficiary(secular_contractual_reading, secular_citizens).
narrative_ontology:constraint_vindicates(secular_contractual_reading, state_sovereignty_in_civil_matters).
narrative_ontology:constraint_vindicates(secular_contractual_reading, individual_autonomy_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERFAITH COUPLE (ROPE) — Experiences the secular framework as pure coordination. Without state civil marriage, their union would be unrecognized by at least one partner's religious community. The secular pathway solves a genuine coordination problem: establishing legal kinship, inheritance, and custody rights where religious frameworks would deny recognition. Minimal extraction — the couple benefits from the framework's existence.
constraint_indexing:constraint_classification(secular_contractual_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE LEGAL SYSTEM (ROPE) — Sees the secular framework as necessary coordination infrastructure. Registration requirements enable enforcement of custody, inheritance, and property rights. The constraint coordinates legitimate state interests (record-keeping, taxation, social welfare eligibility) with minimal coercive overhead. Extraction is low — the state enforces agreements the parties themselves entered voluntarily.
constraint_indexing:constraint_classification(secular_contractual_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GENDER-SYMMETRIC PARTNERSHIP (ROPE) — Partners seeking equal legal standing experience secular civil marriage as coordination. Religious frameworks that assign asymmetric rights (differential divorce initiation, custody presumptions, property allocation) are avoided without penalty. The secular framework enables a coordination outcome unavailable under asymmetric religious codes. Low extraction — the legal structure serves the partners' stated preferences.
constraint_indexing:constraint_classification(secular_contractual_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: RELIGIOUS COMMUNITY LEADERSHIP (TANGLED ROPE) — Sees both coordination and extraction. The secular framework coordinates interfaith cases and gender-symmetric unions the religious system cannot accommodate (genuine coordination function). But it also undermines religious authority over family law — secular registration becomes the sole validity criterion, reducing religious solemnization to optional ceremony. The community leadership experiences jurisdictional loss: members can exit religious marriage norms entirely by choosing civil registration. Moderate extraction alongside real coordination.
constraint_indexing:constraint_classification(secular_contractual_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — At civilizational scope, the secular contractual framework solves the coordination problem of legal pluralism in religiously diverse polities. Without a secular option, interfaith and non-religious couples would face either non-recognition or forced adherence to a religious framework neither accepts. The analytical perspective sees low extraction: the framework's suppression (registration requirements, age restrictions, procedural compliance) is coordination overhead, not asymmetric rent extraction. The constraint enables exits from religious frameworks without foreclosing religious solemnization for those who choose it.
constraint_indexing:constraint_classification(secular_contractual_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secular_contractual_reading_tests).
:- end_tests(secular_contractual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.25): Low-moderate. The secular framework extracts modestly through bureaucratic overhead — registration fees, procedural compliance costs, mandatory waiting periods — but these are coordination costs distributed across participants rather than asymmetric extraction favoring identifiable beneficiaries. Religious communities experience jurisdictional loss as extraction, but this is offset by the framework's coordination function (resolving interfaith cases the religious system cannot handle). The value reflects that most experienced extraction comes from bureaucratic friction, not structural rent-seeking. Suppression (0.40): Moderate. The framework suppresses alternatives modestly: couples cannot create legally binding kinship ties without state registration; age restrictions and procedural requirements constrain timing and form. But suppression is not severe — religious solemnization remains available as optional ceremony, and the framework does not foreclose religious practice. The value reflects real but limited constraint on alternative arrangements. Theater ratio (0.35): Low-moderate. Bureaucratic requirements (document submission, witness attestation, waiting periods) contain performative elements, but most registration procedures serve genuine coordination functions: identity verification, consent confirmation, record-keeping for inheritance and custody. The modest rise over 50 years reflects accumulating procedural requirements that serve administrative convenience more than coordination necessity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is narrow across most seats — interfaith couples, gender-symmetric partners, secular citizens, and the analytical observer all see Rope (coordination with minimal extraction). The gap opens at the religious community leadership seat, which sees Tangled Rope: genuine coordination for cases the religious system cannot handle (interfaith unions) alongside jurisdictional extraction (religious authority made optional). This gap reveals the constraint's core structural tension: what the secular framework coordinates FROM the state perspective (legal pluralism, interfaith recognition) is experienced AS extraction by religious authorities whose jurisdiction it displaces. The gap is not a measurement error — it reflects a real structural difference in how coordination and extraction are distributed. The analytical observer's Rope classification may underweight the extraction experienced by religious communities, treating jurisdictional displacement as neutral coordination when it materially reduces religious institutional authority.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (interfaith_couples, gender_symmetric_partners, secular_citizens) experience low directionality — the constraint flows toward them, not away from them. They gain legal recognition, equal standing, or secular option unavailable under religious frameworks. The engine derives low d → low or negative chi for these agents. The state_legal_system is a coordinating institution with constrained exit (cannot abandon marriage regulation entirely without jurisdictional loss) but is not extracting rents — it experiences low directionality as coordinator rather than beneficiary. Religious_community_leadership occupies a mixed position: benefits from the framework's resolution of interfaith cases the religious system cannot accommodate, but bears jurisdictional cost as religious authority becomes optional. The engine should derive moderate d for this agent, reflecting mixed coordination and extraction. No agent is declared as victim — the constraint's extractiveness is bureaucratic overhead distributed across participants rather than asymmetric extraction targeting identifiable victims. The absence of victims in a 0.25-extractiveness constraint is structurally coherent: low extraction can be diffuse coordination cost rather than targeted rent-seeking.
 *
 * MANDATROPHY ANALYSIS:
 *   The secular contractual framework avoids mandatrophy by maintaining genuine coordination function. The constraint coordinates interfaith recognition, gender-symmetric legal standing, and secular marriage option — all cases where religious frameworks either cannot coordinate (interfaith) or coordinate asymmetrically (gender roles). The framework's modest extractiveness represents coordination overhead, not obsolete mandate persisting through inertia. Mandatrophy risk exists if the bureaucratic layer (theater_ratio = 0.35, rising) grows to overwhelm coordination function, but current measurements show the coordination core remains intact. The framework would reach mandatrophy if registration requirements became pure ritual while alternative coordination mechanisms (cohabitation agreements, private contract, religious solemnization) delivered equivalent legal standing — at that point the state registration requirement would be maintained for institutional revenue rather than coordination necessity. Current structure: not mandatrophy. Rising theater ratio warrants monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_location,
    'Is the secular contractual reading the correct structural framing of state civil marriage authority, or is this one reading of a contested kernel where sibling readings (religious authority over family law) produce structurally different constraints with different beneficiary sets and different ε values?',
    'Cross-reading analysis: compare beneficiary/victim declarations, extractiveness values, and suppression mechanisms across secular_contractual_reading, hindu_dharmashastra_reading, muslim_shariat_reading, christian_canonical_reading, and parsi_zoroastrian_reading. If beneficiary sets overlap substantially and ε values converge, the readings describe the same constraint from different perspectives (observer axis). If beneficiary sets differ and ε values diverge, the readings instantiate structurally distinct constraints (committer axis).',
    'If same constraint: perspectival gap analysis applies — the ''correct'' classification depends on the observer''s seat. If different constraints: the kernel (family_law_authority) is a label covering multiple structurally distinct arrangements, and this story is ONE of them, coexisting with siblings held by different communities under the same state legal system.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_location, conceptual, 'Whether secular civil marriage is one reading among siblings or the sole structural arrangement').

omega_variable(
    jurisdictional_displacement_extraction,
    'Does the secular framework''s displacement of religious jurisdiction over marriage constitute extraction from religious communities, or is it neutral coordination that enables exits from religious frameworks without suppressing religious practice?',
    'Measure whether religious communities experience material loss from secular civil marriage availability: declining membership, reduced institutional revenue, loss of social authority. If religious solemnization rates decline and community leadership identifies civil marriage availability as causal, extraction is present. If religious ceremonies persist at stable rates alongside civil registration (dual-track model), the framework is coordination.',
    'If extraction: religious community leadership perspective reclassifies from tangled_rope toward snare — the secular framework suppresses alternatives by making religious authority optional. If coordination: the tangled_rope classification holds — religious communities experience jurisdictional loss but the framework does not foreclose religious practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_displacement_extraction, empirical, 'Whether secular framework''s jurisdictional scope constitutes extraction from religious communities').

omega_variable(
    alternative_reading_foreclosure_mechanism,
    'Does the secular contractual reading logically foreclose any sibling reading, or do all five readings coexist as live positions held by different communities under the same legal system?',
    'Logical analysis: can a single legal framework hold both ''state registration is sole validity criterion'' (secular reading) and ''religious solemnization under community law is sole validity criterion'' (religious readings) simultaneously? In practice, Indian law holds both: the Special Marriage Act (secular) coexists with personal law codes (religious). No reading forecloses another — they coexist as parallel jurisdictional options.',
    'If no foreclosure: all reading_relations entries should be coexists_with, not forecloses. The kernel is genuinely contested, with no reading ruling out its siblings within any single framework. If foreclosure exists: at least one reading''s core premise contradicts a sibling''s core premise such that no framework could hold both.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure_mechanism, conceptual, 'Whether any reading of family_law_authority forecloses its siblings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secular_contractual_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sec_contract_theater_early, secular_contractual_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sec_contract_theater_mid, secular_contractual_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(sec_contract_theater_current, secular_contractual_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(sec_contract_extract_early, secular_contractual_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(sec_contract_extract_mid, secular_contractual_reading, base_extractiveness, 25, 0.2).
narrative_ontology:measurement(sec_contract_extract_current, secular_contractual_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(sec_contract_suppress_early, secular_contractual_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sec_contract_suppress_mid, secular_contractual_reading, suppression_requirement, 25, 0.35).
narrative_ontology:measurement(sec_contract_suppress_current, secular_contractual_reading, suppression_requirement, 50, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secular_contractual_reading, resource_allocation).
narrative_ontology:affects_constraint(secular_contractual_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(secular_contractual_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(secular_contractual_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(secular_contractual_reading, parsi_zoroastrian_reading).

% DUAL FORMULATION NOTE:
% The secular_contractual_reading is one of five sibling constraints instantiated from the family_law_authority kernel. Each reading has its own extractiveness value, beneficiary set, and suppression mechanism. The secular reading's ε = 0.25 reflects bureaucratic overhead; religious readings will have different ε values reflecting religious authority overhead, gender asymmetry, or interfaith restrictions. The readings are linked via network.affects_constraints because jurisdictional choices (secular vs. religious marriage pathways) affect each framework's institutional authority and membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
