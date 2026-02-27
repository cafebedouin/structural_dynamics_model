% ============================================================================
% CONSTRAINT STORY: hagia_sophia_status
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_status, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hagia_sophia_status
 *   human_readable: The enforced religious and political status of the Hagia Sophia
 *   domain: religious/political
 *
 * SUMMARY:
 *   The Hagia Sophia status constraint represents a contested claim over one
 *   of the world's architecturally and religiously significant structures.
 *   Built as a Byzantine cathedral in 537 CE, converted to a mosque in 1453
 *   after the Ottoman conquest, secularized as a museum in 1935 by Atatürk's
 *   reforms, and reconverted to a mosque in 2020 by presidential decree, the
 *   Hagia Sophia embodies competing frameworks: religious ownership and
 *   access rights, secular heritage preservation, international cultural
 *   norms, Turkish national sovereignty, and democratic legitimacy. The
 *   constraint exhibits structural extraction (suppression of Christian
 *   access, dismissal of international heritage consensus, subordination of
 *   secular governance norms) masked by coordination framing (resolving
 *   electoral demand, aligning with majority religious sentiment). The
 *   extractiveness has increased over the interval as political pressure has
 *   mounted and the secular-religious contestation has intensified. The
 *   theater ratio reflects that both sides deploy symbolic and performative
 *   appeals (international protests that have no enforcement mechanism, state
 *   invocation of sovereignty and religious ownership) rather than functional
 *   negotiation.
 *
 * KEY AGENTS:
 *   - Orthodox Christian Community: Primary victim (powerless/trapped) — no access, no voice in decision, no exit option. Diaspora loses symbolic connection to a foundational religious site.
 *   - Secular Heritage Advocates: Secondary victim (moderate/constrained) — constrained by nationalist and religious political will; can protest but cannot prevent state action. Advocates for universalist heritage preservation lose institutional standing.
 *   - Turkish State Authority: Primary beneficiary (institutional/arbitrage) — gains consolidation of electoral support by converting museum to mosque. Experiences the constraint as solving a political problem (secular-religious tension) via state assertion.
 *   - Sunni Islamic Establishment: Primary beneficiary (institutional/arbitrage) — gains institutional access and religious authority over a site of major symbolic value. Coordinates Muslim worship without external constraint.
 *   - International Heritage Regime (UNESCO, ICOMOS): Tertiary actor (institutional/constrained) — formally opposed but structurally powerless. Maintains norms through performative statements and symbolic censure, with no enforcement mechanism.
 *   - Turkish Secular Intelligentsia: Tertiary victim (moderate/constrained) — culturally and politically marginalized by the conversion. Values universalist heritage preservation but lack electoral power to enforce it.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_status, 0.58).
domain_priors:suppression_score(hagia_sophia_status, 0.68).
domain_priors:theater_ratio(hagia_sophia_status, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_status, extractiveness, 0.58).
narrative_ontology:constraint_metric(hagia_sophia_status, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_status, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_status, snare).
narrative_ontology:human_readable(hagia_sophia_status, "The enforced religious and political status of the Hagia Sophia").
narrative_ontology:topic_domain(hagia_sophia_status, "religious/political").

domain_priors:requires_active_enforcement(hagia_sophia_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_status, turkish_state_authority).
narrative_ontology:constraint_beneficiary(hagia_sophia_status, sunni_islamic_establishment).
narrative_ontology:constraint_victim(hagia_sophia_status, orthodox_christian_community).
narrative_ontology:constraint_victim(hagia_sophia_status, secular_heritage_advocates).
narrative_ontology:constraint_victim(hagia_sophia_status, religious_pluralism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORTHODOX CHRISTIAN DIASPORA (SNARE) — Cannot exit or change the constraint. Access to one of Christianity's holiest sites is blocked by decree. Bears full cost of political-religious exclusion. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.73.
constraint_indexing:constraint_classification(hagia_sophia_status, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GLOBAL HERITAGE COMMUNITY (SNARE) — Cannot enforce UNESCO norms or secular museum status. The Hagia Sophia's status is determined unilaterally by the host state. UNESCO world heritage advocates are structurally powerless to prevent conversion. d≈0.90, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(hagia_sophia_status, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: SECULAR TURKISH INTELLECTUAL ELITE (TANGLED ROPE) — Constrained by majority religious politics and nationalist sentiment, but also benefits from Turkey's secular governance legacy and international credibility. Exit options exist (emigration, institutional relocation) but carry professional cost. Mixed: the constraint both undermines their universalist values and reflects their electoral marginalization. d≈0.65, f(d)≈0.98, σ=1.0 → χ≈0.57.
constraint_indexing:constraint_classification(hagia_sophia_status, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TURKISH STATE AUTHORITY (ROPE) — Experiences the constraint as a coordination mechanism solving a domestic political problem: converting the museum status to mosque status aligns the state with majority religious sentiment and consolidates electoral support. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.006. Net beneficiary; the constraint is functional coordination from this perspective.
constraint_indexing:constraint_classification(hagia_sophia_status, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SUNNI ISLAMIC ESTABLISHMENT (ROPE) — Gains access to one of Islam's most symbolically potent sites and consolidates religious authority. The constraint is purely coordinating from this view: it resolves decades of contention by establishing ownership and use. d≈0.10, f(d)≈-0.09, σ=0.9 → χ≈-0.005. Net beneficiary.
constraint_indexing:constraint_classification(hagia_sophia_status, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: INTERNATIONAL HERITAGE REGIME (PITON) — UNESCO norms, secular preservation, and pluralist access doctrines remain formally stated but are performatively maintained. The international community protests symbolically but enforces nothing; the constraint persists through institutional theater (statements, resolutions) with no functional mechanism. theater_ratio≈0.65 (norms articulated but unenforced). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.50. The regime is degraded — the norms exist but cannot enforce compliance against sovereign state authority.
constraint_indexing:constraint_classification(hagia_sophia_status, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SOVEREIGNTY VIEW (ATTEMPTED MOUNTAIN) — From a pure state-sovereignty lens, the host nation's right to determine the use of its internal cultural sites is seen as a natural law: states control their own property. ε≈0.15 from this pure sovereignty frame. But the structural data (ε=0.58, suppression=0.68) contradicts this — the constraint is enforced precisely because it suppresses alternatives and extracts political value. The 'natural sovereignty' framing is a false summit that naturalizes political choice as law.
constraint_indexing:constraint_classification(hagia_sophia_status, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_status_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hagia_sophia_status, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hagia_sophia_status, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_status, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hagia_sophia_status, TR),
    TR >= 0.70.

:- end_tests(hagia_sophia_status_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from the Orthodox community (complete access denial), from secular preservation advocates (dismissal of their universalist frame), and from international heritage norms (unilateral state override of international consensus). But extractiveness is not maximal (≥0.70) because: (1) The Turkish state's action is responsive to genuine majority electoral sentiment (not purely coercive); (2) The conversion has a legitimate religious-ownership framing that resolves a centuries-old contestation; (3) Some coordination function exists (the conversion does align institutional will and majority sentiment, even if it suppresses minority positions). The extractiveness reflects that political choice is being enforced (not Rope-level ≤0.45) but that extraction is partially legitimated by electoral mandate (not Snare-level ≥0.70 pure coercion). Suppression (0.68): High. Orthodox Christians are completely suppressed from access to a site they view as sacred. Secular governance norms and international heritage regimes are suppressed via state sovereignty assertion. Alternative organizational models (shared access, pluralist stewardship) are suppressed via the exclusivity of state control. The suppression is structural, not accidental. Theater ratio (0.65): High-moderate. Significant performative activity: international protests that have no mechanism to change the outcome, state invocation of religious and national ownership rhetoric, secular appeals to universal heritage value that go unenforced. The theater increases over the interval as the contestation intensifies — both sides perform the conflict more explicitly, while the structural outcome (state determination of status) remains unchanged.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates a perspectival divide between state actors and externally positioned observers. The Turkish state authority and Sunni Islamic establishment both classify the constraint as Rope (coordination) — they are solving a genuine internal political problem (secular-religious tension) by aligning institutional will with majority sentiment. From their view, the conversion is functional and legitimate. The Orthodox Christian community and secular heritage advocates classify it as Snare (extraction) — they are excluded from access and voice, suppressed via state authority, with no exit option. The international heritage regime classifies it as Piton (degraded) — norms are formally stated but unenforced; the regime maintains itself through performative activity (statements, resolutions) that has zero impact on the actual outcome. The secular Turkish intelligentsia classify it as Tangled Rope (mixed) — they are both constrained by electoral political will (which they oppose) and benefit from living in a secular state framework (which is being undermined). The analytical observer risks classifying it as Mountain (sovereign state right) but the structural data reveals this as a false summit: the constraint is contingent on political choice and electoral dynamics, not immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Turkish State Authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. The state consolidates political power and authority by converting the site. Sunni Islamic Establishment: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.09. Net beneficiary. The establishment gains institutional access and authority. Orthodox Christian Community: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction from this group. No access, no voice, no exit. Secular Heritage Advocates: Victim + constrained → d≈0.75, f(d)≈1.10. Significant extraction. Can protest and emigrate but face career and cultural cost. International Heritage Regime: Institutional + constrained → d≈0.50, f(d)≈0.65. Symmetric position. Has norms but no enforcement. Secular Turkish Intelligentsia: Mixed victim + constrained → d≈0.65, f(d)≈0.98. Constrained by electoral will but also live within secular state framework.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by exposing how electoral legitimacy (majority preference) and structural extraction (suppression of minority access and international norms) coexist. The key insight: a Snare classification (from the Orthodox/secular minority view) and a Rope classification (from the state/majority view) are not contradictory — they are perspectival readings of the same structural dynamic. The state IS solving a coordination problem (aligning religious access with majority sentiment). AND the Orthodox Christians ARE being extracted from (denied access to a sacred site they value). Both statements are true from their respective positions. The mandatrophy resolves by recognizing that 'legitimacy' (electoral majority) does not erase 'extraction' (suppression of minority position). The constraint is a Snare with democratic mandate — an uncomfortable hybrid where the majority votes to suppress the minority's access rights. This is politically stable (the majority enforces it) but ethically contentious (it violates pluralist and universal heritage norms). The extractiveness (0.58) and suppression (0.68) reflect this tension: high but not maximal, because the extraction is partially legitimated by electoral process, even though it remains structural suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    religious_access_equivalence,
    'Does restricting Orthodox Christian access to the Hagia Sophia constitute extraction, or is it merely assertion of religious ownership equivalent to restricting Muslim-majority site access for non-Muslims?',
    'Comparative analysis of access restrictions across Christian-majority countries (e.g., restrictions on Muslim access to sites like Canterbury, Notre-Dame in ordinary operation), Islamic-majority countries (Saudi Arabia''s Mecca/Medina restrictions), and Jewish-majority sites; assessment of whether asymmetric restrictions reflect equal reciprocal norms or unequal power',
    'If equivalent reciprocal: classification shifts toward Rope (pure coordination resolving competing property claims). If asymmetric power dynamic: classification remains Snare (extraction via suppression of access to majority-preferred site).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_access_equivalence, conceptual, 'Whether access restrictions are symmetric reciprocal norms or asymmetric power extraction').

omega_variable(
    electoral_legitimacy_threshold,
    'What electoral mandate or public opinion threshold justifies converting a UNESCO world heritage site from secular preservation to religious use?',
    'Survey data on Turkish public opinion regarding Hagia Sophia status (pre-2020 and post-2020); analysis of electoral platforms and campaign messaging; cross-national comparison of heritage site conversion politics in other democracies',
    'If majority sentiment is legitimate democratic mandate: constraint is Tangled Rope (mixed coordination of electoral will + suppression of minority position). If majority sentiment is manipulated via nationalist framing: constraint remains Snare (suppression of alternatives via manufactured consent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_legitimacy_threshold, preference, 'Threshold of democratic legitimacy for heritage site religious conversion').

omega_variable(
    international_enforcement_capacity,
    'Can international heritage regimes (UNESCO, ICOMOS, ICJ) meaningfully enforce preservation or access norms against sovereign state resistance?',
    'Historical analysis of enforcement outcomes in comparable cases (e.g., Bamiyan Buddhas, Angkor Wat, Palmyra); assessment of sanctions, diplomatic pressure, and compliance rates; identification of successful vs failed international interventions',
    'If enforceable: international regime is not piton but active rope with real coordination function. Constraint shifts to institutional standoff. If unenforceable: piton diagnosis confirmed — international norms are purely performative theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_enforcement_capacity, empirical, 'Whether international heritage enforcement mechanisms have real compliance mechanisms').

omega_variable(
    pluralist_access_sustainability,
    'Is there a structurally stable equilibrium in which the Hagia Sophia serves both Muslim worship and Christian/secular heritage functions, or does functional use require exclusive control?',
    'Case study analysis of shared-use religious sites (e.g., Church of the Holy Sepulchre, Cordoba Cathedral-Mosque during Al-Andalus period); assessment of operational models, maintenance responsibilities, access scheduling, and conflict resolution mechanisms',
    'If shared-use models are stable: constraint is organizational (Rope or Scaffold with sunset to pluralism). If shared use is structurally unstable: conversion to exclusive use is inevitable extraction from non-majority perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pluralist_access_sustainability, empirical, 'Whether shared-use religious heritage sites maintain stable pluralist access').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_status, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagia_tr_t0, hagia_sophia_status, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hagia_tr_t35, hagia_sophia_status, theater_ratio, 35, 0.55).
narrative_ontology:measurement(hagia_tr_t70, hagia_sophia_status, theater_ratio, 70, 0.65).

% Extraction over time
narrative_ontology:measurement(hagia_be_t0, hagia_sophia_status, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hagia_be_t35, hagia_sophia_status, base_extractiveness, 35, 0.42).
narrative_ontology:measurement(hagia_be_t70, hagia_sophia_status, base_extractiveness, 70, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_status, enforcement_mechanism).
narrative_ontology:affects_constraint(hagia_sophia_status, turkish_secular_governance_erosion).
narrative_ontology:affects_constraint(hagia_sophia_status, international_heritage_norm_enforcement).

% DUAL FORMULATION NOTE:
% The Hagia Sophia status constraint is downstream of broader dynamics: Turkish electoral shifts toward religious-nationalist politics (upstream), and international heritage regime capacity to enforce norms against sovereign states (upstream). The constraint represents a specific instantiation of the state-sovereignty-vs-international-norm tension in cultural heritage governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_status, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
