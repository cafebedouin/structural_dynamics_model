% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_balanced, []).

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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems: Balanced Coexistence Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   The 'One Country, Two Systems' framework (formalized in the Sino-British
 *   Joint Declaration, 1984, and codified in Hong Kong's Basic Law, 1990)
 *   establishes a structural arrangement in which the People's Republic of
 *   China exercises national sovereignty while the Hong Kong Special
 *   Administrative Region retains substantial institutional autonomy for 50
 *   years (1997-2047). The balanced coexistence reading instantiates this
 *   framework as a genuine negotiated boundary: neither central authority nor
 *   regional autonomy is absolute; both are constrained by functional
 *   interdependence; disputes over boundary jurisdiction are resolved through
 *   political accommodation rather than legal supremacy. This reading
 *   contrasts with sovereignty primacy (Beijing's authority is ultimate;
 *   autonomy is delegated and revocable) and autonomy primacy (Hong Kong's
 *   autonomy is constitutionally entrenched; Beijing's sovereignty is limited
 *   to foreign affairs and defense). The balanced coexistence reading claims
 *   that all three are live positions held by different institutional
 *   actors—Beijing's central government (sovereignty emphasis), Hong Kong's
 *   business establishment and HKSAR administration (coexistence emphasis),
 *   and pro-democracy civil society and international observers (autonomy
 *   emphasis)—and that the constraint's stability depends on the economic and
 *   political costs of violating the boundary exceeding the benefits to any
 *   single party. The framework has experienced systematic extraction
 *   accumulation since 1997: suppression (security law enforcement, electoral
 *   manipulation) has risen from 0.28 to 0.62; theater ratio (performative
 *   autonomy gestures) has risen from 0.38 to 0.68; base extractiveness has
 *   risen from 0.35 to 0.55. The trajectory suggests the constraint is
 *   migrating from stable tangled_rope (mixed coordination and extraction)
 *   toward snare (pure extraction with suppression) or piton (theater
 *   substituting for function). This reading instantiates the constraint at
 *   its current state (2026): substantive negotiation remains possible, but
 *   the bargaining equilibrium has shifted decisively toward Beijing's
 *   preferences.
 *
 * KEY AGENTS:
 *   - Central Government Economic Authority: Institutional beneficiary (institutional/arbitrage) — captures sovereignty enforcement power; benefits from Hong Kong's continued function as international financial hub
 *   - Hong Kong Business Establishment: Institutional beneficiary (institutional/constrained) — benefits from regulatory arbitrage; participates in both systems; faces constraints but retains bargaining power through economic leverage
 *   - Hong Kong Government/HKSAR Administration: Institutional actor (institutional/constrained) — coordinates between systems; dependent on Beijing's authorization; benefits from institutional distinctness but constrained by national security directives
 *   - Hong Kong Civil Society Organizations: Primary victim (powerless/trapped) — faces political constraints, security law enforcement, and economic dependence on mainland; no credible exit; bears extraction without compensation
 *   - Pan-Democratic Political Coalition: Organized victim (organized/constrained) — constrained by electoral manipulation and legal restrictions; retains bargaining power through international attention and protest mobilization; sees sunset path through renegotiation
 *   - International Rule-of-Law Community: Institutional observer (institutional/mobile) — maintains autonomy narrative through theater; can exit but sustains positioning through limited enforcement capacity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contingent institutional arrangement as immutable structural truth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.48).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.52).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems: Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '8786586e-887f-4cf8-91e1-162d633950c9').
narrative_ontology:cs_kernel_codification('8786586e-887f-4cf8-91e1-162d633950c9', formalized).
narrative_ontology:cs_authority_grounding('8786586e-887f-4cf8-91e1-162d633950c9', extraction).
narrative_ontology:cs_interpretation_layer_present('8786586e-887f-4cf8-91e1-162d633950c9').
narrative_ontology:cs_reading_relation('8786586e-887f-4cf8-91e1-162d633950c9', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('8786586e-887f-4cf8-91e1-162d633950c9', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('8786586e-887f-4cf8-91e1-162d633950c9', foundational, mutual_constraint_through_functional_interdependence).
narrative_ontology:cs_axiom_status(mutual_constraint_through_functional_interdependence, holdable).
narrative_ontology:cs_axiom_grounding('8786586e-887f-4cf8-91e1-162d633950c9', mutual_constraint_through_functional_interdependence, instrumental).
narrative_ontology:cs_axiom('8786586e-887f-4cf8-91e1-162d633950c9', foundational, negotiated_boundary_adjustment_over_legal_supremacy).
narrative_ontology:cs_axiom_status(negotiated_boundary_adjustment_over_legal_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('8786586e-887f-4cf8-91e1-162d633950c9', negotiated_boundary_adjustment_over_legal_supremacy, conventional).
narrative_ontology:cs_reference_frame('8786586e-887f-4cf8-91e1-162d633950c9', negotiated_autonomy_equilibrium).
narrative_ontology:cs_drift_state('8786586e-887f-4cf8-91e1-162d633950c9', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('8786586e-887f-4cf8-91e1-162d633950c9', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, central_government_economic_authority).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_institutional_autonomy).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, political_pluralism_in_hong_kong).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, mainland_economic_integration_pressure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HONG KONG CIVIL SOCIETY (SNARE) — NGOs, labor unions, and pro-democracy groups face extraction with no exit. Their political participation is constrained by national security law and electoral manipulation; their economic livelihoods depend on mainland trade; international leverage (visa sanctions, asset freezes) creates material penalties for organizing. High experienced extraction, near-total suppression of alternatives.
constraint_indexing:constraint_classification(one_country_two_systems_framework__balanced_coexistence_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: HONG KONG BUSINESS ESTABLISHMENT (TANGLED ROPE) — Major corporations and trading houses coordinate with both systems: they benefit from Hong Kong's international financial status (autonomy advantage) while extracting mainland market access (sovereignty advantage). They face constraints but retain significant bargaining power through economic leverage. Mixed coordination (stabilizing both systems) and extraction (capturing regulatory arbitrage).
constraint_indexing:constraint_classification(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CENTRAL GOVERNMENT ECONOMIC AUTHORITY (ROPE) — Gains from Hong Kong's continued function as international financial hub (autonomy benefits economic integration). Can arbitrage Hong Kong's legal system for foreign investment and cross-border commerce. Experiences the constraint as coordination: maintaining Hong Kong's institutional distinctness solves the integration problem more efficiently than full assimilation. Net beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(one_country_two_systems_framework__balanced_coexistence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HONG KONG GOVERNMENT/HKSAR (TANGLED ROPE) — Coordinates between mainland sovereignty and local autonomy; benefits from institutional distinctness (administrative capacity, legal separation, service delivery legitimacy) while bearing the cost of managing national security law, electoral constraints, and economic integration pressure. Constrained by Beijing's ultimate authority; coordinates genuine local governance function.
constraint_indexing:constraint_classification(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL RULE-OF-LAW OBSERVERS (PITON) — Western legal frameworks and human-rights institutions maintain narratives of Hong Kong's 'high degree of autonomy' primarily through institutional inertia. International judicial forums (UN Human Rights Council, ICJ amicus positions) perform rule-of-law theater (condemning restrictions while lacking enforcement capacity) without substantively altering the constraint. Mobile exit (can withdraw engagement) sustains the theatrical posture.
constraint_indexing:constraint_classification(one_country_two_systems_framework__balanced_coexistence_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PAN-DEMOCRATIC POLITICAL COALITION (SCAFFOLD) — Organized political parties and activists see the balanced coexistence as temporarily structurally enforced, subject to renegotiation through electoral pressure, international leverage (international attention, investment capital movement), and coordination with civil society. View the framework as having a sunset: sustained political organization and international support can shift the bargaining equilibrium. Constrained but not trapped — see exit path through political mobilization.
constraint_indexing:constraint_classification(one_country_two_systems_framework__balanced_coexistence_reading, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/global perspective, the balanced coexistence framework instantiates an irreducible structural tension: any system claiming both national sovereignty and regional autonomy must distribute authority along some boundary. The tension between unitary state authority and federal/special-status legitimacy is presented as inherent to all such systems. However, structural data reveals beneficiaries (central government, business establishment) and victims (civil society, political pluralism) — the engine will classify this as a false summit.
constraint_indexing:constraint_classification(one_country_two_systems_framework__balanced_coexistence_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(one_country_two_systems_framework__balanced_coexistence_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(one_country_two_systems_framework__balanced_coexistence_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, TR),
    TR >= 0.70.

:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, rising. The constraint extracts substantive benefits to Beijing and Hong Kong's business elite while imposing costs on civil society and political pluralism. The extraction is not total (economic interdependence constrains unilateral enforcement) but is material and accumulating. The rise from 0.35 to 0.55 over 25 years reflects: (1) national security law implementation (2020) expanded suppression machinery; (2) electoral framework changes (2021) narrowed pro-democracy participation; (3) capital flight and professional emigration (2019-2022) increased economic pressure on remaining agents. The trajectory is consistent with extraction accumulation — the constraint is squeezing for greater returns as initial coexistence stability proved durable. Suppression (0.52 base, rising to 0.62): Moderate-high, increasing. Suppression has risen from minimal (autonomy was genuine) to substantial (political opposition is constrained). The rise reflects: (1) national security law creates surveillance infrastructure; (2) electoral manipulation narrows genuine political participation; (3) media pressure and self-censorship reduce information flow. The mechanism is active enforcement (Beijing's security apparatus actively constrains alternatives), not passive structural barriers. Theater ratio (0.58, rising to 0.68): Moderate, increasing. Institutional performances affirm Hong Kong's autonomy (HKSAR government statements, cosmetic governance reforms, international praise) while actual autonomy is declining. The rise suggests theater is substituting for function — as real autonomy erodes, institutional actors must perform it more frequently to maintain stakeholder credibility. This is the classic piton signature, but the constraint is not yet fully piton (coordination function persists) nor fully snare (suppression is high but not total). The constraint is in migration: tangled_rope → piton/snare.
 *
 * PERSPECTIVAL GAP:
 *   The balanced coexistence reading produces maximum perspectival divergence across power and exit options. From the central government's perspective (institutional/arbitrage), the constraint is rope: genuine coordination that maintains Hong Kong's international function. From Hong Kong business's perspective (institutional/constrained), it is tangled_rope: both coordinating and extracting. From the HKSAR's perspective (institutional/constrained), it is also tangled_rope: managing genuine autonomy while accepting Beijing's ultimate authority. From civil society's perspective (powerless/trapped), it is snare: extraction with no exit. From the pan-democratic coalition's perspective (organized/constrained), it is scaffold: a temporary enforced equilibrium subject to renegotiation through political pressure. From international observers' perspective (institutional/mobile), it is piton: performative autonomy gestures lacking enforcement capacity. From the civilizational/analytical perspective (analytical/analytical), it risks appearing as mountain: an immutable feature of sovereignty-autonomy tension. This perspectival range reflects that the constraint is genuinely hybrid: it coordinates some functions (business, economics, basic governance) while extracting others (political participation, civil society space). The distribution of extraction across agents is asymmetric: beneficiaries (Beijing, business) experience low χ; victims (civil society) experience high χ.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the constraint. Central Government: beneficiary + arbitrage exit → d ≈ 0.05, f(d) ≈ -0.12 → negative χ (experiences coordination, not extraction). Hong Kong Business: beneficiary + constrained exit → d ≈ 0.20, f(d) ≈ 0.02 → near-zero χ (both benefits and constraints, nets positive). HKSAR: victim + constrained exit → d ≈ 0.55, f(d) ≈ 0.75 → moderate χ (constrained by Beijing's authority, genuine coordination function makes net-positive in some areas). Pan-Democratic Coalition: victim + constrained exit + organized power → d ≈ 0.50, f(d) ≈ 0.65 → moderate χ (constrained but organized, can mobilize). Civil Society: victim + trapped exit → d ≈ 0.95, f(d) ≈ 1.42 → high χ (maximum experienced extraction). International Observers: beneficiary + mobile exit → d ≈ 0.15, f(d) ≈ -0.01 → negative χ (can maintain positioning with minimal cost). The range of d values across agents (0.05 to 0.95) explains why classifications diverge from rope (beneficiary) to snare (victim) while the constraint maintains its hybrid tangled_rope classification at the balanced-coexistence analytical view.
 *
 * MANDATROPHY ANALYSIS:
 *   BALANCED COEXISTENCE RESOLUTION: This reading resolves the mandatrophy by instantiating the constraint as a genuine hybrid—tangled_rope from the analytical perspective—that contains both coordination and extraction functions simultaneously. The tension is not 'is this coordination or extraction?' but 'who coordinates and who extracts?' The central government coordinates Hong Kong's economic function while extracting political authority. Hong Kong business coordinates economic activity while extracting regulatory arbitrage. Civil society extracts costs while coordinating nothing. The reading holds that this mix is substantively negotiated—neither party can unilaterally impose total control without destroying the framework—but the negotiation is radically asymmetric in distribution of power. The mandatrophy dissolves when we recognize that both coordination and extraction can exist within a single constraint, distributed across different agents. The classification is tangled_rope at the institutional/civilizational analytical level because that is where the framework's designers intended negotiation to occur. At the civil society level, it is snare, because that agent has no bargaining power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_autonomy_boundary_stability,
    'Is the sovereignty-autonomy boundary genuinely stable and negotiated, or does central government authority ultimately prevail in all high-stakes disputes?',
    'Historical case analysis of disputed domains (national security law, electoral framework, cross-border commerce, extradition): did the boundary shift through negotiation, did Beijing impose authority, or was the appearance of negotiation maintained while authority shifted?',
    'If boundary is genuinely negotiated: tangled_rope classification holds; constraint enables coexistence. If Beijing prevails in all disputes: boundary is illusory; reclassify as snare (apparent autonomy is theater). If negotiation was real in past but declining: constraint is shifting from tangled_rope toward snare (extraction accumulation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_autonomy_boundary_stability, empirical, 'Whether the sovereignty-autonomy boundary is substantively negotiated or Beijing''s authority is decisive').

omega_variable(
    economic_arbitrage_sustainability,
    'Can Hong Kong maintain its economic arbitrage function (international financial hub status) indefinitely while political autonomy constraints tighten, or will capital flight and delistings eventually eliminate the arbitrage that motivates central government tolerance?',
    'Long-term tracking of: (a) international capital flows into/out of Hong Kong; (b) corporation relocation patterns; (c) Shanghai/Shenzhen financial integration metrics; (d) cross-border commerce friction costs',
    'If arbitrage is sustainable: central government has ongoing interest in maintaining autonomy, rope classification for beneficiary strengthens. If arbitrage erodes: central government''s interest in coexistence declines, tangled_rope shifts toward snare as enforcement intensifies without coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_arbitrage_sustainability, empirical, 'Whether Hong Kong can sustain economic arbitrage as political constraints tighten').

omega_variable(
    civil_society_bargaining_power_source,
    'Is Hong Kong civil society''s residual bargaining power (international attention, boycott capacity, protest mobilization) structural or dependent on external actors (US sanctions, international investment restrictions) over which Hong Kong has no control?',
    'Analysis of what mechanisms actually constrain central government: (a) endogenous—domestic economic dependence on Hong Kong, fear of capital flight, loss of international legitimacy; (b) exogenous—US sanctions on PRC officials, capital controls by international banks, reputational pressure from allies; (c) mixed. Test by varying external sanctions while holding Hong Kong civil society activity constant.',
    'If structural (endogenous): civil society has genuine leverage; snare perspective is potentially overestimated; reclassify toward tangled_rope. If dependent on external actors: Hong Kong civil society is captured by international geopolitics; snare classification holds; exit options are illusory. If mixed: constraint''s stability depends on US-China competition remaining salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_society_bargaining_power_source, empirical, 'Whether civil society bargaining power is structural or externally dependent').

omega_variable(
    reading_contest_ground,
    'What distinguishes this balanced coexistence reading from its sibling readings (sovereignty primacy, autonomy primacy) in the contested kernel?',
    'Axiomatic analysis: (1) Sovereignty primacy holds that Beijing''s national sovereignty is ultimate; autonomy is delegated authority revocable at Beijing''s discretion. (2) Autonomy primacy holds that Hong Kong''s autonomy is constitutionally entrenched; Beijing''s sovereignty is limited to foreign affairs and defense. (3) Balanced coexistence holds that neither is ultimate; both are constrained by the functional interdependence of the framework and neither can unilaterally revoke the other without destroying the system that benefits it. The readings differ on WHICH authority is ultimate, not on whether limits exist.',
    'This reading (balanced coexistence) instantiates a tangled_rope from the institutional perspective and snare from the civil society perspective, holding both simultaneously. Sovereignty primacy would classify as rope (autonomy is coordination mechanism for sovereignty''s benefit). Autonomy primacy would classify as rope (sovereignty is coordination mechanism for autonomy''s benefit). The classification difference is driven by which axiom is foundational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_ground, conceptual, 'Axiomatic difference between balanced coexistence and sibling readings').

omega_variable(
    performance_ceiling_for_theater,
    'Has the constraint reached a performance ceiling beyond which further theater (statements affirming autonomy, cosmetic reforms to HKSAR governance) no longer produces credible reassurance?',
    'Survey data on Hong Kong and international stakeholder confidence in ''high degree of autonomy'' claim, correlated with: (a) frequency of theater performances (HKSAR government statements, international praise, cosmetic reforms); (b) actual autonomy outcomes (political participation, civil society space, cross-border freedom). If confidence falls despite theater increase, the theater ratio becomes decoupled from function — piton signal emerges.',
    'If ceiling reached: theater ratio should be rising (0.58 → higher) even as functional autonomy is declining, classic piton drift signature. If ceiling not yet reached: theater can continue temporarily substituting for function, maintaining tangled_rope classification. If ceiling exceeded: constraint risk-transitions toward snare as theater loses credibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_ceiling_for_theater, empirical, 'Whether theater performance can continue substituting for functional autonomy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oc2s_bal_theater_1997, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(oc2s_bal_theater_2002, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(oc2s_bal_theater_2012, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 15, 0.58).
narrative_ontology:measurement(oc2s_bal_theater_2022, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 25, 0.68).

% Extraction over time
narrative_ontology:measurement(oc2s_bal_extractiveness_1997, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(oc2s_bal_extractiveness_2002, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(oc2s_bal_extractiveness_2012, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(oc2s_bal_extractiveness_2022, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 25, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(oc2s_bal_suppression_1997, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(oc2s_bal_suppression_2012, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(oc2s_bal_suppression_2022, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework__autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, hong_kong_national_security_law_compliance).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, mainland_economic_integration_pressure).

% DUAL FORMULATION NOTE:
% This constraint is ONE ELEMENT within a constraint family decomposed from the contested ONE COUNTRY, TWO SYSTEMS KERNEL. Three separate constraint stories model the three sibling readings: sovereignty_primacy (ε ≈ 0.25, rope-type from Beijing's perspective), autonomy_primacy (ε ≈ 0.65, snare-type from pro-democracy perspective), and balanced_coexistence (ε ≈ 0.48, tangled_rope). Each story has its own beneficiary/victim declarations, measurement trajectories, and axioms. The three readings contest the same kernel text but derive different classifications. No single story 'is correct'—the presheaf of all three readings over the observation sites is the complete model. Balanced coexistence is the institutional establishment's reading; the other two readings are live positions in the political dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__balanced_coexistence_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
