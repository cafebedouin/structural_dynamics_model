% ============================================================================
% CONSTRAINT STORY: beijing_hong_kong_institutional_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beijing_hong_kong_institutional_autonomy, []).

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
 *   constraint_id: beijing_hong_kong_institutional_autonomy
 *   human_readable: Beijing-Hong Kong Institutional Autonomy Constraint
 *   domain: geopolitical/constitutional
 *
 * SUMMARY:
 *   Beijing's control over Hong Kong's institutional structures exemplifies a
 *   geopolitical snare: the constraint extracts compliance and institutional
 *   subordination from Hong Kong while suppressing alternatives through
 *   coercive mechanisms (National Security Law, loyalist appointments, legal
 *   interpretation authority). The constraint operates through systematic
 *   institutional capture rather than direct violence — judges, educators,
 *   and civil society leaders face graduated pressure through career
 *   termination, credential revocation, and prosecution threats. The 'One
 *   Country, Two Systems' framework persists as theater (formal autonomy
 *   continues in legal documents and official rhetoric) while substantive
 *   institutional independence has been systematically eliminated. The
 *   constraint exhibits classical snare properties: high suppression
 *   (surveillance, prosecutorial discretion, credential-dependent penalties),
 *   high extractiveness (compliance with central authority, elimination of
 *   autonomous institutional voices), minimal coordination function (civil
 *   society actors derive no genuine benefit), and path-dependent enforcement
 *   (early institutional capture creates lock-in for subsequent actors).
 *   Different agent classes experience the constraint differently: Beijing
 *   sees coordination (solving the political integration problem), finance
 *   elites see mixed benefits (market access + legal certainty offset by
 *   reduced autonomy), international democracies see extraction (loss of
 *   democratic exemplar), and Hong Kong civil society sees pure coercion
 *   (trapped status with no exit). The extractiveness has increased from 0.35
 *   to 0.68 over the six-year interval, tracking the National Security Law
 *   implementation (2020), accelerated judicial appointments of loyalists,
 *   and credential revocation of dissidents.
 *
 * KEY AGENTS:
 *   - Beijing Central State Apparatus: Primary beneficiary (institutional/arbitrage) — consolidates authority over historically autonomous territory; benefits from unified legal framework and eliminated political opposition
 *   - Hong Kong Civil Society Actors: Primary victim (powerless/trapped) — judges, teachers, journalists, academics facing career termination, prosecution, credential revocation; trapped by property, professional credentials, family networks
 *   - Hong Kong Professional Institutions: Secondary victim (moderate/constrained) — law societies, medical boards, educator associations lose autonomy to state-appointed loyalists; constrained by dual extraction (direct intervention + members' individual coercion)
 *   - Hong Kong Finance and Business Elites: Complex position (powerful/mobile) — benefit from unified market access and property rights guarantees; have exit options (offshore assets, alternative citizenship); create elite-civil society perspectival asymmetry
 *   - International Democratic Coalition: Organized actor (organized/constrained) — governments and NGOs face extraction of geopolitical compliance; constrained by Hong Kong's finance hub role and cross-border dependencies
 *   - Hong Kong's Formal Autonomy Framework: Institutional artifact (piton) — One Country Two Systems persists as theater (Basic Law, independent judiciary formally intact) while substantive independence is hollow; persists through legacy agreements and finance hub role
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beijing_hong_kong_institutional_autonomy, 0.68).
domain_priors:suppression_score(beijing_hong_kong_institutional_autonomy, 0.75).
domain_priors:theater_ratio(beijing_hong_kong_institutional_autonomy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beijing_hong_kong_institutional_autonomy, extractiveness, 0.68).
narrative_ontology:constraint_metric(beijing_hong_kong_institutional_autonomy, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(beijing_hong_kong_institutional_autonomy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beijing_hong_kong_institutional_autonomy, snare).
narrative_ontology:human_readable(beijing_hong_kong_institutional_autonomy, "Beijing-Hong Kong Institutional Autonomy Constraint").
narrative_ontology:topic_domain(beijing_hong_kong_institutional_autonomy, "geopolitical/constitutional").

domain_priors:requires_active_enforcement(beijing_hong_kong_institutional_autonomy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beijing_hong_kong_institutional_autonomy, central_beijing_state_apparatus).
narrative_ontology:constraint_victim(beijing_hong_kong_institutional_autonomy, hong_kong_institutional_independence).
narrative_ontology:constraint_victim(beijing_hong_kong_institutional_autonomy, hong_kong_civil_society_actors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HONG KONG CIVIL SOCIETY (SNARE) — Judges, teachers, journalists, academics, NGO workers face structural coercion through the National Security Law apparatus. Exit is materially difficult: leaving Hong Kong incurs significant cost (professional credentials, property, family networks). Suppression is high — surveillance, prosecutorial discretion, and credential revocation create pervasive chilling effects. No genuine coordination function benefits these actors; the constraint is pure extraction of compliance and self-censorship.
constraint_indexing:constraint_classification(beijing_hong_kong_institutional_autonomy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: HONG KONG PROFESSIONAL INSTITUTIONS (SNARE) — Law societies, medical boards, educator associations, and press councils face dual extraction: direct state intervention in governance (appointment of loyalists, removal of autonomy over disciplinary standards) and indirect coercion through members' individual constraints. Institutions retain formal existence but lose functional independence. Exit for institutions is constrained: dissolution means loss of professional governance entirely; continued operation means accepting state control. Suppression is enforced through both legal penalties and reputation destruction.
constraint_indexing:constraint_classification(beijing_hong_kong_institutional_autonomy, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BEIJING CENTRAL STATE (ROPE) — The central state experiences the constraint as pure coordination: integrating Hong Kong's institutions into unified state control solves the political coordination problem of maintaining central authority over a historically autonomous territory. The constraint creates coordination mechanisms (loyalist appointment networks, unified legal interpretation) and provides extractive benefits (tax revenue capture, reduced political opposition). Beneficiary position is clear and secure; exit options are broad (implementation flexibility, timing discretion). The constraint functions as coordination from this perspective.
constraint_indexing:constraint_classification(beijing_hong_kong_institutional_autonomy, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL DEMOCRATIC COALITION (TANGLED ROPE) — International governments, NGOs, and democratic institutions experience the constraint as simultaneously a coordination failure (shared interest in rule-of-law norms, press freedom, institutional independence) and a mechanism of extraction (Beijing extracts geopolitical compliance from democracies that want to maintain Hong Kong-routed finance, trade, and soft influence). The coalition has exit options and agency but faces high coordination costs. Theater is moderate — performance of 'high degree of autonomy' is maintained in official rhetoric while substantive autonomy is systematically eliminated.
constraint_indexing:constraint_classification(beijing_hong_kong_institutional_autonomy, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: HONG KONG FORMAL AUTONOMY FICTION (PITON) — The 'One Country, Two Systems' institutional framework persists as a theatrical apparatus divorced from function. The structure (Basic Law, independent judiciary, civil service) exists but is operationally hollowed: appointments go to loyalists, legal interpretation flows from Beijing, administrative autonomy is exercised within narrow permitted bounds. Theater ratio is high (0.58) — the ritual of institutional autonomy persists while its actual independence has degraded. The piton persists through legacy international agreements and Hong Kong's continued finance-hub role, not through genuine functional independence.
constraint_indexing:constraint_classification(beijing_hong_kong_institutional_autonomy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HONG KONG FINANCE AND BUSINESS ELITES (TANGLED ROPE) — A subset of Hong Kong's institutional class (major corporations, banking families, property developers with cross-border interests) experience the constraint as a coordination mechanism that benefits them: unified legal framework with Beijing reduces transaction costs, provides preferential access to mainland markets, and guarantees property rights against political disruption. These elites have exit options (offshore assets, citizenship options, cross-border mobility) but benefit from institutional integration. They see genuine coordination benefits alongside extraction of compliance from the broader civil society. This creates a perspectival asymmetry: finance elites benefit from the constraint while civil society bears costs.
constraint_indexing:constraint_classification(beijing_hong_kong_institutional_autonomy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / GEOPOLITICAL NECESSITY VIEW (MOUNTAIN) — From a civilizational/universal perspective, some extraction of territorial autonomy is treated as inherent to state consolidation: large polities cannot tolerate indefinite quasi-independent enclaves; institutional integration is a natural law of geopolitical physics. However, this naturalizes a contingent extraction mechanism. The analytical perspective risks false-summit classification. The structural data (high suppression, selective beneficiaries, systematic institutional coercion) reveals this as extractive enforcement, not immutable law.
constraint_indexing:constraint_classification(beijing_hong_kong_institutional_autonomy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beijing_hong_kong_institutional_autonomy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(beijing_hong_kong_institutional_autonomy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(beijing_hong_kong_institutional_autonomy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(beijing_hong_kong_institutional_autonomy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(beijing_hong_kong_institutional_autonomy, TR),
    TR >= 0.70.

:- end_tests(beijing_hong_kong_institutional_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint extracts three overlapping commodities from Hong Kong: (1) political compliance (elimination of autonomous institutional voices critical of Beijing), (2) legal subordination (courts now interpret law in alignment with central authority), (3) personnel control (positions in autonomous institutions filled by loyalists). The base extractiveness has risen from 0.35 to 0.68 over six years, tracking the acceleration of institutional capture mechanisms. The extraction is severe because it targets institutional autonomy itself — not just policies but the capacity to generate independent institutional voice. Suppression (0.75): Very high. Multiple overlapping suppression mechanisms: (a) National Security Law creates undefined criminal categories (subversion, collusion with foreign forces) enabling prosecutorial discretion; (b) career penalties are pervasive — judges appointed by loyalists, teachers dismissed for 'bias,' academics denied research funding, journalists prosecuted; (c) credential dependencies create feedback loops — lawyers fear losing bar membership, doctors fear deregistration, creating self-censorship in institutional contexts; (d) surveillance is constant and visible (intelligence apparatus, camera networks, monitored communications), creating chilling effects independent of actual enforcement. Theater ratio (0.58): Moderate-high but declining. The One Country Two Systems framework maintains institutional theater — the Basic Law formally remains in effect, the judiciary formally retains independence, civil service formally retains autonomy. But the operational meaning has been systematically hollowed: appointment authority goes to central loyalists, legal interpretation authority flows from Beijing, administrative decisions are vetted for political alignment. Theater is declining (0.72 → 0.58) because the contradiction between formal and operational autonomy is becoming increasingly visible to international observers and Hong Kong civil society; Beijing has less incentive to maintain the theater as institutional capture nears completion.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the Beijing institutional perspective (Rope, sees coordination) and the Hong Kong civil society perspective (Snare, sees pure extraction) is fundamental and structural. Beijing experiences the constraint as solving a genuine coordination problem: large polities require unified institutional command to maintain authority. Hong Kong's civil society experiences coercion with no offsetting benefit. Hong Kong's finance elites occupy an intermediate position: they experience genuine coordination benefits (market access, legal certainty, property rights protection) combined with institutional subordination that is less directly suppressive than the civil society experience. This elite-civil society perspectival split is the key structural insight: institutional extraction is bearable for a small beneficiary class, which creates feedback that reinforces the constraint. Only when elite defection occurs (when the coordination benefits are outweighed by suppression spreading to the elites) would the constraint face pressure toward reclassification. International democracies see a threat to shared institutional norms but lack direct enforcement mechanisms without paying transaction costs (Hong Kong's finance hub role). The Piton perspective on Hong Kong's formal autonomy framework (theater ratio 0.58) captures the institutional degradation: the structure persists despite losing function, maintained by path dependency and Hong Kong's strategic role.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation operates through beneficiary/victim declarations combined with exit options. Beijing as beneficiary (institutional power, arbitrage exit options) derives d ≈ 0.05-0.15 (low extraction experienced). Hong Kong civil society as victim (powerless status, trapped exit) derives d ≈ 0.95 (maximum extraction experienced). Hong Kong professional institutions as victims (moderate power, constrained exit) derive d ≈ 0.70-0.85 (very high extraction). The finance elite perspectival split reflects asymmetric structural position: they are declared victims through the broader institutional autonomy extraction, but their exit options (mobile) and power level (powerful) modulate their experienced extraction downward. The international coalition as victim (organized power, constrained exit) derives d ≈ 0.55-0.65 (high but not maximal extraction — they have some agency and potential countermeasures). The perspectival asymmetry reveals that Beijing captures enormous asymmetric benefit while Hong Kong's civil society bears concentrated cost with minimal exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy through direct assessment of extraction versus coordination function. The primary mechanism is institutional coercion (National Security Law, loyalist appointments, judicial interpretation authority) with minimal offsetting coordination benefit for the majority of affected agents (Hong Kong's civil society and professional institutions). The elite beneficiary class (finance sector) experience genuine coordination benefits, but the distributional asymmetry (concentrated benefits for a small number of well-connected actors, dispersed costs across Hong Kong's civil society) confirms the extraction classification. The constraint is NOT misclassified as pure extraction when genuine coordination elements exist — the finance elite perspective explicitly acknowledges their experience of coordination benefits. But the dominance of snare classifications (perspectives 1, 2, and the analytical observer's false-summit warning) across different power levels confirms that extraction is the primary mechanism, not a side effect of coordination. The mandatrophy is resolved by the structural clarity: when the beneficiary class is so small that it can be locked in through appointment mechanisms (loyalist elites), and the cost is borne by the much larger civil society, the constraint is a snare with elite-capture properties, not a snare being misidentified as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hong_kong_institutional_resistance_potential,
    'Can Hong Kong''s institutional structures mount sustained resistance to central control without triggering overwhelming coercive response?',
    'Empirical observation of institutional coordination costs for Beijing (enforcement intensity, personnel turnover, legitimacy damage) versus institutional capacity for non-compliance strategies (judicial delay, administrative foot-dragging, professional standard-setting)',
    'If resistance capacity is high: constraint shifts from Snare toward Tangled Rope (genuine coordination problem). If resistance capacity approaches zero: Snare classification confirmed with irreversible institutional capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hong_kong_institutional_resistance_potential, empirical, 'Whether institutional resistance to central control is structurally viable').

omega_variable(
    international_enforcement_coordination_viability,
    'Can the international democratic coalition enforce institutional autonomy norms through coordinated sanctions, visa restrictions, and financial penalties without destroying Hong Kong''s finance hub role?',
    'Analysis of sanction impact on Hong Kong''s financial center status, corporate headquarters relocation, cross-border capital flows, and central bank clearing arrangements',
    'If international enforcement is viable: constraint becomes contingent on international forbearance (higher exit option for Hong Kong actors). If international coordination fails: constraint is enforced solely through central state coercion with no counterbalance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_enforcement_coordination_viability, empirical, 'Whether international coordination can enforce institutional autonomy').

omega_variable(
    elite_defection_threshold,
    'At what level of institutional coercion do Hong Kong finance and business elites shift from beneficiary to victim status?',
    'Tracking of capital flight, corporate relocations, family emigration among banking/property sectors; correlation with specific institutional intrusions (regulatory takeover, personnel appointment, legal interpretation shifts)',
    'If threshold is low: elite perspective shifts to Snare, creating coalition pressure on Beijing. If threshold is high: elites remain beneficiaries, undercutting civil society resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_defection_threshold, empirical, 'Threshold for elite defection from beneficiary status').

omega_variable(
    one_country_two_systems_irreversibility,
    'Is institutional autonomy extraction reversible through negotiated settlement or is the constraint path-dependent (institutional capacity once lost cannot be restored)?',
    'Historical comparison with other post-imperial territory integration cases (Crimea, Tibet, Taiwan-PRC, Northern Ireland); assessment of institutional institutional memory persistence, professional credential portability, and cross-generational norm transmission',
    'If reversible: constraint is contingent on continued enforcement (Snare with persistent but resistible coercion). If irreversible: constraint is becoming a Mountain (institutional autonomy has become structurally impossible, not just prohibited).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(one_country_two_systems_irreversibility, conceptual, 'Whether institutional autonomy loss is reversible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beijing_hong_kong_institutional_autonomy, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bhia_tr_t0, beijing_hong_kong_institutional_autonomy, theater_ratio, 0, 0.72).
narrative_ontology:measurement(bhia_tr_t3, beijing_hong_kong_institutional_autonomy, theater_ratio, 3, 0.65).
narrative_ontology:measurement(bhia_tr_t6, beijing_hong_kong_institutional_autonomy, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(bhia_be_t0, beijing_hong_kong_institutional_autonomy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bhia_be_t3, beijing_hong_kong_institutional_autonomy, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(bhia_be_t6, beijing_hong_kong_institutional_autonomy, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beijing_hong_kong_institutional_autonomy, enforcement_mechanism).
narrative_ontology:affects_constraint(beijing_hong_kong_institutional_autonomy, china_taiwan_institutional_integration).
narrative_ontology:affects_constraint(beijing_hong_kong_institutional_autonomy, uighur_regional_autonomy_elimination).
narrative_ontology:affects_constraint(beijing_hong_kong_institutional_autonomy, hong_kong_financial_hub_role).

% DUAL FORMULATION NOTE:
% This constraint is downstream of Beijing's institutional integration strategy across multiple territories. Related constraints include Taiwan institutional integration (analogous snare with higher international enforcement capacity), Uighur regional autonomy elimination (similar snare with different cultural suppression mechanisms), and Hong Kong's financial hub role (interdependent constraint — the finance hub permits the extractive control; loss of finance status would force renegotiation of institutional autonomy). The constraint family reflects the shared geopolitical strategy of institutional capture in territories with historical autonomy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beijing_hong_kong_institutional_autonomy, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
