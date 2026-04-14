% ============================================================================
% CONSTRAINT STORY: greek_orthodox_minority_rights_turkey
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_greek_orthodox_minority_rights_turkey, []).

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
 *   constraint_id: greek_orthodox_minority_rights_turkey
 *   human_readable: Greek Orthodox Minority Rights in Turkey
 *   domain: geopolitical/religious_minorities/institutional_rights
 *
 * SUMMARY:
 *   Greek Orthodox minorities in Turkey exist within a complex institutional
 *   framework that simultaneously coordinates Turkish state nationalism and
 *   extracts subordination from religious minorities. The constraint's
 *   origins lie in the 1923 Lausanne Treaty and subsequent Turkish
 *   nation-state formation, which explicitly differentiated Muslim citizens
 *   from Christian minorities (Greeks, Armenians, Jews). The Greek Orthodox
 *   community—particularly the Ecumenical Patriarchate based in
 *   Istanbul—occupies an historically unique position: 1,700 years of
 *   institutional continuity as the spiritual center of Orthodox
 *   Christianity, now operating within a state that systematically restricts
 *   its property rights, educational capacity, and institutional autonomy.
 *   The constraint operates through multiple mechanisms: legal exclusion
 *   (Lausanne recognition status that formally protects only three minority
 *   groups, Greek Orthodox not fully included); property seizure
 *   (thousand-plus church and monastery properties confiscated since 1923);
 *   institutional blockage (closure of Halki Theological Seminary in 1971,
 *   preventing succession of Orthodox leadership); and administrative
 *   obstruction (restrictions on property restoration, land purchases, and
 *   institutional recognition). Extractiveness has grown from 0.42
 *   (post-1923) to 0.58 (present), driven by accumulation of property losses
 *   and institutional constraint. Theater ratio has similarly increased from
 *   0.48 to 0.62, reflecting performative compliance with EU minority-rights
 *   conditionalities alongside continued substantive subordination. The
 *   constraint manifests as tangled rope (asymmetric extraction embedded
 *   within coordination of state narrative) from most institutional
 *   perspectives, but appears as pure snare (maximal extraction, minimal
 *   coordination) from the community perspective, and as a false mountain
 *   (naturalized as immutable consequence of state sovereignty) from certain
 *   analytical framings.
 *
 * KEY AGENTS:
 *   - Greek Orthodox Community: Primary victim (powerless/trapped) — bears extraction through property loss, institutional disability, educational barriers; no exit optionality; estimated 3,000–5,000 remaining members in Istanbul
 *   - Ecumenical Patriarchate: Secondary institutional victim (moderate/constrained) — operates under severe constraint but maintains religious authority and coordinating function; structurally trapped by institutional continuity requirements
 *   - Turkish State Apparatus: Primary beneficiary (institutional/arbitrage) — extracts legitimacy through minority subordination, uses minority status to reinforce nation-state boundaries, arbitrage between Lausanne Treaty obligations and nationalist practice
 *   - Turkish Nationalist Narrative: Beneficiary (institutional/arbitrage) — benefits from minorities-as-boundary-markers; uses subordination to define Turkish ethno-religious identity
 *   - EU Minority Rights Regime: Organized pressure actor (organized/mobile) — applies conditionality pressure via accession negotiations; creates exit path through reform incentive; sees constraint as temporary institutional failure
 *   - ECHR: Powerful adjudicator (powerful/mobile) — coordinates regional human rights standards; extracts compliance costs from Turkey through jurisprudence on minority religious freedom
 *   - Lausanne Treaty Framework: Institutional artifact (institutional/arbitrage) — persists through legal inertia despite functional degradation; provides cover story for subordination
 *   - Analytical Observer: Risk of naturalizing contingent arrangement as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(greek_orthodox_minority_rights_turkey, 0.58).
domain_priors:suppression_score(greek_orthodox_minority_rights_turkey, 0.68).
domain_priors:theater_ratio(greek_orthodox_minority_rights_turkey, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(greek_orthodox_minority_rights_turkey, extractiveness, 0.58).
narrative_ontology:constraint_metric(greek_orthodox_minority_rights_turkey, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(greek_orthodox_minority_rights_turkey, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(greek_orthodox_minority_rights_turkey, tangled_rope).
narrative_ontology:human_readable(greek_orthodox_minority_rights_turkey, "Greek Orthodox Minority Rights in Turkey").
narrative_ontology:topic_domain(greek_orthodox_minority_rights_turkey, "geopolitical/religious_minorities/institutional_rights").

domain_priors:requires_active_enforcement(greek_orthodox_minority_rights_turkey).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(greek_orthodox_minority_rights_turkey, turkish_state_apparatus).
narrative_ontology:constraint_beneficiary(greek_orthodox_minority_rights_turkey, turkish_nationalist_narrative).
narrative_ontology:constraint_victim(greek_orthodox_minority_rights_turkey, greek_orthodox_community).
narrative_ontology:constraint_victim(greek_orthodox_minority_rights_turkey, minority_religious_freedom).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GREEK ORTHODOX COMMUNITY (SNARE) — Trapped within Turkish territory with no exit; faces systematic restrictions on property ownership, religious education, institutional autonomy, and theological seminary training. Suppression operates through legal exclusion (Lausanne Treaty minority status limited to three recognized groups, Greek Orthodox not among them post-1923), administrative obstruction, and property seizure. The community cannot exit Turkey; cannot meaningfully exercise religious institutional functions; cannot transmit theological leadership. Minimum extracted coordination benefit—the constraint exists purely to subordinate.
constraint_indexing:constraint_classification(greek_orthodox_minority_rights_turkey, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ECUMENICAL PATRIARCHATE (TANGLED ROPE) — Operates under severe constraint but exercises de facto coordination of Orthodox Christianity across Eastern Europe and the Middle East. Constrained by Turkish sovereignty and minority status restrictions, yet maintains theological authority and symbolic continuity. The constraint embeds asymmetric extraction (legal disabilities, property seizure, administrative obstruction) alongside genuine coordination function (religious authority, ecumenical leadership). Exit options are constrained—relocation would abandon 1,700 years of institutional continuity. Beneficiaries (Turkish state) extract legitimacy through controlled tolerance; victims (Orthodox faithful) extract spiritual and institutional continuity at price of subordination.
constraint_indexing:constraint_classification(greek_orthodox_minority_rights_turkey, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TURKISH STATE (ROPE) — Experiences the constraint as coordination of ethno-nationalist narrative and Ottoman successor-state legitimacy. Greek Orthodox minority status reinforces Turkish nation-state boundaries, differentiates Turkish Muslims from Christian minorities, and justifies subordination through Lausanne-era legal frameworks. State has maximum exit optionality (can change law, recognize minority status, restore properties) and benefits from extraction (legitimacy of nation-state through minority subordination). From state perspective, the constraint solves a coordination problem: it allocates minority status within a Muslim-majority state through legal architecture rather than through explicit persecution (theater masking extraction).
constraint_indexing:constraint_classification(greek_orthodox_minority_rights_turkey, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EU MINORITY RIGHTS REGIME (SCAFFOLD) — Organized actor (EU accession negotiations, ECHR jurisprudence, Council of Europe monitoring) sees the constraint as a temporary coordination failure resolvable through rule-of-law reform. EU conditionality on accession creates exit pressure: Turkey can modernize minority protections in exchange for EU membership pathway. This represents a sunset clause—as Turkey's EU integration advances (or fails), the pressure to maintain restrictions or reform them will intensify. Low experienced extraction from EU's perspective because organized pressure creates alternate exit path. Theater is moderate: EU monitoring creates performative concessions without substantive reform.
constraint_indexing:constraint_classification(greek_orthodox_minority_rights_turkey, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: LAUSANNE TREATY FRAMEWORK (PITON) — The 1923 treaty establishing minority protections (three recognized groups: Greeks, Armenians, Jews) persists as institutional scaffolding despite having lost functional value. The treaty's recognition framework is performative—it legitimizes minority status without enabling minority rights. Greek Orthodox are not recognized minorities under Lausanne (only Rum/Turkish Greeks were protected; Anatolian Greeks were exchanged), creating legal gap where the treaty's theater masks extraction. The framework maintains symbolic authority (nations cite Lausanne to justify minority policy) while being substantially degraded (actual protections are minimal). Institutional inertia sustains it—alternatives (revised treaty, new legal framework) would require renegotiation neither Turkey nor other signatories pursue.
constraint_indexing:constraint_classification(greek_orthodox_minority_rights_turkey, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ECHR (TANGLED ROPE) — Powerful actor with mobile exit options (can sanction Turkey, can declare state violations, can award reparations) operating within a mixed coordination-extraction framework. ECHR coordinates regional human rights standards while extracting compliance costs from states. For Turkey specifically, ECHR rulings both enable (legal precedent for reform) and constrain (sovereignty objections, national security framing). The court benefits from cases that establish jurisprudence on minority rights; Turkey bears compliance costs; Orthodox community benefits from legal visibility but faces sovereignty-based resistance to enforcement. Not quite snare (powerful court has exit and enforcement capability) nor rope (Turkish state objects to external authority)—tangled rope captures the hybrid where human rights law provides coordination framework with asymmetric compliance burden.
constraint_indexing:constraint_classification(greek_orthodox_minority_rights_turkey, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, nation-state systems inherently require ethnic or religious boundary maintenance; minority status is an immutable consequence of state sovereignty and territorial organization. This perspective sees Orthodox minority subordination as a natural law of state formation—incompatible identities within fixed territory create permanent coordination problems without universalist resolution. However, structural data contradicts this: the constraint is enforced through contingent legal architecture (Lausanne, property law, institutional restrictions), not through laws of nature. The mountain classification is a false summit, revealing how ethno-nationalist framing naturalizes what is actually a constructed institutional arrangement. Counter-evidence: multiple EU states (Bulgaria, Greece, Germany) manage minority protections without subordination, proving the 'naturalness' claim false.
constraint_indexing:constraint_classification(greek_orthodox_minority_rights_turkey, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(greek_orthodox_minority_rights_turkey_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(greek_orthodox_minority_rights_turkey, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(greek_orthodox_minority_rights_turkey, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(greek_orthodox_minority_rights_turkey, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(greek_orthodox_minority_rights_turkey, TR),
    TR >= 0.70.

:- end_tests(greek_orthodox_minority_rights_turkey_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The state captures significant value through property seizure (estimated €1–5 billion in Church property), institutional control (ability to regulate minority activities), and boundary maintenance (use of minority status to define national identity). However, extraction is not maximal because the community retains some institutional capacity (the Patriarchate still operates, worship continues, ecumenical influence persists). The value has grown over the 100-year interval as property losses accumulated and as the cost of theological leadership succession became apparent (Halki closure creates generational constraint). Suppression (0.68): High. Multiple suppression mechanisms operate simultaneously: legal exclusion (treaty status ambiguity), property law (seizures justified as national interest), administrative obstruction (restrictions on institutional recognition and property restoration), and educational blockage (seminary closure). Suppression is not total (community survives, some worship permitted) but is severe enough to prevent self-reproduction of institutional capacity. Theater ratio (0.62): Moderate-high. Performative elements include: EU accession-era rhetorical commitments to minority rights without substantive implementation; token gestures (occasional property returns, religious holiday recognition) masking continued systematic exclusion; international diplomatic framing of Turkey as 'protecting' minorities while implementing restrictions. Theater has increased over time as EU pressure has mounted, forcing performative compliance that masks persistent extraction. The constraint's claimed type is tangled_rope because it exhibits both coordination (Turkish state uses it to organize nation-state boundaries and international standing) and extraction (systematic subordination of religious freedoms and property rights).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural arrangement (Turkish legal subordination of Greek Orthodox minority) appears as coordinating rope from the beneficiary's perspective, as extractive snare from the victim's perspective, as temporary scaffold from organized external pressure, and as false mountain from naturalization perspectives. The gap reveals that Turkish state gains legitimate coordination benefit (boundary maintenance, international legitimacy as rule-of-law actor) while the community bears pure extraction (no reciprocal benefit, systematic subordination). The EU's scaffold perspective is contingent on accession dynamics—if Turkey abandons EU accession, the sunset clause evaporates and the constraint reverts to snare + institutional arbitrage. The ECHR's tangled rope reflects that human rights law operates through external enforcement against sovereignty resistance, creating a hybrid where coordination (human rights standards) is embedded in power asymmetry (court vs. state).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values flow from structural position. The Greek Orthodox community as victims + trapped → high d (0.92) → maximum experienced extraction (f(d) ≈ 1.35). The Ecumenical Patriarchate as partial beneficiary (religious authority function) + constrained victim (subordinated operation) → moderate d (0.55) → moderate χ. The Turkish state as beneficiary + arbitrage exit → low d (0.12) → negative or near-zero χ (state experiences the constraint as coordination, not as extraction). EU minority regime as organized actor with mobile exit → moderate d (0.48) → moderate χ (pressure from outside, so structural position is neither beneficiary nor trapped victim). ECHR as powerful external enforcer → analytical d (0.72) → moderate χ (powerful actor with external enforcement capacity, but constrained by sovereignty objections). The directionality cascade directly produces the perspectival gap: the powerless trapped agent experiences maximum extraction; the institutional beneficiary experiences coordination; organized external pressure experiences moderate burden; the analytical observer risks false naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint has not resolved mandatrophy (mandatrophy_resolved: false) because the classification remains sensitive to perspective. From the community perspective it is snare; from the state perspective it is rope; from EU perspective it is scaffold; from analytical perspective it risks mountain. The mandatrophy here is not whether the classification is correct (all are structurally accurate from their respective positions) but whether the state will continue to treat the constraint as coordination (rope) or whether external pressure will force recognition that it is asymmetric extraction (tangled rope or snare). The resolution pathway is EU accession: if Turkey commits to minority rights reform as condition of EU entry, the constraint transitions from snare/tangled rope to scaffold with sunset. If Turkey abandons EU accession, the constraint remains snare indefinitely. The false mountain risk is acute: if Turkish nationalism continues to frame minority subordination as inherent to nation-state organization (natural law), reform becomes unthinkable. The analytical observer must resist this naturalization and recognize the constraint as institutionally contingent, not as law-of-nature immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lausanne_legal_status_ambiguity,
    'Are Greek Orthodox classified as unrecognized minorities under Lausanne (excluded protection framework) or as Turkish citizens without special status (equal-but-subordinate framework)?',
    'Legal analysis of Turkish court interpretations of Lausanne; comparison with how Armenia and Jewish communities are legally classified; historical documentation of treaty negotiation intent',
    'If unrecognized minorities: state obligation to protect exists but is violated (snare + ECHR violation). If equal citizens: no state obligation, minority status is civil society choice (weakens snare classification, strengthens rope or piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lausanne_legal_status_ambiguity, conceptual, 'Ambiguity in Lausanne Treaty recognition status for Greek Orthodox').

omega_variable(
    property_restitution_feasibility,
    'Is restitution of seized Church property structurally feasible without destabilizing Turkish property law and triggering cascading minority claims?',
    'Comparative analysis of restitution in post-Cold War Eastern Europe; legal modeling of property law changes; feasibility studies of partial vs. full restoration',
    'If feasible: property return becomes a reform option (tangled rope with sunset). If structurally infeasible: property theft becomes permanent extraction mechanism (snare strengthened), or state will only offer token restitution (piton strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_restitution_feasibility, empirical, 'Whether property restitution is structurally feasible').

omega_variable(
    theological_seminary_training_bottleneck,
    'Does the closure of Halki Theological Seminary (1971) represent permanent exclusion of Orthodox leadership development or a temporary constraint resolvable through EU accession negotiations?',
    'Historical analysis of Turkish educational policy; modeling of seminary reopening as conditionality in EU accession; comparison with Orthodox church capacity building in diaspora',
    'If permanent: this is an existential constraint (leadership succession impossible; orthodoxy becomes non-self-reproducing minority). If temporary: sunset logic applies; scaffold classification strengthens. If resolvable through diaspora training: extraction mechanisms persist but institutional impact is mitigated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_seminary_training_bottleneck, empirical, 'Whether theological seminary closure is permanent or temporary').

omega_variable(
    eu_accession_conditionality_effectiveness,
    'Do EU minority-rights conditionalities actually drive Turkish reform, or do they become performative compliance theater masking continued subordination?',
    'Longitudinal tracking of Turkish minority rights metrics during accession negotiations (1987–present); comparison of law-on-books vs. implementation; ECHR cases against Turkey pre- and post-conditionality periods',
    'If effective: scaffold sunset clause is real; organized pressure creates exit pathways. If performative theater: EU conditionality is piton mechanism (maintains narrative of reform without substantive change). If ineffective: EU influence is minimal; constraint returns to pure snare + institutional arbitrage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eu_accession_conditionality_effectiveness, empirical, 'Whether EU accession conditionalities effectively drive minority rights reform').

omega_variable(
    ottoman_legacy_institutional_persistence,
    'To what extent does continued Greek Orthodox subordination reflect Ottoman successor-state identity formation (millet system residue) versus explicit ethno-nationalist ideology (post-1923 Kemalism)?',
    'Historical comparative analysis of Ottoman minority management institutions; archival study of Turkish nation-state formation (1923–1950); linguistic analysis of Turkish policy justifications over time',
    'If Ottoman residue dominates: constraint is piton (institutional inertia of degraded millet framework). If Kemalist ideology dominates: constraint is snare + political rationality (deliberate subordination). If hybrid: constraint exhibits both mechanisms (piton + snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ottoman_legacy_institutional_persistence, conceptual, 'Attribution of subordination to Ottoman legacy versus Kemalist ideology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(greek_orthodox_minority_rights_turkey, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gree_tr_t0, greek_orthodox_minority_rights_turkey, theater_ratio, 0, 0.48).
narrative_ontology:measurement(gree_tr_t50, greek_orthodox_minority_rights_turkey, theater_ratio, 50, 0.58).
narrative_ontology:measurement(gree_tr_t100, greek_orthodox_minority_rights_turkey, theater_ratio, 100, 0.62).

% Extraction over time
narrative_ontology:measurement(gree_be_t0, greek_orthodox_minority_rights_turkey, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gree_be_t50, greek_orthodox_minority_rights_turkey, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(gree_be_t100, greek_orthodox_minority_rights_turkey, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(greek_orthodox_minority_rights_turkey, identity_coordination).
narrative_ontology:affects_constraint(greek_orthodox_minority_rights_turkey, armenian_minority_rights_turkey).
narrative_ontology:affects_constraint(greek_orthodox_minority_rights_turkey, turkish_eu_accession_framework).
narrative_ontology:affects_constraint(greek_orthodox_minority_rights_turkey, ottoman_successor_state_legitimacy).

% DUAL FORMULATION NOTE:
% Greek Orthodox minority rights in Turkey form a constraint family with Armenian and Jewish minority protection mechanisms. Each minority community faces distinct historical and structural constraints (Ottoman legacy, property seizure patterns, institutional continuity barriers), but all operate within the Lausanne Treaty framework and Turkish ethno-nationalist state structure. Greek Orthodox constraint is distinguished by the Ecumenical Patriarchate's unique civilizational role and by the Halki seminary closure's existential threat to leadership succession.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(greek_orthodox_minority_rights_turkey, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
