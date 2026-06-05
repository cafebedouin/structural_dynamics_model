% ============================================================================
% CONSTRAINT STORY: secularization_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secularization_pressure, []).

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
 *   constraint_id: secularization_pressure
 *   human_readable: Secularization Pressure on Religious Authority Structures
 *   domain: religious_doctrine/canon_law/political_sociology
 *
 * SUMMARY:
 *   Secularization pressure represents the structural constraint created when
 *   modernizing state apparatus progressively displaces religious
 *   institutional authority from law, education, governance, and public life.
 *   This constraint exhibits multiple structural characters depending on the
 *   observer's position: for religious practitioners whose identity is fused
 *   with faith communities, it appears as a snare (identity-locked
 *   entrapment); for secular state apparatus, it appears as enabling
 *   coordination; for religious leadership navigating institutional survival,
 *   it appears as mixed coordination-extraction; for accommodation movements,
 *   it appears as a temporary problem with a sunset; for formal institutions,
 *   it appears as degraded theater; and from the civilizational analytical
 *   perspective, it risks appearing as inevitable modernization when it is
 *   actually a constructed institutional outcome. The constraint's base
 *   extractiveness has increased from 0.32 to 0.58 over the measured
 *   interval, reflecting accumulating institutional authority loss and
 *   erosion of religious jurisdiction in law, education, and social policy.
 *   Theater ratio has risen from 0.48 to 0.65, indicating increasing
 *   performativity of religious institutions — religious authority persists
 *   through ceremonial and symbolic roles even as functional authority
 *   (doctrinal enforcement, binding community jurisdiction, legal legitimacy)
 *   erodes. Suppression has intensified from 0.45 to 0.62, reflecting both
 *   external enforcement (legal restrictions on religious authority,
 *   secularization of curriculum, elimination of religious exemptions) and
 *   internalized pressure (younger cohorts' cognitive shift toward secular
 *   epistemic frameworks, identity-locking into secular worldviews).
 *
 * KEY AGENTS:
 *   - Doctrinal Transmission Communities: Primary victims (powerless/identity_locked) — practitioners whose identity is constituted through faith; bear full cost of institutional erosion while structurally trapped by identity fusion
 *   - Religious Institutional Leadership: Mixed actor (organized/constrained) — bishops, theologians, administrators managing institutional survival; experience both coordination function and extraction; constrained but not trapped
 *   - Secular State Apparatus: Primary beneficiary (institutional/arbitrage) — state governance structures, legislative bodies, secular administration that benefit from unified legal authority, secular education, religious exemption elimination
 *   - Secular Intellectual Establishment: Secondary beneficiary (powerful/mobile) — universities, research institutions, scientific academies that gain epistemic prestige and institutional resources through secularization; experience mixed coordination-extraction
 *   - Religious Accommodation Movement: Organized reformer (organized/constrained) — interfaith coalitions, religious liberty advocates working to establish plural constitutional frameworks; see sunset pathway for the constraint
 *   - Formal Religious Institution: Institutional actor (institutional/constrained) — established churches, denominations maintaining ceremonial authority even as functional authority erodes; experiencing piton degradation
 *   - Analytical Observer: Universal context (analytical/analytical) — risks naturalizing contingent institutional outcome as inevitable modernization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secularization_pressure, 0.58).
domain_priors:suppression_score(secularization_pressure, 0.62).
domain_priors:theater_ratio(secularization_pressure, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secularization_pressure, extractiveness, 0.58).
narrative_ontology:constraint_metric(secularization_pressure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(secularization_pressure, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secularization_pressure, tangled_rope).
narrative_ontology:human_readable(secularization_pressure, "Secularization Pressure on Religious Authority Structures").
narrative_ontology:topic_domain(secularization_pressure, "religious_doctrine/canon_law/political_sociology").

domain_priors:requires_active_enforcement(secularization_pressure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secularization_pressure, 'bbd9ffa1-d924-476d-becf-08cb10ce3a7e').
narrative_ontology:cs_kernel_codification('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', distributed).
narrative_ontology:cs_authority_grounding('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', extraction).
narrative_ontology:cs_interpretation_layer_present('bbd9ffa1-d924-476d-becf-08cb10ce3a7e').
narrative_ontology:cs_reading_relation('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', pluralist_accommodation_reading, coexists_with).
narrative_ontology:cs_reading_relation('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', religious_state_integration_reading, forecloses).
narrative_ontology:cs_axiom('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', foundational, modernization_requires_religious_displacement).
narrative_ontology:cs_axiom_status(modernization_requires_religious_displacement, holdable).
narrative_ontology:cs_axiom_grounding('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', modernization_requires_religious_displacement, empirically_contingent).
narrative_ontology:cs_axiom('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', foundational, secular_governance_superior_coordination).
narrative_ontology:cs_axiom_status(secular_governance_superior_coordination, holdable).
narrative_ontology:cs_axiom_grounding('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', secular_governance_superior_coordination, instrumental).
narrative_ontology:cs_axiom('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', secondary, religious_authority_incompatible_modern_law).
narrative_ontology:cs_axiom_status(religious_authority_incompatible_modern_law, holdable).
narrative_ontology:cs_axiom_grounding('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', religious_authority_incompatible_modern_law, empirically_contingent).
narrative_ontology:cs_reference_frame('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', secular_rationalist_modernity).
narrative_ontology:cs_drift_state('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', contemporary_pluralist_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bbd9ffa1-d924-476d-becf-08cb10ce3a7e', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secularization_pressure, secular_state_apparatus).
narrative_ontology:constraint_beneficiary(secularization_pressure, modernizing_institutional_sectors).
narrative_ontology:constraint_victim(secularization_pressure, religious_institutional_autonomy).
narrative_ontology:constraint_victim(secularization_pressure, doctrinal_transmission_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOCTRINAL TRANSMISSION COMMUNITY (SNARE) — Religious practitioners and clergy experience secularization pressure as structural entrapment. Exit is identity_locked: abandoning the institutional tradition requires abandoning the self-concept constituted through the faith community. Practitioners cannot exercise structural mobility (many could geographically relocate, change careers) because their identity is fused with the tradition. Suppression includes both external (legal restriction of religious authority, curriculum mandates, tax policy) and internalized (belief that secularization is inevitable, naturalness of secular framing). Maximum extraction: the community bears the costs of institutional erosion while remaining bound by identity commitments.
constraint_indexing:constraint_classification(secularization_pressure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS INSTITUTIONAL LEADERSHIP (TANGLED ROPE) — Bishops, theologians, institutional administrators face secularization pressure with constrained exits (high cost but possible). Leadership experiences both coordination function (organizing faith communities, maintaining institutional infrastructure) and extraction (erosion of legal authority, loss of property rights, educational mandate losses). Active enforcement: the secular state actively constrains religious authority through law and policy. Leadership has some agency — can negotiate with state, adapt doctrine — but faces substantial costs to maintaining institutional autonomy. Mixed experience: genuine coordination of religious practice alongside extraction of institutional authority.
constraint_indexing:constraint_classification(secularization_pressure, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SECULAR STATE APPARATUS (ROPE) — State administration, legislative bodies, educational bureaucracy experience secularization pressure as coordination mechanism. The constraint enables the state to solve genuine coordination problems: unified legal system (rather than plural religious legal traditions), secular education curriculum (rather than competing religious curricula), taxation without religious exemptions. Net beneficiary: the state experiences the constraint as coordination with low extraction costs. Exit options arbitrage: the state can always decouple further from religious authority; it faces no material cost to increased secularization.
constraint_indexing:constraint_classification(secularization_pressure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECULAR INTELLECTUAL ESTABLISHMENT (TANGLED ROPE) — Universities, research institutions, scientific academies experience secularization pressure as mixed coordination-extraction. Genuine coordination function: shared epistemic standards, secular reasoning frameworks, non-doctrinal knowledge production. But extraction occurs through the displacement of religious intellectual traditions from centers of prestige and funding. Mobility is high (intellectuals can move between institutions, countries) but faces some costs (ideological commitment to secular rationalism can become identity-like). The powerful position and mobile exit option moderate the experienced extraction, but asymmetry persists: religious scholarship is marginal to mainstream academic institutions.
constraint_indexing:constraint_classification(secularization_pressure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: RELIGIOUS ACCOMMODATION MOVEMENT (SCAFFOLD) — Interfaith coalitions, religious liberty advocates, institutional reformers see secularization pressure as a temporary coordination failure with a sunset. These organized agents work to establish alternative pathways: robust conscience protections, religious accommodation statutes, pluralistic curricula. Low effective extraction because the movement has agency and envisions an exit path: constitutional pluralism replacing secular monism. Sunset clause is structural: as accommodation frameworks mature (religious liberty law, institutional pluralism, respect for doctrinal autonomy within secular contexts), the pressure mechanism loses force. Estimated sunset: 20-50 years depending on constitutional evolution.
constraint_indexing:constraint_classification(secularization_pressure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FORMAL RELIGIOUS INSTITUTION (PITON) — Large denominations and established churches experience secularization pressure as degraded inertia. The theatrical dimension is high: formal religious authority persists through institutional habits (public holidays, ceremonial functions, social legitimacy) even as functional authority (doctrinal enforcement, legal jurisdiction, binding community membership) has substantially eroded. The institution maintains itself through theater rather than function. The high theater_ratio reflects that much formal religious practice is performative — ceremonies, liturgies, institutional roles — persisting through tradition even as their binding force diminishes. Piton classification derives from this degradation dynamic.
constraint_indexing:constraint_classification(secularization_pressure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, secularization pressure appears as an immutable consequence of modernization: as societies industrialize, urbanize, and develop complex state apparatus, religious authority necessarily contracts. This perspective treats secularization as a law of social development — structural to modernity itself, not contingent on policy choices. However, the structural data contradicts the mountain classification: identifiable beneficiaries (the secular state, modernizing institutions), clear enforcement mechanisms (law, policy, institutional pressure), and measurable extraction dynamics reveal this as a constructed constraint, not a natural law. The engine will identify this as a false summit, revealing that 'secularization is inevitable' naturalizes what is actually a contestable institutional outcome.
constraint_indexing:constraint_classification(secularization_pressure, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secularization_pressure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(secularization_pressure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(secularization_pressure, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(secularization_pressure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(secularization_pressure, TR),
    TR >= 0.70.

:- end_tests(secularization_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Secularization pressure extracts substantially from religious institutions — loss of legal jurisdiction, educational authority, exemption privileges, and institutional property. The 0.58 value reflects that extraction is neither total (some religious institutional capacity persists, some communities maintain doctrinal transmission) nor minimal (significant institutional authority and jurisdictional capacity has been lost in modernized secular states). The upward trajectory (0.32→0.58) shows accumulating extraction as secular apparatus consolidates. Suppression (0.62): Moderate-high. Suppression includes external legal restrictions (restrictions on religious law, curriculum mandates, tax policy, conscience exemptions elimination) and internalized pressure (younger cohorts' cognitive shift toward secular framing, identity-lock into secular epistemic standards). The bidirectionality is critical: suppression operates both as material barrier (legally prohibited religious authority) and as cognitive capture (younger people internalize secular framing and find religious authority unthinkable). Theater ratio (0.65): Moderate-high. Religious institutions persist through performative functions (ceremonial roles, symbolic authority, social legitimacy rituals) even as functional authority erodes. A bishop's ceremonial function at state events carries symbolic legitimacy but minimal binding jurisdiction. The rising trajectory (0.48→0.65) reflects increasing performativity — religious authority must justify itself through theater because functional justification weakens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how structural position produces radically different classifications from identical base properties. The secular state apparatus experiences this as low-extraction coordination (Rope) — secularization solves genuine coordination problems (unified legal system, non-doctrinal education). The religious institutional leadership experiences this as mixed coordination-extraction (Tangled Rope) — genuine institutional functions (organizing faith, maintaining community) persist alongside extraction of authority. The doctrinal transmission community experiences this as pure entrapment (Snare) — the constraint is identity-locked, bearing full extraction cost with no exit. The accommodation movement experiences this as a temporary problem with a sunset (Scaffold) — constitutional pluralism is a real alternative pathway. The formal institution experiences this as degraded inertia (Piton) — ceremonial persistence despite functional erosion. The civilizational observer risks seeing immutable modernization (Mountain) when the structural data reveals contingent institutional arrangement. This perspectival gap reveals how the same constraint exhibits all six types depending on observer position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from the agent's structural position: power level, exit options, and beneficiary/victim relationship. The beneficiary (secular state, institutional/arbitrage) derives low d (approximately 0.15-0.25): benefits flow toward the state, exit options are open, institutional power is high — experienced extractiveness is low or negative (the state experiences coordination value). The identity-locked victim (doctrinal transmission community, powerless/identity_locked) derives high d (approximately 0.88-0.92): bears full costs, exit is psychologically impossible (identity fusion), power is minimal — experienced extractiveness is maximum. Organized intermediates (religious leadership, institutional accommodation movement) derive moderate d (approximately 0.45-0.65): constrained exit options, mixed beneficiary/victim relationship, some institutional power — experienced extractiveness is moderate. The piton perspective derives moderate d with high theater ratio override — the institution's apparent power is undermined by functional degradation masked by ceremonial persistence. The false-summit (mountain) perspective derives d from the analytical position (approximately 0.72), which the FSM detector will flag because the beneficiaries (secular state, secular institutions) are clearly identifiable despite the mountain classification attempting to naturalize the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   THEORETICAL RESOLUTION: Secularization pressure resolves the mandatrophy by demonstrating that classification type is fundamentally perspectival. The constraint is simultaneously all six types from different structural positions. The mandatrophy is not 'which type is really correct' but 'what are the structural conditions that produce each type?' The secular state apparatus's rope perspective is genuine — secularization does solve coordination problems. The religious community's snare perspective is genuine — practitioners do experience identity-locked entrapment. The accommodation movement's scaffold perspective is genuine — constitutional pluralism is a real alternative pathway if activist agents build it. The formal institution's piton perspective is genuine — ceremonial authority does persist through degraded theater. The analytical observer's mountain perspective is a false summit — the constraint naturalizes a contingent institutional outcome. No single type subsumes the others. The presheaf of all perspectives IS the correct analytical description. The mandatrophy resolves through recognition that the constraint's identity is not determinate before specifying the observation position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    secularization_inevitability_thesis,
    'Is secularization pressure an inevitable consequence of modernization, or a contingent outcome of specific institutional choices?',
    'Comparative historical analysis: societies with high institutional secularization vs. societies with robust religious authority coexisting with modern state apparatus (e.g., Israel, Iran, parts of Eastern Europe); examination of counterfactual policies that might preserve religious authority within modern contexts',
    'If inevitable: mountain classification correct; secularization pressure is immutable. If contingent: false summit confirmed; secularization pressure is a constructed constraint whose enforcement depends on specific beneficiaries and could be reshaped by different institutional choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secularization_inevitability_thesis, conceptual, 'Whether secularization is inevitable consequence of modernization or contingent institutional outcome').

omega_variable(
    identity_lock_vs_material_suppression,
    'Among doctrinal transmission communities, what proportion of experienced constraint is identity-locked (cognitive/identity fusion) versus materially suppressed (legal restrictions, economic barriers)?',
    'Longitudinal study of practitioners post-exit: do those who leave the faith community experience persistent suppression effects (internalized beliefs about secular inevitability, difficulty reconstructing identity), or do suppression effects dissipate once legal/economic barriers are removed? Comparison of communities with high legal suppression vs. low legal suppression but high cultural secularization pressure.',
    'If predominantly identity-locked: constraint persists through internalized framing even after legal barriers removed; higher effective suppression than structural measures suggest. If predominantly material: constraint relaxes as legal/economic barriers are removed; exit is genuinely possible for those who can overcome material costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_material_suppression, empirical, 'Proportion of suppression that is identity-locked vs. materially structural').

omega_variable(
    accommodation_framework_sufficiency,
    'Do religious accommodation frameworks (conscience protections, religious liberty law, institutional pluralism) actually restore meaningful religious authority, or do they merely create space for privatized belief within secular governance structures?',
    'Comparative analysis of accommodation frameworks: Do they restore religious institutional capacity for doctrinal enforcement, community binding, legal jurisdiction, or merely protect individual practice? Analysis of whether religious communities experience scaffold sunset dynamics (increasing institutional authority as accommodation matures) or continued extraction under accommodation (protected but marginal status).',
    'If restoration: scaffold sunset is structural; religious authority can be rebuilt within secular constitutional frameworks. If privatization: accommodation merely contains religious practice within secular structures; the constraint persists through different mechanism (pressure to conform practice to secular norms rather than explicit restriction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_framework_sufficiency, empirical, 'Whether accommodation frameworks restore religious authority or merely privatize belief').

omega_variable(
    doctrine_transmission_erosion_mechanism,
    'What is the primary mechanism of doctrinal transmission erosion under secularization pressure: loss of institutional capacity to enforce orthodoxy, or cognitive shift in younger generations away from inherited doctrines?',
    'Analysis of doctrinal disputes and schism patterns: Do traditional authorities lose enforcement power (hierarchical decisions no longer obeyed), or do younger cohorts internalize secular framing and reinterpret doctrine accordingly? Examination of communities where institutional authority remains strong but doctrinal adherence weakens versus communities where doctrinal adherence persists despite institutional erosion.',
    'If institutional capacity loss: the constraint operates through suppression of religious authority''s coercive power; may be reversible if institutions regain enforcement capacity. If cognitive shift: the constraint operates through identity-locking of younger generations into secular frameworks; reversal requires cultural re-orientation of epistemic standards.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_transmission_erosion_mechanism, empirical, 'Primary mechanism of doctrinal transmission erosion').

omega_variable(
    cross_tradition_variable_pressure,
    'Does secularization pressure operate uniformly across religious traditions (Christianity, Islam, Judaism, Hinduism, Buddhism, etc.), or does it vary substantially by tradition''s doctrinal structure, institutional organization, and historical relationship to state authority?',
    'Comparative analysis of secularization trajectories: Christian denominations in Western Europe vs. Christianity in non-Western contexts vs. Islamic law systems vs. Buddhist institutional structures vs. Hindu-majority state arrangements. Identification of which doctrinal features (hierarchical vs. distributed authority, text-based vs. practice-based, state-integrated vs. state-separate historically) predict vulnerability to secularization pressure.',
    'If uniform: single constraint story applies across traditions; ε and suppression values are stable. If variable: multiple constraint stories needed, decomposed by tradition; ε varies substantially by doctrinal and institutional structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cross_tradition_variable_pressure, empirical, 'Whether secularization pressure operates uniformly or varies by religious tradition').

omega_variable(
    performative_religious_authority_stability,
    'Can performative religious authority (piton state) persist indefinitely at high theater ratios, or does it eventually degrade further toward irrelevance?',
    'Historical analysis of institutions at high theater ratios (established churches with ceremonial roles but eroded functional authority): Do they stabilize at theater ratios 0.65-0.85, or do they trend toward complete displacement? Analysis of institutional lifecycle: what sustains theater over decadal timescales? When does theater become insufficient to maintain institutional coherence?',
    'If stable: piton can persist for centuries; secularization pressure''s extraction through institutional degradation reaches an equilibrium. If trending to displacement: piton is a transitional phase toward complete institutional erosion; extraction continues until religious institutions disappear entirely from public structures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(performative_religious_authority_stability, empirical, 'Whether performative religious authority can stabilize long-term').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secularization_pressure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sec_pres_tr_t0, secularization_pressure, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sec_pres_tr_t3, secularization_pressure, theater_ratio, 3, 0.55).
narrative_ontology:measurement(sec_pres_tr_t6, secularization_pressure, theater_ratio, 6, 0.6).
narrative_ontology:measurement(sec_pres_tr_t10, secularization_pressure, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(sec_pres_be_t0, secularization_pressure, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sec_pres_be_t3, secularization_pressure, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(sec_pres_be_t6, secularization_pressure, base_extractiveness, 6, 0.51).
narrative_ontology:measurement(sec_pres_be_t10, secularization_pressure, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sec_pres_su_t0, secularization_pressure, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(sec_pres_su_t6, secularization_pressure, suppression_requirement, 6, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secularization_pressure, enforcement_mechanism).
narrative_ontology:affects_constraint(secularization_pressure, doctrinal_transmission_erosion).
narrative_ontology:affects_constraint(secularization_pressure, religious_institutional_jurisdiction_loss).
narrative_ontology:affects_constraint(secularization_pressure, secular_epistemic_standard_establishment).

% DUAL FORMULATION NOTE:
% Secularization pressure is the macro-level constraint describing the cumulative effect of multiple institutional mechanisms (legal displacement of religious authority, education secularization, epistemic standard shifts, institutional funding/prestige concentration). The downstream constraints track specific mechanisms: doctrinal transmission erosion addresses cognitive/identity mechanisms; jurisdiction loss addresses legal/structural mechanisms; epistemic standard establishment addresses the intellectual authority shift. Secularization pressure as a unifying frame links these mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(secularization_pressure, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
