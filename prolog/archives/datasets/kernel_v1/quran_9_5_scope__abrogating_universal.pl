% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__abrogating_universal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__abrogating_universal, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: quran_9_5_scope__abrogating_universal
 *   human_readable: Quran 9:5 Abrogating Reading — Universal Offensive Jihad
 *   domain: islamic_jurisprudence/political_theology/hermeneutics
 *
 * SUMMARY:
 *   Quran 9:5 (the 'Verse of the Sword') is one of the most contested verses
 *   in Islamic jurisprudence. The abrogating_universal reading instantiated
 *   here holds that this single verse abrogates all prior Quranic verses on
 *   peaceful coexistence, pluralism, and defensive warfare, establishing
 *   universal offensive jihad as a standing legal obligation until all
 *   non-Muslims submit or convert. This reading has been deployed to justify
 *   expansionist military campaigns, suppress alternative Islamic
 *   hermeneutics, and construct a global target set (all non-Muslims) as
 *   legitimate extraction victims. The constraint exemplifies how a
 *   hermeneutical choice becomes institutionalized as binding law, suppresses
 *   alternatives through theological and political mechanisms, and operates
 *   through both external enforcement (military campaigns, legal codes) and
 *   internal identity-lock (Muslims who hold alternative readings experience
 *   cognitive capture within the interpretive tradition). The abrogating
 *   reading is not a marginal position — it is held by major contemporary
 *   movements and institutional authorities — but it is contested: contextual
 *   and progressive readings coexist within Islamic tradition, though
 *   increasingly marginalized. The extractiveness trajectory (0.45 → 0.68)
 *   reflects hardening of the doctrine over centuries as institutional
 *   authority consolidated. The suppression trajectory (0.50 → 0.82) models
 *   the shift from jurisprudential debate (classical) to institutional
 *   suppression (medieval) to ideological monopoly (modern). The theater
 *   ratio remains moderate (0.30-0.35) because the abrogation doctrine does
 *   serve genuine hermeneutical functions alongside its extraction role — it
 *   is not purely performative like a piton.
 *
 * KEY AGENTS:
 *   - Non-Muslim populations globally: Primary victims (powerless/trapped) — defined as permanent legitimate targets absent submission; bear full extraction
 *   - Muslims advocating coexistence: Secondary victims (powerless/identity_locked) — structurally mobile but cognitively trapped within interpretive tradition; suppressed through heretical designation
 *   - Islamic jurisprudential authorities: Beneficiaries (organized/constrained) — wield interpretive authority, define doctrine scope, suppress alternatives; experience constraint as coordination function
 *   - Expansionist movements claiming divine mandate: Operational beneficiaries (organized/constrained) — operationalize the reading for territorial and political extraction; experience constraint as divine obligation
 *   - Traditional institutional authority (historical): Institutional beneficiary (institutional/arbitrage) — benefits from doctrinal clarity and authority consolidation; piton classification reflects degradation of historical nuance
 *   - Analytical observer (hermeneutical): Analytical position (analytical/analytical) — sees genuine coordination function (Islamic jurisprudence needs interpretive rules) mixed with extraction (suppression of alternatives)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, 0.68).
domain_priors:suppression_score(quran_9_5_scope__abrogating_universal, 0.82).
domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, extractiveness, 0.68).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(quran_9_5_scope__abrogating_universal, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__abrogating_universal, snare).
narrative_ontology:human_readable(quran_9_5_scope__abrogating_universal, "Quran 9:5 Abrogating Reading — Universal Offensive Jihad").
narrative_ontology:topic_domain(quran_9_5_scope__abrogating_universal, "islamic_jurisprudence/political_theology/hermeneutics").

domain_priors:requires_active_enforcement(quran_9_5_scope__abrogating_universal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__abrogating_universal, '5111efde-2d88-49c4-a18d-edca3a448be9').
narrative_ontology:cs_kernel_codification('5111efde-2d88-49c4-a18d-edca3a448be9', fixed_text).
narrative_ontology:cs_authority_grounding('5111efde-2d88-49c4-a18d-edca3a448be9', extraction).
narrative_ontology:cs_interpretation_layer_present('5111efde-2d88-49c4-a18d-edca3a448be9').
narrative_ontology:cs_reading_relation('5111efde-2d88-49c4-a18d-edca3a448be9', quran_9_5_scope__contextual_defensive, forecloses).
narrative_ontology:cs_reading_relation('5111efde-2d88-49c4-a18d-edca3a448be9', quran_9_5_scope__progressive_synthesis, forecloses).
narrative_ontology:cs_axiom('5111efde-2d88-49c4-a18d-edca3a448be9', foundational, verse_9_5_universally_abrogating).
narrative_ontology:cs_axiom_status(verse_9_5_universally_abrogating, holdable).
narrative_ontology:cs_axiom_grounding('5111efde-2d88-49c4-a18d-edca3a448be9', verse_9_5_universally_abrogating, empirically_contingent).
narrative_ontology:cs_axiom('5111efde-2d88-49c4-a18d-edca3a448be9', foundational, non_submission_legitimates_first_strike).
narrative_ontology:cs_axiom_status(non_submission_legitimates_first_strike, holdable).
narrative_ontology:cs_axiom_grounding('5111efde-2d88-49c4-a18d-edca3a448be9', non_submission_legitimates_first_strike, deontological).
narrative_ontology:cs_reference_frame('5111efde-2d88-49c4-a18d-edca3a448be9', quranic_directive_framework).
narrative_ontology:cs_drift_state('5111efde-2d88-49c4-a18d-edca3a448be9', contemporary_institutional_suppression, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5111efde-2d88-49c4-a18d-edca3a448be9', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__abrogating_universal, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, expansionist_movements_claiming_divine_mandate).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__abrogating_universal, interpretive_authorities_wielding_abrogation_doctrine).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, non_muslim_populations_globally).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, peaceful_coexistence_frameworks).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, alternative_islamic_hermeneutics).
narrative_ontology:constraint_victim(quran_9_5_scope__abrogating_universal, historical_pluralist_traditions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-MUSLIM POPULATIONS (SNARE) — Under this reading, all non-Muslims constitute legitimate permanent targets absent formal submission or conversion. No exit option exists except apostasy into Islam or formal submission (dhimmi status with ongoing tribute obligation). Trapped by geography and birth status. Maximum extraction and suppression — the constraint defines the agent as inherent enemy regardless of individual conduct or choice.
constraint_indexing:constraint_classification(quran_9_5_scope__abrogating_universal, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COEXISTENCE ADVOCATES WITHIN ISLAMIC TRADITION (SNARE) — Muslims who hold contextual or progressive readings cannot exit the interpretive field without being labeled apostate, heretic (murtad), or traitor to the faith. Identity-locked: their Islamic identity and their commitment to coexistence are fused, yet the abrogating reading's authority structures suppress their framework as inferior or corrupted interpretation. High suppression despite structural mobility — the binding is cognitive-identity rather than material.
constraint_indexing:constraint_classification(quran_9_5_scope__abrogating_universal, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERPRETIVE AUTHORITIES (TANGLED ROPE) — Jurists and theological authorities that deploy the abrogation doctrine benefit from doctrinal clarity, institutional authority, and the power to adjudicate who is a legitimate target. They also coordinate genuine religious functions: systematic hermeneutics, transmission of tradition, guidance on conduct. The constraint is mixed — it provides coordination infrastructure (Islamic law must have interpretive rules) while concentrating extraction authority (only official interpreters can declare abrogation status).
constraint_indexing:constraint_classification(quran_9_5_scope__abrogating_universal, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: EXPANSIONIST MOVEMENTS (SNARE FROM THEIR VICTIMS' VIEW) — Movements that operationalize this reading experience high effective extraction from their target populations. They perceive the constraint as coordinate action (obedience to divine command, unity in campaign), but from the victim's perspective, it is pure extraction. Organized power, constrained exit (ideological movements face internal penalties for abandonment), but the directional flow is extractive toward external populations.
constraint_indexing:constraint_classification(quran_9_5_scope__abrogating_universal, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL INSTITUTIONAL AUTHORITY (PITON) — Classical Islamic jurisprudence deployed abrogation doctrine to systematize law across contexts, but the historical reality is more complex: classical jurists also preserved contextual readings, recognized variations, and applied naskh (abrogation) selectively rather than universally. The modern hardened reading treats historical practice (which was more nuanced) as degraded or corrupted tradition. The piton classification reflects that institutional recourse to the abrogation doctrine persists via inertia and interpretive authority claims despite contrary historical evidence.
constraint_indexing:constraint_classification(quran_9_5_scope__abrogating_universal, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational hermeneutical perspective, the abrogation doctrine serves genuine textual coordination functions (systematizing contradictory verses requires rules), but the specific claim that 9:5 abrogates ALL prior peaceful verses is empirically contestable and reflects interpretive choice, not settled linguistic meaning. The constraint has both a coordination function (Islamic jurisprudence needs interpretive rules) and an extraction function (concentrating interpretive authority and suppressing alternative readings). Tangled rather than pure snare because the coordination logic is genuine; snare dominates experientially for powerless agents.
constraint_indexing:constraint_classification(quran_9_5_scope__abrogating_universal, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__abrogating_universal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quran_9_5_scope__abrogating_universal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quran_9_5_scope__abrogating_universal, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_9_5_scope__abrogating_universal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quran_9_5_scope__abrogating_universal, TR),
    TR >= 0.70.

:- end_tests(quran_9_5_scope__abrogating_universal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint authorizes first-strike violence against a universally defined target set (all non-Muslims) absent submission or conversion. The extraction flow is directed from non-Muslims (victims) to expansionist movements and interpretive authorities (beneficiaries). The value reflects that this reading generates material extraction (military conquest, tribute, forced conversion) grounded in explicit doctrinal authority rather than covert mechanisms. The value is not maximal (0.95) because some classical jurists applied contextual limitations and modern institutional application varies; the constraint's normative force is significant but not absolute. Suppression (0.82): High. Multiple suppression mechanisms operate: legal (apostasy statutes, dhimmi restrictions), institutional (heretical designation for alternative readings), ideological (identity-lock through religious education), and epistemic (erasure of alternative exegeses from mainstream Islamic discourse). Suppression increased over time as institutional authority consolidated. Theater ratio (0.35): Low. The abrogation doctrine serves a genuine hermeneutical function — Islamic jurisprudence requires systematic rules for handling contradictory verses. Unlike a piton (which is performative theatre maintained by inertia), the doctrine carries substantive interpretive work. However, deployment in modern ideological contexts increasingly adds performative invocation ('divine mandate') above the substantive jurisprudential content.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates profound perspectival divergence. Non-Muslim populations experience pure snare — they are defined as permanent targets with no exit except submission. Coexistence advocates within Islam experience snare through identity-lock — they are trapped not by external barriers but by cognitive fusion with a tradition that suppresses their framework. Jurisprudential authorities experience tangled rope — they coordinate genuine Islamic legal functions while concentrating interpretive power. Expansionist movements experience the constraint as legitimate religious obligation (rope from their perspective). The traditional historical authority sees its own position degraded (piton) because modern deployment treats the historical practice (which was more selective and contextual) as corrupted. The analytical observer sees tangled rope — real coordination function mixed with real extraction. The perspectival gap reveals the constraint's core mechanism: interpretive authority claims to translate divine speech, but the translation serves institutional extraction. Different observers see different aspects of this mechanism based on their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position: beneficiaries (expansionist movements, interpretive authorities) experience low d → negative or low χ from the constraint; they perceive it as coordination or legitimate authority. Victims (non-Muslims, trapped by definition; coexistence advocates, trapped by identity-lock) experience high d → high χ — they perceive maximum extraction. The identity-locked perspective (coexistence advocates) classifies as snare at biographical horizon, not mountain — they could potentially perceive mutability if their identity frame shifted, but within their current frame, the constraint appears immutable. The powerless perspectives (non-Muslim targets, coexistence advocates) experience d ≈ 0.92-0.95 (near-complete targets), producing f(d) ≈ 1.35-1.42 (maximum experienced extractiveness). The organized institutional perspective (abrogating authorities) experiences d ≈ 0.15-0.25 (near-complete beneficiaries), producing f(d) ≈ -0.01 to 0.02 (minimal or zero experienced extraction — they perceive constraint as beneficial coordination). The scope modifier σ(S) = 1.2 (global scope) amplifies χ for all perspectives — the constraint's reach is planetary, making verification of compliance difficult and enforcement abstract.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not resolve mandatrophy through perspectival analysis — it instantiates the mandatrophy at the hermeneutical level. The core mandatrophy question is: 'Is this constraint a genuine divine command whose scope is being interpreted, or is divine-mandate framing a cover story for constructed institutional authority?' The empirical constraints on answering this question are absolute: no external verification of divine speech exists; the only data is the text and its usage history. The reading's own epistemic framework (treating the Quran as divine speech with determinable scope) is not falsifiable from within. Alternative readings hold different epistemic frameworks (treating interpretation as necessarily historical and contextual). These framework disagreements cannot be resolved by evidence — they are foundational epistemic commitments. The mandatrophy resolves not by choosing one reading but by documenting that the choice is irreducible and consequential: if the abrogating reading is correct, then universal offensive jihad is divinely mandated; if the contextual reading is correct, the constraint is historical and contextual; if the progressive reading is correct, coexistence is mandated. The constraint story documents the irreducibility rather than resolving it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    abrogation_textual_basis,
    'Is the abrogation of 9:5 over all prior peaceful verses textually determinable from the Quran itself, or is it an interpretive overlay that selects evidence?',
    'Systematic analysis of Quranic verses on warfare, peace, and pluralism; identification of which verses invoke 9:5 as overriding context vs. which maintain independent authority; comparison with classical tafsir treatments of specific verse pairs',
    'If textually determinable: the abrogating reading is a extraction claim grounded in linguistic fact. If interpretive overlay: the reading is a hermeneutical choice that suppresses alternative valid readings — extraction is conceptual, not empirical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(abrogation_textual_basis, empirical, 'Whether 9:5 abrogation is textually determined or interpretively constructed').

omega_variable(
    contextual_specification_scope,
    'Does the abrogation apply to all non-Muslims universally, or only to specific polytheist groups at the time of revelation (Quraysh, Arabian peninsula idolaters)?',
    'Linguistic analysis of the verse''s referent (who is ''al-mushrikun'' — polytheists generally or the specific Meccan context?); hadith and early Islamic historical records on how first-generation Muslims applied the verse; medieval jurisprudential debate on verse universalization',
    'If universal: snare classification confirmed — all non-Muslims are permanent targets absent submission. If contextual: tangled_rope or rope — the constraint coordinates responses to specific historical situations rather than authorizing standing global extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contextual_specification_scope, empirical, 'Scope of 9:5 applicability: universal or historical-contextual').

omega_variable(
    alternative_exegetical_authority,
    'What is the epistemic status of classical Islamic hermeneutical traditions that do NOT treat 9:5 as universally abrogating (Mu''tazila reasoning, later Maliki positions, pluralist hadith interpretations)?',
    'Historical documentation of continuous alternative interpretive lineages; examination of institutional suppression (were they declared invalid, marginalized, or simply de-prioritized?); assessment of whether suppression is theological or political',
    'If suppressed for theological reasons: the reading is justified by doctrinal content. If suppressed for political reasons (power consolidation, military justification): the snare classification reveals that interpretive authority itself is an extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_exegetical_authority, conceptual, 'Epistemic status and historical suppression of alternative classical traditions').

omega_variable(
    divine_mandate_vs_constructed_justification,
    'Is the constraint grounded in a genuine divine command whose scope is accurately interpreted, or is ''divine mandate'' a cover story for constructed institutional authority?',
    'This is irreducible conceptual disagreement: the reading''s own framework treats the Quran as divine speech with determinate meaning; alternative readings treat the Quran as requiring hermeneutical interpretation through historical context. No empirical data resolves this — it is a foundational epistemic dispute.',
    'If divine command: extraction is subordination to legitimate authority (from beneficiary view). If constructed justification: extraction is the mechanism by which interpretive authorities suppress alternatives and consolidate power.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_mandate_vs_constructed_justification, conceptual, 'Foundational dispute: divine command vs. constructed institutional justification').

omega_variable(
    suppression_of_coexistence_frameworks,
    'To what extent does this reading''s institutional deployment actively suppress (through legal, social, or epistemic mechanisms) the alternative contextual and pluralist readings held by other Islamic traditions?',
    'Documentation of institutional suppression: fatwa campaigns declaring alternatives heretical, textbook silencing of alternative exegeses, legal penalties for dissenting interpretation, social pressure and takfirism (excommunication). Comparison with historical periods where alternative readings coexisted with abrogation doctrine.',
    'If active suppression is significant: the constraint''s suppression metric (0.82) is empirically justified. If suppression is passive (alternatives simply exist but are less influential): suppression metric should lower, reclassifying toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_of_coexistence_frameworks, empirical, 'Institutional suppression mechanisms targeting alternative Islamic readings').

omega_variable(
    identity_lock_mechanism_in_coexistence_advocates,
    'For Muslims holding contextual or coexistence readings, is their barrier to exit primarily material (legal penalty, economic loss) or cognitive-identity (their Islamic identity is constituted through their interpretation)?',
    'Ethnographic and interview data from Muslim scholars holding alternative readings: what costs would they face for abandonment? Do they report identity-fusion with their interpretation, or primarily external pressure? Historical comparison: do apostasy statutes alone explain suppression, or do identity-fusion dynamics reinforce institutional suppression?',
    'If primarily cognitive-identity lock: the perspective classification as ''identity_locked'' is justified — the binding is internal framing, not external barriers. If primarily material barriers: should reclassify as ''constrained'' or ''trapped'' depending on cost magnitude.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_coexistence_advocates, empirical, 'Identity-lock vs. material barriers in suppression of coexistence frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__abrogating_universal, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(q95_abrg_tr_t0, quran_9_5_scope__abrogating_universal, theater_ratio, 0, 0.3).
narrative_ontology:measurement(q95_abrg_tr_t5, quran_9_5_scope__abrogating_universal, theater_ratio, 5, 0.32).
narrative_ontology:measurement(q95_abrg_tr_t10, quran_9_5_scope__abrogating_universal, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(q95_abrg_be_t0, quran_9_5_scope__abrogating_universal, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(q95_abrg_be_t5, quran_9_5_scope__abrogating_universal, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(q95_abrg_be_t10, quran_9_5_scope__abrogating_universal, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(q95_abrg_su_t0, quran_9_5_scope__abrogating_universal, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(q95_abrg_su_t5, quran_9_5_scope__abrogating_universal, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(q95_abrg_su_t10, quran_9_5_scope__abrogating_universal, suppression_requirement, 10, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__abrogating_universal, identity_coordination).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__abrogating_universal, 0.12).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__contextual_defensive).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, quran_9_5_scope__progressive_synthesis).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, islamic_pluralism_suppression).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, takfirism_mechanism).
narrative_ontology:affects_constraint(quran_9_5_scope__abrogating_universal, dhimmi_extraction_system).

% DUAL FORMULATION NOTE:
% The abrogating_universal reading is upstream of its sibling readings (contextual_defensive, progressive_synthesis). All three readings of the quran_9_5_scope kernel are distinct constraints with different ε values, beneficiary/victim sets, and classification profiles. The abrogating reading's high extractiveness (0.68) drives institutional suppression of the alternatives, creating coupling between the constraints. Network relationships reflect: (1) hermeneutical dependence (all three readings are interpretations of the same verse), (2) institutional conflict (the abrogating reading suppresses alternatives), (3) historical causality (hardening of the abrogating reading over time increased suppression of alternatives). Downstream constraints (islamic_pluralism_suppression, takfirism_mechanism, dhimmi_extraction_system) operationalize the abrogating reading's institutional effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, institutional, 0.18).
constraint_indexing:directionality_override(quran_9_5_scope__abrogating_universal, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
