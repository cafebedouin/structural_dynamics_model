% ============================================================================
% CONSTRAINT STORY: quran_hadith_substrate__traditionalist_taqlid
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_hadith_substrate__traditionalist_taqlid, []).

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
 *   constraint_id: quran_hadith_substrate__traditionalist_taqlid
 *   human_readable: Traditionalist Taqlid: Classical Fiqh Schools as Binding Authority
 *   domain: islamic_jurisprudence/religious_authority/legal_theory
 *
 * SUMMARY:
 *   Classical fiqh schools (Hanafi, Maliki, Shafi'i, Hanbali in Sunni
 *   tradition; Ja'fari and others in Shi'a) represent institutionalized
 *   jurisprudential consensus supposedly derived from Qur'an, Hadith, and
 *   scholarly ijma (community agreement). Traditionalist taqlid doctrine
 *   prescribes that contemporary Muslims are obligated to follow established
 *   madhhab rulings rather than conduct independent ijtihad (jurisprudential
 *   reasoning). This constraint exhibits the tangled-rope structure: it
 *   solves a genuine coordination problem (unified jurisprudence across
 *   diverse Muslim communities, preservation of scholarly expertise,
 *   prevention of ad hoc Qur'anic interpretation by untrained believers)
 *   while simultaneously enabling extraction (freezing jurisprudence into
 *   historical forms, suppressing reinterpretation by marginalized groups,
 *   institutionalizing clerical authority). The constraint's evolution over
 *   the interval reflects increasing suppression as traditionalist authority
 *   structures have institutionalized taqlid enforcement mechanisms (formal
 *   madhhab curricula, clerical certification, mosque-based authority
 *   hierarchies) while simultaneously experiencing rising theater as modern
 *   states invoke fiqh for legitimacy while circumventing it through
 *   constitutional law and legislative override. The constraint is ONE
 *   READING of the contested kernel: Qur'an-Hadith substrate authority.
 *   Sibling readings (reformist ijtihad, state hybrid) offer structurally
 *   different claims about how Islamic jurisprudence should relate to
 *   contemporary contexts.
 *
 * KEY AGENTS:
 *   - Institutional Ulama & Madhhab Establishment (institutional/arbitrage): Primary beneficiary — control jurisprudential authority, derive prestige and institutional power from taqlid enforcement, have exit optionality (can issue fatwa, adapt rulings through doctrinal reserves)
 *   - Progressive Muslim Interpreters (powerless/identity_locked): Primary victim — structurally mobile (can read texts) but identity-bound to Islamic tradition; taqlid forecloses independent interpretation experienced as apostasy from within identity frame
 *   - Women Seeking Doctrinal Equality (powerless/trapped): Victims locked into classical rulings on marriage, inheritance, testimony; multiple barriers to exit (identity, material law systems, family authority, economic dependency)
 *   - Religious Minorities Under Dhimmi Framework (organized/constrained): Victims subject to classical legal disabilities; coordinated exit options (conversion, emigration) carry heavy costs
 *   - Reformist Ulama (moderate/constrained): Secondary victims/beneficiaries — benefit from madhhab authority legitimacy but constrained by intellectual closure; face career risk if departing taqlid
 *   - Modern State Legal Systems (institutional/arbitrage): Piton actors — formally invoke fiqh for legitimacy while systematically overriding classical rules through constitutional law; theater persists through inertia
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent institutional arrangements as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, 0.62).
domain_priors:suppression_score(quran_hadith_substrate__traditionalist_taqlid, 0.68).
domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, extractiveness, 0.62).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_hadith_substrate__traditionalist_taqlid, tangled_rope).
narrative_ontology:human_readable(quran_hadith_substrate__traditionalist_taqlid, "Traditionalist Taqlid: Classical Fiqh Schools as Binding Authority").
narrative_ontology:topic_domain(quran_hadith_substrate__traditionalist_taqlid, "islamic_jurisprudence/religious_authority/legal_theory").

domain_priors:requires_active_enforcement(quran_hadith_substrate__traditionalist_taqlid).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_hadith_substrate__traditionalist_taqlid, 'afd9b7a5-9eb7-463a-abd7-9bb4751a02ce').
narrative_ontology:cs_kernel_codification('afd9b7a5-9eb7-463a-abd7-9bb4751a02ce', fixed_text).
narrative_ontology:cs_authority_grounding('afd9b7a5-9eb7-463a-abd7-9bb4751a02ce', lineage).
narrative_ontology:cs_interpretation_layer_present('afd9b7a5-9eb7-463a-abd7-9bb4751a02ce').
narrative_ontology:cs_reading_relation('afd9b7a5-9eb7-463a-abd7-9bb4751a02ce', quran_hadith_substrate__reformist_ijtihad, coexists_with).
narrative_ontology:cs_reading_relation('afd9b7a5-9eb7-463a-abd7-9bb4751a02ce', quran_hadith_substrate__state_hybrid, influences).
narrative_ontology:cs_axiom('afd9b7a5-9eb7-463a-abd7-9bb4751a02ce', foundational, ijma_creates_binding_consensus).
narrative_ontology:cs_axiom_status(ijma_creates_binding_consensus, holdable).
narrative_ontology:cs_axiom_grounding('afd9b7a5-9eb7-463a-abd7-9bb4751a02ce', ijma_creates_binding_consensus, conventional).
narrative_ontology:cs_axiom('afd9b7a5-9eb7-463a-abd7-9bb4751a02ce', foundational, classical_closure_necessary_for_community_unity).
narrative_ontology:cs_axiom_status(classical_closure_necessary_for_community_unity, holdable).
narrative_ontology:cs_axiom_grounding('afd9b7a5-9eb7-463a-abd7-9bb4751a02ce', classical_closure_necessary_for_community_unity, instrumental).
narrative_ontology:cs_reference_frame('afd9b7a5-9eb7-463a-abd7-9bb4751a02ce', ijma_consensus_binding_authority).
narrative_ontology:cs_drift_state('afd9b7a5-9eb7-463a-abd7-9bb4751a02ce', contemporary_post_colonial_pluralism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('afd9b7a5-9eb7-463a-abd7-9bb4751a02ce', '').
narrative_ontology:cs_kernel_id(quran_hadith_substrate__traditionalist_taqlid, quran_hadith_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, institutional_ulama).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, madhhab_establishment).
narrative_ontology:constraint_beneficiary(quran_hadith_substrate__traditionalist_taqlid, mosque_hierarchies).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, progressive_muslim_interpreters).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, women_seeking_equal_legal_status).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, religious_minorities_under_dhimmi).
narrative_ontology:constraint_victim(quran_hadith_substrate__traditionalist_taqlid, lay_believers_constrained_by_closure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PROGRESSIVE MUSLIM INTERPRETER (SNARE) — Structurally mobile (can read primary texts, access scholarship, teach) but identity-locked into Islamic tradition that requires fidelity to Qur'an and Hadith as ultimate authority. The taqlid constraint forecloses independent interpretation; departure from the madhhab is experienced as apostasy or heresy from within the identity frame. High extraction: the constraint forbids the agent from exercising their interpretive capacity even though the resources (texts) are available. Suppression is high — clerical authority and community sanction enforce the closure.
constraint_indexing:constraint_classification(quran_hadith_substrate__traditionalist_taqlid, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: WOMEN SEEKING DOCTRINAL EQUALITY (SNARE) — In many traditionalist-dominant contexts, taqlid to classical fiqh schools freezes women into jurisprudential roles (witness inequality, inheritance asymmetry, marriage guardianship) that classical scholars derived under different historical conditions. Women cannot exit the constraint through reinterpretation (that would violate taqlid) and face material barriers (legal systems, family authority, economic dependency) preventing exit through evasion. Maximum extraction: the constraint simultaneously locks identity, restricts exit, and enforces gendered legal inferiority.
constraint_indexing:constraint_classification(quran_hadith_substrate__traditionalist_taqlid, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: REFORMIST ULAMA (TANGLED ROPE) — Scholars trained within the madhhab who recognize classical rulings as historically contingent but are constrained by career dependence on mosque positions, institutional affiliation, and community authority structures that require madhhab legitimacy. They benefit from the coordination function of taqlid (it establishes their authority, provides predictable jurisprudence) while bearing extraction costs (intellectual constraint, doctrinal closure). Can theoretically exit (some do, becoming independent muftis or leaving institutional roles) but at high cost to professional identity and livelihood.
constraint_indexing:constraint_classification(quran_hadith_substrate__traditionalist_taqlid, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL ULAMA & MADHHAB ESTABLISHMENT (ROPE) — Primary beneficiaries. Taqlid solves a genuine coordination problem: it provides unified jurisprudence across communities, enables delegation of legal interpretation to trained scholars, and prevents ad hoc scriptural reasoning by untrained believers. The constraint generates real coordination benefits. These actors have arbitrage options (they can reinterpret if needed, leverage fatwa authority, adapt rulings through istislah or maslaha reasoning) and experience the constraint as legitimate authority that they willingly enforce. Low net extraction — the benefits are genuine.
constraint_indexing:constraint_classification(quran_hadith_substrate__traditionalist_taqlid, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RELIGIOUS MINORITIES UNDER DHIMMI FRAMEWORK (TANGLED ROPE) — In traditionalist contexts that enforce classical fiqh, non-Muslim minorities operate under dhimmi legal status (derived from Umayyad-era jurisprudence, frozen via taqlid). These communities have some coordination benefit: legal status is predictable, contracts are enforceable, limited autonomy is guaranteed. But extraction is asymmetric: legal disabilities (jizya tax, dress codes, testimony restrictions, marriage law) are encoded and suppression is high. Constrained exit: minorities can convert to Islam (theoretically avoiding dhimmi status) or emigrate, but both options carry heavy costs.
constraint_indexing:constraint_classification(quran_hadith_substrate__traditionalist_taqlid, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MODERN STATE LEGAL SYSTEMS (PITON) — Many contemporary Muslim-majority states formally retain classical fiqh (through constitutions, personal status law codes, shari'a courts) while simultaneously enforcing modern secular law, constitutional rights, and international human rights norms. This creates performative traditionalism: taqlid is invoked for legitimacy while being systematically circumvented through legislative amendment, executive decree, and parallel legal regimes. Theater ratio is high (0.58) — the ritualistic invocation of fiqh schools masks functional departure from classical rulings. The institutional setup is degraded: the classical fiqh-based system persists through inertia and legitimacy theater, not because it functions without contradiction.
constraint_indexing:constraint_classification(quran_hadith_substrate__traditionalist_taqlid, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a civilizational frame, the taqlid constraint might be read as immutable: legal systems require interpretive closure to function; Islamic tradition's solution (madhhab consensus, binding precedent, doctrinal coherence) reflects inherent limits of jurisprudential knowledge. The constraint appears as a natural law of how complex legal traditions stabilize. However, this classification is a FALSE SUMMIT: the structural data reveals beneficiaries (institutional ulama) and victims (progressive interpreters, women, minorities) whose interests are served by naturalizing this constraint. The 'immutability' is maintained through institutional power, not inherent logic.
constraint_indexing:constraint_classification(quran_hadith_substrate__traditionalist_taqlid, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_hadith_substrate__traditionalist_taqlid_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quran_hadith_substrate__traditionalist_taqlid, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quran_hadith_substrate__traditionalist_taqlid, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_hadith_substrate__traditionalist_taqlid, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(quran_hadith_substrate__traditionalist_taqlid, TR),
    TR >= 0.70.

:- end_tests(quran_hadith_substrate__traditionalist_taqlid_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. The constraint generates real coordination benefits (unified jurisprudence, expert authority, community cohesion) but also asymmetric extraction: progressive interpreters cannot reinterpret despite having access to texts; women are frozen into historical legal roles; minorities face legal disabilities. The extractiveness value reflects that the coordination function is genuine (supporting rope classification from some perspectives) but substantially undermined by identifiable beneficiaries and victims whose structural positions indicate extraction. Suppression (0.68): High and increasing. The interval measurements show suppression rising from 0.52 to 0.68 as traditionalist institutions have formalized enforcement mechanisms (madhhab-based legal education, clerical certification, mosque hierarchies that enforce doctrinal conformity). Suppression is not just structural barriers to exit but also identity-level constraint: departure from taqlid is experienced as religious violation. Theater ratio (0.58): Moderate-high and increasing. Modern Muslim-majority states formally retain classical fiqh (constitutional references, personal status law codes, shari'a courts) while functionally departing through constitutional guarantees, legislative amendment, and executive decree. The theater has increased as the gap between invoked traditionalism and actual law has widened — states need the legitimacy of fiqh-based authority while unable to enforce classical rulings on gender, apostasy, dhimmi status.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Institutional ulama perceive genuine coordination (Rope) — taqlid solves the problem of unified jurisprudence and preserves scholarly expertise. Progressive interpreters perceive imprisonment (Snare) — the constraint forbids reinterpretation despite having epistemic capacity and textual access. Women perceive compound extraction (Snare) — legal disability plus identity lock plus material barriers. Minorities perceive asymmetric legal status (Tangled Rope) — coordination benefits exist (predictable law) alongside systematic disabilities. Reformist ulama perceive constrained benefit (Tangled Rope) — they benefit from institutional legitimacy but constrained by intellectual closure. Modern states perceive their own degradation (Piton) — they invoke fiqh for legitimacy while functionally departing from it. The analytical observer risks perceiving immutable law (Mountain) but the structural data reveals a false summit: beneficiaries and victims whose interests are served by naturalizing the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to extraction flow. Institutional ulama as beneficiaries with arbitrage exit options derive low d (around 0.15–0.20), experiencing negative or low χ — they see coordination. Progressive interpreters as powerless identity-locked victims derive high d (around 0.88–0.92), experiencing high χ — they see extraction despite having some structural mobility (can read texts). Women as powerless trapped victims derive maximum d (around 0.95), experiencing maximum χ. Minorities as organized-constrained victims derive moderate-high d (around 0.75–0.80). Reformist ulama as moderate constrained agents benefit from authority but bear intellectual costs, deriving middle-range d. The piton perspective's arbitrage exit (same as beneficiary ulama) but temporal framing (civilizational) and analytical power (institutional observing their own degradation) produce complex directionality. The mountain perspective's analytical position derives canonical d ≈ 0.73 but risks false-summit reclassification when beneficiary ulama facts are compiled into the system.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy not by reducing all perspectives to one type, but by recognizing that the perspectival divergence IS the structural reality. The apparent mandate (classical fiqh as binding authority) is simultaneously experienced as coordination (rope) and extraction (snare) depending on the observer's position. No single type 'resolves' the mandate — instead, the presheaf of classifications across the observation site reveals how institutional power structures the constraint differently for differently-positioned agents. The false-summit mountain perspective exposes the mechanism: naturalizing the constraint as immutable law serves the interests of beneficiaries who benefit from its institutionalization.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ijma_consensus_authenticity,
    'Did the early classical scholars (8th-10th centuries) genuinely achieve consensus-based ijma, or was the consensus partly constructed retroactively to justify centralized jurisprudential authority?',
    'Historical examination of early jurisprudential texts; reconstruction of actual scholarly disagreement vs. later consensus narratives; analysis of how majorities were determined in early fiqh councils',
    'If genuine consensus: taqlid is coordination-based (strengthens rope classification). If constructed: taqlid is enforcement-based (strengthens snare classification from victims'' perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_consensus_authenticity, empirical, 'Whether classical ijma was authentic consensus or constructed retroactively').

omega_variable(
    qiyas_suppression_mechanism,
    'Is the restriction on independent qiyas (legal analogy) a necessary condition for preventing jurisprudential chaos, or does it primarily serve institutional authority by foreclosing lay interpretation?',
    'Comparative analysis of how different traditions handle analogical reasoning (Jewish halakha, Christian canon law, secular common law); empirical assessment of whether qiyas restriction actually prevents inconsistency or merely concentrates interpretive power',
    'If necessary: suppression is coordination cost (strengthens rope reading). If primarily authority-preserving: suppression is extractive (strengthens snare reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(qiyas_suppression_mechanism, empirical, 'Whether qiyas restriction is coordination necessity or power preservation').

omega_variable(
    historical_contingency_of_rulings,
    'Are classical fiqh school rulings (especially on women''s legal status, non-Muslim rights, slavery) historically contingent responses to 7th-9th century conditions, or are they principled doctrinal positions applicable across contexts?',
    'Textual analysis of classical jurisprudential reasoning; comparison of how different schools justified the same rulings; examination of classical scholars'' explicit reasoning about contextual vs. universal principles',
    'If contingent: taqlid inappropriately freezes outdated rules (strengthens progressive critique, raises ε). If principled: classical reasoning remains applicable (strengthens traditionalist defense, lowers ε).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_contingency_of_rulings, conceptual, 'Whether classical rulings are historically contingent or principled universals').

omega_variable(
    taqlid_as_identity_constitution,
    'For Muslims in traditionalist-dominant communities, is taqlid experienced primarily as external enforcement (suppression via clerical authority) or as internal identity constitution (the agent''s understanding of what it means to be Muslim)?',
    'Ethnographic analysis of how Muslims describe their relationship to madhhab authority; examination of whether departure from taqlid is experienced as constraint violation (external) or identity dissolution (internal); comparison of exit narratives across reform-movement Muslims vs. apostasy cases',
    'If primarily external: suppression is structural (classification stable). If primarily internal: identity_locked exit option becomes primary mechanism (changes perspectives from trapped/constrained to identity_locked, lowers experienced χ but reveals cognitive capture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taqlid_as_identity_constitution, empirical, 'Whether taqlid functions as external enforcement or internal identity constitution').

omega_variable(
    reading_kernel_contest,
    'Which reading of the Qur''an-Hadith kernel is the authoritative interpretation: traditionalist taqlid (classical schools as binding consensus), reformist ijtihad (return to primary texts for each generation), or state_hybrid (legislative override of classical fiqh)?',
    'Textual analysis of Qur''anic verses about interpretation authority; historical examination of how Prophet and Companions handled jurisprudential disagreement; meta-analysis of how each reading justifies itself from primary sources',
    'This is the irreducible uncertainty of kernel reading. The three readings coexist in contemporary Islamic discourse; no empirical fact can resolve which is ''correct'' because the disagreement is fundamentally about authority legitimacy, not about factual claims. See cs_structure.reading_relations and axioms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Kernel reading contest: which interpretation of Qur''an-Hadith is authoritative').

omega_variable(
    modern_state_compatibility,
    'Can traditionalist taqlid coexist with modern constitutional governance, human rights law, and gender equality norms, or does adherence to classical fiqh necessarily conflict with these modern frameworks?',
    'Comparative analysis of Muslim-majority states that maintain fiqh-based law codes; examination of how constitutional courts resolve conflicts between shari''a and human rights norms; historical tracking of whether taqlid has in practice adapted to constitutional constraints',
    'If compatible: piton perspective is misclassified (the system is stable, not degraded). If incompatible: piton theater is genuine (the constraint persists through performative traditionalism despite functional rejection).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_state_compatibility, empirical, 'Compatibility of traditionalist taqlid with modern constitutional governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_hadith_substrate__traditionalist_taqlid, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trad_taqlid_tr_t0, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 0, 0.38).
narrative_ontology:measurement(trad_taqlid_tr_t3, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 3, 0.48).
narrative_ontology:measurement(trad_taqlid_tr_t6, quran_hadith_substrate__traditionalist_taqlid, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(trad_taqlid_be_t0, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(trad_taqlid_be_t3, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(trad_taqlid_be_t6, quran_hadith_substrate__traditionalist_taqlid, base_extractiveness, 6, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(trad_taqlid_su_t0, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(trad_taqlid_su_t3, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(trad_taqlid_su_t6, quran_hadith_substrate__traditionalist_taqlid, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_hadith_substrate__traditionalist_taqlid, identity_coordination).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, reformist_ijtihad).
narrative_ontology:affects_constraint(quran_hadith_substrate__traditionalist_taqlid, state_hybrid).

% DUAL FORMULATION NOTE:
% The Qur'an-Hadith substrate kernel decomposes into three structurally distinct constraints with different ε values, different victim/beneficiary sets, and different institutional implementations. Traditionalist_taqlid (this story, ε≈0.62) treats classical schools as binding. Reformist_ijtihad (ε≈0.35, Rope/Tangled Rope) treats primary texts as open to reinterpretation. State_hybrid (ε≈0.55, Tangled Rope) treats modern law as authoritative with fiqh as cultural reference. The three readings coexist in contemporary Islamic discourse; the network links show which constraints affect which.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quran_hadith_substrate__traditionalist_taqlid, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
