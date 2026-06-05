% ============================================================================
% CONSTRAINT STORY: quranic_gender_verses__literal_hierarchical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quranic_gender_verses__literal_hierarchical, []).

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
 *   constraint_id: quranic_gender_verses__literal_hierarchical
 *   human_readable: Qur'anic Gender Verses (Literal Hierarchical Reading): Divine Ordinance of Male Guardianship
 *   domain: islamic_jurisprudence/legal_hermeneutics/gender_studies/religious_authority
 *
 * SUMMARY:
 *   Verses Qur'an 4:11 (inheritance allocation), 2:282 (witness testimony),
 *   and 4:34 (al-Qawwamun: male guardians/maintainers) establish, on the
 *   literal hierarchical reading, a divinely ordained differentiation of
 *   rights and authority between men and women. Under this reading, male
 *   household heads hold guardianship authority (wilayah); women's
 *   inheritance is fixed at half a male heir's share; female testimony in
 *   financial contracts is weighted at half that of male testimony. The
 *   literal reading treats these verses as timeless legal ordinances
 *   establishing permanent structural hierarchy. This constraint story
 *   instantiates the literal_hierarchical reading of the contested kernel
 *   quranic_gender_verses. Two sibling readings coexist:
 *   contextual_egalitarian (treating the verses as responsive to 7th-century
 *   contingency and subject to reinterpretation in light of changed
 *   circumstances) and progressive_abrogation (treating later Qur'anic verses
 *   about women's autonomy as implicitly superseding earlier hierarchical
 *   verses). This story generates ONLY the literal hierarchical reading — a
 *   single, ε-invariant constraint capturing how male beneficiaries,
 *   religious courts, and patrilineal inheritance systems experience and
 *   enforce these verses as extractive mechanisms.
 *
 * KEY AGENTS:
 *   - Male Household Heads: Institutional beneficiary (institutional/arbitrage) — gain guardianship authority, double inheritance share, contractual autonomy
 *   - Religious Court Authority: Institutional beneficiary (institutional/arbitrage) — enforce guardianship and inheritance rules; derive legitimacy from literal textual interpretation
 *   - Women Under Guardianship: Primary victim (powerless/trapped) — require guardian approval for marriage, contracts, property disposition; restricted inheritance and testimony weight
 *   - Female Legal-Autonomy Seekers: Secondary victim (moderate/identity_locked) — structurally mobile but identity-fused with religious authority and community belonging; exit requires abandoning Islamic frame
 *   - Modernist Islamic Scholars: Reinterpreter (organized/constrained) — attempt contextual readings; constrained by orthodox authority; visible in diaspora intellectual spaces
 *   - Analytical Observer: Cross-position view (analytical/analytical) — civilizational scope; sees extraction and false-summit dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, 0.68).
domain_priors:suppression_score(quranic_gender_verses__literal_hierarchical, 0.72).
domain_priors:theater_ratio(quranic_gender_verses__literal_hierarchical, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, extractiveness, 0.68).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(quranic_gender_verses__literal_hierarchical, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quranic_gender_verses__literal_hierarchical, snare).
narrative_ontology:human_readable(quranic_gender_verses__literal_hierarchical, "Qur'anic Gender Verses (Literal Hierarchical Reading): Divine Ordinance of Male Guardianship").
narrative_ontology:topic_domain(quranic_gender_verses__literal_hierarchical, "islamic_jurisprudence/legal_hermeneutics/gender_studies/religious_authority").

domain_priors:requires_active_enforcement(quranic_gender_verses__literal_hierarchical).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quranic_gender_verses__literal_hierarchical, 'd967d39e-51c1-4c0a-95d2-e179d878e058').
narrative_ontology:cs_kernel_codification('d967d39e-51c1-4c0a-95d2-e179d878e058', fixed_text).
narrative_ontology:cs_authority_grounding('d967d39e-51c1-4c0a-95d2-e179d878e058', lineage).
narrative_ontology:cs_interpretation_layer_present('d967d39e-51c1-4c0a-95d2-e179d878e058').
narrative_ontology:cs_reading_relation('d967d39e-51c1-4c0a-95d2-e179d878e058', quranic_gender_verses__contextual_egalitarian, coexists_with).
narrative_ontology:cs_reading_relation('d967d39e-51c1-4c0a-95d2-e179d878e058', quranic_gender_verses__progressive_abrogation, coexists_with).
narrative_ontology:cs_axiom('d967d39e-51c1-4c0a-95d2-e179d878e058', foundational, hierarchical_verses_permanently_binding).
narrative_ontology:cs_axiom_status(hierarchical_verses_permanently_binding, holdable).
narrative_ontology:cs_axiom_grounding('d967d39e-51c1-4c0a-95d2-e179d878e058', hierarchical_verses_permanently_binding, conventional).
narrative_ontology:cs_axiom('d967d39e-51c1-4c0a-95d2-e179d878e058', foundational, male_authority_structural_necessity).
narrative_ontology:cs_axiom_status(male_authority_structural_necessity, holdable).
narrative_ontology:cs_axiom_grounding('d967d39e-51c1-4c0a-95d2-e179d878e058', male_authority_structural_necessity, instrumental).
narrative_ontology:cs_reference_frame('d967d39e-51c1-4c0a-95d2-e179d878e058', classical_patriarchal_hierarchy).
narrative_ontology:cs_drift_state('d967d39e-51c1-4c0a-95d2-e179d878e058', contemporary_gender_autonomy_era, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('d967d39e-51c1-4c0a-95d2-e179d878e058', '').
narrative_ontology:cs_kernel_id(quranic_gender_verses__literal_hierarchical, quranic_gender_verses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, male_household_heads).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, religious_court_authority).
narrative_ontology:constraint_beneficiary(quranic_gender_verses__literal_hierarchical, patrilineal_inheritance_beneficiaries).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_under_guardianship).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, daughters_inheritance_claimants).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, female_testimony_bearers).
narrative_ontology:constraint_victim(quranic_gender_verses__literal_hierarchical, women_seeking_autonomous_legal_personhood).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WOMEN UNDER GUARDIANSHIP — Trapped by legal structure (male guardian approval required for marriage, contract, inheritance disposition), religious authority (religious courts enforce guardianship rules), family structure (apostasy or defiance risks family rupture), and financial dependency (reduced inheritance rights, restricted property autonomy). Exit requires abandoning family, community, and religious identity simultaneously. Maximum experienced extraction — no exit options.
constraint_indexing:constraint_classification(quranic_gender_verses__literal_hierarchical, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: WOMEN SEEKING LEGAL AUTONOMY — Structurally mobile (some exit to secular legal systems, professional careers, geographic relocation possible) but identity-locked by internalized legitimacy of religious authority, family honor codes, and community belonging. Exit requires abandoning Islamic identity frame or facing community expulsion. The binding is cognitive/identity-based rather than purely material — the constraint is changeable in principle but unthinkable from within the identity frame.
constraint_indexing:constraint_classification(quranic_gender_verses__literal_hierarchical, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: MALE HOUSEHOLD HEADS — Institutional power holder benefiting from guardianship authority, inheritance advantage (double share vs female relatives), and legal autonomy. Experiences the constraint as coordination mechanism for household decision-making and property management. Arbitrage exit: can switch between religious and secular legal frameworks depending on advantage in each case.
constraint_indexing:constraint_classification(quranic_gender_verses__literal_hierarchical, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: RELIGIOUS COURT AUTHORITY — Institutional beneficiary wielding enforcement power over guardianship rules. Experiences constraint as legitimate coordination mechanism for family law adjudication. Arbitrage option: can defer to secular courts on technical matters while preserving authority over family structure interpretation.
constraint_indexing:constraint_classification(quranic_gender_verses__literal_hierarchical, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: MODERNIST ISLAMIC SCHOLARS — Organized actors attempting to reinterpret the verses within the Islamic framework (contextual reading, historical contingency arguments). Constrained by textual literalism objections and orthodox authority; benefits from legitimacy-building in diaspora contexts. Experiences the constraint as a coordination problem solvable through hermeneutics, but faces suppression from orthodox authorities defending literal reading.
constraint_indexing:constraint_classification(quranic_gender_verses__literal_hierarchical, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — Views the constraint from civilizational scope across multiple legal traditions. Sees the literal hierarchical reading as instantiating a pure extraction mechanism: it concentrates legal autonomy, inheritance rights, and contractual power in male agents while displacing costs to women. The constraint's coherence relies on treating the Qur'anic text as unambiguously hierarchical and enforcement as legitimate — both empirically contested. Chi = 0.68 × f(d_analytical) × σ_global, where f(d) reflects the observer's position outside the constraint's authority structure.
constraint_indexing:constraint_classification(quranic_gender_verses__literal_hierarchical, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quranic_gender_verses__literal_hierarchical_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(quranic_gender_verses__literal_hierarchical, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quranic_gender_verses__literal_hierarchical, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(quranic_gender_verses__literal_hierarchical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quranic_gender_verses__literal_hierarchical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The literal hierarchical reading concentrates legal autonomy, inheritance rights, and contractual authority in male agents. Women are locked into dependent legal status requiring guardian authorization for major life decisions. The measurement shows extraction rising from 0.52 to 0.68 over 1000 years (roughly 7th century CE to present), suggesting that enforcement intensity and resource-capture through inheritance rules have accumulated rather than remaining static. This trajectory reflects both institutional calcification (religious courts formalizing rules that began as jurisprudential guidance) and active enforcement amplification as patriarchal interests weaponized the literal reading against contextual alternatives. Suppression (0.72): High and rising. Structural barriers include legal requirement for guardian approval, reduced inheritance entitlement, and discounted testimony. Internalized suppression emerges through family honor codes, piety narratives that frame obedience as virtue, and epistemic closure regarding alternative interpretations. The measurement shows suppression rising from 0.48 to 0.72, indicating that enforcement machinery has intensified — religious courts now systematize what were once flexible family arrangements, and family structure increasingly relies on formal legal threat rather than informal social coordination. Theater ratio (0.58): Moderate. Some functionality exists (inheritance rules do allocate property, guardianship does coordinate family decisions) but increasing performativity. Religious scholars invest effort in defending the literal reading against contextual objections; the interpretive labor that goes into justifying fixed male authority relative to the actual administrative function of guardianship suggests theater has risen over time.
 *
 * PERSPECTIVAL GAP:
 *   This constraint manifests six distinct classification types depending on the observer's structural position. Women trapped in the system see pure extraction (Snare) with no exit. Women identity-locked by religious framing see the same structural barrier but perceive it as legitimately binding through divine authority — the classification is identical (Snare) but the binding mechanism is cognitive rather than purely material, which has implications for exit trajectories. Male household heads see a coordination mechanism (Rope) — they experience the verses as solving the problem of family decision-making and property continuity. Religious court authorities see legitimate authority exercise (Rope) — the verses grant them interpretive jurisdiction. Modernist scholars attempting reinterpretation see a constraint problem with organizational solutions (Tangled Rope) — the literal reading creates extraction but has a coordination function (family stability) that contextual readings might preserve while reducing asymmetry. The analytical observer at civilizational scope sees pure extraction (Snare), but with a layer of false-summit dynamics: the constraint is presented as divine ordinance (natural law) when its extractive character and historical contingency are identifiable. The perspectival gap is maximal here — from Rope (beneficiary) to Snare (victim) to Mountain-as-false-summit (analytical observer recognizing naturalization).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. Male household heads as beneficiaries with arbitrage exit (can switch between Islamic and secular legal frameworks) have low d (~0.15), producing negative chi contribution — they experience the constraint as subsidizing them. Women trapped as victims have maximum d (~0.95), producing high chi (~1.42× the base extraction). Women identity-locked as victims have d ~0.89, producing chi ~1.28× base extraction — lower than trapped because identity-locked agents maintain some structural mobility even as their identity frame prevents exercising it. This distinction is diagnostically important: at biographical time horizon, an identity_locked agent perceives the constraint as Rope (changeable in principle) while a trapped agent perceives Mountain (unchangeable). The identity lock is a perceptual filter on structural mobility, not immobility itself. Religious courts as institutional beneficiaries with arbitrage exit behave like male household heads (d ~0.15). The modernist scholars as organized agents with constrained exit (they can leave Islam but lose professional legitimacy and community standing) have d ~0.55, producing moderate chi. These directionality values drive the perspectival gap: the beneficiaries' low d makes them experience the constraint as coordination (Rope, chi ≤ 0.35); the trapped victims' high d makes them experience extraction (Snare, chi ≥ 0.66); the identity-locked agents at the boundary show the diagnostic gap between Rope (perceived mutability from within another identity frame) and Snare (perceived immutability from within this identity frame).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The literal hierarchical reading instantiates a coherent extraction mechanism with genuine coordination function. Male guardianship DOES coordinate family decision-making and property allocation — these are real coordination problems in household economies. The extraction problem is not that the verses create coordination (they do), but that they solve coordination by concentrating power asymmetrically and hiding the asymmetry behind claims of divine ordinance. The literal reading achieves coordination THROUGH extraction, not despite it. Modernist reinterpretations argue that the coordination function (family stability, property allocation) could persist without the asymmetric extraction (equal inheritance, equal legal autonomy) — that the coordination and extraction are separable. The literal reading insists they are not — that asymmetric male authority IS the coordination mechanism. This is the fundamental disagreement between literal_hierarchical and contextual_egalitarian readings. The mandatrophy is resolved by recognizing that both the literal reading AND the modernist reading are Tangled Rope candidates in some analytical frames (real coordination + real extraction), but the literal reading ENFORCES the entanglement as theologically mandatory, while the modernist reading seeks to decompose them. The literal reading's Snare classification reflects that enforcement machinery now privileges the extraction benefit (inherited by male-centered institutions) over the coordination function (which could be achieved more efficiently without asymmetry). Theater ratio rising from 0.42 to 0.58 suggests that the interpretive work defending literal hierarchy has grown relative to the actual coordination work performed — more effort spent justifying asymmetry against modernist critiques than spent managing household decision-making. This is consistent with Snare dynamics: when extraction becomes fragile, enforcement intensifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_literal_vs_contextual_ambiguity,
    'Is Verse 4:34 (al-Qawwamun) an unambiguous mandate for male guardianship, or does the text admit contextual interpretation that renders male authority conditional on protective obligation?',
    'Linguistic analysis of Qur''anic grammar and classical tafsir traditions; comparison of variant manuscript readings and lexical etymology of ''qawwamun'' (maintainers/managers vs rulers); historical application patterns in early Islamic jurisprudence',
    'If literal: male guardianship is divinely ordained and cannot be revised without denying scriptural authority. If contextual: male authority is contingent on protective competence and distributional conditions — enabling reinterpretation. This omega gates the core classification binary: Snare (literal reading) vs Tangled Rope (contextual reading with embedded extraction logic).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_literal_vs_contextual_ambiguity, empirical, 'Whether Qur''anic gender verses admit contextual interpretation or mandate literal male hierarchy').

omega_variable(
    divine_ordinance_vs_historical_contingency,
    'Are these verses divine ordinances transcending historical context, or are they historically-rooted rulings that were progressive relative to 7th-century pre-Islamic Arabian norms?',
    'Comparative analysis of women''s legal status in contemporaneous Arabian, Byzantine, and Sassanid legal systems; historical documentation of verse application patterns across Islamic centuries; theological examination of revelation doctrine (does ''timeless'' mean context-independent or eternally applicable within evolving contexts?)',
    'If ordinance: classification holds as Snare with high legitimacy from religious authority. If contingent: the literal reading naturalizes a historical arrangement, triggering false summit dynamics — the constraint is extractive, not divine. This omega gates the false summit detection for this constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_ordinance_vs_historical_contingency, conceptual, 'Whether gender verses are context-transcending divine ordinance or historically-contingent rulings').

omega_variable(
    suppression_mechanism_internalization,
    'What proportion of the measured suppression (0.72) is structural (legal barriers, family economic dependency, enforcement machinery) versus internalized (acceptance of male authority as legitimate, identity fusion with guardian role, epistemic closure regarding alternative readings)?',
    'Post-exit trajectory analysis: women exiting guardianship systems (via emigration, secular legal status, apostasy) show persistence or erosion of suppression patterns; comparative study of women transitioning from high-enforcement to low-enforcement contexts; discourse analysis of legitimacy claims in women''s own accounts of guardianship acceptance',
    'If high internalization: suppression measurement understates binding force — the constraint carries with exiting agents psychologically. If structural: suppression is removable by legal/institutional change without identity reconstruction. This omega informs the identity_locked vs trapped distinction in perspective 2.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Proportion of guardianship suppression that is structural versus internalized').

omega_variable(
    enforcement_machinery_variation,
    'How does enforcement intensity vary across Islamic legal schools and modern jurisdictions claiming Islamic authority? Does variation reveal extraction patterns or legitimate contextual application?',
    'Comparative jurisprudence across Maliki, Hanafi, Shafi''i, Hanbali schools; enforcement statistics from Islamic courts in different modern states; documentation of formal vs informal enforcement patterns (legal requirement vs social sanction)',
    'If variation is high and correlated with political extraction: enforcement is instrumentalized — the literal reading serves state or patriarchal interests, not divine ordinance. If variation tracks principled jurisprudential difference: the literal reading accommodates legitimate diversity. This omega feeds into the network.affects_constraints analysis and the sibling reading relationships.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_machinery_variation, empirical, 'Variation in guardianship enforcement across legal schools and jurisdictions').

omega_variable(
    identity_locked_binding_mechanism,
    'For women identity-locked within the guardianship system (Perspective 2), what is the specific mechanism of identity-fusion: internalized piety (submission is virtue), relational identity (selfhood constituted through family/guardian role), epistemic closure (alternative readings invisible from within the framework), or community belonging (exit = expulsion)?',
    'Qualitative research with women exiting and women remaining in guardianship systems; analysis of discourse patterns in women''s accounts of authority acceptance; psychological assessment of identity shift in migration contexts; theological examination of how ''obedience'' is framed in devotional literature',
    'Identifies which identity layer drives the lock: theological (piety reframe), relational (family identity), epistemic (interpretive closure), or social (belonging threat). Each mechanism suggests different intervention points and different classification implications. Identity-locked agents may reclassify if the identity frame shifts, whereas trapped agents perceive immutability regardless of frame change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_binding_mechanism, empirical, 'Identity-fusion mechanism in women''s acceptance of guardianship constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quranic_gender_verses__literal_hierarchical, 0, 1000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qgv_lit_theater_t0, quranic_gender_verses__literal_hierarchical, theater_ratio, 0, 0.42).
narrative_ontology:measurement(qgv_lit_theater_t500, quranic_gender_verses__literal_hierarchical, theater_ratio, 500, 0.51).
narrative_ontology:measurement(qgv_lit_theater_t1000, quranic_gender_verses__literal_hierarchical, theater_ratio, 1000, 0.58).

% Extraction over time
narrative_ontology:measurement(qgv_lit_extract_t0, quranic_gender_verses__literal_hierarchical, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(qgv_lit_extract_t500, quranic_gender_verses__literal_hierarchical, base_extractiveness, 500, 0.63).
narrative_ontology:measurement(qgv_lit_extract_t1000, quranic_gender_verses__literal_hierarchical, base_extractiveness, 1000, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qgv_lit_suppress_t0, quranic_gender_verses__literal_hierarchical, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(qgv_lit_suppress_t500, quranic_gender_verses__literal_hierarchical, suppression_requirement, 500, 0.61).
narrative_ontology:measurement(qgv_lit_suppress_t1000, quranic_gender_verses__literal_hierarchical, suppression_requirement, 1000, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quranic_gender_verses__literal_hierarchical, resource_allocation).
narrative_ontology:boltzmann_floor_override(quranic_gender_verses__literal_hierarchical, 0.25).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__contextual_egalitarian).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, quranic_gender_verses__progressive_abrogation).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, islamic_inheritance_law_gender_asymmetry).
narrative_ontology:affects_constraint(quranic_gender_verses__literal_hierarchical, shariah_court_guardianship_enforcement).

% DUAL FORMULATION NOTE:
% The literal_hierarchical reading is one of three structurally distinct constraint stories sharing the kernel quranic_gender_verses. Each reading instantiates different epsilon values, beneficiary/victim structures, and classification types because they represent different empirical claims about the Qur'anic text's semantic content and theological status. The literal_hierarchical reading has high extractiveness (0.68) because it concentrates authority and resources; contextual_egalitarian may have lower extractiveness if it distributes authority more symmetrically while preserving coordination function; progressive_abrogation may classify as Rope or Tangled Rope depending on how supersession operates. Each reading is a self-contained constraint story with its own perspectives, omegas, and measurements. They are linked via network.affects_constraints because they compete for legitimacy within Islamic jurisprudence and each reading shapes institutional outcomes for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(quranic_gender_verses__literal_hierarchical, moderate, 0.89).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
