% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__islamic_sovereignty_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia Islamic Sovereignty Reading (1453 Waqf Endowment)
 *   domain: cultural_heritage/religious_authority/state_sovereignty
 *
 * SUMMARY:
 *   Hagia Sophia's legitimacy claim under Islamic sovereignty reading derives
 *   from the 1453 Ottoman conquest and continuous Islamic endowment (waqf),
 *   positioning it as sovereign Islamic worship space under Turkish state
 *   authority. This reading is ONE of three contested interpretations of the
 *   site's fundamental legitimacy. The Islamic sovereignty reading frames the
 *   2020 reclassification from mosque (1453) → museum (1934 Atatürk
 *   secularization) → mosque (2020 AKP reclassification) as restoration of
 *   continuous Islamic legal substrate rather than political choice. The
 *   constraint exhibits a classical committer structure: the same physical
 *   site and historical facts support three incommensurable legitimacy
 *   claims, each grounded in different constitutional principles (Islamic
 *   endowment law, Atatürk secularism, universal heritage doctrine). The 2020
 *   reclassification was operationalized through executive decree validated
 *   by court reversal of 1934 precedent, creating a structural moment where
 *   the authority grounding shifts from lineage (continuous Ottoman/Islamic
 *   legal transmission) to extraction (state demonstration of institutional
 *   control over courts and secular bureaucracy). Base extractiveness (0.52)
 *   reflects moderate political consolidation value, significant ideological
 *   cost to secularist constituency, and moderate suppression (0.58) of
 *   non-Muslim access and UNESCO jurisdiction. Theater ratio (0.65) reflects
 *   that the reclassification broadcasts religious governance while
 *   maintaining tourism revenue via World Heritage status — heritage norms
 *   are invoked selectively (tourism marketing) while being denied
 *   (jurisdiction rejection).
 *
 * KEY AGENTS:
 *   - AKP Political Coalition: Primary institutional beneficiary (institutional/arbitrage) — consolidates state sovereignty demonstration and Islamic constituency alignment; experiences constraint as coordination of power and religious identity
 *   - Turkish Islamic Constituency: Secondary institutional beneficiary (institutional/arbitrage at continental scope) — receives symbolic governance signal; benefits from state alignment with Islamic institutional authority
 *   - Non-Muslim Visitors: Primary victim (powerless/trapped) — structurally excluded from worship space; no legal standing for access restoration; bears cost of symbolic exclusion
 *   - Turkish Secularist Constituency: Secondary victim (moderate/constrained) — constrained by state authority and majority Muslim polity; experiences ideological defeat and constitutional principle erosion; biographical cost to accepting mosque status
 *   - UNESCO/International Heritage Regime: Victim (organized/constrained) — jurisdiction denied; constrained by diplomatic cost and lack of enforcement mechanism; experiences selective invocation of heritage status
 *   - Global Heritage Conservation Norms: Degraded institution (piton perspective) — invoked selectively for tourism while normative force rejected; persists through inertia, not functional verification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.52).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.58).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia Islamic Sovereignty Reading (1453 Waqf Endowment)").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/religious_authority/state_sovereignty").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, '59e1c10a-c395-4418-adb8-a31399de0200').
narrative_ontology:cs_kernel_codification('59e1c10a-c395-4418-adb8-a31399de0200', fixed_text).
narrative_ontology:cs_authority_grounding('59e1c10a-c395-4418-adb8-a31399de0200', extraction).
narrative_ontology:cs_interpretation_layer_present('59e1c10a-c395-4418-adb8-a31399de0200').
narrative_ontology:cs_reading_relation('59e1c10a-c395-4418-adb8-a31399de0200', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_reading_relation('59e1c10a-c395-4418-adb8-a31399de0200', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('59e1c10a-c395-4418-adb8-a31399de0200', foundational, islamic_conquest_vests_permanent_endowment_authority).
narrative_ontology:cs_axiom_status(islamic_conquest_vests_permanent_endowment_authority, holdable).
narrative_ontology:cs_axiom_grounding('59e1c10a-c395-4418-adb8-a31399de0200', islamic_conquest_vests_permanent_endowment_authority, conventional).
narrative_ontology:cs_axiom('59e1c10a-c395-4418-adb8-a31399de0200', foundational, state_sovereignty_overrides_international_heritage_jurisdiction).
narrative_ontology:cs_axiom_status(state_sovereignty_overrides_international_heritage_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('59e1c10a-c395-4418-adb8-a31399de0200', state_sovereignty_overrides_international_heritage_jurisdiction, deontological).
narrative_ontology:cs_reference_frame('59e1c10a-c395-4418-adb8-a31399de0200', ottoman_islamic_endowment_sovereignty).
narrative_ontology:cs_drift_state('59e1c10a-c395-4418-adb8-a31399de0200', contemporary_secular_constitution_reversal, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('59e1c10a-c395-4418-adb8-a31399de0200', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitor_access).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_heritage_regime).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_secularist_constituency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-MUSLIM VISITOR (SNARE) — Structurally excluded from worship space under Islamic sovereignty doctrine. No legal standing to challenge access restrictions or prayer time closures. Bears cost of symbolic exclusion and enforced erasure of pre-1453 Christian history from primary site narrative. Exit from this role requires religious conversion or departure — no constrained-cost option exists.
constraint_indexing:constraint_classification(hagia_sophia_substrate__islamic_sovereignty_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNESCO HERITAGE REGIME (SNARE) — Jurisdiction denied by Turkish executive authority (2020 court reversal of 1934 secularist ruling). UNESCO cannot enforce universal heritage standards or access norms. Constrained by diplomatic cost of enforcement (sanctions, reputation loss) and lack of enforcement mechanism on Turkish soil. Experiences pure extraction of legitimacy — the 1980 World Heritage designation is invoked selectively to advance tourism while jurisdiction is denied when inconvenient.
constraint_indexing:constraint_classification(hagia_sophia_substrate__islamic_sovereignty_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: TURKISH SECULARIST CONSTITUENCY (TANGLED ROPE) — Structurally constrained by state authority and cultural majority position. Benefits from Hagia Sophia's existence as an architectural/historical monument (tourism, cultural identity, secular governance principle of state neutrality on religion). But experiences extraction through ideological defeat: the 2020 reclassification as mosque signals state rejection of Atatürk's 1934 museum designation and the secularist constitutional framework. Constrained exit — accepting mosque status costs ideological coherence and constitutional principle, but alternatives (political protest, emigration) carry significant biographical cost.
constraint_indexing:constraint_classification(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: AKP POLITICAL COALITION (ROPE) — Primary beneficiary. The 2020 reclassification coordinates two genuine functions: (1) delivering symbolic religious governance to Islamic base constituency, (2) consolidating state sovereignty over all institutions (demonstrating executive authority over courts and secular bureaucracy). Experiences the constraint as pure coordination — solves the party's political problem of demonstrating Islamic governance credentials while maintaining state monopoly on institutional control. Low suppression experienced; the constraint amplifies rather than constrains this actor's power.
constraint_indexing:constraint_classification(hagia_sophia_substrate__islamic_sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SUNNI UMMAH / BROADER ISLAMIC CONSTITUENCY (ROPE) — Symbolic beneficiary at continental scale. The reclassification signals Turkish state alignment with Islamic governance principles and provides legitimation for Islamist political movements across the region. Experiences constraint as coordination of religious identity and political sovereignty. The constraint broadcasts state commitment to Islamic institutional authority rather than secular state neutrality, creating alignment with Sunni-majority governance movements elsewhere. Low suppression — this actor benefits from the constraint's enforcement.
constraint_indexing:constraint_classification(hagia_sophia_substrate__islamic_sovereignty_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: GLOBAL HERITAGE CONSERVATION NORMS (PITON) — The international heritage preservation regime (UNESCO, ICOMOS, academic archaeology) once had substantial force over the site (1980 World Heritage designation, 1985 criteria compliance, decades of conservation coordination). The 2020 reclassification degrades this regime without formally abandoning it — the Turkish state continues tourism marketing via World Heritage status while rejecting UNESCO jurisdiction on religious use. Theater ratio high: heritage conservation norms are invoked selectively (tourism branding) while their normative force (universal access, secular curatorial authority) is denied. The constraint persists through institutional inertia and because no actor has sufficient power to enforce alternative norms, not because heritage conservation still functions.
constraint_indexing:constraint_classification(hagia_sophia_substrate__islamic_sovereignty_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / IMMUTABLE SOVEREIGNTY VIEW (MOUNTAIN) — From a civilizational/universal lens, state sovereignty over territory and institutions is presented as an immutable fact: the Turkish state holds exclusive authority over all institutions within its borders; UNESCO and international heritage norms are contracts the state can revoke unilaterally; religious identity claims are grounded in 1453 conquest and continuous waqf endowment (natural law of Islamic property). This perspective naturalizes the constraint as an inevitable expression of state authority. However, the beneficiary/victim structure reveals false summit dynamics: the 'naturalness' of state sovereignty is invoked selectively (enforced against UNESCO, against secularist legal precedent, against non-Muslim access) and would be rejected if applied to actors with greater power.
constraint_indexing:constraint_classification(hagia_sophia_substrate__islamic_sovereignty_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hagia_sophia_substrate__islamic_sovereignty_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hagia_sophia_substrate__islamic_sovereignty_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, TR),
    TR >= 0.70.

:- end_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, measuring political consolidation value of the reclassification for the AKP coalition. The value reflects that the constraint provides genuine political benefit (signaling Islamic governance, consolidating state institutional control) while imposing moderate costs on victims. The measurement trajectory (0.15 → 0.38 → 0.52) tracks the reclassification's intensification from initial court decision to enforcement of access restrictions and prayer scheduling. Suppression (0.58): Moderate-high. Non-Muslim access is structurally prohibited; UNESCO jurisdiction is denied through executive authority; secularist opposition is constrained by state capacity and democratic majoritarian position. However, suppression is not total — internal (Turkish) contestation continues through courts and civil society; international pressure and humanitarian access for some groups persist; the constraint relies on active enforcement rather than total capacity denial. Theater ratio (0.65): Moderate-high. The reclassification broadcasts commitment to Islamic governance (genuine signaling function) while maintaining World Heritage tourism revenue (performative heritage conservation). The constraint's enforcement includes prayer times announced publicly (genuine religious use) alongside continued tourism access during non-prayer windows (performative heritage inclusion). The theater has increased over the measurement interval as the initial restoration work (functional) was succeeded by enforcement of religious use standards (behavioral theater) and UNESCO jurisdiction denial.
 *
 * PERSPECTIVAL GAP:
 *   The Islamic sovereignty reading produces maximum perspectival divergence across the observation set. The primary beneficiary (AKP coalition) experiences pure coordination — solving the political problem of demonstrating Islamic governance while maintaining institutional control. The primary victim (non-Muslim visitors) experiences pure extraction — symbolic exclusion with no exit option and no legal recourse. The secularist constituency experiences Tangled Rope — the same institutional arrangement provides benefits (site preservation, tourism revenue) alongside extraction (ideological defeat, constitutional principle erosion). The piton perspective reveals that heritage conservation norms are invoked opportunistically. The mountain perspective naturalizes state sovereignty as immutable, masking the reading's contingency on specific constitutional choices. The perspectival gaps reveal that the constraint's legitimacy depends entirely on which foundational principle (Islamic endowment, state secularism, universal heritage) is treated as prior — they are logically incompatible within a single institutional framework, yet the Turkish state claims all three simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation traces how each agent's structural position (power level, exit options, beneficiary/victim status) maps to experienced extractiveness. The AKP coalition combines institutional power with arbitrage options (can mobilize state authority, can access international relations alternatives); derives d ≈ 0.15 (beneficiary + high exit capacity → low experienced extraction). Turkish secularists combine moderate power with constrained exit (can protest, publish, emigrate, but all carry biographical cost); derive d ≈ 0.65 (victim + high-cost exit → moderate-high experienced extraction). Non-Muslim visitors combine powerless status with trapped exit (structural prohibition, no legal standing); derive d ≈ 0.95 (victim + zero exit → maximum experienced extraction). UNESCO regime combines organized power with constrained exit (diplomatic pressure possible, but limited enforcement capacity); derives d ≈ 0.55 (victim + moderate-cost exit → moderate extraction despite institutional status). The perspectival gap between institutional beneficiary (d=0.15, rope experience) and powerless victim (d=0.95, snare experience) measures 0.80 in directionality space, one of the largest gaps in the constraint corpus.
 *
 * MANDATROPHY ANALYSIS:
 *   The Islamic sovereignty reading avoids mandatrophy collapse through consistent Tangled Rope classification: it genuinely coordinates religious use and state sovereignty while asymmetrically extracting legitimacy and access from non-Muslim and secularist constituencies. The constraint would collapse into mandatrophy if it were claimed to be pure coordination (rope) — the victim set is real and substantial. It would also collapse if claimed as pure extraction (snare) — the coordination function for beneficiaries is genuine. The reading's stability depends on the claim that these two functions are inseparable: that recognizing Islamic sovereignty necessarily entails restricting non-Muslim access. The omega variable on 'symbolism vs. functional extraction' probes whether the two could be decoupled (maintaining religious use without non-Muslim exclusion) — if they could, the reading reclassifies to rope and loses normative justification for the beneficiary's extraction. The mandatrophy is therefore resolved by the thesis that Islamic sovereignty and non-Muslim access are structurally incompatible (a premise of the reading), not by the mere existence of beneficiaries and victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    waqf_continuity_claim,
    'Does the 1453 Ottoman Islamic endowment (waqf) create continuous religious sovereignty over the site, or is it a historical claim selectively invoked to legitimize a 2020 political reclassification?',
    'Historical analysis of waqf legal status under Ottoman, Republican, and contemporary Turkish law; examination of whether the 1934 museum designation constitutionally terminated waqf claims or merely suspended their institutional expression; comparison with treatment of other post-conquest religious endowments in Turkish law',
    'If waqf continuity is genuine legal substrate: the reclassification is restoration of continuous sovereignty (the reading''s core premise holds). If waqf is invoked retroactively to justify political choice: the constraint becomes pure extraction (snare from all perspectives except beneficiary); extractiveness climbs toward 0.70.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waqf_continuity_claim, empirical, 'Whether 1453 waqf endowment creates continuous legal sovereignty').

omega_variable(
    secular_constitutional_precedent_foreclosure,
    'Does the 1934 Atatürk secularization — enshrining Hagia Sophia as a state museum under constitutional separation of religion and state — legally foreclose the 2020 Islamic reclassification, or was that precedent legitimately overridden by 2018 court reversal?',
    'Constitutional law analysis of 1924 constitution vs. 1982 constitution provisions on state secularism; analysis of whether 2018 administrative court reversal of 1934 ruling had valid legal authority or constituted executive override of settled constitutional principle; comparison with how other states handle constitutional reversals of foundational secular principles',
    'If 1934 rule was legitimately overridden: two reading coexist, each grounded in different constitutional interpretation (the ''coexists_with'' relation holds). If override violated constitutional hierarchy: the Islamic reading forecloses the secular reading within a unified legal framework (the ''forecloses'' relation applies, suggesting they are incompatible rather than coexistent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secular_constitutional_precedent_foreclosure, conceptual, 'Whether 1934 secularization was legitimately overridden or foreclosed').

omega_variable(
    symbolism_vs_functional_extraction,
    'Does the Islamic sovereignty reading function primarily as religious legitimation (coordination benefit to beneficiaries) or as political consolidation mechanism (extraction from secularist and non-Muslim constituencies)?',
    'Measurement of access changes, prayer scheduling enforcement, Muslim prayer volume vs. tourism revenue, political messaging emphasis in AKP discourse, polling on secularist/non-Muslim perception of symbolic exclusion vs. material access loss',
    'If primarily symbolic: constraint is Rope (coordination) from beneficiary perspective and manageable Tangled Rope from victim perspective. If primarily extractive: constraint is Snare (pure extraction) for non-Muslim/secularist victims; extractiveness rises above 0.55 and suppression approaches 0.70.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolism_vs_functional_extraction, empirical, 'Whether constraint is religious coordination or political extraction mechanism').

omega_variable(
    reading_kernel_ambiguity,
    'Is Hagia Sophia''s Islamic legitimacy grounded in the 1453 waqf substrate (a kernel reading), or is the ''substrate'' itself a constructed narrative used to justify 2020 political choice?',
    'Analysis of whether the 1453 endowment claim predates 2020 reclassification debate (genuine historical substrate) or emerged as retroactive justification (constructed narrative). Examination of pre-2020 Islamic legal scholarship claims vs. post-2020 claims.',
    'If substrate is genuine historical kernel: this is a legitimate reading of a contested endowment (committer frame applies; cs_structure is well-motivated). If substrate is constructed narrative: this constraint should be reclassified as ''political_consolidation'' with different beneficiary/victim structure, and the kernel frame is misapplied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether Islamic sovereignty claim derives from genuine historical substrate or constructed narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagia_islamic_tr_t0, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(hagia_islamic_tr_t5, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(hagia_islamic_tr_t10, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(hagia_islamic_be_t0, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(hagia_islamic_be_t5, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(hagia_islamic_be_t10, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hagia_islamic_su_t0, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(hagia_islamic_su_t5, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(hagia_islamic_su_t10, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% The Hagia Sophia substrate kernel generates three constraint stories, each representing one legitimacy reading. The three stories are linked through network.affects_constraints: each sibling reading influences the others by competing for institutional authority and legitimacy validation. The Islamic sovereignty reading became institutionally dominant in 2020 through Turkish executive authority; the Orthodox restitution reading continues as organized diaspora claim; the universal heritage reading persists in international heritage regimes and academic disciplines. The ε values differ substantially across readings because they measure different extraction mechanisms: Islamic sovereignty (0.52) measures political consolidation extraction; Orthodox restitution (estimated 0.65) measures historical justice extraction; Universal heritage (estimated 0.35) measures jurisdictional extraction. The readings are structurally incompatible — no single institutional framework can honor all three simultaneously — but they coexist through geographic/jurisdictional separation: Turkish state authority validates the Islamic reading within Turkish borders; diaspora communities maintain Orthodox claims in dispersed networks; UNESCO maintains universal heritage claims through international instruments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
