% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel_flat_control, []).

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
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jurisprudential_method_kernel_flat_control
 *   human_readable: Islamic Jurisprudential Method (Fiqh) — Quranic Derivation System
 *   domain: islamic_jurisprudence/legal_theory/commitment_systems
 *
 * SUMMARY:
 *   The jurisprudential method (fiqh) for deriving legal rulings from Quranic
 *   and prophetic sources (Sunnah) is formalized in a hierarchical structure:
 *   Quran (foundational), Sunnah (prophetic precedent), ijma (scholarly
 *   consensus), qiyas (analogical reasoning), istihsan (juristic preference),
 *   and maslaha (public interest). This constraint exhibits genuine
 *   coordination function (scholars across regions need a shared method to
 *   produce consistent rulings) and asymmetric extraction (the method
 *   concentrates interpretive authority in trained scholars, excludes lay
 *   input, stabilizes gender hierarchies, and suppresses heterodox
 *   innovation). The jurisprudential method is presented as a natural logical
 *   necessity — if you have fixed texts and new cases, you must develop a
 *   derivation system — but the specific hierarchy, scope conditions, and
 *   application norms reflect historical power settlements among early
 *   scholars and states. The method stabilizes patriarchal family law as a
 *   fundamental principle, encodes male scholarly authority as epistemically
 *   necessary, and uses the apparent rigor of methodological citation to
 *   naturalize political outcomes. Rising theater_ratio over the interval
 *   (0.35 → 0.62) reflects the increasing gap between the method's
 *   performative citation and the actual mechanisms driving contemporary
 *   jurisprudential conclusions.
 *
 * KEY AGENTS:
 *   - Lay Believers: Powerless/trapped — depend entirely on scholars' interpretations for religious validity; cannot verify derivations; bear costs of restrictions (prayer times, marriage rules, inheritance allocations) without input
 *   - Women Under Guardianship: Powerless/identity_locked — trapped by jurisprudential rulings that structure family law (wali, wilayah, inheritance asymmetry); identity fused with the framework (the method teaches them who they are); structurally mobile in some contexts but cannot exercise mobility from within identity frame
 *   - Heterodox Interpreters and Reform Movements: Organized/constrained — scholars and movements (Ahl-e-Hadith, progressive Islamic thinkers, Ijtihad revival) who use the method to contest orthodox conclusions; constrained by institutional gatekeeping and accusations of illegitimacy
 *   - Four Orthodox Schools and Network: Institutional/arbitrage — beneficiaries of the established method; their authority is legitimated by alignment with recognized jurisprudential structure; can switch between schools (arbitrage) without loss of standing
 *   - State Authority and Formal Legal Systems: Institutional/constrained — states that incorporate Islamic law formally benefit from jurisprudential legitimacy but are constrained by method requirements; cannot arbitrarily reinterpret without appearing to violate the framework
 *   - Scholarly Apparatus (Performative): Institutional/arbitrage — the ritualized citation and methodological performance that masks the shift of actual derivation to political/cultural preference
 *   - Analytical Observer: Analytical/analytical — sees the method as emerging naturally from the logical problem of applying fixed sources to unforeseen cases; risks naturalizing what is actually a constructed, power-laden settlement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel_flat_control, 0.35).
domain_priors:suppression_score(jurisprudential_method_kernel_flat_control, 0.48).
domain_priors:theater_ratio(jurisprudential_method_kernel_flat_control, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel_flat_control, extractiveness, 0.35).
narrative_ontology:constraint_metric(jurisprudential_method_kernel_flat_control, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(jurisprudential_method_kernel_flat_control, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel_flat_control, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel_flat_control, "Islamic Jurisprudential Method (Fiqh) — Quranic Derivation System").
narrative_ontology:topic_domain(jurisprudential_method_kernel_flat_control, "islamic_jurisprudence/legal_theory/commitment_systems").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(jurisprudential_method_kernel_flat_control, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel_flat_control, juridical_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel_flat_control, institutional_authority_holders).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel_flat_control, orthodox_schools).
narrative_ontology:constraint_victim(jurisprudential_method_kernel_flat_control, heterodox_interpreters).
narrative_ontology:constraint_victim(jurisprudential_method_kernel_flat_control, lay_believers).
narrative_ontology:constraint_victim(jurisprudential_method_kernel_flat_control, women_under_male_guardianship).
narrative_ontology:constraint_victim(jurisprudential_method_kernel_flat_control, jurisprudential_innovation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LAY BELIEVER (SNARE) — Structurally dependent on scholars' derivations for religious legitimacy; cannot verify the chains of reasoning themselves; bears the costs of rulings (compliance, restriction, reinterpretation) without meaningful input into method design. Trapped by epistemic asymmetry and religious authority.
constraint_indexing:constraint_classification(jurisprudential_method_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: WOMEN UNDER GUARDIANSHIP (SNARE + IDENTITY_LOCKED) — Trapped by jurisprudential rulings that encode male guardianship as a core principle (wali, wilayah). Identity locked because the jurisprudential framework constitutes their religious identity (the framework itself teaches them who they are); structurally mobile in some contexts (economic participation, marriage consent) but cannot exercise mobility within their identity frame. Extraction runs through the method's stabilization of asymmetric family structures.
constraint_indexing:constraint_classification(jurisprudential_method_kernel_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: HETERODOX INTERPRETERS (TANGLED ROPE) — Organized agents (Ahl-e-Hadith reformers, Salafi movements, progressive Islamic scholars, Ijtihad revival movements) who engage with the jurisprudential method to contest its conclusions while respecting its structure. Constrained by institutional pressure, scholarly establishment resistance, and accusations of illegitimacy. Receive benefits from the method's infrastructure (it is a shared epistemic framework) while bearing costs of non-recognition. Active enforcement required to maintain orthodox authority over alternative readings.
constraint_indexing:constraint_classification(jurisprudential_method_kernel_flat_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ORTHODOX SCHOOLS (ROPE) — Institutional beneficiaries of the established jurisprudential method. The four Sunni schools (Hanafi, Maliki, Shafi'i, Hanbali) and their scholarly networks capture epistemic authority and social legitimacy through the method. See the system as genuine coordination: the method enables binding jurisprudential consensus across different cultural contexts and regions. Net beneficiaries with arbitrage options (alternative methods exist but carrying institutional risk). The method legitimates their authority.
constraint_indexing:constraint_classification(jurisprudential_method_kernel_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: STATE AUTHORITY (TANGLED ROPE) — Nation-states that incorporate Islamic law formally (constitutional recognition, Shari'ah courts, national legal codes) benefit from the jurisprudential method's legitimizing power (derives authority from Quranic sources) but are constrained by its requirements (must maintain appearance of alignment with established method, cannot arbitrarily reinterpret). Active enforcement required to maintain state authority over religious interpretation. Coordination function: legitimizes state law; extraction function: state gains religious legitimacy while restricting heterodox interpretation.
constraint_indexing:constraint_classification(jurisprudential_method_kernel_flat_control, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SCHOLARLY APPARATUS (PITON) — The actual jurisprudential method (citation chains, hadith verification, qiyas application) has increasingly atrophied as a live epistemic practice, replaced by rehearsal of established positions. Contemporary scholarship often performs adherence to the method while deriving conclusions from political/cultural preference and then retrofitting the methodological justification. High theater ratio: the ritual of citing sources and structuring arguments according to method persists, but the functional derivation has moved elsewhere. Maintained through institutional inertia and traditionalist gatekeeping.
constraint_indexing:constraint_classification(jurisprudential_method_kernel_flat_control, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, the jurisprudential method appears as an inevitable logical structure: any religious tradition with fixed foundational texts (Quran) and recorded prophetic practices (Sunnah) must develop a systematic method to derive applicable guidance for cases not explicitly addressed in the sources. The method itself (the hierarchy: Quran > Sunnah > consensus > analogy) emerges as a natural consequence of epistemic closure (no new prophecy). However, the structural data contradict pure naturalness: the method is socially stabilized, benefits specific agents, extracts from others, and requires enforcement. Engine will compute this as a false summit.
constraint_indexing:constraint_classification(jurisprudential_method_kernel_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(jurisprudential_method_kernel_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(jurisprudential_method_kernel_flat_control, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(jurisprudential_method_kernel_flat_control, TR),
    TR >= 0.70.

:- end_tests(jurisprudential_method_kernel_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The jurisprudential method does solve a genuine coordination problem — regions with different cultural contexts need a shared framework for deriving applicable rulings. However, the method's structure concentrates authority (in scholars, in men, in orthodox schools) and suppresses innovation, generating asymmetric benefit. The value reflects genuine coordination function alongside structural extraction. Suppression (0.48): Moderate-high. Multiple suppression mechanisms: epistemic gatekeeping (only trained scholars can perform ijtihad), institutional enforcement (state and religious hierarchies enforce orthodox rulings), and identity-based suppression (women are taught their subordination is Quranic mandate). But suppression is incomplete — heterodox scholars exist, lay believers sometimes innovate, women exercise agency despite constraints. Theater ratio (0.62, endpoint): Moderate-high. Contemporary jurisprudential scholarship frequently performs methodological rigor (chains of citation, source hierarchies) while deriving conclusions from political preference and retroactively justifying them methodologically. Classical jurisprudence (t=0, theater=0.35) involved more genuine derivation; modern jurisprudence increasingly rehearses established positions while citing method. The temporal increase reflects institutional degradation of the epistemic function.
 *
 * PERSPECTIVAL GAP:
 *   The jurisprudential method exhibits maximum perspectival variance. Institutional beneficiaries (orthodox schools, state authority) see rope — a genuine coordination mechanism that enables binding rulings across regions. Heterodox interpreters see tangled_rope — they use the same method but are suppressed by those who control the authority to declare what counts as valid interpretation. The lay believer sees snare — they depend entirely on scholars' derivations, cannot verify them, and bear the extraction costs. Women see snare + identity_locked — trapped both structurally (male guardianship rules) and psychologically (the framework constitutes their religious identity). The scholarly apparatus sees itself as piton — performing methodological rigor while actual derivations move elsewhere. The analytical observer risks mountain — seeing the method as a natural logical necessity — but the structural data (beneficiaries, victims, enforcement, suppression) reveal this as false summit. The perspectival gap reflects that no single type is correct; the presheaf over observer positions is the constraint's actual structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) range from 0.0 (full beneficiary) to 1.0 (full target). Orthodox schools: d ≈ 0.15 (institutional, arbitrage options, beneficiary status) — low extractiveness experienced, high coordination benefit. Heterodox interpreters: d ≈ 0.60 (organized, constrained, victim+beneficiary mix) — constrained by enforcement but engaged in the method. Lay believers: d ≈ 0.90 (powerless, trapped, no beneficiary status, full victim) — maximum experienced extraction. Women under guardianship: d ≈ 0.85 (powerless, identity_locked, full victim) — similarly maximal, with additional psychological binding. State authority: d ≈ 0.55 (institutional, constrained, mixed beneficiary/victim) — gains legitimacy but loses interpretive freedom. These d values flow through the engine's sigmoid to produce χ (effective extraction), which varies across perspectives. The heterodox perspective's tangled_rope classification reflects moderate d and mixed coordination/extraction benefits. The lay believer's snare reflects high d and high victim status with no escape mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The jurisprudential method exhibits no resolved mandatrophy — the method's founding problem (derive applicable law from fixed sources in new contexts) remains live. The method continues to solve this coordination problem, which is why it persists across 1400+ years. However, omega variables name genuine uncertainties about whether the method's current operation still matches its founding rationale: the rising theater_ratio suggests the functional derivation has partially decoupled from the methodological performance. The mandatrophy is not resolved but deepens over time as the gap between method-as-performed and method-as-lived widening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_fixity_vs_interpretive_multiplicity,
    'Is the jurisprudential method a system for deriving a single correct answer from fixed sources, or a framework legitimizing a multiplicity of interpretations all equally grounded in those sources?',
    'Historical analysis of interpretive variance across orthodox schools for the same case. Examine whether schools'' disagreements represent genuine alternative derivations or failures to find the true method. Compare scholarly writings on whether multiple positions can be simultaneously valid.',
    'If single answer: method''s extractiveness increases (authority is concentrated in whoever determines the derivation); victims become those whose interpretation is rejected as illegitimate. If multiple valid answers: extractiveness decreases (authority is distributed); hierarchy among schools dissolves.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_fixity_vs_interpretive_multiplicity, conceptual, 'Whether the method derives a single correct answer or legitimizes multiple interpretations').

omega_variable(
    qiyas_epistemic_status,
    'Does qiyas (analogical reasoning) constitute a valid epistemic method for deriving law from Quranic principles, or is it a pragmatic extrapolation that naturalizes contingent preferences?',
    'Comparative analysis of qiyas chains: do they consistently apply stated principles, or do different scholars apply qiyas selectively to reach predetermined conclusions? Examine cases where qiyas produces conclusions that contradict literal text, and track whether such cases are rejected or accommodated.',
    'If valid epistemic method: qiyas preserves the coordination function; method remains legitimate. If pragmatic extrapolation: scholars'' authority rests on rhetorical skill in chain construction, not on methodological rigor; extractiveness increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qiyas_epistemic_status, empirical, 'Whether analogical reasoning is a valid epistemic method or rhetorical practice').

omega_variable(
    ijma_formation_mechanism,
    'How is scholarly consensus (ijma) actually formed and recognized? Is it a genuine emergent property of distributed scholarly agreement, or a constructed authority claim by dominant schools?',
    'Historical case study: track the formation process for claimed instances of ijma (e.g., forbidden times for prayer, inheritance ratios). Identify whose agreement counted, whose dissent was noted or ignored, what communication mechanisms existed. Compare medieval vs modern ijma claims.',
    'If emergent property: ijma is genuine coordination mechanism; tangled_rope classification holds. If constructed: ijma is a rhetorical move that naturalizes power concentration; extractiveness and suppression increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ijma_formation_mechanism, empirical, 'Whether ijma is emergent scholarly consensus or constructed authority claim').

omega_variable(
    gender_asymmetry_necessity,
    'Are the asymmetric rulings on gender and guardianship (wali, wilayah, inheritance, divorce rights) derived from the jurisprudential method applied to the sources, or is the method designed backward from predetermined gender hierarchy?',
    'Textual analysis: examine Quranic verses on women''s agency (e.g., Khadijah''s business ownership, Ayesha''s scholarship, women''s consent in marriage). Compare those verses'' principles with jurisprudential conclusions. Identify where the method selects hierarchical interpretations over egalitarian ones. Track which interpretive choices are defended as methodologically necessary vs culturally contingent.',
    'If derived: the method generates extractive conclusions but these are structurally legitimate within Islamic jurisprudence. If predetermined: the method is a tool for naturalizing patriarchal extraction; women''s victim classification is confirmed. Either way, perspective 2 (identity_locked) is clarified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gender_asymmetry_necessity, conceptual, 'Whether gender asymmetry follows from the method or precedes it').

omega_variable(
    maslaha_scope_ambiguity,
    'What is the scope of maslaha (public interest / welfare considerations) in jurisprudential derivation? Can it override explicit textual rulings, or only fill gaps in the sources?',
    'Comparative jurisprudence: identify cases where Maliki and Hanbali scholars (who recognize maslaha more broadly) reach different conclusions from Shafi''i and Hanafi scholars on the same matter. Examine whether maslaha is invoked consistently or selectively. Track how modern scholars handle conflicts between classical jurisprudential conclusions and contemporary welfare (e.g., interest-free banking, women''s participation).',
    'If maslaha is unconstrained: the method becomes elastic, permitting scholars to reinterpret law to match contemporary preference; extractiveness decreases, coordination flexibility increases. If tightly constrained: the method is rigid, blocking innovation; extractiveness persists through resistance to reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maslaha_scope_ambiguity, empirical, 'Scope and constraints on maslaha as a derivation principle').

omega_variable(
    scholarly_gatekeeping_mechanism,
    'What mechanism determines who is recognized as a qualified jurist (faqih) with authority to perform ijtihad (independent reasoning)? Is qualification based on demonstrable methodological mastery, or on institutional position and social recognition?',
    'Historical and contemporary case analysis: examine how new scholars gain or fail to gain recognition. Track whether recognized scholars'' derivations are scrutinized using the same standards as unrecognized scholars'' derivations. Identify whether heterodox scholars are rejected on methodological grounds or on authority/position grounds.',
    'If qualification is rigorous methodological mastery: gatekeeping is legitimate (coordinates expertise). If institutional position determines recognition: gatekeeping is extractive (concentrates authority without epistemic justification). Affects both powerless and heterodox perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholarly_gatekeeping_mechanism, empirical, 'Mechanism for recognizing qualified jurists and access to ijtihad').

omega_variable(
    historical_contingency_of_hierarchy,
    'Is the established hierarchy of sources (Quran > Sunnah > ijma > qiyas > istihsan) natural to the jurisprudential task, or a historical construction that reflects the politics of the early Islamic state and later institutional settlements?',
    'Historical analysis of how the hierarchy solidified over time. Examine whether alternative hierarchies were seriously debated or proposed. Track whether the hierarchy''s justifications are internal-epistemological or historical-sociological. Compare across different legal traditions (Islamic vs common law vs civil law) to see whether the hierarchy is universal or culturally specific.',
    'If natural: method''s legitimacy is grounded in epistemic structure; mountain aspects are confirmed (with beneficiaries, making it a false summit). If historical construction: the hierarchy reflects and perpetuates power arrangements among early scholars and states; extractiveness is structural, not incidental.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_contingency_of_hierarchy, conceptual, 'Whether the source hierarchy is natural or historically contingent').

omega_variable(
    lay_versus_scholarly_religious_authority,
    'In the jurisprudential system, is lay religious understanding and practice treated as a valid epistemic input to derive law, or is the method structured to exclude lay voices and reserve authority to trained scholars?',
    'Examine the method''s own treatment of lay believers'' practices (custom, urf). Does the method systematize these as data to derive from, or do scholars cite them only when they support conclusions reached by other means? Compare classical and modern jurisprudence on whether lay consensus has probative force.',
    'If lay voices are included as valid input: extractiveness decreases (power is more distributed). If excluded: extractiveness increases (powerless perspective is confirmed as snared). Affects powerless and heterodox victim classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_versus_scholarly_religious_authority, empirical, 'Whether lay religious understanding is a valid input to the jurisprudential method').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel_flat_control, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jurip_tr_t0, jurisprudential_method_kernel_flat_control, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jurip_tr_t200, jurisprudential_method_kernel_flat_control, theater_ratio, 200, 0.48).
narrative_ontology:measurement(jurip_tr_t400, jurisprudential_method_kernel_flat_control, theater_ratio, 400, 0.58).
narrative_ontology:measurement(jurip_tr_t600, jurisprudential_method_kernel_flat_control, theater_ratio, 600, 0.62).

% Extraction over time
narrative_ontology:measurement(jurip_be_t0, jurisprudential_method_kernel_flat_control, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(jurip_be_t200, jurisprudential_method_kernel_flat_control, base_extractiveness, 200, 0.31).
narrative_ontology:measurement(jurip_be_t400, jurisprudential_method_kernel_flat_control, base_extractiveness, 400, 0.35).
narrative_ontology:measurement(jurip_be_t600, jurisprudential_method_kernel_flat_control, base_extractiveness, 600, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(jurip_su_t0, jurisprudential_method_kernel_flat_control, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(jurip_su_t200, jurisprudential_method_kernel_flat_control, suppression_requirement, 200, 0.45).
narrative_ontology:measurement(jurip_su_t400, jurisprudential_method_kernel_flat_control, suppression_requirement, 400, 0.48).
narrative_ontology:measurement(jurip_su_t600, jurisprudential_method_kernel_flat_control, suppression_requirement, 600, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel_flat_control, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel_flat_control, islamic_family_law_wali_system).
narrative_ontology:affects_constraint(jurisprudential_method_kernel_flat_control, hadith_verification_chain).
narrative_ontology:affects_constraint(jurisprudential_method_kernel_flat_control, orthodox_school_legitimacy).
narrative_ontology:affects_constraint(jurisprudential_method_kernel_flat_control, ijtihad_restriction_mechanism).

% DUAL FORMULATION NOTE:
% The jurisprudential method is a unified commitment system with multiple decomposable constraints downstream. The family law system (wali, guardianship) is one specific output of the method applied to sources about women's agency. The hadith verification chain is the specific epistemic gatekeeping mechanism within the method. Orthodox school legitimacy depends on alignment with the method. Ijtihad restriction is the enforcement mechanism that maintains the method's boundary. Each downstream constraint has its own extractiveness value reflecting its specific mechanisms; the upstream jurisprudential method has its own reflecting the general coordination function and the systematic extraction encoded in the hierarchy itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jurisprudential_method_kernel_flat_control, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
