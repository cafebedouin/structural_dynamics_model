% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Territorial Sovereignty Legitimacy: Covenant-Continuity Reading
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint models ONE READING of a contested kernel: territorial
 *   sovereignty legitimacy in the context of Israeli-Palestinian territorial
 *   claims. The kernel itself is the question 'On what basis is territorial
 *   sovereignty legitimated in this territory?' Different readings answer
 *   this question differently and invoke different legitimacy sources,
 *   temporal scopes, and historical narratives. This constraint instantiates
 *   the COVENANT-CONTINUITY READING: legitimacy is grounded in ancient divine
 *   covenant combined with continuous Jewish identity and presence (including
 *   diaspora connection and return), integrated with modern international
 *   recognition (Balfour Declaration, UN Partition Plan, 1948 statehood). The
 *   reading's core structural claim is that covenantal title survives
 *   demographic absence and that partition represents a compromise of a
 *   pre-existing right rather than creation of a new one. This reading
 *   coexists with the SELF-DETERMINATION READING (which grounds legitimacy in
 *   modern self-determination principle applied to Arab populations with
 *   demographic majority and continuous modern residence) and the
 *   EXISTENTIAL-MATRIX READING (which frames sovereignty legitimacy as
 *   existential rather than juridical — a precondition for group survival
 *   regardless of historical or legal arguments). The covenant-continuity
 *   reading exhibits tangled-rope structure: it coordinates the mobilization
 *   of diaspora identity around territorial statehood (genuine coordination
 *   function) while simultaneously extracting legitimacy from Palestinian and
 *   Arab self-determination claims by subordinating them to pre-existing
 *   covenantal title (asymmetric extraction function). The reading requires
 *   active enforcement through territorial control, military capacity, and
 *   delegitimization of competing claims. Its theater ratio (0.55) reflects
 *   moderate reliance on narrative and identity framing — the covenantal
 *   grounding is not empirically validated as historical fact but is
 *   sustained through religious tradition, nationalist identity fusion, and
 *   institutional commitment.
 *
 * KEY AGENTS:
 *   - Jewish political sovereignty claim and covenantal identity constituency (institutional/arbitrage): Primary beneficiary — the reading grants them pre-existing covenantal legitimacy that survives demographic absence and subordinates competing claims
 *   - Palestinian territorial claims and Arab self-determination applicants (powerless/trapped and moderate/constrained): Primary victims — the reading's legitimacy hierarchy delegitimizes their territorial claims and frames their presence as secondary to covenantal title
 *   - International recognition institutions (UN, Balfour signatories) (institutional/arbitrage): Secondary beneficiary — the reading treats their recognition acts as legitimating events; they coordinate around the partition framework
 *   - Religious-historical scholarship tradition (organized/constrained): Sustains the reading through deontological axioms and narrative interpretation; largely performative in contemporary law
 *   - Two-state solution architects (powerful/mobile): Attempt to work around the reading by proposing alternative legitimacy frameworks (mutual recognition, borders based on 1967, land-for-peace) with aspirational sunset
 *   - Analytical observer (analytical/analytical): Risks naturalizing the reading's particular framing as a universal legitimacy structure inherent to sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.58).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Territorial Sovereignty Legitimacy: Covenant-Continuity Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '9303c414-50d4-44ef-a387-33bd1dfab7e4').
narrative_ontology:cs_kernel_codification('9303c414-50d4-44ef-a387-33bd1dfab7e4', fixed_text).
narrative_ontology:cs_authority_grounding('9303c414-50d4-44ef-a387-33bd1dfab7e4', lineage).
narrative_ontology:cs_interpretation_layer_present('9303c414-50d4-44ef-a387-33bd1dfab7e4').
narrative_ontology:cs_reading_relation('9303c414-50d4-44ef-a387-33bd1dfab7e4', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_reading_relation('9303c414-50d4-44ef-a387-33bd1dfab7e4', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('9303c414-50d4-44ef-a387-33bd1dfab7e4', foundational, covenantal_title_survives_demographic_absence).
narrative_ontology:cs_axiom_status(covenantal_title_survives_demographic_absence, holdable).
narrative_ontology:cs_axiom_grounding('9303c414-50d4-44ef-a387-33bd1dfab7e4', covenantal_title_survives_demographic_absence, deontological).
narrative_ontology:cs_axiom('9303c414-50d4-44ef-a387-33bd1dfab7e4', foundational, international_recognition_confirms_covenantal_right).
narrative_ontology:cs_axiom_status(international_recognition_confirms_covenantal_right, holdable).
narrative_ontology:cs_axiom_grounding('9303c414-50d4-44ef-a387-33bd1dfab7e4', international_recognition_confirms_covenantal_right, deontological).
narrative_ontology:cs_reference_frame('9303c414-50d4-44ef-a387-33bd1dfab7e4', covenantal_territorial_right_antecedent_to_modern_states).
narrative_ontology:cs_drift_state('9303c414-50d4-44ef-a387-33bd1dfab7e4', contemporary_post_partition_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9303c414-50d4-44ef-a387-33bd1dfab7e4', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_political_sovereignty_claim).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, covenantal_identity_constituency).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_territorial_claims).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, arab_self_determination_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN POPULATIONS (SNARE) — Trapped within a territorial partition framework that declares them the primary victims but denies them the legitimacy framework that the covenant-continuity reading grants to the Jewish claim. Cannot exit the territorial constraint; bears full cost of the partition settlement. The covenant-continuity reading's legitimacy framing actively delegitimizes their own self-determination claim by subordinating modern demographic and residence-based arguments to pre-existing covenantal title. Maximum experienced extraction and suppression.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__covenant_continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ARAB STATE SYSTEM (TANGLED ROPE) — Constrained by international law (UN Partition Plan, 1948 borders) but also benefits from the treaty system and regional coordination structures. The covenant-continuity reading constrains Arab state self-determination claims by anchoring Jewish legitimacy in pre-modern covenantal right, but also enables certain state-to-state agreements and recognition frameworks. Mixed extraction and coordination: constrained by the legitimacy hierarchy but organized at the state level.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERNATIONAL RECOGNITION INSTITUTIONS (ROPE) — Institutional actors (UN, Britain, other Balfour signatories) experience the covenant-continuity reading as coordination: formalizing the Jewish territorial claim through partition and international recognition solves a multiparty coordination problem around territorial allocation. Net beneficiary position — the reading treats their recognition acts as legitimating events, not as arbitrary impositions. Low experienced extraction because the reading aligns institutional interest with legitimacy justification.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__covenant_continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: JEWISH POLITICAL SOVEREIGNTY CLAIMANT (TANGLED ROPE) — The covenant-continuity reading frames this agent as the primary beneficiary of legitimacy (covenantal title survives demographic absence; partition is compromise rather than creation), but also constrains it via enforcement requirements (must maintain settlements, demonstrate continuous presence, defend borders against competing claims). Both benefits from the legitimacy framing and bears the cost of active enforcement and suppression of competing claims. The reading creates a coordination function (mobilizing diaspora, justifying state institutions) alongside extraction (requiring continuous enforcement against Palestinians).
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: RELIGIOUS-HISTORICAL SCHOLARSHIP TRADITION (PITON) — The covenant-continuity reading relies on a deontological grounding (ancient divine covenant) that is largely performative in contemporary international law discourse. Modern scholarship emphasizes the textual, redactional, and historically contingent nature of biblical covenantal claims; the reading's invocation of covenant as legitimating authority persists through institutional inertia (religious community investment, nationalist identity fusion) rather than through epistemically robust claims about historical fact. Theater ratio reflects that the reading sustains itself through narrative and identity commitment, not through empirically defended historical reconstruction.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__covenant_continuity_reading, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TWO-STATE SOLUTION ARCHITECTS (SCAFFOLD) — Powerful actors proposing alternative legitimacy frameworks (land-for-peace, two-state partition based on 1967 borders, mutual recognition) see the covenant-continuity reading as a constraint to be worked around, not resolved. The legitimacy framework has a potential sunset: if mutual recognition and border agreement are formalized, the covenant-continuity reading's enforcement necessity decreases (territorial control is no longer contested at the same intensity). However, no formal sunset clause currently exists; this is an aspirational rather than structural scaffold.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__covenant_continuity_reading, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some form of group territorial claim is inherent to political sovereignty; every nation-state claim invokes some legitimacy narrative (conquest, settlement, self-determination, covenantal right). The covenant-continuity reading appears as one instantiation of a universal legal structure: the need to ground territorial authority in a pre-political legitimacy source. However, the structural data reveals this as a false summit: the reading is not a natural law but a highly contingent committer-specific framing (covenantal interpretation, demographic continuity claim, integration of biblical timeline into modern law). The appearance of universality masks a particular historical reading.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__covenant_continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__covenant_continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__covenant_continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, TR),
    TR >= 0.70.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading's primary extractive function is the subordination of Palestinian territorial claims and Arab self-determination arguments to covenantal title. This is a real asymmetric extraction: one group's legitimacy claim is preempted by another's temporal priority claim. However, extractiveness is not maximal because the reading also incorporates coordination mechanisms (international recognition, diaspora mobilization) and coexists with acknowledged competing legitimacy frameworks rather than claiming to monopolize truth. The modern international recognition components (Balfour, UN, 1948) have genuine coordination function — they solve multiparty allocation problems. Suppression (0.68): High but not maximal. The reading requires substantial suppression of competing claims: Palestinian self-determination arguments, Arab state territorial claims, and alternative historical narratives are actively delegitimized. The suppression mechanism is primarily epistemic (treating covenantal title as binding over self-determination) and institutional (international recognition of Jewish statehood) rather than purely coercive, though coercive suppression (military control, settlement enforcement) is required to maintain the territorial condition the reading presupposes. Theater ratio (0.55): Moderate. The reading relies significantly on narrative and religious-historical interpretation — covenantal claims are not empirically validated historical reconstructions but theological-legal arguments sustained through tradition and identity commitment. However, the reading also incorporates modern legal forms (Balfour Declaration, UN Partition Plan, 1948 Declaration) that have documentary and institutional grounding. The balance between narrative/theological grounding and institutional/legal grounding produces a moderate theater ratio. The increase over the interval (0.48 → 0.55) reflects growing reliance on identity and narrative framing as historical and empirical justifications have become more contested.
 *
 * PERSPECTIVAL GAP:
 *   The covenant-continuity reading produces stark perspectival variation across contexts. For Palestinian populations trapped within the territorial framework the reading establishes, it appears as a snare: they are denied the legitimacy framework (covenantal priority) that determines territorial allocation while simultaneously being partitioned away from territory the reading treats as already-covenanted. For Arab states, it appears as tangled rope: they are constrained by the international recognition framework (UN, Balfour) that legitimates Jewish statehood, but also benefit from state-to-state treaty structures and regional coordination. For international recognition institutions, it appears as rope: the reading aligns their institutional role (allocating territory, recognizing statehood) with legitimacy justification (formal recognition confirms covenantal claim). For the Jewish sovereignty claimant, it appears as tangled rope: they benefit from the legitimacy hierarchy but are constrained by active enforcement requirements and the burden of sustaining suppression against competing claims. For scholars working with empirical historical methods, it appears as piton: covenantal grounding is largely performative in academic discourse, sustained through religious and nationalist identity rather than epistemically robust historical reconstruction. For two-state solution architects, it appears as scaffold: the reading is treated as a temporary constraint that can be worked around through alternative legitimacy frameworks (mutual recognition, 1967 borders) with potential sunset. For the analytical observer at civilizational scope, it risks appearing as mountain (an inherent feature of sovereignty claims) — but the structural data reveals this as a false summit. The covenant-continuity reading is not a universal law but a particular historical reading that benefits identifiable agents (Jewish political claim, certain institutional actors) while suppressing others (Palestinians, Arab states). The perspectival gap reveals that the appearance of universality (all sovereignty claims need legitimacy) masks particular choices (which legitimacy source, which temporal scope, which competing claims are delegitimized).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by each agent's structural relationship to the reading's legitimacy and enforcement mechanisms. The Jewish sovereignty claimant is a net beneficiary (d ≈ 0.20): the reading grants them covenantal priority, treats international recognition as confirmation rather than creation, and frames settlements as return. International institutions also benefit from the reading's coordination function (d ≈ 0.15): their recognition acts become legitimating rather than arbitrary. Palestinian populations bear maximum extraction (d ≈ 0.95): they are trapped without alternative exit options, their territorial claims are delegitimized by the reading, and they face active suppression through enforcement of the covenant-continuity framework. Arab states occupy a middle position (d ≈ 0.55): constrained by international recognition but also benefiting from state-level coordination and regional structures. These directionality values drive the experienced extractiveness (chi) through the sigmoid function: beneficiaries experience low chi (the reading seems fair, coordinating), victims experience high chi (the reading appears as pure extraction and suppression). The piton perspective's directionality reflects that scholars and religious traditions sustain the reading through performative means rather than through direct material extraction or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The covenant-continuity reading resolves potential mandatrophy (classification confusion) by clarifying that tangled-rope classification reflects the reading's STRUCTURE, not a judgment about its legitimacy or justification. The reading is tangled rope because: (1) it has a genuine coordination function (mobilizing diaspora, formalizing international recognition), and (2) it simultaneously extracts legitimacy from competing claims (subordinating self-determination to covenantal title, framing Palestinian presence as secondary). Both functions are real. The mandatrophy is not 'is this extraction or coordination?' but 'for whom and in what direction?' For the beneficiary (Jewish sovereignty claim), the reading appears as pure coordination (rope) — they see the legitimate problem of formalizing statehood being solved. For the victim (Palestinian claims), the reading appears as pure extraction (snare) — they see their legitimacy being preempted. For the analytical observer, the reading is tangled rope: both coordination and extraction are structurally present. The reading is NOT a natural law (mountain) — it is a highly contingent framing that benefits particular agents and suppresses others. The reading is NOT rope (pure coordination) — it has substantial asymmetric extraction. The reading IS tangled rope: genuine coordination exists alongside genuine extraction, and which one is salient depends on the observer's structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenantal_title_binding_force,
    'Does a pre-modern religious covenant constitute a binding legitimacy claim for modern territorial sovereignty under contemporary international law?',
    'Comparative analysis of legitimacy grounds across territorial claims: how many sovereignty claims rest on pre-modern religious covenants vs. other legitimacy sources (conquest, settlement, self-determination, international treaty). If covenant-based claims are outliers, the force of covenantal title is convention-dependent rather than inherent.',
    'If binding: covenant-continuity reading''s deontological axiom is holdable. If not binding: reading survives only through political power and institutional inertia (piton), not through epistemic force. Axiom status shifts from holdable to overridden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(covenantal_title_binding_force, conceptual, 'Whether pre-modern religious covenants bind contemporary territorial sovereignty').

omega_variable(
    demographic_continuity_gap,
    'Does a 2000-year demographic absence constitute a breach of continuous occupancy sufficient to invalidate continuity-based legitimacy claims?',
    'International law analysis: comparison with other territorial claims involving demographic gaps (Greek Cyprus, Kosovo, Armenian historical presence in Anatolia). Does international recognition track demographic continuity or other legitimacy grounds?',
    'If continuity requires unbroken presence: reading''s legitimacy claim is undermined; self-determination reading (based on modern demographic majority) gains strength. If continuity can survive absence: reading is reinforced; settlements frame as return becomes structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_continuity_gap, conceptual, 'Whether 2000-year demographic absence breaks continuity-based territorial claims').

omega_variable(
    partition_as_compromise_vs_creation,
    'Does the 1948 partition establish a new state entity (creation framework) or formalize an existing covenantal right (compromise framework)?',
    'Historical-institutional analysis of the UN Partition Plan, Balfour Declaration, and 1948 Declaration of Independence. Did institutional actors at the time frame the partition as recognizing a pre-existing right or as allocating territory to a newly claiming group?',
    'If creation framework: Palestinian claims gain equal legitimacy weight as Jewish claims (self-determination reading strengthens). If compromise framework: Jewish claim is pre-existing, Palestinian claim is secondary (covenant-continuity reading is reinforced). This distinction is the hinge between readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_as_compromise_vs_creation, empirical, 'Whether 1948 partition created a new state or formalized a pre-existing right').

omega_variable(
    settlement_framing_return_vs_colonization,
    'Are post-1967 settlements framing as return to covenanted territory structurally distinct from settler colonialism, or does the distinction collapse under comparison with other territorial expansions?',
    'Comparative settler colonialism analysis: are return-framed settlements structurally similar to (a) other return movements (Greek-Turkish population exchanges, Jewish diaspora migration to non-Palestinian territories), (b) standard settler colonialism (European colonization, Russian eastward expansion), or (c) a novel hybrid? What institutional and demographic patterns distinguish them?',
    'If distinct: covenant-continuity reading''s settlement logic is coherent. If collapse: reading relies on exceptional pleading and identity-based special case (weakens legitimacy claim). Affects both axiom holdability and perspectival classification of settlement actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_framing_return_vs_colonization, empirical, 'Whether settlement framing as return is structurally distinct from settler colonialism').

omega_variable(
    existential_zero_sum_incompatibility,
    'Is the covenant-continuity reading logically incompatible with the existential-matrix reading (both groups require territorial control for survival), or can both frameworks coexist?',
    'Formal logical analysis: does covenantal legitimacy for one group necessarily preclude existential territorial requirement for another? Can two groups each hold covenantal/existential claims to the same territory within a single normative framework?',
    'If incompatible: the readings foreclose one another (rare gate). If coexistent: they remain live positions held by different parties (standard gate). The relation choice between covenant-continuity and existential-matrix readings depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(existential_zero_sum_incompatibility, conceptual, 'Whether covenant-continuity and existential-matrix readings logically foreclose each other').

omega_variable(
    international_recognition_retroactive_legitimation,
    'Does international recognition (Balfour Declaration, UN Partition Plan, 1948) retroactively legitimate a covenantal claim that predates recognition, or does recognition constitute the legitimacy source?',
    'Conceptual-institutional analysis: is the covenant-continuity reading''s legitimacy grounded in the covenant itself (recognition merely confirms) or in the recognition acts themselves (covenant is historical narrative without contemporary legal force)? Which entity or process did institutional actors treat as the legitimacy source?',
    'If covenant self-legitimates: reading is deontologically grounded and resistant to institutional challenge. If recognition is source: reading is vulnerable to recognition withdrawal and depends on institutional will continuation. This affects reading''s structural stability and axiom status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_recognition_retroactive_legitimation, conceptual, 'Whether international recognition retroactively legitimates or constitutes covenantal claim').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_sov_cov_theater_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(terr_sov_cov_theater_t25, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(terr_sov_cov_theater_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(terr_sov_cov_extract_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(terr_sov_cov_extract_t25, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(terr_sov_cov_extract_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_sov_cov_supp_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(terr_sov_cov_supp_t25, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(terr_sov_cov_supp_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, settlement_legitimacy_framing).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_territorial_claims_structure).

% DUAL FORMULATION NOTE:
% The territorial sovereignty legitimacy kernel contains three structurally distinct constraint stories, each with different epsilon values and beneficiary/victim structures. The covenant-continuity reading (this story) has ε=0.58 and treats covenantal title as a binding legitimacy source. The self-determination reading has different ε (higher extraction for Palestinians under covenantal framing) and treats modern self-determination principle as primary legitimacy source. The existential-matrix reading has yet different ε reflecting the shift from historical/legal grounding to existential survival framing. All three are related via network edges and share the same kernel (territorial sovereignty legitimacy) but instantiate different readings with different authority-grounding structures and reference frames.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
