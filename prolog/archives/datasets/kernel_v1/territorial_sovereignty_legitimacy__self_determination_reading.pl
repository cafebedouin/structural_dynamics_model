% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty Legitimacy (Self-Determination Reading)
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested kernel of
 *   territorial sovereignty legitimacy: the principle that legitimacy derives
 *   from modern self-determination applied to the Arab population with
 *   demographic majority and continuous residence in the territory during the
 *   modern period (19th-20th centuries). This reading is structurally
 *   distinct from two sibling readings — the covenant_continuity reading
 *   (Jewish historical presence + international recognition) and the
 *   existential_matrix reading (existential security as the primary basis for
 *   legitimacy). The self-determination reading produces a Tangled Rope
 *   classification: it contains a genuine coordination principle
 *   (self-determination is a real doctrine with real organizing power across
 *   anti-colonial movements worldwide) combined with asymmetric extraction
 *   (the principle is selectively enforced, suspended for strategic allies,
 *   used as legitimacy theater). The constraint's growing suppression (0.55 →
 *   0.72) and theater ratio (0.45 → 0.65) over the interval reflect the
 *   increasing gap between the principle's stated universality and its actual
 *   selective application, particularly as the Israeli state has consolidated
 *   control and international enforcement of Palestinian self-determination
 *   has remained suspended. The extractiveness (0.35 → 0.58) captures the
 *   conversion of a coordination principle (anti-colonial organizing) into an
 *   enforcement mechanism against a specific state (Israeli state as colonial
 *   project requiring reversal). The reading frames the partition as an
 *   unjust external imposition, the right of return as restoration of the
 *   status quo ante, and the Israeli state as a colonial project — these
 *   framings distinguish this reading from alternatives and set up the
 *   potential foreclosure or coexistence relationships with sibling readings.
 *
 * KEY AGENTS:
 *   - Arab Palestinian population: Primary beneficiary (moderate/constrained) — the reading asserts their self-determination right based on demographic majority and continuous residence; they experience extraction through suppression of enforcement
 *   - Displaced Palestinian refugees: Victim (powerless/trapped) — the reading's legitimacy principle should protect their right of return, but international non-enforcement and Israeli state law block return; they experience pure extraction (Snare perspective)
 *   - Jewish Israeli population: Dual position (powerful/mobile as state apparatus, but trapped by the legitimacy contradiction this reading imposes) — the reading characterizes the Israeli state as colonial and therefore delegitimized; from this reading's frame, the state apparatus extracts against the demographic majority
 *   - Israeli state apparatus: Target (institutional/mobile but logically trapped) — the reading frames the state as the enforcement mechanism of partition (external imposition); maintaining territorial control requires continuous suppression of the self-determination claim
 *   - International anti-colonial sovereignty movements: Beneficiary (institutional/arbitrage) — the self-determination principle provides legitimacy infrastructure for challenging other colonial arrangements; they benefit from the principle's universality
 *   - International legal order (UN): Constrained beneficiary (institutional/constrained) — coordinates post-WWII decolonization using self-determination principle but selectively enforces it; extraction: the principle is treated as suspended for geopolitically strategic cases
 *   - Analytical observer: Civilizational witness (analytical/analytical) — risks naturalizing the reading's principle as an immutable law rather than recognizing it as a modern construct contingently applied
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.58).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.72).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Territorial Sovereignty Legitimacy (Self-Determination Reading)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, '0379cc10-6518-4b8d-abec-7b5d3a3b1027').
narrative_ontology:cs_kernel_codification('0379cc10-6518-4b8d-abec-7b5d3a3b1027', formalized).
narrative_ontology:cs_authority_grounding('0379cc10-6518-4b8d-abec-7b5d3a3b1027', lineage).
narrative_ontology:cs_interpretation_layer_present('0379cc10-6518-4b8d-abec-7b5d3a3b1027').
narrative_ontology:cs_reading_relation('0379cc10-6518-4b8d-abec-7b5d3a3b1027', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('0379cc10-6518-4b8d-abec-7b5d3a3b1027', territorial_sovereignty_legitimacy__existential_matrix_reading, influences).
narrative_ontology:cs_axiom('0379cc10-6518-4b8d-abec-7b5d3a3b1027', foundational, demographic_majority_legitimacy).
narrative_ontology:cs_axiom_status(demographic_majority_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0379cc10-6518-4b8d-abec-7b5d3a3b1027', demographic_majority_legitimacy, deontological).
narrative_ontology:cs_axiom('0379cc10-6518-4b8d-abec-7b5d3a3b1027', foundational, partition_as_external_imposition).
narrative_ontology:cs_axiom_status(partition_as_external_imposition, holdable).
narrative_ontology:cs_axiom_grounding('0379cc10-6518-4b8d-abec-7b5d3a3b1027', partition_as_external_imposition, empirically_contingent).
narrative_ontology:cs_axiom('0379cc10-6518-4b8d-abec-7b5d3a3b1027', secondary, right_of_return_as_status_quo_restoration).
narrative_ontology:cs_axiom_status(right_of_return_as_status_quo_restoration, holdable).
narrative_ontology:cs_axiom_grounding('0379cc10-6518-4b8d-abec-7b5d3a3b1027', right_of_return_as_status_quo_restoration, deontological).
narrative_ontology:cs_reference_frame('0379cc10-6518-4b8d-abec-7b5d3a3b1027', pre_partition_arab_demographic_majority_framework).
narrative_ontology:cs_drift_state('0379cc10-6518-4b8d-abec-7b5d3a3b1027', contemporary_post_oslo_framework, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0379cc10-6518-4b8d-abec-7b5d3a3b1027', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_palestinian_population).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, anti_colonial_sovereignty_movements).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, jewish_israeli_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, partition_agreement_enforcement).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, international_law_continuity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED PALESTINIAN REFUGEE (SNARE) — Trapped by military occupation, legal restrictions on return, and international non-enforcement of right of return. Experiences the constraint as pure extraction: demographic presence in territory during modern period establishes legitimacy claim, yet that very presence is criminalized or barred from enforcement. No exit option; maximum suppression of the legitimacy claim through military and legal enforcement against the claimant population.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__self_determination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PALESTINIAN AUTHORITY (TANGLED ROPE) — Constrained by dependence on international recognition and funding; also benefits from the legitimacy principle (self-determination rhetoric) in negotiations. Genuine coordination function: the Palestinian Authority coordinates resource allocation and governance among a stateless population. But asymmetric extraction persists: the authority's power depends on accepting Oslo framework which denies the full self-determination principle this reading instantiates. Mixed experience — some agency through coordination, significant extraction through structural constraint.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL ANTI-COLONIAL SOVEREIGNTY MOVEMENT (ROPE) — Benefits from the self-determination principle as a legitimacy frame for challenging other colonial arrangements (India, Algeria, Vietnam, etc.). Experiences this constraint as coordination: the principle enables collective action across multiple territories and time periods. Net beneficiary — the constraint provides intellectual and moral infrastructure for anti-colonial organizing. Arbitrage exit: can frame their cause using this principle and exit to other jurisdictions with similar legitimacy logic.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__self_determination_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ISRAELI STATE APPARATUS (SNARE) — From this reading's perspective, the Israeli state is the primary target of extraction. This reading classifies the state as a colonial project (external imposition by Great Power partition and international recognition) whose legitimacy contradicts the self-determination principle applied to the territorial majority. The state has high power and mobility (can relocate institutions, redefine territory, maintain military dominance), yet is trapped by the legitimacy contradiction: the more the state enforces its control, the more it demonstrates that self-determination is being denied to the demographic majority. Extraction runs toward enforcement of this denial, creating a permanent suppression regime. This perspective produces Snare not because the Israeli state is powerless, but because the logical structure of the reading forces the state into a position where maintaining territorial control requires continuous extraction (military occupation, legal discrimination, settlement expansion) against a population with a superior legitimacy claim under this reading's own principle.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__self_determination_reading, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL ORDER (TANGLED ROPE) — Coordination function: the self-determination principle is one of the foundational doctrines of the UN Charter and international law, enabling decolonization and legitimacy for over a hundred post-colonial states. But asymmetric extraction: the principle is selectively enforced. This reading generates permanent tension between the principle (self-determination for all) and its actual application (self-determination enforced only where Great Powers permit). The international order benefits from having the principle as legitimacy cover for its own authority; the constrained exit comes from the fact that abandoning the principle would delegitimize the entire post-WWII order. Suppression: the international community suppresses enforcement of this reading by treating the Palestinian case as exceptional, frozen, or requiring competing principles (existing state sovereignty, right of return vs. one-state reality).
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SELF-DETERMINATION DOCTRINE AS PITON (PERFORMATIVE) — The self-determination principle has been degraded into theater. It is invoked in UN speeches and NGO advocacy but enforcement is suspended for strategically favored states (Israel, Turkey in Cyprus, Morocco in Western Sahara). The doctrine persists through institutional inertia (it is foundational to UN legitimacy) but functions primarily as legitimacy theater for selective application. Theater ratio high because the gap between the principle's stated universality and its actual selective enforcement is wide and stable.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__self_determination_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, self-determination as applied to demographic majorities appears as an immutable principle of political legitimacy in the modern world: any population with continuous residence and demographic majority in a territory has an inherent right to determine its political status. This perspective naturalizes the reading's core principle. However, the structural data reveals this as a false summit: the principle is a modern construct (19th-20th century emergence), contingently applied, and contradicts other legitimacy principles (ancient covenant, existential security needs). The mountain classification here instantiates the oracle gap (Theorem 4): the analytical observer's native instruments cannot detect the reading's contingency because the observer's framework is internal to the liberal internationalist tradition that produced the principle.
constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__self_determination_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__self_determination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(territorial_sovereignty_legitimacy__self_determination_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, TR),
    TR >= 0.70.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The self-determination principle is genuine coordination doctrine with real organizing power (base coordination value ~0.30-0.35), but application to the Israel-Palestine case converts it into an enforcement mechanism against a state that has rejected partition. The extraction is elevated because the principle is invoked to delegitimize an existing state rather than to coordinate among multiple self-determining peoples. The reading's directive is asymmetric: it asserts Palestinian self-determination as correct and frames the Israeli state as the extraction mechanism. Suppression (0.72): High. The reading identifies multiple suppression mechanisms: military occupation prevents Palestinian territorial self-governance; legal discrimination prevents equal status within the Israeli state; international non-enforcement of right of return suspends the reading's core remedial claim; treatment of the Palestinian case as exceptional (not subject to the self-determination principle that applied elsewhere in decolonization) is itself a suppression mechanism. Theater ratio (0.65): Moderate-high. The self-determination principle is invoked rhetorically by UN organs, NGOs, and Palestinian political movements, but enforcement is suspended. The gap between the principle's stated universality and its actual application to selected cases makes the doctrine substantially performative in this context. Suppression requirement rising over time (0.55 → 0.72) reflects increasing state enforcement machinery required to maintain partition against a reading that asserts it unjust. Theater ratio rising (0.45 → 0.65) reflects increasing rhetorical invocation of self-determination principle without corresponding enforcement capacity.
 *
 * PERSPECTIVAL GAP:
 *   Maximum: the reading instantiates the full range of DR types from a single logical kernel. The perspectival gap reveals the reading's structural function: it is both a genuine coordination principle (enabling anti-colonial organizing across multiple territories) and a delegitimizing assertion against a specific state. This dual function cannot be resolved into a single classification — the presheaf over perspectives IS the analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural position relative to the extraction flow. Beneficiaries of the principle (Palestinian population, anti-colonial movements) occupy low-d positions because they would benefit from enforcement (d ~ 0.2-0.4 depending on constrain/arbitrage exit); they experience negative or low effective extraction. Victims of the reading's assertion that Israeli state is colonial/delegitimized (Israeli state, partition framework) occupy high-d positions (d ~ 0.75-0.95) because the reading frames their position as extraction; they experience high effective extraction. The Israeli state's directionality is unusual: as an institutional actor with high power and mobile options, it would normally have low d (beneficiary-type position), but the reading's frame (colonial project, illegitimate partition) forces the state into a high-d position by asserting that maintaining control = extracting against the demographic majority. This is a frame-dependent directionality reversal. The international legal order occupies a middle position (d ~ 0.55): it benefits from the principle as legitimacy cover but is trapped by selective enforcement, creating constrained extraction both toward itself (criticism for non-enforcement) and toward claimants (suspended enforcement).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED VIA READING STRUCTURE: This constraint exemplifies mandatrophy resolution through kernel-reading distinction. The mandate is: 'Is self-determination a coordinating principle (Rope) or an extractive delegitimization (Snare)?' The reading's answer is: both, in different perspectives. The anti-colonial movement experiences it as Rope (coordination infrastructure). The Palestinian refugee experiences it as Snare (extracted principle: should legitimate return but doesn't). The Israeli state experiences it as Snare (assertion that the state is colonial and must be reversed). The international order experiences it as Piton (performative invocation without enforcement). The analytical observer experiences it as Mountain (naturalized law) but the structure reveals false summit (modern construct, contingently applied). Mandatrophy dissolves when you recognize that the reading is ONE reading of a contested kernel, and different reading-position pairs produce different classifications. The structure is coherent at this meta-level: the self-determination reading is internally consistent (Tangled Rope mixing coordination + selective enforcement asymmetry), and the perspectival divergence is diagnostic of the reading's structural position within the contested kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_majority_census_baseline,
    'At what historical moment should demographic majority be measured to determine legitimacy? Pre-1948? 1948? Pre-1967? Contemporary?',
    'Specification of the census baseline within the self-determination principle. Different baselines produce different legitimacy conclusions. Historical demography of Palestine (Ottoman census 1893, British Mandate censuses 1922-1931, 1948 status quo ante, etc.).',
    'If baseline = 1893 Ottoman census: Arab majority legitimacy is clear, right of return is restoration. If baseline = contemporary: Jewish Israeli majority in Israel proper challenges Palestinian majority claim in same territory. If baseline = pre-1967: Palestinian majority in West Bank/Gaza is clear, Israeli majority in Israel proper is clear, partition becomes negotiable rather than illegitimate. The reading''s internal consistency depends on this choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_majority_census_baseline, empirical, 'Historical baseline for measuring demographic majority').

omega_variable(
    continuous_residence_definition,
    'Does ''continuous residence during the modern period'' require unbroken presence for every individual, every family, or only aggregate population presence? How does forced displacement, exile, and refugee status affect the claim?',
    'Specification of what ''continuous residence'' means in the principle. Does it require indigenous continuity (pre-Zionist presence), or only residence during the period when legitimacy is being claimed (1880s onward)? How are 1948 refugees treated — do they retain continuous-residence status despite expulsion?',
    'If ''continuous'' means unbroken individual presence: refugee populations lose legitimacy standing. If ''continuous'' means aggregate population with right of return: refugee status is temporary interruption of continuous presence, legitimacy claim persists. This is the crux of the right of return debate — whether displacement breaks the legitimacy chain or suspends it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuous_residence_definition, conceptual, 'Definition of continuous residence and treatment of displacement').

omega_variable(
    external_imposition_vs_international_recognition,
    'Does international legal recognition (Balfour Declaration, UN Partition Plan) legitimate external imposition, or does legitimacy require consent of the self-determining people?',
    'Clarification of the relationship between self-determination principle and international recognition. Can an external power legitimately invoke self-determination for one group against another group''s self-determination claim? The reading frames partition as ''unjust imposition by external powers'' — this presumes that Great Power recognition cannot override self-determination consent.',
    'If external recognition can constitute legitimacy: partition is legitimate (Balfour + UN + Israeli Declaration of Independence). If self-determination requires consent of the self-determining people: partition is legitimate only if the Arab Palestinian population consents — which it did not. This gap opens the sibling readings'' alternative framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(external_imposition_vs_international_recognition, conceptual, 'Whether international recognition can override self-determination consent').

omega_variable(
    colonial_project_framing,
    'Is the Israeli state accurately characterized as a ''colonial project'' under this reading, or is this a frame choice that forecloses other readings?',
    'Definition of colonialism applied to the case. Does colonialism require external European power (Britain) as the imperialist, or does it apply to any settlement project justified by external ideology and enforced by external military support? Does the distinction between ''settler-colonial'' (European-origin settlers in colonized territory) and ''national self-determination movement'' map onto this case or does it collapse?',
    'If Israeli state is colonial: legitimacy derives from the covenant_continuity and existential_matrix readings as alternative frames to challenge colonial framing. If Israeli state is legitimate sovereignty response to diaspora self-determination: the covenant_continuity reading gains ground. The reading''s characterization of Israel as colonial is itself a contested interpretive choice, not a neutral description.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_project_framing, conceptual, 'Whether Israeli state is accurately characterized as colonial project').

omega_variable(
    right_of_return_status_quo_ante,
    'Does the right of return constitute restoration of status quo ante (undoing the 1948 partition) or is it a remedial principle compatible with accepting 1948 partition?',
    'Specification of whether right of return is deployed as a legitimacy principle (Palestinians have a residual claim to the territory because demographic majority) or as a remedial principle (Palestinians were wrongly displaced and should be able to return without this implying rejection of the Israeli state''s existence). Historical Palestinian and international legal positions on this distinction.',
    'If status quo ante restoration: two-state solution is incompatible with the principle, partition reversal is implied. If remedial compatibility: right of return can coexist with Israeli state sovereignty. This distinction separates this reading from versions that would foreclose the covenant_continuity reading entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(right_of_return_status_quo_ante, preference, 'Whether right of return implies partition reversal or is remedially compatible with 1948').

omega_variable(
    reading_versus_sibling_mutual_intelligibility,
    'Can holders of this reading (self-determination reading) understand and engage with the covenant_continuity reading (Jewish continuity + international recognition legitimacy) as a competing principle within the same legal/political framework, or are they logically foreclosed?',
    'Test case: does accepting the self-determination reading require denying the covenant reading''s core claim (continuous Jewish presence in the territory), or can both claims coexist with the dispute being about which principle takes priority? Does the reading''s framing of the Israeli state as ''colonial project'' foreclose or merely downrank the covenant reading?',
    'If the readings are mutually intelligible (both valid, different priority orders): the constraint is coexists_with. If self-determination reading forecloses the covenant reading (denial of Jewish continuity or invalidity of Balfour/UN): relation is forecloses. Current discourse suggests coexistence — both readings are actively held by different parties without requiring the other to be logically impossible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_versus_sibling_mutual_intelligibility, conceptual, 'Mutual logical relationship between self-determination and covenant_continuity readings').

omega_variable(
    kernel_contestation_versus_constraint_classification,
    'Is the kernel (territorial sovereignty legitimacy) itself contested, or is the constraint classification of THIS reading''s logical form what is being instantiated?',
    'Clarification: the kernel is the underlying commitment (''what makes a territorial claim legitimate?''). The reading is one answer to that kernel (''self-determination of the demographic majority''). The constraint story is the logical and structural properties of THIS reading when applied to the Israel-Palestine case. We are classifying the reading''s own structure (Tangled Rope: mixing genuine self-determination coordination principle with asymmetric suppression of its application), not adjudicating whether the reading is true.',
    'This is the meta-constraint: the self-determination principle is genuine (Rope-level coordination mechanism) but its application to this case creates suppression (international non-enforcement, selective application, treatment as exception). The constraint is not ''is self-determination legitimate?'' but ''what happens when you apply self-determination principle to a case where external powers have already partitioned the territory and created a competing state?''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contestation_versus_constraint_classification, conceptual, 'The constraint is the structure of the reading''s application, not truth of the reading itself').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_sov_sd_theater_1948, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(terr_sov_sd_theater_1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1, 0.55).
narrative_ontology:measurement(terr_sov_sd_theater_contemporary, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2, 0.65).

% Extraction over time
narrative_ontology:measurement(terr_sov_sd_extractiveness_1948, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(terr_sov_sd_extractiveness_1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1, 0.48).
narrative_ontology:measurement(terr_sov_sd_extractiveness_contemporary, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_sov_sd_suppression_1948, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(terr_sov_sd_suppression_1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1, 0.68).
narrative_ontology:measurement(terr_sov_sd_suppression_contemporary, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% The territorial_sovereignty_legitimacy kernel has three structurally distinct readings, each with its own ε, perspectives, and classification profile. The self_determination_reading (this story) has ε=0.58 (Tangled Rope) because it mixes genuine coordination principle (anti-colonial self-determination organizing) with selective enforcement (international non-enforcement against geopolitically strategic state). The covenant_continuity_reading has different ε reflecting different balance of coordination/extraction. The existential_matrix_reading has different ε reflecting non-juridical framing. Each reading is a separate constraint; together they form a family linked by network.affects_constraints. The readings do not resolve into a single type — the presheaf structure IS the diagnosis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__self_determination_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
