% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__zionist_refuge_reading, []).

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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Israeli Territorial Legitimacy (Zionist Refuge Reading): Historical Persecution, Divine Promise, UN Partition
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint models Israel's territorial legitimacy as instantiated
 *   through the zionist refuge reading — legitimacy grounded in historical
 *   persecution of Jews, divine promise of the Land, and acceptance of UN
 *   partition in 1948. This is ONE reading of a contested kernel: the same
 *   territorial legitimacy claim is read differently in the
 *   palestinian_autochthony_reading (grounding legitimacy in continuous
 *   Palestinian habitation and displacement trauma) and the
 *   two_state_coexistence_reading (grounding mutual recognition in 1967
 *   boundaries as compromise). The zionist refuge reading frames 1948
 *   legitimacy as uncontested, 1967 boundaries as negotiable, Palestinian
 *   displacement as a consequence of Arab rejection of partition, and
 *   security concerns as justifying Israeli territorial control. The
 *   constraint exhibits tangled_rope structure: it coordinates genuine state
 *   security and institutional stability (beneficiary functions) while
 *   simultaneously extracting Palestinian territorial claims and displacing
 *   communities (victim functions). The extractiveness trajectory shows
 *   accumulation over 75 years: from 0.35 (1948 foundational moment with
 *   partition acceptance) to 0.58 (2026, after decades of occupation and
 *   failed boundary negotiation). Suppression rises from 0.42 to 0.62,
 *   reflecting increasing military and institutional enforcement requirements
 *   as the reading's factual premises (Arab rejection, security necessity,
 *   partition legitimacy) become increasingly contested. Theater ratio rises
 *   from 0.48 to 0.68, indicating that the partition legitimacy citation has
 *   become more performative — the original functional problem (Jewish
 *   refugee resettlement) was resolved decades ago, yet the reading persists
 *   in institutional discourse as the justification for continuing
 *   territorial control.
 *
 * KEY AGENTS:
 *   - Israeli State Security Establishment: Primary beneficiary (institutional/arbitrage) — gains legitimacy, international recognition, resource mobilization, security justification; experiences constraint as coordination mechanism
 *   - Jewish Diaspora Refuge Seekers: Historical beneficiary (moderate/mobile to constrained) — the reading grounds their refugee protection and resettlement; earlier cohorts benefited directly; later diaspora benefits through identity affiliation
 *   - Palestinian Displacement Communities: Primary victim (powerless/trapped) — dispossessed under the reading's framing as consequence of Arab rejection; no exit within the reading's logic; no right of return
 *   - Arab State Coalition: Secondary victim/constrained actor (powerful to organized/constrained) — experience extraction (positioned as rejecters of partition, security threat); also coordinate regionally using the constraint
 *   - Palestinian National Authority: Identity-locked secondary actor (moderate/identity_locked) — governance structures partially recognize the constraint while national identity is constituted through rejection of it
 *   - International Legal Arbiters: Organized coordinating actors (organized/constrained) — enforce partition precedent selectively; maintain legitimacy framework; themselves constrained by diplomatic pressure and precedent contradictions
 *   - Analytical Observer: Sees the reading's natural law framing as a false summit — the constraint naturalizes a contingent reading as immutable international law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.62).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Israeli Territorial Legitimacy (Zionist Refuge Reading): Historical Persecution, Divine Promise, UN Partition").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, 'eb642aa5-9606-4516-ab02-00a22af5a9b5').
narrative_ontology:cs_kernel_codification('eb642aa5-9606-4516-ab02-00a22af5a9b5', fixed_text).
narrative_ontology:cs_authority_grounding('eb642aa5-9606-4516-ab02-00a22af5a9b5', lineage).
narrative_ontology:cs_interpretation_layer_present('eb642aa5-9606-4516-ab02-00a22af5a9b5').
narrative_ontology:cs_reading_relation('eb642aa5-9606-4516-ab02-00a22af5a9b5', territorial_legitimacy_dual__palestinian_autochthony_reading, forecloses).
narrative_ontology:cs_reading_relation('eb642aa5-9606-4516-ab02-00a22af5a9b5', territorial_legitimacy_dual__two_state_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('eb642aa5-9606-4516-ab02-00a22af5a9b5', foundational, jewish_refugee_right_to_territorial_self_determination).
narrative_ontology:cs_axiom_status(jewish_refugee_right_to_territorial_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('eb642aa5-9606-4516-ab02-00a22af5a9b5', jewish_refugee_right_to_territorial_self_determination, deontological).
narrative_ontology:cs_axiom('eb642aa5-9606-4516-ab02-00a22af5a9b5', secondary, divine_promise_historical_legitimacy).
narrative_ontology:cs_axiom_status(divine_promise_historical_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('eb642aa5-9606-4516-ab02-00a22af5a9b5', divine_promise_historical_legitimacy, theological).
narrative_ontology:cs_reference_frame('eb642aa5-9606-4516-ab02-00a22af5a9b5', jewish_refugee_territorial_self_determination_1948).
narrative_ontology:cs_drift_state('eb642aa5-9606-4516-ab02-00a22af5a9b5', contemporary_occupation_and_failed_negotiation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('eb642aa5-9606-4516-ab02-00a22af5a9b5', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora_refuge_seekers).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, zionist_institutional_continuity).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_territorial_claims).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, arab_state_jurisdictional_authority).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_displacement_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISRAELI SECURITY ESTABLISHMENT (ROPE) — Experiences the constraint as coordination: the legitimacy claim enables resource mobilization for defense, establishes legal standing in international law, and coordinates diaspora support. The constraint functions as a coordination mechanism — it solves the collective action problem of why the international community should recognize and support a state in a contested territory. Net beneficiary with exit options (can negotiate boundaries, modify claim content within the reading's framework).
constraint_indexing:constraint_classification(territorial_legitimacy_dual__zionist_refuge_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: ARAB STATE COALITION (TANGLED ROPE) — Arab states both coordinate within the constraint (mutual recognition of territorial borders, defense treaties) and experience extraction (legitimacy framework positions them as rejecters of partition, justifies Israeli security dominance). Constrained exit — they can negotiate but cannot simply walk away without abandoning regional legitimacy claims.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PALESTINIAN DISPLACEMENT COMMUNITIES (SNARE) — Trapped. The legitimacy frame (Arab rejection caused displacement) forecloses the right of return within this reading's logic. Displaced communities have no exit from the territorial constraint; their dispossession is treated as a consequence of Arab state rejection of partition rather than as the founding act of Israeli statehood. Maximum extraction, minimal coordination function.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__zionist_refuge_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: PALESTINIAN NATIONAL AUTHORITY (TANGLED ROPE / IDENTITY-LOCKED) — Constrained by the territorial legitimacy framework yet participating in governance structures that implicitly recognize the constraint. Identity-locked: Palestinian institutional identity is partly constituted through rejection of the zionist refugee reading, yet governance structures require partial engagement with the legitimacy framework (Oslo, post-2000 negotiations). Experiences both coordination (state institutions stabilize territory) and extraction (legitimacy denial).
constraint_indexing:constraint_classification(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL ARBITERS (TANGLED ROPE) — Organized bodies that both coordinate (UN partition decision, recognition framework) and extract (arbitration power, selective application of partition precedent to Israel vs. disputed territories elsewhere). Constrained by precedent and diplomatic pressure; participate in maintaining the legitimacy framework while managing contradictions with other partition/self-determination claims.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the legitimacy of territorial states grounded in historical persecution and UN recognition appears as a settled, unchangeable feature of international law post-WWII. The reading naturalizes the specific instantiation (Zionist refuge claim) as following inevitably from universal principles (right to self-determination, refugee protection, genocide prevention). However, the structural data contradicts the mountain classification — the engine's false summit detector will identify this as naturalization of a contingent reading of the partition kernel, revealing that what appears as immutable law is actually one contested interpretation among multiple live readings.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__zionist_refuge_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: INSTITUTIONAL REPETITION OF UN PARTITION PRECEDENT (PITON) — The UN partition framework persists as a legitimacy citation long after the foundational conditions (refugee crisis, post-WWII institutional moment, uncontested Jewish majority in designated territory) have shifted. The theater ratio (0.68) reflects that partition citation in 2026 is substantially performative — the original functional coordination problem (resettlement of displaced Jews) was resolved decades ago, yet the legitimacy claim continues in its institutional form due to inertia and absence of alternative resolution framework. The piton perspective shows the constraint as a degraded institutional form maintained through repetition rather than function.
constraint_indexing:constraint_classification(territorial_legitimacy_dual__zionist_refuge_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(territorial_legitimacy_dual__zionist_refuge_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(territorial_legitimacy_dual__zionist_refuge_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, TR),
    TR >= 0.70.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The reading coordinates genuine state security functions and institutional stability (beneficiary functions: security apparatus, international recognition, diaspora mobilization). Simultaneously, it extracts Palestinian territorial claims and justifies displacement. The extractiveness is not maximal (would require snare classification with near-zero coordination) because the coordination functions are real — the state does stabilize territory, enable Jewish refuge, and coordinate regional order. However, extractiveness is substantial because the coordination benefits accrue to Israeli institutions and Jewish diaspora, while extraction (territorial dispossession, displacement, jurisdictional exclusion) falls on Palestinians. The measurement trajectory shows accumulation: extractiveness rises from 0.35 to 0.58 as the foundational conditions (refugee crisis, Arab state rejection) become historically distant, yet the legitimacy claim persists in institutional form. This accumulation signals that extraction is increasing relative to coordination function — the original refugee resettlement problem was solved decades ago, yet the reading continues to justify territorial control. Suppression (0.62): Moderate-high. Substantial enforcement requirements maintain the territorial claim: military occupation (1967-present), settlement expansion, administrative control of Palestinian space, legal restrictions on Palestinian statehood claims, international diplomatic enforcement of partition framework. Suppression is not total (0.70+) because some Palestinian self-governance exists (PA) and some Arab states have recognized Israel. But suppression is substantial because Palestinian territorial and return claims are structurally foreclosed within the reading's logic, and enforcement of Israeli territorial control requires continuous military and administrative mechanisms. Theater ratio (0.68): High. The partition legitimacy citation has become substantially performative. The original functional problem — resettlement of Jewish refugees displaced by WWII and antisemitism — was resolved in the 1950s-60s. Yet the reading persists in 2026 institutional discourse as the primary justification for Israeli territorial control, 1967 occupation, and settlement policy. The partition citation functions as a legitimacy ritual rather than as a contemporary justification (security concerns, demographic majority, institutional stability are the real contemporaneous justifications). The high theater ratio indicates the constraint operates more through institutional repetition than through active coordination or contemporary functional necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same territorial legitimacy claim classifies differently from different structural positions. The Israeli security establishment sees rope (coordination of security and institutional stability). The Arab state coalition sees tangled_rope (both coordinating regionally and experiencing extraction through positioning as 'rejecters'). Palestinian displacement communities see snare (pure extraction with no exit within this reading's framework). The Palestinian Authority sees tangled_rope + identity_locked (constrained to participate in governance structures that implicitly recognize the reading while constitutionally rejecting it). International arbiters see tangled_rope (coordinating through partition framework while managing contradictions with other self-determination claims). The analytical observer risks seeing mountain (immutable international law post-WWII) but the engine's false summit detector reveals this as naturalization of a contingent reading. The perspectival gap exposes that the reading's legitimacy force is not universal — it is strongly persuasive to institutions that benefit from it (Israeli state, Western liberal democracies, international legal order) and directly contradictory to Palestinians whose territorial claims it forecloses. The claimed universality of 'UN partition acceptance' masks that partition is here interpreted as creating a single (Jewish) state's legitimacy while treating Palestinian territorial claims as negotiable remnants of that partition, rather than as coequal partition outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) — the agent's structural position relative to extraction flow — is determined by their beneficiary/victim status and exit options. Israeli security establishment: beneficiary + arbitrage exit → low d → low/negative χ → sees rope. Palestinian displacement communities: victim + trapped exit → high d → high χ → sees snare. Palestinian Authority: victim-coded (their territorial claims are extracted) + identity_locked exit (cannot exit the legitimacy framework without abandoning Palestinian national identity) → high d but partially mitigated by identity frame → sees tangled_rope with identity_locked binding. Arab states: mixed (victims of security framing yet coordinators within regional system) + constrained exit → moderate-high d → sees tangled_rope. International arbiters: beneficiaries of the coordination function (partition framework enables their authority) + constrained exit (bound by precedent) → moderate d → sees tangled_rope. The analytical observer: neutral witness + analytical exit → derivation produces high d (1.15 f(d) value) reflecting the observer's distance from the extraction flow but their structural inability to resolve the contest.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_promise_vs_secular_international_law,
    'Can a territorial legitimacy claim rest on divine promise and historical persecution simultaneously with UN partition acceptance, or does invoking divine warrant undermine secular legal grounding?',
    'Examine foundational Israeli legal documents and diplomatic discourse; track whether divine promise is invoked in legal arguments or remains in nationalist/religious narrative; identify cases where secular and theological grounds diverge',
    'If divine warrant is essential to the reading: legitimacy collapses outside monotheistic frameworks; universalizability fails. If divine warrant is ornamental: legitimacy stands on secular grounds alone, which also undergird Palestinian and two-state readings. The core reading becomes: ''Partition acceptance + historical persecution justify statehood,'' and the divine component becomes cultural-nationalist decoration rather than essential premise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_promise_vs_secular_international_law, conceptual, 'Whether divine promise is foundational or ornamental to this reading''s legitimacy claim').

omega_variable(
    arab_rejection_causation_for_displacement,
    'Is Palestinian displacement a consequence of Arab state rejection of UN partition (as this reading frames it) or a cause of Arab rejection and ongoing conflict?',
    'Chronological analysis of 1947-1949 events: did displacement occur during and as consequence of military rejection, or did displacement precede and trigger rejection? Examine contemporaneous Arab state communications to distinguish retroactive justification from documented causation.',
    'If displacement is consequence of rejection: the reading''s framing is structurally accurate within its internal logic. If displacement precedes and causes rejection: the reading inverts causal order to naturalize Israeli agency as responsive rather than initiatory. The classification would shift toward snare (pure extraction) from tangled_rope if the causation inversion is recognized as a structural feature of the legitimacy claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arab_rejection_causation_for_displacement, empirical, 'Causal direction: does Arab rejection cause displacement, or displacement causes Arab rejection?').

omega_variable(
    id_1967_boundary_negotiability_vs_existential_security,
    'Are the 1967 (pre-occupation) boundaries genuinely negotiable within this reading''s framework, or does the security concern (existential threat from Arab states) make any territorial contraction structurally untenable?',
    'Examine Israeli policy documents and diplomatic proposals; identify stated red lines for territorial control; track whether security rationales can be satisfied at 1967 boundaries with alternative arrangements (demilitarization, international guarantees) or whether control of occupied territories is presented as non-negotiable',
    'If 1967 boundaries are genuinely negotiable: the reading accommodates territorial compromise while maintaining legitimacy. If security concerns render contraction untenable: the reading''s ''negotiable boundaries'' claim is performative, and actual territorial expansion is treated as non-negotiable security requirement. The constraint would show higher extraction and lower coordination if the security frame precludes actual boundary revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(id_1967_boundary_negotiability_vs_existential_security, empirical, 'Whether 1967 boundary negotiability is genuine or performative within security framework').

omega_variable(
    jewish_diaspora_as_beneficiary_vs_israeli_arabs,
    'How does the zionist refuge reading incorporate or exclude Israeli Arab citizens? Do they benefit from the territorial legitimacy claim (citizenship protection, state stability) or experience extraction (national identity defined as non-Jewish refuge)?',
    'Examine citizenship law, national identity law, and legal status of Israeli Arab communities; track whether legitimacy framework extends refugee/refuge logic to non-Jewish citizens or restricts it to Jewish diaspora; identify exclusions or differential status',
    'If Israeli Arabs are beneficiaries of the state security/stability coordination: the reading''s beneficiary set is broader, and extraction is less asymmetric. If Israeli Arabs experience the constraint as extraction (national identity hierarchy, demographic anxiety narratives): the beneficiary set is Jewish-specific, and the tangled_rope classification understates asymmetric extraction. The reading''s logical scope would be clarified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jewish_diaspora_as_beneficiary_vs_israeli_arabs, empirical, 'Whether Israeli Arabs are included as beneficiaries or excluded from refuge reading').

omega_variable(
    kernel_reading_contested,
    'Is this reading (zionist refuge grounded in persecution, divine promise, partition acceptance) the operative legitimacy framework of Israeli institutions, or is it one narrative among competing Israeli legitimacy readings?',
    'Analyze Israeli institutional discourse (government, military, judiciary, education); identify which legitimacy premises are legally operative vs. culturally resonant; distinguish between readings embraced by different Israeli political coalitions',
    'If this reading is institutionally dominant: it is the constraint you are modeling. If multiple Israeli legitimacy readings coexist (religious zionism, secular liberalism, territorial nationalism, diaspora identity): the engine should model multiple stories, one per reading, linked by network.affects_constraints. A single story assumes the zionist refuge reading is the institutional kernel; multiple stories recognize internal Israeli contestation about what grounds legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contested, conceptual, 'Whether zionist refuge reading is institutionally dominant or one of multiple Israeli legitimacy framings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_legit_zion_theater_t0, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(terr_legit_zion_theater_t20, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(terr_legit_zion_theater_t40, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(terr_legit_zion_extract_t0, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(terr_legit_zion_extract_t20, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(terr_legit_zion_extract_t40, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_legit_zion_supp_t0, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(terr_legit_zion_supp_t20, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(terr_legit_zion_supp_t40, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual__two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% The territorial legitimacy kernel admits three structurally distinct constraint readings with different extractiveness values and beneficiary/victim structures. The zionist refuge reading (this story, ε=0.58) coordinates state security while extracting Palestinian claims. The palestinian autochthony reading (ε~0.65) inverts the victim/beneficiary mapping and frames Zionist settlement as extraction. The two_state_coexistence_reading (ε~0.35) reframes both as beneficiaries of partition logic with 1967 boundaries as the compromise framework. These are not the same constraint viewed from different angles — they have materially different ε values, different causation narratives, and different institutional force. They are linked as readings of a single contested kernel, competing for institutional adoption. The decomposition follows the ε-invariance principle: when the observables used to evaluate the legitimacy claim produce different ε values across readings, the constraints are structurally distinct and modeled separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__zionist_refuge_reading, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
