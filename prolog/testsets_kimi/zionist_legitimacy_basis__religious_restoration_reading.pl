% ============================================================================
% CONSTRAINT STORY: zionist_legitimacy_basis__religious_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zionist_legitimacy_basis__religious_restoration_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: zionist_legitimacy_basis__religious_restoration_reading
 *   human_readable: Religious Restoration Reading of Zionist Legitimacy (Post-1967)
 *   domain: political/historical/religious
 *
 * SUMMARY:
 *   This constraint story instantiates the religious_restoration_reading of
 *   the zionist_legitimacy_basis kernel: the interpretation, ascendant after
 *   the 1967 Six-Day War, that Zionism fulfills biblical divine promise and
 *   activates a messianic process of Jewish territorial restoration. The
 *   reading coordinates religious Zionist communities, institutions, and
 *   state-adjacent actors around settlement expansion and sovereignty claims
 *   while extracting land, autonomy, and resources from Palestinian
 *   communities and imposing diffuse costs on secular Israeli and diaspora
 *   Jewish publics. It is claimed as a theological mountain by adherents but
 *   operates as an actively enforced ideological structure with identifiable
 *   beneficiaries and victims. As a kernel reading, it is cleanly separated
 *   from its national_liberation and settler_colonial siblings; the
 *   structural delta is that religious obligation overrides secular political
 *   considerations and mandates territorial maximalism.
 *
 * KEY AGENTS:
 *   - religious_zionist_institutions: Agenda-setter (institutional/identity_locked) â interprets divine mandate, issues rulings, mobilizes communities
 *   - religious_zionist_settlers: Primary beneficiary (moderate/identity_locked) â receives land, subsidies, and theological fulfillment
 *   - palestinian_communities: Primary target (powerless/trapped) â bears displacement, occupation, and denial of self-determination
 *   - secular_israeli_public: Secondary target (moderate/identity_locked) â funds settlements, provides conscripts, bears international isolation
 *   - israeli_state_apparatus: Enforcement agent (institutional/constrained) â administers occupation and settlement infrastructure
 *   - international_community: Excluded observer (organized/constrained) â objects but lacks leverage to alter the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, 0.8).
domain_priors:suppression_score(zionist_legitimacy_basis__religious_restoration_reading, 0.84).
domain_priors:theater_ratio(zionist_legitimacy_basis__religious_restoration_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(zionist_legitimacy_basis__religious_restoration_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zionist_legitimacy_basis__religious_restoration_reading, tangled_rope).
narrative_ontology:human_readable(zionist_legitimacy_basis__religious_restoration_reading, "Religious Restoration Reading of Zionist Legitimacy (Post-1967)").
narrative_ontology:topic_domain(zionist_legitimacy_basis__religious_restoration_reading, "political/historical/religious").

domain_priors:requires_active_enforcement(zionist_legitimacy_basis__religious_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(zionist_legitimacy_basis__religious_restoration_reading, '46bd1276-b61c-45c1-bf35-f9a12b3a1f41').
narrative_ontology:cs_kernel_codification('46bd1276-b61c-45c1-bf35-f9a12b3a1f41', fixed_text).
narrative_ontology:cs_authority_grounding('46bd1276-b61c-45c1-bf35-f9a12b3a1f41', lineage).
narrative_ontology:cs_interpretation_layer_present('46bd1276-b61c-45c1-bf35-f9a12b3a1f41').
narrative_ontology:cs_reading_relation('46bd1276-b61c-45c1-bf35-f9a12b3a1f41', zionist_legitimacy_basis__national_liberation_reading, coexists_with).
narrative_ontology:cs_reading_relation('46bd1276-b61c-45c1-bf35-f9a12b3a1f41', zionist_legitimacy_basis__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('46bd1276-b61c-45c1-bf35-f9a12b3a1f41', foundational, divine_sovereignty_overrides_human_politics).
narrative_ontology:cs_axiom_status(divine_sovereignty_overrides_human_politics, holdable).
narrative_ontology:cs_axiom_grounding('46bd1276-b61c-45c1-bf35-f9a12b3a1f41', divine_sovereignty_overrides_human_politics, theological).
narrative_ontology:cs_axiom('46bd1276-b61c-45c1-bf35-f9a12b3a1f41', foundational, territorial_maximalism_as_messianic_obligation).
narrative_ontology:cs_axiom_status(territorial_maximalism_as_messianic_obligation, holdable).
narrative_ontology:cs_axiom_grounding('46bd1276-b61c-45c1-bf35-f9a12b3a1f41', territorial_maximalism_as_messianic_obligation, theological).
narrative_ontology:cs_reference_frame('46bd1276-b61c-45c1-bf35-f9a12b3a1f41', divine_covenantal_sovereignty).
narrative_ontology:cs_drift_state('46bd1276-b61c-45c1-bf35-f9a12b3a1f41', contemporary_israeli_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('46bd1276-b61c-45c1-bf35-f9a12b3a1f41', '').
narrative_ontology:cs_kernel_id(zionist_legitimacy_basis__religious_restoration_reading, zionist_legitimacy_basis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_institutions).
narrative_ontology:constraint_beneficiary(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_communities).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_public).
narrative_ontology:constraint_victim(zionist_legitimacy_basis__religious_restoration_reading, diaspora_jewish_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret biblical texts as mandating exclusive Jewish sovereignty over all of Eretz Israel; issue religious rulings sanctifying settlement expansion; mobilize communities and state resources toward territorial maximalism; their authority derives from continuity with rabbinic lineage and divine covenant.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_institutions, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Settle in occupied territories under explicit religious mandate; receive state subsidies, military protection, and housing; their presence makes territorial withdrawal politically and theologically costly; identity is fused with the land as messianic fulfillment.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, religious_zionist_settlers, beneficiary,
    moderate, generational, identity_locked, regional).

% Provides military protection, legal frameworks, and budget allocations for settlements; conscripts soldiers to administer occupation and guard outposts; policy is partly captured by the religious restoration narrative but constrained by international law and secular constituencies.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Experience displacement, land confiscation, military administration, movement restrictions, and denial of political self-determination as the restoration reading structures Jewish exclusive sovereignty over the land.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_communities, payer,
    powerless, immediate, trapped, local).

% Live as a subordinated minority in a state defined by Jewish restorationist theology; face discriminatory land, housing, and resource policies justified by the demographic and territorial imperatives of the religious-national project.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, palestinian_citizens_of_israel, payer,
    powerless, biographical, trapped, national).

% Fund settlement enterprise through taxation; send children to military service enforcing the occupation; bear international isolation and security risks; political alternatives constrained by the fusion of religious restoration with broader Zionist national identity.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, secular_israeli_public, payer,
    moderate, biographical, identity_locked, national).

% Expected to donate, lobby, and provide moral support for the restoration project; dissent risks communal ostracism; identity fusion with Israel makes exit from the support role socially and relationally costly.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, diaspora_jewish_communities, payer,
    moderate, biographical, identity_locked, global).

% Objects to settlement expansion and occupation through diplomatic, legal, and humanitarian channels; structurally sidelined by Israeli sovereignty and great-power politics from altering the constraint's operation.
narrative_ontology:constraint_stakeholder(zionist_legitimacy_basis__religious_restoration_reading, international_community, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish religious-nationalist communities around a shared theological mission of territorial restoration, providing collective purpose, resource mobilization, and settlement expansion under a unified divine mandate.
% TRANSFER_FUNCTION: Moves land, state resources, and political sovereignty from Palestinian communities and the secular Israeli public to religious Zionist settlers and institutions, justified as fulfillment of covenantal obligation.
% ABSENT_VOICES: Palestinian refugees and their descendants, anti-Zionist Jewish religious communities (e.g., Neturei Karta), and secular Israeli peace movements are structurally excluded from the theological legitimating discourse; their objections are ruled out by the divine-mandate framing.
% DISAPPEARANCE_RATIONALE: If the divine-promise reading vanished overnight, the settlement enterprise would lose its primary theological justification, territorial maximalism would fracture into secular cost-benefit debates, and the Israeli state would have to renegotiate its legitimacy basis with religious-nationalist communities â the political map of Israel-Palestine would reorganize around secular or liberal-Zionist frameworks.
% FOUNDING_PROBLEM: The problem of Jewish statelessness and the perceived fragility of secular Zionism without divine sanction; the theological gap of how to maintain Jewish sovereignty in the biblical homeland under modern political conditions.
% FOUNDING_PROBLEM_CORROBORATION: Religious Zionist rabbis and institutions attest the problem is live from within the benefiting framework. Secular Israeli historians and Palestinian scholars contest that the founding problem was ever genuinely theological rather than colonial; no external corroboration from outside the benefiting parties supports the divine-mandate framing as an empirical description of history.
narrative_ontology:disappearance_verdict(zionist_legitimacy_basis__religious_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(zionist_legitimacy_basis__religious_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(zionist_legitimacy_basis__religious_restoration_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(zionist_legitimacy_basis__religious_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(zionist_legitimacy_basis__religious_restoration_reading, 0.8, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(zionist_legitimacy_basis__religious_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(zionist_legitimacy_basis__religious_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.80) is high because the constraint structures systematic transfer of land and sovereignty to one group at the expense of another through a theologically justified exclusionary framework. Suppression (0.84) is higher still because the arrangement requires active military occupation, legal discrimination, and suppression of Palestinian political expression to persist; additionally, dissent within Jewish communities is socially policed through identity-locking. Theater_ratio (0.48) reflects that while theological belief is genuine, an increasing share of state activity involves performative religious nationalism (e.g., settlement ceremonies, biblical tourism) that obscures the material extraction underneath. Accessibility_collapse (0.75) is high: within the religious Zionist framework, territorial compromise is theologically unimaginable; for Palestinian targets, alternatives are collapsed by force. Resistance (0.70) reflects sustained Palestinian resistance, international legal pressure, and internal Israeli dissent. The measurement series on a shared grid shows extraction and suppression rising monotonically from 1967 to the present as settlement institutionalization deepened.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (religious Zionist settlers and institutions) experiences the constraint as genuine coordination toward a transcendent purpose; their directionality sits near the beneficiary end. The Palestinian seat experiences pure extraction and sits at full target. The secular Israeli seat is bifurcated: they receive national-coordination benefits but pay extraction through taxes, conscription, and lost international standing, placing them in the middle-high target range. The engine computes this divergence from the structural data without requiring claim reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the religious Zionist institutions and settlers who receive land, state resources, and theological legitimation. Victims are Palestinian communities (direct territorial and political extraction), Palestinian citizens of Israel (discriminatory allocation), and the secular Israeli and diaspora Jewish publics (diffuse cost-bearing and identity-locked compliance). The directionality derivation chain maps these declarations: beneficiaries get low d (subsidized by the constraint's operation), victims get high d (extracted from). The Israeli state apparatus sits ambiguously as enforcer with constrained exit; its directionality is moderated by its dual role as both coordinator and extractor.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope rather than snare prevents misreading genuine theological coordination as pure extraction: religious Zionist communities do solve real collective-action problems (community formation, resource pooling, ideological reproduction) through this framework. However, the simultaneous presence of active enforcement and identifiable victims blocks classification as rope. If the theological coordination function were entirely cover (no genuine community formation), it would compute as snare; if extraction vanished (no Palestinian displacement), it would compute as rope. The mandatrophy risk is obsolescence: the founding problem of Jewish vulnerability in a secular state may have been solved by Israel's military dominance, in which case the constraint persists by inertia and theater â the measurements show rising theater_ratio supporting this hypothesis, though the founding_problem_status remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_grounding_empirical_status,
    'Does the divine promise and messianic process describe an empirically real historical mechanism, or is it a theological construct whose social effects are real but whose referent is unverifiable?',
    'Historical-archaeological investigation of covenantal events is methodologically contested; resolution would require agreement on evidentiary standards between theological and secular historiography, which the constraint itself suppresses.',
    'If the grounding is purely theological, the constraint''s extraction cannot be justified by natural-law appeal and computes as tangled_rope or snare; if empirically real, it approaches mountain status for adherents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_grounding_empirical_status, conceptual, 'Whether the divine mandate is empirically verifiable or theological.').

omega_variable(
    territorial_maximalism_as_cover,
    'Does the religious restoration reading primarily coordinate a genuine communal theological mission, or does it function as ideological cover for territorial expansion and resource extraction?',
    'Comparative analysis of settlement resource flows versus theological community welfare; if gains concentrate in real-estate and state-contractor networks while theological costs are borne by the community, extraction dominates.',
    'If extraction dominates, classification shifts toward snare; if coordination dominates, it remains tangled_rope with asymmetric extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_maximalism_as_cover, empirical, 'Whether the coordination function or extraction function dominates.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem of Jewish statelessness been rendered obsolete by the establishment and military dominance of Israel, leaving the religious restoration narrative as a persistence mechanism?',
    'Examine whether the narrative''s urgency tracks actual existential threat or territorial opportunity; a decoupling indicates mandatrophy.',
    'If the founding problem is dead but the arrangement persists, the constraint exhibits piton characteristics with rising theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem is live or the constraint persists post-function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zionist_legitimacy_basis__religious_restoration_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zion_tr_t0, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(zion_tr_t10, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(zion_tr_t20, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(zion_tr_t30, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(zion_tr_t40, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 40, 0.45).
narrative_ontology:measurement(zion_tr_t55, zionist_legitimacy_basis__religious_restoration_reading, theater_ratio, 55, 0.48).

% Extraction over time
narrative_ontology:measurement(zion_be_t0, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(zion_be_t10, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(zion_be_t20, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(zion_be_t30, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 30, 0.7).
narrative_ontology:measurement(zion_be_t40, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(zion_be_t55, zionist_legitimacy_basis__religious_restoration_reading, base_extractiveness, 55, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(zion_su_t0, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(zion_su_t10, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(zion_su_t20, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(zion_su_t30, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(zion_su_t40, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(zion_su_t55, zionist_legitimacy_basis__religious_restoration_reading, suppression_requirement, 55, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zionist_legitimacy_basis__religious_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, national_liberation_reading).
narrative_ontology:affects_constraint(zionist_legitimacy_basis__religious_restoration_reading, settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the zionist_legitimacy_basis kernel. It is decomposed from national_liberation_reading and settler_colonial_reading per the Îµ-invariance principle: each reading has a distinct Îµ, beneficiary/victim structure, and classification. Religious restoration claims higher Îµ due to theological supremacy and territorial maximalism; national liberation claims lower Îµ as defensive coordination; settler_colonial claims high Îµ as pure extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
