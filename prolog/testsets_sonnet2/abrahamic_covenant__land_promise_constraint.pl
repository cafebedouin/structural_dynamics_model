% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Territorial Covenant Reading — Land of Canaan as Perpetual Grant
 *   domain: religious_studies/geopolitical
 *
 * SUMMARY:
 *   This constraint isolates one specific reading within the broader
 *   Abrahamic-covenant kernel: the claim that the Genesis territorial grant
 *   of the Land of Canaan is unconditional, perpetual, and operative today,
 *   such that it can serve as a legitimating warrant for contemporary state
 *   territorial claims. This is distinct from — though it interacts with —
 *   the isaac_covenant_reading and ishmael_covenant_reading, which concern
 *   covenant LINEAGE (who inherits the covenant) rather than covenant SCOPE
 *   (what the covenant grants and for how long). The land_promise_constraint
 *   is authored here as its own ε-stable claim: the standing arrangement
 *   under contest is the operationalization of the perpetual-grant reading
 *   into 20th- and 21st-century territorial policy, assessed by this
 *   reading's own lights (a literalist-perpetual theological claim used as
 *   political warrant), not by the conditional-reading alternative it
 *   displaces. ε is high because, once adopted by state and settlement
 *   institutions, the reading has functioned as a durable justificatory layer
 *   beneath policies with severe, concentrated material consequences for a
 *   specific population.
 *
 * KEY AGENTS:
 *   - state_actors_claiming_covenant_legitimacy: institutional agenda-setter, operationalizes the reading into land policy
 *   - settlement_movement_institutions: organized beneficiary, receives land access and legal cover
 *   - displaced_palestinian_communities: powerless, trapped payer bearing displacement and loss
 *   - palestinian_residents_under_permit_regimes: powerless payer bearing daily administrative constraint
 *   - religious_communities_holding_conditional_reading: excluded internal theological dissent
 *   - international_legal_bodies: analytical observer applying a non-theological legal standard
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.81).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.76).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.81).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.84).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Territorial Covenant Reading — Land of Canaan as Perpetual Grant").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious_studies/geopolitical").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, '330e0ddb-df52-43aa-81ea-1f6b1d3ed164').
narrative_ontology:cs_kernel_codification('330e0ddb-df52-43aa-81ea-1f6b1d3ed164', fixed_text).
narrative_ontology:cs_authority_grounding('330e0ddb-df52-43aa-81ea-1f6b1d3ed164', lineage).
narrative_ontology:cs_interpretation_layer_present('330e0ddb-df52-43aa-81ea-1f6b1d3ed164').
narrative_ontology:cs_reading_relation('330e0ddb-df52-43aa-81ea-1f6b1d3ed164', abrahamic_covenant__isaac_covenant_reading, influences).
narrative_ontology:cs_reading_relation('330e0ddb-df52-43aa-81ea-1f6b1d3ed164', abrahamic_covenant__ishmael_covenant_reading, influences).
narrative_ontology:cs_axiom('330e0ddb-df52-43aa-81ea-1f6b1d3ed164', foundational, territorial_grant_is_unconditional_and_perpetual).
narrative_ontology:cs_axiom_status(territorial_grant_is_unconditional_and_perpetual, holdable).
narrative_ontology:cs_axiom_grounding('330e0ddb-df52-43aa-81ea-1f6b1d3ed164', territorial_grant_is_unconditional_and_perpetual, theological).
narrative_ontology:cs_axiom('330e0ddb-df52-43aa-81ea-1f6b1d3ed164', secondary, covenant_land_promise_licenses_present_day_political_claim).
narrative_ontology:cs_axiom_status(covenant_land_promise_licenses_present_day_political_claim, holdable).
narrative_ontology:cs_axiom_grounding('330e0ddb-df52-43aa-81ea-1f6b1d3ed164', covenant_land_promise_licenses_present_day_political_claim, instrumental).
narrative_ontology:cs_reference_frame('330e0ddb-df52-43aa-81ea-1f6b1d3ed164', unconditional_perpetual_territorial_grant).
narrative_ontology:cs_drift_state('330e0ddb-df52-43aa-81ea-1f6b1d3ed164', post_1967_state_operationalization, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('330e0ddb-df52-43aa-81ea-1f6b1d3ed164', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, state_actors_claiming_covenant_legitimacy).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, settlement_movement_institutions).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, displaced_palestinian_communities).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_residents_under_permit_regimes).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, territorial_promise_is_literal_and_perpetual).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, covenant_grant_survives_diaspora).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invokes the territorial-grant reading of the covenant in political rhetoric, land-use policy, and legal argument for settlement expansion and annexation claims. Administers permitting, zoning, and security regimes that operationalize the reading into facts on the ground. Can shift the intensity of invocation depending on diplomatic context — this flexibility is itself a form of exit unavailable to those governed by the resulting policy.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, state_actors_claiming_covenant_legitimacy, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, state_actors_claiming_covenant_legitimacy, beneficiary).

% Religious and political organizations that fund, plan, and populate settlements justified explicitly by the perpetual-grant reading. Receive land access, state subsidy, and legal cover flowing from the reading's institutional adoption. Retain mobility and legal recourse largely unavailable to the populations displaced by the settlements they establish.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, settlement_movement_institutions, beneficiary,
    organized, generational, mobile, regional).

% Communities whose land, homes, or agricultural access have been reallocated, restricted, or removed under policies citing or consistent with the territorial-grant reading. Bear demolition, displacement, and loss of livelihood. Have no standing within the religious-legal framework being invoked against them and no realistic exit from the territory whose status is contested.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, displaced_palestinian_communities, payer,
    powerless, generational, trapped, regional).

% Residents subject to movement, building, and water-access permits administered under a legal-political architecture that draws part of its legitimating narrative from the covenant reading. Navigate daily constraint on mobility and property use; formal appeal exists but operates within the same system asserting the underlying territorial claim.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_residents_under_permit_regimes, payer,
    powerless, biographical, trapped, local).

% Jewish, Christian, and other religious voices — including significant currents within rabbinic and theological tradition itself — who read the land promise as conditional on covenant fidelity, already fulfilled in specific historical settlements, or requiring ethical constraint on its exercise. Their reading exists in the tradition but carries little weight in the political and legal arenas where the perpetual-grant reading is operationalized.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_communities_holding_conditional_reading, excluded,
    moderate, generational, constrained, global).

% UN bodies, international courts, and human rights organizations that assess settlement activity and territorial claims against international law rather than against theological warrant. Issue findings and rulings that carry normative but limited enforcement weight against the state actors invoking the covenant reading.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__land_promise_constraint, state_actors_claiming_covenant_legitimacy).
narrative_ontology:fixing_cost_class(abrahamic_covenant__land_promise_constraint, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At its narrowest, the reading coordinates a religious-national community's self-understanding of continuity with ancestral land and provides a shared narrative anchor for return, settlement, and national identity after historical displacement (exile, persecution, statelessness).
% TRANSFER_FUNCTION: The reading, once operationalized through state and settlement institutions, moves land, water rights, freedom of movement, and residency security from Palestinian communities to Israeli state and settler institutions, justified in part by appeal to the covenant's territorial clause treated as literal and unconditional.
% ABSENT_VOICES: Palestinian communities affected by land policy have no standing within the theological argument being made about them or their land. Conditional-reading religious voices within Judaism and Christianity — who would dispute the perpetual/unconditional framing on textual and ethical grounds — are marginal to the political operationalization even though they are present within the tradition itself.
% DISAPPEARANCE_RATIONALE: If the perpetual-grant reading lost its legitimating force in policy and public argument overnight, the specific justificatory architecture behind settlement expansion, annexation claims, and certain permit regimes would lose a central pillar — forcing those policies to stand or fall on secular security, historical, or legal arguments alone. Settlement institutions would lose access to a powerful mobilizing narrative; displaced communities' claims would no longer be met with a theological rebuttal that sits outside ordinary legal contestation.
% FOUNDING_PROBLEM: The Genesis narrative was composed, in its final redacted form, substantially amid or after the experience of exile — the land promise functioned as a theological answer to landlessness, covenant fidelity, and communal survival, not as a real-estate title deed enforceable in perpetuity by a future nation-state.
% FOUNDING_PROBLEM_CORROBORATION: Biblical scholars and historians of ancient Israel (working from textual-critical and archaeological method, outside both the Israeli state apparatus and Palestinian advocacy organizations) largely attest that the land-promise texts reflect theological and identity-consolidation functions tied to the exilic and post-exilic communities, not a claim of permanent unconditional territorial title. Religious authorities within conditional-reading traditions corroborate a fulfillment- or condition-based reading from inside the tradition itself. State actors and settlement institutions, who are the primary beneficiaries of the perpetual-unconditional reading, are the main voices asserting founding-problem status as still live and literally binding — this is exactly the corroboration gap the covenant-reading dispute turns on.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.81) reflects the material severity of land loss, displacement, and movement restriction attributable in significant part to policies whose justificatory architecture includes this reading — not merely rhetorical cost but concrete transfer of land, water access, and residency security. Suppression (0.76) is high because the arrangement is maintained through active legal, administrative, and at times military enforcement of the territorial claims the reading underwrites — checkpoints, permit systems, demolition orders, zoning law. Theater ratio (0.42) is moderate: the theological argument is not purely decorative (it genuinely shapes settler motivation and some policy design) but a growing share of its public invocation functions as legitimating cover for decisions increasingly driven by security, demographic, and strategic considerations. Accessibility collapse (0.58) is moderate rather than near-total — the conditional reading remains textually and theologically available and is actively argued by scholars and clergy — but political and legal accessibility to that alternative reading has collapsed substantially within the operative policy apparatus. Resistance (0.84) is high: this is among the most actively, persistently, and internationally contested constraints in the corpus, resisted by displaced communities, international bodies, and dissenting religious voices alike.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting state seat, the reading functions as continuity with a founding national narrative — coordination of identity and historical justice after exile and persecution. From the payer seats, the identical structure operates as an active, enforced mechanism of dispossession dressed in theological language that forecloses ordinary legal and political contestation. The engine should register sharp seat divergence: the beneficiary seats plausibly compute toward rope/tangled_rope framing (a coordinating national narrative), while payer seats compute toward snare (extraction with no coordination benefit to them and no meaningful voice in the framework used against them).
 *
 * DIRECTIONALITY LOGIC:
 *   State actors and settlement institutions are declared beneficiaries because they demonstrably gain land, legal standing, and political capital from the reading's persistence — low d, near the full-beneficiary end. Displaced and permit-regime Palestinian communities are declared victims because the same structure transfers land, water, and mobility away from them with no reciprocal benefit and no exit from the territory in question — high d, near the full-target end, reinforced by trapped exit_options. Religious dissenters are excluded rather than victimized in the strict material sense — they bear reputational and communal marginalization, not the primary material extraction, so they sit as excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem — theological answer to exile and landlessness — is contested as to whether it remains live. Corroboration from outside the beneficiary set (biblical scholarship, conditional-reading religious authorities) supports a 'dead as literal title, alive as identity narrative' reading; the mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges is itself diagnostic — the arrangement's material entrenchment has outpaced and now exceeds whatever function the founding narrative may still serve, which is precisely the capture/zombie signature the R5 interview is designed to surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conditional_vs_unconditional_textual_warrant,
    'Does the Genesis text itself support an unconditional perpetual land grant, or does the broader Torah narrative (including conquest-conditionality passages, exile as covenant breach, and prophetic conditionality) support a conditional or already-fulfilled reading?',
    'Textual-critical and canonical analysis of the full covenant cycle (Genesis through Deuteronomy and the Prophets) by scholars working outside both Israeli state institutions and Palestinian advocacy organizations, assessing whether conditionality clauses attach to the land promise specifically.',
    'If the conditional reading is textually dominant, the perpetual-grant reading used in policy is a selective theological construction rather than a straightforward transmission of the source text, strengthening the case that this constraint is extraction wearing genealogical cover rather than a natural reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditional_vs_unconditional_textual_warrant, conceptual, 'Whether the perpetual-unconditional reading is the textually dominant reading or a selected minority reading elevated for political use.').

omega_variable(
    kernel_reading_independence,
    'Is the land_promise_constraint''s territorial-scope question genuinely independent of the isaac_covenant_reading/ishmael_covenant_reading lineage question, or does a lineage determination (who inherits the covenant) necessarily settle or heavily weight the land-scope question?',
    'Comparative analysis across the three sibling readings: does any tradition hold an inclusive-lineage view (Ishmael included) while ALSO holding a perpetual-unconditional land-grant view, or does lineage exclusivity and land-grant perpetuity travel together in practice?',
    'If lineage and land-scope are empirically coupled in how traditions actually hold them, the three-way decomposition understates the interaction; if they vary independently (as the framework assumes per Rule 1), the decomposition is structurally sound and each constraint''s ε remains stable on its own.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_independence, conceptual, 'Whether covenant-lineage and covenant-land-scope readings are structurally independent or empirically coupled across traditions.').

omega_variable(
    state_actor_sincerity_vs_instrumentalization,
    'To what extent do state and settlement institutions actually hold the perpetual-grant reading as sincere theological conviction versus instrumentalizing it as post-hoc legitimation for demographic, security, or strategic land policy?',
    'Comparative analysis of policy justification across contexts where theological language is and is not invoked for functionally similar land actions (e.g., security-only justified settlements vs. explicitly covenant-justified ones), and internal policy documents where available.',
    'If largely instrumentalized, theater_ratio is understated and the reading functions closer to pure cover story (pushing the classification further toward snare); if substantially sincere among a meaningful subset of beneficiary institutions, the coordination-function claim carries more genuine weight for that subset.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_actor_sincerity_vs_instrumentalization, empirical, 'Whether the theological warrant is sincerely held or primarily instrumentalized by beneficiary institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t1948, abrahamic_covenant__land_promise_constraint, theater_ratio, 1948, 0.2).
narrative_ontology:measurement_basis(abra_tr_t1948, observed).
narrative_ontology:measurement(abra_tr_t1967, abrahamic_covenant__land_promise_constraint, theater_ratio, 1967, 0.26).
narrative_ontology:measurement_basis(abra_tr_t1967, observed).
narrative_ontology:measurement(abra_tr_t1980, abrahamic_covenant__land_promise_constraint, theater_ratio, 1980, 0.31).
narrative_ontology:measurement_basis(abra_tr_t1980, observed).
narrative_ontology:measurement(abra_tr_t1995, abrahamic_covenant__land_promise_constraint, theater_ratio, 1995, 0.35).
narrative_ontology:measurement_basis(abra_tr_t1995, observed).
narrative_ontology:measurement(abra_tr_t2010, abrahamic_covenant__land_promise_constraint, theater_ratio, 2010, 0.39).
narrative_ontology:measurement_basis(abra_tr_t2010, observed).
narrative_ontology:measurement(abra_tr_t2024, abrahamic_covenant__land_promise_constraint, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(abra_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(abra_be_t1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement_basis(abra_be_t1948, observed).
narrative_ontology:measurement(abra_be_t1967, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1967, 0.58).
narrative_ontology:measurement_basis(abra_be_t1967, observed).
narrative_ontology:measurement(abra_be_t1980, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1980, 0.66).
narrative_ontology:measurement_basis(abra_be_t1980, observed).
narrative_ontology:measurement(abra_be_t1995, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1995, 0.71).
narrative_ontology:measurement_basis(abra_be_t1995, observed).
narrative_ontology:measurement(abra_be_t2010, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2010, 0.77).
narrative_ontology:measurement_basis(abra_be_t2010, observed).
narrative_ontology:measurement(abra_be_t2024, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2024, 0.81).
narrative_ontology:measurement_basis(abra_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1948, 0.4).
narrative_ontology:measurement_basis(abra_su_t1948, observed).
narrative_ontology:measurement(abra_su_t1967, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1967, 0.55).
narrative_ontology:measurement_basis(abra_su_t1967, observed).
narrative_ontology:measurement(abra_su_t1980, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1980, 0.63).
narrative_ontology:measurement_basis(abra_su_t1980, observed).
narrative_ontology:measurement(abra_su_t1995, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement_basis(abra_su_t1995, observed).
narrative_ontology:measurement(abra_su_t2010, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement_basis(abra_su_t2010, observed).
narrative_ontology:measurement(abra_su_t2024, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2024, 0.76).
narrative_ontology:measurement_basis(abra_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__land_promise_constraint, 0.08).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, ishmael_covenant_reading).

% DUAL FORMULATION NOTE:
% This story is the territorial-scope member of a three-story kernel decomposition of the abrahamic_covenant kernel. isaac_covenant_reading and ishmael_covenant_reading concern covenant LINEAGE (who inherits the covenant relationship); this story concerns covenant SCOPE (what the covenant grants territorially and for how long). The three are linked because lineage readings are frequently invoked alongside land-scope readings in political argument (e.g., exclusive-lineage claims reinforcing exclusive-land claims), but each carries its own independent ε: the lineage readings are primarily contested at the level of religious identity and doctrinal legitimacy (moderate ε), while this land-scope reading, once operationalized into state policy, carries substantially higher ε due to its direct material consequences for displaced populations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
