% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Historical Treaties as Nation-to-Nation Contracts (Sovereignty Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint models treaties as binding nation-to-nation contracts
 *   between sovereign equals, operating under modern treaty law principles
 *   that require ongoing consent and prohibit unilateral resource extraction
 *   without renegotiation. The nation-to-nation reading is one interpretation
 *   of a historically contested kernel (the status and force of historical
 *   treaties between settler states and Indigenous nations). In this reading,
 *   treaties create enforceable obligations on both parties; unilateral
 *   resource extraction becomes a treaty violation; Indigenous nations retain
 *   decision-making power over territorial changes; and consent is an ongoing
 *   requirement, not a one-time historical event. This reading directly
 *   opposes the extinguishment reading (which treats settlement as
 *   terminating Indigenous sovereignty) and distinguishes itself from the
 *   stewardship reading (which treats the settler state as a trustee with
 *   fiduciary rather than coordinate obligations). The constraint exhibits
 *   tangled rope structure: genuine coordination exists (resource-sharing
 *   protocols, jurisdictional boundaries, dispute resolution) alongside
 *   asymmetric extraction (settler states extract resources and reinterpret
 *   treaties despite nation-to-nation language). Theater ratio decline over
 *   the measurement interval reflects increasing indigenous organizational
 *   capacity and international treaty law enforcement, reducing the
 *   performative content of settler-state treaty administration.
 *
 * KEY AGENTS:
 *   - Indigenous Nations: Primary beneficiary (powerless/trapped historically, moderate/constrained contemporary) — regain sovereignty recognition, veto power over resource extraction, legal standing in international courts
 *   - Settler State (Institutional): Primary beneficiary of extraction until constrained by the nation-to-nation reading (institutional/arbitrage) — retains resource access and fiscal flexibility if treaties are reinterpreted; loses both if nation-to-nation reading is enforced
 *   - Treaty Rights Holders (Contemporary): Secondary beneficiary (moderate/constrained) — individuals and communities whose livelihoods depend on treaty protections; gain legal standing but remain materially dependent on state enforcement
 *   - Settler State Fiscal Interests: Victim of the constraint (no independent agent; beneficiary set item representing resource-extraction claims) — resource extraction becomes subject to nation consent, reducing state unilateral control
 *   - International Treaty Law Community: Organized actor (organized/mobile) — arXiv of international law; creates scaffold through ILO 169, UNDRIP, regional human rights courts that enforce the nation-to-nation reading
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the nation-to-nation reading as inherent to sovereignty itself, missing that it is a contingent institutional achievement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.38).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.52).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Historical Treaties as Nation-to-Nation Contracts (Sovereignty Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, 'be7b32b4-6957-490d-bab9-2f6c18116f65').
narrative_ontology:cs_kernel_codification('be7b32b4-6957-490d-bab9-2f6c18116f65', fixed_text).
narrative_ontology:cs_authority_grounding('be7b32b4-6957-490d-bab9-2f6c18116f65', lineage).
narrative_ontology:cs_interpretation_layer_present('be7b32b4-6957-490d-bab9-2f6c18116f65').
narrative_ontology:cs_reading_relation('be7b32b4-6957-490d-bab9-2f6c18116f65', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('be7b32b4-6957-490d-bab9-2f6c18116f65', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('be7b32b4-6957-490d-bab9-2f6c18116f65', foundational, treaty_perpetual_sovereign_contract).
narrative_ontology:cs_axiom_status(treaty_perpetual_sovereign_contract, holdable).
narrative_ontology:cs_axiom_grounding('be7b32b4-6957-490d-bab9-2f6c18116f65', treaty_perpetual_sovereign_contract, conventional).
narrative_ontology:cs_axiom('be7b32b4-6957-490d-bab9-2f6c18116f65', foundational, indigenous_sovereignty_retained_by_treaty).
narrative_ontology:cs_axiom_status(indigenous_sovereignty_retained_by_treaty, holdable).
narrative_ontology:cs_axiom_grounding('be7b32b4-6957-490d-bab9-2f6c18116f65', indigenous_sovereignty_retained_by_treaty, deontological).
narrative_ontology:cs_reference_frame('be7b32b4-6957-490d-bab9-2f6c18116f65', mutual_sovereign_obligation_framework).
narrative_ontology:cs_drift_state('be7b32b4-6957-490d-bab9-2f6c18116f65', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('be7b32b4-6957-490d-bab9-2f6c18116f65', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, treaty_rights_holders).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_fiscal_flexibility).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_claims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPOSSESSED INDIGENOUS NATION (SNARE) — Structurally trapped by historical power asymmetry, yet the nation-to-nation reading constrains unilateral extraction by asserting treaty enforceability. The reading provides formal legal recourse but cannot override material powerlessness. The constraint binds the settler state but leaves the indigenous nation with constrained exit options (litigation, political organizing, but not material alternatives to territorial negotiation).
constraint_indexing:constraint_classification(historical_treaty_substrate__nation_to_nation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDIGENOUS NATION WITH TREATY RIGHTS (TANGLED ROPE) — Contemporary indigenous nations experience the constraint as genuinely mixed: treaties provide legal standing and enforceable protections against unilateral settler-state action, yet settlement and resource extraction continue through loopholes, reinterpretation, and administrative procedures that bypass treaty negotiation. The nation coordinates with the settler state (resource-sharing, jurisdiction), but extraction flows asymmetrically. This is the core reading perspective.
constraint_indexing:constraint_classification(historical_treaty_substrate__nation_to_nation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SETTLER STATE TREATY ADMINISTRATION (ROPE) — The settler state institution views treaties as coordination mechanisms: establishing jurisdiction boundaries, resource-sharing protocols, and legal standing for both parties. From the institutional perspective, the nation-to-nation reading is functional coordination with mutual obligation. The state has arbitrage options (reinterpret, renegotiate unilaterally through administrative procedure, challenge jurisdiction), but the treaty framework itself is experienced as enabling rather than extractive.
constraint_indexing:constraint_classification(historical_treaty_substrate__nation_to_nation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL TREATY LAW COMMUNITY (SCAFFOLD) — Global human rights frameworks, ILO conventions (especially 169), and UN Declaration on the Rights of Indigenous Peoples create scaffolding that enforces the nation-to-nation reading by embedding it in international obligations. These frameworks are temporary structural support (sunset as indigenous sovereignty becomes normalized) that shift power toward indigenous nations by making unilateral settler-state action a treaty violation under multiple international regimes.
constraint_indexing:constraint_classification(historical_treaty_substrate__nation_to_nation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the nation-to-nation reading appears as a natural law of sovereignty: when two sovereign entities make a binding commitment, that commitment persists until mutually rescinded or the commitment is materially impossible to perform. This perspective sees treaty enforceability as logically entailed by the concept of sovereignty itself — if treaties can be unilaterally violated, sovereignty is illusory. However, the structural data contradicts this: the settler state's extractive power and the indigenous nation's constrained exit options are contingent institutional facts, not natural laws. The engine's false summit detector will identify this.
constraint_indexing:constraint_classification(historical_treaty_substrate__nation_to_nation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(historical_treaty_substrate__nation_to_nation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(historical_treaty_substrate__nation_to_nation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The nation-to-nation reading constrains unilateral extraction by requiring Indigenous consent for resource projects and territorial changes. However, extraction is not eliminated — settler states maintain fiscal advantage, administrative procedures can circumvent treaty language, and Indigenous nations lack material capacity to prevent all resource access. The extractiveness trajectory (0.62 → 0.38) models historical decline as international treaty law and indigenous organizing capacity increase. Suppression (0.52): Moderate. Significant barriers to enforcement include settler-state administrative machinery (reinterpretation of treaties through regulation, jurisdiction claims, capacity arguments), material resource dependence of indigenous communities on state infrastructure, and asymmetric litigation capacity. These are real suppressive forces, but not insurmountable — international courts, domestic litigation, and political organizing have achieved treaty enforcement outcomes. Theater ratio (0.48): Moderate. The nation-to-nation reading generates genuine coordination (resource-sharing negotiations, jurisdictional protocols) but includes performative elements (settler states claim to honor treaties while extracting resources through loopholes; indigenous nations must perform 'reasonable nation' compliance to maintain legitimacy in international forums). The ratio decline reflects that contemporary indigenous organizing increasingly bypasses performative frameworks, using direct action and international publicity rather than respectful petition.
 *
 * PERSPECTIVAL GAP:
 *   The settler state (institutional/arbitrage/immediate) experiences the constraint as coordination with an exit option — renegotiate, reinterpret, claim changed circumstances. The indigenous nation (moderate/constrained/biographical to generational) experiences the constraint as binding obligation with limited exit — can litigate, organize, appeal internationally, but cannot materially force the settler state to honor treaties without capacity. The gap is a function of power asymmetry, not logical disagreement. Both parties can affirm the nation-to-nation reading in principle while experiencing it differently in practice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: who benefits from treaty enforceability, who bears costs of constraint. Indigenous nations as beneficiaries of the constraint's coordination function experience low-d directionality (the constraint subsidizes their sovereignty claims). Settler-state resource interests as victims of the constraint experience high-d directionality (the constraint extracts from unilateral extraction claims). The settler-state institution's arbitrage position produces low-d (they can renegotiate, reinterpret, claim changed circumstances), but the indigenous nation's constrained exit (can appeal to international law but cannot materially resist if the settler state violates) produces high-d. The settler-state institutional perspective shows rope because their power advantage is structural — they can coordinate with indigenous nations from a position of strength. The indigenous nation's tangled-rope reflects that they are both participants in genuine coordination and targets of extraction through the same institutional mechanism.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_enforceability_mechanism,
    'What enforces treaty obligations when the settler state has material power advantage? Are international courts, domestic litigation, or political organizing the actual enforcement mechanism?',
    'Historical analysis of treaty violations and consequences; examination of cases where indigenous nations successfully enforced treaty rights vs cases where extraction proceeded despite treaty language',
    'If enforcement is international law: the nation-to-nation reading is structurally robust (Tangled Rope from indigenous perspective remains stable). If enforcement is primarily political (organizing, publicity, coalition-building): the reading is contingent on indigenous organizational power and shifts toward Snare when power asymmetries are severe.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_enforceability_mechanism, empirical, 'Empirical enforcement mechanisms for treaty rights in settler-colonial contexts').

omega_variable(
    reading_vs_extinguishment_foreclosure,
    'Does the nation-to-nation reading logically foreclose the extinguishment reading, or do they coexist as competing frameworks held by different parties?',
    'Legal-historical analysis: can a single legal framework simultaneously hold that treaties are sovereign contracts (nation-to-nation) AND that indigenous title was extinguished by settlement (extinguishment)? Examine case law from jurisdictions that claim both.',
    'If forecloses: the readings are genuinely contradictory; settler states cannot legally hold both. If coexists: the readings occupy different institutional spaces (nation-to-nation in indigenous legal traditions, extinguishment in settler-state doctrine) and the contradition is an institutional feature, not a logical impossibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_extinguishment_foreclosure, conceptual, 'Whether nation-to-nation reading forecloses or coexists with extinguishment reading').

omega_variable(
    consent_content_ambiguity,
    'What constitutes ''consent'' in the nation-to-nation framework? Do historical treaties (signed under duress, with translation/interpretation disputes, before capacity to refuse was real) satisfy the modern consent requirement?',
    'Examination of contemporary treaty renegotiation and affirmation processes; analysis of how consent standards have shifted; comparison of historical vs modern treaty-making procedures',
    'If historical coerced consent counts: extractiveness remains high (state can claim ''we got consent, even if under duress''). If only modern affirmative consent counts: extractiveness drops and the nation-to-nation reading becomes more constraining on unilateral state action.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_content_ambiguity, conceptual, 'Definition of ''consent'' in historical treaty framework and its applicability to coerced historical agreements').

omega_variable(
    temporal_scope_of_obligations,
    'Are treaty obligations perpetual or do they expire after a specified term or change of circumstances? Does the nation-to-nation reading imply indefinite state constraint or time-bounded obligation?',
    'Treaty law analysis: examination of rebus sic stantibus doctrine and modern interpretation; survey of indigenous-nation positions on treaty perpetuity vs settler-state reinterpretation claims',
    'If perpetual: the constraint is durable and the nation-to-nation reading is robust. If time-bounded or circumstance-dependent: settler states have exit paths and extractiveness increases as states claim changed circumstances justify extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_scope_of_obligations, conceptual, 'Temporal scope and perpetuity of treaty obligations in nation-to-nation framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_treaty_tr_t0, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0, 0.68).
narrative_ontology:measurement(hist_treaty_tr_t50, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 50, 0.55).
narrative_ontology:measurement(hist_treaty_tr_t100, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 100, 0.48).

% Extraction over time
narrative_ontology:measurement(hist_treaty_be_t0, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(hist_treaty_be_t50, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(hist_treaty_be_t100, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 100, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(hist_treaty_su_t0, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(hist_treaty_su_t50, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement(hist_treaty_su_t100, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% The historical_treaty_substrate kernel is interpreted via three structurally distinct constraint stories. The nation-to-nation reading treats treaties as sovereign contracts; the extinguishment reading treats them as historical artifacts; the stewardship reading treats them as fiduciary relationships. Each instantiation has its own ε value and perspectives. The nation-to-nation reading (this file) has ε=0.38 and genuine coordination function (Tangled Rope). The extinguishment reading has higher ε (extraction without coordination function) and lower beneficiary standing for indigenous nations. The stewardship reading distributes beneficiary status differently (settler state as trustee retains some control). All three are linked via network.affects_constraints because they share a kernel (the status of treaties) and compete for institutional adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__nation_to_nation_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
