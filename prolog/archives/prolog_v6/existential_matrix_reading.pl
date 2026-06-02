% ============================================================================
% CONSTRAINT STORY: existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_existential_matrix_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: existential_matrix_reading
 *   human_readable: Existential Matrix Reading: Territorial Sovereignty as Zero-Sum Survival Conflict
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the existential matrix reading of the
 *   territorial sovereignty legitimacy kernel. Under this reading,
 *   sovereignty legitimacy is not a juridical or historical question but an
 *   existential one: each people's survival and identity expression requires
 *   territorial control as an irreducible precondition. The reading does not
 *   argue about who owns which land, but that the *logic of ownership itself*
 *   is secondary to the logic of survival. Territorial compromise frameworks
 *   (two-state solutions, confederal arrangements, shared sovereignty models)
 *   are structurally unstable under this reading's premises because they
 *   require both parties to accept residual territorial vulnerability, which
 *   contradicts the existential survival requirement. The conflict becomes
 *   fundamentally zero-sum: one side's security gain is the other's security
 *   loss. The beneficiary is whichever faction achieves demographic or
 *   military dominance sufficient to reduce its own vulnerability below the
 *   threshold that triggers the existential frame in the other faction. The
 *   constraint exhibits snare characteristics for all parties trapped within
 *   the frame (neither can exit without accepting unacceptable vulnerability)
 *   and piton characteristics for compromise institutions (performing
 *   negotiation theater while their functional goal is structurally
 *   foreclosed by the reading's own premises). The extractiveness rises over
 *   the time interval (0.42 → 0.78) as military/demographic asymmetries
 *   accumulate and one faction transitions from trapped to dominant.
 *   Theater_ratio remains moderate-high (0.35 → 0.62) because diplomatic and
 *   legal frameworks continue despite their structural futility within the
 *   existential frame.
 *
 * KEY AGENTS:
 *   - Territorial Minority: Powerless agents trapped by symmetrical existential logic (any compromise requires accepting vulnerability). Primary victims of the constraint's zero-sum structure.
 *   - Demographic Majority: Initially trapped by symmetrical logic but gradually becomes dominant beneficiary as military/demographic asymmetries favor this faction. Extractiveness becomes one-directional.
 *   - Compromise Architects (UN, mediation bodies, two-state frameworks): Institutional actors maintaining performative commitment to settlement while the reading's logic forecloses their functional goal. Primary targets of extraction through legitimation and institutional relevance.
 *   - International Mediators: Moderate-power third parties constrained by the framework's impossibility. Extract value through appearance of neutrality while unable to produce settlement.
 *   - Great Powers: Institutional beneficiaries through strategic arbitrage — sustain the conflict as geopolitical leverage while maintaining plausible non-involvement.
 *   - Analytical Observer: Risk naturalizing a contingent political doctrine as an immutable human-existential law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(existential_matrix_reading, 0.78).
domain_priors:suppression_score(existential_matrix_reading, 0.85).
domain_priors:theater_ratio(existential_matrix_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(existential_matrix_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(existential_matrix_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(existential_matrix_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(existential_matrix_reading, snare).
narrative_ontology:human_readable(existential_matrix_reading, "Existential Matrix Reading: Territorial Sovereignty as Zero-Sum Survival Conflict").
narrative_ontology:topic_domain(existential_matrix_reading, "political_theory/international_relations/territorial_sovereignty").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(existential_matrix_reading, '34ea59ac-e447-4bff-9f38-112460d7b58b').
narrative_ontology:cs_created_at('34ea59ac-e447-4bff-9f38-112460d7b58b', '').
narrative_ontology:cs_kernel_codification('34ea59ac-e447-4bff-9f38-112460d7b58b', distributed).
narrative_ontology:cs_authority_grounding('34ea59ac-e447-4bff-9f38-112460d7b58b', extraction).
narrative_ontology:cs_kernel_id(existential_matrix_reading, territorial_sovereignty_legitimacy).
narrative_ontology:cs_reading_relation('34ea59ac-e447-4bff-9f38-112460d7b58b', covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('34ea59ac-e447-4bff-9f38-112460d7b58b', self_determination_reading, forecloses).
narrative_ontology:cs_axiom('34ea59ac-e447-4bff-9f38-112460d7b58b', foundational, territorial_exclusivity_as_survival_precondition).
narrative_ontology:cs_axiom_status(territorial_exclusivity_as_survival_precondition, holdable).
narrative_ontology:cs_axiom_grounding('34ea59ac-e447-4bff-9f38-112460d7b58b', territorial_exclusivity_as_survival_precondition, empirically_contingent).
narrative_ontology:cs_axiom('34ea59ac-e447-4bff-9f38-112460d7b58b', foundational, security_asymmetry_forecloses_settlement).
narrative_ontology:cs_axiom_status(security_asymmetry_forecloses_settlement, holdable).
narrative_ontology:cs_axiom_grounding('34ea59ac-e447-4bff-9f38-112460d7b58b', security_asymmetry_forecloses_settlement, empirically_contingent).
narrative_ontology:cs_reference_frame('34ea59ac-e447-4bff-9f38-112460d7b58b', symmetric_existential_threat).
narrative_ontology:cs_drift_state('34ea59ac-e447-4bff-9f38-112460d7b58b', contemporary_asymmetric_advantage, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(existential_matrix_reading, demographic_military_dominant_faction).
narrative_ontology:constraint_victim(existential_matrix_reading, territorial_minority_population).
narrative_ontology:constraint_victim(existential_matrix_reading, international_compromise_frameworks).
narrative_ontology:constraint_victim(existential_matrix_reading, neutral_third_parties).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TERRITORIAL MINORITY (SNARE) — Powerless agents trapped within the existential security frame. From this reading's logic, any compromise that requires accepting territorial vulnerability contradicts the existential survival requirement. Exit is logically impossible within the framework — accepting vulnerability = accepting group extinction risk. Maximum extraction: the reading's own premises preclude any settlement that would be acceptable to this agent.
constraint_indexing:constraint_classification(existential_matrix_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DEMOGRAPHIC MAJORITY (SNARE, GENERATIONAL) — Initially trapped by symmetrical existential logic (both sides claim territorial control as survival necessity). But asymmetric: if demographic trends favor one faction, the logic shifts — this faction gradually transitions from trapped to dominant beneficiary. Extraction becomes one-directional as military/demographic asymmetries accumulate. The snare classification persists because the zero-sum frame forecloses voluntary exit for either party.
constraint_indexing:constraint_classification(existential_matrix_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: COMPROMISE ARCHITECT / TWO-STATE INSTITUTIONS (PITON) — UN mandates, international mediation bodies, and two-state solution frameworks maintain performative commitment to compromise despite facing structural foreclosure from the existential matrix reading itself. These institutions persist through inertia — the reading's logic makes their functional goal (negotiated partition) structurally unstable. Theater_ratio high because institutions continue diplomatic theater while the underlying premise (existential threat requiring absolute control) renders negotiated compromise impossible. Theater rises as failure accumulates but institutions persist.
constraint_indexing:constraint_classification(existential_matrix_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL MEDIATOR (SNARE) — Moderate-power third parties attempting mediation face the constraint as binding on both sides. Cannot negotiate away the existential premise from either party. Their efforts extract value (legitimation, funding, institutional relevance) while structurally unable to produce the settlement they ostensibly pursue. Extraction directed toward the mediators themselves through the appearance of neutrality.
constraint_indexing:constraint_classification(existential_matrix_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: GREAT POWERS / STRATEGIC ARBITRAGE (TANGLED ROPE) — Institutional actors with military/economic stakes in the region experience genuine coordination (proxy conflicts, arms markets, alliance maintenance) overlaid on extraction (sustaining the conflict as leverage against rivals, preventing regional hegemon, controlling strategic territory). Arbitrage options mean these actors can exit at minimal cost — the constraint benefits them through its continuation. Mixed rope-and-snare: rope for the coordination function they maintain, snare for the perpetual extraction of geopolitical advantage.
constraint_indexing:constraint_classification(existential_matrix_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational timescale, the reading claims that existential security requirements are immutable: every people requires territorial control; territorial compromise = existential vulnerability; therefore zero-sum conflict is an unchangeable feature of the human condition. This perspective presents the extraction as following naturally from biological/existential imperatives rather than constructed institutional arrangements. FALSE SUMMIT CANDIDATE: identifiable beneficiaries (dominant demographic faction, great power actors) and theatrical performance (compromise institutions) suggest this is a contingent political reading, not a natural law.
constraint_indexing:constraint_classification(existential_matrix_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(existential_matrix_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(existential_matrix_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(existential_matrix_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(existential_matrix_reading, TR),
    TR >= 0.70.

:- end_tests(existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High and rising. The reading's zero-sum logic produces asymmetric extraction once demographic/military imbalances emerge. Initially symmetrical (both sides trapped equally), extractiveness is moderate (0.42). As one faction gains advantage, extraction becomes directional toward the dominant faction, rising to 0.78 by interval end. The reading creates rents for actors who benefit from perpetuating the zero-sum frame (military establishments, security industries, great powers maintaining regional leverage). Suppression (0.85): Very high. The existential frame forecloses psychological acceptance of compromise from any party within its logic. Additionally, institutional actors (security services, military hierarchies, ideological movements) have incentive to maintain the existential narrative because it justifies expanded control and resource allocation. Alternative framings (covenant continuity, self-determination) are suppressed through narrative dominance and institutional filtering. Theater_ratio (0.62): Moderate-high. Compromise institutions (two-state frameworks, international mediation) persist despite being structurally foreclosed by the existential reading. Diplomatic meetings, agreement negotiations, and legal frameworks are performed as if settlement is possible, while the underlying zero-sum logic makes compromise psychologically impossible for parties trapped within the frame. Theater rises as the gap between diplomatic performance and actual settlement possibility widens. Claimed_type (Snare): The zero-sum structure with no exit for trapped parties, combined with high suppression of alternatives, produces the snare classification. Neither trapped faction can exit without accepting existential vulnerability (which their own frame forbids). Great powers benefit through strategic extraction while maintaining deniability.
 *
 * PERSPECTIVAL GAP:
 *   The existential matrix reading generates maximum perspectival divergence across the six classification types. The territorial minority sees unambiguous snare (trapped by existential logic, no acceptable exit). The demographic majority sees snare with time-dependent asymmetry (initially symmetric, becoming extractive as advantage accumulates). Compromise architects see piton (their institutions persist through inertia while their functional goal is foreclosed). International mediators see snare (constrained by the impossibility of negotiated exit from the zero-sum frame). Great powers see tangled rope with strategic arbitrage (genuine coordination function in proxy conflict, overlaid on extraction of geopolitical leverage). The analytical observer risks seeing mountain (existential security as immutable human necessity) but the structural data reveals false summit: beneficiaries exist, theater is high, and alternative framings are suppressed rather than naturally foreclosed. The perspectival gap is not merely interpretive—it tracks the actual differentiation in extraction and constraint experience across structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives due to asymmetric power dynamics. Territorial minorities have d ≈ 0.95 (full victims/trapped) — the reading's logic extracts maximum vulnerability from these agents; f(d) ≈ 1.42. Demographic majorities transition from d ≈ 0.50 (symmetric trapped) to d ≈ 0.10 (dominant beneficiary with arbitrage) as military/demographic advantage accumulates. Compromise institutions have d ≈ 0.72 (analytical observer position) — they experience the constraint as an analytical impossibility rather than material extraction. Great powers have d ≈ 0.05 (full beneficiaries with arbitrage exit) — they can deploy the zero-sum frame to extract geopolitical advantage while disengaging. The perspectival gap is diagnostic: agents at different d values experience radically different classifications of the same constraint (snare vs. tangled rope vs. piton), revealing that the reading's logic produces differentiated extraction rather than neutral coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   HIGH EXTRACTIVENESS (0.78) RESOLVED: This reading embodies a classical mandatrophy resolution through structural asymmetry. The reading begins as a coherent coordination claim (all parties require territorial security; therefore territorial control is legitimate) but resolves into pure extraction once demographic/military asymmetries allow one faction to achieve dominance. The mandatrophy is resolved not by showing the reading is false, but by showing that its internal logic produces increasingly asymmetric outcomes: early in the interval, both parties are trapped symmetrically (coordination failure); later, one party transitions to dominant extraction while the other remains trapped. The resolution involves acknowledging that the reading's coherence at the coordination level (both need security) masks its extractive outcome at the asymmetry level (dominance makes one faction's security come at catastrophic cost to the other). The constraint thus exhibits high extractiveness despite containing genuine coordination content—the coordination (security provision) is available only to the dominant faction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_vs_political_framing,
    'Is the ''existential threat'' driving territorial conflict an objective condition (demographic/military vulnerability) or a constructed frame (identity narrative, historical grievance amplification, strategic threat exaggeration)?',
    'Comparative analysis of threat perception vs. actual military/demographic asymmetries; historical record of settlement periods (when conflict paused despite identical ''existential'' conditions); experimental evidence on narrative framing''s effect on territory valuation; agent testimony on whether compromise was psychologically acceptable before conflict escalation',
    'If objective condition: the existential matrix reading is validated — territorial conflict is zero-sum and compromise is unstable. If constructed frame: the reading is a contingent political doctrine, not a natural law — the constraint is a Tangled Rope or Snare of institutional design, not a Mountain of human nature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_vs_political_framing, empirical, 'Whether existential threat perception is objective condition or constructed narrative').

omega_variable(
    demographic_control_sufficiency,
    'Does territorial control actually provide existential security (group survival and identity preservation) or does it merely displace the vulnerability to different mechanisms (economic dependence, cultural assimilation, institutional capture)?',
    'Longitudinal study of groups that achieved territorial sovereignty: correlation between sovereignty and group extinction rates; analysis of mechanisms by which territories fail to prevent cultural dissolution (diaspora, mixed marriage, institutional fragmentation); ethnographic evidence from post-colonial states on whether territorial control delivered promised security',
    'If control is sufficient: existential matrix reading validated at the mechanism level. If control is displaced vulnerability: the reading misdiagnoses the actual threat — pursuing absolute territorial control generates new extraction mechanisms without removing the underlying vulnerability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_control_sufficiency, empirical, 'Whether territorial control provides existential security or displaces vulnerability').

omega_variable(
    settlement_structural_stability,
    'Are negotiated territorial compromises (two-state, cantons, confederal arrangements) inherently unstable because neither side can psychologically accept vulnerability, or are they unstable only under specific institutional conditions that could be reformed?',
    'Case studies of settlements that persisted vs. collapsed: correlation between stability and institutional provisions for security guarantees, third-party enforcement, graduated integration (vs. sharp boundaries), economic interdependence, supranational authority; analysis of whether collapsed settlements failed due to existential fear or due to remediable institutional design (weak enforcement, unstable guarantor commitment, perverse incentive structures)',
    'If inherently unstable: existential matrix reading is validated — no institutional reform can fix the zero-sum structure. If institutionally remediable: the reading diagnoses the correct problem (vulnerability) but offers the wrong solution (exclusive control rather than security architecture), misclassifying a Tangled Rope coordination problem as a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_structural_stability, empirical, 'Whether territorial compromises are structurally unstable or institutionally remediable').

omega_variable(
    alternative_security_mechanisms,
    'Do historical or contemporary cases exist where groups maintain collective identity and survival without requiring territorial exclusivity—through networked diaspora identity, supranational authority with minority protections, shared institutions, or distributed governance?',
    'Ethnographic and historical survey of groups that maintained cohesion and identity without territorial exclusivity (Jewish diaspora, European supranational integration, networked migrant communities, religious transnational orders); longitudinal data on whether these groups show higher or lower extinction/assimilation rates than territorially exclusive groups; agent testimony on psychological security in non-territorial arrangements',
    'If such cases are robust and psychologically sustainable: existential matrix reading is falsified — alternative security mechanisms exist that don''t require zero-sum territorial control. If such cases are rare or psychologically unstable: existential matrix reading gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_security_mechanisms, empirical, 'Existence of stable non-territorial security mechanisms for group survival').

omega_variable(
    reading_foreclosure_mechanism,
    'Does this existential matrix reading logically foreclose the covenant continuity and self-determination readings, or can all three coexist as different framings of the same conflict held by different parties and observers?',
    'Logical analysis of the core premises: does existential security necessarily contradict covenant obligation or self-determination right, or can both be affirmed within a single framework (e.g., ''our covenant requires territorial security as precondition for fulfilling obligations''; ''our self-determination includes the right to secure territory'')? Evidence from actors who hold multiple readings simultaneously.',
    'If the reading forecloses siblings: the three readings are mutually exclusive — describing the conflict requires choosing one framework. If they coexist: the conflict is one object read through multiple incommensurable lenses, and no single reading fully captures the structure. Affects how the engine routes foreclosure logic in the commitment system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_mechanism, conceptual, 'Whether existential matrix reading logically forecloses sibling readings').

omega_variable(
    temporal_reversion_to_covenant,
    'If the existential matrix reading loses empirical grounding (demographic parity restored, military balance stabilizes, supranational authority provides credible security guarantees), would the same parties revert to covenant continuity or self-determination framings, or does the existential frame become self-sustaining through institutional inertia and historical trauma?',
    'Historical cases where existential framing was deployed then abandoned; interviews with agents about conditions under which they would reframe; analysis of whether reversion to alternative readings is prevented by sunk costs in military/demographic competition, institutional capture by security apparatuses, or trauma narratives that persist independent of current material conditions',
    'If reversion is possible under changed conditions: existential matrix reading is contingent on specific historical moments, not universal. If self-sustaining: reading becomes piton-like after initial period — maintained through inertia even when empirical conditions change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_reversion_to_covenant, empirical, 'Whether existential framing is reversible or self-sustaining under changed conditions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(existential_matrix_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exis_tr_t0, existential_matrix_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(exis_tr_t3, existential_matrix_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(exis_tr_t6, existential_matrix_reading, theater_ratio, 6, 0.6).
narrative_ontology:measurement(exis_tr_t10, existential_matrix_reading, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(exis_be_t0, existential_matrix_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(exis_be_t3, existential_matrix_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(exis_be_t6, existential_matrix_reading, base_extractiveness, 6, 0.71).
narrative_ontology:measurement(exis_be_t10, existential_matrix_reading, base_extractiveness, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(existential_matrix_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(existential_matrix_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(existential_matrix_reading, self_determination_reading).

% DUAL FORMULATION NOTE:
% The territorial sovereignty legitimacy kernel decomposes into three constraint stories with distinct epsilon values: the existential matrix reading (ε=0.78, high extraction) emphasizes survival-driven zero-sum logic and generates snare/piton classifications; the covenant continuity reading (ε≈0.45, estimated, tangled rope) emphasizes inherited obligation with some coordination function; the self-determination reading (ε≈0.55, estimated, tangled rope) emphasizes democratic legitimacy with majority-rule extraction and minority suppression. These are NOT alternative measurements of one constraint but three structurally distinct claims about legitimacy grounds that produce different beneficiary/victim structures, different suppression mechanisms, and different settlement predictions. All three readings are live positions in actual territorial conflicts; actors often oscillate between them or hold multiple readings simultaneously. The network links show influence: the existential matrix reading narrows what the other readings can achieve (forecloses some self-determination outcomes, constrains what covenants can justify). Network integration enables analysis of how switching readings changes the constraint's extracted value and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
