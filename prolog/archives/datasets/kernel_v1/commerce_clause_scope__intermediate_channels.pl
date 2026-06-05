% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__intermediate_channels
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__intermediate_channels, []).

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
 *   constraint_id: commerce_clause_scope__intermediate_channels
 *   human_readable: Commerce Clause Scope: Intermediate Channels Reading
 *   domain: constitutional_law/federalism/commerce_power
 *
 * SUMMARY:
 *   The Commerce Clause's intermediate channels reading represents one
 *   constitutional interpretation of federal power's proper scope. This
 *   reading permits federal authority to regulate (1) channels of interstate
 *   commerce (highways, railroads, telephone lines), (2) instrumentalities of
 *   interstate commerce (ships, trucks, goods in transit), and (3) activities
 *   substantially affecting interstate commerce, with limiting principles:
 *   non-economic activity requires a jurisdictional element tying the
 *   regulated activity to a channel or instrumentality; aggregation applies
 *   only to economic activity; federal authority cannot regulate via
 *   attenuated causal chains. This reading sits between two siblings: the
 *   narrow originalist reading (federal power limited to direct regulation of
 *   interstate commercial transactions themselves) and the broad effects-test
 *   reading (any activity substantially affecting interstate commerce is
 *   federally regulable, without sharp economic/non-economic or aggregation
 *   limits). The intermediate channels reading provides moderate federal
 *   authority — extensive within the economic sphere, constrained in the
 *   non-economic sphere, but with unstable limiting principles that create
 *   opportunities for doctrinal expansion. The constraint exhibits all six DR
 *   types, making it a diagnostic exemplar for how constitutional authority
 *   distributes across institutional positions and time horizons.
 *
 * KEY AGENTS:
 *   - Federal Commerce Authority: Institutional beneficiary (institutional/arbitrage) — benefits from jurisdictional clarity and broad-enough reach to regulate national economic problems
 *   - State Regulatory Authority: Institutional beneficiary and victim (organized/constrained) — benefits from categorical carve-out protecting state autonomy over non-economic regulation; victimized by manipulation of limiting principles
 *   - Local Non-Economic Regulation: Primary victim (powerless/trapped) — cannot exit interpretation of jurisdictional nexus; loses autonomy when federal authority recharacterizes nexus
 *   - Interstate Commerce Participants: Moderate victim (moderate/constrained) — benefit from uniform federal standards; constrained by doctrinal instability in application of limiting principles
 *   - Judicial Enforcement Apparatus: Institutional actor (institutional/arbitrage) — maintains performative limiting principles while simultaneously enabling expansions through recharacterization
 *   - Conceptual Coherence: Abstract victim (powerless/trapped) — cannot organize; bears cost of doctrinal instability and manipulability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, 0.48).
domain_priors:suppression_score(commerce_clause_scope__intermediate_channels, 0.52).
domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, extractiveness, 0.48).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(commerce_clause_scope__intermediate_channels, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__intermediate_channels, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__intermediate_channels, "Commerce Clause Scope: Intermediate Channels Reading").
narrative_ontology:topic_domain(commerce_clause_scope__intermediate_channels, "constitutional_law/federalism/commerce_power").

domain_priors:requires_active_enforcement(commerce_clause_scope__intermediate_channels).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__intermediate_channels, 'ee1813eb-fe91-40cf-964f-e410915a4ada').
narrative_ontology:cs_kernel_codification('ee1813eb-fe91-40cf-964f-e410915a4ada', formalized).
narrative_ontology:cs_authority_grounding('ee1813eb-fe91-40cf-964f-e410915a4ada', lineage).
narrative_ontology:cs_interpretation_layer_present('ee1813eb-fe91-40cf-964f-e410915a4ada').
narrative_ontology:cs_reading_relation('ee1813eb-fe91-40cf-964f-e410915a4ada', commerce_clause_scope__narrow_originalist, coexists_with).
narrative_ontology:cs_reading_relation('ee1813eb-fe91-40cf-964f-e410915a4ada', commerce_clause_scope__broad_effects_test, influences).
narrative_ontology:cs_axiom('ee1813eb-fe91-40cf-964f-e410915a4ada', foundational, federal_commerce_authority_permits_channel_regulation).
narrative_ontology:cs_axiom_status(federal_commerce_authority_permits_channel_regulation, holdable).
narrative_ontology:cs_axiom_grounding('ee1813eb-fe91-40cf-964f-e410915a4ada', federal_commerce_authority_permits_channel_regulation, deontological).
narrative_ontology:cs_axiom('ee1813eb-fe91-40cf-964f-e410915a4ada', foundational, non_economic_activity_requires_jurisdictional_nexus).
narrative_ontology:cs_axiom_status(non_economic_activity_requires_jurisdictional_nexus, holdable).
narrative_ontology:cs_axiom_grounding('ee1813eb-fe91-40cf-964f-e410915a4ada', non_economic_activity_requires_jurisdictional_nexus, instrumental).
narrative_ontology:cs_reference_frame('ee1813eb-fe91-40cf-964f-e410915a4ada', federal_commerce_authority_with_state_autonomy).
narrative_ontology:cs_drift_state('ee1813eb-fe91-40cf-964f-e410915a4ada', contemporary_regulatory_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee1813eb-fe91-40cf-964f-e410915a4ada', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(commerce_clause_scope__intermediate_channels, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, federal_commerce_authority).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__intermediate_channels, state_regulatory_autonomy).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, conceptual_coherence).
narrative_ontology:constraint_victim(commerce_clause_scope__intermediate_channels, local_non_economic_regulation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% LOCAL NON-ECONOMIC REGULATION (SNARE): Local jurisdictions attempting to regulate non-economic conduct (family law, criminal procedure, education) face federal encroachment when federal authority discovers a channel or instrumentality nexus. Cannot exit the interpretation of nexus; bears full cost of doctrinal instability. Maximum extraction from this perspective — locality cannot control whether the federal anchor is found.
constraint_indexing:constraint_classification(commerce_clause_scope__intermediate_channels, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% STATE REGULATORY AUTHORITY (TANGLED ROPE): States benefit from genuine categorical boundaries (cannot federally regulate purely local family law, criminal procedure, or education) but are constrained by the instrumentality and channel prongs. Extraction exists in the form of doctrinal instability and shifting judicial interpretation, but coordination function persists — the limiting principles create predictable zones of state autonomy. Constrained exit because states cannot opt out of federal supremacy but can strategically frame regulations to avoid nexus.
constraint_indexing:constraint_classification(commerce_clause_scope__intermediate_channels, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% FEDERAL COMMERCE AUTHORITY (ROPE): Benefits from jurisdictional clarity via categorical distinctions (economic activity vs. non-economic, channels vs. isolated conduct). The limiting principles create coordination function — states know where federal reach extends. Federal authority experiences the constraint as enabling rather than constraining: the intermediate channels framework provides a clear doctrine for asserting legitimate federal commerce jurisdiction without appearing boundless.
constraint_indexing:constraint_classification(commerce_clause_scope__intermediate_channels, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% JUDICIAL ENFORCEMENT APPARATUS (PITON): The intermediate channels doctrine is substantially performative at the margins. Courts apply the framework while simultaneously complaining about its manipulability (channel definitions are unstable, instrumentality nexus is often presumed, aggregation doctrine is circular). The enforcement ritual persists through institutional inertia — abandoning the framework would require choosing between pure originalism or pure effects-test, both more radical. Theater ratio rises as edge cases proliferate.
constraint_indexing:constraint_classification(commerce_clause_scope__intermediate_channels, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% INTERSTATE COMMERCE PARTICIPANTS (TANGLED ROPE): Businesses engaged in interstate commerce benefit from federal authority ensuring uniform standards and preventing protectionist state regulation. They are simultaneously constrained by federal authority's unpredictable application of limiting principles — the economic/non-economic distinction and aggregation doctrine create compliance uncertainty. Moderate extraction because regulation is necessary but doctrinally unstable.
constraint_indexing:constraint_classification(commerce_clause_scope__intermediate_channels, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% ANALYTICAL OBSERVER / STRUCTURAL NECESSITY VIEW (MOUNTAIN): From a civilizational perspective, some federal authority over interstate commerce is an immutable constitutional necessity — the alternative (fifty independent economic regimes) is structurally impossible for a federal union. This perspective sees the limiting principles as natural law constraints on doctrine, not contingent institutional choices. However, this reading challenges the mountain classification by showing that the 'limiting principles' are themselves contestable and highly manipulable, revealing the false summit structure.
constraint_indexing:constraint_classification(commerce_clause_scope__intermediate_channels, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__intermediate_channels_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commerce_clause_scope__intermediate_channels, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commerce_clause_scope__intermediate_channels, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__intermediate_channels, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(commerce_clause_scope__intermediate_channels, TR),
    TR >= 0.70.

:- end_tests(commerce_clause_scope__intermediate_channels_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Federal authority extracts significant expansionary capability through the three prongs (channels, instrumentalities, substantial effects), but the limiting principles constrain this extraction within the economic sphere. Non-economic activity nominally receives protection, but the protection is unstable because 'jurisdictional element' and 'channel nexus' are ex-post rationalization opportunities. The measurement trajectory (0.38 → 0.44 → 0.48) reflects historical doctrine drift: the intermediate channels framework has been steadily expanded through recharacterization of activities and channels, particularly as courts have embraced the effects-test variant of aggregation. Suppression (0.52): Moderate. Federal authority suppresses alternative constitutional readings through institutional control of interpretation (federal courts decide what counts as a channel or instrumentality). But suppression is not total because the limiting principles themselves create check points — states can argue for narrow channel definitions, originalists can argue for narrower commerce power, effects-test advocates can argue for explicit rejection of limiting principles. Theater ratio (0.58): Moderate-high. The limiting principles are substantially performative. Courts invoke them while simultaneously expanding their scope (channels are redefined as needs arise, instrumentalities broadly construed, aggregation doctrine routinely applied). The performance increases over time (0.42 → 0.58) because judges increasingly recognize they are applying malleable categories while maintaining the pretense of stable limiting doctrine.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same constitutional framework distributes radically differently across institutional and temporal positions. Federal authority sees coordination (Rope) — the limiting principles enable federal reach over genuine national economic problems while respecting state autonomy. States see mixed extraction and coordination (Tangled Rope) — they benefit from categorical carve-outs but are victimized by unstable limiting principles. Local non-economic conduct sees pure extraction (Snare) — cannot control whether federal authority discovers a nexus. The judicial apparatus sees degraded ritual (Piton) — performs limiting-principle role while enabling expansion. The civilizational analytical observer risks seeing structural constitutional necessity (Mountain) — federal commerce power is inherent to federal union — but the reading reveals false summit: the 'necessary' architecture is actually a contingent institutional arrangement vulnerable to doctrinal drift. The perspectival gap reveals that the intermediate channels reading is not a genuine limiting principle but a rationalization framework that permits gradual expansion while maintaining the appearance of constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) follows from the agent's structural relationship to federal vs. state authority allocation. Federal beneficiaries with arbitrage options experience low d (federal authority sees coordination function). State authority with constrained options experiences moderate d (genuine authority but restricted by limiting-principle instability). Local non-economic conduct with no exit experiences high d (trapped, powerless, bears full cost of doctrinal manipulation). Interstate commerce participants with moderate power and constrained exit experience moderate-high d (benefit from uniform regulation but constrained by unpredictable application). The analytical observer occupies d ≈ 0.72 (organizational analytical position observing the distribution of authority across positions). The engine derives d from beneficiary/victim declarations: federal commerce authority is beneficiary → low d; state regulatory autonomy is mixed (beneficiary in non-economic sphere, victim in economic sphere) → moderate d; local non-economic conduct is victim → high d; conceptual coherence is victim → high d.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that the intermediate channels reading's legitimacy rests on the coherence and stability of its three limiting principles. As those principles degrade (through channel recharacterization, instrumentality expansion, and aggregation doctrine drift), the reading collapses toward the broad effects-test sibling. The mandatrophy is not 'which type is correct?' but 'under what conditions do limiting principles function as genuine restraints vs. post-hoc rationalizations?' The Tangled Rope classification depends on the limiting principles being genuinely limiting; if they prove purely performative, the classification shifts toward Snare (for state and local actors) and Rope (for federal beneficiaries). The measurement trajectory suggests this drift is in progress: extractiveness and theater both rising, indicating the limiting principles are being stretched beyond their original scope while maintaining performative invocation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    channel_definition_instability,
    'What constitutes a ''channel of interstate commerce'' with sufficient clarity to serve as a limiting principle rather than a manipulation vector?',
    'Historical case law analysis tracking channel-definition breadth over time; identification of whether channels are defined ex-ante (channels inherently interstate) or ex-post (any channel can be recharacterized as interstate in hindsight)',
    'If channels are genuinely pre-defined and stable: intermediate channels reading is a legitimate limiting principle (Rope-like restraint). If channels are ex-post rationalizations: the reading collapses into de facto effects-test (much broader federal authority), and classification shifts toward Snare from local perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(channel_definition_instability, empirical, 'Whether channel definitions provide meaningful doctrinal limits or post-hoc rationalization').

omega_variable(
    economic_noneconomic_boundary,
    'Is the economic/non-economic distinction a coherent constitutional category or a manipulable label that migrates with judicial willingness to find federal interest?',
    'Linguistic and doctrinal analysis: track whether acts labeled ''economic'' in recent cases would have been labeled identically in earlier periods; examine whether the same activity has been recategorized across doctrine',
    'If coherent: the non-economic carve-out genuinely protects local autonomy, and Tangled Rope classification is accurate (moderate extraction, real limiting function). If manipulable: the carve-out is performative, and classification shifts toward Snare from local non-economic conduct perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_noneconomic_boundary, empirical, 'Coherence and stability of economic/non-economic boundary').

omega_variable(
    aggregation_doctrine_circularity,
    'Does the aggregation doctrine (combining individual effects across many actors to find substantial federal impact) identify a genuinely distinct limiting principle, or does it collapse into effects-test reasoning applied pluralistically?',
    'Formal logic: compare aggregation doctrine to pure effects-test; identify whether aggregation doctrine produces different outcomes than effects-test applied to collective conduct; track whether courts actually reject aggregation claims or routinely affirm them',
    'If genuinely distinct: aggregation doctrine limits federal reach to cases where individual actors'' cumulative conduct substantially affects interstate commerce (Rope restraint). If circular: aggregation is effects-test with extra steps, and the intermediate channels reading lacks meaningful doctrinal boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_doctrine_circularity, conceptual, 'Aggregation doctrine as independent limiting principle vs. effects-test variant').

omega_variable(
    jurisdictional_element_requirement_scope,
    'How broad is the ''jurisdictional element'' requirement — does it meaningfully exclude non-economic activity, or is any federal element sufficient to trigger intermediate channels scrutiny?',
    'Case law analysis: catalog jurisdictional elements that courts have found sufficient; identify whether any element truly excludes federal reach on non-economic activity or whether courts find nexus even with attenuated federal connections',
    'If requirement is genuinely limiting: non-economic local conduct has real protected zone (state autonomy beneficiary). If requirement is weak: federal authority reaches most conduct with any federal nexus, and non-economic regulation becomes increasingly vulnerable (Snare-like extraction for local jurisdiction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_element_requirement_scope, empirical, 'Jurisdictional element requirement as meaningful limitation on federal reach').

omega_variable(
    reading_coherence_via_limiting_principles,
    'Do the three limiting principles (channels, instrumentalities, substantial effects with aggregation limits, and non-economic carve-out) function as a unified doctrine, or do they operate independently and sometimes contradictorily?',
    'Doctrinal coherence analysis: identify cases where principles conflict or yield inconsistent outcomes; track whether courts apply all three or select among them; examine whether the principles converge or diverge as factual complexity increases',
    'If unified: the intermediate channels reading is a coherent constitutional framework (Tangled Rope). If contradictory: the reading fragments into multiple constraints with different ε values and classification profiles, requiring decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coherence_via_limiting_principles, conceptual, 'Doctrinal unity and mutual coherence of limiting principles').

omega_variable(
    kernel_reading_validity,
    'Is the ''intermediate channels'' reading of the commerce clause kernel an enduring constitutional framework, or a transitional reading being superseded by broader effects-test or narrower originalist interpretations?',
    'Constitutional history and jurisprudential trajectory: assess whether courts are solidifying, expanding, or contracting the intermediate channels framework; identify whether recent doctrinal shifts suggest reading is ascendant or declining',
    'If enduring: this constraint story captures a stable constitutional architecture. If transitional: this reading is being displaced by a sibling reading (broad effects-test or narrow originalism), and the intermediate channels constraint''s classification reflects a temporary holding pattern rather than stable constitutional law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_validity, empirical, 'Durability and constitutional trajectory of intermediate channels reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__intermediate_channels, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comch_tr_t0, commerce_clause_scope__intermediate_channels, theater_ratio, 0, 0.42).
narrative_ontology:measurement(comch_tr_t20, commerce_clause_scope__intermediate_channels, theater_ratio, 20, 0.52).
narrative_ontology:measurement(comch_tr_t40, commerce_clause_scope__intermediate_channels, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(comch_be_t0, commerce_clause_scope__intermediate_channels, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comch_be_t20, commerce_clause_scope__intermediate_channels, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(comch_be_t40, commerce_clause_scope__intermediate_channels, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(comch_su_t0, commerce_clause_scope__intermediate_channels, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(comch_su_t20, commerce_clause_scope__intermediate_channels, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(comch_su_t40, commerce_clause_scope__intermediate_channels, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__intermediate_channels, enforcement_mechanism).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__intermediate_channels, dormant_commerce_clause_state_autonomy).

% DUAL FORMULATION NOTE:
% The commerce clause scope constraint decomposes into three structurally distinct readings with different ε values and beneficiary/victim structures. The intermediate channels reading (this file) has ε=0.48 and exhibits Tangled Rope structure from federal/state institutional positions, Snare from local non-economic perspectives. The narrow originalist reading has lower ε (federal authority genuinely constrained) and exhibits stronger state beneficiary positions. The broad effects-test reading has higher ε (federal authority less constrained) and exhibits Snare structure from state/local perspectives. Each reading is generated as a separate constraint story linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
