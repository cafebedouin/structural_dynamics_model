% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__narrow_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__narrow_originalist, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: commerce_clause_scope__narrow_originalist
 *   human_readable: Commerce Clause Scope (Narrow Originalist Reading)
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   The narrow originalist reading of the Commerce Clause asserts that
 *   federal regulatory power is limited to removing state-imposed barriers to
 *   interstate trade and establishing uniform commercial rules for
 *   transactions crossing state lines. Under this reading, 'regulate' means
 *   'make regular' (facilitate coordination), not restrict; and federal
 *   authority is categorically excluded from intrastate economic activity,
 *   non-commercial conduct (civil rights, environmental protection, labor
 *   standards), and purely local affairs. This constraint exemplifies how a
 *   single constitutional text is read differently depending on which
 *   historico-interpretive community (originalist vs. progressive, textualist
 *   vs. purposivist, libertarian vs. nationalist) provides the reading
 *   framework. The narrow originalist reading benefits state governments and
 *   local businesses by preserving regulatory autonomy and experimental
 *   variation; it extracts from civil rights claimants in states that refuse
 *   equal protection, and from national regulatory uniformity advocates. The
 *   constraint exhibits high theater because originalist textual fidelity
 *   claims to exclude judicial policy-making, yet the boundary between
 *   'commerce' and 'economic activity' (or between 'regulate' and 'restrict')
 *   requires extensive jurisprudential determination — the interpretive labor
 *   is hidden behind a claim of textual clarity.
 *
 * KEY AGENTS:
 *   - State Governments: Primary beneficiaries (powerful/mobile) — retain police powers and regulatory autonomy under narrow reading
 *   - Local Businesses: Secondary beneficiaries (powerful/mobile) — face less federal regulatory burden, more state variation
 *   - Civil Rights Claimants: Primary victims (powerless/trapped) — excluded from federal protection when discrimination is intrastate or non-commercial
 *   - Interstate Commerce Stakeholders: Mixed (organized/constrained) — benefit from uniform federal rules for truly interstate activity but bear burden of proving interstate nexus
 *   - Federal Regulatory Apparatus: Institutional actor (institutional/arbitrage) — inherits broad post-1937 authority; faces delegitimation under originalist canon but continues operating through inertia
 *   - Federal Courts: Institutional interpreter (institutional/constrained) — must coordinate federalism boundaries while managing re-litigation of constitutional authority for assumed-valid statutes
 *   - Originalist Legal Community: Analytical community (analytical/analytical) — claims to read text faithfully but selects among competing historical sources
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__narrow_originalist, 0.38).
domain_priors:suppression_score(commerce_clause_scope__narrow_originalist, 0.52).
domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, extractiveness, 0.38).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(commerce_clause_scope__narrow_originalist, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__narrow_originalist, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__narrow_originalist, "Commerce Clause Scope (Narrow Originalist Reading)").
narrative_ontology:topic_domain(commerce_clause_scope__narrow_originalist, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__narrow_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__narrow_originalist, '6e5e6b50-4883-4797-b80a-099e5a007090').
narrative_ontology:cs_kernel_codification('6e5e6b50-4883-4797-b80a-099e5a007090', fixed_text).
narrative_ontology:cs_authority_grounding('6e5e6b50-4883-4797-b80a-099e5a007090', lineage).
narrative_ontology:cs_interpretation_layer_present('6e5e6b50-4883-4797-b80a-099e5a007090').
narrative_ontology:cs_reading_relation('6e5e6b50-4883-4797-b80a-099e5a007090', commerce_clause_scope__broad_effects_test, forecloses).
narrative_ontology:cs_reading_relation('6e5e6b50-4883-4797-b80a-099e5a007090', commerce_clause_scope__intermediate_channels, coexists_with).
narrative_ontology:cs_axiom('6e5e6b50-4883-4797-b80a-099e5a007090', foundational, enumerated_powers_strict_construction).
narrative_ontology:cs_axiom_status(enumerated_powers_strict_construction, holdable).
narrative_ontology:cs_axiom_grounding('6e5e6b50-4883-4797-b80a-099e5a007090', enumerated_powers_strict_construction, deontological).
narrative_ontology:cs_axiom('6e5e6b50-4883-4797-b80a-099e5a007090', foundational, original_public_meaning_determines_scope).
narrative_ontology:cs_axiom_status(original_public_meaning_determines_scope, holdable).
narrative_ontology:cs_axiom_grounding('6e5e6b50-4883-4797-b80a-099e5a007090', original_public_meaning_determines_scope, empirically_contingent).
narrative_ontology:cs_axiom('6e5e6b50-4883-4797-b80a-099e5a007090', secondary, intrastate_local_autonomy).
narrative_ontology:cs_axiom_status(intrastate_local_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('6e5e6b50-4883-4797-b80a-099e5a007090', intrastate_local_autonomy, deontological).
narrative_ontology:cs_reference_frame('6e5e6b50-4883-4797-b80a-099e5a007090', enumerated_federal_powers_with_state_police_autonomy).
narrative_ontology:cs_drift_state('6e5e6b50-4883-4797-b80a-099e5a007090', contemporary_post_1937_administrative_state, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6e5e6b50-4883-4797-b80a-099e5a007090', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__narrow_originalist, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, local_businesses).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__narrow_originalist, regulatory_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, national_regulatory_uniformity).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, civil_rights_enforcement).
narrative_ontology:constraint_victim(commerce_clause_scope__narrow_originalist, interstate_commerce_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE SOVEREIGNTIES (ROPE) — From the originalist reading, states retain enumerated police powers over local commerce and social regulation. The constraint coordinates federalism by clarifying state autonomy boundaries. States can exit overreach by enforcing the constitutional limits themselves through courts. Perceived as legitimate coordination, not extraction.
constraint_indexing:constraint_classification(commerce_clause_scope__narrow_originalist, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL RIGHTS CLAIMANTS (SNARE) — Under narrow originalism, federal civil rights laws regulating non-commercial conduct (accommodations, employment, housing) exceed the Commerce Clause because they target intrastate activity or non-commercial rights. Claimants trapped within states that refuse to provide equal protection. No exit from state barriers; federal recourse eliminated. Pure extraction from the claimant's perspective.
constraint_indexing:constraint_classification(commerce_clause_scope__narrow_originalist, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERSTATE COMMERCE STAKEHOLDERS (TANGLED ROPE) — Businesses engaged in truly interstate commerce face genuine coordination function (uniform federal rules reduce transaction costs), but also bear enforcement burden of proving interstate nexus and navigating state carve-outs. Mixed benefit and cost; organized because they can lobby and relocate with some friction.
constraint_indexing:constraint_classification(commerce_clause_scope__narrow_originalist, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: FEDERAL REGULATORY APPARATUS (PITON) — The federal administrative state (EPA, OSHA, EEOC, etc.) inherited broad Commerce Clause interpretations from 1937-2010 era. Under narrow originalism, much of this apparatus is theoretically unconstitutional. The apparatus persists through institutional inertia and grandfathered legitimacy, but faces delegitimation under originalist canon. Theater is high: agencies continue regulating while constitutional authority is contested.
constraint_indexing:constraint_classification(commerce_clause_scope__narrow_originalist, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL COURTS (TANGLED ROPE) — Courts must coordinate federalism boundaries while managing litigation load and practical governance. Originalist reading creates enforcement burden: every federal statute must be re-tested against narrow Commerce Clause limits. Courts experience both genuine coordination function (clarifying boundaries) and extraction burden (revalidating assumed-constitutional statutes).
constraint_indexing:constraint_classification(commerce_clause_scope__narrow_originalist, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal reading-as-epistemic-claim perspective, the text of Article I §8 cl.3 is fixed and unambiguous: 'Congress shall have Power...To regulate Commerce...among the several States.' The narrow reading claims this text is immutable law; commerce means trade, regulate means make regular (facilitate not restrict), and the Framers' intent is knowable. However, this risks false summiturgy: the text is ambiguous regarding scope, and the 'original public meaning' of 'regulate' is itself contested among historians.
constraint_indexing:constraint_classification(commerce_clause_scope__narrow_originalist, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__narrow_originalist_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commerce_clause_scope__narrow_originalist, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commerce_clause_scope__narrow_originalist, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(commerce_clause_scope__narrow_originalist, TR),
    TR >= 0.70.

:- end_tests(commerce_clause_scope__narrow_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The narrow originalist reading preserves substantial federal authority for genuinely interstate commerce coordination while excluding federal reach into intrastate, non-commercial, or local-effects domains. The extraction flow is asymmetric: it benefits state sovereignties and local regulatory experimentation (low extraction from them) while harming civil rights claimants and national uniformity advocates (high extraction from them). The moderate overall value reflects that the beneficiaries are state governments (powerful, mobile) rather than powerless actors — the constraint coordinates federalism rather than purely extracting. Suppression (0.52): Moderate-high. The constraint suppresses alternatives through doctrinal authority and constitutional interpretation. States are not legally permitted to ignore state sovereignty limits; civil rights claimants cannot appeal to federal authority if the Clause is construed narrowly; federal agencies face pressure to revalidate authority. The suppression is not total because ongoing litigation and constitutional amendment remain open (though costly). Theater ratio (0.65): Moderate-high. Originalist reading claims to exclude judicial policy-making through fidelity to text and original intent, yet the determination of what 'regulate' means, what counts as 'interstate' vs. 'intrastate,' and what the 'original public meaning' was requires extensive interpretive work. The performative element is the claim of textual constraint without judicial discretion. As the constraint extends over time (measurement trajectory shows rising theater from 0.45 to 0.65), the gap between claimed textual clarity and actual doctrinal contestation widens — more cases require fact-specific determination of commerce power applicability, yet the originalist frame insists the answer is in the text.
 *
 * PERSPECTIVAL GAP:
 *   The narrow originalist reading produces maximum perspectival divergence. State governments see this as legitimate coordination (Rope): the constraint clarifies federalism boundaries and enables regulatory experimentation. Civil rights claimants see pure extraction (Snare): federal protection is unavailable for intrastate discrimination, trapping claimants within recalcitrant states. The federal regulatory apparatus sees theatrical degradation (Piton): it continues operating under post-1937 authority while facing delegitimation. Federal courts see mixed coordination and burden (Tangled Rope): they coordinate federalism but carry enforcement costs of re-litigating assumed-valid statutes. The analytical observer risks seeing immutable law (Mountain): the text is fixed, meaning is discoverable, and the Framers' intent constrains application. But this risks false summiturgy because the boundary between 'commerce' and 'economic activity,' and the scope of 'regulate,' are themselves historically contested. The perspectival gap reveals that narrowness is not textually mandated but rather a choice among competing interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   The narrow originalist reading derives directionality from beneficiary/victim declarations and the power-exit pair. State governments are beneficiaries with powerful/mobile status (can exit federal constraint through state policy and litigation) — directionality low, experienced extraction near zero. Civil rights claimants are victims with powerless/trapped status (no exit from state barriers when federal protection is unavailable) — directionality high, experienced extraction maximum. Federal regulatory agencies are institutional beneficiaries with arbitrage exit (can relocate regulatory authority to state level, reframe as state cooperation) — directionality low. Federal courts are institutional agents with constrained exit (must interpret the Constitution regardless of institutional burden) — directionality moderate. The perspectival gap reflects that the reading benefits the powerful (states, institutional authorities) while harming the powerless (civil rights claimants, disadvantaged minorities in non-regulating states). This is the signature of a tangled rope: genuine coordination function for interstate commerce, but asymmetric extraction from those excluded from federal protection.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by clarifying what federalism coordination entails under this specific reading. The narrow originalist reading is not asking 'Is federalism coordination legitimate?' (answered: yes, coordination is rope/tangled_rope). It is asking 'What is the scope of federal regulatory authority?' and answering 'Limited to interstate commerce; excluded from intrastate, non-commercial, purely local activity.' The mandatrophy resolution comes from recognizing that the beneficiaries of the reading (state sovereignty, regulatory autonomy) are genuinely served by coordination (uniform interstate rules reduce transaction costs, clarified boundaries enable experimentation), while the extraction from civil rights claimants is a downstream consequence of excluding them from federal protection — not a feature of the coordination mechanism itself, but rather of which actor groups the mechanism protects. The theater trajectory (rising from 0.45 to 0.65) documents increasing mandatrophy pressure: as the originalist reading encounters real-world cases requiring boundary determination, the gap between claimed textual clarity and actual doctrinal work widens. This suggests that the constraint is oscillating between tangled_rope (when applied to genuine interstate commerce) and false-summit-adjacent (when applied to intrastate activity with interstate effects, where the originalist answer remains indeterminate).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_public_meaning_regulate,
    'What did ''regulate'' mean in 1787-1789 original public meaning: narrow (set rules for existing commerce) or broad (manage/coordinate/control commerce)?',
    'Linguistic corpus analysis of 18th-century usage; comparison with contemporary usages in state constitutions, commercial law, and political philosophy',
    'If narrow: strict limits on federal commerce power. If broad: originalism permits extensive federal regulation. The same reading''s foundational axiom (original_intent_textualism) yields opposite structural outcomes depending on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_public_meaning_regulate, empirical, 'Semantic scope of ''regulate'' in original public meaning').

omega_variable(
    commerce_vs_economic_activity_boundary,
    'Is the distinction between ''commerce'' (interstate trade in goods/services) and ''economic activity'' (production, labor, environmental resource use) stable and knowable?',
    'Historical doctrine track from 1787 through Wickard (1942), Gonzales (2005), and contemporary originalist scholarship; identification of boundary test coherence across cases',
    'If stable and knowable: narrow reading enforces clear federal limits. If contested/evolving: narrow reading provides false certainty; boundary collapses under application, and the constraint reverts to institutional/political struggle (not mountain). This determines whether the constraint is a genuine mountain or a false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commerce_vs_economic_activity_boundary, conceptual, 'Stability of commerce/economic-activity boundary').

omega_variable(
    state_police_power_definition_contest,
    'Can state ''police powers'' (public health, safety, morals, welfare) be coherently distinguished from federal commerce regulation, or do they necessarily overlap in a commercial economy?',
    'Comparison of claimed state police power exercises with federal commerce power exercises in same regulatory domains (environmental protection, labor standards, civil rights); test whether functional distinction survives scrutiny',
    'If coherently distinguished: narrow originalism provides workable federalism rule. If overlapping/indistinguishable: the reading provides theatrical boundary clarity but generates friction in application (piton tendency). The constraint''s suppression value increases as boundary disputes multiply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_police_power_definition_contest, empirical, 'Coherence of state/federal regulatory boundary').

omega_variable(
    kernel_reading_ambiguity,
    'Is this narrow originalist reading a faithful interpretation of the historical Framers'' intent, or a selective modern reading that privileges certain sources and downweights others?',
    'Historiography of competing readings (Akhil Amar, Randy Barnett, Jack Rakove, Mark Graber); meta-analysis of source selection and evidentiary standards across originalist and progressive legal scholarship',
    'If faithful: narrow reading is legitimate judicial position within originalist canon. If selective: reading is a modern ideological projection, and the constraint''s claimed_type (tangled_rope with false-mountain oscillation) reflects institutional contestation rather than constitutional clarity. This omega documents the kernel-reading instability itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Historiographical status of narrow originalist reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__narrow_originalist, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(commerce_narrow_tr_t0, commerce_clause_scope__narrow_originalist, theater_ratio, 0, 0.45).
narrative_ontology:measurement(commerce_narrow_tr_t20, commerce_clause_scope__narrow_originalist, theater_ratio, 20, 0.58).
narrative_ontology:measurement(commerce_narrow_tr_t40, commerce_clause_scope__narrow_originalist, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(commerce_narrow_be_t0, commerce_clause_scope__narrow_originalist, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(commerce_narrow_be_t20, commerce_clause_scope__narrow_originalist, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(commerce_narrow_be_t40, commerce_clause_scope__narrow_originalist, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(commerce_narrow_su_t0, commerce_clause_scope__narrow_originalist, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(commerce_narrow_su_t20, commerce_clause_scope__narrow_originalist, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(commerce_narrow_su_t40, commerce_clause_scope__narrow_originalist, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__narrow_originalist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__narrow_originalist, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__broad_effects_test).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, dormant_commerce_clause_protectionism).
narrative_ontology:affects_constraint(commerce_clause_scope__narrow_originalist, civil_rights_act_jurisdictional_scope).

% DUAL FORMULATION NOTE:
% The commerce_clause_scope kernel has three major readings, each yielding distinct constraint stories with different epsilon values and victim sets. The narrow_originalist reading (this story, ε=0.38) is upstream of the broad_effects_test reading (ε=0.65) and intermediate_channels reading (ε=0.48). These are not observable-dependent variants of one constraint — they are three structurally distinct constraints with incompatible beneficiary/victim structures, grounded in different interpretive premises about what the constitutional text means.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__narrow_originalist, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
