% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__zionist_refuge_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: territorial_legitimacy_dual__zionist_refuge_reading
 *   human_readable: Zionist Refuge Reading of Israeli Territorial Legitimacy
 *   domain: political/international relations/territorial sovereignty
 *
 * SUMMARY:
 *   This story instantiates the Zionist refuge reading of a contested
 *   legitimacy kernel over territorial sovereignty in historic
 *   Palestine/Israel. Under this reading, Israeli statehood is uncontested at
 *   its 1948 foundation — grounded in documented historical persecution, the
 *   diplomatic legitimacy of UN General Assembly Resolution 181, and (for
 *   religious-nationalist strands within the reading) a scriptural claim to
 *   the land. Post-1967 territorial holdings are treated as negotiable rather
 *   than foundational, and Palestinian displacement is causally attributed to
 *   Arab state rejection of the 1947 partition plan and the wars that
 *   followed rather than to the founding act itself. This is ONE of three
 *   readings of a single contested kernel (territorial_legitimacy_dual); the
 *   sibling readings — Palestinian autochthony (continuous habitation,
 *   displacement trauma, right of return) and two-state coexistence (mutual
 *   dual legitimacy under 1967-boundary compromise) — are separate constraint
 *   stories with their own ε values and structural data, not alternative
 *   measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy_dual__zionist_refuge_reading, 0.62).
domain_priors:theater_ratio(territorial_legitimacy_dual__zionist_refuge_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__zionist_refuge_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__zionist_refuge_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__zionist_refuge_reading, "Zionist Refuge Reading of Israeli Territorial Legitimacy").
narrative_ontology:topic_domain(territorial_legitimacy_dual__zionist_refuge_reading, "political/international relations/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__zionist_refuge_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__zionist_refuge_reading, 'bf405ba1-2241-4c11-9161-2f09e4e67aa7').
narrative_ontology:cs_kernel_codification('bf405ba1-2241-4c11-9161-2f09e4e67aa7', distributed).
narrative_ontology:cs_authority_grounding('bf405ba1-2241-4c11-9161-2f09e4e67aa7', distributed).
narrative_ontology:cs_reading_relation('bf405ba1-2241-4c11-9161-2f09e4e67aa7', territorial_legitimacy_dual__palestinian_autochthony_reading, coexists_with).
narrative_ontology:cs_reading_relation('bf405ba1-2241-4c11-9161-2f09e4e67aa7', territorial_legitimacy_dual__two_state_coexistence_reading, influences).
narrative_ontology:cs_axiom('bf405ba1-2241-4c11-9161-2f09e4e67aa7', foundational, un_partition_acceptance_confers_sovereign_legitimacy).
narrative_ontology:cs_axiom_status(un_partition_acceptance_confers_sovereign_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('bf405ba1-2241-4c11-9161-2f09e4e67aa7', un_partition_acceptance_confers_sovereign_legitimacy, conventional).
narrative_ontology:cs_axiom('bf405ba1-2241-4c11-9161-2f09e4e67aa7', foundational, historical_persecution_grounds_refuge_entitlement).
narrative_ontology:cs_axiom_status(historical_persecution_grounds_refuge_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('bf405ba1-2241-4c11-9161-2f09e4e67aa7', historical_persecution_grounds_refuge_entitlement, empirically_contingent).
narrative_ontology:cs_axiom('bf405ba1-2241-4c11-9161-2f09e4e67aa7', secondary, post_1967_territory_status_is_negotiable_not_foundational).
narrative_ontology:cs_axiom_status(post_1967_territory_status_is_negotiable_not_foundational, holdable).
narrative_ontology:cs_axiom_grounding('bf405ba1-2241-4c11-9161-2f09e4e67aa7', post_1967_territory_status_is_negotiable_not_foundational, instrumental).
narrative_ontology:cs_reference_frame('bf405ba1-2241-4c11-9161-2f09e4e67aa7', un_resolution_181_partition_acceptance).
narrative_ontology:cs_drift_state('bf405ba1-2241-4c11-9161-2f09e4e67aa7', post_oslo_settlement_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bf405ba1-2241-4c11-9161-2f09e4e67aa7', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__zionist_refuge_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_jewish_citizens).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora_seeking_refuge).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_security_apparatus).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees_1948).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, west_bank_palestinian_residents).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__zionist_refuge_reading, gaza_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__zionist_refuge_reading, religious_zionist_settler_movement).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, un_partition_resolution_181_legitimacy).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, jewish_indigenous_connection_to_land).
narrative_ontology:constraint_vindicates(territorial_legitimacy_dual__zionist_refuge_reading, necessity_of_jewish_sovereign_refuge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a state whose founding they regard as vindicated by UN partition acceptance, historical persecution culminating in the Holocaust, and continuous historical/religious connection to the land. Benefit from sovereign protection and the 'never again' security logic embedded in state institutions, military service, and settlement policy in contested territories.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_jewish_citizens, beneficiary,
    organized, generational, constrained, national).

% Hold a standing right of return under the Law of Return, treated as the concrete guarantee that the persecution narrative is meant to prevent from recurring. Most never exercise it, but its existence is load-bearing for the legitimacy claim — a sovereign refuge that would exist for them if needed.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, jewish_diaspora_seeking_refuge, beneficiary,
    moderate, civilizational, arbitrage, global).

% Administers settlement expansion, checkpoint systems, and territorial control in the West Bank by framing 1967-onward acquisitions as security necessities flowing from unresolved threats since Arab rejection of the 1947 partition and subsequent wars. Sets the operational boundary between what is treated as 'uncontested 1948 legitimacy' and 'negotiable post-1967 territory,' and enforces that boundary through military and administrative control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, israeli_state_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Displaced during and after the 1948 war; under this reading, their displacement is framed as a consequence of Arab state rejection of partition rather than of Israeli state formation itself. They and their descendants remain barred from return, bearing the enduring cost of a founding narrative that assigns causal responsibility for their displacement elsewhere.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_refugees_1948, payer,
    powerless, generational, trapped, regional).

% Live under military occupation and settlement expansion justified by this reading as necessary security management of negotiable, not foundational, territory. Movement, land use, and political status are constrained by a legitimacy framework in which their national claims are treated as subordinate to Israeli security requirements.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, west_bank_palestinian_residents, payer,
    powerless, biographical, trapped, regional).

% Live under blockade and periodic military operations justified within this reading as responses to security threats rooted in the original rejection of partition and subsequent conflicts. Bear direct physical and economic costs of a security logic that treats their territory's status as unresolved.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, gaza_residents, payer,
    powerless, immediate, trapped, regional).

% UN bodies, mediating states, and international courts that have repeatedly challenged the security-justification framing for post-1967 territorial control (settlement legality opinions, refugee return resolutions). Their findings are acknowledged as diplomatically relevant but are not treated as binding on the legitimacy claim itself, which rests on 1948 foundations this reading holds as settled.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, international_diplomatic_community, excluded,
    institutional, generational, analytical, global).

% Draws on the divine-promise strand of this reading to press for permanent settlement of territories this reading otherwise treats as merely 'negotiable,' pushing the boundary between uncontested and contested territory outward. Directly shapes settlement policy and land allocation in the West Bank.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__zionist_refuge_reading, religious_zionist_settler_movement, beneficiary,
    organized, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy_dual__zionist_refuge_reading, religious_zionist_settler_movement, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy_dual__zionist_refuge_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__zionist_refuge_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a sovereign, defensible refuge for a historically persecuted diaspora people, coordinated around international legal recognition (UN Partition Resolution 181), and organizes collective security and self-determination for Israeli Jewish citizens against a background of documented historical genocide and statelessness.
% TRANSFER_FUNCTION: Moves territorial control, land use rights, and freedom of movement from Palestinian residents and 1948 refugees to the Israeli state and its Jewish citizens, justified within this reading by security necessity and by causal attribution of displacement to Arab rejection of partition rather than to the state's own formation and expansion.
% ABSENT_VOICES: Palestinian refugees and West Bank/Gaza residents hold a fundamentally different causal account of 1948 and 1967 (see the sibling autochthony reading) that this reading does not incorporate; their claims of continuous habitation and involuntary displacement are treated as a downstream Arab-rejection consequence rather than an independent legitimacy claim requiring equal weight.
% DISAPPEARANCE_RATIONALE: If this specific legitimacy reading collapsed — if the 1948-uncontested/1967-negotiable framing and its security justification lost force — Israeli territorial policy, settlement expansion rationale, international legal argumentation, and diaspora right-of-return provisions would all require re-grounding on different premises; the practical architecture of occupation and sovereignty claims depends on this reading remaining operative for its adherents.
% FOUNDING_PROBLEM: Statelessness and repeated, escalating persecution of Jewish populations in Europe and the Middle East, culminating in the Holocaust, combined with the absence of any sovereign territory offering guaranteed refuge and self-determination.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international bodies outside the Zionist movement corroborate the historical persecution and the 1947 UN partition vote as documented fact (League of Nations mandate history, Holocaust historiography, UN General Assembly records). However, independent international legal bodies (ICJ advisory opinions, UN human rights mechanisms) and historians of the Nakba dispute whether the founding problem — the need for refuge — remains the operative justification for continued post-1967 territorial control, or whether that control now serves purposes (settlement expansion, permanent occupation infrastructure) disconnected from the original refuge rationale.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__zionist_refuge_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__zionist_refuge_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__zionist_refuge_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__zionist_refuge_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__zionist_refuge_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__zionist_refuge_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (ε) is authored as substantial (0.58) and rising because, by this reading's own lights, the security-necessity framing that legitimately governs the 1948 founding has been extended over decades to justify open-ended control over post-1967 territory whose status this reading itself calls 'negotiable' — the gap between the founding claim and its territorial application widens over the measured interval, particularly post-2000 with settlement expansion. Suppression is authored high (0.62) reflecting the checkpoint, permit, and military administrative systems required to maintain the 1967-onward territorial arrangement, which this reading does not dispute is actively enforced. Theater ratio is moderate-low (0.28): the security rationale corresponds to a real, contested threat environment, not pure performance, but rises over time as some settlement activity in this reading's own account outruns strict security justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli Jewish citizens and diaspora Jews seeking refuge sit near the beneficiary end: the constraint (a sovereign state grounded in this legitimacy account) subsidizes their security and self-determination. Palestinian refugees, West Bank residents, and Gaza residents sit near the full-target end: trapped exit options, no return right, and territorial/movement control administered against them. The security apparatus is the agenda-setter with arbitrage-level exit (it sets and can revise the enforcement boundary). The settler movement is both beneficiary and a secondary agenda-setter, since its political activity actively pushes the 'negotiable' boundary this reading nominally maintains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish statelessness and persecution) is genuinely live in the historical record and corroborated outside the beneficiary set (Holocaust historiography, League of Nations/UN documentary record). But the reading's own internal distinction between 'uncontested 1948' and 'negotiable 1967+' territory creates a mandatrophy risk specific to the post-1967 layer: if the security rationale for continued control has in practice detached from the founding refuge problem and instead sustains permanent settlement infrastructure, the post-1967 extraction persists on inertia and ideology (divine promise, strategic depth) rather than on the founding justification. The disappearance_verdict of world_rearranges applies to the whole constraint as authored; the founding_problem_status is marked contested specifically because this bifurcation is where dispute concentrates even within pro-Zionist scholarship.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_versus_extended_justification_gap,
    'Does the security-necessity logic that legitimates the 1948 founding, by this reading''s own terms, still apply to post-1967 territorial control, or has settlement and occupation infrastructure outrun the founding refuge rationale?',
    'Comparative analysis of settlement location/timing data against documented security threat assessments; testimony from Israeli security establishment figures (including dissenting former officials) on whether specific settlement decisions were security-driven or ideologically/strategically driven independent of imminent threat.',
    'If the gap is wide and growing, the post-1967 layer of this reading functions increasingly as tangled rope shading toward snare (coordination cover thinning, extraction persisting on inertia); if the gap is narrow, the reading''s internal claim that 1967 territory is ''negotiable'' security management remains structurally coherent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_versus_extended_justification_gap, empirical, 'Whether post-1967 territorial control still tracks the founding security/refuge justification or has decoupled from it.').

omega_variable(
    causal_attribution_of_1948_displacement,
    'Is the causal attribution of Palestinian displacement to Arab rejection of partition (rather than to the mechanics of the 1948 war and expulsion/flight patterns as documented by Israeli and Palestinian historians) an adequate account, or does it externalize responsibility that this reading''s own historical record does not fully support?',
    'Cross-reference against the ''New Historians'' archival scholarship (Israeli military and government archives on 1948 population transfers) versus traditional Zionist historiography; assess where documented events diverge from the rejection-as-sole-cause narrative.',
    'A resolution toward mixed causation (both Arab rejection AND active Israeli military policy contributing to displacement) would require this reading to reduce the weight it places on external causal attribution without abandoning the 1948 legitimacy claim itself; a resolution fully supporting rejection-as-cause would leave the reading''s causal account intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_attribution_of_1948_displacement, empirical, 'Whether the reading''s causal account of 1948 displacement holds up against archival historiography.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly does this reading structurally diverge from the sibling readings — is it the causal account of 1948 displacement, the moral weight assigned to divine/historical claims versus continuous habitation, or the boundary drawn between ''uncontested'' and ''negotiable'' territory?',
    'This is inherently a conceptual/committer-frame question, not resolvable by additional data alone; documented here per Rule 2 to route committer structure to omega rather than into the constraint''s own classification.',
    'Clarifies that the three sibling constraints are not competing measurements of one constraint but three structurally distinct constraints sharing a kernel; a reader conflating them would misapply findings from one reading to another.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'Locates the specific structural disagreement points between this reading and its siblings (palestinian_autochthony_reading, two_state_coexistence_reading).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__zionist_refuge_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1947, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(terr_tr_t1980, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(terr_tr_t2015, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy_dual__zionist_refuge_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(terr_be_t1947, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1947, 0.22).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1967, 0.38).
narrative_ontology:measurement(terr_be_t1980, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1980, 0.46).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 1993, 0.42).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(terr_be_t2015, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2015, 0.56).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy_dual__zionist_refuge_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1947, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1947, 0.3).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1967, 0.48).
narrative_ontology:measurement(terr_su_t1980, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(terr_su_t2015, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy_dual__zionist_refuge_reading, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__zionist_refuge_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, palestinian_autochthony_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__zionist_refuge_reading, two_state_coexistence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_legitimacy_dual kernel. palestinian_autochthony_reading authors continuous-habitation and displacement-trauma as the legitimacy ground with a different (likely higher, from that reading's perspective) ε for the 1948 founding event itself; two_state_coexistence_reading authors a compromise framework treating both 1948 and 1967 as negotiated rather than settled/negotiable. Each carries independent ε, beneficiary/victim sets, and classification. This reading structurally influences the coexistence reading (its 1967-negotiable framing supplies one input to compromise negotiation) and stands in tension with the autochthony reading's causal account of 1948.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
