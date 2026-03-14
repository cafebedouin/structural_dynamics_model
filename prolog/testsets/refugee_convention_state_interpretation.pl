% ============================================================================
% CONSTRAINT STORY: refugee_convention_state_interpretation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_state_interpretation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: refugee_convention_state_interpretation
 *   human_readable: Refugee Convention State Interpretation and Application Variance
 *   domain: international_law/humanitarian_protection
 *
 * SUMMARY:
 *   The 1951 Refugee Convention and its 1967 Protocol establish the
 *   international legal framework for protection of persons fleeing
 *   persecution. However, the treaty's core terms — 'persecution,' 'due to,'
 *   'particular social group' — lack binding definitions, delegating
 *   interpretation authority to individual state signatories. This structural
 *   indeterminacy creates a constraint where restrictive interpretation
 *   extracts protection from asylum seekers while benefiting
 *   restrictive-interpretation states through enforced sovereign discretion.
 *   The constraint exhibits characteristics of both coordination
 *   (establishing shared norms and burdens) and extraction (enabling
 *   unilateral benefit through divergent interpretation). The convention
 *   governance mechanism is substantially performative: the Committee of
 *   Experts provides non-binding guidance; state implementation is not
 *   audited; dispute resolution is absent. Meanwhile, restrictive
 *   interpretations have intensified since 2010 as states face domestic
 *   political pressure on immigration, causing base extractiveness and
 *   theater ratio to drift upward across the measurement interval.
 *
 * KEY AGENTS:
 *   - Asylum Seekers: Primary victims (powerless/trapped) — lack standing in interpretation debates, face maximum extraction from restrictive interpretations, cannot exit the constraint
 *   - Restrictive-Interpretation States: Primary beneficiaries (institutional/arbitrage) — capture border control, security, and social policy objectives through narrow interpretations; can exit treaty costlessly; experience constraint as pure coordination of sovereignty with nominal participation
 *   - Progressive-Interpretation States: Secondary actors (powerful/mobile) — experience extraction as burden-shifting from restrictive neighbors; enjoy reputational benefits from broader interpretation but face domestic pressure to restrict; can exit through policy or treaty withdrawal
 *   - Humanitarian Organizations (UNHCR, NGOs): Moderate actors (moderate/constrained) — genuine coordination function (assessment, protection services) alongside extraction (resource diversion into legal advocacy); partially exit-capable through alternative funding or jurisdictions
 *   - International Coordination Coalition: Organized actors (organized/constrained) — building supplementary frameworks (CAT, ICCPR, regional directives) to constrain variance; see constraint as temporary problem with sunset (scaffold perspective); partially exit-capable through framework accumulation
 *   - Treaty Governance Mechanism: Institutional structure (institutional/arbitrage) — maintains performative compliance theater; persists through inertia rather than function; beneficiary of interpretation variance (absence of binding adjudication preserves institutional authority)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_state_interpretation, 0.58).
domain_priors:suppression_score(refugee_convention_state_interpretation, 0.68).
domain_priors:theater_ratio(refugee_convention_state_interpretation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_state_interpretation, extractiveness, 0.58).
narrative_ontology:constraint_metric(refugee_convention_state_interpretation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(refugee_convention_state_interpretation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_state_interpretation, tangled_rope).
narrative_ontology:human_readable(refugee_convention_state_interpretation, "Refugee Convention State Interpretation and Application Variance").
narrative_ontology:topic_domain(refugee_convention_state_interpretation, "international_law/humanitarian_protection").

domain_priors:requires_active_enforcement(refugee_convention_state_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_state_interpretation, restrictive_interpretation_states).
narrative_ontology:constraint_beneficiary(refugee_convention_state_interpretation, border_control_institutions).
narrative_ontology:constraint_victim(refugee_convention_state_interpretation, asylum_seekers).
narrative_ontology:constraint_victim(refugee_convention_state_interpretation, protection_gap_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ASYLUM SEEKER (SNARE) — Structurally trapped with no exit alternatives. Faces the constraint's full force: restrictive state interpretation of refugee definition denies protection regardless of circumstances. Lacks representation in treaty negotiation or enforcement mechanisms. Cannot organize collectively across jurisdictions. Suppression is maximum — legal barriers, geographic mobility restrictions, and information asymmetry prevent escape. Effective extraction is high (χ ≈ 0.85) despite moderate base extractiveness, because the trapped exit option produces high directionality coefficient.
constraint_indexing:constraint_classification(refugee_convention_state_interpretation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HUMANITARIAN ORGANIZATION (TANGLED ROPE) — Coordination function: organizations (UNHCR, national NGOs) genuinely coordinate asylum assessment and protection services. Extraction function: states' restrictive interpretation creates resource scarcity — organizations must divert capacity from protection into advocacy, documentation, and legal challenges. Agents experience mixed benefit (coordination enables their mission) and cost (narrowed legal scope reduces whom they can protect). Can partially exit through private funding or alternative jurisdictions, but face career and institutional constraints.
constraint_indexing:constraint_classification(refugee_convention_state_interpretation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RESTRICTIVE INTERPRETATION STATE (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination: interpreting refugee definition narrowly coordinates state sovereignty with treaty participation. No enforcement mechanism prevents narrow interpretation; states retain interpretive authority. Can exit costlessly (withdraw from treaty, reinterpret unilaterally). Net extraction flows toward this agent. Effective extraction is negative (χ ≈ -0.15) — the constraint subsidizes the state's security and border control objectives.
constraint_indexing:constraint_classification(refugee_convention_state_interpretation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PROGRESSIVE STATE WITH BROADER INTERPRETATION (TANGLED ROPE) — Coordination function: broader interpretation of refugee definition coordinates collective action on protection norms and regional burden-sharing. Extraction function: restrictive interpretation by neighboring states externalizes migration burden — receiving asylum seekers fleeing both conflict and persecution-adjacent harms requires domestic social capacity. States can exit through border controls or policy shifts, but face reputational and normative costs. Experience both the coordination benefit (participating in international protection regime) and extraction cost (asymmetric burden from neighboring states' restrictive interpretations).
constraint_indexing:constraint_classification(refugee_convention_state_interpretation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: INTERNATIONAL COORDINATION COALITION (SCAFFOLD) — Organized actors (UN General Assembly resolutions, regional protocols, humanitarian networks) are building supplementary protection frameworks (Convention Against Torture, International Covenant on Civil and Political Rights, regional asylum directives) that constrain the interpretive variance. The original 1951 Convention's underspecified refugee definition is being worked around rather than reformed — parallel frameworks create alternative protection pathways. Sunset logic: as regional and supplementary frameworks mature, the restrictive interpretation's leverage declines. States cannot escape treaty participation via narrow interpretation if other obligations create overlapping protections.
constraint_indexing:constraint_classification(refugee_convention_state_interpretation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TREATY GOVERNANCE MECHANISM (PITON) — The 1951 Convention's governance structure is substantially performative. The Committee of Experts provides interpretation guidance (the Handbook) but lacks enforcement power. State reporting is ritualistic — implementation accountability is nonbinding. The mechanism persists through institutional inertia: states maintain treaty participation to signal humanitarian values while interpretive practice diverges widely. Theater ratio high (0.65) because the gap between nominal obligations and enforced practice is large. No beneficiary actively maintains the ritual; it persists because exit (treaty amendment) is costlier than compliance theater.
constraint_indexing:constraint_classification(refugee_convention_state_interpretation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a civilizational view, interpretive variance among treaty signatories appears immutable: sovereign states retain authority over interpretation; decentralized enforcement creates structural indeterminacy; treaty language ('persecution,' 'due to membership in a social group') is inherently polysemic. This perspective naturalizes the variance as a feature of international law itself. However, structural data reveals this as a false summit: the variance is not inherent but engineered — states actively choose narrow interpretation and resist supplementary frameworks. The mountain perspective mistakes contingent institutional choice for immutable structural fact.
constraint_indexing:constraint_classification(refugee_convention_state_interpretation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_state_interpretation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(refugee_convention_state_interpretation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(refugee_convention_state_interpretation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_state_interpretation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(refugee_convention_state_interpretation, TR),
    TR >= 0.70.

:- end_tests(refugee_convention_state_interpretation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. States with restrictive interpretations extract tangible benefits (reduced asylum numbers, maintained border control discretion, social policy alignment with anti-immigration constituencies). However, the extraction is not total — the constraint does obligate states to participate in the international protection regime and provide some procedural review. The upward drift from 0.42 to 0.58 over 40 years reflects intensifying political pressure on asylum and corresponding tightening of interpretation without formal treaty amendment. Suppression (0.68): High. Multiple barriers prevent asylum seekers from challenging restrictive interpretations: no standing in treaty interpretation processes, information asymmetries about admissibility criteria, geographic barriers to reaching asylum territory, legal barriers to appeal across jurisdictions, and political barriers to mobilization. But suppression is not total — humanitarian organizations, some states, and international courts provide partial counterbalance. Theater ratio (0.65): Moderate-high. The treaty governance mechanism is substantially performative: states signal compliance through ratification and reporting, but implementation diverges widely. The Committee of Experts' guidance is non-binding; state practice varies by orders of magnitude (some states approve 60%+ of asylum claims, others 5%+); no mechanism enforces interpretation consistency. Theater has increased as the gap between nominal obligations (protect refugees) and actual practice (restrict interpretations) has widened. The rituals persist (reporting, committee meetings) because exit (treaty amendment, withdrawal) is costlier than compliance theater.
 *
 * PERSPECTIVAL GAP:
 *   The gap between snare (asylum seeker) and rope (restrictive state) perspectives is maximal. Both occupy the same institutional context and face the same legal text, yet classify the constraint differently. This reveals that the indexical position determines perception more than the constraint's objective features. The asylum seeker cannot escape the constraint without legal status; the state cannot be forced to interpret broadly. The progressive state and humanitarian organization occupy middle positions, seeing mixed extraction/coordination. The analytical observer risks naturalizing the divergence as immutable — 'state sovereignty is inherent to international law' — but structural data shows that supplementary frameworks, case law, and regional coordination are actively constraining variance. The scaffold and piton perspectives reveal that the constraint is engineered and can be unmade through mechanism design, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each actor's structural relationship to the constraint. Asylum seekers (trapped, powerless): d ≈ 0.95, f(d) ≈ 1.42, maximum experienced extraction χ despite moderate base ε. Restrictive states (arbitrage, institutional beneficiaries): d ≈ 0.05, f(d) ≈ -0.12, negative experienced extraction (constraint subsidizes them). Progressive states (mobile, secondary targets): d ≈ 0.55, f(d) ≈ 0.75, moderate-high extraction. Humanitarian organizations (constrained, mixed function): d ≈ 0.50, f(d) ≈ 0.65, moderate extraction. The disparity in experienced χ despite similar base ε reveals that the indexical position (power, exit capacity) is the primary determinant of extraction magnitude, not the constraint's intrinsic features. An asylum seeker in a restrictive state experiences χ ≈ 0.85; an institutional beneficiary in the same state experiences χ ≈ -0.15. The constraint is the same; the experience is structurally inverse.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint coordinates divergent state interests (some states want restrictive interpretation, others want protection coordination) while enabling the restrictive coalition to extract unilateral benefits through interpretive authority without formal treaty amendment. The coordination function is genuine but asymmetric: the treaty coordinates state participation, but the coordination produces extractive outcomes because enforcement is decentralized and absent. The tangled rope classification captures this: both coordination (treaty participation, international regime) and extraction (interpretive divergence enabling unilateral benefit) are structurally real. The constraint is not pure coordination (rope) because the absence of binding interpretation mechanisms enables extractive divergence. It is not pure extraction (snare) because states genuinely coordinate on burden-sharing norms and procedural standards, even where interpretations diverge. The theater ratio (0.65) captures that the governance mechanism performs compliance signaling while actual implementation diverges. The mandatrophy resolution is that all six types are legitimate readings from their respective indexical positions, and the presheaf over the observation site — the full set of perspectival readings — is the analytical answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persecution_definition_boundary,
    'What constitutes ''persecution'' under Article 1(A)(2) of the 1951 Convention? Does persecution include generalized violence, economic deprivation, or only targeted state action?',
    'Comparative analysis of state practice, UNHCR case law, and regional court decisions; identification of common vs divergent interpretation clusters; statistical analysis of approval rates by interpretation type',
    'Narrow interpretation (state-targeted acts only): snare classification confirmed; asylum denials increase by estimated 40-60% globally. Broad interpretation (harms from any powerful actor): constraint shifts toward rope; protection expands to persecution-adjacent harms. Middle interpretation (state-attributable harms with weak nexus): tangled rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(persecution_definition_boundary, empirical, 'Definition of persecution in refugee law').

omega_variable(
    social_group_membership_clarity,
    'Does ''persecution due to membership in a particular social group'' include gender-based violence, gang violence, domestic abuse, or only formally defined groups (ethnicity, caste, sexual orientation)?',
    'Case law precedent analysis; comparative jurisprudence across EU, US, Canadian, and Australian immigration courts; identification of divergence points and approval rate variation by interpretation',
    'If narrow (only formal groups): extinguishes protection for gender persecution, gang violence victims, family-based harm — extraction increases (snare deepens). If broad (social harm vectors): protection expands — constraint shifts toward rope, suppression decreases.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_group_membership_clarity, empirical, 'Interpretation of ''particular social group'' in refugee law').

omega_variable(
    internal_relocation_alternative,
    'Does a viable internal relocation alternative (escape to safer region within home country) extinguish refugee status, and under what conditions is relocation truly viable?',
    'Empirical assessment: violence/persecution reach of government or non-state actors in proposed relocation zones; economic viability of relocation; state capacity to protect in alternative regions; case law review of ''internal relocation'' denials and subsequent protection outcomes',
    'If strict viability requirement: many denials reversed on appeal — suppression decreases, constraint shifts toward rope. If lenient viability (mere theoretical safety): extraction increases — snare deepens for applicants in weak-state jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_relocation_alternative, empirical, 'Viability of internal relocation as persecution escape').

omega_variable(
    nexus_causation_standard,
    'What causation standard applies between persecution and protected ground (race, religion, nationality, political opinion, social group)? Must persecution be direct/intentional result of protected ground, or is correlation/coincidence sufficient?',
    'Comparative case law analysis across jurisdictions; examination of approval rates under strict vs loose nexus standards; applicant outcome tracking by nexus interpretation type',
    'Strict nexus (persecution must target protected ground explicitly): extraction increases (snare deepens). Loose nexus (persecution coincident with protected ground): constraint shifts toward rope. This determines whether regime change violence, civil war, or generalized conflict can ground refugee claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nexus_causation_standard, empirical, 'Causation standard between persecution and protected ground').

omega_variable(
    state_capacity_attribution,
    'When non-state actors (rebels, gangs, private militias) commit persecution, can it be attributed to the state for refugee purposes? What state action or negligence threshold triggers attribution?',
    'Jurisprudential analysis of state attribution doctrine; case law review of persecution by non-state actors; comparative examination of EU, common law, and international humanitarian law approaches to attribution',
    'Strict attribution (only state action, not negligence): protection gaps for persecution by powerful non-state actors (gangs, rebels) — extraction increases. Loose attribution (state responsibility for failure to protect): protection expands to non-state persecution — constraint shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_attribution, empirical, 'Attribution of non-state persecution to state').

omega_variable(
    enforcement_mechanism_gap,
    'Why does the 1951 Convention lack a binding dispute resolution mechanism? Is this a feature (state autonomy preservation) or a design flaw enabling extractive interpretation?',
    'Historical analysis of treaty negotiation; examination of ratification debates and sovereignty concerns; comparative analysis with enforcement mechanisms in other human rights treaties; assessment of whether optional compulsory jurisdiction would change state behavior',
    'If feature: enforcement gap is inherent to international law structure — mountain perspective partially confirmed. If flaw: mechanism could be patched via Optional Protocol or new Convention — constraint is engineered, not immutable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_gap, conceptual, 'Why the Convention lacks binding enforcement mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_state_interpretation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refug_tr_t0, refugee_convention_state_interpretation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(refug_tr_t20, refugee_convention_state_interpretation, theater_ratio, 20, 0.6).
narrative_ontology:measurement(refug_tr_t40, refugee_convention_state_interpretation, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(refug_be_t0, refugee_convention_state_interpretation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(refug_be_t20, refugee_convention_state_interpretation, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(refug_be_t40, refugee_convention_state_interpretation, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_state_interpretation, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_state_interpretation, internally_displaced_person_status_gap).
narrative_ontology:affects_constraint(refugee_convention_state_interpretation, burden_sharing_norm_violation).
narrative_ontology:affects_constraint(refugee_convention_state_interpretation, humanitarian_access_restriction).

% DUAL FORMULATION NOTE:
% The refugee convention interpretation variance is a structurally distinct constraint from specific refugee status determinations. Base extractiveness reflects the asymmetry of interpretation authority, not the outcome of individual claims. Downstream constraints (IDP status gaps, burden-shifting) are enabled by this constraint's structural indeterminacy and lack of enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_state_interpretation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
