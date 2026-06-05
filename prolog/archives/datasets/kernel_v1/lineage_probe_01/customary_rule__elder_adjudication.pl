% ============================================================================
% CONSTRAINT STORY: customary_rule__elder_adjudication
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_customary_rule__elder_adjudication, []).

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
 *   constraint_id: customary_rule__elder_adjudication
 *   human_readable: Customary Rule: Elder Adjudication Moot
 *   domain: political/comparative_law/conflict_resolution
 *
 * SUMMARY:
 *   The elder adjudication moot represents a specific institutional form of
 *   customary rule where disputes are resolved through discussion directed
 *   toward reconciliation rather than binary judgment. This reading
 *   instantiates the judicial dimension of the broader customary rule kernel,
 *   emphasizing how conflict is processed and remedies are structured. The
 *   constraint exhibits Tangled Rope characteristics: it provides genuine
 *   coordination of relational restoration (beneficiary = community cohesion
 *   maintenance, elder authority preservation) while simultaneously
 *   extracting bright-line rights claims and suppressing adversarial finality
 *   (victims = bright-line rights claimants, winning-party finality). The
 *   extractiveness value (0.38) reflects that while real extraction
 *   occurs—stronger legal claims are compromised for relational balance—the
 *   mechanism is not as severe as pure extraction would be, because genuine
 *   coordination functions occur alongside the extraction. The suppression
 *   value (0.52) is moderate-high, capturing that alternatives (state
 *   adjudication, binary judgment) are available but socially costly. The
 *   theater ratio (0.48) is slightly below 0.50, indicating that the
 *   performative dimension (ritual reconciliation, ceremonial compensation)
 *   is roughly balanced with actual functional restoration of working
 *   relations.
 *
 * KEY AGENTS:
 *   - Bright-line rights claimants: Primary victims (powerless/trapped) — hold stronger legal claims but must compromise toward reconciliation; bear full extraction of legal advantage into relational debt
 *   - Elder council: Primary beneficiary (institutional/arbitrage) — authority preserved and reinforced through moot adjudication; experiences coordination rather than extraction
 *   - Community members embedded in dispute: Secondary victims and beneficiaries (moderate/constrained) — both benefit from relational restoration (genuine coordination) and bear extraction through compromise demands
 *   - Extended lineage authority: Secondary beneficiary (moderate/constrained) — coordinates resource flows and social reintegration while extracting respect and positioning
 *   - Community repair institution: Organized agent (organized/constrained) — sees moot as temporary institutional form with sunset as formal justice systems extend reach
 *   - Colonial administrative legacy: Institutional observer (institutional/arbitrage) — recognizes customary adjudication as degraded ritual maintained through administrative accommodation
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing specific institutional choice (reconciliation over finality) as immutable feature of human conflict resolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(customary_rule__elder_adjudication, 0.38).
domain_priors:suppression_score(customary_rule__elder_adjudication, 0.52).
domain_priors:theater_ratio(customary_rule__elder_adjudication, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(customary_rule__elder_adjudication, extractiveness, 0.38).
narrative_ontology:constraint_metric(customary_rule__elder_adjudication, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(customary_rule__elder_adjudication, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(customary_rule__elder_adjudication, tangled_rope).
narrative_ontology:human_readable(customary_rule__elder_adjudication, "Customary Rule: Elder Adjudication Moot").
narrative_ontology:topic_domain(customary_rule__elder_adjudication, "political/comparative_law/conflict_resolution").

domain_priors:requires_active_enforcement(customary_rule__elder_adjudication).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(customary_rule__elder_adjudication, 'c0cb69c1-371c-4048-bfa5-6f3070837cc5').
narrative_ontology:cs_kernel_codification('c0cb69c1-371c-4048-bfa5-6f3070837cc5', distributed).
narrative_ontology:cs_authority_grounding('c0cb69c1-371c-4048-bfa5-6f3070837cc5', practice).
narrative_ontology:cs_interpretation_layer_present('c0cb69c1-371c-4048-bfa5-6f3070837cc5').
narrative_ontology:cs_reading_relation('c0cb69c1-371c-4048-bfa5-6f3070837cc5', customary_rule__customary_land_tenure, coexists_with).
narrative_ontology:cs_reading_relation('c0cb69c1-371c-4048-bfa5-6f3070837cc5', customary_rule__lineage_chieftaincy, coexists_with).
narrative_ontology:cs_axiom('c0cb69c1-371c-4048-bfa5-6f3070837cc5', foundational, reconciliation_over_finality).
narrative_ontology:cs_axiom_status(reconciliation_over_finality, holdable).
narrative_ontology:cs_axiom_grounding('c0cb69c1-371c-4048-bfa5-6f3070837cc5', reconciliation_over_finality, instrumental).
narrative_ontology:cs_axiom('c0cb69c1-371c-4048-bfa5-6f3070837cc5', foundational, elder_epistemic_authority).
narrative_ontology:cs_axiom_status(elder_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('c0cb69c1-371c-4048-bfa5-6f3070837cc5', elder_epistemic_authority, conventional).
narrative_ontology:cs_reference_frame('c0cb69c1-371c-4048-bfa5-6f3070837cc5', community_relational_integrity_framework).
narrative_ontology:cs_drift_state('c0cb69c1-371c-4048-bfa5-6f3070837cc5', contemporary_state_legal_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c0cb69c1-371c-4048-bfa5-6f3070837cc5', '').
narrative_ontology:cs_kernel_id(customary_rule__elder_adjudication, customary_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(customary_rule__elder_adjudication, community_cohesion_maintenance).
narrative_ontology:constraint_beneficiary(customary_rule__elder_adjudication, elder_authority_preservation).
narrative_ontology:constraint_victim(customary_rule__elder_adjudication, bright_line_rights_claimants).
narrative_ontology:constraint_victim(customary_rule__elder_adjudication, winning_party_finality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BRIGHT-LINE RIGHTS CLAIMANT (SNARE) — A party with an objectively stronger legal claim (theft, breach, trespass under written law) becomes trapped in discussion-toward-reconciliation that demands compromise for relational restoration. Cannot exit; the moot's finality is binding via social pressure and shunning. Experiences the constraint as pure extraction of their legal advantage into relational debt. Maximum experienced extraction.
constraint_indexing:constraint_classification(customary_rule__elder_adjudication, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DISPUTING COMMUNITY MEMBER (TANGLED ROPE) — A member with legitimate grievance but also embedded in kinship networks experiences both extraction and coordination. The moot genuinely coordinates a restoration of working relations (a real coordination function) but also extracts compromise from the stronger party. Constrained by kinship and economic dependency within the community; cannot easily exit without relational costs.
constraint_indexing:constraint_classification(customary_rule__elder_adjudication, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: ELDER COUNCIL (ROPE) — Elders perceive the moot as pure coordination: a mechanism for maintaining the community's relational fabric and their own authority to adjudicate. The constraint serves to reinforce elder legitimacy and ensure disputes do not escalate to violence. Benefits from the moot through preserved authority and social capital. Experiences minimal extraction — the mechanism works in their favor.
constraint_indexing:constraint_classification(customary_rule__elder_adjudication, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: LINEAGE AUTHORITY (TANGLED ROPE) — Extended family or descent group authority (chief, lineage head) coordinates the distribution of compensation and social reintegration across broader kin networks while also extracting respect and deference through the moot process. Genuine coordination of resource flows and relational restoration alongside extraction of authority and social positioning.
constraint_indexing:constraint_classification(customary_rule__elder_adjudication, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: COMMUNITY REPAIR INSTITUTION (SCAFFOLD) — Organized agents (community councils, reconciliation practitioners, restorative justice reformers) see the moot as a temporary institutional form that will sunset as formal justice systems extend reach and written law becomes the primary dispute resolution mechanism. The moot is a transitional support structure — it serves a real function now but its extractive suppression of binary outcomes will eventually be superseded by rights-based adjudication or hybrid institutional forms.
constraint_indexing:constraint_classification(customary_rule__elder_adjudication, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: COLONIAL ADMINISTRATIVE LEGACY (PITON) — Formal state institutions (courts, police, administration) that recognize customary adjudication as part of a dual legal system see the moot as a degraded ritual with low functional verification. The moot persists through institutional accommodation and inertia rather than because it actually works — state courts are the 'real' adjudication mechanism, and customary moots are maintained for administrative convenience and cultural tolerance. Theater ratio high; actual enforcement depends on state backing.
constraint_indexing:constraint_classification(customary_rule__elder_adjudication, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the moot represents an immutable property of human conflict resolution: all societies must balance justice (determining fault) against reconciliation (restoring relations), and the moot locks in a specific resolution of that eternal tension. The suppression of binary outcomes appears as an unchangeable feature of how communities maintain cohesion. However, the structural data (identifiable beneficiaries, extractive suppression, contestation over judicial form) reveals this as a false summit naturalizing a specific institutional arrangement.
constraint_indexing:constraint_classification(customary_rule__elder_adjudication, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(customary_rule__elder_adjudication_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(customary_rule__elder_adjudication, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(customary_rule__elder_adjudication, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(customary_rule__elder_adjudication, TR),
    TR >= 0.70.

:- end_tests(customary_rule__elder_adjudication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The elder moot suppresses and extracts bright-line rights outcomes, but this extraction is partially genuine coordination—relational restoration is a real function with measurable value (reduced dispute escalation, maintained kinship networks). The extractiveness is not as high as pure extraction (0.72+) because the mechanism serves both coordination and extraction simultaneously. The measurement shows slight rise over 50 units (0.28 → 0.38), indicating that as state law becomes more visible as an alternative, the extractive character of the moot becomes more salient—rights claimants increasingly perceive themselves as sacrificing legal advantage for relational compliance. Suppression (0.52): Moderate-high. Bright-line rights claimants cannot easily exit—social shunning, kinship obligations, and lack of access to state adjudication in some contexts create material barriers. But suppression is not total—state courts, written law, and exit migration are increasingly available. The measurement shows slight rise (0.48 → 0.52) as state systems become more accessible, making the moot's suppression of alternatives increasingly visible as enforcement rather than natural. Theater ratio (0.48): Moderate-low. The moot combines genuine relational work (understanding parties' needs, identifying compensation, rebuilding communication) with performative ritual (ceremonial speeches, witnesses, formalized apologies). The ratio is nearly balanced; the performative element is present but not dominant. The slight rise (0.42 → 0.48) reflects that as communities experience state adjudication, the ritual dimension of the moot becomes more salient—what once appeared as integral to legitimate resolution increasingly appears as ceremony.
 *
 * PERSPECTIVAL GAP:
 *   The bright-line rights claimant and the elder council have inverted classifications from the same structural data. The claimant sees Snare (pure extraction of their advantage); the elder sees Rope (pure coordination). This inversion is the diagnostic signal that classification depends on structural position: the claimant bears extraction; the elder benefits from it. The moderate agent (disputing community member) experiences the true structure—Tangled Rope—because they genuinely benefit from relational restoration while also bearing costs of compromise. The organized agent (community repair institution) sees the constraint as temporary (Scaffold) because they are actively building alternative pathways (hybrid judicial forms, written judgment paired with reconciliation). The state institutional observer sees the moot as degraded ritual (Piton)—performative rather than functionally effective—because state law has become the backdrop against which customary adjudication is now measured. The analytical observer risks the false summit (Mountain)—naturalizing the reconciliation-over-finality choice as an unchangeable feature of human societies—but the structural data (beneficiaries, extractive suppression, contestation with other judicial forms) reveals it as a specific institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position: beneficiaries of community cohesion (elders, lineage authorities) have low d values (experience low or negative effective extraction because the constraint serves them); victims bearing extraction (bright-line rights claimants) have high d values (experience maximum effective extraction). Community members caught between coordination benefits and extraction costs have moderate d values, resulting in mixed classifications. The piton perspective derives from the theater gate—state institutions perceive the moot as substantially performative (theater_ratio ≈ 0.48) compared to the state's own written procedures, so despite moderate extractiveness, the performative character dominates the classification. The mountain perspective applies analytical position through the canonical d fallback (0.73, producing f(d) ≈ 1.15), but the false summit detector will identify the declared beneficiaries as evidence that the constraint is constructed (benefits identifiable agents) rather than natural.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that Tangled Rope classification is valid—genuine coordination (relational restoration, community cohesion maintenance) coexists with asymmetric extraction (suppression of bright-line rights, extraction of compromise). The constraint satisfies all three Tangled Rope gates: (1) beneficiaries declared (community cohesion maintenance, elder authority); (2) victims declared (bright-line rights claimants); (3) requires_active_enforcement = true (the moot's finality depends on social enforcement via shunning and kinship pressure). The false summit (mountain perspective) is resolved by noting the structural data: identifiable beneficiaries, extractive suppression, and contestation with other judicial forms (state adjudication) all reveal that reconciliation-over-finality is a specific institutional choice, not a natural law. The perspectival gap (claimant sees Snare, elder sees Rope, moderate member sees Tangled Rope, observer sees Piton, analyst risks Mountain) is the expected signature of a genuinely hybrid constraint with both coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reconciliation_versus_rights_tradeoff,
    'Is the suppression of bright-line rights outcomes necessary to achieve reconciliation, or is it an extractive mechanism that uses reconciliation as justification?',
    'Longitudinal comparison: communities that adopt hybrid forms (moot discussion followed by written judgment that honors reconciliation) versus communities that maintain pure moots; measurement of relational stability and repeat-dispute rates in each model',
    'If tradeoff is genuine: the constraint is structural (Tangled Rope from community perspective). If suppression is unnecessary: the constraint becomes a pure extraction mechanism (Snare), and the ''reconciliation'' framing is epistemic cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconciliation_versus_rights_tradeoff, empirical, 'Whether suppression of rights finality is necessary for reconciliation').

omega_variable(
    elder_authority_source,
    'Is elder authority in the moot grounded in genuine community legitimacy or in extractive kinship control that uses consensus language as cover?',
    'Analysis of exit options for community members: can a disputing party opt for state court without severe social penalty? If exit cost is extreme, elder authority extraction is high; if exit is available, authority is more genuinely consensual.',
    'If authority is legitimate: classification trends toward Rope (elders) and Tangled Rope (members). If authority is extractive: classification trends toward Snare with elder capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elder_authority_source, empirical, 'Legitimacy source of elder authority in moot adjudication').

omega_variable(
    kernel_reading_contest,
    'This reading (elder_adjudication) emphasizes the judicial form; sibling readings emphasize economic form (customary_land_tenure) and political form (lineage_chieftaincy). Are these three aspects of a single unified constraint, or three distinct constraints observable from different angles?',
    'Structural decomposition analysis: if the extractiveness value changes materially when using land-tenure observable versus judicial-form observable, they are two distinct constraints per ε-invariance principle. Historical and ethnographic documentation of how communities partition these functions.',
    'If unified: the constraint is larger and its suppression value applies across all three functions. If decomposed: each reading is a separate constraint with its own ε and suppression, linked via network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether elder adjudication is one aspect of a unified customary rule constraint or a distinct constraint from land tenure and chieftaincy').

omega_variable(
    identity_lock_in_reconciliation,
    'Are bright-line rights claimants identity-locked into accepting reconciliation through internalized narratives (''I am a member of this community; membership requires accepting the moot''), or trapped by material barriers (social ostracism, kinship obligations)?',
    'Post-exit analysis: disputing parties who leave the community permanently or adopt state law — do they report that their identity shifted (identity-lock resolution) or that they escaped material constraints (trap resolution)? Post-moot suppression trajectories.',
    'If identity-locked: classification reflects cognitive capture; perspectives using identity_locked exit option change from mountain to rope at biographical time. If trapped: classification reflects material barriers; exit options remain trapped regardless of identity frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_reconciliation, empirical, 'Identity-lock versus material-trap mechanism in reconciliation suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(customary_rule__elder_adjudication, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cust_tr_t0, customary_rule__elder_adjudication, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cust_tr_t25, customary_rule__elder_adjudication, theater_ratio, 25, 0.45).
narrative_ontology:measurement(cust_tr_t50, customary_rule__elder_adjudication, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(cust_be_t0, customary_rule__elder_adjudication, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cust_be_t25, customary_rule__elder_adjudication, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(cust_be_t50, customary_rule__elder_adjudication, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cust_su_t0, customary_rule__elder_adjudication, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(cust_su_t25, customary_rule__elder_adjudication, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(cust_su_t50, customary_rule__elder_adjudication, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(customary_rule__elder_adjudication, resource_allocation).
narrative_ontology:affects_constraint(customary_rule__elder_adjudication, customary_rule__customary_land_tenure).
narrative_ontology:affects_constraint(customary_rule__elder_adjudication, customary_rule__lineage_chieftaincy).

% DUAL FORMULATION NOTE:
% The elder adjudication reading foregrounds the judicial form of customary rule (how disputes are resolved, what counts as remedy). This is structurally distinct from the land tenure reading (how property rights are allocated via community membership) and the chieftaincy reading (how authority is held and exercised via descent groups). All three readings share the broader kernel (customary rule as institutionalized alternative to written law), but they have different ε values reflecting their different observables. The elder_adjudication constraint (ε=0.38) focuses on the suppression of binary judgment; the land_tenure constraint (if decomposed) would have different ε reflecting the suppression of title-registry systems; the chieftaincy constraint (if decomposed) would have different ε reflecting the suppression of democratic election. These constraints are linked via network.affects_constraints to represent that challenging one reading (e.g., introducing written adjudication) creates pressure on the others (e.g., land tenure begins to require registration as state courts enforce title-based claims).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(customary_rule__elder_adjudication, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
