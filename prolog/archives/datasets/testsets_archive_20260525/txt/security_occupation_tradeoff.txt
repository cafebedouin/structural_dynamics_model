% ============================================================================
% CONSTRAINT STORY: security_occupation_tradeoff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_security_occupation_tradeoff, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: security_occupation_tradeoff
 *   human_readable: Security-Occupation Tradeoff: Territorial Legitimacy and Coercive Control
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   The security-occupation tradeoff structures a decades-long constraint on
 *   territorial sovereignty, self-determination, and civilian security across
 *   the Israeli-Palestinian conflict. The constraint is defined by the
 *   intersection of two competing claims: the occupying state justifies
 *   military control of territory as necessary for security against
 *   existential threats; the occupied population claims a right to
 *   self-determination and freedom from coercive rule regardless of security
 *   arguments. This is not a simple extraction mechanism — the occupying
 *   military establishment perceives genuine coordination functions
 *   (preventing attacks, securing borders, managing demographic control). Yet
 *   the same institutional arrangement is experienced as pure coercion
 *   (Snare) by the trapped population with no exit options. The constraint
 *   exhibits all six classification types from different observer positions,
 *   making it a diagnostic case for how indexical classification reveals
 *   structural perspectives rather than objective constraint types. The base
 *   extractiveness has drifted upward from 0.52 (1967) to 0.68 (2022),
 *   suggesting that the coordination component (initial security claim) has
 *   given way to extraction component (territorial control, resource
 *   extraction, demographic engineering). The theater ratio has similarly
 *   increased from 0.35 to 0.58, indicating that legal/diplomatic
 *   justifications have become more prominent as direct coercive mechanisms
 *   have routinized. This trajectory is consistent with a Snare
 *   interpretation: the constraint persists because suppression mechanisms
 *   are effective, not because coordination benefits justify the arrangement.
 *
 * KEY AGENTS:
 *   - Occupied Civilian Population: Primary victim (powerless/trapped) — subject to movement restrictions, permit systems, military law; bears full suppression with minimal exit options; experiences constraint as Snare
 *   - Occupying Military Establishment: Primary beneficiary (institutional/arbitrage) — controls territory, determines security policy, maintains monopoly on force; experiences constraint as coordination solution; classified as Rope
 *   - Occupying State Political Leadership: Secondary beneficiary (institutional/constrained) — gains territorial control and geostrategic positioning; constrained by international pressure and settlement constituency; experiences as Tangled Rope
 *   - Palestinian Authority / Gaza Administration: Organized victim (organized/constrained) — maintains administrative functions within occupation framework; dependent on occupier for authority; constrained by occupation limits; classified as Tangled Rope
 *   - Settler Establishment: Tertiary beneficiary (powerful/mobile) — gains territorial access, resource benefits, military security; classified as Tangled Rope; powerful but increasingly constrained by international pressure
 *   - International Legal Order: Institutional observer (institutional/analytical) — condemns occupation, issues resolutions, maintains legal challenge; lacks enforcement mechanisms; classified as Piton (degraded ritual)
 *   - Realist Analyst: Civilizational observer (analytical/analytical) — sees security necessity as natural law; risks false summit (naturalizing contingent institutional arrangement); classified as Mountain from this perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(security_occupation_tradeoff, 0.68).
domain_priors:suppression_score(security_occupation_tradeoff, 0.75).
domain_priors:theater_ratio(security_occupation_tradeoff, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(security_occupation_tradeoff, extractiveness, 0.68).
narrative_ontology:constraint_metric(security_occupation_tradeoff, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(security_occupation_tradeoff, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(security_occupation_tradeoff, tangled_rope).
narrative_ontology:human_readable(security_occupation_tradeoff, "Security-Occupation Tradeoff: Territorial Legitimacy and Coercive Control").
narrative_ontology:topic_domain(security_occupation_tradeoff, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(security_occupation_tradeoff).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(security_occupation_tradeoff, fixed_text).
narrative_ontology:cs_authority_grounding(security_occupation_tradeoff, lineage).
narrative_ontology:cs_interpretation_layer_present(security_occupation_tradeoff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(security_occupation_tradeoff, occupying_military_establishment).
narrative_ontology:constraint_beneficiary(security_occupation_tradeoff, occupied_territory_security_apparatus).
narrative_ontology:constraint_victim(security_occupation_tradeoff, civilian_population_under_occupation).
narrative_ontology:constraint_victim(security_occupation_tradeoff, international_legal_order).
narrative_ontology:constraint_victim(security_occupation_tradeoff, self_determination_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OCCUPIED CIVILIAN (SNARE) — Structurally trapped. Movement restricted by checkpoints, curfews, and permit systems. Cannot exit the territorial jurisdiction without occupying authority permission. Suppression is total at the immediate horizon: military law supersedes civilian law; security rationales override due process. High experienced extraction with minimal coordination benefit — the occupying force justifies control as security necessity, but the civilian experiences only coercion with no exit pathway.
constraint_indexing:constraint_classification(security_occupation_tradeoff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: OCCUPIED POLITY (TANGLED ROPE) — Organized actors with constrained exit. The PA maintains some administrative coordination functions (civil services, law enforcement in Area A/B zones) that nominally serve the civilian population, yet these functions are embedded within the occupation's security apparatus and dependent on continued cooperation with the occupying authority. Genuine coordination (shared infrastructure, joint security committees) coexists with asymmetric extraction (ultimate authority retained by occupier; budget and policy dependent on occupier approval). Exit to full sovereignty is politically demanded but militarily and economically constrained.
constraint_indexing:constraint_classification(security_occupation_tradeoff, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: OCCUPYING MILITARY (ROPE) — Institutional actor with maximum exit optionality (arbitrage). Can withdraw, redeploy, or adjust the scope of occupation at state decision. Experiences the constraint as a coordination problem to solve: security threats require territory control; civilian administration requires some degree of order and cooperation. The military sees itself as solving a genuine coordination problem (physical security, border control) while the occupied perceive extraction. From the occupier's institutional perspective, the constraint is administrative necessity with coordination functions — the snare emerges only from other perspectives.
constraint_indexing:constraint_classification(security_occupation_tradeoff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OCCUPYING STATE POLITICS (TANGLED ROPE) — Institutional but constrained exit. Maintains genuine coordination functions (territorial integration, resource allocation, demographic management) while also extracting political benefit (territorial control, settlement expansion, geostrategic positioning). Exit would require concessions on security claims and territorial claims — politically costly domestically. Genuine coordination coexists with asymmetric benefit extraction. More constrained than the military (political costs of withdrawal) but less trapped than occupied civilians.
constraint_indexing:constraint_classification(security_occupation_tradeoff, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL ORDER (PITON) — Institutional but degraded. The occupation violates international humanitarian law and self-determination norms (high theater via legal proceedings, advisory opinions, resolutions). Yet enforcement mechanisms are absent, and the constraint persists through institutional inertia: the Security Council is gridlocked, the ICC lacks enforcement authority, and alternative dispute mechanisms have failed. The international legal order's condemnation is substantially performative — the ritual of legal challenge continues despite zero functional capacity to alter the constraint. Theater ratio high (0.60+) because legal procedures create the appearance of enforcement without enforcement capacity.
constraint_indexing:constraint_classification(security_occupation_tradeoff, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: REALIST OBSERVER / SECURITY NECESSITY (MOUNTAIN) — From a civilizational/universal perspective, the security-occupation tradeoff appears as an immutable constraint of political economy: territorial control is necessary for military security; security dilemmas generate automatic coercive responses; the logic is structural rather than contingent. This perspective naturalizes the occupation as a necessary consequence of geopolitical reality. However, the structural data reveals this as a false summit candidate — identifiable beneficiaries (military, political establishment) gain from naturalizing the occupation as inevitable, and the suppression mechanisms are contingent on institutional arrangements, not laws of nature.
constraint_indexing:constraint_classification(security_occupation_tradeoff, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: SETTLER ESTABLISHMENT (TANGLED ROPE) — Powerful actors with mobile exit but declining willingness to exercise it. Benefits from occupation (territorial acquisition, resource access, security provided by military). Provides coordination benefit to the occupying state (demographic facts on the ground, political constituency for continued occupation). Yet increasingly constrained as international pressure, internal Palestinian resistance, and global delegitimization increase exit costs. Genuine coordination with the occupying state coexists with extraction from the occupied population.
constraint_indexing:constraint_classification(security_occupation_tradeoff, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(security_occupation_tradeoff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(security_occupation_tradeoff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(security_occupation_tradeoff, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(security_occupation_tradeoff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(security_occupation_tradeoff, TR),
    TR >= 0.70.

:- end_tests(security_occupation_tradeoff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, and rising. The initial 1967 value (0.52) reflected genuine security coordination mixed with territorial control extraction. The upward drift to 0.68 reflects that the coordination component (preventing attacks through territorial denial) has become less credible or necessary, while the extraction component (demographic control, settlement expansion, resource access, geostrategic positioning) has become more explicit. The constraint is not primarily a security mechanism anymore — it is a control mechanism justified by security claims. Suppression (0.75): Very high and sustained. Movement restrictions (checkpoints, permit systems, curfews), administrative detention, military law, settlement-imposed resource constraints, and monopoly control over entry/exit. Suppression is not declining over the measurement interval — it has routinized into administrative systems. Theater ratio (0.58 at present, rising from 0.35): Moderate-to-high. The occupation is increasingly justified through legal/diplomatic processes: UN resolutions (performative because unenforced), ICJ advisory opinions (performative because non-binding), peace negotiations (performative because asymmetric power prevents genuine agreement). The theater has grown as direct coercion has become routinized — the occupation no longer needs to be explicitly justified to individual actions; it is embedded in bureaucratic procedure. Claimed type (Tangled Rope): Justified by presence of genuine coordination functions (security, border control, demographic management) coexisting with asymmetric extraction (territorial control, resource extraction, political benefit). The coordination component is real but serving the beneficiary's interests rather than the occupied population's. Mandatrophy resolved: Yes. The constraint resolves mandatrophy by showing that the classification (Tangled Rope) is the correct synthesis: neither pure Rope (ignoring extraction) nor pure Snare (ignoring coordination). The mandatrophy emerges from the perspective gap: the beneficiary sees Rope, the victim sees Snare, the analytical observer risks seeing Mountain (false summit). The Tangled Rope classification captures all three dynamics in one frame.
 *
 * PERSPECTIVAL GAP:
 *   Enormous. The occupied civilian experiences the constraint as pure coercion with no exit pathway (Snare, d=0.95) — checkpoints block movement, military law supersedes civilian law, permits determine livelihood access. The military establishment experiences the constraint as a coordination solution to security threats (Rope, d=0.15) — territory control prevents attacks, borders are secured, demographic risks are managed. The political establishment sees Tangled Rope (d=0.45) — genuine coordination (security, territorial integration) coexists with extraction (territorial control, settlement expansion). The international legal order sees Piton (d=0.72) — legality is challenged repeatedly but enforcement is absent; the constraint persists through institutional inertia. The settler establishment sees Tangled Rope (d=0.35) — benefits from occupation while providing coordination function (demographic/political anchoring). The realist observer risks seeing Mountain (d=0.72, mapped to analytical perspective) — security dilemmas are universal structures, occupation is natural necessity. This cascading gap from d=0.15 (beneficiary) to d=0.95 (victim) is the diagnostic signature of the constraint's extractive character: different observers perceive the same phenomenon as coordination (low d) or extraction (high d) based entirely on their structural position relative to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary derivation: The occupying military establishment is classified as institutional/arbitrage. Arbitrage exit means the military could withdraw, redeploy, or adjust scope at state decision. The institutional power level combined with arbitrage exit produces canonical d=0.00 (full beneficiary). The sigmoid f(d) maps 0.00 → -0.12 (negative effective extraction — the constraint subsidizes this agent). The occupying political establishment is institutional/constrained — constrained by domestic political costs of withdrawal, international pressure, settlement constituencies. Constrained exit + institutional power produces derived d≈0.30, mapped by f(d) → 0.05 (low positive extraction, but much higher than arbitrage). Victim derivation: The occupied civilian is powerless/trapped. Trapped exit means no exit options exist at any cost; movement is controlled by permit, residence is controlled by law, employment is controlled by resource allocation. Powerless power + trapped exit produces canonical d=1.00 (full target). The sigmoid f(d) maps 1.00 → 1.42 (maximum experienced extractiveness). This d=1.00 is the ceiling of the framework — the trapped victim experiences the constraint at maximum intensity. The occupied polity (PA) is organized/constrained — constrained by dependence on occupier for authority, budget, and security cooperation, but organized enough to negotiate limited autonomy. Organized power + constrained exit produces derived d≈0.60, mapped by f(d) → 0.95 (high experienced extraction). The directionality overrides are NOT used here because the derivation chain (beneficiary/victim + power + exit → d → f(d) → chi) produces accurate classification gaps automatically. The perspectival difference between beneficiary (d=0.00-0.30) and victim (d=0.60-1.00) is built into the framework by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is the tension between (1) genuine coordination functions (security coordination, territorial administration, border management) that the occupying state performs and (2) asymmetric extraction (territorial control, resource extraction, political benefit, demographic engineering) that the same institutions accomplish. Neither Rope (coordination-only, classifying at chi ≈ 0.35) nor Snare (extraction-only, classifying at chi ≥ 0.66) captures the hybrid structure. Tangled Rope (chi = 0.40-0.90, requiring both coordination and extraction) is the resolution. The mandatrophy is resolved by acknowledging that the same institutional apparatus serves dual functions: genuine security coordination for the beneficiary population (Israeli civilians), and asymmetric extraction from the occupied population. The constraint is not 'really' one type hiding behind another — it is structurally both, and different observers perceive different weightings based on their position. The rising theater_ratio (0.35 → 0.58) indicates that the coordination justification has become increasingly performative over time: the initial claim was genuine security necessity (coordination argument), but as the constraint has stabilized and extraction mechanisms have become explicit (settlements, resource control, demographic policy), the coordination claim has become more theatrical — security is still invoked but is no longer the primary mechanism maintaining the constraint. This trajectory from genuine coordination (1967) toward extraction-with-performative-coordination (2022) is consistent with the upward extractiveness drift (0.52 → 0.68), suggesting that Snare classification would be more accurate today than Tangled Rope. However, the coordination functions have not disappeared — security threats are still real, border control is still necessary, demographic risks are still managed. The constraint therefore remains Tangled Rope, but with warning that the temporal drift may eventually cross into pure Snare classification if extraction mechanisms continue to accumulate and coordination functions become fully performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    shared_kernel_or_distinct_kernels,
    'Is this a single contested kernel (territorial legitimacy) read differently by two parties, or two entirely distinct kernels with no shared normative substrate?',
    'Test for minimal coherence: Can both parties'' legitimacy claims be expressed in a shared language of territorial law, self-determination, and security? If yes, one kernel with competing readings. If no, distinct kernels require separate constraint stories. Empirical test: Do peace negotiations operate on a single kernel (border definitions, sovereignty arrangements) or do they fail because the parties cannot even agree on what is being negotiated?',
    'If one contested kernel: both perspectives operate on a shared normative substrate (international law, territorial principle) and could theoretically converge on interpretation. If distinct kernels: no shared substrate; no interpretation can reconcile them; the constraint decomposes into two separate stories with incomparable legitimacy grounds. Classification strategy changes: one kernel → one constraint story; distinct kernels → two constraint stories with network link.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(shared_kernel_or_distinct_kernels, conceptual, 'Whether territorial legitimacy is one contested kernel or two distinct kernels').

omega_variable(
    extractiveness_temporal_drift,
    'Has the extractiveness of the occupation increased over the 55+ year timeline (1967-2026), or has it remained structurally constant with oscillations in intensity?',
    'Temporal analysis of suppression mechanisms (checkpoint density, permit requirements, settlement expansion, incitement control), theater ratio (UN resolutions, legal processes, rhetorical justifications), and base extractiveness (resource extraction, military expenditure burden, civilian harm rates). Compare era structures: 1967-1987 (military occupation), 1987-2000 (first intifada and negotiation), 2000-2005 (second intifada and Gaza withdrawal), 2005-present (fragmented occupation). If extractiveness trends upward: snare classification gains support. If constant with oscillations: tangled_rope gains support.',
    'If upward drift: treat as evidence of escalating extraction mechanism (rent-seeking layered onto coordination). If constant: treat as evidence of hybrid structure where coordination and extraction coexist structurally, not temporally sequenced. Measurement interval and omega resolution directly inform the mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_temporal_drift, empirical, 'Whether occupation extractiveness increases or oscillates structurally').

omega_variable(
    security_claim_empirical_validity,
    'To what degree does the occupying state''s security claim (that occupation is necessary to prevent attacks on civilians) correspond to measurable security outcomes? Is the security improved by occupation, or does the occupation create the threat dynamic it claims to prevent?',
    'Time-series analysis comparing security incident rates under different occupation intensity levels; counterfactual modeling of security outcomes without occupation; analysis of attack patterns relative to occupation measures (checkpoints, settlements, military presence). Does reducing occupation intensity (e.g., Area A autonomy) correlate with increased security threats, or with decreased threats?',
    'If security claim is empirically grounded: occupation qualifies as genuine coordination mechanism (security) with extraction overlay (control benefit). If security claim is inconsistent with data: occupation is pure extraction justified by false necessity claim (reclassifies toward snare, reduces rope coordination component). This determines whether the beneficiary framing (security coordination) is genuine or theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_claim_empirical_validity, empirical, 'Whether occupation''s security claims are empirically valid or create the threat dynamics they claim to prevent').

omega_variable(
    false_summit_mountain_detection,
    'Is the realist observer''s ''security necessity'' perspective a genuine natural law (security dilemmas are structural), or a contingent institutional arrangement that benefits specific actors?',
    'Historical comparative analysis: Do other polities facing comparable security threats resort to territorial occupation, or do they use alternative mechanisms (international alliance, deterrence, intelligence, border control without internal occupation)? Does the specific geographic/historical context (Palestinian-Israeli conflict) differ fundamentally from other security dilemmas, or is the occupation a contingent choice within a range of security strategies?',
    'If security dilemma is universal: mountain classification is appropriate; security-occupation link is immutable. If occupation is contingent: mountain is false summit; the realist perspective naturalizes what is actually a political choice; the constraint reclassifies to snare or tangled_rope from the analytical observer''s perspective. FSM engine will detect beneficiaries (military establishment, political leadership) and trigger reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_mountain_detection, empirical, 'Whether security necessity is a natural law or a false summit naturalizing contingent institutional arrangements').

omega_variable(
    legitimacy_incommensurability,
    'Are the two polities'' legitimacy claims logically incommensurable (no shared truth conditions, no possible integration), or merely incompatible (mutually exclusive but expressible in a common language)?',
    'Linguistic and philosophical analysis: Can both claims be made true in the same speech community? Can they be resolved by new evidence, historical discovery, or reinterpretation? If incommensurable: the constraint decomposes into two stories with distinct kernels. If merely incompatible: one story with two readings of a contested kernel is appropriate.',
    'If incommensurable: current story is incomplete; generate two separate constraint stories (Israeli security-occupation_legitimacy_claim and Palestinian territorial_self_determination_legitimacy_claim) with network link but distinct ε values, distinct beneficiary/victim structures, and distinct analytical coherence. If merely incompatible: one story with explicit kernel_context noting the competing readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_incommensurability, conceptual, 'Whether territorial legitimacy claims are logically incommensurable or merely incompatible').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the measured suppression (0.75) is structural (military checkpoints, legal restrictions, resource control) versus internalized (internalized powerlessness, normalized coercion, cognitive capture by security narratives)?',
    'Post-withdrawal ethnographic analysis (Gaza 2005-2006 period shows some reduction in external suppression; compare civilian behavior/agency trajectories to pre-withdrawal baseline). Psychometric measurement of learned helplessness, cognitive closure, and identity fusion with victimhood narratives. Longitudinal tracking of agency recovery vs. stagnation after removal of structural barriers.',
    'If primarily structural: exit from occupation would produce rapid suppression reduction. If substantially internalized: suppression persists after structural barriers are removed; the constraint requires both military withdrawal AND cognitive deprogramming. Classification impact: if internalized, identity_locked exit options become relevant; the occupied polity''s classifications shift from purely trapped to identity_locked in some perspectives; mandatrophy resolution becomes more complex.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(security_occupation_tradeoff, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(security_occupation_theater_1967, security_occupation_tradeoff, theater_ratio, 0, 0.35).
narrative_ontology:measurement(security_occupation_theater_1982, security_occupation_tradeoff, theater_ratio, 15, 0.42).
narrative_ontology:measurement(security_occupation_theater_1997, security_occupation_tradeoff, theater_ratio, 30, 0.55).
narrative_ontology:measurement(security_occupation_theater_2007, security_occupation_tradeoff, theater_ratio, 40, 0.58).
narrative_ontology:measurement(security_occupation_theater_2022, security_occupation_tradeoff, theater_ratio, 55, 0.58).

% Extraction over time
narrative_ontology:measurement(security_occupation_extractiveness_1967, security_occupation_tradeoff, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(security_occupation_extractiveness_1982, security_occupation_tradeoff, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(security_occupation_extractiveness_1997, security_occupation_tradeoff, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(security_occupation_extractiveness_2007, security_occupation_tradeoff, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(security_occupation_extractiveness_2022, security_occupation_tradeoff, base_extractiveness, 55, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(security_occupation_tradeoff, enforcement_mechanism).
narrative_ontology:affects_constraint(security_occupation_tradeoff, international_humanitarian_law_compliance).
narrative_ontology:affects_constraint(security_occupation_tradeoff, palestinian_state_capacity_development).
narrative_ontology:affects_constraint(security_occupation_tradeoff, israeli_civilian_security_policy).

% DUAL FORMULATION NOTE:
% The security-occupation tradeoff is the primary constraint linking Israeli security policy, Palestinian territorial claims, and international law enforcement. Upstream constraints (historical territorial claims, religious/cultural identity) affect how actors perceive this constraint's legitimacy but do not determine its structure. Downstream constraints (refugee rights, settler expansion, military court jurisdiction) are specific mechanisms within this broader tradeoff. The network defines the ecosystem of related constraints without decomposing the kernel ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(security_occupation_tradeoff, organized, 0.6).
constraint_indexing:directionality_override(security_occupation_tradeoff, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
