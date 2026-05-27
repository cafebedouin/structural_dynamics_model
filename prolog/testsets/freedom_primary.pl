% ============================================================================
% CONSTRAINT STORY: freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_freedom_primary, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: freedom_primary
 *   human_readable: Border Enforcement Against Freedom of Movement (Freedom-Primary Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates the freedom-primary reading of the contested
 *   border_normative_status kernel. The reading asserts that freedom of
 *   movement is a fundamental human right that borders impermissibly
 *   restrict, and that exclusion requires extraordinary justification
 *   grounded in compelling state interests. This reading stands in contest
 *   with two sibling readings: sovereignty-primary (borders as irreducible
 *   expressions of state authority) and qualified-sovereignty (borders
 *   permissible under specified conditions). From the freedom-primary
 *   perspective, the border enforcement apparatus extracts from excluded
 *   migrants by restricting their access to safe refuge, economic
 *   opportunity, and family reunification. The constraint appears as snare
 *   when viewed from the excluded migrant's structural position (powerless,
 *   trapped, no exit options). It appears as tangled-rope from the border
 *   state's perspective (genuine coordination functions—disease screening,
 *   identity verification—mixed with extraction). It appears as rope from
 *   global capital's perspective (borders enable selective mobility for the
 *   powerful while extracting from the powerless). The extractiveness has
 *   increased from 0.52 to 0.68 over the interval as border enforcement
 *   infrastructure has intensified (digitalization, biometric systems,
 *   detention capacity expansion) while humanitarian crisis drivers (climate
 *   displacement, regional conflict) have intensified exclusion pressures.
 *   The theater-ratio remains low (0.38) because border enforcement is
 *   substantive rather than performative—people are actually prevented from
 *   crossing, detained, deported, and died attempting border crossing—unlike
 *   more theatrical constraints where the ritual exceeds the actual
 *   mechanism.
 *
 * KEY AGENTS:
 *   - Excluded Migrants: Primary victims (powerless/trapped) — face absolute barrier to movement; birth location determines life trajectory
 *   - Asylum Seekers: Primary victims (powerless/trapped) — lack legal status pathways; face deportation to persecution
 *   - Internally Displaced Persons: Primary victims (powerless/trapped) — cannot cross borders to safety; trapped by internal conflict or climate displacement
 *   - Border State (Enforcement Apparatus): Institutional beneficiary (institutional/arbitrage) — benefits from territorial closure, labor-market control, political legitimacy; coordinates genuine screening functions alongside extraction
 *   - Global Capital and Professional Class: Secondary beneficiary (powerful/arbitrage) — experience borders as coordination mechanisms enabling selective mobility; arbitrage their way across barriers
 *   - Domestic Labor Market Workers: Secondary victims (moderate/constrained) — face wage competition narrative but actually locked into low-wage labor markets by mobility restriction alongside capital freedom
 *   - International Migration Rights Advocacy: Organized advocacy (organized/constrained) — generates normative counter-claims but lacks institutional enforcement leverage; maintains rights discourse through piton mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(freedom_primary, 0.68).
domain_priors:suppression_score(freedom_primary, 0.75).
domain_priors:theater_ratio(freedom_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(freedom_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(freedom_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(freedom_primary, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(freedom_primary, snare).
narrative_ontology:human_readable(freedom_primary, "Border Enforcement Against Freedom of Movement (Freedom-Primary Reading)").
narrative_ontology:topic_domain(freedom_primary, "political_philosophy/international_law/migration").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(freedom_primary, 'eafaf5cf-e303-47de-bd2f-537a39be9a55').
narrative_ontology:cs_created_at('eafaf5cf-e303-47de-bd2f-537a39be9a55', '').
narrative_ontology:cs_kernel_codification('eafaf5cf-e303-47de-bd2f-537a39be9a55', fixed_text).
narrative_ontology:cs_authority_grounding('eafaf5cf-e303-47de-bd2f-537a39be9a55', lineage).
narrative_ontology:cs_interpretation_layer_present('eafaf5cf-e303-47de-bd2f-537a39be9a55').
narrative_ontology:cs_kernel_id(freedom_primary, border_normative_status).
narrative_ontology:cs_reading_relation('eafaf5cf-e303-47de-bd2f-537a39be9a55', sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('eafaf5cf-e303-47de-bd2f-537a39be9a55', qualified_sovereignty, influences).
narrative_ontology:cs_axiom('eafaf5cf-e303-47de-bd2f-537a39be9a55', foundational, movement_fundamental_right).
narrative_ontology:cs_axiom_status(movement_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('eafaf5cf-e303-47de-bd2f-537a39be9a55', movement_fundamental_right, deontological).
narrative_ontology:cs_axiom('eafaf5cf-e303-47de-bd2f-537a39be9a55', foundational, territorial_closure_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(territorial_closure_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('eafaf5cf-e303-47de-bd2f-537a39be9a55', territorial_closure_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('eafaf5cf-e303-47de-bd2f-537a39be9a55', universal_human_rights_tradition).
narrative_ontology:cs_drift_state('eafaf5cf-e303-47de-bd2f-537a39be9a55', contemporary_nationalist_resurgence, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_victim(freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(freedom_primary, asylum_seekers).
narrative_ontology:constraint_victim(freedom_primary, internally_displaced_persons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED MIGRANT (SNARE) — Faces absolute barrier to exit: cannot move across borders without permission; domestic poverty/persecution/conflict cannot be escaped through movement. Birth location becomes a life sentence. Suppression is maximal — no legal recourse, no appeal, no alternative pathway. Extraction mechanism: wealth and security stratification is enforced and reproduced through border control.
constraint_indexing:constraint_classification(freedom_primary, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMESTIC LABOR MARKET WORKERS — SOVEREIGNTY READING (SNARE) — From the sovereignty-primary reading's perspective, these workers face extraction through wage competition from migrants. From freedom-primary, these workers are themselves victims of the border constraint: restricted movement keeps them locked in low-wage labor markets while capital moves freely. Their constrained exit reflects material barriers, not migration barriers per se. This perspective reveals the asymmetry: labor is immobile while capital is mobile, reproducing inequality across borders.
constraint_indexing:constraint_classification(freedom_primary, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BORDER STATE — ENFORCEMENT APPARATUS (TANGLED ROPE) — The state apparatus benefits from border enforcement (revenue, labor market control, political legitimacy through sovereignty claims) and coordinates genuine functions (disease control screening, criminal identity verification, labor regulation). However, the enforcement mechanism also extracts: detention infrastructure, deportation machinery, surveillance systems create a parallel extraction apparatus justified by the coordination function but exceeding it. State sees constraint as legitimately necessary (coordination frame) while targets see only extraction (snare frame).
constraint_indexing:constraint_classification(freedom_primary, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL CAPITAL AND PROFESSIONAL MOBILITY (ROPE) — Capital flows across borders with minimal friction; high-skill professionals (tech workers, executives, academics) access visa pathways denied to low-skill migrants. This agent experiences border constraints as coordination mechanisms: visa screening, trade protocols, intellectual property enforcement. Borders enable their movement through legitimacy (selective access) while restricting others. They experience arbitrage—the constraint subsidizes their mobility.
constraint_indexing:constraint_classification(freedom_primary, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER — NATURALIZED SOVEREIGNTY (MOUNTAIN) — From this analytical perspective, state borders appear as natural features of the international system: sovereign territorial organization is presented as an irreducible requirement of political order. Border control emerges as natural law, not as a contingent institutional arrangement. However, the false-summit detection will activate: the beneficiaries (border-enforcing states, capital, professional-class mobility) are identifiable, and the constraint's function (resource extraction through mobility restriction) exceeds coordination necessity. This natural-law framing naturalizes what is structurally contingent.
constraint_indexing:constraint_classification(freedom_primary, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: INTERNATIONAL MIGRATION RIGHTS ADVOCACY (PITON) — Organized advocacy for freedom of movement exists (UN protocols, regional rights frameworks, NGO networks) but institutional leverage is minimal relative to state sovereignty claims. The advocacy machinery persists through normative commitment rather than functional efficacy: international human rights institutions cannot enforce freedom of movement against state sovereignty. Theater ratio is high—the institutions perform rights protection while lacking enforcement capacity. Constraints on advocacy (visa denials to activists, NGO funding cuts, legal suppression of smuggling networks) maintain the theatrical separation between rights claims and enforcement.
constraint_indexing:constraint_classification(freedom_primary, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(freedom_primary_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(freedom_primary, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(freedom_primary, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(freedom_primary, TR),
    TR >= 0.70.

:- end_tests(freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The freedom-primary reading identifies border enforcement as a mechanism of wealth and opportunity stratification. Excluded migrants cannot access higher-wage labor markets, cannot flee persecution, cannot reunify with family, and cannot benefit from lower-cost-of-living arbitrage—all mechanisms that movement-enabled agents exploit. The extractiveness has increased over time as border enforcement capacity has expanded (digital surveillance, detention infrastructure, deportation machinery) while migration drivers (climate displacement, regional conflict, wealth inequality) have intensified. Suppression (0.75): Very high. The constraint eliminates exit options for excluded populations: no legal pathway to movement, no appeal process, no negotiation with enforcement agents, no alternative route when borders are closed. The suppression includes prevention mechanisms (walls, detention) and normalization mechanisms (sovereignty doctrine teaching that borders are natural/legitimate). Theater ratio (0.38): Moderate-low. Border enforcement is substantive, not primarily performative. People are actually prevented from crossing, detained, deported. However, some theater exists: sovereignty doctrine performs legitimacy work, humanitarian exceptions and asylum processing create appearance of discretion within systems of near-total closure, the 'rule of law' frame legitimizes extraction as administration.
 *
 * PERSPECTIVAL GAP:
 *   The excluded migrant perceives a snare (maximum extraction, no exit). The border state perceives tangled rope (genuine coordination functions—identity verification, disease screening—alongside necessary enforcement of territorial boundaries). Global capital perceives rope (borders enable their selective mobility while restricting others, producing arbitrage opportunity). Domestic workers perceive extraction but through a sovereignty-framing lens (migrants as extractors of wages), not through the freedom-primary lens (both workers and migrants are victims of mobility restriction). The analytical observer risks perceiving mountain (borders as natural features of state order) until the beneficiary analysis forces recognition that the mountain is a false summit: specific institutional actors benefit from the border's maintenance. The perspectival gap is maximized between the excluded migrant (snare) and the border state (tangled rope)—they are measuring the same constraint through entirely different frames.
 *
 * DIRECTIONALITY LOGIC:
 *   The freedom-primary reading produces maximum directionality d values toward victimhood (d ≈ 0.95 for trapped excluded migrants; d ≈ 0.80 for constrained domestic workers). The f(d) sigmoid transforms this into high experienced extractiveness (f(0.95) ≈ 1.42, f(0.80) ≈ 1.15). From the border state's perspective (institutional/arbitrage), directionality is reversed: the state is the beneficiary, experiencing low or negative d (d ≈ 0.10), producing low experienced extraction f(0.10) ≈ -0.02. This perspectival inversion is the diagnostic signature of the snare: the target and extractor experience radically different chi values from the same ε. Analytical observers face the mountain temptation—to naturalize sovereignty as an immutable feature of political order—but the declared beneficiaries (border-enforcing states) expose this as false-summit naturalization. The constraint benefits identifiable institutional actors; it is not a natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The freedom-primary reading avoids mandatrophy by accepting that border enforcement is legitimately a snare from the target's perspective but may be perceived as coordination (tangled rope or even rope) from the beneficiary's perspective. The reading does not claim that all perspectives must classify identically—rather, it asserts that the excluded migrant's snare classification reveals the underlying structure (extraction masked by sovereignty doctrine). The analytical observer's mountain is a false summit. The mandatrophy is resolved by accepting perspectival pluralism while maintaining that the victims' perspective (snare) is the one that correctly identifies the constraint's function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_action_problem_exit,
    'Is the freedom-of-movement reading internally coherent if it requires unilateral open borders by some states while others maintain closure, creating migration flood dynamics that undermine the reading''s own legitimacy claims?',
    'Historical analysis of open-migration policies; modeling of sustainable global mobility equilibrium; comparison of successful vs failed open-border experiments (EU Schengen, internal USA migration, post-Soviet freedom of movement)',
    'If collective action failure is inevitable: reading requires global coordination (makes it a coordination constraint, not pure snare). If sustainable equilibrium exists: reading remains valid as snare, requiring global norm shift. Determines whether the constraint is a prisoner''s dilemma (coordination problem) or a genuine rights violation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_problem_exit, empirical, 'Whether freedom-of-movement reading requires coordinated global implementation or sustains unilaterally').

omega_variable(
    sovereignty_kernel_foreclosure,
    'Does the freedom-primary reading logically foreclose the sovereignty-primary reading, or do they coexist as incommensurable normative commitments?',
    'Logical analysis of the core axioms: if freedom of movement is a fundamental right that borders impermissibly restrict (freedom-primary) AND if states have irreducible authority to control their borders (sovereignty-primary), can both be maintained in a single normative framework? Or does one entail the rejection of the other?',
    'If forecloses: the readings are in genuine logical contradiction; frameworks must choose. If coexists: both readings are live despite tension; the dispute is about weighted priorities, not truth. Determines the reading_relations classification (forecloses vs coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_kernel_foreclosure, conceptual, 'Whether freedom-primary logically forecloses sovereignty-primary').

omega_variable(
    natural_rights_vs_contingent_institutions,
    'Is freedom of movement a universal natural right (grounded in human dignity) or a contingent institutional claim (grounded in particular political traditions)?',
    'Genealogical analysis of freedom-of-movement claims; identification of which traditions generate this right claim and which do not; examination of whether the claim is pre-institutional (discoverable in state of nature) or post-institutional (generated by particular legal regimes)',
    'If natural right: the constraint (border enforcement) is a violation of immutable human entitlements. If contingent: the reading is culturally particular and lacks universal force. Affects whether false-summit detection applies (naturalization is fraudulent vs. legitimate philosophical claim).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_rights_vs_contingent_institutions, conceptual, 'Whether freedom of movement is natural right or contingent institutional claim').

omega_variable(
    extraction_vs_coordination_decomposition,
    'Can border control be decomposed into a coordination component (disease screening, identity verification, criminal detection) and an extraction component (mobility restriction, wealth gatekeeping)? Or is border control necessarily unified?',
    'Analysis of technical requirements: what border functions genuinely require preventing movement? Which could be accomplished with movement permission + post-movement monitoring? What proportion of border capacity is devoted to each function?',
    'If decomposable: the coordination component is legitimate (tangled rope perspective gains force), but extraction mechanism is separable and target of rights claims. If unified: border control necessarily extracts through its coordination function. Determines whether the constraint should be split into separate stories per ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_decomposition, empirical, 'Whether border-control coordination and extraction components can be separated').

omega_variable(
    reading_kernel_contest_status,
    'This constraint is the freedom-primary reading of the border_normative_status kernel. Is this reading currently holdable (a live normative position in contemporary discourse)? Or has it been substantially overridden (rejected by dominant institutions)?',
    'Assessment of institutional adoption: UN declarations, state practice, movement momentum, funding and advocacy capacity, alignment with dominant state interests. If institutions actively suppress the reading, and scholars risk professional penalty for endorsing it, status approaches overridden despite philosophical coherence.',
    'If holdable: the reading remains a live option for normative commitment; perspectives should reflect ongoing contest. If overridden: the reading exists as a minority philosophical position; perspectives should reflect its marginalization. Affects axiom status declarations and drift analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_status, empirical, 'Current institutional status of freedom-primary reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(freedom_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(free_tr_t0, freedom_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement(free_tr_t20, freedom_primary, theater_ratio, 20, 0.33).
narrative_ontology:measurement(free_tr_t40, freedom_primary, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(free_be_t0, freedom_primary, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(free_be_t20, freedom_primary, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(free_be_t40, freedom_primary, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(freedom_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(freedom_primary, sovereignty_primary).
narrative_ontology:affects_constraint(freedom_primary, qualified_sovereignty).
narrative_ontology:affects_constraint(freedom_primary, global_labor_market_segmentation).
narrative_ontology:affects_constraint(freedom_primary, climate_refugee_entrapment).

% DUAL FORMULATION NOTE:
% The freedom-primary reading is one of three readings of the border_normative_status kernel. Each reading produces different base_properties.extractiveness values and beneficiary/victim declarations. The freedom-primary reading (this file) treats migrants as victims. The sovereignty-primary reading treats migrant inflows as extraction targets for domestic workers and treats state authority as the primary beneficiary. The qualified-sovereignty reading occupies the logical space between them, accepting some freedom-of-movement principle while recognizing state-interest constraints. All three are separate constraint stories with different ε values and perspective structures; they are linked via network.affects_constraints because the kernel contest means they structurally influence one another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
