% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_spanish_conquest, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Grant as License for Spanish Conquest and Indigenous Subjugation (Tordesillas Demarcation Reading)
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   The Spanish reading of the 1493 papal grant (Inter Caetera) and the 1494
 *   Treaty of Tordesillas frames territorial conquest and indigenous
 *   subjugation as legitimately authorized by supreme Christian authority.
 *   Under this reading, the Spanish Crown's conquest of territories west of
 *   the demarcation line, forced conversion of indigenous populations,
 *   expropriation of land, and institution of forced labor (encomienda) are
 *   presented as legally sanctioned papal-authorized activities rather than
 *   illegitimate seizure. The constraint operates through a dual mechanism:
 *   (1) external suppression via military conquest and institutional
 *   enforcement; (2) internal suppression via religious authority claims that
 *   delegitimize indigenous political and cosmological frameworks.
 *   Extractiveness is extremely high (0.82) because the Spanish beneficiaries
 *   capture territorial sovereignty, labor value, resource extraction, and
 *   institutional authority while indigenous populations lose autonomy,
 *   property, self-governance, and religious autonomy. The theater ratio
 *   (0.65) reflects that while the papal grant framework provides ceremonial
 *   legitimation, the actual conquest mechanism is militarily direct—the
 *   performance is present (religious justification, legal formality) but not
 *   dominant relative to coercive force. Suppression is extreme (0.88)
 *   because indigenous agents face military overwhelm, disease catastrophe
 *   beyond their capacity to counter, institutional erasure of all rival
 *   legitimacy sources, and religious conversion enforced through
 *   inquisitorial and encomienda mechanisms. This constraint is one reading
 *   of the contested kernel of papal demarcation authority. The sibling
 *   reading (Portuguese exploration legitimation) interprets the same papal
 *   texts differently—emphasizing trade and exploration rights rather than
 *   conquest authorization. The two readings coexist as competing
 *   instantiations of the same kernel, held by rival powers with different
 *   beneficiary structures and extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Indigenous populations (Aztec, Inca, Maya, regional polities): Primary victims (powerless/trapped) — entire populations subjected to conquest, forced labor, religious conversion, territorial dispossession with zero exit options
 *   - Spanish Crown Administration: Primary beneficiary (institutional/arbitrage) — captures territorial sovereignty, labor value, tribute extraction, institutional authority over western hemisphere; experiences constraint as enabling coordination
 *   - Roman Catholic Church Authority: Co-beneficiary (institutional/arbitrage) — gains spiritual jurisdiction, property accumulation via encomienda partnerships, institutional centrality in colonial administration, legitimacy to authorize temporal conquest
 *   - Portuguese Crown/Rival Powers: Constrained beneficiary (institutional/constrained) — their alternative reading of papal demarcation grants them Eastern Hemisphere rights but faces Spanish competition for interpretation authority
 *   - Indigenous Religious Leaders: Secondary victim (powerless/trapped) — face systematic suppression of traditional cosmologies, ritual prohibition, institutional erasure; no structural capacity to resist religious conversion mechanism
 *   - Anti-slavery and Decolonial Movements: Future-perspective organized actor (organized/constrained) — from civilizational horizon, this constraint is being replaced by alternative legitimacy frameworks (human rights, indigenous sovereignty, UN protocols); sees the grant as degraded theater persisting through institutional inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.82).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.88).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.82).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Grant as License for Spanish Conquest and Indigenous Subjugation (Tordesillas Demarcation Reading)").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'fc4bc043-f418-4191-aa37-2e03f9bb6df3').
narrative_ontology:cs_kernel_codification('fc4bc043-f418-4191-aa37-2e03f9bb6df3', formalized).
narrative_ontology:cs_authority_grounding('fc4bc043-f418-4191-aa37-2e03f9bb6df3', extraction).
narrative_ontology:cs_interpretation_layer_present('fc4bc043-f418-4191-aa37-2e03f9bb6df3').
narrative_ontology:cs_reading_relation('fc4bc043-f418-4191-aa37-2e03f9bb6df3', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, influences).
narrative_ontology:cs_axiom('fc4bc043-f418-4191-aa37-2e03f9bb6df3', foundational, papal_authority_legitimizes_territorial_conquest).
narrative_ontology:cs_axiom_status(papal_authority_legitimizes_territorial_conquest, holdable).
narrative_ontology:cs_axiom_grounding('fc4bc043-f418-4191-aa37-2e03f9bb6df3', papal_authority_legitimizes_territorial_conquest, theological).
narrative_ontology:cs_axiom('fc4bc043-f418-4191-aa37-2e03f9bb6df3', foundational, christianization_justifies_indigenous_subjugation).
narrative_ontology:cs_axiom_status(christianization_justifies_indigenous_subjugation, holdable).
narrative_ontology:cs_axiom_grounding('fc4bc043-f418-4191-aa37-2e03f9bb6df3', christianization_justifies_indigenous_subjugation, theological).
narrative_ontology:cs_axiom('fc4bc043-f418-4191-aa37-2e03f9bb6df3', secondary, papal_demarcation_supersedes_indigenous_sovereignty).
narrative_ontology:cs_axiom_status(papal_demarcation_supersedes_indigenous_sovereignty, overridden).
narrative_ontology:cs_axiom_grounding('fc4bc043-f418-4191-aa37-2e03f9bb6df3', papal_demarcation_supersedes_indigenous_sovereignty, deontological).
narrative_ontology:cs_reference_frame('fc4bc043-f418-4191-aa37-2e03f9bb6df3', papal_sovereignty_over_spiritual_and_temporal_authority).
narrative_ontology:cs_drift_state('fc4bc043-f418-4191-aa37-2e03f9bb6df3', contemporary_human_rights_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('fc4bc043-f418-4191-aa37-2e03f9bb6df3', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown_treasury).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, roman_catholic_church_authority).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_western_hemisphere).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, precolonial_political_sovereignties).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_religious_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS POPULATIONS (SNARE) — Trapped by military conquest justified through papal authority they never accepted. No exit option; no alternative legitimacy framework available within the imposed colonial structure. Bear full costs of forced labor (encomienda), forced conversion, territorial dispossession, and institutional erasure. Maximum experienced extraction with zero alternatives.
constraint_indexing:constraint_classification(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRECOLONIAL POLITICAL SOVEREIGNTIES (SNARE) — Aztec, Inca, Maya, and other state-level polities cannot exit the constraint through diplomatic or military means (technological and epidemiological asymmetry). Extraction manifests as loss of territorial control, forced vassalage, tribute extraction, and institutional dissolution. The papal grant provides the Spanish legal fiction for conquest while denying legitimacy to indigenous political structures.
constraint_indexing:constraint_classification(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIGENOUS RELIGIOUS AUTONOMY (SNARE) — Forced conversion to Catholicism backed by papal authority and enforced through encomienda and inquisitorial mechanisms. Indigenous agents face suppression of traditional cosmologies, temple destruction, ritual prohibition, and incorporation into church hierarchy. Constrained exit via syncretism/hidden practice, but no legitimate path to exit without social annihilation.
constraint_indexing:constraint_classification(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: SPANISH CROWN ADMINISTRATION (ROPE) — Experiences the constraint as enabling coordination and resource capture. The papal grant provides legal justification (reduces internal legitimacy costs), consolidates territorial claims against Portuguese competition, and enables systematic extraction through encomienda, tribute, and trade monopoly. Beneficiary with maximum arbitrage — can exit or redefine the constraint without cost.
constraint_indexing:constraint_classification(tordesillas_demarcation_kernel__spanish_conquest_legitimation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ROMAN CATHOLIC CHURCH AUTHORITY (ROPE) — The papal grant (Inter Caetera, 1493) enables church expansion, spiritual jurisdiction, property accumulation through encomienda partnerships, and institutional centrality in colonial administration. Church sees the constraint as pure coordination: justifying conquest in religious terms expands Christendom and church authority. Beneficiary with high arbitrage.
constraint_indexing:constraint_classification(tordesillas_demarcation_kernel__spanish_conquest_legitimation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANTI-SLAVERY AND DECOLONIAL MOVEMENTS (SCAFFOLD) — From civilizational perspective, the papal grant legitimation mechanism is being replaced by alternative legitimacy frameworks: human rights doctrine, indigenous sovereignty recognition, UN decolonization protocols. The constraint operates with decreasing functional force as alternative institutional pathways (independence movements, indigenous land claims, international recognition) bypass papal authority. Theater ratio reflects that the grant's force persists through institutional inertia despite formal repudiation.
constraint_indexing:constraint_classification(tordesillas_demarcation_kernel__spanish_conquest_legitimation, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a universalized, dehistoricized position, the constraint might be framed as expressing an immutable truth: that territorial conquest inevitably requires some form of legitimizing authority claim; that power differentials naturally produce hierarchical authority structures; that European conquest was 'inevitable' given technological advantage. This perspective naturalizes the papal grant as expressing unchangeable principles of power distribution. Engine will classify this as FALSE SUMMIT — the naturalization conceals the contingent institutional choice to invoke papal authority rather than other possible legitimacy frameworks.
constraint_indexing:constraint_classification(tordesillas_demarcation_kernel__spanish_conquest_legitimation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tordesillas_demarcation_kernel__spanish_conquest_legitimation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tordesillas_demarcation_kernel__spanish_conquest_legitimation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82): Extremely high. The Spanish beneficiaries (Crown, Church) capture maximal value: territorial sovereignty over entire hemisphere west of demarcation line, perpetual labor extraction through encomienda, resource monopoly, institutional authority. Indigenous populations lose all structural assets—autonomy, territory, labor control, cosmological authority. The asymmetry is extreme. Measurement trajectory shows rising extractiveness over the first 100 years (consolidation of encomienda system, church institutional embedding) then plateau at 0.80+ as the system matures. Suppression (0.88): Extreme. Indigenous populations face layered suppression: military conquest (conquistador armies, technological asymmetry), epidemiological catastrophe (disease from which they have no countermeasures), institutional suppression (encomienda binding), religious suppression (forced conversion, inquisition, temple destruction), and delegitimation of all rival authority claims. The measurement trajectory shows suppression intensifying from 0.72 to 0.88 over first century as enforcement infrastructure (encomienda, inquisitorial apparatus, Catholic missionary apparatus) accumulates. Theater ratio (0.65): Moderate-high. The papal grant provides ceremonial/theatrical legitimation (legal fiction, religious authority cover) for what is fundamentally military conquest and labor extraction. The conquistador rationale mixes military justification with papal authority invocation. Over the interval, theater increases as the initial naked conquest phase transitions to institutionalized colonial administration—more of the extraction becomes formalized in legal structures (encomienda codes, tribute systems, church hierarchies) rather than direct plunder. By t=300, theater rises to 0.72 as the system's procedural legitimacy becomes more important to maintenance than raw force, though suppression remains extreme.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. Indigenous populations (powerless/trapped) classify the structure as pure snare—military conquest justified through an alien authority (papal) they never accepted, with zero exit options and total extraction. Precolonial sovereignties (powerless/trapped over generations) experience snare classification—institutional dissolution and forced vassalage with no diplomatic alternatives. Indigenous religious autonomy (constrained via forced syncretism/hidden practice) remains snare due to suppression through inquisitorial apparatus. Spanish Crown (institutional/arbitrage) experiences rope classification—the papal grant enables coordination of conquest claim against Portuguese rivals while legitimating extraction through legal formalism. Church (institutional/arbitrage) experiences rope classification—papal authority provides jurisdiction and property accumulation through encomienda partnerships. Anti-slavery/decolonial movements (organized/constrained, civilizational horizon) see scaffold classification—the papal grant mechanism is being replaced by human rights frameworks and indigenous sovereignty recognition; the constraint operates with decreasing functional force. The analytical observer risks mountain classification (natural law: conquest requires legitimating authority; power differences are unchangeable facts) but this is a FALSE SUMMIT—the constraint is contingent on the choice to invoke papal authority rather than alternative justification frameworks. The perspectival gaps are extreme: snare from victims, rope from beneficiaries, scaffold from organized challengers, false-summit risk from analytical observers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to extraction flow. Spanish Crown and Church beneficiaries with arbitrage options have d ≈ 0.05-0.10 (full beneficiary position) → f(d) ≈ -0.12 to -0.01 → negative or near-zero effective extraction χ (they experience the constraint as enabling, not extractive). Indigenous victims with trapped exit have d ≈ 0.95 (full target position) → f(d) ≈ 1.42 → maximum effective extraction χ ≈ 1.16 (0.82 × 1.42 × 1.0 for global scope, σ=1.0). Organized challengers (anti-slavery movements) with constrained exit and organized power have d ≈ 0.40 (victim-leaning but with some agency) → f(d) ≈ 0.40 → moderate experienced extraction χ ≈ 0.33. The directionality derivation confirms that the same structural metrics (ε=0.82, suppression=0.88) produce radically different experienced extractiveness χ depending on agent position: beneficiaries see negative χ (coordination benefit), victims see maximum χ (extreme extraction), organized challengers see moderate χ (constrained but pushing back).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    papal_authority_legitimacy_contingency,
    'Is the papal grant''s legitimacy for conquest a contingent institutional invention (choice to invoke papal authority) or an expression of necessary truths about power distribution and conquest?',
    'Historical analysis of alternative conquest justifications: (a) conquistador narratives invoking papal authority vs. those invoking military necessity, (b) indigenous responses that accept/reject papal authority frame, (c) philosophical texts from the period debating whether papal authority was necessary for legitimacy vs. merely convenient. Comparative analysis: how did non-Catholic powers justify territorial conquest in the same period without papal grant?',
    'If contingent: constraint is snare-class institutional extraction; false summit detection triggers engine reclassification. If necessary: constraint approaches mountain classification, though false summit still applies due to identifiable beneficiaries (Catholic Church, Spanish Crown).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(papal_authority_legitimacy_contingency, conceptual, 'Whether papal authority for conquest legitimacy is contingent or necessary').

omega_variable(
    indigenous_agency_within_colonial_hierarchy,
    'Did indigenous agents retain any structural capacity to negotiate, contest, or reframe the papal grant''s legitimacy claims during the colonial period (16th-18th centuries)?',
    'Archival analysis of indigenous petitions to the Spanish Crown, letters to the Pope, legal arguments in Spanish colonial courts invoking alternative legitimacy claims (natural law, indigenous prior sovereignty, divine will toward indigenous peoples). Analysis of instances where indigenous agents successfully negotiated encomienda terms, tribute reductions, or religious autonomy via colonial legal structures. Counterfactual: would recognition of indigenous political agency in colonial period change classification from snare toward tangled_rope or constrained?',
    'If agency was substantial but structurally suppressed: classification remains snare but omegas reveal constraining mechanisms were active enforcement rather than structural immobility. If agency was negligible: confirms mountain-like immobility under colonial hierarchy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_agency_within_colonial_hierarchy, empirical, 'Degree of indigenous structural agency within colonial legal/institutional framework').

omega_variable(
    portuguese_reading_foreclosure_or_coexistence,
    'Does the Spanish reading of the papal grant as legitimation for conquest logically foreclose the Portuguese reading (exploration + trade rights under separate demarcation line), or do these readings coexist as simultaneous legitimate interpretations held by rival powers?',
    'Textual analysis of Inter Caetera (1493) and Treaty of Tordesillas (1494) for ambiguities that permit both readings simultaneously. Historical analysis: did Spanish and Portuguese authorities treat their respective readings as mutually exclusive (foreclosure) or as coexisting valid interpretations of the same grant? Can the same papal authority structure ground both readings without internal contradiction?',
    'If foreclosed: this reading''s core axiom (papal authority legitimizes conquest) rules out Portuguese reading; Spanish-Portuguese conflict is logical, not merely competitive. If coexistent: both readings are live within different national frameworks; the kernel permits multiple simultaneous readings without collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(portuguese_reading_foreclosure_or_coexistence, conceptual, 'Whether Spanish and Portuguese readings of papal demarcation are mutually exclusive or coexistent').

omega_variable(
    encomienda_system_dependency_on_grant,
    'Is the encomienda system (forced labor extraction) logically dependent on the papal grant legitimation, or would the system have emerged through local Spanish colonial institution-building regardless of papal authority?',
    'Comparative institutional analysis: (a) did colonial systems without papal authority (e.g., private trading posts, non-Christian powers) develop analogous forced labor mechanisms? (b) Spanish colonial documents: what explicit connections are made between papal grant legitimacy and encomienda legal justification? (c) Counterfactual: would Spanish conquistadors have invaded and established encomienda without Inter Caetera framing?',
    'If dependent: papal grant is a structural necessity for the full snare mechanism; remove the grant and extraction mechanisms lose legitimating cover. If independent: encomienda emerges from conquest dynamics regardless; grant is ceremonial rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(encomienda_system_dependency_on_grant, empirical, 'Structural dependence of encomienda system on papal grant legitimation').

omega_variable(
    reading_vs_portuguese_kernel_identity,
    'Does this reading (Spanish conquest legitimation) instantiate a different kernel from the Portuguese reading, or are both readings of the same underlying kernel (papal authority to distribute territorial rights)?',
    'Kernel-level analysis: what is the minimal codified commitment (the kernel) that both readings interpret? If the kernel is ''papal authority grants territorial rights in the Americas'' — same kernel, different readings (Spanish emphasizes conquest legitimation; Portuguese emphasizes exploration/trade rights). If the kernel is ''the specific wording and intent of Inter Caetera and Tordesillas'' — kernels might differ because the readings attribute contradictory intents to the same text.',
    'If same kernel: both readings belong in a single constraint family linked by network.affects_constraints; each reading gets its own story with its own beneficiary/victim structure. If different kernels: each reading instantiates a separate kernel and should be in separate constraint families.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_portuguese_kernel_identity, conceptual, 'Whether Spanish and Portuguese readings instantiate the same kernel or different kernels').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tordesillas_sp_theater_t0, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0, 0.55).
narrative_ontology:measurement(tordesillas_sp_theater_t100, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 100, 0.65).
narrative_ontology:measurement(tordesillas_sp_theater_t300, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 300, 0.72).

% Extraction over time
narrative_ontology:measurement(tordesillas_sp_extract_t0, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(tordesillas_sp_extract_t100, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 100, 0.82).
narrative_ontology:measurement(tordesillas_sp_extract_t300, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 300, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(tordesillas_sp_suppress_t0, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(tordesillas_sp_suppress_t100, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 100, 0.88).
narrative_ontology:measurement(tordesillas_sp_suppress_t300, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 300, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel__portuguese_exploration_legitimation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomienda_labor_extraction_system).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, forced_indigenous_conversion_mechanism).

% DUAL FORMULATION NOTE:
% This is one of two readings of the Tordesillas demarcation kernel. The Portuguese reading (exploration legitimation) instantiates different beneficiary/victim structure and lower extractiveness (0.45-0.55 estimate, focused on trade rights rather than conquest authorization). Both readings instantiate the same kernel but with different axioms, authority grounds, and drift states. The Spanish reading is upstream of the encomienda system and forced conversion mechanism—it provides the legitimating framework within which those extraction systems operate. Removing this reading (via decolonization and indigenous sovereignty recognition) changes the justificatory structure of downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
