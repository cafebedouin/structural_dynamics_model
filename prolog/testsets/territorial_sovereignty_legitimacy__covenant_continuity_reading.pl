% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Territorial Sovereignty Legitimacy: Covenant-Continuity Reading
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   The covenant-continuity reading frames territorial sovereignty legitimacy
 *   as derived from three pillars: (1) ancient divine covenant granting the
 *   territory to the Jewish people, treated as a continuous juridical claim
 *   surviving millennia of diaspora; (2) interpretation of modern Jewish
 *   presence (immigration, settlement) as fulfillment of ancient right and
 *   'continuous presence' redefined to include diaspora connection; (3)
 *   modern international recognition (Balfour Declaration, UN Partition Plan,
 *   1948 establishment) as legitimating a pre-existing claim rather than
 *   creating a new one. This reading directly marginalizes Palestinian Arab
 *   self-determination by treating their demographic presence as legally and
 *   temporally subordinate. The constraint operates through both
 *   theological-historical narration and institutional enforcement (security
 *   apparatus, settlement policy, international alliance maintenance).
 *   Temporal scope is uniquely extended to biblical period; legitimacy claim
 *   structurally survives demographic absence; partition is framed as
 *   compromise of pre-existing right rather than creation; settlements are
 *   framed as return rather than colonization.
 *
 * KEY AGENTS:
 *   - jewish_sovereignty_claimants: Institutional beneficiary; hold agenda-setting power through state apparatus and international alliances; identity-locked to the covenant-continuity reading; exit requires reconstructing collective legitimacy
 *   - palestinian_arab_population: Structural target; powerless; identity-locked to competing self-determination claims; bear territorial displacement and subordination within the sovereignty framework
 *   - international_recognition_authorities: Institutional agenda-setters; provided modern-legal pillar (partition, recognition); constrained exit by diplomatic complexity; enforcement involves border guarantees and normalization
 *   - historical_covenant_doctrine_interpreters: Organized beneficiaries; theological and academic authority vindicated by reading; identity-locked exit requires reinterpretation within own tradition
 *   - competing_sovereignty_claimants: Excluded from agenda-setting; moderate power; structurally marginalized by temporal scope and presence criteria; constrained exit
 *   - settlement_movement_actors: Moderate power; payer-beneficiary duality; constrained by international pressure but enabled by legitimacy narrative
 *   - international_legal_order: Institutional observer; tension with modern self-determination principle; covenant-continuity reading operates partly outside formal international legal authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.72).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Territorial Sovereignty Legitimacy: Covenant-Continuity Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'a46537be-f697-4a42-8762-d59503ba351c').
narrative_ontology:cs_kernel_codification('a46537be-f697-4a42-8762-d59503ba351c', fixed_text).
narrative_ontology:cs_authority_grounding('a46537be-f697-4a42-8762-d59503ba351c', lineage).
narrative_ontology:cs_interpretation_layer_present('a46537be-f697-4a42-8762-d59503ba351c').
narrative_ontology:cs_reading_relation('a46537be-f697-4a42-8762-d59503ba351c', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_reading_relation('a46537be-f697-4a42-8762-d59503ba351c', territorial_sovereignty_legitimacy__existential_matrix_reading, influences).
narrative_ontology:cs_axiom('a46537be-f697-4a42-8762-d59503ba351c', foundational, ancient_covenant_continuous_juridical_claim).
narrative_ontology:cs_axiom_status(ancient_covenant_continuous_juridical_claim, holdable).
narrative_ontology:cs_axiom_grounding('a46537be-f697-4a42-8762-d59503ba351c', ancient_covenant_continuous_juridical_claim, theological).
narrative_ontology:cs_axiom('a46537be-f697-4a42-8762-d59503ba351c', foundational, diaspora_connection_as_continuous_presence).
narrative_ontology:cs_axiom_status(diaspora_connection_as_continuous_presence, holdable).
narrative_ontology:cs_axiom_grounding('a46537be-f697-4a42-8762-d59503ba351c', diaspora_connection_as_continuous_presence, conventional).
narrative_ontology:cs_reference_frame('a46537be-f697-4a42-8762-d59503ba351c', biblical_covenant_promise).
narrative_ontology:cs_drift_state('a46537be-f697-4a42-8762-d59503ba351c', contemporary_post_1948_settlement_entrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a46537be-f697-4a42-8762-d59503ba351c', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_sovereignty_claimants).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_arab_population).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) reflects the constraint's core asymmetry: Jewish claimants benefit from legitimacy framework that privileges ancient covenant and diaspora-inclusive continuity; Palestinian Arabs pay through territorial loss and subordination. Extractiveness rises over the interval (0.45→0.68) as settlement expansion and institutional entrenchment deepen the territorial asymmetry. Suppression (0.72) is higher than extractiveness because the constraint's persistence depends on active military enforcement, administrative control, and international alliance maintenance—not on participant preference. Palestinian resistance remains high throughout (0.81), indicating the constraint is actively contested. Theater ratio (0.41) reflects a substantial share of institutional activity dedicated to maintaining the legitimacy narrative (historical commission, settlement rhetoric framing as 'return', theological interpretation) rather than delivering new coordination or public goods. The measurement series models escalating enforcement and narrative entrenchment over a 100-year interval anchored at 1948.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (Jewish sovereignty claimants and supporting interpreters), the constraint appears as restoration of ancient right, fulfillment of diaspora-end aspiration, and legitimate modern statehood. From the victim seat (Palestinian Arabs), the same structure appears as displacement, negation of competing legitimacy claims, and enforced subordination. The engine computes this perspectival divergence from the structural data: high directionality for victims (d→1.0), low directionality for beneficiaries (d→0.0), and institutional-level beneficiary organizing (institutional power amplifies positive d derivation). No single observer seat perceives the constraint as symmetric coordination—the asymmetry is structural, not perceptual.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish sovereignty claimants receive d near 0.0 (full beneficiary end) by combination of: institutional power organizing, identity-locked exit (rejection requires collective legitimacy reconstruction), and beneficiary status that collects sovereignty without running ongoing coordination. Palestinian Arabs receive d near 1.0 (full target end) by: powerless structural position, identity-locked exit (acceptance requires abandoning self-determination), and victim status bearing territorial subordination. International recognition authorities receive intermediate d (~0.4) by: institutional power but constrained exit (diplomatic renegotiation costs), beneficiary status (legitimacy framework they codified) balanced by payer function (enforcing continued suppression). Settlement actors receive d near 0.7 (target-adjacent) by: moderate power but constrained by international pressure, trapped in enforcement machinery despite moderate organizational capacity. Competing sovereignty claimants receive d near 0.95 (near-total target) by: powerless-to-moderate structure, identity-locked exclusion, and complete structural marginalization by the framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The covenant-continuity reading faces a mandatrophy hypothesis: the founding problem (diaspora legitimacy for territorial claim) has been substantially resolved by 1948 establishment and modern statehood, yet the constraint persists and strengthens through settlement expansion and institutional deepening. If the founding legitimacy problem is dead—if modern statehood has established the Jewish state as a fact of international law—then continued enforcement of the covenant-continuity reading as the primary legitimacy basis becomes theater and rent-seeking on the theological narrative. However, the beneficiary community contests this: they argue the founding problem remains live (security threats, existential precarity, need for territorial depth) and the covenant-continuity reading continues to serve the coordination function of settling diaspora populations and maintaining collective claim. This contest maps to the measured theater_ratio rise (0.28→0.41): as institutional entrenchment proceeds without resolution of the Palestinian question, more enforcement activity is dedicated to maintaining the theological-historical narrative rather than solving new coordination problems. The disappearance_verdict (world_rearranges) indicates the constraint is not a natural law but a constructed, enforceable arrangement whose removal would trigger major institutional renegotiation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_continuous_claim_legitimacy,
    'Does an ancient covenant claim retain juridical force across millennia of diaspora and demographic absence, or does sovereignty legitimacy depend on continuous physical control and modern legal recognition only?',
    'Comparative analysis of other historical covenant-claims (Pontius, Kashmir territorial claims grounded in ancient texts, Armenian historical claims) and examination of international law doctrine on the temporal scope of territorial rights. Examination of whether international law admits ancient theological claims as primary grounds for sovereignty.',
    'If covenant claims retain force: the covenant-continuity reading is structurally sound and the classification holds as tangled-rope (legitimacy-asymmetry with real coordination function). If ancient claims require continuous physical control to survive: the reading becomes reclassifiable as snare (pure extraction of legitimacy through theological narrative without modern legal or demographic basis).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_continuous_claim_legitimacy, conceptual, 'Whether ancient covenant claims have independent juridical force or require continuous physical control to survive.').

omega_variable(
    diaspora_presence_redefinition,
    'Can diaspora connection and religious continuity count as ''continuous presence'' for sovereignty purposes, or does continuous presence require unbroken territorial habitation?',
    'International law doctrine analysis; comparative historical cases (diaspora return movements, homeland-nostalgia claims); examination of whether any other sovereignty claim succeeds through diaspora-reconceptualized presence. Direct: does the international legal order accept the covenant-continuity reading''s definition of continuous presence?',
    'If diaspora presence is internationally accepted: the covenant-continuity reading''s temporal scope is legitimate and the classification holds. If rejected: the reading becomes a theological claim rather than a legal argument, reclassifying extractiveness upward (pure narrative legitimacy without modern legal grounding).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_presence_redefinition, empirical, 'Whether international law recognizes diaspora connection as continuous presence for sovereignty claims.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) primarily structural (military enforcement, administrative control, border restriction) or internalized (Palestinian acceptance of legitimacy hierarchy, belief in Jewish historical claim as superior)?',
    'Post-constraint removal trajectory: if suppression persists after enforcing institutions are dismantled (e.g., in territories without Israeli military presence), the suppression is partially internalized. Polling data on Palestinian acceptance of covenant-continuity framing; discourse analysis of Palestinian legitimacy narratives; trajectory of Palestinian youth cohorts under long-term exposure to the constraint.',
    'If primarily structural: removing enforcement institutions would allow alternative sovereignty readings to crystallize. If internalized: the constraint carries higher effective suppression than the structural measure suggests—targets carry the internalized hierarchy with them across territorial movements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in Palestinian response to covenant-continuity framing.').

omega_variable(
    settlement_framing_as_return_vs_colonization,
    'Does the covenant-continuity reading''s framing of settlements as ''return'' represent a legitimate exercise of the ancient right, or is it a post-hoc narrative overlay on territorial expansion that displaces current residents?',
    'Comparative-historical analysis: examination of whether other return-to-ancestral-homeland movements (Armenian, Pontic Greek) use identical legitimacy framings and whether international law has adjudicated between return-right and displacement-right. Analysis of settlement timing relative to ancient demographic patterns: do settlement locations track biblical-era Jewish population distribution, or do they follow strategic-military logic independent of historical presence claims?',
    'If settlements align with ancient presence patterns and return-framing is legitimate: the covenant-continuity reading''s land claims are consistent with ancient-covenant logic. If settlements follow strategic-military logic only: the return-framing is narrative overlay (theater) and extractiveness should be reclassified upward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settlement_framing_as_return_vs_colonization, empirical, 'Whether settlement patterns and framing are consistent with ancient-covenant restoration logic or represent strategic territorial expansion with post-hoc justification.').

omega_variable(
    competing_reading_foreclosure,
    'Does the covenant-continuity reading logically foreclose the self-determination reading, or do they represent genuinely coexisting positions that both parties hold simultaneously?',
    'Examination of whether a single legal or ethical framework could accommodate both readings (ancient right + modern self-determination) or whether accepting one requires rejecting the other. Analysis of whether any binding international authority (UN, ICJ) has attempted a ruling that accommodates both readings or has chosen one.',
    'If foreclosure: the sibling readings are related by logical incompatibility (forecloses relation). If coexistence: they represent competing truth-claims held by different parties without mutual negation (coexists_with relation). This affects the network structure and the diagnosis of whether the kernel can be resolved or only negotiated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_reading_foreclosure, conceptual, 'Whether covenant-continuity and self-determination readings are logically incompatible (foreclosure) or genuinely coexistent positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(terr_tr_t12, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(terr_tr_t25, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 25, 0.37).
narrative_ontology:measurement(terr_tr_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(terr_tr_t75, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 75, 0.41).
narrative_ontology:measurement(terr_tr_t100, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 100, 0.41).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(terr_be_t12, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(terr_be_t25, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 25, 0.59).
narrative_ontology:measurement(terr_be_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement(terr_be_t75, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 75, 0.67).
narrative_ontology:measurement(terr_be_t100, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(terr_su_t12, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(terr_su_t25, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(terr_su_t50, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(terr_su_t75, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 75, 0.72).
narrative_ontology:measurement(terr_su_t100, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.12).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_self_determination_constraint).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, settlement_expansion_extraction).

% DUAL FORMULATION NOTE:
% The territorial_sovereignty_legitimacy kernel decomposes into three structurally distinct constraint stories, each instantiating a different reading of what legitimates sovereignty in the disputed territory. The covenant_continuity_reading (this story) frames legitimacy through ancient covenant and diaspora-continuous presence; the self_determination_reading frames it through modern demographic majority and twentieth-century residence; the existential_matrix_reading frames it through zero-sum survival requirements. These are not three measurements of one constraint—they are three different constraints, each with different ε values, different beneficiary structures, and different classifications. The covenant-continuity reading carries substantially higher extractiveness (0.68) than a pure coordination reading would, because it privileges one claimant's ancient narratives over another's modern legal rights. The self-determination reading carries lower extractiveness if measured from the Arab claimant seat (the reading vindicates their claim). The existential-matrix reading carries high extractiveness from both seats but frames it as structurally necessary rather than unjust. They coexist as live political and legal positions, linked by network.affects_constraints to show constraint-family dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
