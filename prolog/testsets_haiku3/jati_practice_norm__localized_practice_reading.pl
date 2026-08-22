% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Practice Norms: Localized Renegotiation Reading
 *   domain: social/religious/political
 *
 * SUMMARY:
 *   Under this reading, jati boundaries are understood not as fixed
 *   scriptural categories or reified colonial census boxes, but as
 *   continuously negotiated coordination norms operative at village and
 *   regional scale. The same constraint (jati identity) is read by different
 *   stakeholders as: (1) flexible craft guilds and occupational coordination
 *   (merchant reading); (2) marriage and kinship insurance networks
 *   (community reading); (3) ritual domain requiring boundary interpretation
 *   by specialist authorities (brahminical reading filtered through local
 *   practice). The reading's core claim is that the proliferation of jati
 *   categories documented by anthropologists (3000+ recorded variations
 *   across India) reflects not breakdown or corruption of an original
 *   four-varna system, but the normal operation of the boundary-renegotiation
 *   machinery. Low extractiveness (~0.38) reflects the genuine coordination
 *   function; modest suppression (~0.22) reflects the light enforcement
 *   touch—ritual authority corrects pollution violations but cannot mandate
 *   jati membership, which is constituted through genealogy, occupation, and
 *   community acceptance. The constraint persists through voluntary
 *   participation in shared institutions, not coercion.
 *
 * KEY AGENTS:
 *   - local_communities: Primary beneficiaries (provide and reproduce jati identity structure); moderate power; generational time horizon
 *   - ritual_specialists: Secondary beneficiaries and partial agenda-setters (interpret and adjudicate boundaries); organized power; regional scope
 *   - merchant_guilds: Beneficiaries using jati-like forms for occupational coordination; organized power; mobile exit options
 *   - individual_aspirants: Payers (bear costs of boundary renegotiation); powerless; identity-locked exit
 *   - brahminical_orthodoxy: Observer authority (supplies interpretive framework, maintains textual standard)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.38).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.22).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Practice Norms: Localized Renegotiation Reading").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social/religious/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '57f014d0-674d-4aa4-9822-7da2fb5db685').
narrative_ontology:cs_kernel_codification('57f014d0-674d-4aa4-9822-7da2fb5db685', distributed).
narrative_ontology:cs_authority_grounding('57f014d0-674d-4aa4-9822-7da2fb5db685', practice).
narrative_ontology:cs_interpretation_layer_present('57f014d0-674d-4aa4-9822-7da2fb5db685').
narrative_ontology:cs_reading_relation('57f014d0-674d-4aa4-9822-7da2fb5db685', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_reading_relation('57f014d0-674d-4aa4-9822-7da2fb5db685', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_axiom('57f014d0-674d-4aa4-9822-7da2fb5db685', foundational, jati_boundaries_locally_negotiated).
narrative_ontology:cs_axiom_status(jati_boundaries_locally_negotiated, holdable).
narrative_ontology:cs_axiom_grounding('57f014d0-674d-4aa4-9822-7da2fb5db685', jati_boundaries_locally_negotiated, empirically_contingent).
narrative_ontology:cs_axiom('57f014d0-674d-4aa4-9822-7da2fb5db685', foundational, jati_coordination_function_primary).
narrative_ontology:cs_axiom_status(jati_coordination_function_primary, holdable).
narrative_ontology:cs_axiom_grounding('57f014d0-674d-4aa4-9822-7da2fb5db685', jati_coordination_function_primary, instrumental).
narrative_ontology:cs_reference_frame('57f014d0-674d-4aa4-9822-7da2fb5db685', village_scale_occupational_coordination).
narrative_ontology:cs_drift_state('57f014d0-674d-4aa4-9822-7da2fb5db685', contemporary_post_independence, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('57f014d0-674d-4aa4-9822-7da2fb5db685', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, local_communities).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, ritual_specialists).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, merchant_guilds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, individual_aspirants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain shared identity and occupational boundaries through jati membership. Benefit from clear status signaling, endogamous marriage networks, and mutual obligation systems that provide insurance against individual hardship. Jati membership confers predictable social obligations and access to community resources.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, local_communities, beneficiary,
    moderate, generational, constrained, local).

% Adjudicate jati boundaries through ritual authority: determine who may participate in ceremonies, declare pollution violations, and negotiate corrections through purification rites. Collect fees and deference for these services. Control the interpretive machinery that allows jati categories to shift without explicit rupture with tradition.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, ritual_specialists, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, ritual_specialists, agenda_setter).

% Use jati-like associational forms to coordinate craft standards, apprenticeship, price-setting, and market access. Renegotiate boundaries around occupational skill and specialization. Benefit from the flexibility to create new subcategories as trade routes shift and new goods enter circulation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, merchant_guilds, beneficiary,
    organized, biographical, mobile, regional).

% Attempt occupational or status mobility within or across jati categories. Pay costs in ritual correction, community sanction, or negotiation with ritual authorities when seeking boundary movement. Their aspirations drive continuous renegotiation, but they are constrained by the identity frame itself—mobility is thinkable only within jati logic, not outside it.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, individual_aspirants, payer,
    powerless, biographical, identity_locked, local).

% Arrive in new territories without established jati position and are classified by local ritual specialists using flexible boundary-matching logic. Their positioning is a negotiated outcome, but they have minimal say in the process. Would challenge the system if they had organizational capacity or exit options.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, excluded_migrants, excluded,
    powerless, biographical, trapped, local).

% Maintains textual varna framework as normative theory while tolerating empirical proliferation. Observes and occasionally critiques local practice as deviation from proper order, but supplies the interpretive vocabulary (varna categories, dharma logic) that communities use to rationalize their jati boundaries.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, brahminical_orthodoxy_authority, observer,
    institutional, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes durable occupational and kinship boundaries that allow coordination on marriage rules (endogamy), ritual obligations, mutual aid, and occupational apprenticeship within local territories. Each jati solves repeated collective-action problems—how to maintain craft standards, ensure labor reliability, organize rituals—without centralized enforcement. The flexibility to renegotiate boundaries allows the system to absorb occupational change and demographic mobility.
% TRANSFER_FUNCTION: Transfers deference, ritual fees, and occupational protection from aspirants and boundary-crossers to ritual specialists and established jati groups. Low-volume; mostly symbolic rather than material. The 'payment' is primarily in social recognition and sanction vulnerability, not extraction of goods or labor.
% ABSENT_VOICES: Individuals who reject jati identity entirely and those excluded from jati membership by extreme pollution status (historically sweepers, tanners) are not in the conversation. They would argue for exit from the system entirely rather than renegotiation within it. Women within jati groups also have limited formal voice in boundary-setting, though they enforce endogamy norms.
% DISAPPEARANCE_RATIONALE: If jati norms and the renegotiation machinery vanished overnight, occupational organization and marriage alliance systems would need to reorganize. Guild coordination would shift to other associational forms. Ritual specialists would lose their interpretive authority. Local communities would need alternative institutions for status signaling and mutual aid—the vacuum would not remain empty.
% FOUNDING_PROBLEM: How to coordinate repeated occupational, ritual, and kinship interactions in village societies where trust relationships are face-to-face and occupational specialization requires reliable training pipelines and non-kin labor. The founding problem is the perpetual need to match new people and occupational categories to recognized social positions within existing frameworks.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary anthropological field studies document ongoing jati renegotiation and proliferation (Appadurai, Mayer, Dirks). Local communities affirm the coordination function—endogamy rules, ritual obligations, occupational apprenticeship remain live. The brahminical orthodoxy acknowledges the founding problem in classical varna literature while treating local proliferation as deviation.
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.38) because the coordination benefits genuinely accrue to participants—endogamy, ritual insurance, occupational apprenticeship are real. The extraction that does occur (ritual fees, deference to specialists) is limited by the fact that ritual authority cannot create jati membership ex nihilo; it only interprets and corrects boundaries participants already occupy. Suppression is notably low (0.22) because no centralized enforcement apparatus punishes jati violation—community sanction is diffuse and ritual correction is voluntary. Theater ratio is low (0.18), indicating the coordination function remains primary; what performative activity exists (elaborate rituals, symbolic pollution narratives) genuinely supports boundary maintenance rather than masking extraction. Accessibility collapse is moderate (0.42) because alternatives to jati membership DO exist in principle (rejection of the identity frame itself), but are practically foreclosed by the identity-locking mechanism: individuals born into jati logic cannot easily imagine or access exit. Resistance is substantial (0.58) because aspirants continuously push boundaries, merchants innovate categories, and migrants challenge placement, creating friction that prevents the system from settling into fixed hierarchies. The measurement trajectory shows extractiveness rising slightly early (as colonial census pressures begin to introduce external reification), then stabilizing and slightly declining as local renegotiation reasserts dominance. Theater ratio and suppression remain flat and low throughout, confirming the coordination reading against the extraction reading.
 *
 * PERSPECTIVAL GAP:
 *   The ritual specialist and the individual aspirant see different constraints operating under the same label. From the specialist's seat, jati is a hermeneutic domain requiring expert interpretation and occasional correction—a professional niche. From the aspirant's seat, jati is an identity-locked category whose boundaries are negotiable only with specialist approval and community performance—extraction dressed as inevitability. The scholar seat (brahminical orthodoxy observer) sees both: jati as coordination machinery that the local practice reading captures accurately, but also as deviation from the varna ideal that the orthodox reading claims. The merchant seat sees opportunity: jati categories are plastic enough to accommodate occupational innovation, making them more resilient than the fixed-varna reading would suggest. The engine computes these divergences from the power/exit/beneficiary structure; the diversity of measured perceptions is the signal that the constraint operates differently at different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (local communities, ritual specialists, merchant guilds) sit at the low-extraction end: they benefit from the coordination function without bearing asymmetric costs. Their exit options range from constrained (communities) to mobile (merchants), but exit would be costly due to the loss of coordination benefits, not due to coercion. Individual aspirants are identity-locked—they accept the jati frame itself and seek mobility within it, not out of it. Their directionality is moderate (d ~0.55) because they both benefit from coordination and bear costs of boundary renegotiation; if their identity lock broke, directionality would shift to high-target, but the frame itself is not coerced. Excluded migrants face higher extraction (higher d) because they have minimal say in their initial placement and are classified by local specialists using opacity. Ritual specialists are beneficiaries but also agenda-setters; their d is low because they collect fees and deference voluntarily, not through suppression—communities could reject their authority but choose participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination of occupational, kinship, and ritual interaction in decentralized village societies) remains live and substantially solved by the jati system under this reading. The constraint has NOT suffered mandatrophy—its original function (matching individuals to reliable social positions) continues to operate, and empirical proliferation to 3000+ categories actually demonstrates the system's success at absorbing occupational and demographic change. Under the colonial-census reading (a sibling constraint), the same proliferation would signal breakdown; under the orthodox-textual reading, it would signal corruption. Under the localized-practice reading, it signals adaptation. No mandatrophy signal is present because the system is doing what it was built for. The constraint could suffer future mandatrophy if centralized state bureaucracies (census, reservation, quota) solidify jati categories into rigid administrative boxes, foreclosing local renegotiation—that would be a transition toward the colonial-census reading's structural logic. But that transition is not yet complete, and this reading captures the pre-solidification state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_voluntary_participation,
    'Is the suppression we observe in jati systems structural (external barriers, caste enforcement by state/dominant groups) or is it primarily internalized (identity frame so deep that exit is unthinkable even absent external barriers)?',
    'Comparative study of exit and renegotiation in contexts with varying state enforcement: do local jati boundaries remain fluid and participatory in the absence of state caste laws, or does internalized identity lock provide sufficient constraint without external enforcement?',
    'If internalized (high), the constraint''s measured suppression is lower than its effective suppression—individuals carry the constraint even after exit from local community. This would shift d upward for aspirants and suggest the reading underestimates extractiveness. If structural (low), the measured suppression accurately reflects the constraint and the reading''s rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_vs_voluntary_participation, empirical, 'Whether suppression is structural or internalized in jati identity.').

omega_variable(
    merchant_guild_vs_jati_distinction,
    'Are merchant guilds and jati categories genuinely the same coordination mechanism, or is the reading conflating two different structures that only superficially resemble each other?',
    'Ethnographic comparison of guild membership acquisition, boundary renegotiation processes, and enforcement mechanisms across historical periods and regions. Do guilds and jati use the same interpretive machinery, or do they differ in how membership is determined and boundaries policed?',
    'If genuinely the same coordination type, merchant participation in jati renegotiation confirms the rope reading and the flexibility of the system. If functionally distinct, the merchant seat might be operating under a different constraint (guild membership), and the jati constraint would show lower renegotiation capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merchant_guild_vs_jati_distinction, empirical, 'Whether merchant guilds and jati categories are the same or distinct coordination mechanisms.').

omega_variable(
    proliferation_as_adaptation_vs_dissolution,
    'Does the proliferation of jati categories (3000+) represent successful adaptation of the coordination system to changing occupational and demographic conditions, or does it represent the system''s erosion and failure to maintain meaningful boundaries?',
    'Longitudinal ethnographic study tracking whether proliferated jati categories remain socially salient for endogamy, occupational apprenticeship, and ritual obligation, or whether proliferation signals categories have become nominal and lost coordination function.',
    'If adaptive, the rope reading is correct—proliferation demonstrates the system''s resilience. If erosion, proliferation might signal transition toward mandatrophy or dissolution, and the constraint might be a piton (form persists, function atrophies). This omega sits at the boundary between the localized_practice_reading and both the orthodox_textual_reading and colonial_census_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_as_adaptation_vs_dissolution, empirical, 'Whether jati proliferation reflects system success or system breakdown.').

omega_variable(
    committer_frame_reading_underdetermination,
    'Does the evidence of continuous local renegotiation and flexibility actually favor the localized_practice_reading over the colonial_census_reading, or is the flexibility we observe itself a product of the interstitial period before colonial state enforcement fully solidifies categories?',
    'Temporal comparison: ethnographic data from pre-colonial, colonial-census, and post-independence periods. If flexibility persists post-census and despite active state enforcement, the reading is robust. If flexibility was characteristic of the pre-census period and has degraded since, the reading misattributes a temporary historical state to the constraint''s essential logic.',
    'If the reading is misattributing transience to essence, the constraint''s true classification might align with the colonial_census_reading under state enforcement, with jati becoming increasingly rigid and extractive over the 60-year interval. This omega documents the framing underdetermination itself: which reading''s reference frame better captures what is happening.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_frame_reading_underdetermination, conceptual, 'Whether the observed flexibility reflects the constraint''s essence or a transient historical window.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t0, jati_practice_norm__localized_practice_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jati_tr_t10, jati_practice_norm__localized_practice_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(jati_tr_t20, jati_practice_norm__localized_practice_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(jati_tr_t30, jati_practice_norm__localized_practice_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(jati_tr_t40, jati_practice_norm__localized_practice_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(jati_tr_t60, jati_practice_norm__localized_practice_reading, theater_ratio, 60, 0.18).

% Extraction over time
narrative_ontology:measurement(jati_be_t0, jati_practice_norm__localized_practice_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(jati_be_t10, jati_practice_norm__localized_practice_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(jati_be_t20, jati_practice_norm__localized_practice_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(jati_be_t30, jati_practice_norm__localized_practice_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(jati_be_t40, jati_practice_norm__localized_practice_reading, base_extractiveness, 40, 0.39).
narrative_ontology:measurement(jati_be_t60, jati_practice_norm__localized_practice_reading, base_extractiveness, 60, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t0, jati_practice_norm__localized_practice_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(jati_su_t10, jati_practice_norm__localized_practice_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(jati_su_t20, jati_practice_norm__localized_practice_reading, suppression_requirement, 20, 0.21).
narrative_ontology:measurement(jati_su_t30, jati_practice_norm__localized_practice_reading, suppression_requirement, 30, 0.22).
narrative_ontology:measurement(jati_su_t40, jati_practice_norm__localized_practice_reading, suppression_requirement, 40, 0.22).
narrative_ontology:measurement(jati_su_t60, jati_practice_norm__localized_practice_reading, suppression_requirement, 60, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__localized_practice_reading, 0.1).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__colonial_census_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, jati_practice_norm__orthodox_textual_reading).

% DUAL FORMULATION NOTE:
% The jati_practice_norm kernel is instantiated by three structurally distinct constraints: (1) localized_practice_reading (this constraint): jati as flexible coordination norms, low extractiveness, continuous renegotiation; (2) colonial_census_reading: jati as reified administrative categories, higher extractiveness, state-enforced boundaries; (3) orthodox_textual_reading: jati as deviation from varna, extractive through pollution enforcement. All three share the empirical referent (jati practice) but differ in interpretation and ε. The readings represent different seats' experience of the same institution: local communities and ritual specialists see coordination machinery; colonial administrators and census-makers see reifiable categories; brahminical orthodoxy sees deviation from normative order. The localized_practice_reading influences both siblings: it documents the pre-solidification state that the census reading would stabilize, and it provides the alternative interpretation that the orthodox reading contests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
