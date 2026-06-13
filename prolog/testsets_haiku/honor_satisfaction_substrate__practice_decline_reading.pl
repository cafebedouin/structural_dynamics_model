% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__practice_decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__practice_decline_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: honor_satisfaction_substrate__practice_decline_reading
 *   human_readable: Honor Code Coordination Under Legal Suppression (Practice Decline Reading)
 *   domain: cultural/legal/historical
 *
 * SUMMARY:
 *   This constraint models the honor code as a persistent NORMATIVE SUBSTRATE
 *   whose practice (dueling) declined due to exogenous legal enforcement, not
 *   because the code itself lost legitimacy or became unthinkable. The
 *   reading asserts: (1) the honor code remains culturally live as a
 *   coordination mechanism for reputation; (2) dueling declined because legal
 *   prohibition, institutional barriers, and opportunity costs made it
 *   impractical, not because the code's core premises were delegitimated; (3)
 *   the constraint is ROPE (coordination function + enforcement overhead) not
 *   mountain erosion or cultural transformation. Dueling remains imaginable
 *   within the code; it is enforcement that makes it prohibitively costly.
 *   This reading contrasts with the cultural_contraction_reading (honor code
 *   itself transformed from honor-culture to dignity-culture) and the
 *   composite_overdetermined_reading (both exogenous suppression AND
 *   endogenous delegitimation operated simultaneously with non-independent
 *   pathways).
 *
 * KEY AGENTS:
 *   - honor_code_practitioners (aristocrats, officers) — maintain and enforce the code; benefit from reputation coordination; identity-locked to the framework
 *   - honor_code_challengers (those insulted) — bear the direct cost of dueling; constrained exit; reputation damage if they refuse challenge
 *   - legal_authority (state apparatus) — criminalized dueling, prosecuted violators; enforces monopoly on conflict resolution
 *   - military_officer_corps — institutional preserve where honor code persists; maintains de facto enforcement while formally subordinate to law
 *   - bourgeois_commercial_class — adopted honor norms for status; mobile exit when legal penalties rise; less identity-locked than aristocratic practitioners
 *   - legal_scholars (excluded) — argue for law-based reputation remedies; excluded from honorific conversation; work to undermine code's legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__practice_decline_reading, 0.38).
domain_priors:suppression_score(honor_satisfaction_substrate__practice_decline_reading, 0.72).
domain_priors:theater_ratio(honor_satisfaction_substrate__practice_decline_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__practice_decline_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__practice_decline_reading, rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__practice_decline_reading, "Honor Code Coordination Under Legal Suppression (Practice Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__practice_decline_reading, "cultural/legal/historical").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__practice_decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__practice_decline_reading, 'b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae').
narrative_ontology:cs_kernel_codification('b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae', distributed).
narrative_ontology:cs_authority_grounding('b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae', practice).
narrative_ontology:cs_interpretation_layer_present('b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae').
narrative_ontology:cs_reading_relation('b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae', foundational, honor_code_normative_persistence).
narrative_ontology:cs_axiom_status(honor_code_normative_persistence, holdable).
narrative_ontology:cs_axiom_grounding('b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae', honor_code_normative_persistence, conventional).
narrative_ontology:cs_axiom('b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae', foundational, dueling_decline_exogenous_suppression_driven).
narrative_ontology:cs_axiom_status(dueling_decline_exogenous_suppression_driven, holdable).
narrative_ontology:cs_axiom_grounding('b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae', dueling_decline_exogenous_suppression_driven, empirically_contingent).
narrative_ontology:cs_reference_frame('b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae', decentralized_peer_enforced_reputation).
narrative_ontology:cs_drift_state('b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae', legal_monopolization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b1376a0e-36f3-4bc8-b7dd-f8cd441c00ae', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, honor_code_practitioners).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, social_reputation_system).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, honor_code_challengers).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, legal_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, bourgeois_commercial_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__practice_decline_reading, bourgeois_commercial_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentlemen and aristocrats whose social standing, family reputation, and personal identity depend on maintaining honor through adherence to the code. The code solved the coordination problem of reputation verification in pre-legal contexts: challenge-response establishes standing without state intermediation. Their benefit is continued legitimacy within their peer set. Exit would mean loss of social status within the reference community.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_code_practitioners, beneficiary,
    powerful, generational, identity_locked, national).

% Those insulted or challenged within the honor code framework who feel compelled to defend their reputation through prescribed dueling practices. They bear the direct cost: death, injury, legal prosecution if caught. They cannot simply ignore a challenge without accepting permanent reputation damage within their social circle. The code binds them into a costly practice they cannot exit without losing identity and standing.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, honor_code_challengers, payer,
    moderate, biographical, constrained, national).

% State apparatus that progressively criminalized dueling, prosecuted violators, and created institutional barriers (licensing, registration, oversight) that made the practice legally hazardous. Operates through courts, law enforcement, and institutional oversight. Does not directly benefit from honor coordination but enforces monopoly on legitimate conflict resolution.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, legal_authority, agenda_setter,
    institutional, generational, analytical, national).

% Merchant and professional classes who adopted honor-code norms as status climb but were not fully embedded in aristocratic honor networks. They benefit from the code's reputation function for commercial trustworthiness but can more easily exit dueling when legal penalties rise (business reputation and legal standing matter more than aristocratic honor). Lower identity lock than traditional practitioners.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, bourgeois_commercial_class, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, bourgeois_commercial_class, payer).

% Formally subordinate to legal authority but maintains de facto enforcement and endorsement of honor codes within military hierarchy. Officers are prosecuted for dueling but continue to defend code publicly; military culture becomes the institutional preserve where honor-code coordination persists legally and culturally. Maintains the code as normative substrate through institutional gatekeeping.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__practice_decline_reading, military_officer_corps, beneficiary).

% Intellectual class arguing that reputation can be defended through law (defamation suits, judicial channels) and that honor dueling is irrational. Excluded from the honor-code framework itself but actively work to undermine the code's legitimacy. They would argue for rational-legal substitutes for honor coordination. Their exclusion from the honorific conversation is structural to the code's persistence.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, legal_scholars_reform_advocates, excluded,
    organized, generational, analytical, national).

% Servants, workers, and non-gentry populations who cannot meaningfully challenge or defend honor within the aristocratic code. They are structurally excluded from the coordination function; their reputation defenses operate through different mechanisms (guild systems, communal witness, religious authority). The honor code's persistence does not benefit them and creates legal hazard if they attempt to participate.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, lower_social_orders, excluded,
    powerless, immediate, trapped, local).

% Historians, sociologists, and anthropologists who examine whether the honor code persists as normative substrate or underwent foundational transformation. They measure the constraint from outside; they do not benefit or bear costs directly.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__practice_decline_reading, cultural_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__practice_decline_reading, honor_code_practitioners).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__practice_decline_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Honor code solved reputation verification in contexts where state-backed legal identity was unavailable or insufficient: a challenge-response protocol established standing and trustworthiness within peer networks without relying on formal law or institutional intermediaries. Prior to legal monopolization, it coordinated high-status merchants, officers, and aristocrats around a shared understanding of what constitutes reputation and how it is defended.
% TRANSFER_FUNCTION: Transfers reputation legitimacy from the state-legal domain to the peer-network domain: honor satisfaction bypasses courts and magistrates and operates through direct challenge-response. Also transfers risk from reputation damage to physical injury and legal prosecution — challengers bear the cost of defending their standing within the code.
% ABSENT_VOICES: Lower-social-order populations have no seat in the honor framework; they cannot challenge nor be challenged. They would argue that reputation defense should be available to all statuses or mediated by law rather than privatized through dueling. Legal scholars pushing for law-based reputation remedies (defamation courts, libel remedies) are excluded from the honorific conversation itself — the code's legitimacy depends partly on their exclusion.
% DISAPPEARANCE_RATIONALE: If the honor code and its enforcement substrate vanished, reputation verification in commercial and military contexts would reorganize around legal channels (courts, licensing, credentials, written contracts). The institutional gap — the failure of early legal systems to provide trustworthy reputation mechanisms — would still be addressed, but through law rather than honor. Upper-class social standing and military hierarchy would shift to different legitimacy grounds.
% FOUNDING_PROBLEM: Reputation verification in the absence of state-backed legal identity: how do peers establish trustworthiness, standing, and readiness to defend promises in contexts where formal law is weak, unavailable, or inaccessible? The honor code provided a decentralized, peer-enforced answer.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and sociologists attest that early modern reputation problems were real and acute, particularly for merchant networks and military officer corps operating across jurisdictions. However, dispute arises over whether strengthened legal systems (contract law, defamation remedies, credentialing) actually solved the problem (making honor obsolete) or whether honor persists because law cannot fully substitute (supporting the practice-decline reading). No corroboration from outside the benefiting parties exists; the core debate is internal to historians and legal scholars.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__practice_decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__practice_decline_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__practice_decline_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__practice_decline_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__practice_decline_reading_tests).
:- end_tests(honor_satisfaction_substrate__practice_decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises from 0.18 to 0.38 across 1600–1900 because the coordination benefit (reputation verification within peer networks) persists while legal suppression costs accumulate — practitioners must defend honor against both social challenge AND legal prosecution. Suppression requirement rises steeply (0.12 to 0.72) as legal apparatus intensifies: dueling is progressively criminalized, prosecuted, and administratively blocked. Theater ratio rises (0.08 to 0.41) because by 1900 much of the activity maintaining the code is performative — public declarations of honor values, ceremonial endorsements in military and genteel contexts, continued emphasis on honor in literature and memoirs — rather than actual dueling. The constraint claims ROPE because (a) it solves a real coordination problem (reputation verification without state intermediation) and (b) its persistence depends on active enforcement (legal suppression creating the cost-benefit squeeze). At t=1600 (early), extraction is low because the coordination function is primary and enforcement burden is minimal. At t=1900 (late), extraction is higher and enforcement is the binding constraint: the code persists but is actively suppressed; practitioners maintain it despite increasing legal and institutional costs.
 *
 * PERSPECTIVAL GAP:
 *   The honor-code-practitioner seat experiences this as rope: they coordinate reputation within their peer network and the legal prohibition is external noise they manage through caution and discretion. The legal_authority seat experiences it as snare: dueling is a practice they are suppressing and the reputation coordination function is cover for status-preservation. The bourgeois_commercial_class sits between: they benefit from reputation coordination but can more easily exit dueling when legal penalties rise because their identity is less fused with the code. The engine computes these divergences from the structural data (power, exit_options, role); they should emerge as different computed types across seats.
 *
 * DIRECTIONALITY LOGIC:
 *   honor_code_practitioners are near-full beneficiaries (they set the code, enforce it socially, derive status from it; d near 0.0–0.2) despite bearing legal risk — the legal risk is exogenous to their benefit calculation. honor_code_challengers are full targets (they pay the cost of defending challenged reputation; they cannot refuse without losing status; d near 0.9–1.0). legal_authority sits as the enforcer collecting no direct benefit (d symmetric, ~0.5). military_officer_corps are asymmetrically positioned: they benefit from honor coordination AND maintain its enforcement, but they are also subordinate to legal authority, so their directionality is split (d ~0.3–0.4, a complex institutional relationship). The beneficiary/victim split is not symmetric because the code's coordination function genuinely benefits its practitioners, but the legal suppression is extractive for those trapped in the challenge-response structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reputation verification in weak-law contexts) is CONTESTED. Legal scholars argue it is DEAD — strengthened contract law, defamation courts, and commercial credentialing have provided superior reputation mechanisms. Practitioners argue it is LIVE — law cannot fully substitute for peer-network reputation, especially in military and genteel contexts where written law is too slow or politically entangled. The practice-decline reading resolves the mandatrophy by asserting the founding problem remains LIVE (reputation verification via law is incomplete) while the PRACTICE (dueling) declined due to exogenous cost, not endogenous delegitimation. This prevents false-summit classification: the code persists not as a natural law (emerges_naturally: false) but as a rope maintained through enforcement and social identity. If the constraint were reclassified as mountain (the honor code is a natural feature of human status hierarchies), the reading would fail — natural laws do not require legal suppression to persist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    code_persistence_vs_transformation,
    'Does the honor code persist as a stable normative commitment (practice_decline reading), or did the code itself transform from honor-culture to dignity-culture (cultural_contraction reading)?',
    'Textual analysis of honor-code articulations before and after 1750: do practitioners describe the same reputation-defense norms in different institutional contexts, or do the core values shift (from courage/readiness to law-abidingness/personal integrity)? Cross-cultural comparison: do honor codes persist in low-enforcement contexts (e.g., contemporary rural Mediterranean, Upper South) in recognizably continuous forms, or do they transform toward dignity-culture universally?',
    'If the code persists unchanged, the reading supports practice_decline (coordination function survives; practice declined due to exogenous cost). If the code transforms uniformly, the reading flips toward cultural_contraction (the code itself delegitimated endogenously). If partial persistence with transformation, the composite_overdetermined reading gains weight (both processes occurred).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(code_persistence_vs_transformation, empirical, 'Whether honor-code norms remain stable across institutional contexts or undergo foundational value transformation.').

omega_variable(
    counterfactual_low_enforcement,
    'In regions or periods where legal enforcement against dueling was weak or absent, did honor-code dueling persist at higher rates, or did it decline anyway for other reasons (urbanization, commercialization, changed military tactics)?',
    'Historical comparison of dueling rates in high-enforcement jurisdictions (Prussia, France post-1850) vs. low-enforcement contexts (Ottoman Empire, Russia before 1900, contemporary honor-culture societies). Control for urbanization, military professionalization, and commercial integration separately.',
    'If dueling persisted where enforcement was weak, exogenous suppression is the primary driver (practice_decline reading). If dueling declined regardless of enforcement, endogenous delegitimation or other structural factors dominate (cultural_contraction or overdetermined readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_low_enforcement, empirical, 'Whether legal suppression was necessary and sufficient for dueling decline.').

omega_variable(
    military_persistence_asymmetry,
    'Why did honor codes persist longer and more robustly in military institutions than in civilian society, if the code underwent cultural transformation (cultural_contraction reading)?',
    'Institutional comparison: military honor codes show continuous articulation from 1600–2000+ with recognizable terminology and practice (field of honor, officer''s duty, personal honor); civilian honor codes show rapid attenuation. Does this divergence reflect different institutional enforcement regimes (military exemption from prosecution, civilian legal pressure), or do military and civilian populations hold fundamentally different values?',
    'Institutional-enforcement divergence supports practice_decline (the code persists where enforcement allows). Value divergence supports cultural_contraction (military professions maintained honor-culture while civilian populations shifted to dignity-culture). Partial institutional explanation + partial value divergence supports composite_overdetermined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_persistence_asymmetry, empirical, 'Whether military persistence of honor codes reflects institutional exemption or cultural continuity.').

omega_variable(
    identity_lock_mechanism,
    'For honor-code practitioners, is the psychological/identity lock to the code rooted in culturally internalized norms (would persist even without legal status), or rooted in institutional position and social network dependency (would weaken if those structures changed)?',
    'Psychological and sociological analysis of practitioners'' self-reported attachment to honor norms: do they cite intrinsic values (courage, integrity, standing) or instrumental network effects (career advancement, peer respect, marriage eligibility)? Post-exit interviews from former practitioners: how quickly does honor-code identification decay after institutional or geographic exit?',
    'If identity lock is primarily institutional/network-dependent, legal suppression + urbanization + institutional change would predictably erode the code (supporting practice_decline as the achievable trajectory). If identity lock is primarily cultural/intrinsic, the code would persist even across institutional boundaries (supporting cultural_contraction''s implication that only value transformation halts persistence).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether honor-code identity fusion is rooted in internalized values or institutional position.').

omega_variable(
    sibling_reading_entanglement,
    'Are the practice_decline_reading and cultural_contraction_reading truly independent, or do they describe overlapping processes that the kernel''s abstraction separates artificially?',
    'Historical microanalysis of individual practitioners'' transitions: as they encounter legal suppression, do they reinterpret the code (cultural transformation) or do they compartmentalize it (maintain private normative commitment while avoiding public practice)? If individuals oscillate between reinterpretation and compartmentalization, the readings are entangled; if they consistently follow one path, the readings are structurally distinct.',
    'If entangled, the composite_overdetermined_reading captures the true constraint structure and the practice_decline/cultural_contraction split is an analyst''s convenience. If distinct, the readings remain valid alternative accounts and may be assigned to different populations (military: practice_decline; bourgeois: cultural_contraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_entanglement, conceptual, 'Whether the reading divergence reflects genuine structural alternatives or artificial analytical separation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__practice_decline_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1600, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1600, 0.08).
narrative_ontology:measurement_basis(hono_tr_t1600, projected).
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1700, 0.12).
narrative_ontology:measurement_basis(hono_tr_t1700, observed).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1750, 0.18).
narrative_ontology:measurement_basis(hono_tr_t1750, observed).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1800, 0.32).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1850, 0.39).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__practice_decline_reading, theater_ratio, 1900, 0.41).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1600, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1600, 0.18).
narrative_ontology:measurement_basis(hono_be_t1600, projected).
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1700, 0.28).
narrative_ontology:measurement_basis(hono_be_t1700, observed).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1750, 0.32).
narrative_ontology:measurement_basis(hono_be_t1750, observed).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1800, 0.38).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1850, 0.36).
narrative_ontology:measurement_basis(hono_be_t1850, observed).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__practice_decline_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement_basis(hono_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1600, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1600, 0.12).
narrative_ontology:measurement_basis(hono_su_t1600, projected).
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1700, 0.38).
narrative_ontology:measurement_basis(hono_su_t1700, observed).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1750, 0.52).
narrative_ontology:measurement_basis(hono_su_t1750, observed).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1800, 0.64).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1850, 0.7).
narrative_ontology:measurement_basis(hono_su_t1850, observed).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__practice_decline_reading, suppression_requirement, 1900, 0.72).
narrative_ontology:measurement_basis(hono_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__practice_decline_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__practice_decline_reading, 0.12).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__cultural_contraction_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__practice_decline_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the honor_satisfaction_substrate kernel. The kernel encodes a contested commitment about how reputation is verified and defended in peer networks. The practice_decline_reading (this story) asserts the honor code persists as a stable normative substrate while dueling declined due to exogenous legal suppression. Sibling readings: cultural_contraction_reading asserts the code itself transformed from honor-culture to dignity-culture; composite_overdetermined_reading asserts both exogenous suppression AND endogenous delegitimation operated simultaneously. See cs_structure.reading_relations for structural relationships between readings. Each reading has a distinct epsilon and independent metrics; the readings are linked because they contest the same kernel and explain the same historical fact (dueling's decline) through different structural mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__practice_decline_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
