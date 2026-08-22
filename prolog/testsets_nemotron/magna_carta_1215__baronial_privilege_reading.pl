% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__baronial_privilege_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta 1215 — Baronial Privilege Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story models the 1215 Magna Carta as the barons who forced it
 *   understood it: a feudal peace treaty binding King John to specific fiscal
 *   and judicial concessions for the benefit of the landowning baronial
 *   class. 'Free man' (liber homo) is a term of art designating free tenants
 *   — a narrow legal category excluding the villein majority, women as
 *   rights-bearing subjects, and the landless. The constraint is a tangled
 *   rope: it solved a genuine coordination problem (credible commitment
 *   between king and barons) while extracting from the crown and
 *   systematically excluding the majority population from its protections.
 *   The reissues of 1216/1217/1225 progressively stripped enforcement
 *   mechanisms (council of 25, distraint clause) while retaining the symbolic
 *   text — the extraction from the crown declined, but the exclusionary
 *   structure persisted.
 *
 * KEY AGENTS:
 *   - landowning_barons: Primary beneficiaries and agenda-setters (organized/constrained) — gained enforceable limits on royal power
 *   - crown_fiscal_authority: Primary payer (institutional/constrained) — lost arbitrary fiscal/judicial discretion
 *   - commoners_excluded, women_excluded, non_landowners_excluded: Excluded victims (powerless/trapped) — no protections, intensified subjection
 *   - royal_administration_agents: Secondary beneficiaries (institutional/mobile) — gained procedural regularization
 *   - legal_historian_observer: Analytical seat (analytical/analytical) — sees full feudal structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.78).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.82).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215 — Baronial Privilege Reading").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '26da8d8e-496a-44ba-b332-c78f2b71d421').
narrative_ontology:cs_kernel_codification('26da8d8e-496a-44ba-b332-c78f2b71d421', fixed_text).
narrative_ontology:cs_authority_grounding('26da8d8e-496a-44ba-b332-c78f2b71d421', lineage).
narrative_ontology:cs_reading_relation('26da8d8e-496a-44ba-b332-c78f2b71d421', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('26da8d8e-496a-44ba-b332-c78f2b71d421', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('26da8d8e-496a-44ba-b332-c78f2b71d421', foundational, charter_binds_only_contracting_parties).
narrative_ontology:cs_axiom_status(charter_binds_only_contracting_parties, holdable).
narrative_ontology:cs_axiom_grounding('26da8d8e-496a-44ba-b332-c78f2b71d421', charter_binds_only_contracting_parties, conventional).
narrative_ontology:cs_axiom('26da8d8e-496a-44ba-b332-c78f2b71d421', foundational, liber_homo_means_free_tenant).
narrative_ontology:cs_axiom_status(liber_homo_means_free_tenant, holdable).
narrative_ontology:cs_axiom_grounding('26da8d8e-496a-44ba-b332-c78f2b71d421', liber_homo_means_free_tenant, conventional).
narrative_ontology:cs_reference_frame('26da8d8e-496a-44ba-b332-c78f2b71d421', runnymede_1215_settlement).
narrative_ontology:cs_drift_state('26da8d8e-496a-44ba-b332-c78f2b71d421', henry_iii_minority_reissues, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('26da8d8e-496a-44ba-b332-c78f2b71d421', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, royal_administration_agents).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, crown_fiscal_authority).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, commoners_excluded).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, women_excluded).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, non_landowners_excluded).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, feudal_contractualism).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, baronial_counsel_consent).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, customary_law_binding_king).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The barons who forced the charter at Runnymede. They gained enforceable limits on royal arbitrariness over their persons and property, secured consent rights for scutage/aid, and established a council of 25 to enforce compliance. Their exit is constrained: rebellion was the enforcement mechanism, and the charter's viability depended on their collective military credibility.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, landowning_barons, agenda_setter).

% Sheriffs, castellans, and royal justices who received procedural regularization from clauses standardizing assizes, inquests, and amercements. The charter reduced arbitrary royal demands on their offices, making administration more predictable. They could transfer to other royal posts or ecclesiastical careers — mobile within the institutional field.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, royal_administration_agents, beneficiary,
    institutional, generational, mobile, national).

% The king's treasury and fiscal machinery. The charter capped relief payments, fixed scutage rates, required common counsel for aids, and banned arbitrary amercements — directly reducing extractive capacity. The crown could not exit the constraint without repudiating the charter and renewing civil war; its enforcement mechanism (baronial council) was a standing threat.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, crown_fiscal_authority, payer,
    institutional, generational, constrained, national).

% Villeins, cottars, and free tenants not of baronial rank. Clause 39 ('no free man') was read at the time as protecting only those who held by free tenure — a small minority. Commoners had no voice at Runnymede, no enforcement access, and no exit from manorial jurisdiction. They bore the charter's opportunity cost: royal concessions to barons were often offset by intensified seigneurial extraction downward.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, commoners_excluded, excluded,
    powerless, immediate, trapped, local).

% Noble and non-noble women alike. The charter's language of 'free men' (liber homo) was gendered in practice: widows received specific protections (dower, remarriage consent) but as property-holding exceptions, not rights-bearing subjects. Women had no independent standing in the enforcement council and no exit from the patriarchal tenure system the charter reinforced.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, women_excluded, excluded,
    powerless, immediate, trapped, local).

% Merchants, clergy without temporalities, landless laborers. The charter's protections attached to land tenure and baronial status. Urban merchants gained limited customs protections (Clause 41) but only as a class concession, not individual right. No enforcement access, no exit from the social order the charter ratified.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, non_landowners_excluded, excluded,
    powerless, immediate, trapped, local).

% Modern scholarly seat analyzing the 1215 text in its feudal context. Sees the charter as a specific peace treaty between king and baronial opposition, not a constitution. Reads 'free man' as a term of art for free tenants-in-chief and their immediate mesne tenants — a narrow legal category, not a universal anthropological one.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, legal_historian_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ended a civil war by converting baronial military leverage into a written settlement: the king's arbitrary fiscal and judicial power over the barons was bounded; a council of 25 barons was authorized to enforce compliance by distraint of royal castles and lands. The coordination problem was credible commitment — neither side trusted the other to honor oral promises.
% TRANSFER_FUNCTION: Moves fiscal authority and judicial discretion from the crown to the baronial class: relief payments capped at 100 pounds for earls/barons/knights; scutage fixed at customary rates; amercements proportional to offense and assessed by peers; no aid without common counsel. The transfer is from royal purse to baronial retention.
% ABSENT_VOICES: The villein majority, women of all classes, urban merchants beyond limited customs clauses, and the landless. They were not at Runnymede, had no role in the enforcement council, and the charter's language ('free man') was understood by the parties to exclude them. Their objections — had they been articulable in 1215 terms — would have been that the settlement intensified their subjection by legitimating baronial autonomy.
% DISAPPEARANCE_RATIONALE: If the 1215 charter vanished overnight, the civil war would resume immediately. The barons' military position would collapse without the charter's legitimating text; the crown would reclaim full fiscal discretion. The specific institutional settlement — council of 25, capped reliefs, proportional amercements — would disappear. The world of 1215 England rearranges.
% FOUNDING_PROBLEM: King John's arbitrary and escalating fiscal extraction from the baronial class (excessive reliefs, arbitrary scutage, punitive amercements, seizure of heiresses/wardships) combined with judicial capriciousness (selling justice, delaying courts, removing cases to coram rege) provoked a baronial rebellion that military stalemate could not resolve. The charter was the peace treaty.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — John's specific fiscal-judicial tyranny over the barons — died with John (1216) and the charter's reissue under Henry III's minority. The baronial class's immediate grievances were substantially addressed by the 1216/1217/1225 reissues. No external corroborator attests the 1215 problem as live; the charter's persistence after 1225 is explained by its symbolic utility, not the original problem. (Carpenter 1990; Holt 1992; Turner 2003 — historians outside the baronial beneficiary line.)
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end, 0.85 at 1215) because the charter transferred substantial fiscal authority from crown to barons — relief caps, scutage limits, counsel requirements, proportional amercements. Suppression is high (0.82) because the charter's persistence depended on active enforcement: the council of 25 barons with distraint authority, the threat of renewed war, and the explicit exclusion of the majority from any claim on the charter's protections. Theater is moderate (0.25): the charter performed 'law' and 'custom' but its enforcement mechanism was naked baronially-military power. The measurement series shows extraction and suppression declining across the reissues (1216-1225) as enforcement mechanisms were stripped — the constraint became more symbolic, less operationally extractive from the crown, but its exclusionary structure (who counts as 'free man') remained stable.
 *
 * PERSPECTIVAL GAP:
 *   From the baronial seat, the charter is genuine coordination: a credible commitment device that ended war and bounded royal arbitrariness. From the crown's seat, it is enforced extraction: a gun-to-the-head fiscal transfer. From the excluded seats, it is a snare: a settlement that legitimated their subjection by ratifying baronial autonomy. The engine computes these divergent per-seat classifications from the structural data — the claimed_type (tangled_rope) reflects the baronial seat's experience of coordination + the crown's experience of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The barons are structural beneficiaries (d ~0.15): they collected the fiscal transfer, controlled enforcement, and faced constrained but credible exit (rebellion). The crown is the structural target (d ~0.85): it bore the full fiscal transfer, faced active enforcement machinery, and could not exit without civil war. Royal administrators are near-symmetric beneficiaries (d ~0.4): procedural regularization benefited them but they remained royal agents. The excluded groups (commoners, women, non-landowners) are structural victims with d ~0.95: they bore the opportunity cost of baronial gains (intensified seigneurial extraction), had no enforcement access, and were trapped in the social order the charter ratified. The engine derives these directionalities from the beneficiary/victim declarations plus exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (John's tyranny over barons) was dead by 1225 — the reissues under Henry III's minority addressed the specific grievances. Yet the charter persisted and was mythologized. The constraint did not dissolve because its symbolic capital was captured by subsequent actors (parliamentarians, common lawyers, colonists) who read it as a universal rights document — a reading this story does not instantiate. The baronial privilege reading has no mandatrophy: it accurately describes the 1215-1225 constraint as a time-bounded settlement. The mandatrophy belongs to the universal_rights_reading, which claims the charter solves a problem (universal due process) it was not built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baronial_coordination_vs_extraction_boundary,
    'How much of the charter''s fiscal transfer from crown to barons was the price of genuine coordination (credible commitment ending civil war) versus pure baronial rent-seeking?',
    'Counterfactual modeling: if John had voluntarily offered the same terms without baronial rebellion, would the coordination function (bounded royal power, regularized administration) have been achieved at lower extraction? Comparative analysis with other medieval charters (e.g., Catalan Corts, Hungarian Golden Bull) where coordination emerged with different extraction profiles.',
    'If the transfer was mostly coordination-price, the constraint is more rope-like; if mostly rent-seeking, more snare-like. The tangled_rope claim rests on the judgment that both are substantially present.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(baronial_coordination_vs_extraction_boundary, conceptual, 'Whether the baronial fiscal gains were coordination cost or extraction surplus').

omega_variable(
    free_man_semantic_stability,
    'Was ''liber homo'' in 1215 a stable term of art with a fixed technical meaning (free tenant), or was it semantically open in ways the barons exploited or the crown contested?',
    'Paleographic and diplomatic analysis of contemporary usage across royal charters, manorial records, and legal pleadings 1199-1225. Track whether the term''s extension was litigated in the 1215-1225 period.',
    'If ''free man'' was technically fixed, the exclusionary structure is a deliberate feature of the baronial reading. If semantically contested, the universal_rights_reading has a stronger textual foothold even in 1215. Affects the foreclosure relation between readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(free_man_semantic_stability, empirical, 'Whether the charter''s key term was semantically closed or open at issuance').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Does the baronial_privilege_reading logically foreclose the universal_rights_reading within any single commitment framework, or do they occupy non-overlapping domains (1215 textual meaning vs. transhistorical symbolic meaning)?',
    'Structural analysis of the two readings'' axiom sets: if the baronial reading''s foundational axiom (''charter binds only contracting parties'') directly contradicts the universal reading''s foundational axiom (''charter emits universal due process''), and no framework can hold both without contradiction, the relation is forecloses. If they operate at different temporal/register levels, coexists_with.',
    'Determines the reading_relation declaration in cs_structure. A forecloses relation means the kernel has genuine logical fracture; coexists_with means the contest is about interpretive authority, not logical consistency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'Logical relationship between baronial and universal readings of the same kernel').

omega_variable(
    mandatrophy_attribution,
    'Does the baronial_privilege_reading itself suffer mandatrophy (claiming to solve a dead problem), or is the mandatrophy entirely located in the sibling universal_rights_reading?',
    'Check whether this reading''s claimed_type and metrics describe a constraint whose founding problem (John''s tyranny over barons) was live during 1215-1225. The founding_problem_status = dead with disappearance_verdict = world_rearranges is a mismatch the engine flags — but only if the reading claims continuing relevance. This reading claims historical accuracy for 1215-1225 only.',
    'If this reading has mandatrophy, its claimed_type should shift toward piton or snare. If not, the mandatrophy flag correctly targets only the universal reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_attribution, conceptual, 'Whether the baronial reading misrepresents its own temporal scope').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc1215_bpr_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement(mc1215_bpr_tr_t1216, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1216, 0.2).
narrative_ontology:measurement(mc1215_bpr_tr_t1217, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1217, 0.22).
narrative_ontology:measurement(mc1215_bpr_tr_t1225, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1225, 0.25).

% Extraction over time
narrative_ontology:measurement(mc1215_bpr_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.85).
narrative_ontology:measurement(mc1215_bpr_be_t1216, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1216, 0.65).
narrative_ontology:measurement(mc1215_bpr_be_t1217, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1217, 0.6).
narrative_ontology:measurement(mc1215_bpr_be_t1225, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1225, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(mc1215_bpr_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.9).
narrative_ontology:measurement(mc1215_bpr_su_t1216, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1216, 0.75).
narrative_ontology:measurement(mc1215_bpr_su_t1217, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1217, 0.7).
narrative_ontology:measurement(mc1215_bpr_su_t1225, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1225, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__baronial_privilege_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__living_document_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, english_common_law_due_process).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, parliamentary_supremacy_tradition).

% DUAL FORMULATION NOTE:
% The magna_carta_1215 kernel decomposes into three constraint stories: this baronial_privilege_reading (tangled rope: genuine coordination + asymmetric extraction + exclusion), the universal_rights_reading (snare: universal rights cover for ongoing exclusion), and the living_document_reading (scaffold: transitional interpretive authority with sunset at codification). This reading is the upstream historical anchor — the other two readings cite it as their origin while structurally transforming its beneficiary/victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_1215__baronial_privilege_reading, organized, 0.15).
constraint_indexing:directionality_override(magna_carta_1215__baronial_privilege_reading, institutional, 0.85).
constraint_indexing:directionality_override(magna_carta_1215__baronial_privilege_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
