% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta 1215 — Baronial Privilege Reading (Feudal Contract)
 *   domain: constitutional/legal_history/political_theory
 *
 * SUMMARY:
 *   June 1215, Runnymede: an armed baronial covenant forces a sealed
 *   settlement on King John limiting reliefs, scutage, wardship, amercement,
 *   and unilateral levy, with a baronial-elected committee of twenty-five
 *   empowered to distrain the crown on default. This file instantiates the
 *   BARONIAL PRIVILEGE READING of that instrument: the charter is a feudal
 *   contract among specific parties; 'free man' denotes the contracting
 *   tenants-in-chief; the protection set is limited to those parties (plus
 *   the church by express clause). On this reading the arrangement solves a
 *   real king-vassal coordination problem while concentrating its benefits in
 *   the contracting class — the crown pays, Jewish creditors pay through
 *   clauses 10-11, and the unfree majority, women as a class, and the borough
 *   commons stand wholly outside the protection set. The claim/metric gap is
 *   deliberate: claimed_type is authored from what I believe structurally
 *   true (hybrid coordination-plus-extraction, actively enforced), the
 *   metrics from what I believe descriptively true of the arrangement's
 *   operation; the engine computes per-seat classifications from the
 *   structural data and measures any divergence. This is one member of a
 *   three-file constraint family: the sibling readings change the extension
 *   of the protected class, which changes the victim/beneficiary sets, which
 *   changes epsilon — hence separate files linked by network edges, per the
 *   epsilon-invariance decomposition rule.
 *
 * KEY AGENTS:
 *   - rebel_landowning_barons: Primary beneficiary and enforcement principal (organized/constrained) — collects scheduled reliefs, protected tenure, and clauses 10-11 debt-relief value; elects and runs the Twenty-Five
 *   - the_crown: Primary target (institutional/constrained) — bears the transfer of extractive capacity; sealed under duress, repudiated via Rome, warred, died mid-conflict
 *   - english_church_hierarchy: Secondary beneficiary (institutional/constrained) — Clause 1 liberties and clerical amercement exemptions; brokered the settlement
 *   - jewish_moneylenders: Collateral payer (powerless/trapped) — clauses 10-11 move debt-relief value to baronial estates at their expense; no seat, no shield
 *   - unfree_villeinage_tenants: Excluded majority (powerless/trapped) — outside the protection set entirely; Clause 60 reciprocity gives them no standing mechanism
 *   - noble_women_and_widows: Excluded class (powerless/trapped) — clauses 7-8 operate as estate-preservation for contracting lineages, not class protection
 *   - urban_townsfolk: Excluded commons (organized/constrained) — incidental commercial clauses, no seat in common counsel
 *   - pope_innocent_iii: External veto player (institutional/analytical) — annulled the charter and reshaped the enforcement environment without being a party
 *   - constitutional_historians: Analytical observer (analytical/analytical) — attests the fiscal crisis and the narrow extension from outside every party
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.45).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.55).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215 — Baronial Privilege Reading (Feudal Contract)").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '1e8f873a-6fb3-4629-bd2f-0cee6c86584b').
narrative_ontology:cs_kernel_codification('1e8f873a-6fb3-4629-bd2f-0cee6c86584b', fixed_text).
narrative_ontology:cs_authority_grounding('1e8f873a-6fb3-4629-bd2f-0cee6c86584b', lineage).
narrative_ontology:cs_interpretation_layer_present('1e8f873a-6fb3-4629-bd2f-0cee6c86584b').
narrative_ontology:cs_reading_relation('1e8f873a-6fb3-4629-bd2f-0cee6c86584b', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('1e8f873a-6fb3-4629-bd2f-0cee6c86584b', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('1e8f873a-6fb3-4629-bd2f-0cee6c86584b', foundational, rights_attach_to_covenant_parties_only).
narrative_ontology:cs_axiom_status(rights_attach_to_covenant_parties_only, holdable).
narrative_ontology:cs_axiom_grounding('1e8f873a-6fb3-4629-bd2f-0cee6c86584b', rights_attach_to_covenant_parties_only, conventional).
narrative_ontology:cs_axiom('1e8f873a-6fb3-4629-bd2f-0cee6c86584b', secondary, charter_language_tracks_feudal_usage).
narrative_ontology:cs_axiom_status(charter_language_tracks_feudal_usage, holdable).
narrative_ontology:cs_axiom_grounding('1e8f873a-6fb3-4629-bd2f-0cee6c86584b', charter_language_tracks_feudal_usage, empirically_contingent).
narrative_ontology:cs_reference_frame('1e8f873a-6fb3-4629-bd2f-0cee6c86584b', feudal_covenant_tradition).
narrative_ontology:cs_drift_state('1e8f873a-6fb3-4629-bd2f-0cee6c86584b', post_1225_definitive_reissue, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1e8f873a-6fb3-4629-bd2f-0cee6c86584b', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, rebel_landowning_barons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, english_church_hierarchy).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, the_crown).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, jewish_moneylenders).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, feudal_covenant_supremacy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, henrician_coronation_charter_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Northern and eastern magnates in armed covenant since early 1215. They dictated the settlement terms at Runnymede, received scheduled reliefs and protection of tenure, and reserved to themselves the right to elect twenty-five of their number to seize crown castles and revenues if the king defaults. Leaving the arrangement means disbanding the army that produced it and returning to the king's mercy; staying means administering the enforcement machinery themselves.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, rebel_landowning_barons, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, rebel_landowning_barons, agenda_setter).

% King John sealed the charter with twenty-five witnesses after his field army dissolved before London. The terms cap his reliefs, require common counsel before any scutage or aid, subject his amercements to sworn assessment, and expose him to distraint by the Twenty-Five. His exits: comply, appeal to his liege lord the Pope for annulment (pursued within weeks), or reopen the war (pursued by autumn). He died mid-war in October 1216; his nine-year-old successor reissued diluted versions under regency.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, the_crown, payer,
    institutional, biographical, constrained, national).

% Archbishop Stephen Langton and the episcopate brokered between the parties. Clause 1 confirms the church's freedom and free elections; later clauses exempt clerical property from most amercements and route intestate goods through diocesan supervision. The church gained formal guarantees while remaining exposed to papal politics — Langton was suspended by Rome during the crisis.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, english_church_hierarchy, beneficiary,
    institutional, generational, constrained, national).

% Royal-dependent creditor communities whose loans financed baronial estates. Clauses 10-11 suspend interest on debts to Jews during a tenant-in-chief's minority and give widows their shares free of interest, moving debt-relief value to the contracting families at the creditors' direct expense. No Jewish party sat at Runnymede; their traditional shield — the king's protection — is the very power being bound.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, jewish_moneylenders, payer,
    powerless, biographical, trapped, national).

% The majority of the population, bound to manors by hereditary servility. The settlement's liberties attach to free tenure; nothing in the covenant reaches them, and the enforcement machinery answers to baronial interests alone. Clause 60's reciprocity formula gestures at lords observing the same customs toward their men, but no mechanism gives their grievances standing. Exit from the manor is not available to them.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, unfree_villeinage_tenants, excluded,
    powerless, generational, trapped, local).

% Wives, widows, and heiresses of the contracting class. Clauses 7-8 secure a widow's dower and forbid forced remarriage, and wards may not be married beneath their station — provisions that preserve estate integrity within baronial lineages. Women as a class have no standing in the covenant; what reaches them arrives only as vessels of family land.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, noble_women_and_widows, excluded,
    powerless, biographical, trapped, national).

% London's commune and the greater boroughs. Clause 13 confirms London's ancient liberties and Clause 41 grants merchant safe-conduct, but the common counsel of clauses 12 and 14 summons bishops, abbots, earls, barons, and tenants-in-chief — no borough seat. Towns receive incidental commercial grace and consent to nothing.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, urban_townsfolk, excluded,
    organized, biographical, constrained, regional).

% John's liege lord for the surrendered kingdom. He declared the charter null in August 1215, suspended Archbishop Langton, and absolved the king's oath — recasting the settlement as an affront to divinely ordered hierarchy rather than a lawful covenant. His verdict reshaped the enforcement environment without making him a party to the terms.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, pope_innocent_iii, observer,
    institutional, generational, analytical, continental).

% Later analysts working from Pipe Rolls, Close Rolls, papal registers, and chronicles. They reconstruct the fiscal crisis behind the settlement and test who could actually invoke the charter's clauses in period courts. They hold no stake in the arrangement and attest its structure from outside every party.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__baronial_privilege_reading, rebel_landowning_barons).
narrative_ontology:fixing_cost_class(magna_carta_1215__baronial_privilege_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes and adjudicates the king-vassal fiscal relationship: reliefs fixed at scheduled rates, wardship and marriage of heirs regulated, amercements scaled to offense and assessed by sworn neighbors, extraordinary levies requiring common counsel, and disputes routed through counsel and the security clause instead of the battlefield.
% TRANSFER_FUNCTION: Moves discretionary extractive capacity from the crown to the baronial class: caps royal levies, converts arbitrary reliefs into scheduled ones, hands enforcement power to a baronial-elected committee, and (clauses 10-11) shifts debt-relief value from Jewish creditors to baronial estates.
% ABSENT_VOICES: Unfree peasants, borough commons beyond London's incidental grace, women as a class, and Jewish creditors — none summoned to Runnymede. The common counsel of clauses 12-14 comprises bishops, abbots, earls, barons, and tenants-in-chief only. Their objections are unrecorded because no channel existed through which to record them.
% DISAPPEARANCE_RATIONALE: If the settlement vanished overnight in June 1215, the armed covenant does not dissolve — the bargaining continues by other instruments (coronation-oath renewals, ad hoc confirmations, continued war). The specific settlement terms demonstrably organized what followed: the 1216 and 1217 reissues, the Forest Charter spun off in 1217, the tax-for-confirmation bargain of 1225, and the regency's entire legitimating strategy ran through this document.
% FOUNDING_PROBLEM: Cap royal fiscal and judicial predation on concentrated vassal wealth after the loss of Normandy in 1204 made exaction unbearable: scutage at unprecedented rates, reliefs set at the king's will, wardship and marriage sold, amercements ruinous, foreign favorites enriched.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by: Pipe Roll and Norman Exchequer records showing the post-1204 exaction spike; papal registers (Innocent III acknowledged the grievances even while annulling the remedy); the semi-neutral Barnwell chronicler's account of the war's causes; and modern fiscal-legal historiography (J.C. Holt, David Carpenter, Ralph Turner) reconstructing the crisis from the financial records. Baronial-party testimony obviously exists; the corroborating weight rests on the non-party sources named above.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).
:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.45: the arrangement reallocates extractive capacity rather than abolishing it — crown levies are capped and scheduled (real service to vassals), but the benefit pool is closed to the contracting class, clauses 10-11 take value from Jewish creditors outright, and enforcement power concentrates in baronial hands. Suppression 0.55: persistence required armed enforcement, papal contestation, and civil war, offset by genuine consent machinery (common counsel, sworn assessment) — suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater 0.25: the fiscal and judicial functions were real; the ceremonial preamble and the security clause's ritual apparatus contribute modest performance. Accessibility collapse 0.35: alternatives stayed open throughout — appeal to Rome, renegotiation, reissue, renewed war — so understanding the arrangement did not foreclose exit-shaped responses. Resistance 0.75: near-maximal — royal repudiation within ten weeks, papal annulment, reciprocal excommunication politics, civil war with French invasion, three reissues in a decade. The measurement series run on one shared seven-point grid (1215, 1216, 1217, 1219, 1221, 1223, 1225) with every tracked metric authored at every point; the trajectories are monotone rather than cyclical — extraction and enforcement intensity decline as imposed settlement consolidates into bargained confirmation, while theater rises slowly as the document acquires symbolic weight. Receipt surface: the gains demonstrably accrue to the baronial seat (scheduled reliefs, protected tenure, the enforcement machinery, clauses 10-11 value), so gain_flow names that seat rather than asserting diffuseness; the only actor positioned to remove the arrangement — the crown — could do so only by civil war, which cost John his life, so fixing_cost is prohibitive. The papal annulment route looked cheap and proved non-durable: the charter returned within a year under military sponsorship.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit incompatible documents. From the crown's seat the settlement is confiscatory usurpation — John called its authors traitors and moved to annul it within weeks. From the baronial seat it is restoration of ancient lawful custom, the coronation-oath tradition finally reduced to enforceable text. From the excluded seats (unfree tenants, women, borough commons, Jewish creditors) it is neither protection nor burden but invisibility — a settlement that organizes the realm's highest fiscal relationship while giving them no standing in it. From the papal seat it is rebellion against divinely ordered hierarchy, void ab initio. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The crown sits at the full-target end: it bears the entire transfer (capped levies, scheduled reliefs, distraint exposure) and its exit options were compliance, Roman annulment, or war — no arbitrage. The barons sit at the full-beneficiary end: they collect the protections AND administer enforcement through the Twenty-Five, a dual position captured by their beneficiary role with agenda_setter secondary role. The church is near-beneficiary with mild symmetry: Clause 1 guarantees are real, but the church paid in papal politics (Langton's suspension) and acted as broker rather than pure collector. Jewish moneylenders derive high directionality from the victim declaration: clauses 10-11 extract from them directly, and their exit was closed because royal protection — their shield — is precisely what the settlement bound. The excluded stakeholders (villeins, women, townsfolk) are deliberately absent from the beneficiary/victim arrays: their structural position is scope-boundary, not extraction-edge, and their classification follows from that absence plus their stakeholder situations rather than from any override. No directionality overrides are authored: the derivation chain from beneficiary/victim declarations plus exit options captures every seated agent's position.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim is what prevents mislabeling in both directions. Reading the arrangement as pure rope erases the crown's coerced position, the clauses 10-11 extraction, and the closed protection set; reading it as pure snare erases the genuine fiscal-judicial coordination the settlement delivered — scheduled reliefs, proportional amercement, and consent-before-levy solved a real collective-action problem between crown and vassals that both parties had lived without since 1204. The R5 genealogy interview locates the mandate: the founding problem (royal fiscal predation on concentrated vassal wealth after the Norman loss) was live across the entire interval — hence three reissues — so no mandate-outlived-function condition arises yet; the arrangement transformed by renegotiated reissue rather than atrophying into performance. The founding_problem_status x disappearance_verdict pair (live x world_rearranges) is internally consistent and raises no zombie flag; the theater_ratio series stays well under the 0.5 substitution threshold throughout, confirming the arrangement never decayed into ceremonial maintenance within this window.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liber_homo_extension_omega,
    'This story is the baronial_privilege_reading of kernel magna_carta_1215: does ''liber homo'' in clauses 39-40, 52, and 60 denote the contracting tenants-in-chief (earls, barons, greater clergy) or all free subjects of the realm?',
    'Philological comparison of charter language against contemporary plea rolls, the 1217 reissue''s explicit class distinctions, and who actually invoked the clauses in period litigation; the sibling readings (universal_rights_reading, living_document_reading) instantiate the alternative extensions as separate constraint files.',
    'If the extension is broad, this reading''s victim set contracts and the arrangement approaches general coordination; if narrower still, extraction concentrates further on the crown and the excluded boundary hardens. The sibling files carry the alternative classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liber_homo_extension_omega, conceptual, 'Kernel-reading scope: the extension of the protected class determines the victim/beneficiary sets and therefore epsilon for this reading.').

omega_variable(
    clause61_operational_status,
    'Was the Clause 61 security apparatus — twenty-five elected barons empowered to distrain the crown — ever operationally exercised, or did enforcement run entirely through military occupation and negotiation?',
    'Close Roll and chronicle evidence for any convening, distraint, or castle-seizure executed under Clause 61 authority between June and August 1215.',
    'An operational committee makes the arrangement a functioning contractual enforcement regime; a dead letter makes armed coercion the sole enforcement, raising computed suppression and pushing the computed type toward the snare side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clause61_operational_status, empirical, 'Whether the charter''s contractual enforcement machinery ever functioned as designed.').

omega_variable(
    exclusion_distributive_effect,
    'Did capping crown exaction on the baronial class redistribute predation downward onto unfree tenants and towns (higher tallages, amercements, rents across 1215-1225), or did total extraction simply fall?',
    'Manorial account rolls and Pipe Roll comparisons across the settlement decade; sparse survival of manorial accounts for this exact window limits resolution.',
    'If redistribution occurred, the excluded classes belong in the victim structure and epsilon rises; if not, exclusion is a scope boundary rather than a transfer mechanism and the current victim set stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_distributive_effect, empirical, 'Whether elite-captured protection intensified extraction below the protection line.').

omega_variable(
    duress_binding_force,
    'Can a settlement sealed at swordpoint, annulled by the suzerain within ten weeks, and reimposed by conquest constitute a binding constraint at all — or is its force purely the battlefield balance it froze?',
    'Track whether post-1217 compliance tracks the text''s terms or the regency''s military position; divergent trajectories indicate independent normative force.',
    'A pure-force reading collapses the coordination function and recomputes the arrangement as imposed tribute; durable normative force supports the covenant classification and keeps the tangled_rope computation stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_binding_force, conceptual, 'Whether the arrangement binds as law or only as frozen military advantage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.15).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1216, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1216, 0.17).
narrative_ontology:measurement_basis(magn_tr_t1216, observed).
narrative_ontology:measurement(magn_tr_t1217, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1217, 0.19).
narrative_ontology:measurement_basis(magn_tr_t1217, observed).
narrative_ontology:measurement(magn_tr_t1219, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1219, 0.2).
narrative_ontology:measurement_basis(magn_tr_t1219, observed).
narrative_ontology:measurement(magn_tr_t1221, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1221, 0.22).
narrative_ontology:measurement_basis(magn_tr_t1221, observed).
narrative_ontology:measurement(magn_tr_t1223, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1223, 0.23).
narrative_ontology:measurement_basis(magn_tr_t1223, observed).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1225, 0.25).
narrative_ontology:measurement_basis(magn_tr_t1225, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.55).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1216, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1216, 0.51).
narrative_ontology:measurement_basis(magn_be_t1216, observed).
narrative_ontology:measurement(magn_be_t1217, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1217, 0.48).
narrative_ontology:measurement_basis(magn_be_t1217, observed).
narrative_ontology:measurement(magn_be_t1219, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1219, 0.47).
narrative_ontology:measurement_basis(magn_be_t1219, observed).
narrative_ontology:measurement(magn_be_t1221, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1221, 0.46).
narrative_ontology:measurement_basis(magn_be_t1221, observed).
narrative_ontology:measurement(magn_be_t1223, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1223, 0.45).
narrative_ontology:measurement_basis(magn_be_t1223, observed).
narrative_ontology:measurement(magn_be_t1225, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1225, 0.45).
narrative_ontology:measurement_basis(magn_be_t1225, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.85).
narrative_ontology:measurement_basis(magn_su_t1215, observed).
narrative_ontology:measurement(magn_su_t1216, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1216, 0.92).
narrative_ontology:measurement_basis(magn_su_t1216, observed).
narrative_ontology:measurement(magn_su_t1217, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1217, 0.78).
narrative_ontology:measurement_basis(magn_su_t1217, observed).
narrative_ontology:measurement(magn_su_t1219, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1219, 0.72).
narrative_ontology:measurement_basis(magn_su_t1219, observed).
narrative_ontology:measurement(magn_su_t1221, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1221, 0.68).
narrative_ontology:measurement_basis(magn_su_t1221, observed).
narrative_ontology:measurement(magn_su_t1223, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1223, 0.62).
narrative_ontology:measurement_basis(magn_su_t1223, observed).
narrative_ontology:measurement(magn_su_t1225, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1225, 0.55).
narrative_ontology:measurement_basis(magn_su_t1225, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, resource_allocation).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, living_document_reading).

% DUAL FORMULATION NOTE:
% Constraint family for kernel magna_carta_1215, decomposed per the epsilon-invariance principle: the colloquial label 'Magna Carta' covers three structurally distinct claims that differ in the extension of the protected class. This file (baronial_privilege_reading) authors epsilon for the narrow-contract arrangement: victims are the crown and Jewish creditors, beneficiaries the baronage and church. universal_rights_reading authors epsilon for the same text read as universal due process emission — different victim set, different epsilon, different classification. living_document_reading authors epsilon for the adaptive-substrate arrangement. The upstream/downstream structure runs from this reading to living_document_reading (the narrow historical account is the baseline from which precedential accumulation is measured), while this reading and universal_rights_reading are mutually exclusive within any single legal framework. Each family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
