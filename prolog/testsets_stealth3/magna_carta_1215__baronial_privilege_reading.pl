% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Magna Carta 1215 as Sealed Baronial Covenant (Baronial Privilege Reading)
 *   domain: constitutional/legal_history/political_theory
 *
 * SUMMARY:
 *   This story instantiates the baronial-privilege reading of the 1215
 *   Runnymede settlement: Magna Carta as a sealed feudal covenant between the
 *   Angevin crown and its tenants-in-chief, in which 'free men' (liber homo)
 *   denotes the landowning free-tenancy class and the protection set closes
 *   at the contracting parties. The constraint's operating geometry is the
 *   king-baron axis: the crown yields unilateral fiscal and judicial
 *   prerogative toward free tenants; the baronial estate acquires consent
 *   machinery, peer-administered fines, restitution procedures, and a
 *   twenty-five-man enforcement committee with distraint authority. Roughly
 *   half the realm — villeins, bordars, cottars, slaves — lies wholly outside
 *   the protection set, and no woman sits in the coalition or the committee;
 *   security for the few is financed by the unchanged condition of the many.
 *   The epsilon referent is this standing arrangement — the charter as sealed
 *   in 1215 and reissued through 1225 — assessed by this reading's own
 *   lights.
 *
 * KEY AGENTS:
 *   - - landowning_barons: Agenda-setting estate (organized/constrained) — drafts the Articles, enforces via the clause 61 committee, collects the settlement's principal securities
 *   - - the_crown: Primary payer seat (institutional/trapped) — surrenders prerogative under duress; bears enforcement costs as civil war, invasion, and reissue concessions
 *   - - knightly_freeholders: Secondary beneficiary (moderate/constrained) — covered derivatively by the liber homo umbrella without seats in drafting
 *   - - english_church_estate: Beneficiary with external lever (institutional/arbitrage) — clause 1 freedoms defended through Rome rather than arms
 *   - - unfree_rural_tenants: Payer outside the protection set (powerless/trapped) — demographic majority excluded from clause 39; costs possibly baseline rather than attributable (see omega excluded_majority_cost_attribution)
 *   - - women_outside_contracting_class: Excluded voice (powerless/trapped) — derivative protections only; no seat in negotiation or enforcement
 *   - - papacy_curia_romana: Agenda-setter over textual validity (institutional/arbitrage) — annuls the first charter, legitimates the reissues
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.48).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.58).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215 as Sealed Baronial Covenant (Baronial Privilege Reading)").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '4c560664-03fd-43ed-8672-6e0c955f0771').
narrative_ontology:cs_kernel_codification('4c560664-03fd-43ed-8672-6e0c955f0771', fixed_text).
narrative_ontology:cs_authority_grounding('4c560664-03fd-43ed-8672-6e0c955f0771', extraction).
narrative_ontology:cs_interpretation_layer_present('4c560664-03fd-43ed-8672-6e0c955f0771').
narrative_ontology:cs_reading_relation('4c560664-03fd-43ed-8672-6e0c955f0771', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('4c560664-03fd-43ed-8672-6e0c955f0771', magna_carta_1215__living_document_reading, forecloses).
narrative_ontology:cs_axiom('4c560664-03fd-43ed-8672-6e0c955f0771', foundational, protection_set_closed_to_contracting_parties).
narrative_ontology:cs_axiom_status(protection_set_closed_to_contracting_parties, holdable).
narrative_ontology:cs_axiom_grounding('4c560664-03fd-43ed-8672-6e0c955f0771', protection_set_closed_to_contracting_parties, conventional).
narrative_ontology:cs_axiom('4c560664-03fd-43ed-8672-6e0c955f0771', foundational, liber_homo_denotes_landowning_freeholder).
narrative_ontology:cs_axiom_status(liber_homo_denotes_landowning_freeholder, holdable).
narrative_ontology:cs_axiom_grounding('4c560664-03fd-43ed-8672-6e0c955f0771', liber_homo_denotes_landowning_freeholder, empirically_contingent).
narrative_ontology:cs_axiom('4c560664-03fd-43ed-8672-6e0c955f0771', secondary, protection_tracks_enforceable_leverage).
narrative_ontology:cs_axiom_status(protection_tracks_enforceable_leverage, holdable).
narrative_ontology:cs_axiom_grounding('4c560664-03fd-43ed-8672-6e0c955f0771', protection_tracks_enforceable_leverage, instrumental).
narrative_ontology:cs_axiom('4c560664-03fd-43ed-8672-6e0c955f0771', secondary, distraint_committee_self_enforcement).
narrative_ontology:cs_axiom_status(distraint_committee_self_enforcement, overridden).
narrative_ontology:cs_axiom_grounding('4c560664-03fd-43ed-8672-6e0c955f0771', distraint_committee_self_enforcement, conventional).
narrative_ontology:cs_reference_frame('4c560664-03fd-43ed-8672-6e0c955f0771', runnymede_sealed_covenant).
narrative_ontology:cs_drift_state('4c560664-03fd-43ed-8672-6e0c955f0771', post_annulment_reissue_regime, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4c560664-03fd-43ed-8672-6e0c955f0771', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, knightly_freeholders).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, english_church_estate).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, the_crown).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, unfree_rural_tenants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tenants-in-chief who assembled in arms through spring 1215, seized London in May, and dictated the Articles of the Barons at Runnymede. They received taxation consent-gates (clauses 12/14), peer-administered fines (clause 21), restitution of disseised lands (52/53), and clause 61: a twenty-five-man committee drawn from their own number, empowered to distrain the king's castles and lands on breach. Selling English holdings and withdrawing across the Channel was nominally open but economically ruinous and politically indistinguishable from treason; their standing was constituted by the tenure the covenant secures.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary).

% The monarchy as governing institution. It conceded under duress: surrendered unilateral scutage and aids, arbitrary disseisin, and discretionary imprisonment or exile toward free tenants, and submitted to a private committee with power to seize royal castles. Within ten weeks it procured papal annulment; the escape attempt produced civil war, a French invasion, and a minority regency that bought peace by reissuing the charter twice. There is no institutional exit from the settlement short of reconquering the baronial coalition, which the crown twice attempted and twice failed to afford.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, the_crown, payer,
    institutional, generational, trapped, national).

% Sub-vassal knights and humbler free tenants covered derivatively by the liber homo umbrella: capped reliefs, protection against arbitrary disseisin, access to the due-process and fixed-price justice clauses, and wardship reforms ending the sale of heirs' marriages to enemies. They held no seats in drafting and appear in the record only through baronial spokesmen; their service obligations were regularized rather than lifted.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, knightly_freeholders, beneficiary,
    moderate, biographical, constrained, national).

% Clause 1 grants the English Church freedom and free canonical elections, remedies for a decade of interdict-era predation. Archbishop Langton brokered the settlement and supplied its ideological framing from coronation-oath tradition. The estate uniquely holds an external enforcement lever unavailable to lay parties — interdict and excommunication through Rome — allowing it to defend its gains without fielding troops, and to exit adverse royal policy altogether by appealing over the king's head.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, english_church_estate, beneficiary,
    institutional, generational, arbitrage, national).

% Villeins, bordars, cottars, and slaves — roughly half the population. The charter's protection clauses do not reach them: clause 39's beneficiary class is the free tenant, and clause 20 prices a villein's chattels only as an input to fine calculation. Labor services, tallage at will, and seigneurial jurisdiction continue at their lords' discretion, and clause 60 propagates the new disciplinary norms down the tenurial chain, giving mesne lords the same procedural security over their own men that the barons extracted from the king. Their status is hereditary and legally inescapable.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, unfree_rural_tenants, payer,
    powerless, generational, trapped, national).

% No woman took part in the negotiation, the sealing, or the enforcement committee. Protections arrive only derivatively through male kin-status: widows of free tenants receive their marriage portions and cannot be forced into remarriage (clauses 7-8), and ward-heirs gain limits on the sale of their marriages. Women as independent holders of tenure stand outside the protected class, and heiresses' marriages remained instruments of wardship politics throughout the interval.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, women_outside_contracting_class, excluded,
    powerless, biographical, trapped, national).

% Innocent III quashed the charter within weeks of sealing as a vassal's violation of his liege lord (John had commuted England to a papal fief) and threatened crusading sanctions; the regency later secured papal legitimation for the 1216 and 1217 reissues when reconciliation demanded it. The curia thus decides which textual versions of the covenant carry spiritual validity — agenda power over what the sealed text IS — without administering day-to-day enforcement, and it observes the settlement's working from a transnational seat above every local party.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, papacy_curia_romana, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, papacy_curia_romana, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:fixing_cost_class(magna_carta_1215__baronial_privilege_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the crown-magnate credible-commitment problem: converting ad hoc royal exactions into bargained finance — scutage and aids only by common counsel (12/14), standardized reliefs, regulated wardship — and substituting procedural dispute-resolution (39/40) for confiscatory punishment, so military-financial service could be supplied without fear of arbitrary reprisal. Clause 60 propagates the same discipline down the free tenurial chain.
% TRANSFER_FUNCTION: Moves discretionary prerogative and revenue-setting authority from the crown to the baronial estate (consent gates, the clause 61 distraint committee, peer-only fines); moves procedural security in life, land, and access to justice to the free-tenancy class; and finances both by leaving the unfree majority's obligations and legal status untouched.
% ABSENT_VOICES: Unfree tenants — villeins, bordars, cottars, slaves, roughly half the realm — would object to total omission from the protection set; no villein voice existed at Runnymede or on the committee, and none appears in the surviving record. Women appear only derivatively (widow clauses 7-8) and sat in no negotiating or enforcement seat. Chartered towns obtained clause 13 and 41 terms but had no independent voice in drafting. The excluded are absent by legal incapacity, not by oversight.
% DISAPPEARANCE_RATIONALE: A mid-interval vanishing (say 1217) strips the regency of its reconciliation instrument: the taxation-consent machinery collapses, reviving the unilateral-exaction dynamic that produced the war; the due-process forms that seed common-law procedure never consolidate; crown-baron dealings revert to armed bargaining. The unfree majority's condition is unchanged either way — their exclusion predates and survives the charter.
% FOUNDING_PROBLEM: King John's post-Bouvines extortion: escalated scutage (1202-1214), punitive disseisin of defeated barons' lands, reliefs and wardships marketed for profit, justice weaponized against refractory tenants — the credibility collapse of royal government toward the military-financial elite that supplied its wars.
% FOUNDING_PROBLEM_CORROBORATION: Papal curia registers and comparatively disinterested monastic chronicles (Barnwell, Waverley) — sources outside the baronial coalition — attest both the exactions that produced the settlement and the problem's persistence past 1217: the regency reissued precisely because the underlying conflict stayed live. No corroborating voice exists for the excluded majority; no villein or female testimony survives anywhere in the record, and that structural silence is itself signal (logged under absent_voices).
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim and metrics are authored independently. The claimed type is tangled_rope from structure alone: a real coordination function (credible commitment converting extortion into bargained finance — participants were net beneficiaries, which blocks a snare reading) fused with asymmetric extraction (a closed protection set, a coerced crown, and an enforcement committee constituted as private oligarchic power — which blocks a rope reading), actively enforced (clause 61; armed coalition; two wars), which blocks mountain and piton. Metrics describe operation: epsilon declines from 0.66 to 0.48 as the enforcement committee is stripped from the reissues and coercion migrates to judicial-parliamentary channels, but stays substantial because the settlement's gains remain concentrated and its exclusions intact. Suppression is authored as a RAW structural property — it is not scaled by power or scope in the engine's computation; only extractiveness is scaled. Its series is authored because enforcement capacity changes qualitatively across the interval (armed distraint, wartime maximum 0.78, then judicialization), which is exactly the enforcement-infrastructure dynamic the scalar cannot carry. Theater peaks at 0.38 in 1217 — the annulled first text plus an aspirational wartime reissue — then falls as the reissued provisions become administratively real; the slight 1225 uptick marks the beginning of ceremonial confirmation culture. Accessibility collapse is low-moderate (0.42): the annulment path, the deposition-and-invite-Louis route, and renegotiation all stayed live alternatives. Resistance is high (0.72): papal quashing, mercenary recruitment, and civil war. Fixing_cost is prohibitive on the record itself: the crown's one serious removal attempt (annulment plus mercenaries) produced civil war and foreign invasion costing far more than bearing the arrangement; the barons could have abandoned it cheaply, but no fixer for whom removal was costly lacked the option to simply defect instead. Measurement series run on one shared grid (1215/1217/1219/1221/1223/1225) with every tracked metric authored at every point; the trajectory is decaying-with-one-wartime-spike rather than cyclical.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the baronial seat the arrangement is hard-won security: coordination dominant, extraction experienced as the price already paid in blood at Runnymede. From the crown seat the same structure operates as coerced surrender — a private committee empowered to seize royal castles reads, from that chair, as institutionalized exaction with a coordination fig leaf. From the knightly seat it is derivative protection: mostly benefit, little say. From the excluded seats (unfree tenants, women outside the class) there is no protection surface at all — the arrangement registers chiefly as the consolidation of the order that holds them in place, with whatever cost-attribution omega excluded_majority_cost_attribution assigns. The papal seat experiences it as a legitimacy instrument whose value lies in version-selection power. The engine computes these divergences from the structural data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (barons, knightly freeholders, church estate) derive low directionality — subsidy-side seats; the church additionally carries an arbitrage-grade exit that pushes it toward the beneficiary extreme. Declared payers derive high directionality: the crown sits near the full-target end (trapped, no institutional exit, identity constituted by the throne it occupies), unfree rural tenants likewise trapped with generational horizon. Two authoring decisions worth recording. First, no directionality overrides are authored although the crown enjoys second-order stabilization gains (the reissues bought it peace and legitimacy): overrides key on the power atom rather than the agent, so an upward correction aimed at the institutional crown seat would contaminate the institutional church seat, which is a clean beneficiary; the structural derivation is left to handle the mixed signal. Second, unfree tenants' cost-bearing is declared rather than suppressed even though their exposure plausibly predates the charter — the attributional uncertainty is routed to omega excluded_majority_cost_attribution instead of being laundered out of the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   Within the interval the founding problem is live and the arrangement demonstrably load-bearing, so no mandatrophy declaration is authored and the mismatch guard stays quiet: founding_problem_status=live crossed with disappearance_verdict=world_rearranges is the consistent cell, not a zombie flag. The tangled_rope classification is what prevents both mislabels: a pure-extraction reading would erase the genuine collective-action solution (and with it the reason the settlement survived four governments), while a pure-coordination reading would launder the closed protection set and the coerced crown. Theater is the metric to watch for Goodhart drift: if omega clause61_functionality_window resolves as never-operational, the authored theater series understates theatricality and the persistence story migrates from enforcement toward inertial maintenance — at which point a later-interval reauthoring should test the piton signature. The 1217 deletion of the enforcement committee from the reissues is logged as an overridden axiom inside this reading's own tradition, which is the honest genealogy of the drift_state entry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_frame,
    'This constraint is one reading of kernel magna_carta_1215 (baronial_privilege_reading): what changes structurally if a sibling reading is adopted instead?',
    'Comparative classification across the sibling story files: recompute beneficiary/victim populations and epsilon referents under universal_rights_reading (protection set opens to all persons) and living_document_reading (drift-indexed adaptive substrate).',
    'Adopting the universal sibling expands the party set from the king-baron axis to the whole polity and transforms the excluded-majority seats from bystanders into the central victim population; adopting the living-document sibling replaces fixed-text geometry with drift geometry, changing persistence analysis from enforcement-based to accumulative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Committer-frame routing: kernel membership and sibling deltas recorded here rather than in invented fields.').

omega_variable(
    liber_homo_extension_boundary,
    'Does clause 39''s liber homo denote only tenants-in-chief and their knightly vassals, or all free tenants however humble — socmen, free sokemen, urban freemen?',
    'Philological comparison of the charter''s clause terminology against thirteenth-century tenurial records (Red Book of the Exchequer, pipe rolls) and the earliest judicial applications of the due-process clauses.',
    'A wider extension shrinks the extraction asymmetry (more coordination, less tangle); a narrower extension concentrates privilege further and pushes the computed geometry toward snare flavors from the excluded seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liber_homo_extension_boundary, empirical, 'Boundary of the protection set within the baronial reading.').

omega_variable(
    clause61_functionality_window,
    'Did the clause 61 committee of twenty-five ever actually exercise its distraint authority, or was the enforcement machinery a dead letter from sealing?',
    'Administrative-record audit of the August-September 1215 window between Runnymede and the papal annulment: writs, summonses, or seizures traceable to the twenty-five named enforcers.',
    'If functional, enforcement-based extraction is real and the theater series understates nothing; if never operational, clause 61 is theatrical from inception, theater_ratio should be revised upward, and the charter''s persistence rests entirely on reissue politics rather than its own authored enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clause61_functionality_window, empirical, 'Functionality versus theatricality of the charter''s authored enforcement mechanism.').

omega_variable(
    excluded_majority_cost_attribution,
    'Do unfree rural tenants bear costs attributable to this arrangement, or is their exposure a pre-existing baseline the charter merely fails to remedy?',
    'Compare seigneurial exaction trajectories (labor services, tallage, entry fines) on estates held by charter-drafting barons against comparable non-coalition estates across 1215-1225; assess whether clause 60''s propagation of disciplinary norms measurably hardened mesne-lord practice.',
    'If costs are pre-existing baseline, unfree_rural_tenants drop from the victims declaration to bystander status, flattening that payer seat''s directionality and moving computed classification toward rope from the excluded seat; if charter-consolidated, the tangled_rope geometry is confirmed and strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(excluded_majority_cost_attribution, empirical, 'Attributional boundary for the excluded majority''s cost-bearing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1225).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.3).
narrative_ontology:measurement_basis(magn_tr_t1215, observed).
narrative_ontology:measurement(magn_tr_t1217, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1217, 0.38).
narrative_ontology:measurement_basis(magn_tr_t1217, observed).
narrative_ontology:measurement(magn_tr_t1219, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1219, 0.33).
narrative_ontology:measurement_basis(magn_tr_t1219, observed).
narrative_ontology:measurement(magn_tr_t1221, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1221, 0.28).
narrative_ontology:measurement_basis(magn_tr_t1221, observed).
narrative_ontology:measurement(magn_tr_t1223, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1223, 0.25).
narrative_ontology:measurement_basis(magn_tr_t1223, observed).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1225, 0.27).
narrative_ontology:measurement_basis(magn_tr_t1225, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.66).
narrative_ontology:measurement_basis(magn_be_t1215, observed).
narrative_ontology:measurement(magn_be_t1217, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1217, 0.6).
narrative_ontology:measurement_basis(magn_be_t1217, observed).
narrative_ontology:measurement(magn_be_t1219, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1219, 0.56).
narrative_ontology:measurement_basis(magn_be_t1219, observed).
narrative_ontology:measurement(magn_be_t1221, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1221, 0.53).
narrative_ontology:measurement_basis(magn_be_t1221, observed).
narrative_ontology:measurement(magn_be_t1223, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1223, 0.5).
narrative_ontology:measurement_basis(magn_be_t1223, observed).
narrative_ontology:measurement(magn_be_t1225, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1225, 0.48).
narrative_ontology:measurement_basis(magn_be_t1225, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.72).
narrative_ontology:measurement_basis(magn_su_t1215, observed).
narrative_ontology:measurement(magn_su_t1217, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1217, 0.78).
narrative_ontology:measurement_basis(magn_su_t1217, observed).
narrative_ontology:measurement(magn_su_t1219, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1219, 0.64).
narrative_ontology:measurement_basis(magn_su_t1219, observed).
narrative_ontology:measurement(magn_su_t1221, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1221, 0.57).
narrative_ontology:measurement_basis(magn_su_t1221, observed).
narrative_ontology:measurement(magn_su_t1223, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1223, 0.55).
narrative_ontology:measurement_basis(magn_su_t1223, observed).
narrative_ontology:measurement(magn_su_t1225, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1225, 0.58).
narrative_ontology:measurement_basis(magn_su_t1225, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, resource_allocation).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__living_document_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, charter_of_the_forest_1217).

% DUAL FORMULATION NOTE:
% The colloquial label 'Magna Carta' decomposes under epsilon-invariance into three structurally distinct readings sharing one kernel: baronial privilege (this file — closed protection set, king-baron extraction geometry), universal rights (universal protection set; different victim/beneficiary population and different epsilon), and living document (adaptive substrate; drift-indexed rather than extension-indexed). Family links run through network.affects_constraints. The baronial reading is the historically-grounded base case from which the universal reading generalizes and against which the living reading measures accumulation; charter_of_the_forest_1217 is a downstream structural descendant (the 1217 reissue split forest jurisdiction into its own charter).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
