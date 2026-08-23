% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoousios_reading, []).

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
 *   constraint_id: nicene_christological_kernel__homoousios_reading
 *   human_readable: Nicene Homoousios Enforcement Arrangement (Consubstantiality Reading)
 *   domain: historical theology/ecclesiastical authority
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Nicene Christological kernel:
 *   the homoousios (same-substance) definition of the Son's relation to the
 *   Father, together with the enforcement arrangement that carried it —
 *   conciliar definition, imperial legislation, anathema, exile, and property
 *   transfer. The reading solved a real coordination problem (how a
 *   monotheist church worships a divine Christ, with one baptismal confession
 *   and one communion boundary across a continental body) and simultaneously
 *   extracted heavily from those who taught or lived the rival reading:
 *   Homoian Gothic congregations lost legal standing and meeting houses,
 *   regional North African networks had sees and property reassigned, and
 *   bishops of equal rank were deposed and exiled when the formula turned
 *   against them. KEY AGENTS (by structural relationship): -
 *   nicene_episcopal_hierarchy: Agenda-setter and principal collector
 *   (institutional/constrained) — drafts the definition, administers the
 *   anathema, receives disputed sees - roman_imperial_state_apparatus:
 *   Instrumental beneficiary and co-setter (institutional/arbitrage) —
 *   legislates and enforces orthodoxy, flips between readings when preference
 *   changes - homoian_gothic_communities: Target (organized/identity_locked)
 *   — vernacular-bible confessional identity fused with ethnicity, assembly
 *   rights stripped - north_african_regional_congregations: Target
 *   (moderate/trapped) — regional autonomy absorbed, sees reassigned -
 *   deposed_homoian_episcopate: Same-rank targets (institutional/trapped) —
 *   office attached to subscription - urban_lay_communicants and
 *   egyptian_ascetic_movement: Beneficiaries (constrained/mobile) -
 *   platonist_monotheist_critics: Excluded voice (powerful/arbitrage) —
 *   metaphysical critique with no conciliar franchise -
 *   modern_patristic_historians: Analytical observer — attests the mixed
 *   genealogy from outside the beneficiary set
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, 0.78).
domain_priors:suppression_score(nicene_christological_kernel__homoousios_reading, 0.86).
domain_priors:theater_ratio(nicene_christological_kernel__homoousios_reading, 0.19).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, theater_ratio, 0.19).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoousios_reading, tangled_rope).
narrative_ontology:human_readable(nicene_christological_kernel__homoousios_reading, "Nicene Homoousios Enforcement Arrangement (Consubstantiality Reading)").
narrative_ontology:topic_domain(nicene_christological_kernel__homoousios_reading, "historical theology/ecclesiastical authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoousios_reading, '76e5cb75-bf1a-4442-a93a-19cd14fb7d13').
narrative_ontology:cs_kernel_codification('76e5cb75-bf1a-4442-a93a-19cd14fb7d13', fixed_text).
narrative_ontology:cs_authority_grounding('76e5cb75-bf1a-4442-a93a-19cd14fb7d13', extraction).
narrative_ontology:cs_interpretation_layer_present('76e5cb75-bf1a-4442-a93a-19cd14fb7d13').
narrative_ontology:cs_reading_relation('76e5cb75-bf1a-4442-a93a-19cd14fb7d13', nicene_christological_kernel__homoiousios_reading, forecloses).
narrative_ontology:cs_axiom('76e5cb75-bf1a-4442-a93a-19cd14fb7d13', foundational, son_is_homoousios_with_father).
narrative_ontology:cs_axiom_status(son_is_homoousios_with_father, holdable).
narrative_ontology:cs_axiom_grounding('76e5cb75-bf1a-4442-a93a-19cd14fb7d13', son_is_homoousios_with_father, theological).
narrative_ontology:cs_axiom('76e5cb75-bf1a-4442-a93a-19cd14fb7d13', secondary, worship_of_christ_requires_full_deity).
narrative_ontology:cs_axiom_status(worship_of_christ_requires_full_deity, holdable).
narrative_ontology:cs_axiom_grounding('76e5cb75-bf1a-4442-a93a-19cd14fb7d13', worship_of_christ_requires_full_deity, theological).
narrative_ontology:cs_reference_frame('76e5cb75-bf1a-4442-a93a-19cd14fb7d13', conciliar_consubstantiality_settlement).
narrative_ontology:cs_drift_state('76e5cb75-bf1a-4442-a93a-19cd14fb7d13', post_constantinopolitan_settlement, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('76e5cb75-bf1a-4442-a93a-19cd14fb7d13', '').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, nicene_episcopal_hierarchy).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, roman_imperial_state_apparatus).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, egyptian_ascetic_movement).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, urban_lay_communicants).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, homoian_gothic_communities).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, north_african_regional_congregations).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, deposed_homoian_episcopate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, urban_lay_communicants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and ratifies the consubstantiality definition at general councils, fixes the ordination and communion boundaries around it, and administers the anathema that closes the creed. Backed after 380 by imperial law, it receives disputed sees, transfers congregational property to compliant clergy, and expels bishops who refuse subscription (thirty-six expelled at Constantinople in 381). Its own standing depends on the arrangement persisting: a bishop who repudiates the formula faces deposition and exile.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, nicene_episcopal_hierarchy, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, nicene_episcopal_hierarchy, beneficiary).

% Convenes councils, legislates orthodoxy (Edict of Thessalonica, Cunctos populos), and supplies magistrates, exile orders, and confiscation enforcement. Gains a single religious anchor for administration and a loyalty test usable across the army and civil service. Its commitment to this particular formula is instrumental: under Constantius and Valens the same apparatus enforced rival formulas, and it inherits or hands off the enforcement machinery whichever side prevails.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, roman_imperial_state_apparatus, beneficiary,
    institutional, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, roman_imperial_state_apparatus, agenda_setter).

% Descendants of Ulfilas' mission, worshipping from a Gothic vernacular scripture under the Homoian creed. Inside imperial territory after 380 their assemblies lose legal standing, their meeting houses pass to compliant clergy, and their clergy are banished. Their confessional identity is fused with Gothic ethnic identity through the translated bible, so abandoning the formula reads as cultural erasure rather than mere correction; military organization gives them leverage at the frontier but nothing inside the enforcement zone.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, homoian_gothic_communities, payer,
    organized, generational, identity_locked, continental).

% Regional congregations with vernacular liturgical habits and locally autonomous episcopal networks, some aligned with Homoian teaching under Vandal patronage. As imperial-ecclesiastical authority extends back over Africa, their sees are reassigned to subscription-compliant bishops, their property follows the sees, and their regional autonomy is absorbed into the centralized structure; geographic position leaves no jurisdiction to relocate to.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, north_african_regional_congregations, payer,
    moderate, generational, trapped, continental).

% Bishops of equal canonical rank to the winners, deposed and exiled when their formula loses: Eusebius of Nicomedia's exile after Nicaea is the pattern, repeated at scale in the 360s and again in 381. Office, residence, and income attach to subscription; refusal costs the see itself. Their institutional power is real but unusable against the enforcement coalition once imperial preference turns.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, deposed_homoian_episcopate, payer,
    institutional, biographical, trapped, continental).

% Receive a single baptismal formula, unified liturgy, and clear catechetical teaching across the cities; communion disputes stop being a local guessing game. They pay indirectly through taxation funding enforcement, through riots and disruptions when their city's congregational allegiance conflicts with imperial preference, and through the loss of teachers they trusted.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, urban_lay_communicants, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nicene_christological_kernel__homoousios_reading, urban_lay_communicants, payer).

% Desert monastic networks allied with the Alexandrine defense of the formula; they supply mass legitimacy, shelter exiled clergy, and lend charismatic weight to the winning side. In return they gain official protection, patronage, and immunity from the suspicion that falls on heterodox movements.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, egyptian_ascetic_movement, beneficiary,
    moderate, generational, mobile, regional).

% Philosophically trained monotheists who regarded the raw substance vocabulary as metaphysically confused and would argue for careful terminological reformulation rather than anathema. They hold no conciliar franchise, publish outside the enforcement zone, and can retreat into private academies and syncretic circles; their critique circulates precisely because they are outside the arrangement's reach.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, platonist_monotheist_critics, excluded,
    powerful, generational, arbitrage, continental).

% Reconstruct the fourth-century settlement from acta, letters, legal codes, and sermons from a seat outside any confession's enforcement. They see the whole structure — the doctrinal achievement, the enforcement record, the flipping of imperial preference between readings — and attest to the mixed genealogy from outside the benefiting parties.
narrative_ontology:constraint_stakeholder(nicene_christological_kernel__homoousios_reading, modern_patristic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_christological_kernel__homoousios_reading, nicene_episcopal_hierarchy).
narrative_ontology:fixing_cost_class(nicene_christological_kernel__homoousios_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a problem no community could solve alone: how the church could worship Christ as divine while professing one God. The formula gave every congregation one baptismal confession, one test of communion, one boundary of acceptable teaching, and gave the empire one religious anchor for law and appointment.
% TRANSFER_FUNCTION: Moves doctrinal legitimacy, episcopal office, church buildings, and communal standing toward those professing the consubstantiality formula; moves conformity, property, offices, and physical security away from dissenting teachers and congregations, with exile and confiscation as the collection instruments.
% ABSENT_VOICES: Homoian bishops were unseated before the enforcing councils finished their work — thirty-six were expelled at Constantinople in 381 rather than heard. Gothic presbyters were never seated anywhere. Platonist metaphysical critics had no franchise in the deliberations that fixed the substance vocabulary. Jewish communities affected by the same uniformity legislation had no voice in its drafting.
% DISAPPEARANCE_RATIONALE: Overnight removal would reopen the succession wars: subordinationist teaching would reclaim sees, the empire would lose its religious anchor for law and appointment, and the baptismal and liturgical unity of the Mediterranean church would fragment along the pre-Nicene lines. Congregational allegiance, property titles, and clerical careers all hang on the formula.
% FOUNDING_PROBLEM: Fourth-century Christians worshipped Christ while confessing one God; Arius' presbyters taught the Son was a creature, no shared formula marked the boundary of acceptable teaching, communion fractured city by city, and the emperor had no settled church to anchor policy after the earlier schisms.
% FOUNDING_PROBLEM_CORROBORATION: Non-confessional late-antique historiography — university patristics and scholarship on the Constantinian and Theodosian settlements — corroborates a mixed genealogy: genuine doctrinal necessity alongside imperial unity-seeking and episcopal rivalry. The claim that the enforcement form (anathema, exile, confiscation) was necessary rather than chosen is attested almost exclusively by the tradition's own documents; no corroborating source outside the beneficiary set attests it.
narrative_ontology:disappearance_verdict(nicene_christological_kernel__homoousios_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_christological_kernel__homoousios_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_christological_kernel__homoousios_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_christological_kernel__homoousios_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_christological_kernel__homoousios_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoousios_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nicene_christological_kernel__homoousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the arrangement's collection instruments — deposition, exile, confiscation of meeting houses and sees — operated at full strength after 380 and the gains accrued to identifiable seats. Suppression is higher still (0.86) because persistence depended on actively closing exits: rival assemblies lost legal standing, and the formula was imposed by law rather than adopted by consensus. Theater ratio is low-to-moderate (0.19): during the contest era, confession was a costly, functional signal; the modest theater bump in the middle of the interval reflects the formula-signing culture of the 350s, where clergy recited whatever text the prevailing court demanded — compliance as performance. Accessibility collapse is 0.62: alternatives collapsed almost completely inside the empire after the Theodosian laws, but survived beyond the frontier in the Gothic kingdoms, so the collapse is broad but not total. Resistance is 0.70: this arrangement met decades of open, organized contest — riots, rival councils, armed political backing for the rival reading — before winning. All three temporal metrics run on one shared nine-point grid (t=0..66 years after Nicaea); no metric skips a row. The base_extractiveness series is deliberately NOT monotonic: it dips to 0.27 at t=32 (the Sirmium era) when imperial preference swung to the rival reading and THIS arrangement's enforcement went quiet, then surges after 380. The suppression_requirement series rises monotonically through the same trough — the enforcement machinery (courts, exile orders, confiscation procedure) was built up continuously and was reading-agnostic: Constantius built it against Nicene holders, Theodosius inherited it for this reading. Suppression is authored as an unscaled structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats should classify differently. From the episcopal agenda-setter seat, the arrangement appears as fidelity vindicated: the formula preserved worship-coherence against subordinationism, and enforcement defended a settled truth against destabilizing revision. From the Homoian and North African payer seats, the same structure is experienced as dispossession — assemblies closed, sees reassigned, teachers exiled — with the doctrinal question never fairly adjudicated because their seats were removed before the enforcing councils concluded. The imperial seat experiences neither: it holds the machinery instrumentally and evaluates the arrangement by administrative yield. The observer seat sees the coordination achievement and the extraction record simultaneously. The engine computes these divergences from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   The episcopal hierarchy sits nearest the beneficiary pole (collects offices, property, and definitional authority) but is not frictionless — its standing is hostage to the arrangement it runs, which pulls it slightly off the floor. The imperial apparatus derives near-beneficiary from its declared role but carries real enforcement costs and holds arbitrage-grade exit (it demonstrably switched readings), so its effective extraction is materially lower than the hierarchy's. Payers sit near the target pole: the Gothic communities combine victim declaration with identity-locked exit (the vernacular bible fuses confession to ethnicity), which pushes them further toward full-target than their military organization alone would suggest; the North African congregations and the deposed bishops are trapped, with no jurisdiction to relocate into. Lay communicants and the ascetic movement derive low d from their beneficiary declarations, with the laity's secondary payer role damping it toward symmetric. No directionality overrides are used: the derivation chain reads role and exit options directly, and the two institutional seats differ on exactly those axes (agenda_setter/constrained vs beneficiary/arbitrage), so a power-atom-keyed override would misfire on both.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters most where a story tempts both errors. Reading this arrangement as a pure snare erases the genuine coordination function: the formula really did solve the worship-coherence problem, really did unify baptism and communion across a continental body, and endured for sixteen centuries because it answers something — a pure extraction scheme with no coordination content decays when enforcement lapses, and this one did not. Reading it as a pure rope erases the asymmetric extraction: the same structure that coordinated also stripped legal standing from Gothic assemblies, reassigned African sees, and exiled same-rank bishops, and it required continuous active enforcement to hold. Tangled rope keeps both faces visible, which is what lets the corpus ask the live question — how much of the arrangement's persistence is owed to the problem it solves, and how much to the machinery it built. The mandate is not dead: the founding problem's status is contested, not resolved, so no mandatrophy resolution is declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (homoousios) of the nicene_christological_kernel; what structural facts of the arrangement change if the sibling reading (homoiousios) had held the enforcement seat?',
    'Compare this story with the sibling homoiousios_reading story: the enforcement machinery, the imperial arbitrage behavior, and the coordination function are shared; the beneficiary/victim sets and the anathematized populations invert with the reading in power.',
    'If the machinery and coordination function are reading-invariant while only the victim sets rotate, then the extraction measured here is partly attributable to the enforcement FORM rather than this reading''s content, and cross-reading comparisons become the right analytic unit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which kernel, which reading, and what the sibling reading would change structurally.').

omega_variable(
    ousia_semantic_indeterminacy,
    'Did ''ousia'' (substance/essence) in the fourth-century usage pick out a determinate metaphysical item, or was the formula''s content unstable enough that enforcement outran definition?',
    'Semantic reconstruction of fourth-century philosophical usage and the Cappadocian settlement distinguishing ousia from hypostasis; trace whether the 381 expansion functioned as clarification of a stable meaning or as retro-fitted determinacy onto an ambiguous term.',
    'If indeterminate, part of the arrangement''s extractiveness was spent enforcing a moving target — supporting a larger theater component and weakening the claim that the anathema protected a crisply defined truth; if determinate, enforcement defended a stable content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ousia_semantic_indeterminacy, conceptual, 'Whether the kernel term carried stable content or drifted under enforcement.').

omega_variable(
    coordination_extraction_separability,
    'Is the uniformity function of the creed separable from its suppression machinery — could a binding consubstantiality confession have coordinated the church without anathema, exile, and confiscation?',
    'Counterfactual comparison with coordination achieved by non-coercive credal standards: pre-Constantinian regula fidei practice, and later ecumenical agreements that align confessions without enforcement powers.',
    'If separable, the excess extraction above the coordination floor is enforcement rent attributable to the arrangement''s choice of instruments; if inseparable in the fourth-century environment, part of the measured burden is the price of achieving the coordination at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the coordination and extraction components are structurally separable.').

omega_variable(
    enforcement_machinery_attribution,
    'How much of the measured suppression belongs to THIS reading versus the reading-agnostic enforcement apparatus it inherited (built under Constantius against Nicene holders, reused by Theodosius for them)?',
    'Compare suppression trajectories of both sibling readings across the interval: if each reading''s suppression series rises on the same curve regardless of which reading holds power, the machinery is the invariant and the readings are interchangeable operators of it.',
    'Attribution changes the epsilon decomposition: a large reading-invariant component shifts explanatory weight from doctrinal content to imperial-enforcement form, and predicts that either reading in power produces a similar extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_machinery_attribution, empirical, 'Attribution of suppression between this reading''s content and the shared enforcement machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoousios_reading, 0, 66).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nck_homoousios_tr_t0, nicene_christological_kernel__homoousios_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nck_homoousios_tr_t8, nicene_christological_kernel__homoousios_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(nck_homoousios_tr_t16, nicene_christological_kernel__homoousios_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(nck_homoousios_tr_t24, nicene_christological_kernel__homoousios_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement(nck_homoousios_tr_t32, nicene_christological_kernel__homoousios_reading, theater_ratio, 32, 0.34).
narrative_ontology:measurement(nck_homoousios_tr_t40, nicene_christological_kernel__homoousios_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(nck_homoousios_tr_t48, nicene_christological_kernel__homoousios_reading, theater_ratio, 48, 0.22).
narrative_ontology:measurement(nck_homoousios_tr_t56, nicene_christological_kernel__homoousios_reading, theater_ratio, 56, 0.17).
narrative_ontology:measurement(nck_homoousios_tr_t66, nicene_christological_kernel__homoousios_reading, theater_ratio, 66, 0.19).

% Extraction over time
narrative_ontology:measurement(nck_homoousios_be_t0, nicene_christological_kernel__homoousios_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nck_homoousios_be_t8, nicene_christological_kernel__homoousios_reading, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(nck_homoousios_be_t16, nicene_christological_kernel__homoousios_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(nck_homoousios_be_t24, nicene_christological_kernel__homoousios_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement(nck_homoousios_be_t32, nicene_christological_kernel__homoousios_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(nck_homoousios_be_t40, nicene_christological_kernel__homoousios_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(nck_homoousios_be_t48, nicene_christological_kernel__homoousios_reading, base_extractiveness, 48, 0.6).
narrative_ontology:measurement(nck_homoousios_be_t56, nicene_christological_kernel__homoousios_reading, base_extractiveness, 56, 0.75).
narrative_ontology:measurement(nck_homoousios_be_t66, nicene_christological_kernel__homoousios_reading, base_extractiveness, 66, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(nck_homoousios_su_t0, nicene_christological_kernel__homoousios_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(nck_homoousios_su_t8, nicene_christological_kernel__homoousios_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(nck_homoousios_su_t16, nicene_christological_kernel__homoousios_reading, suppression_requirement, 16, 0.52).
narrative_ontology:measurement(nck_homoousios_su_t24, nicene_christological_kernel__homoousios_reading, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(nck_homoousios_su_t32, nicene_christological_kernel__homoousios_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement(nck_homoousios_su_t40, nicene_christological_kernel__homoousios_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(nck_homoousios_su_t48, nicene_christological_kernel__homoousios_reading, suppression_requirement, 48, 0.73).
narrative_ontology:measurement(nck_homoousios_su_t56, nicene_christological_kernel__homoousios_reading, suppression_requirement, 56, 0.84).
narrative_ontology:measurement(nck_homoousios_su_t66, nicene_christological_kernel__homoousios_reading, suppression_requirement, 66, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoousios_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, homoiousios_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Nicene Christology' covers two structurally distinct enforcement arrangements — the homoousios reading (this file) and the homoiousios_reading (sibling file). Each gets its own epsilon, its own beneficiary/victim structure, and its own classification, per the epsilon-invariance principle: measuring the arrangement under the same-substance definition versus the similar-substance definition yields different extraction profiles because the victim sets and the doctrinal stakes differ. The upstream reading (this one, the eventual winner with higher empirical consolidation) structurally influenced the sibling: after 381 the homoiousius position lost sees, property, and legal standing, changing its operating environment without logically dissolving it as a position. Both files link each other via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
