% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__vassal_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__vassal_coordination_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath as Chartered Mutual Obligation (Vassal Coordination Reading)
 *   domain: medieval political economy / legal history / institutional analysis
 *
 * SUMMARY:
 *   A lord grants a vassal land; the vassal swears homage and fealty; a
 *   charter fixes what each owes the other — defined knight service, castle
 *   guard, and counsel from the vassal; protection, secure heritable tenure,
 *   and judgment by peers from the lord. Because the terms are written,
 *   witnessed, sealed, and preserved, the bargain outlives both parties and
 *   can be pleaded by widows, heirs, and successor lords. This story authors
 *   that arrangement as the vassal_coordination_reading of the
 *   feudal_oath_reciprocity kernel: a low-extraction coordination mechanism
 *   whose enforcement is textual and mutual. It is linked to its sibling
 *   readings, which read the same oaths as authorization for extraction or as
 *   sacramentally limited trust; those are separate constraints with separate
 *   files and separate epsilon values.
 *
 * KEY AGENTS:
 *   - vassal_knights: net gainer (organized/constrained) — holds heritable fief for chartered service; litigates rather than exits
 *   - seigneurial_lords: administering grantor (powerful/constrained) — receives service, owes protection and peer judgment, bound by sealed text
 *   - royal_overlords: layered beneficiary (institutional/constrained) — harvests mobilization and incidents up the homage chain
 *   - monastic_charter_custodians: evidentiary infrastructure (institutional/constrained) — draft, seal, preserve; neither owe nor collect service
 *   - unfree_villeins: excluded seat (powerless/trapped) — bear undocumented dues beneath the fief; not parties to the charter
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.16).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.29).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.16).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.29).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath as Chartered Mutual Obligation (Vassal Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval political economy / legal history / institutional analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__vassal_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '9be0330d-4246-4d68-833c-bf8b6736f21e').
narrative_ontology:cs_kernel_codification('9be0330d-4246-4d68-833c-bf8b6736f21e', fixed_text).
narrative_ontology:cs_authority_grounding('9be0330d-4246-4d68-833c-bf8b6736f21e', practice).
narrative_ontology:cs_interpretation_layer_present('9be0330d-4246-4d68-833c-bf8b6736f21e').
narrative_ontology:cs_reading_relation('9be0330d-4246-4d68-833c-bf8b6736f21e', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('9be0330d-4246-4d68-833c-bf8b6736f21e', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('9be0330d-4246-4d68-833c-bf8b6736f21e', foundational, chartered_oath_binds_lord_and_vassal_symmetrically).
narrative_ontology:cs_axiom_status(chartered_oath_binds_lord_and_vassal_symmetrically, holdable).
narrative_ontology:cs_axiom_grounding('9be0330d-4246-4d68-833c-bf8b6736f21e', chartered_oath_binds_lord_and_vassal_symmetrically, conventional).
narrative_ontology:cs_axiom('9be0330d-4246-4d68-833c-bf8b6736f21e', foundational, service_demand_beyond_charter_text_is_wrongful).
narrative_ontology:cs_axiom_status(service_demand_beyond_charter_text_is_wrongful, holdable).
narrative_ontology:cs_axiom_grounding('9be0330d-4246-4d68-833c-bf8b6736f21e', service_demand_beyond_charter_text_is_wrongful, conventional).
narrative_ontology:cs_reference_frame('9be0330d-4246-4d68-833c-bf8b6736f21e', charter_bounded_mutual_service).
narrative_ontology:cs_drift_state('9be0330d-4246-4d68-833c-bf8b6736f21e', late_thirteenth_century_royal_justice_expansion, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('9be0330d-4246-4d68-833c-bf8b6736f21e', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, vassal_knights).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, seigneurial_lords).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, royal_overlords).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__vassal_coordination_reading, vassal_knights).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold land of a lord in return for service fixed in the charter of enfeoffment: typically forty days' knight service a year, castle guard, and suit of court. The charter caps what can be demanded; a lord demanding beyond the stated service faces formal defiance (diffidatio) or a plea in the honor court. Renouncing homage is a recognized procedure but costs the fief and standing, so most vassals litigate rather than leave. Service passes with the fief to heirs, so families plan across generations.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, vassal_knights, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, vassal_knights, payer).

% Grant fiefs and receive the chartered services; preside over the honor court that hears disputes between themselves and their vassals; issue or confirm the charters that fix obligations. They bear the reciprocal side: protection of the vassal's holding, maintenance, and judgment of the vassal by his peers. Their administration is constrained by the written terms they have sealed — renegotiating a charter requires fresh grant and witnesses, and the sealed text binds their heirs.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, seigneurial_lords, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, seigneurial_lords, beneficiary).

% Sit at the apex of layered homage: fealty chains upward through sub-vassals, giving the crown a summons-based host and a ladder of incidents (reliefs, wardship, escheat) at each succession. They gain mobilization capacity without salaried administration, and their courts increasingly supply the ultimate forum in which charter terms are tested.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, royal_overlords, beneficiary,
    institutional, generational, constrained, national).

% Draft, witness, seal, and preserve the charters in cartularies; monastic houses hold extensive temporalities whose security depends on the same documented tenure. They are present at every enfeoffment and inherit the evidentiary role when memory fails, but they neither owe nor collect the services the documents record.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, monastic_charter_custodians, observer,
    institutional, generational, constrained, regional).

% Work the demesne and tenancy lands beneath the fief. The documented reciprocity runs between lord and vassal; their own dues — week-work, tallages, merchet — are recorded in manorial accounts they cannot read and are set without their consent or countersignature. They appear in the record as objects of the arrangements, never as parties.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, unfree_villeins, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__vassal_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__vassal_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the credible-commitment problem of a world without standing armies or state courts: how an armed householder can commit to appear with horses and mail when summoned, and how a lord can commit to protect the holding and judge disputes by settled rules instead of seizure. Written charters make both commitments inspectable by third parties, heirs, and successor lords, so the bargain survives the individuals who struck it.
% TRANSFER_FUNCTION: Moves defined military service, castle guard, and counsel from vassals to lords, and protection, secure heritable tenure, and peer judgment from lords to vassals. Both legs are capped by the charter text; nothing in the documented exchange is open-ended.
% ABSENT_VOICES: Unfree cultivators beneath the fief would object that the celebrated mutuality stops one rung above their heads: their labor dues are fixed by manorial account, not by any charter they could invoke, and they enter witness lists only as landmarks ('from the oak to the brook'). They are excluded from the honor court, the charter, and the literacy that makes the charter enforceable. Widows and younger sons hold partial voice through dower and inheritance pleas.
% DISAPPEARANCE_RATIONALE: Without the chartered exchange, military mobilization reverts to ad hoc retainer and plunder, landholding loses its heritable security and with it the incentive to plant, drain, and build in stone, and dispute settlement reverts to force between armed neighbors. Every seated party's position is constituted by the arrangement, so its overnight loss reorganizes all of them.
% FOUNDING_PROBLEM: After the collapse of public authority, armed men and territorial lords needed a way to make promises about future violence and future protection credible to each other across decades and dynasties, without a state to enforce either side.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: royal plea rolls record vassals suing lords on charter terms and lords suing vassals for withheld service, showing both sides treated the documented bargain as real; monastic annalists complain when either side breaches. Modern historiography contests the framing itself — Ganshof and Bloch reconstruct a classical fixed-reciprocal institution from the charters, while Reynolds argues the tidy package is a twelfth-century lawyers' retrojection onto looser earlier practice — so the founding problem's shape is attested even where its systematic character is disputed.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.16, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).
:- end_tests(feudal_oath_reciprocity__vassal_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.16 at interval end) because the reading's defining feature is that the charter caps demand on both sides: the residual above the coordination floor is court costs, ceremony, and occasional incidental labor, not rent. Suppression (0.29) is authored as a raw structural property — it is not scaled by power or scope — reflecting real but mutual sanction: forfeiture for withheld service, dower and disseisin remedies for seized holdings. Theater ratio (0.22) is low but rising: homage ceremony remains constitutive early and grows ornamental as royal writs carry more of the load. Accessibility collapse (0.40) is moderate: allodial holding, ecclesiastical tenure, and paid retinue remained real alternatives throughout. Resistance (0.28) is correspondingly modest: defiance (diffidatio) and charter pleas occur, but the institution broadly holds. The measurement series share one time grid (950-1300 at fifty-year steps) with all three metrics authored at every point. The suppression_requirement series is authored deliberately: the story tracks an enforcement-capacity transition from feud and self-help to charter-plus-court enforcement, so the falling trajectory is the finding, not noise. End-state scalars match the final grid values.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical nominal relations. From the vassal seat the charter is armor: the same document that obliges him to serve forbids the lord from taking more. From the lord seat the charter is a concession purchased once and binding his heirs forever — the administering seat experiences textual bounding as loss of discretion. From the royal seat the whole lattice is infrastructure: a summons tree he did not build and need not maintain. The villein seat sees none of the mutuality at all, only the demesne bell. The engine derives these divergences from power, exit, and role data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Both principal parties are declared beneficiaries because each collects the protection the other's commitment provides — that is what reciprocity means — but neither sits at the beneficiary pole, because each also pays. The automatic derivation from beneficiary declarations alone would push both seats toward low d; overrides correct this: vassal_knights (organized) are authored at d=0.50 because chartered service is a real, large payment offsetting received tenure and protection; seigneurial_lords (powerful) at d=0.40 because they collect the service stream yet bear protection and adjudication costs and have surrendered discretionary exaction. royal_overlords are left to derivation: layered homage nets them mobilization at low marginal cost, near the beneficiary end. monastic_charter_custodians observe without collecting; unfree_villeins are excluded rather than targeted by this constraint — their burden arises from a separate manorial arrangement, which is why no victims are declared here.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways. Against the extraction sibling: fixing the referent to the chartered exchange prevents reading the lord's legitimate collection of agreed service as pure extraction — a charge that would make every paid obligation a snare. Against romance: the villein exclusion and the rising theater ratio are authored honestly rather than smoothed away, and the mutuality-boundary omega forces the question of whether the no-victim premise survives contact with those beneath the fief. The founding problem is marked contested rather than dead, so no spurious obsolescence flag fires on a constraint whose decline came by absorption into royal justice, not by decay of function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the vassal_coordination_reading of the feudal_oath_reciprocity kernel; what would adopting a sibling reading change structurally?',
    'Generate lord_extraction_reading and ecclesiastical_mediation_reading as separate stories and compare computed types and epsilon over the same charter corpus; the disagreement is located in whether charter bounds bind (this reading), merely mask capacity-limited exaction (extraction reading), or sit under a sacramental ceiling (ecclesiastical reading).',
    'If the extraction reading is adopted, epsilon rises sharply and the type moves toward tangled_rope or snare with lords as concentrated receivers; if the ecclesiastical reading is adopted, suppression drops further as church sanctions substitute for coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of the feudal-oath kernel; sibling readings instantiate different constraints.').

omega_variable(
    charter_bound_enforceability,
    'Were charter-stated bounds actually enforceable against a determined lord, or did power override text when it mattered?',
    'Code honor-court and royal plea-roll outcomes in disputes over services demanded beyond charter terms: sanction rates against breaching lords versus breaching vassals.',
    'If lords breach without sanction, effective extraction exceeds the authored 0.16 and the coordination reading decays toward the extraction sibling; if sanctions bite, the low-epsilon profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(charter_bound_enforceability, empirical, 'Whether the written bounds had teeth against the stronger party.').

omega_variable(
    historiographical_construct_status,
    'Is the fixed, bounded, mutually enforceable package a description of period practice or a twelfth-century legal scholastic retrojection onto looser custom?',
    'Date the language of fixed service terms in surviving charters against practice evidence (account rolls, campaign records): if precise bounds appear only after professional lawyers circulate, the reading describes the treatise stratum, not the tenth-century oath.',
    'If retrojected, the referent of epsilon shifts to the lawyer-made institution of the late period, and the early-interval measurements describe a different, looser arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historiographical_construct_status, conceptual, 'Whether the reading''s referent is practice or learned reconstruction.').

omega_variable(
    mutuality_boundary_villein_exclusion,
    'Does the villein''s exclusion from the documented reciprocity fall inside this constraint''s referent or inside a separate manorial-dues constraint?',
    'Decompose per the epsilon-invariance rule: author the manorial labour-dues arrangement as its own story with its own victims and compare; the oath-charter story keeps only the lord-vassal exchange.',
    'If the exclusion is ruled inside this referent, the no-structural-victim premise fails, victims must be declared, and the type moves off rope toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutuality_boundary_villein_exclusion, conceptual, 'Where the boundary of the reading''s referent sits relative to those beneath the fief.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 950, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_oath_vcr_tr_t950, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 950, 0.08).
narrative_ontology:measurement_basis(feudal_oath_vcr_tr_t950, observed).
narrative_ontology:measurement(feudal_oath_vcr_tr_t1000, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1000, 0.09).
narrative_ontology:measurement_basis(feudal_oath_vcr_tr_t1000, observed).
narrative_ontology:measurement(feudal_oath_vcr_tr_t1050, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1050, 0.1).
narrative_ontology:measurement_basis(feudal_oath_vcr_tr_t1050, observed).
narrative_ontology:measurement(feudal_oath_vcr_tr_t1100, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1100, 0.11).
narrative_ontology:measurement_basis(feudal_oath_vcr_tr_t1100, observed).
narrative_ontology:measurement(feudal_oath_vcr_tr_t1150, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1150, 0.13).
narrative_ontology:measurement_basis(feudal_oath_vcr_tr_t1150, observed).
narrative_ontology:measurement(feudal_oath_vcr_tr_t1200, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1200, 0.16).
narrative_ontology:measurement_basis(feudal_oath_vcr_tr_t1200, observed).
narrative_ontology:measurement(feudal_oath_vcr_tr_t1250, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1250, 0.19).
narrative_ontology:measurement_basis(feudal_oath_vcr_tr_t1250, observed).
narrative_ontology:measurement(feudal_oath_vcr_tr_t1300, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1300, 0.22).
narrative_ontology:measurement_basis(feudal_oath_vcr_tr_t1300, observed).

% Extraction over time
narrative_ontology:measurement(feudal_oath_vcr_be_t950, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 950, 0.24).
narrative_ontology:measurement_basis(feudal_oath_vcr_be_t950, observed).
narrative_ontology:measurement(feudal_oath_vcr_be_t1000, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1000, 0.22).
narrative_ontology:measurement_basis(feudal_oath_vcr_be_t1000, observed).
narrative_ontology:measurement(feudal_oath_vcr_be_t1050, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1050, 0.2).
narrative_ontology:measurement_basis(feudal_oath_vcr_be_t1050, observed).
narrative_ontology:measurement(feudal_oath_vcr_be_t1100, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1100, 0.18).
narrative_ontology:measurement_basis(feudal_oath_vcr_be_t1100, observed).
narrative_ontology:measurement(feudal_oath_vcr_be_t1150, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1150, 0.17).
narrative_ontology:measurement_basis(feudal_oath_vcr_be_t1150, observed).
narrative_ontology:measurement(feudal_oath_vcr_be_t1200, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1200, 0.16).
narrative_ontology:measurement_basis(feudal_oath_vcr_be_t1200, observed).
narrative_ontology:measurement(feudal_oath_vcr_be_t1250, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1250, 0.16).
narrative_ontology:measurement_basis(feudal_oath_vcr_be_t1250, observed).
narrative_ontology:measurement(feudal_oath_vcr_be_t1300, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1300, 0.16).
narrative_ontology:measurement_basis(feudal_oath_vcr_be_t1300, observed).

% Suppression requirement over time
narrative_ontology:measurement(feudal_oath_vcr_su_t950, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 950, 0.55).
narrative_ontology:measurement_basis(feudal_oath_vcr_su_t950, observed).
narrative_ontology:measurement(feudal_oath_vcr_su_t1000, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1000, 0.5).
narrative_ontology:measurement_basis(feudal_oath_vcr_su_t1000, observed).
narrative_ontology:measurement(feudal_oath_vcr_su_t1050, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1050, 0.46).
narrative_ontology:measurement_basis(feudal_oath_vcr_su_t1050, observed).
narrative_ontology:measurement(feudal_oath_vcr_su_t1100, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1100, 0.42).
narrative_ontology:measurement_basis(feudal_oath_vcr_su_t1100, observed).
narrative_ontology:measurement(feudal_oath_vcr_su_t1150, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1150, 0.38).
narrative_ontology:measurement_basis(feudal_oath_vcr_su_t1150, observed).
narrative_ontology:measurement(feudal_oath_vcr_su_t1200, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1200, 0.34).
narrative_ontology:measurement_basis(feudal_oath_vcr_su_t1200, observed).
narrative_ontology:measurement(feudal_oath_vcr_su_t1250, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1250, 0.31).
narrative_ontology:measurement_basis(feudal_oath_vcr_su_t1250, observed).
narrative_ontology:measurement(feudal_oath_vcr_su_t1300, feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 1300, 0.29).
narrative_ontology:measurement_basis(feudal_oath_vcr_su_t1300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'feudal oath reciprocity' decomposes into three structurally distinct claims sharing one kernel. This story (vassal_coordination_reading) authors the low-epsilon coordination claim: charter text fixes and bounds a mutual exchange. lord_extraction_reading authors the same oaths as extraction authorization (high epsilon, victims declared); ecclesiastical_mediation_reading authors the sacramental-limit overlay. The coordination reading is upstream in evidentiary terms — both siblings cite the same charter corpus, and the extraction reading's case rests on showing the bounds this reading takes as binding fail in practice. Each file links the others via affects_constraints; epsilon differs across the family because the referent differs, not because the observables differ.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__vassal_coordination_reading, organized, 0.5).
constraint_indexing:directionality_override(feudal_oath_reciprocity__vassal_coordination_reading, powerful, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
