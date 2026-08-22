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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: feudal_oath_reciprocity__vassal_coordination_reading
 *   human_readable: Feudal Oath as Chartered Mutual Obligation (Vassal Coordination Reading)
 *   domain: medieval political economy / legal history / institutional analysis
 *
 * SUMMARY:
 *   This file instantiates the vassal_coordination_reading of the contested
 *   kernel feudal_oath_reciprocity: the oath-charter complex as a system of
 *   fixed, bounded, reciprocal obligations enforced by charter text. On this
 *   reading, homage and fealty create a mutual bond; the charter caps and
 *   specifies the vassal's service and the lord's protective and adjudicative
 *   duties; the fief court, where the vassal's peers render judgment
 *   alongside the lord, enforces the terms in both directions; and formal
 *   defiance (diffidatio) gives the vassal a lawful final remedy against
 *   persistent breach. Per the epsilon-invariance principle, the sibling
 *   readings — lord_extraction_reading (the oath as an authorization grant
 *   bounded only by vassal capacity) and ecclesiastical_mediation_reading
 *   (the oath as a charity- and sacrament-limited bond) — are separate
 *   constraints over the same standing arrangement, linked through
 *   network.affects_constraints; this file authors only this reading's
 *   epsilon over the shared referent. KEY AGENTS (by structural
 *   relationship): - charter_issuing_lords: agenda-setting lord seat
 *   (powerful/constrained) — issues charters, presides over the fief court,
 *   receives defined service and tenure incidents - enfeoffed_vassals:
 *   coordinating vassal seat (organized/constrained) — renders fixed service,
 *   holds heritable tenure, sues and judges as peers -
 *   unchartered_knightly_tenants: excluded seat (moderate/trapped) — serves
 *   on oral homage without written bounds - royal_chancery_draftsmen:
 *   analytical observer seat (institutional/analytical) — drafts and
 *   registers the charter apparatus across lordships
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__vassal_coordination_reading, 0.26).
domain_priors:suppression_score(feudal_oath_reciprocity__vassal_coordination_reading, 0.32).
domain_priors:theater_ratio(feudal_oath_reciprocity__vassal_coordination_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__vassal_coordination_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__vassal_coordination_reading, rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__vassal_coordination_reading, "Feudal Oath as Chartered Mutual Obligation (Vassal Coordination Reading)").
narrative_ontology:topic_domain(feudal_oath_reciprocity__vassal_coordination_reading, "medieval political economy / legal history / institutional analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__vassal_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__vassal_coordination_reading, '9bab3904-849b-4a9e-b00c-a29c48b5cd83').
narrative_ontology:cs_kernel_codification('9bab3904-849b-4a9e-b00c-a29c48b5cd83', fixed_text).
narrative_ontology:cs_authority_grounding('9bab3904-849b-4a9e-b00c-a29c48b5cd83', practice).
narrative_ontology:cs_interpretation_layer_present('9bab3904-849b-4a9e-b00c-a29c48b5cd83').
narrative_ontology:cs_reading_relation('9bab3904-849b-4a9e-b00c-a29c48b5cd83', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('9bab3904-849b-4a9e-b00c-a29c48b5cd83', feudal_oath_reciprocity__ecclesiastical_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('9bab3904-849b-4a9e-b00c-a29c48b5cd83', foundational, charter_text_bounds_obligation).
narrative_ontology:cs_axiom_status(charter_text_bounds_obligation, holdable).
narrative_ontology:cs_axiom_grounding('9bab3904-849b-4a9e-b00c-a29c48b5cd83', charter_text_bounds_obligation, conventional).
narrative_ontology:cs_axiom('9bab3904-849b-4a9e-b00c-a29c48b5cd83', foundational, mutual_breach_entitles_defiance).
narrative_ontology:cs_axiom_status(mutual_breach_entitles_defiance, holdable).
narrative_ontology:cs_axiom_grounding('9bab3904-849b-4a9e-b00c-a29c48b5cd83', mutual_breach_entitles_defiance, conventional).
narrative_ontology:cs_reference_frame('9bab3904-849b-4a9e-b00c-a29c48b5cd83', chartered_mutual_obligation_order).
narrative_ontology:cs_drift_state('9bab3904-849b-4a9e-b00c-a29c48b5cd83', high_medieval_royal_justice_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9bab3904-849b-4a9e-b00c-a29c48b5cd83', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__vassal_coordination_reading, charter_issuing_lords).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, reciprocal_fealty_doctrine).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__vassal_coordination_reading, judgment_by_peers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Grants fiefs in return for specified service, seals the charters that record the terms, and presides over the fief court where disputes are heard. Receives homage, fealty, a stated quantity of knight service, counsel attendance, and the customary incidents of tenure such as relief and aid. Owes protection, maintenance of the vassal's holding, and judgment in his court. Cannot rewrite the written terms unilaterally; a lord who breaches faces suit in his own court and, ultimately, his vassals' lawful withdrawal of fealty.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, charter_issuing_lords, agenda_setter,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, charter_issuing_lords, beneficiary).

% Holds land by charter in return for fixed service — commonly a stated number of days' knight service per year, counsel when summoned, and agreed payments. May sue the lord in his court, with judgment rendered by fellow vassals sitting as peers. If the lord persistently breaches — taking beyond right or denying judgment — the vassal may formally defy him and renounce fealty, surrendering the fief. Exit is costly: the fief is the family's patrimony, and defiance forfeits it.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__vassal_coordination_reading, enfeoffed_vassals, payer).

% Serves lords on the strength of oral homage alone, without a sealed charter fixing the quantity of service or the lord's duties. Performs the same kind of service as chartered vassals but has no document to plead when a lord demands more than custom remembers. Would ask for the same written bounds; stands outside the charter-drafting conversation because sealing a charter costs parchment, witnesses, and a lord willing to bind himself in writing.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, unchartered_knightly_tenants, excluded,
    moderate, biographical, trapped, regional).

% Drafts, copies, and registers charters and writs for kings and great lords. Watches the charter form spread, standardizes clause language, and records renewals and confirmations. Collects nothing from any particular fief's terms and pays nothing; the seat sees the whole textual apparatus across many lordships.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__vassal_coordination_reading, royal_chancery_draftsmen, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feudal_oath_reciprocity__vassal_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feudal_oath_reciprocity__vassal_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts mutual opportunism between armed elites into fixed, mutually enforceable commitments: the vassal's service is capped and specified in writing; the lord's protection and adjudication duties are specified in writing; the fief court, staffed by the vassal's peers, gives both sides a forum; lawful defiance gives the vassal a final remedy. It solves the bilateral commitment problem of private defense where no public authority can compel performance.
% TRANSFER_FUNCTION: Moves defined military service, counsel, and agreed incident payments from vassals to lords, and protection, secure heritable tenure, and peer judgment from lords to vassals — each side's transfer fixed by charter text rather than renegotiated under duress.
% ABSENT_VOICES: Unchartered knightly tenants stand just outside the charter's protection and would demand the same written bounds. Peasant cultivators, whose labor funds the fief economy, are party to no oath and appear in these instruments only as objects of grants. In most customs, women able to hold land did so through guardianship or marital intermediaries rather than direct oath-holding. None of these seats sits in the charter-drafting conversation.
% DISAPPEARANCE_RATIONALE: If the oath-charter complex vanished overnight, landholding across western Christendom would revert to seizure and allodial uncertainty, armed retinues would dissolve into purely mercenary or predatory bands, and the courts where vassals sued lords would close — the entire tenure order of the high medieval nobility depends on it.
% FOUNDING_PROBLEM: After the Carolingian public order collapsed, raiding was endemic and no central authority could enforce bargains between armed men: warriors needed secure, heritable reward for service; landholders needed dependable armed protection; both needed a way to commit each other when neither king nor court could compel performance.
% FOUNDING_PROBLEM_CORROBORATION: Royal governments attest the problem's recession from outside the beneficiary set: Capetian and Angevin records present royal justice and the royal summons as replacing private retinues, and thirteenth-century charter confirmations increasingly justify themselves by reference to the royal peace. Ecclesiastical chroniclers corroborate the original problem's reality — the violence the Peace of God movement answered — while disputing that the secular oath was its proper solution. No source outside the beneficiary set attests that the founding problem remains fully live; the lords' own charters concede expanding royal jurisdiction.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__vassal_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__vassal_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__vassal_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__vassal_coordination_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low (0.26 at interval end) because the reading's own lights show transfers bounded by text and running in both directions: reliefs, aids, and wardship incidents are specified incidents of tenure, not open-ended takings, and the slow creep across the interval tracks hardening incident practices rather than any collapse of the bounds. Suppression is moderate-low (0.32) and authored as a static scalar: the enforcement character is stable across the interval — mutual, procedural, court-mediated, with lawful defiance available — so per the static-enforcement rule no suppression_requirement series is authored; the scalar carries the picture. Theater_ratio rises from 0.08 to 0.22: early homage ritual is constitutive (the ceremony IS the bond-making), while by 1300 some ceremonial forms persist as functions migrate to royal justice and money payments. Accessibility_collapse is 0.38: real alternatives persisted (allodial tenure in the south, free towns, resort to royal courts, diffidatio itself), so understanding the constraint did not extinguish exits. Resistance is 0.42: private war, defiance, litigation, and baronial league-making (the Magna Carta pattern) met particular lords constantly, though the institution itself drew less resistance than any specific breach. The claim (rope) and the metrics are independently authored: the rope claim states this reading's structural view of the arrangement; the metrics describe its observed operation; the engine computes per-seat types from the structural data. All temporal series share one nine-point grid (900–1300 at fifty-year steps) so every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same charter text. From the lord's seat the arrangement is an instrument he drafted that secures reliable service and counsel; from the vassal's seat it is tenure security and a court where his peers can check his lord; from the unchartered tenant's seat it is a protection he can see but cannot obtain, since his homage bought no written bounds; from the chancery seat it is a spreading documentary technology with no stake in any particular fief. The engine derives these divergences from role, power, and exit data — the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Both principal parties sit near the symmetric midpoint. Enfeoffed_vassals are declared beneficiaries (protected heritable tenure, peer judgment) carrying a secondary payer position (the service and incidents they render); charter_issuing_lords are the agenda-setters and beneficiaries (defined service, counsel) whose own duties run in the opposite direction. Neither declaration supports a full-target or full-beneficiary derivation for either seat — the structure is deliberately two-sided. Unchartered_knightly_tenants, as an excluded seat, feed the absent-voices analysis rather than the directionality derivation: they are adjacent to the constraint, not governed by its charter terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — credible mutual commitment between armed men under collapsed public authority — was live for most of the interval and recedes by its end as royal justice expands; hence founding_problem_status is contested rather than dead, and the status-times-verdict pair (contested x world_rearranges) raises no zombie flag. Classifying this reading as rope guards against the overcorrection of reading pure extraction into a genuinely mutual structure simply because the surrounding society was violent; conversely, the omega variables keep the extraction and sacramental readings live as separate constraints, so the rope claim here cannot silently absorb the lord_extraction_reading's higher epsilon. The receipt surface pairs diffuse gains with prohibitive fixing cost — nominally the piton cell — but the piton reading does not fit: theater_ratio is 0.22, the coordination function is demonstrably live at interval end, and the prohibitive cost of removal reflects systemic entanglement (tenure security collapsing if the complex vanished), not atrophied function maintained by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the feudal oath''s binding content fixed reciprocal obligation measured by charter text (this reading), an authorization of extraction bounded only by vassal capacity (lord_extraction_reading), or a sacramentally limited bond (ecclesiastical_mediation_reading)?',
    'Comparative coding of charter clauses and court rolls across polities: whether texts specify upper bounds on lord takings, whether courts enforce those bounds against lords, and whether sacramental sanctions appear in enforcement records.',
    'Adopting the lord_extraction_reading raises epsilon sharply and shifts classification toward snare or tangled_rope; adopting the ecclesiastical_mediation_reading lowers suppression further and relocates enforcement to church courts. This file''s rope classification holds only under this reading''s premises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this constraint is the vassal_coordination_reading of kernel feudal_oath_reciprocity; sibling readings instantiate different constraints over the same arrangement.').

omega_variable(
    fief_court_enforcement_symmetry,
    'Did fief courts actually render judgment against lords at rates comparable to judgments against vassals?',
    'Quantitative analysis of surviving court rolls recording plaints, judgments, and amercements by party status.',
    'Systematic asymmetry would raise effective extraction above the authored epsilon and push even this reading toward tangled_rope; documented symmetry confirms the rope profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fief_court_enforcement_symmetry, empirical, 'Whether mutual enforceability was practiced or merely nominal.').

omega_variable(
    party_boundary_framing,
    'Does the no-structural-victim claim hold only because the constraint''s parties are defined as oath-holders, excluding the peasant labor that underwrites the fief economy?',
    'Explicit boundary decision: classify the oath-charter complex narrowly (oath parties only, as authored here) or widen to the whole tenure system including manorial obligations; the widened boundary imports victims and raises epsilon.',
    'Widening the boundary converts this rope into a component of a larger tangled_rope or snare structure; the authored epsilon is valid only for the narrow boundary this file declares.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(party_boundary_framing, conceptual, 'Framing choice documenting why peasant cultivators are absent from the party set.').

omega_variable(
    commutation_preserves_bounds,
    'Did commutation of military service into money payments (scutage and its continental equivalents) preserve the charter''s fixed bounds, or open unbounded assessment?',
    'Trace assessed rates against charter-stated incidents across the twelfth and thirteenth centuries; compare rates agreed with vassals against rates imposed unilaterally.',
    'Unbounded assessment would raise late-interval epsilon above the authored endpoint and date a rope-to-tangled_rope transition before 1300.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commutation_preserves_bounds, empirical, 'Whether monetization of service preserved or eroded the fixed-bounds premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__vassal_coordination_reading, 900, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t900, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 900, 0.08).
narrative_ontology:measurement_basis(feud_tr_t900, observed).
narrative_ontology:measurement(feud_tr_t950, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 950, 0.09).
narrative_ontology:measurement_basis(feud_tr_t950, observed).
narrative_ontology:measurement(feud_tr_t1000, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement_basis(feud_tr_t1000, observed).
narrative_ontology:measurement(feud_tr_t1050, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1050, 0.11).
narrative_ontology:measurement_basis(feud_tr_t1050, observed).
narrative_ontology:measurement(feud_tr_t1100, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1100, 0.13).
narrative_ontology:measurement_basis(feud_tr_t1100, observed).
narrative_ontology:measurement(feud_tr_t1150, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1150, 0.15).
narrative_ontology:measurement_basis(feud_tr_t1150, observed).
narrative_ontology:measurement(feud_tr_t1200, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1200, 0.17).
narrative_ontology:measurement_basis(feud_tr_t1200, observed).
narrative_ontology:measurement(feud_tr_t1250, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1250, 0.2).
narrative_ontology:measurement_basis(feud_tr_t1250, observed).
narrative_ontology:measurement(feud_tr_t1300, feudal_oath_reciprocity__vassal_coordination_reading, theater_ratio, 1300, 0.22).
narrative_ontology:measurement_basis(feud_tr_t1300, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t900, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 900, 0.14).
narrative_ontology:measurement_basis(feud_be_t900, observed).
narrative_ontology:measurement(feud_be_t950, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 950, 0.15).
narrative_ontology:measurement_basis(feud_be_t950, observed).
narrative_ontology:measurement(feud_be_t1000, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1000, 0.16).
narrative_ontology:measurement_basis(feud_be_t1000, observed).
narrative_ontology:measurement(feud_be_t1050, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1050, 0.18).
narrative_ontology:measurement_basis(feud_be_t1050, observed).
narrative_ontology:measurement(feud_be_t1100, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1100, 0.2).
narrative_ontology:measurement_basis(feud_be_t1100, observed).
narrative_ontology:measurement(feud_be_t1150, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1150, 0.22).
narrative_ontology:measurement_basis(feud_be_t1150, observed).
narrative_ontology:measurement(feud_be_t1200, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1200, 0.23).
narrative_ontology:measurement_basis(feud_be_t1200, observed).
narrative_ontology:measurement(feud_be_t1250, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1250, 0.25).
narrative_ontology:measurement_basis(feud_be_t1250, observed).
narrative_ontology:measurement(feud_be_t1300, feudal_oath_reciprocity__vassal_coordination_reading, base_extractiveness, 1300, 0.26).
narrative_ontology:measurement_basis(feud_be_t1300, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(feudal_oath_reciprocity__vassal_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__vassal_coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__vassal_coordination_reading, feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the feudal oath' decomposes into three structurally distinct readings of one kernel. This file authors the vassal_coordination_reading (fixed bounded reciprocity, low epsilon, rope). The lord_extraction_reading authors the same standing arrangement as an authorization grant (high epsilon); the ecclesiastical_mediation_reading authors it as a charity- and sacrament-limited bond (lower suppression, different enforcement locus). Epsilon differs across the family because the readings locate the oath's binding content differently; per the epsilon-invariance principle each is a separate constraint linked here rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
