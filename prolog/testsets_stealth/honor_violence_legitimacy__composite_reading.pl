% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__composite_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Overdetermined Decline of Honor-Violence Legitimacy (Composite Reading)
 *   domain: historical sociology / legal anthropology / commitment systems
 *
 * SUMMARY:
 *   Between the late eighteenth and early twentieth centuries, the European
 *   honor-violence settlement was dismantled by two mechanisms at once. The
 *   first was external cost: statutes, courts-martial, fines, and career
 *   destruction raised the price of answering a challenge while large parts
 *   of elite opinion still held the old code legitimate. The second was
 *   conceptual contraction: clergy, publicists, and eventually the
 *   professions redefined honor itself so that violent vindication ceased to
 *   be what honor was, making the duel progressively unthinkable rather than
 *   merely dangerous. This story instantiates the composite reading: the two
 *   mechanisms ran simultaneously, produced different victim sets with
 *   different extractiveness profiles, and — the composite's distinctive edge
 *   — the contraction component is what made the decline durable, since the
 *   drop mechanism alone predicts revival wherever enforcement lapses, and
 *   revival is what the record does not show. KEY AGENTS (by structural
 *   relationship): - centralizing_state_authorities: Agenda-setter
 *   (institutional/arbitrage) — administers the penal mechanism, collects the
 *   precedents - moral_reform_elite: Beneficiary with agenda-setting reach
 *   (organized/mobile) — authors the redefinition - prosecuted_duelists:
 *   Primary target of the cost mechanism (moderate/trapped) — bear prison and
 *   ruin - honor_traditionalist_gentry: Primary target of the redefinition
 *   mechanism (powerful/identity_locked) — bear uncompensated status
 *   expropriation - rising_professional_classes and bourgeois_civil_society:
 *   Beneficiaries (organized/mobile) - commoners_denied_satisfaction:
 *   Excluded seat (powerless/trapped) — never admitted to the satisfaction
 *   economy - historical_sociology_observers: Analytical observer — sees both
 *   mechanisms at once
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.58).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.35).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Overdetermined Decline of Honor-Violence Legitimacy (Composite Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "historical sociology / legal anthropology / commitment systems").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, 'd5acd776-d3bd-4a61-bfb6-f707d9cc6ba3').
narrative_ontology:cs_kernel_codification('d5acd776-d3bd-4a61-bfb6-f707d9cc6ba3', distributed).
narrative_ontology:cs_authority_grounding('d5acd776-d3bd-4a61-bfb6-f707d9cc6ba3', practice).
narrative_ontology:cs_interpretation_layer_present('d5acd776-d3bd-4a61-bfb6-f707d9cc6ba3').
narrative_ontology:cs_reading_relation('d5acd776-d3bd-4a61-bfb6-f707d9cc6ba3', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('d5acd776-d3bd-4a61-bfb6-f707d9cc6ba3', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_axiom('d5acd776-d3bd-4a61-bfb6-f707d9cc6ba3', foundational, mechanisms_jointly_necessary).
narrative_ontology:cs_axiom_status(mechanisms_jointly_necessary, holdable).
narrative_ontology:cs_axiom_grounding('d5acd776-d3bd-4a61-bfb6-f707d9cc6ba3', mechanisms_jointly_necessary, empirically_contingent).
narrative_ontology:cs_axiom('d5acd776-d3bd-4a61-bfb6-f707d9cc6ba3', secondary, transition_harms_jointly_authored).
narrative_ontology:cs_axiom_status(transition_harms_jointly_authored, holdable).
narrative_ontology:cs_axiom_grounding('d5acd776-d3bd-4a61-bfb6-f707d9cc6ba3', transition_harms_jointly_authored, conventional).
narrative_ontology:cs_reference_frame('d5acd776-d3bd-4a61-bfb6-f707d9cc6ba3', practice_adjudicated_satisfaction_regime).
narrative_ontology:cs_drift_state('d5acd776-d3bd-4a61-bfb6-f707d9cc6ba3', post_redefinition_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('d5acd776-d3bd-4a61-bfb6-f707d9cc6ba3', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, centralizing_state_authorities).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, moral_reform_elite).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, rising_professional_classes).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, bourgeois_civil_society).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, prosecuted_duelists).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, honor_traditionalist_gentry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislates against dueling, courts-martials officers who fight, collects fines and forfeitures, and converts each prosecution into precedent extending official jurisdiction over conduct previously governed by the honor economy. Sets both the penal schedule and, through allied institutions, the terms on which honor is publicly defined. Writes the rules it enforces, so exit from its own position is meaningless.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, centralizing_state_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Clergy, evangelical movements, publicists, and later physicians who campaign to redefine honor as conscience, profession, and self-command rather than readiness to fight. They do not run the prosecutions, but they author the redefinition that makes the prosecutions unnecessary. They collect influence, audiences, and institutional positions as the new definition spreads.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, moral_reform_elite, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, moral_reform_elite, agenda_setter).

% Lawyers, bureaucrats, engineers, and merchants whose claim to elite standing rests on credential and competence. Once honor is defined by profession rather than by lineage and courage-display, the gatekeeping function the duel performed passes to examinations and careers, which these classes win.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, rising_professional_classes, beneficiary,
    organized, biographical, mobile, national).

% The broad commercial and civic public that gains from pacified elite politics: parliamentary stability, insurable lives, newspapers free of feud vendettas. Its gains are diffuse and incidental rather than administered.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, bourgeois_civil_society, beneficiary,
    organized, biographical, mobile, national).

% Officers and gentlemen who answered a challenge while the old code still governed their world, and who bore prison, fines, dismissal, or ruin for it. Their bind is total: fight and face the prosecutor, refuse and face dishonor and the end of a career built on the old code. Individually they hold little leverage even when their class holds much.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, prosecuted_duelists, payer,
    moderate, biographical, trapped, national).

% Holders of the dynastic honor code whose status capital — lineage prestige, reputation for courage, patronage networks built on a record of satisfaction — is written off as the definition of honor shifts. Nothing is confiscated from them and no court touches them; their loss is that the currency they spent generations accumulating stops circulating. Leaving the code would mean dissolving the self their families constructed.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, honor_traditionalist_gentry, payer,
    powerful, generational, identity_locked, continental).

% The lower orders were never admitted to the satisfaction economy: their quarrels were prosecuted as brawls, their deaths in affrays went uncommemorated, and they served as the seconds, surgeons, and soldiers of the honor class without ever holding a challengeable honor of their own. They would object that the entire legitimacy structure was a class franchise, and that its retirement traded their safety for elite pacification without their consent. They are outside the archive in which the debate is conducted.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, commoners_denied_satisfaction, excluded,
    powerless, generational, trapped, national).

% Scholars working across legal archives, regimental records, and prescriptive literature who reconstruct the decline's mechanisms. They bear no costs and collect no rents; their seat exists to see both mechanisms at once, which participants could not.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, historical_sociology_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__composite_reading, centralizing_state_authorities).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces cyclical private vengeance among an armed elite with centralized adjudication: disputes that once escalated through challenge, negotiation, and satisfaction loops are routed into courts, military procedure, and reputational markets. The problem solved is real — feud instability among the class that monopolizes weapons is destructive to everyone, including the feuding class.
% TRANSFER_FUNCTION: Moves status capital and adjudicative authority from the gentlemanly honor economy to the state and the professions: the individual's right to settle scores becomes the state's monopoly on legitimate violence, and honor's definition passes from the regimental mess to the credentialing institution. Incidentally it moves liberty and livelihood from prosecuted duelists to the enforcement apparatus as fines, forfeitures, and precedent.
% ABSENT_VOICES: The common orders excluded from the satisfaction economy would object loudest — they died as seconds and uncommemorated brawlers under the old regime and were never consulted about the new one. Women of honor families bore the casualties and the widowing without standing in the code. The dead of the dueling ground itself are the permanently absent party. All of them are missing because the sources the historiography runs on — elite correspondence, court records, prescriptive manuals — were produced by the seats at the table.
% DISAPPEARANCE_RATIONALE: If the composite settlement vanished overnight, the question of when violence legitimately answers an affront reopens: the state's claim to a monopoly on legitimate violence, the professional classes' title to elite standing, and the entire architecture of elite-conflict adjudication rest on it. Courts, military discipline, and parliamentary order would need to rebuild their authority over the armed classes from scratch.
% FOUNDING_PROBLEM: The original kernel answered an older problem: how may a gentleman answer an affront without feud or murder, given a weak state that cannot adjudicate elite disputes. The decline arrangement addressed a newer one: how does a centralizing state disarm and pacify an armed honor class without provoking defiance, martyrdom, or civil rupture?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: nineteenth-century radical and liberal critics attested at the time that enforcement targeted class privilege as much as violence; modern historical sociology on the state's violence monopoly confirms the armed honor class no longer exists as a political actor; military historians establish dueling's extinction independently of any state interest in the finding. No seat in the beneficiary set is needed to carry the claim that the pacification problem is closed.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__composite_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_violence_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claim is tangled_rope on structure: the arrangement solves a genuine collective-action problem (feud cycles among an armed class), requires active enforcement (statutes, courts-martial — without which the cost mechanism does not exist), and concentrates real costs on identifiable groups while benefits accrue elsewhere. The metrics describe operation as the record shows it. Extractiveness ends at 0.58: substantial but not dominant, because the arrangement's largest output is pacification, with extraction riding on it — coercive harm to prosecuted duelists at the enforcement peak, and the write-off of the traditionalist gentry's status capital without compensation. Suppression ends at 0.35 and FALLS across the interval: this is the composite reading's central observable. Enforcement requirement decays because the desire to duel decays — the tell that the cost mechanism was never self-sustaining. Theater ratio rises to 0.55 as the residual machinery (dead-letter statutes, show prosecutions ending in acquittal, ceremonial condemnation) increasingly performs opposition to a practice nobody wants to resume. Accessibility_collapse is 0.45, moderate: the old alternative (violent vindication) collapsed almost completely, but the arrangement channeled elite conflict into courts and careers rather than foreclosing alternatives wholesale. Resistance is 0.6 and is largely coalition resistance: juries drawn from the honor class refused to convict, regiments shielded duelists, and the gentry defended the code in print for three generations — a reminder that 'powerless' targets can sometimes act as a class, though here the resisting class was powerful and still lost. All three tracked series share one time grid (points 0-30 at step 5) so no metric's end-state leaks backward into earlier rows. Suppression is authored as a raw structural property; only extractiveness gets scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute very different constraints from the same structure. From the prosecuted duelist's chair (trapped, moderate power), the arrangement is experienced as pure coercion — a machine that punishes you whichever horn of the code you take — and computes toward the extractive end of the space. From the traditionalist gentry's chair (identity_locked), nothing visible happens at all: no statute touches them, yet their life's status capital stops circulating — extraction experienced as meaning-loss rather than force. From the state's chair (arbitrage exit, agenda control), the same arrangement is legitimate institution-building, the founding act of the modern violence monopoly. The engine derives this divergence from the declared roles, power atoms, and exit options; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the state (agenda-setter, arbitrage exit) sits nearest the beneficiary pole; the reform elite, professional classes, and civil society take low directionalities as net collectors. Victim declarations drive the opposite pole: prosecuted duelists (trapped) and the traditionalist gentry (identity_locked) sit near the full-target end, with the identity lock pushing the gentry further toward it than their raw power would suggest — identity fusion amplifies effective extraction regardless of formal standing. One override is declared: for the powerless atom, d is pinned to 0.5. The derivation would otherwise treat the excluded commoners as pseudo-victims (powerless agents adjacent to an extractive structure typically derive high d), but they stand OUTSIDE the satisfaction economy this arrangement retires — neither subsidized by it nor harvested by it; their grievance is against the franchise itself, not against the transition's incidence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — pacifying an armed honor class — is dead: that class no longer exists as a political actor, and the corroboration comes from outside the beneficiary set. Yet the arrangement persists as dead-letter statutes and theatrical prosecutions, which is exactly the status=dead x world_rearranges mismatch the genealogy interview is designed to surface, and it cross-checks against the rising theater_ratio (0.55 at interval end). Full piton conversion is nonetheless blocked, because the arrangement's core was not abandoned but absorbed: the violence monopoly and the professional definition of honor migrated into general state function, where they remain load-bearing. The mandatrophy framing prevents two symmetrical errors: reading the whole arrangement as pure extraction (which erases the real pacification achieved and the genuine coordination problem solved) or as pure coordination (which erases the uncompensated victims — the hanged, imprisoned, and status-expropriated — and lets the transition's winners write its history).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_revival_test,
    'Would dueling have revived where enforcement lapsed, if legitimacy had truly persisted as the drop reading holds? The composite reading stands or falls on this counterfactual.',
    'Geographic and temporal natural experiments: Britain after prosecution effectively ceased (mid-century) shows extinction without revival; French jury nullification periods show continued practice under formal prohibition; the fin-de-siecle French and German revival shows the cost mechanism reactivating exactly where contraction had lagged. Systematic comparison of enforcement-lapse episodes against practice trajectories resolves the sufficiency question.',
    'If any jurisdiction shows durable revival after enforcement lapse, the drop mechanism regains sufficiency and the composite weakens toward drop_reading; if lapse-without-revival is universal (as the current record suggests), the contraction edge is confirmed and the composite holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_revival_test, empirical, 'Whether the decline''s durability requires the redefinition mechanism or costs alone suffice.').

omega_variable(
    reading_decomposition_location,
    'This constraint is one reading of kernel honor_violence_legitimacy (reading: composite_reading). Where exactly do the sibling readings disagree with it — can the two mechanisms'' causal weights be separated at all, or is the decomposition itself the contested move?',
    'Sibling stories honor_violence_legitimacy__drop_reading and honor_violence_legitimacy__contraction_reading author their own victim sets and epsilon values; cross-reading comparison of computed classifications locates the disagreement. If the siblings'' epsilons bracket the composite''s, the decomposition is doing work; if all three converge, the kernel may be better treated as a single constraint.',
    'A sibling reading adopted instead of this one changes the victim set (drop: prosecuted duelists only; contraction: status-expropriated traditionalists only) and shifts epsilon toward the coercive or the identity-expropriation profile respectively; the composite''s tangled_rope claim depends on both victim sets being real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_decomposition_location, conceptual, 'Committer structure: which reading of the honor-violence kernel is instantiated, and what the siblings would change.').

omega_variable(
    status_expropriation_classification,
    'Does the uncompensated devaluation of the traditionalist gentry''s honor-capital count as extraction (supporting the tangled_rope claim and the second victim set), or as ordinary cultural change that no arrangement can be charged with?',
    'Compare analogous status transitions: the demise of chivalric ideals, the devaluation of religious vocations, the obsolescence of cavalry prestige. Where carriers received compensating repositioning (pensions, translated status, honored retirement), the transition reads as managed change; where capital simply expired, it reads as expropriation.',
    'If ordinary cultural change, epsilon drops materially, the contraction victim set thins, and the classification trends toward rope; if expropriation, the tangled_rope claim holds with both victim sets intact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(status_expropriation_classification, conceptual, 'Whether meaning-level losses inflicted by redefinition are extraction or background cultural weather.').

omega_variable(
    identity_lock_vs_material_interest,
    'How much of the traditionalist gentry''s attachment to the old code was identity fusion versus material interest — dueling as a gatekeeping device that kept arrivistes out of elite standing?',
    'Trace behavior of gentry subgroups whose material position did not depend on exclusion (impoverished cadet branches, colonial officers outside metropolitan competition): if they clung to the code as fervently as the gatekept core, identity dominates; if they adapted quickly, material interest dominates.',
    'If material interest dominates, the contraction victim set is smaller and more strategic than identity-locked, effective extraction for that seat falls, and the composite''s second mechanism looks more like ordinary interest-group politics than meaning-expropriation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_material_interest, empirical, 'Composition of the traditionalist attachment: fused identity versus exclusionary rent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hvl_composite_tr_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hvl_composite_tr_t5, honor_violence_legitimacy__composite_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(hvl_composite_tr_t10, honor_violence_legitimacy__composite_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(hvl_composite_tr_t15, honor_violence_legitimacy__composite_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement(hvl_composite_tr_t20, honor_violence_legitimacy__composite_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(hvl_composite_tr_t25, honor_violence_legitimacy__composite_reading, theater_ratio, 25, 0.47).
narrative_ontology:measurement(hvl_composite_tr_t30, honor_violence_legitimacy__composite_reading, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(hvl_composite_be_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(hvl_composite_be_t5, honor_violence_legitimacy__composite_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(hvl_composite_be_t10, honor_violence_legitimacy__composite_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(hvl_composite_be_t15, honor_violence_legitimacy__composite_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(hvl_composite_be_t20, honor_violence_legitimacy__composite_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(hvl_composite_be_t25, honor_violence_legitimacy__composite_reading, base_extractiveness, 25, 0.51).
narrative_ontology:measurement(hvl_composite_be_t30, honor_violence_legitimacy__composite_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hvl_composite_su_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(hvl_composite_su_t5, honor_violence_legitimacy__composite_reading, suppression_requirement, 5, 0.74).
narrative_ontology:measurement(hvl_composite_su_t10, honor_violence_legitimacy__composite_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(hvl_composite_su_t15, honor_violence_legitimacy__composite_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(hvl_composite_su_t20, honor_violence_legitimacy__composite_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(hvl_composite_su_t25, honor_violence_legitimacy__composite_reading, suppression_requirement, 25, 0.44).
narrative_ontology:measurement(hvl_composite_su_t30, honor_violence_legitimacy__composite_reading, suppression_requirement, 30, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the decline of dueling' decomposes into three readings of the kernel honor_violence_legitimacy. drop_reading authors epsilon for a cost-suppression arrangement over intact legitimacy (high suppression, coercive victim set: prosecuted duelists). contraction_reading authors epsilon for a conceptual-redefinition arrangement (low overt suppression, identity victim set: status-expropriated traditionalists). This composite_reading authors epsilon for the joint arrangement: both mechanisms operating simultaneously, with the added claim that the contraction edge makes the drop mechanism insufficient alone. The composite's epsilon (0.58) sits between its siblings' because it blends a coercive profile with a meaning-expropriation profile; the upstream sibling (drop_reading, better archival grounding in prosecution records) influences the downstream one (contraction_reading, more interpretively contested), and the composite links both because it is the only reading that can state why each alone fails.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, powerless, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
