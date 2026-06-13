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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: honor_violence_legitimacy__composite_reading
 *   human_readable: Honor-Violence Legitimacy (Composite Decline Mechanism)
 *   domain: social/legal/historical
 *
 * SUMMARY:
 *   Between the 17th and 19th centuries, honor-based dueling as a legitimate
 *   status mechanism declined across Western Europe. This reading
 *   instantiates the composite decline thesis: two distinct mechanisms
 *   operated simultaneously and reinforced each other. The DROP mechanism:
 *   nation-states criminalized dueling, increasing legal costs (imprisonment,
 *   exile), making participation expensive and risky. The CONTRACTION
 *   mechanism: intellectual and cultural redefinition of honor from
 *   warrior/martial virtue to intellectual/restrained virtue made dueling
 *   ideologically illegitimate — duelers came to be seen as barbaric rather
 *   than noble. These mechanisms targeted different victim sets (duelers
 *   faced legal costs; advocates of violence-based honor faced legitimacy
 *   collapse) and had different ε profiles in isolation. Together, they
 *   created a regime where both the legal structure and the cultural meaning
 *   of the system changed, making return to the old form structurally and
 *   conceptually difficult. The composite reading asserts that neither
 *   mechanism alone was sufficient to explain the persistence of the decline
 *   — understanding the system requires analyzing how external costs (drop)
 *   and redefinition (contraction) reinforced each other.
 *
 * KEY AGENTS:
 *   - aristocratic_warrior_class: agenda-setters who defined honor, collected status from the system; locked into identity-based participation (warrior as honorable). Faced simultaneous loss of autonomy (drop: state monopoly on violence) and delegitimacy (contraction: honor redefined without them).
 *   - dueling_participants: constrained by obligation, benefited from victory (status recovery), faced rising legal and social costs. The identity-lock meant exit was social death even before legal prohibition; the contraction made identity-locked participation shameful rather than noble.
 *   - honor_bound_servants: minor actors in the system, trapped by their dependence on principals; as the system declined, their participation became criminalized without the social benefit majors could claim.
 *   - women_in_honor_systems: powerless, trapped; the drop mechanism removed violence-based protection but not dependency; the contraction mechanism removed their position in the honor economy entirely without offering alternative security.
 *   - emerging_nation_state: operated the drop mechanism as monopoly enforcement; supported or tolerated the contraction mechanism as delegitimizing competition for status authority.
 *   - reformist_honor_theorists: excluded from aristocratic honor-adjudication but drivers of the contraction mechanism through intellectual and cultural work; their arguments about honor's true meaning shaped the kernel's own reinterpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__composite_reading, 0.67).
domain_priors:suppression_score(honor_violence_legitimacy__composite_reading, 0.71).
domain_priors:theater_ratio(honor_violence_legitimacy__composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(honor_violence_legitimacy__composite_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_violence_legitimacy__composite_reading, "Honor-Violence Legitimacy (Composite Decline Mechanism)").
narrative_ontology:topic_domain(honor_violence_legitimacy__composite_reading, "social/legal/historical").

domain_priors:requires_active_enforcement(honor_violence_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__composite_reading, '11c48614-7051-47e5-81c1-8525e9018290').
narrative_ontology:cs_kernel_codification('11c48614-7051-47e5-81c1-8525e9018290', distributed).
narrative_ontology:cs_authority_grounding('11c48614-7051-47e5-81c1-8525e9018290', extraction).
narrative_ontology:cs_interpretation_layer_present('11c48614-7051-47e5-81c1-8525e9018290').
narrative_ontology:cs_reading_relation('11c48614-7051-47e5-81c1-8525e9018290', honor_violence_legitimacy__drop_reading, influences).
narrative_ontology:cs_reading_relation('11c48614-7051-47e5-81c1-8525e9018290', honor_violence_legitimacy__contraction_reading, influences).
narrative_ontology:cs_axiom('11c48614-7051-47e5-81c1-8525e9018290', foundational, honor_requires_autonomous_adjudication).
narrative_ontology:cs_axiom_status(honor_requires_autonomous_adjudication, overridden).
narrative_ontology:cs_axiom_grounding('11c48614-7051-47e5-81c1-8525e9018290', honor_requires_autonomous_adjudication, deontological).
narrative_ontology:cs_axiom('11c48614-7051-47e5-81c1-8525e9018290', foundational, violence_is_legitimate_honor_expression).
narrative_ontology:cs_axiom_status(violence_is_legitimate_honor_expression, overridden).
narrative_ontology:cs_axiom_grounding('11c48614-7051-47e5-81c1-8525e9018290', violence_is_legitimate_honor_expression, empirically_contingent).
narrative_ontology:cs_reference_frame('11c48614-7051-47e5-81c1-8525e9018290', aristocratic_honor_autonomous).
narrative_ontology:cs_drift_state('11c48614-7051-47e5-81c1-8525e9018290', post_enlightenment_state_consolidation, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('11c48614-7051-47e5-81c1-8525e9018290', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, aristocratic_warrior_class).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, state_monopoly_on_violence).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, dueling_participants).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, honor_bound_servants).
narrative_ontology:constraint_victim(honor_violence_legitimacy__composite_reading, women_in_honor_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__composite_reading, dueling_participants).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, aristocratic_moral_autonomy).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__composite_reading, honor_as_social_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the honor code and interprets legitimate grievance-response within it. Claims honor is an autonomous moral domain where aristocrats, not states, adjudicate worth and satisfaction. Collects social status and relative rank from the system's operation. As the system declines, faces simultaneous pressure: external state criminalization of dueling (drop mechanism) and ideological redefinition of honor to exclude violence (contraction mechanism). The identity-lock is professional: warrior identity is fused with honor-based status adjudication.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, aristocratic_warrior_class, agenda_setter,
    powerful, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, aristocratic_warrior_class, beneficiary).

% Aristocrats and gentry who are obligated to defend their honor through dueling when challenged. As participants, they may recover social standing through victory, but the obligation to fight is binding — refusal means social death and loss of marriageability, position, and family standing. Under the drop mechanism, face increasing legal penalty (imprisonment, exile) for dueling. Under the contraction mechanism, face redefinition of honor itself such that participation looks cowardly or barbaric rather than noble. The identity-lock is personal: honor is fused with self-concept and social position; exit means abandoning social identity entirely.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, dueling_participants, payer,
    powerful, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_violence_legitimacy__composite_reading, dueling_participants, beneficiary).

% Retainers, subordinates, and minor gentry whose honor (and thus their masters') is defended by participation in feuds and honor-revenge cycles. They may not initiate duels but are obligated to support their superior's honor claims through violence and testimony. As the system declines, face increasing legal jeopardy for participation (assault, conspiracy charges) and loss of institutional protection (masters no longer shield them). The contraction mechanism makes their participation appear primitive and shameful rather than loyal and necessary.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, honor_bound_servants, payer,
    moderate, biographical, constrained, national).

% Women whose sexuality, family alliance, and reputation are the objects around which honor violence revolves. A woman's alleged infidelity, broken betrothal, or family insult triggers male honor-defense violence on her behalf or against her. Under the drop mechanism, women may gain protection from legal prohibition of dueling — but only if male relatives accept the new legal framework, which many do not. Under the contraction mechanism, if honor is redefined to exclude violence, women's reputation and sexuality cease to be legitimate triggers for male violence — but also cease to be legitimate domains of male protection and agency. The trap is that both mechanisms devalue women's position in the honor economy while removing the structural dependency without replacing it with alternative status or security.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, women_in_honor_systems, payer,
    powerless, biographical, trapped, national).

% Monopolizes legitimate violence and criminalizes private honor-based violence as a threat to state authority and legal predictability. Operates the drop mechanism: dueling becomes illegal, carrying criminal penalties. Simultaneously supports or tolerates the contraction mechanism by promoting new definitions of gentlemen (as law-abiding, rational) versus the old definition (as honor-autonomous, violent). The state's position is agenda-setting but not fully capturing the extraction — the state substitutes state-legitimized status hierarchies for aristocratic honor, gaining monopoly but not collecting direct rents.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, emerging_nation_state, agenda_setter,
    institutional, generational, analytical, national).

% Intellectuals and moral reformers who argue that true honor is incompatible with violence, that courage shows in law-abiding restraint rather than swordplay, and that honor should attach to intellectual achievement, family legacy, and economic standing rather than martial prowess. They are excluded from the warrior class's honor-adjudication process but their redefinitional arguments shape the cultural logic that makes dueling increasingly illegitimate. They serve as the cultural carrier of the contraction mechanism.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, reformist_honor_theorists, excluded,
    organized, generational, constrained, national).

% External observers (historians, comparative legal scholars, anthropologists) who document the decline and identify both mechanisms operating: the drop (legal prohibition, increasing costs) and the contraction (redefinition of honor itself). Their analysis is structurally external to the constraint system but becomes part of the historical record that allows decomposition of the mechanisms.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__composite_reading, colonial_or_comparative_observers, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__composite_reading, emerging_nation_state).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor-violence system coordinates status adjudication within an aristocratic hierarchy: grievances that the formal legal system cannot address (insults, challenges to rank, family honor) are resolved through ritualized combat. The outcome establishes relative standing without requiring third-party judgment. The system also coordinates deterrence: the threat of honor-defense violence constrains behavior (preventing insults, enforcing alliance obligations, maintaining family reputation).
% TRANSFER_FUNCTION: Transfers social status, rank, and marriageability from the challenged to the victor in duels; transfers risk of death and legal penalty from the aristocratic class to duelers and honor-bound servants. The system also transfers authority to adjudicate legitimate grievance from the state to the warrior class. As the system declines, it transfers victims (those obligated to fight become criminalized) without transferring the benefits (those who defend honor through violence lose status rather than gain it).
% ABSENT_VOICES: Excluded from the honor-adjudication process: reformist intellectuals (who argue for redefinition); women (whose honor is the object of violence, not the agent); and the emerging state bureaucracy (which claims monopoly on violence adjudication). The reformist excluded voices are particularly important — they drive the contraction mechanism from outside the warrior class's own logic.
% DISAPPEARANCE_RATIONALE: If honor-violence legitimacy vanished, status adjudication would consolidate entirely into the state legal system and market mechanisms (wealth, education, professional achievement). The aristocratic warrior class would lose the autonomous space to define their own rank. Dueling participants would lose the obligation (and the status opportunity) to fight. Women's honor would cease to be a subject of male violence-based defense. The entire social economy of honor would reorganize around non-violent markers (property, education, state office, family lineage in legal terms).
% FOUNDING_PROBLEM: In pre-state or weak-state societies with decentralized authority, honor-based status systems solved the problem of grievance adjudication and deterrence without requiring a central legal apparatus. A gentleman's honor was his word and his willingness to defend it; insults could be answered, alliances enforced, and rank established through combat. The system emerged when formal legal recourse was unavailable or illegitimate for aristocratic disputes.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem was substantially solved or made irrelevant by the rise of the nation-state and centralized legal systems (c. 17th–19th centuries, depending on jurisdiction). Historians documenting the decline (Kiernan, Esenstein, Billacois) and legal scholars studying the transition from private to public justice (Foucault, Reinhardt) all attest that the state's capacity to adjudicate grievances made honor-violence unnecessary for its original coordination function. However, the warrior class and some reformist theorists initially resisted this analysis, claiming honor remained a live problem the state could not address — this claim was contested and ultimately lost as new definitions of honor emerged.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__composite_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__composite_reading, 'none', 1).

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
 *   Extractiveness rises from 0.42 to 0.67 over the interval, with the steepest climb from t=0 to t=20 (when both mechanisms are actively operating and reinforcing), then plateaus as the old system moves toward complete collapse. Theater rises from 0.08 to 0.42, indicating that dueling becomes increasingly a residual performance — rare in actual practice but symbolically maintained as a test of legitimacy. Suppression rises sharply from 0.35 to 0.71, tracking the intensification of enforcement (legal penalties rise, cultural shaming increases). Accessibility collapse rises as alternatives to honor-violence disappear: individual level reaches 0.85 (by t=40, exit from honor-participation becomes nearly impossible without total social dissolution); structural level only reaches 0.68 (the system itself becomes optional for the state, which no longer needs aristocratic honor to establish order). The grid captures the uneven pressure across levels: duelers and the warrior class experience maximum suppression (0.78 individual, 0.76 organizational); the structural level experiences lower suppression (0.64) because the state can tolerate persistence of the old form as long as its monopoly on violence is protected. Resistance falls over the interval (0.68 to 0.38 at individual level) as the ideology of honor-violence erodes and legal costs accumulate. This pattern shows how overdetermination works: if either mechanism operated alone, the constraint might persist or transform; together, they create a convergent dynamic toward collapse.
 *
 * PERSPECTIVAL GAP:
 *   The aristocratic warrior class and duelers should compute as experiencing very different types from the state's perspective. From the warrior class seat (agenda-setter, powerful, identity-locked), the constraint appears as an erosion of autonomy — the drop mechanism is pure external coercion and the contraction mechanism is a delegitimization conspiracy. From the state seat (institutional, analytical), the constraint appears as a coordination problem being solved — monopolizing violence is necessary, and delegitimizing private honor-adjudication is a coordination gain. From the servant and women seats (powerless/moderate, trapped/constrained), the system appears as pure extraction with no offsetting benefit (servants lose the principal's protection as dueling becomes illegal; women lose both protection and status without gain). The engine should compute tangled-rope type from the structural data (beneficiary class + victims + enforcement), while the warrior-class seat might compute snare (experiencing only loss) or piton (seeing only theater). This divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   The aristocratic warrior class is the structural beneficiary (collects status, sets rules, defines honor) but becomes trapped as both mechanisms operate — d starts near 0.15 (beneficiary end) but spirals toward 0.65 (near target end) as the system declines and benefits evaporate. Dueling participants start at d~0.4 (symmetric: benefits from status, costs from obligation) and move to d~0.8 (target: facing legal costs, social shaming, identity invalidation). Honor-bound servants start at d~0.7 (target: obligated, penalized for participation) and move to d~0.85 (target: losing the institutional protection that made participation meaningful). Women in the system start at d~0.8 (trapped, no benefit) and remain high as both mechanisms operate but neither offers them security or alternative status. The state begins as a passive backdrop (not yet codified as enforcer) and becomes d~0.2 (beneficiary: gains monopoly, order, legitimacy) from its perspective. The grid captures this: individual-level suppression rises steeply (affecting duelers and servants who face legal jeopardy), organizational suppression rises but less steeply (the warrior class can coordinate resistance for some time), structural suppression rises slowest (the state's commitment to monopoly is bedrock, not dependent on individual duelers complying).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved via the R5 genealogy: the founding problem (grievance adjudication and status allocation without state apparatus) is DEAD — the state's centralized legal system made it obsolete by the 18th century. Yet the constraint persists as theater (0.42 at end of interval) maintained by inertia, identity commitment, and ritual. This is the piton signal, except the constraint also shows active enforcement (suppression 0.71) and substantial extraction (0.67), which blocks piton classification. The resolution: the constraint is NOT a piton because it is not a cost-neutral theater. The warrior class and duelers continue to extract status/identity value from participation (even as it becomes symbolic), the state extracts legitimacy from monopoly enforcement, and servants/women face real costs. The theater ratio rise (0.08 to 0.42) shows the system moving toward piton-like operation (performance replacing function), but the extraction and suppression metrics show it remains Tangled Rope — coordination (status adjudication within aristocracy, though neutered) plus asymmetric extraction (duelers pay legal/social costs, servants pay employment costs, women pay security/status costs) plus active enforcement (state prosecution, social shaming from reformists). The mandatrophy is resolved by recognizing that the constraint's founding problem is dead but the constraint persists as an extractive ritual, not as a utility. The decline is overdetermined because removal requires both law change (drop mechanism) and identity/ideology change (contraction mechanism); either alone would allow the system to adapt and persist in modified form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_attribution_ambiguity,
    'Did the honor-violence system decline primarily because external legal costs made participation prohibitive (drop mechanism) or because honor itself was redefined to exclude violence (contraction mechanism), or was the decline genuinely overdetermined by both operating simultaneously?',
    'Comparative analysis across jurisdictions with different legal enforcement profiles vs. different ideological reformism trajectories. If jurisdictions with weak law enforcement still abandoned dueling as honor-redefinition spread, contraction is primary; if jurisdictions with strong reformist intellectual movements but weak enforcement maintained dueling, drop is primary. If the pattern varies, overdetermination is confirmed.',
    'Single-mechanism attribution (drop OR contraction) suggests the constraint is Snare (external force) or Scaffold (conceptual transition); overdetermination suggests Tangled Rope (two structural asymmetries reinforcing each other). The claimed type depends on whether both mechanisms were necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_attribution_ambiguity, empirical, 'Which mechanism(s) were necessary and sufficient for the decline.').

omega_variable(
    identity_lock_persistence,
    'For aristocratic duelers under the drop mechanism, was the suppression primarily structural (legal enforcement made dueling costly) or internalized (duelers came to believe honor-violence was wrong)?',
    'Analysis of elite writing, court testimony, and institutional memory in the decades after legal prohibition. If duelers continued to assert honor''s legitimacy despite legal penalties, suppression is structural; if they internalized the redefinition and claimed they had been barbaric, suppression is internalized. Mixed evidence (some internalized, some holdout) indicates a regime that functions through both mechanisms.',
    'If suppression is primarily internalized, the identity-lock persists after the constraint is removed — duelers carry the redefinition with them. If structural, the lock can be reset by changing the rules (a potential jailbreak scenario). The measurement of suppression at 0.71 is agnostic about this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether suppression is internalized or structural in the identity-locked seats.').

omega_variable(
    contraction_mechanism_genealogy,
    'Was the redefinition of honor from warrior/violent to intellectual/restrained an autonomous intellectual development, a top-down state imposition, or a synthesis driven by reformist intellectuals who were excluded from but shaped the honor system?',
    'Genealogical analysis of honor-theory writings, state propaganda, and cultural transmission. Tracing the actual source of new honor definitions (philosophical schools, state education, professional associations, religious movements) shows which parties drove the contraction.',
    'If the state imposed the redefinition, the constraint carries a power axis favoring institutional seats; if reformists drove it, it reveals how excluded voices can reshape the kernel of a system they don''t directly control. This affects interpretation of whether the constraint''s decline was natural or enforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contraction_mechanism_genealogy, empirical, 'The genealogy of the honor redefinition mechanism.').

omega_variable(
    kernel_overdetermination_vs_composite_reading,
    'Is this a single constraint (honor-violence legitimacy) with two decline mechanisms operating, or are the drop and contraction mechanisms each separate constraints that should be decomposed per the ε-invariance principle?',
    'Counterfactual test: in a high-enforcement, low-reformism scenario (drop without contraction), would the system persist as a live option? In a low-enforcement, high-reformism scenario (contraction without drop), would redefinition alone end legitimacy? If both transitions reach the same terminal state (honor-violence ends) through different paths, the composite reading is appropriate.',
    'Single constraint (composite reading): the claim is Tangled Rope, extraction is 0.67, both mechanisms are co-responsible. Two constraints (decomposition): drop_reading as Snare, contraction_reading as Scaffold, this file becomes a network report instead of a single constraint story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_overdetermination_vs_composite_reading, conceptual, 'Whether the composite reading is ε-invariant or whether two separate constraint stories should be authored.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__composite_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_violence_legitimacy__composite_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(hono_tr_t0, observed).
narrative_ontology:measurement(hono_tr_t5, honor_violence_legitimacy__composite_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(hono_tr_t5, observed).
narrative_ontology:measurement(hono_tr_t10, honor_violence_legitimacy__composite_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(hono_tr_t10, observed).
narrative_ontology:measurement(hono_tr_t15, honor_violence_legitimacy__composite_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(hono_tr_t15, observed).
narrative_ontology:measurement(hono_tr_t20, honor_violence_legitimacy__composite_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(hono_tr_t20, observed).
narrative_ontology:measurement(hono_tr_t25, honor_violence_legitimacy__composite_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(hono_tr_t25, observed).
narrative_ontology:measurement(hono_tr_t30, honor_violence_legitimacy__composite_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(hono_tr_t30, observed).
narrative_ontology:measurement(hono_tr_t40, honor_violence_legitimacy__composite_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(hono_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_violence_legitimacy__composite_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(hono_be_t0, observed).
narrative_ontology:measurement(hono_be_t5, honor_violence_legitimacy__composite_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(hono_be_t5, observed).
narrative_ontology:measurement(hono_be_t10, honor_violence_legitimacy__composite_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement_basis(hono_be_t10, observed).
narrative_ontology:measurement(hono_be_t15, honor_violence_legitimacy__composite_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(hono_be_t15, observed).
narrative_ontology:measurement(hono_be_t20, honor_violence_legitimacy__composite_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(hono_be_t20, observed).
narrative_ontology:measurement(hono_be_t25, honor_violence_legitimacy__composite_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(hono_be_t25, observed).
narrative_ontology:measurement(hono_be_t30, honor_violence_legitimacy__composite_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(hono_be_t30, observed).
narrative_ontology:measurement(hono_be_t40, honor_violence_legitimacy__composite_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(hono_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_violence_legitimacy__composite_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(hono_su_t0, observed).
narrative_ontology:measurement(hono_su_t5, honor_violence_legitimacy__composite_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement_basis(hono_su_t5, observed).
narrative_ontology:measurement(hono_su_t10, honor_violence_legitimacy__composite_reading, suppression_requirement, 10, 0.51).
narrative_ontology:measurement_basis(hono_su_t10, observed).
narrative_ontology:measurement(hono_su_t15, honor_violence_legitimacy__composite_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(hono_su_t15, observed).
narrative_ontology:measurement(hono_su_t20, honor_violence_legitimacy__composite_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(hono_su_t20, observed).
narrative_ontology:measurement(hono_su_t25, honor_violence_legitimacy__composite_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(hono_su_t25, observed).
narrative_ontology:measurement(hono_su_t30, honor_violence_legitimacy__composite_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(hono_su_t30, observed).
narrative_ontology:measurement(hono_su_t40, honor_violence_legitimacy__composite_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(hono_su_t40, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(hono_grid_01, honor_violence_legitimacy__composite_reading, accessibility_collapse(class), 0, 0.65).
narrative_ontology:measurement(hono_grid_02, honor_violence_legitimacy__composite_reading, accessibility_collapse(class), 40, 0.82).
narrative_ontology:measurement(hono_grid_03, honor_violence_legitimacy__composite_reading, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(hono_grid_04, honor_violence_legitimacy__composite_reading, accessibility_collapse(individual), 40, 0.85).
narrative_ontology:measurement(hono_grid_05, honor_violence_legitimacy__composite_reading, accessibility_collapse(organizational), 0, 0.72).
narrative_ontology:measurement(hono_grid_06, honor_violence_legitimacy__composite_reading, accessibility_collapse(organizational), 40, 0.88).
narrative_ontology:measurement(hono_grid_07, honor_violence_legitimacy__composite_reading, accessibility_collapse(structural), 0, 0.48).
narrative_ontology:measurement(hono_grid_08, honor_violence_legitimacy__composite_reading, accessibility_collapse(structural), 40, 0.68).
narrative_ontology:measurement(hono_grid_09, honor_violence_legitimacy__composite_reading, resistance(class), 0, 0.75).
narrative_ontology:measurement(hono_grid_10, honor_violence_legitimacy__composite_reading, resistance(class), 40, 0.35).
narrative_ontology:measurement(hono_grid_11, honor_violence_legitimacy__composite_reading, resistance(individual), 0, 0.68).
narrative_ontology:measurement(hono_grid_12, honor_violence_legitimacy__composite_reading, resistance(individual), 40, 0.38).
narrative_ontology:measurement(hono_grid_13, honor_violence_legitimacy__composite_reading, resistance(organizational), 0, 0.72).
narrative_ontology:measurement(hono_grid_14, honor_violence_legitimacy__composite_reading, resistance(organizational), 40, 0.42).
narrative_ontology:measurement(hono_grid_15, honor_violence_legitimacy__composite_reading, resistance(structural), 0, 0.62).
narrative_ontology:measurement(hono_grid_16, honor_violence_legitimacy__composite_reading, resistance(structural), 40, 0.48).
narrative_ontology:measurement(hono_grid_17, honor_violence_legitimacy__composite_reading, stakes_inflation(class), 0, 0.35).
narrative_ontology:measurement(hono_grid_18, honor_violence_legitimacy__composite_reading, stakes_inflation(class), 40, 0.65).
narrative_ontology:measurement(hono_grid_19, honor_violence_legitimacy__composite_reading, stakes_inflation(individual), 0, 0.38).
narrative_ontology:measurement(hono_grid_20, honor_violence_legitimacy__composite_reading, stakes_inflation(individual), 40, 0.72).
narrative_ontology:measurement(hono_grid_21, honor_violence_legitimacy__composite_reading, stakes_inflation(organizational), 0, 0.42).
narrative_ontology:measurement(hono_grid_22, honor_violence_legitimacy__composite_reading, stakes_inflation(organizational), 40, 0.68).
narrative_ontology:measurement(hono_grid_23, honor_violence_legitimacy__composite_reading, stakes_inflation(structural), 0, 0.28).
narrative_ontology:measurement(hono_grid_24, honor_violence_legitimacy__composite_reading, stakes_inflation(structural), 40, 0.52).
narrative_ontology:measurement(hono_grid_25, honor_violence_legitimacy__composite_reading, suppression(class), 0, 0.32).
narrative_ontology:measurement(hono_grid_26, honor_violence_legitimacy__composite_reading, suppression(class), 40, 0.68).
narrative_ontology:measurement(hono_grid_27, honor_violence_legitimacy__composite_reading, suppression(individual), 0, 0.28).
narrative_ontology:measurement(hono_grid_28, honor_violence_legitimacy__composite_reading, suppression(individual), 40, 0.78).
narrative_ontology:measurement(hono_grid_29, honor_violence_legitimacy__composite_reading, suppression(organizational), 0, 0.38).
narrative_ontology:measurement(hono_grid_30, honor_violence_legitimacy__composite_reading, suppression(organizational), 40, 0.76).
narrative_ontology:measurement(hono_grid_31, honor_violence_legitimacy__composite_reading, suppression(structural), 0, 0.42).
narrative_ontology:measurement(hono_grid_32, honor_violence_legitimacy__composite_reading, suppression(structural), 40, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__composite_reading, 0.12).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__composite_reading, honor_violence_legitimacy__contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one decomposed reading of a contested kernel (honor_violence_legitimacy). The drop_reading isolates the external-cost mechanism (legal prohibition); the contraction_reading isolates the redefinition mechanism (cultural-ideological shift). This composite_reading asserts that BOTH mechanisms were necessary and operating simultaneously to explain the system's decline. The three stories form a constraint family linked by network.affects_constraints. Each has its own ε and its own stakeholder directionality profile. The kernel is the claim about what legitimizes honor-violence; the three readings provide different answers grounded in different structural mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_violence_legitimacy__composite_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
