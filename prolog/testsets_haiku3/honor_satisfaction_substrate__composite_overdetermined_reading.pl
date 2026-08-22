% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate_composite_overdetermined_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor Satisfaction Substrate (Composite Overdetermined Decline Reading)
 *   domain: cultural/legal/historical
 *
 * SUMMARY:
 *   This reading instantiates the 'composite overdetermined decline'
 *   interpretation of honor-code dueling's historical disappearance. The
 *   constraint describes dueling's operation as a tangled rope—simultaneously
 *   coordinating elite male status through combat-based satisfaction AND
 *   extracting lethal costs from challenged men and their families. The
 *   reading asserts that dueling's decline was caused by two non-independent
 *   mechanisms operating in causal entanglement: exogenous suppression (state
 *   legal prohibition, institutional barriers, rising criminal penalties) AND
 *   endogenous delegitimation (the honor substrate itself eroded as dignity
 *   frameworks gained cultural and institutional authority). Neither
 *   mechanism alone accounts for the historical record; both operated such
 *   that legal pressure accelerated honor-code questioning, which weakened
 *   community enforcement, which lowered resistance to further legal
 *   prohibition. The claim/metric divergence is intentional: exogenous and
 *   endogenous mechanisms are theoretically distinct but empirically
 *   intertwined, which the metrics capture (rising suppression requirement,
 *   rising theater ratio indicating performative vs. real-function
 *   degradation, rising resistance as the substrate weakens). The measurement
 *   series show extractiveness peaking at mid-interval (1800, value 0.65)
 *   then plateauing as the constraint approached functional collapse—a
 *   signature pattern when two decay mechanisms reach critical phase.
 *
 * KEY AGENTS:
 *   - honor_code_bearers: Gentlemen in military/aristocratic/professional networks who maintain and police the code
 *   - challenged_gentlemen: Direct targets bearing combat demands and identity-lock costs
 *   - families_of_dueling_casualties: Powerless payers bearing mortality and morbidity costs
 *   - legal_enforcement_apparatus: State institutions progressively criminalizing dueling
 *   - emergent_dignity_framework_advocates: Thinkers and reformers delegitimizing honor combat as substrate
 *   - code_adherent_communities: Military officers and professional societies at the pivot
 *   - analytical_observer: The reading's own seat asserting causal entanglement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.62).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.71).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor Satisfaction Substrate (Composite Overdetermined Decline Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "cultural/legal/historical").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, 'f2427d05-ef9c-48b2-b340-f69a15d51e19').
narrative_ontology:cs_kernel_codification('f2427d05-ef9c-48b2-b340-f69a15d51e19', distributed).
narrative_ontology:cs_authority_grounding('f2427d05-ef9c-48b2-b340-f69a15d51e19', extraction).
narrative_ontology:cs_interpretation_layer_present('f2427d05-ef9c-48b2-b340-f69a15d51e19').
narrative_ontology:cs_reading_relation('f2427d05-ef9c-48b2-b340-f69a15d51e19', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('f2427d05-ef9c-48b2-b340-f69a15d51e19', honor_satisfaction_substrate__cultural_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('f2427d05-ef9c-48b2-b340-f69a15d51e19', foundational, decline_mechanistically_overdetermined).
narrative_ontology:cs_axiom_status(decline_mechanistically_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('f2427d05-ef9c-48b2-b340-f69a15d51e19', decline_mechanistically_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('f2427d05-ef9c-48b2-b340-f69a15d51e19', foundational, exogenous_endogenous_causal_entanglement).
narrative_ontology:cs_axiom_status(exogenous_endogenous_causal_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('f2427d05-ef9c-48b2-b340-f69a15d51e19', exogenous_endogenous_causal_entanglement, empirically_contingent).
narrative_ontology:cs_axiom('f2427d05-ef9c-48b2-b340-f69a15d51e19', secondary, dignity_framework_substrate_transformation).
narrative_ontology:cs_axiom_status(dignity_framework_substrate_transformation, holdable).
narrative_ontology:cs_axiom_grounding('f2427d05-ef9c-48b2-b340-f69a15d51e19', dignity_framework_substrate_transformation, empirically_contingent).
narrative_ontology:cs_reference_frame('f2427d05-ef9c-48b2-b340-f69a15d51e19', honor_code_community_adjudication).
narrative_ontology:cs_drift_state('f2427d05-ef9c-48b2-b340-f69a15d51e19', dignity_framework_ascendance_1850_1900, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f2427d05-ef9c-48b2-b340-f69a15d51e19', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, honor_code_community_membership).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, challenged_gentlemen).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, families_of_dueling_casualties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, honor_code_bearers).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, emergent_dignity_framework_advocates).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, honor_obligation_doctrine).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__composite_overdetermined_reading, masculinity_through_combat_vindication).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gentlemen embedded in military, aristocratic, and professional societies who maintain the honor code as the legitimating framework for their social position. They police the code's boundaries through informal pressure, reputation management, and the threat of ostracism. They benefit from the code's operation insofar as it produces recognizable social hierarchy and validates their competitive status within a bounded elite.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, honor_code_bearers, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, honor_code_bearers, beneficiary).

% Men in the honor code's target demographic who face direct demands for satisfaction through combat. They bear the immediate cost of dueling—physical injury, death risk, legal jeopardy—and the asymmetric burden of responding to challenges they cannot refuse without losing social standing, professional opportunity, and familial name. Their exit from the code is cognitively and relationally foreclosed: abandoning it means social death within their peer networks.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, challenged_gentlemen, payer,
    moderate, biographical, identity_locked, national).

% Bear the mortality and morbidity costs of dueling through loss, disability, economic disruption, and intergenerational trauma. They lack standing to refuse participation in the honor code framework and have no exit mechanism: their relatives' bodies are the constraint's substrate.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, families_of_dueling_casualties, payer,
    powerless, biographical, trapped, local).

% State machinery (legislatures, courts, police, military discipline) that progressively criminalized dueling, raised penalties for participation, and shifted enforcement authority from the honor code community to formal law. They impose exogenous suppression—legal prohibition, prosecution, imprisonment—that makes code compliance increasingly costly relative to legal compliance. Their enforcement is structural (legal barriers, institutional consequences) rather than purely coercive.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, legal_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Enlightenment and post-Enlightenment thinkers, reformers, and institutional actors who actively delegitimized honor-code combat as primitive, irrational, and incompatible with emerging norms of dignity grounded in personhood rather than reputation-through-combat. They offer an alternative legitimating substrate (dignity, rights, rational self-respect) that restructures what 'satisfaction' and 'standing' mean, transforming the foundational premises the honor code rests on.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, emergent_dignity_framework_advocates, beneficiary,
    powerful, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, emergent_dignity_framework_advocates, agenda_setter).

% Military officers, regional aristocracies, professional societies (law, medicine, journalism) that localized honor-code enforcement. They sit at the intersection: internally loyal to the code (their identity and peer networks depend on it), externally vulnerable to legal prohibition and cultural delegitimation. They witness the constraint dissolving through non-independent causal pathways—legal suppression and honor substrate transformation arriving simultaneously.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, code_adherent_communities, observer,
    organized, generational, constrained, national).

% The reading's own epistemic seat: holds that dueling's decline is overdetermined by simultaneous, causally entangled mechanisms (exogenous legal suppression AND endogenous honor-substrate transformation via dignity framework). Neither mechanism alone is sufficient; both operate with non-independent causal pathways—legal pressure accelerates and legitimizes honor-code questioning, which in turn weakens community enforcement and lowers resistance to legal prohibition.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__composite_overdetermined_reading, honor_code_bearers).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__composite_overdetermined_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code's primary coordination function: it solves the problem of status and reputation adjudication within elite male communities (military, aristocratic, professional) where institutional authority is diffuse or contested. Combat-based satisfaction provides a recognized, formalized mechanism through which claims to rank, sexual reputation, family name, and professional standing are adjudicated privately, without recourse to state courts.
% TRANSFER_FUNCTION: Moves willingness-to-risk, bodily integrity, life, and compliance-with-lethal-demand from challenged gentlemen and their families to the honor-code-bearing community. The transfer is not wealth but the assumption of combat obligation, injury risk, death risk, and legal jeopardy. Social standing and reputation validation flow to code-bearing gentlemen; mortality, morbidity, and emotional trauma flow to the vulnerable seats (families, challenged men who refuse and lose standing).
% ABSENT_VOICES: Voices structurally excluded: (1) Women, whose reproductive and emotional stakes in male dueling were high but who held no formal role in code adjudication or enforcement; (2) Clergy and moral philosophers whose authority competed with honor code (largely excluded until dignity framework gained institutional power, at which point they provided intellectual support); (3) Families and dependents of casualties, whose voices were never solicited in code maintenance decisions; (4) Merchants, industrial workers, and middle classes whose status pathways increasingly bypassed honor combat, creating structural silence about alternatives. The reading pairs this with 'excluded' stakeholder (rival_payment_networks analogue): dignity-framework advocates were initially excluded from honor-code community conversations, then became the dominant voice as they gained institutional (state, academic, church) authority.
% DISAPPEARANCE_RATIONALE: This reading asserts the dispute itself is instructive. If dueling disappeared completely: honor-code defenders say a vacuum in private status adjudication would remain, requiring either restoration of the code or expansion of state authority to fill it—and indeed, state authority DID expand (courts, credentialing, professional licensing). Dignity-framework advocates say what would rearrange is only the status-granting infrastructure, not the social order itself—dignity-based standing and formal institutional adjudication substitute smoothly, which also occurred. The reading's own verdict is that BOTH happened: the social order rearranged precisely because the two mechanisms (exogenous suppression + endogenous substrate transformation) operated non-independently, making the rearrangement neither inevitable (code could have persisted if only legal pressure applied, given strong community enforcement) nor smooth (dignity frameworks alone might not have sufficed if legal prohibition remained absent). The contested verdict reflects the reading's causal claim: no pure narrative captures the change.
% FOUNDING_PROBLEM: Elite communities lacked formalized, non-state mechanisms for status and reputation adjudication. Where institutional hierarchy was contested or decentralized (military ranks in dispute, aristocratic privilege challenged, professional prestige unformalized), honor code provided a recognized pathway for resolving claims to standing that state authority either could not or would not adjudicate.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested as LIVE up through the 1700s by: military historians (generals' memoirs document dueling as status mechanism in officer corps); legal historians (France's Richelieu consolidated state authority specifically by asserting judicial over honor-code adjudication, documented in court records and edict texts). The founding problem is attested as DEAD by 1850 by: legal historians and historical sociologists (state authority was consolidated; formal credentialing and professional licensing emerged; courts had authority to adjudicate honor claims); comparative regional analysis (countries with different legal regimes but similar institutional development all saw dueling decline). No corroboration from outside the benefiting parties (honor-code community) for the problem remaining live past 1800—which itself is signal. The reading's corroboration strategy is indirect: show that societies with different suppression regimes but similar dignity-frame adoption (US North vs. US South: both had legal prohibition, different code persistence due to dignity-frame regional variation) and societies with same legal regime but different institutional alternatives (early France with vs. without credentialing formalization) reveal that BOTH mechanisms matter and are non-independent.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, contested).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_substrate__composite_overdetermined_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.62 (interval end) to reflect the constraint's operation as a structured extraction mechanism (mortality, injury, legal jeopardy for payers; social standing gained by code-bearers). It is NOT authored lower (say 0.45) despite the constraint's near-functional collapse by 1900, because this reading measures extractiveness OF THE STANDING ARRANGEMENT AT THE READING'S CHOSEN MOMENT, not its residual institutional authority. Suppression rises from 0.35 (1600, mostly informal via reputation and ostracism) to 0.71 (1900, legal prohibition + institutional barriers + dignity-frame stigma). Theater ratio rises from 0.08 to 0.28: early dueling was almost entirely functional (real status adjudication); late dueling became increasingly performative—a demonstration that one still honored the code rather than a mechanism that meaningfully adjudicated status (dignity frameworks and state institutions already did that work). The leveled coercion grid captures the reading's core claim: at the INDIVIDUAL level, alternatives collapsed almost completely in 1600 (0.92 accessibility_collapse) because a gentleman's identity was constituted through honor-code membership; by 1900, alternatives had reopened (0.28) because dignity frameworks provided a culturally legitimated exit path. At the STRUCTURAL level, suppression rose dramatically (0.28 to 0.65) as law and institution building created enforceable barriers, but accessibility_collapse fell (0.78 to 0.22) because the system-level alternatives multiplied. This pattern—rising structural suppression + falling individual accessibility_collapse—is the grid signature of overdetermined decline: exogenous suppression making the code costly at all levels, while endogenous substrate transformation makes the code thinkable-alternate-to at the cultural level, creating the non-independent causal entanglement the reading asserts.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of honor-code-bearers (agenda_setters, identity_locked), the constraint persists as long as the community's identity is invested in it—their seat computes toward ROPE (genuine coordination that community members freely support, even under legal pressure). From the seat of challenged_gentlemen (payers, identity_locked), the constraint computes toward SNARE or TANGLED_ROPE (extraction maintained by identity lock and the threat of social death, with legal barriers piling on structural barriers). From the dignity-framework advocates' seat, the constraint is already PITON by 1900—the real function (status adjudication) has been replaced by dignity-based institutions and state authority, but the code persists theatrically because dismantling it requires active effort and its defenders retain organizational resources. The engine computes per-seat from the structural data (power, exit_options, directionality); the reading's claim is that all three computations are SIMULTANEOUSLY TRUE because the mechanism is one of overdetermined collapse, not one of class-uniform extraction or genuine coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Honor-code-bearers (institutional power, identity_locked exit) benefit from the code's operation—it validates their position and provides status rents they could not access through other mechanisms. Their directionality is low (around 0.1-0.25: beneficiary side). Challenged_gentlemen (moderate power, identity_locked exit) bear direct costs—combat demands, injury risk, legal jeopardy—while also holding investments in code participation (identity fusion, peer network embeddedness, professional credibility). Their directionality is elevated (around 0.75-0.85: substantial target positioning, despite some identity-investment that creates secondary benefit). Families_of_dueling_casualties (powerless, trapped exit) bear costs with no direct code participation—mortality and trauma—and zero say in the code's enforcement. Their directionality is at the target end (0.95+). The coercion grid's individual-level rise in resistance (0.22 to 0.68) reflects this: as dignity frameworks provide a culturally legitimated alternative identity substrate, the identity_locked exit begins to unfreeze—gentlemen gain cognitive permission to question the code without losing identity entirely. This is the endogenous delegitimation mechanism: the substrate itself transforms such that exit becomes identity-coherent, not just identity-suicidal. Exogenous suppression (legal rising from 0.28 to 0.65 structural level) piles cost onto that opening.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading explicitly addresses mandatrophy—the phenomenon where a constraint's founding problem dies but the constraint persists. The founding problem was elite status adjudication absent state authority. By 1800-1850, that problem was substantially solved: state authority had expanded, institutional credentialing emerged, professions formalized status-granting mechanisms. Yet dueling persisted into the early 20th century in some societies (Germany, Russia) and lingered vestigially in others (France, Italy, US South). The reading's mandatrophy claim is that the constraint persisted NOT because the founding problem remained live, but because the TWO decay mechanisms (exogenous suppression + endogenous substrate transformation) operated non-independently, creating path-dependent persistence. Legal prohibition alone would have been insufficient if the honor substrate remained culturally intact (as it partially did in German military society); substrate transformation alone would not have required legal prohibition if the community had abandoned the code voluntarily (as it roughly did in post-Enlightenment France). The causal entanglement meant that both mechanisms had to reach criticality before collapse—a slower, more contested process than either mechanism solo. Theater ratio captures this: 0.28 by 1900 means the constraint persists mostly as performance, no longer as real function, yet performance itself requires resources and credibility. Mandatrophy is resolved (or rather, rendered visible) by reading the founding_problem_status (dead by 1850) against disappearance_verdict (contested: does the world rearrange or stay the same?) and finding the reading's own seat asserting BOTH: the founding problem is dead, but the constraint persists in degraded form, and its actual disappearance required both legal suppression AND cultural substrate shift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_entanglement_vs_additive_mechanisms,
    'Are exogenous legal suppression and endogenous substrate transformation operating as independent, additive mechanisms (both necessary but separately sufficient), or as truly entangled mechanisms (neither sufficient alone, interactive effects critical)?',
    'Comparative historical analysis: regions/periods where legal prohibition preceded cultural delegitimation (testing whether law alone could suppress code, or whether substrate transformation was necessary); regions where dignity frameworks flourished but legal prohibition remained light (testing whether culture alone could suppress code). Examination of institutional records (military disciplinary codes, legal prosecution rates, newspaper coverage of duels) for temporal sequencing and causal claims made by actors themselves.',
    'If mechanisms are additive, each reading captures part of the truth; if entangled, composite_overdetermined_reading is structurally necessary and both sibling readings are incomplete. Entanglement also means the constraint''s terminal attractor (functional collapse vs. indefinite piton persistence) depends on the pathway taken—different region-period combinations exhibit different terminal states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_entanglement_vs_additive_mechanisms, empirical, 'Causal structure of decline mechanisms: independence vs. entanglement').

omega_variable(
    identity_lock_vs_substrate_transformation,
    'When dignity frameworks gain cultural authority, do identity-locked gentlemen exit the code because their identity itself transforms (the substrate shifts and so do they), or because they gain permission to exit while maintaining a coherent (though changed) identity?',
    'Analysis of self-reported justifications by gentlemen who abandoned dueling in the dignity-frame era: do they claim the code is wrong (identity transformation), or do they claim it is no longer necessary for maintaining honor (substrate intact but exit legitimated)? Examination of transitional figures (mid-19th-century military officers who gradually abandoned the code) for evidence of identity renegotiation vs. identity continuity.',
    'If identity transforms (wholesale cultural contraction), cultural_contraction_reading''s core axiom (honor codes underwent foundational transformation) is validated. If identity persists but exit becomes legitimate (dignity frameworks provide an alternative satisfaction path without requiring the code to be ''wrong''), composite_overdetermined_reading''s mechanistic account holds: exogenous suppression becomes costly only when endogenous delegitimation has already weakened community enforcement. The distinction also affects piton diagnosis: if identity persists, late dueling is pure theater (piton); if identity transforms, late dueling is marginal practice within a residual community (piton with different terminal attractor).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_substrate_transformation, conceptual, 'Identity transformation vs. identity-coherent exit under substrate shift').

omega_variable(
    false_summit_mountain_candidate,
    'Is the honor code''s eventual disappearance evidence that it was always a constructed constraint (snare/tangled_rope throughout), or evidence that it rested on a substrate (honor as social-status mechanism) that was genuinely foundational for centuries before that substrate eroded?',
    'Long-run analysis of duel prevalence and casualty rates: if the constraint was always pure extraction (snare signature), casualty rates should be high and stable throughout the interval (extraction persists as long as coercive power holds). If it rested on a foundational substrate, casualty rates and participation should show a high plateau followed by collapse (substrate intact, then eroded). Comparison of societies with similar legal prohibition timing but different cultural legitimacy trajectories: if law is sufficient, both should show equal suppression; if substrate matters, societies with stronger dignity-frame adoption should show earlier functional collapse despite equal legal force.',
    'If the code was always extraction (false summit—a mountain reading that was actually a snare all along), that falsifies composite_overdetermined_reading''s core claim (that decline involved substrate transformation as a distinct mechanism). If the code rested on a genuine foundational substrate that eroded, composite_overdetermined_reading''s account holds: the constraint was once structurally defensible (rope or low-extraction arrangement for communities that endorsed honor combat), became unsustainable only when BOTH mechanisms converged (legal suppression + dignity-frame delegitimation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_mountain_candidate, empirical, 'Was honor code ever a structurally defensible arrangement, or always extraction dressed as coordination?').

omega_variable(
    composite_vs_sibling_framings,
    'Does accepting composite_overdetermined_reading''s causal entanglement necessarily foreclose the practice_decline_reading (law + opportunity cost alone) and cultural_contraction_reading (dignity framework alone), or do all three framings remain live for different subset-histories?',
    'Region-period decomposition: map which readings best fit specific historical trajectories (France, Germany, Russia, Italy, US South show different patterns). Where practice_decline wins (legal pressure + low cultural delegitimation = persistence of code in military subcultures), where cultural_contraction wins (dignity-frame adoption + light legal enforcement = rapid code abandonment), where composite_overdetermined wins (both mechanisms necessary for collapse).',
    'If composite_overdetermined reading forecloses siblings, it asserts a single global mechanism; if siblings coexist, the kernel remains genuinely contested and different institutional actors experienced different decline pathways. The latter supports the coexists_with reading relation; the former would support forecloses. Current evidence suggests coexistence: composite_overdetermined reading is the attempt at synthesis but does not eliminate the regions where one mechanism dominated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(composite_vs_sibling_framings, conceptual, 'Does composite reading subsume or merely coexist with sibling framings?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 1600, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1600, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1600, 0.08).
narrative_ontology:measurement_basis(hono_tr_t1600, projected).
narrative_ontology:measurement(hono_tr_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1700, 0.12).
narrative_ontology:measurement_basis(hono_tr_t1700, observed).
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1750, 0.18).
narrative_ontology:measurement_basis(hono_tr_t1750, observed).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1800, 0.26).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1850, 0.32).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 1900, 0.28).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1600, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1600, 0.48).
narrative_ontology:measurement_basis(hono_be_t1600, projected).
narrative_ontology:measurement(hono_be_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1700, 0.55).
narrative_ontology:measurement_basis(hono_be_t1700, observed).
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1750, 0.59).
narrative_ontology:measurement_basis(hono_be_t1750, observed).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1800, 0.65).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1850, 0.61).
narrative_ontology:measurement_basis(hono_be_t1850, observed).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 1900, 0.62).
narrative_ontology:measurement_basis(hono_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1600, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1600, 0.35).
narrative_ontology:measurement_basis(hono_su_t1600, projected).
narrative_ontology:measurement(hono_su_t1700, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1700, 0.42).
narrative_ontology:measurement_basis(hono_su_t1700, observed).
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1750, 0.54).
narrative_ontology:measurement_basis(hono_su_t1750, observed).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1800, 0.68).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1850, 0.74).
narrative_ontology:measurement_basis(hono_su_t1850, observed).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 1900, 0.71).
narrative_ontology:measurement_basis(hono_su_t1900, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1600, tn=1900
narrative_ontology:measurement(hono_grid_01, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(class), 1600, 0.88).
narrative_ontology:measurement(hono_grid_02, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(class), 1900, 0.35).
narrative_ontology:measurement(hono_grid_03, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(individual), 1600, 0.92).
narrative_ontology:measurement(hono_grid_04, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(individual), 1900, 0.28).
narrative_ontology:measurement(hono_grid_05, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(organizational), 1600, 0.85).
narrative_ontology:measurement(hono_grid_06, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(organizational), 1900, 0.42).
narrative_ontology:measurement(hono_grid_07, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(structural), 1600, 0.78).
narrative_ontology:measurement(hono_grid_08, honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse(structural), 1900, 0.22).
narrative_ontology:measurement(hono_grid_09, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(class), 1600, 0.25).
narrative_ontology:measurement(hono_grid_10, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(class), 1900, 0.75).
narrative_ontology:measurement(hono_grid_11, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(individual), 1600, 0.22).
narrative_ontology:measurement(hono_grid_12, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(individual), 1900, 0.68).
narrative_ontology:measurement(hono_grid_13, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(organizational), 1600, 0.28).
narrative_ontology:measurement(hono_grid_14, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(organizational), 1900, 0.71).
narrative_ontology:measurement(hono_grid_15, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(structural), 1600, 0.18).
narrative_ontology:measurement(hono_grid_16, honor_satisfaction_substrate__composite_overdetermined_reading, resistance(structural), 1900, 0.72).
narrative_ontology:measurement(hono_grid_17, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(class), 1600, 0.58).
narrative_ontology:measurement(hono_grid_18, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(class), 1900, 0.52).
narrative_ontology:measurement(hono_grid_19, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(individual), 1600, 0.65).
narrative_ontology:measurement(hono_grid_20, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(individual), 1900, 0.48).
narrative_ontology:measurement(hono_grid_21, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(organizational), 1600, 0.62).
narrative_ontology:measurement(hono_grid_22, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(organizational), 1900, 0.55).
narrative_ontology:measurement(hono_grid_23, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(structural), 1600, 0.45).
narrative_ontology:measurement(hono_grid_24, honor_satisfaction_substrate__composite_overdetermined_reading, stakes_inflation(structural), 1900, 0.38).
narrative_ontology:measurement(hono_grid_25, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(class), 1600, 0.35).
narrative_ontology:measurement(hono_grid_26, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(class), 1900, 0.74).
narrative_ontology:measurement(hono_grid_27, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(individual), 1600, 0.38).
narrative_ontology:measurement(hono_grid_28, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(individual), 1900, 0.72).
narrative_ontology:measurement(hono_grid_29, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(organizational), 1600, 0.32).
narrative_ontology:measurement(hono_grid_30, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(organizational), 1900, 0.68).
narrative_ontology:measurement(hono_grid_31, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(structural), 1600, 0.28).
narrative_ontology:measurement(hono_grid_32, honor_satisfaction_substrate__composite_overdetermined_reading, suppression(structural), 1900, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the composite_overdetermined_reading instantiation within the honor_satisfaction_substrate kernel family. Sibling readings practice_decline_reading and cultural_contraction_reading decompose the single natural-language concept 'dueling's decline' into three distinct causal framings. All three stories share the same referent (the standing honor-code arrangement under contest) and the same empirical interval (1600-1900), but author different epsilon values reflecting different mechanisms: practice_decline sees exogenous suppression as primary (ε moderately extractive, driven by legal coercion); cultural_contraction sees endogenous substrate shift as primary (ε declining over interval as dignity framework gains legitimacy); composite_overdetermined sees both mechanisms as necessary and entangled (ε initially sustained by dual mechanisms, then degrading only when both reach criticality). The three stories are linked via network.affects_constraints to enable contamination analysis—if one reading's core mechanism is shown empirically inert, downstream implications propagate to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__composite_overdetermined_reading, moderate, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
