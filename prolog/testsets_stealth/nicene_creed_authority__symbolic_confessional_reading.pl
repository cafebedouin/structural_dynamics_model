% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__symbolic_confessional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__symbolic_confessional_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: nicene_creed_authority__symbolic_confessional_reading
 *   human_readable: Nicene Creed as Historically Contingent Witness — Symbolic-Confessional Reading
 *   domain: theological/ecclesial
 *
 * SUMMARY:
 *   Under the symbolic-confessional reading, the Nicene Creed functions in
 *   church life as a historically contingent witness — a fourth-century
 *   testimony received, weighed, and re-interpreted by each discerning
 *   community — rather than as a binding metaphysical verdict. Authority over
 *   the creed's meaning is distributed: no office compels assent, no tribunal
 *   sanctions deviation; what the words commit anyone to is settled between
 *   congregations, consciences, and the historical record the creed carries.
 *   The arrangement coordinates memory and continuity across scattered
 *   communities while deliberately refusing doctrinal enforcement, which
 *   inverts the classical authority topology: the seats that gain are local
 *   congregations, individual believers, the historical-critical academy, and
 *   interfaith partners; the seats that bear costs are centralized doctrinal
 *   authorities, whose sanction function atrophies where the reading spreads,
 *   and creedal dissenters, who absorb the residual consensus pressure that
 *   discernment produces locally. The claim and the metrics are authored
 *   independently: the constraint is claimed as rope — genuine coordination
 *   without coercive overhead — and the metrics describe that low-extraction
 *   operation as this reading assesses it, including its honest residual.
 *   Epsilon's referent is the standing arrangement under contest — the
 *   creed's authority-role in church life — assessed by this reading's own
 *   lights per the kernel-reading rule; the strict sibling reading assesses
 *   the same arrangement at high extraction, and that divergence is the
 *   kernel contest measured, not an inconsistency. KEY AGENTS (by structural
 *   relationship): - local_congregations: Primary beneficiary
 *   (organized/mobile) — receive the creed as witness and set its weight
 *   locally - individual_believers: Beneficiary (moderate/mobile) — assent
 *   freely, no compulsion - creedal_dissenters: Residual payer
 *   (powerless/identity_locked) — bear communal consensus pressure -
 *   centralized_doctrinal_authorities: Payer (institutional/identity_locked)
 *   — bear loss of sanction authority; contest the arrangement -
 *   academic_theologians: Beneficiary (organized/mobile) —
 *   historical-critical program underwrites the witness framing -
 *   interfaith_dialogue_partners: Beneficiary (organized/mobile) — pluralism
 *   opens doctrinal conversation
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__symbolic_confessional_reading, 0.22).
domain_priors:suppression_score(nicene_creed_authority__symbolic_confessional_reading, 0.18).
domain_priors:theater_ratio(nicene_creed_authority__symbolic_confessional_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__symbolic_confessional_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__symbolic_confessional_reading, "Nicene Creed as Historically Contingent Witness — Symbolic-Confessional Reading").
narrative_ontology:topic_domain(nicene_creed_authority__symbolic_confessional_reading, "theological/ecclesial").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__symbolic_confessional_reading, 'b9d449fc-48d1-4380-a293-0b5b47f81339').
narrative_ontology:cs_kernel_codification('b9d449fc-48d1-4380-a293-0b5b47f81339', fixed_text).
narrative_ontology:cs_authority_grounding('b9d449fc-48d1-4380-a293-0b5b47f81339', distributed).
narrative_ontology:cs_reading_relation('b9d449fc-48d1-4380-a293-0b5b47f81339', nicene_creed_authority__strict_orthodox_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9d449fc-48d1-4380-a293-0b5b47f81339', nicene_creed_authority__liturgical_habituation_reading, coexists_with).
narrative_ontology:cs_axiom('b9d449fc-48d1-4380-a293-0b5b47f81339', foundational, creedal_authority_is_discerned_not_coerced).
narrative_ontology:cs_axiom_status(creedal_authority_is_discerned_not_coerced, holdable).
narrative_ontology:cs_axiom_grounding('b9d449fc-48d1-4380-a293-0b5b47f81339', creedal_authority_is_discerned_not_coerced, deontological).
narrative_ontology:cs_axiom('b9d449fc-48d1-4380-a293-0b5b47f81339', foundational, creed_is_historically_contingent_witness).
narrative_ontology:cs_axiom_status(creed_is_historically_contingent_witness, holdable).
narrative_ontology:cs_axiom_grounding('b9d449fc-48d1-4380-a293-0b5b47f81339', creed_is_historically_contingent_witness, empirically_contingent).
narrative_ontology:cs_axiom('b9d449fc-48d1-4380-a293-0b5b47f81339', secondary, doctrinal_pluralism_is_permitted).
narrative_ontology:cs_axiom_status(doctrinal_pluralism_is_permitted, holdable).
narrative_ontology:cs_axiom_grounding('b9d449fc-48d1-4380-a293-0b5b47f81339', doctrinal_pluralism_is_permitted, deontological).
narrative_ontology:cs_reference_frame('b9d449fc-48d1-4380-a293-0b5b47f81339', creed_as_contingent_witness).
narrative_ontology:cs_drift_state('b9d449fc-48d1-4380-a293-0b5b47f81339', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('b9d449fc-48d1-4380-a293-0b5b47f81339', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, local_congregations).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, individual_believers).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, academic_theologians).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, interfaith_dialogue_partners).
narrative_ontology:constraint_victim(nicene_creed_authority__symbolic_confessional_reading, centralized_doctrinal_authorities).
narrative_ontology:constraint_victim(nicene_creed_authority__symbolic_confessional_reading, creedal_dissenters).
narrative_ontology:constraint_vindicates(nicene_creed_authority__symbolic_confessional_reading, freedom_of_conscience).
narrative_ontology:constraint_vindicates(nicene_creed_authority__symbolic_confessional_reading, historical_critical_method).
narrative_ontology:constraint_vindicates(nicene_creed_authority__symbolic_confessional_reading, communal_discernment_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive the creed as a testimony handed down from the fourth century rather than as a verdict imposed on them. Each community discerns what the text means for its own life: some recite it weekly as a summary of inherited faith, others treat it as one voice among the witnesses of the tradition. They decide, congregation by congregation, how much weight the words carry in worship and teaching, and they may revise that weight without answering to a central office. Leaving the arrangement would mean surrendering a shared historical touchstone, but any single community can reinterpret or de-emphasize the text at little cost.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, local_congregations, beneficiary,
    organized, generational, mobile, regional).

% Affirm or question the creed's statements as a matter of personal faith. No office can compel their assent or sanction their doubt; what they believe about the creed's claims is settled between them, their community, and their own conscience. They can join communities whose discernment matches theirs, and moving between congregations carries modest social cost.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, individual_believers, beneficiary,
    moderate, biographical, mobile, local).

% Members of discerning communities whose own discernment cannot reach the creed's words — non-creedal Christians, post-confessional humanists, or believers whose reading of the tradition diverges from the local consensus. No tribunal punishes them, but the community's shared recitation and its settled sense of what the words mean create a steady expectation to conform. Their family ties, friendships, and religious identity are rooted in the community, so walking away costs them the social world they live in; staying means absorbing the pressure quietly.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, creedal_dissenters, payer,
    powerless, biographical, identity_locked, local).

% Offices and magisteria — conciliar successors, confessional enforcement bodies, doctrinal congregations — whose custodial role rests on the creed binding all believers to one metaphysical ontology. Where the discernment arrangement spreads, their sanction power over deviation atrophies: anathemas land on communities that no longer recognize the tribunal. They cannot resign the custodial role without dissolving the office itself, so they contest the arrangement through teaching authority, ecumenical objection, and the maintenance of parallel binding jurisdictions where they still hold sway.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, centralized_doctrinal_authorities, payer,
    institutional, generational, identity_locked, global).

% Historical-critical scholars whose work documents the creed's fourth-century context, its imperial enforcement, and its editorial history. The discernment arrangement treats their findings as input to communal discernment rather than as a threat to fixed doctrine, which gives their discipline standing inside the churches it studies. They can move between universities, denominations, and secular forums without loss.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, academic_theologians, beneficiary,
    organized, generational, mobile, continental).

% Jewish, Muslim, and secular interlocutors who engage Christian communities in doctrinal conversation. Where the creed functions as contingent witness rather than enforced boundary, these partners meet communities willing to say 'this is our inherited testimony' rather than 'this is the ontological truth you must accept,' which opens conversations the enforced-boundary arrangement forecloses. They hold no stake in the creed's internal authority and can disengage at will.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__symbolic_confessional_reading, interfaith_dialogue_partners, beneficiary,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nicene_creed_authority__symbolic_confessional_reading, diffuse).
narrative_ontology:fixing_cost_class(nicene_creed_authority__symbolic_confessional_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains continuity between present communities and the inherited tradition: the creed gives scattered congregations a common historical touchstone, a shared liturgical inheritance, and a common vocabulary for faith, while leaving the question of what the words commit anyone to — metaphysically or morally — to local discernment and personal assent. It coordinates memory and identity without requiring uniform belief.
% TRANSFER_FUNCTION: Moves interpretive authority and disciplinary sanction away from centralized doctrinal offices and toward local congregations and individual consciences; moves the creed's accumulated prestige and historical continuity to the communities that receive it, as an inheritance they may use rather than a debt they must repay in enforced assent.
% ABSENT_VOICES: Centralized doctrinal authorities and confessional traditionalists would object that a creed no one must answer to is not a creed but a museum piece; they are present in the wider dispute but outside the communities where the arrangement operates, where their objection carries no standing. Closer to home, creedal dissenters within discerning congregations are rarely convened: discernment processes tend to be run by the like-minded, so the members whose discernment fails to reach the creed's words are structurally under-represented in the very process that claims to honor discernment.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — if the creed reverted to enforced boundary or disappeared from use entirely — congregational worship would lose its historical anchor: some communities would re-subscribe to binding formulations and re-empower doctrinal offices, others would drop creedal language and lose continuity with the tradition, and ecumenical conversation would lose the common text around which it currently organizes. The mainline Protestant landscape, the historical-critical theological establishment, and interfaith channels all currently route through this arrangement.
% FOUNDING_PROBLEM: The creed was forged in the fourth century to settle the Arian controversy: to bind the church to a single metaphysical account of Christ's relation to God, with imperial power standing behind the conciliar formula and deviation punishable as heresy.
% FOUNDING_PROBLEM_CORROBORATION: Historians of doctrine — seated outside every beneficiary — corroborate the genealogy: the Nicene-Constantinopolitan text was produced as a boundary instrument against Arian teaching, backed by imperial enforcement, and its anathemas targeted named positions. Whether the founding problem is dead is disputed by the parties: centralized doctrinal authorities attest it live (the ontological question is permanent and binding definition remains the church's task), while the communities operating this arrangement attest it dead (faith cannot be compelled and the Arian controversy is historically closed). Academic historical consensus corroborates the genealogy and the closure of the original controversy, but takes no seat on whether binding definition remains necessary.
narrative_ontology:disappearance_verdict(nicene_creed_authority__symbolic_confessional_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__symbolic_confessional_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__symbolic_confessional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nicene_creed_authority__symbolic_confessional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__symbolic_confessional_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__symbolic_confessional_reading_tests).
:- end_tests(nicene_creed_authority__symbolic_confessional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The interval spans the reading's rise (roughly 1900-2020): the era in which historical-critical scholarship documented the creed's contingent origins and mainline churches dismantled subscription and discipline machinery. All three tracked series share one grid (points 0,20,40,60,80,100,120) so no metric is backfilled or end-state-substituted. base_extractiveness falls 0.34 to 0.22 as binding force erodes — the standing arrangement becomes steadily less costly to those inside it, ending at the authored epsilon. theater_ratio stays low (0.08 to 0.12): the arrangement's function — memory, continuity, common vocabulary — is actively practiced, with a mild rise from heritage recitation in shrinking congregations. suppression_requirement falls 0.42 to 0.18, tracking the deliberate decay of enforcement capacity; this trajectory is the story's central dynamic, so the series is authored even though base_properties.suppression is a raw, unscaled structural property (only extractiveness is engine-scaled by directionality and scope). accessibility_collapse is 0.25 because the arrangement leaves alternatives standing by design — pluralism, rival readings, and interfaith engagement are features, not leaks. resistance 0.38 records the contest: centralized authorities and confessional traditionalists actively resist the reading, though they lack jurisdiction where it holds. The residual extraction the metrics do record is diffuse: no named seat collects it as rent — the authorities' lost authority dissipates into distributed discernment rather than transferring to a captor, and the dissenters' burden accrues to no one's account but the community's general comfort — hence gain_flow is authored 'diffuse' as a checked claim, and fixing is cheap: a discerning congregation can accommodate non-assent procedurally (opt-out recitation, alternative affirmations) with no structural lock-in preventing it.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the congregational and believer seats the arrangement is a coordination mechanism they operate on themselves: low cost, real benefit, revisable at will. From the dissenter seat the same arrangement reads as a mild enforced consensus — pressure without tribunal, borne by those with the least exit. From the authority seat it reads as expropriation: the arrangement strips the office of its function while leaving the office intact, which is why these actors contest rather than comply. Most importantly, the strict sibling reading computes the identical standing arrangement as a high-extraction enforced-ontology regime; the cross-reading divergence in computed type is the kernel contest itself, taken as a measurement rather than adjudicated by this story's claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations put local_congregations, individual_believers, academic_theologians, and interfaith_dialogue_partners near the beneficiary end (low d), with mobile exits damping further. creedal_dissenters derive high d despite powerlessness: identity_locked exit amplifies their position toward full target, which is where the arrangement's honest residual extraction concentrates. centralized_doctrinal_authorities would derive near-full-target from the victim declaration plus identity_locked exit, but the derivation overstates their case: the cost they bear is foregone authority — the arrangement stops subsidizing their sanction function — not resources taken from them; they remain wealthy, staffed, and jurisdictionally intact where they hold territory. The directionality override sets the institutional atom to 0.7 to record de-subsidization rather than extraction. Suppression, by contrast, is authored unscaled: the 0.18 is the raw structural fact that no enforcement machinery stands behind the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — settling a fourth-century ontological controversy by enforced conciliar definition — is dead, and the arrangement persists anyway: the classic mandatrophy signature, and the R5 mismatch (dead founding problem x world_rearranges) correctly raises the capture/zombie hypothesis. The classification data resolve it toward repurposing rather than atrophy: theater_ratio is low and nearly flat (0.08 to 0.12), the coordination function is actively practiced rather than performed, and the arrangement's authority is renewed by each community's discernment rather than maintained by inertia. What the classification prevents: reading the dead founding problem as proof of pure extraction (the strict sibling's accusation — 'a creed no one must answer to is a corpse') would mislabel a genuinely low-extraction arrangement; reading the low extraction as proof of health would miss the residual burden the measurements track on the least powerful seat. The arrangement is a repurposed instrument with a live function, and the corpus should hold both facts simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_locus_of_authority,
    'This constraint is one reading of the nicene_creed_authority kernel; the sibling readings (strict_orthodox_reading, liturgical_habituation_reading) locate the creed''s authority elsewhere — in coercive conciliar binding with sanctioned deviation, and in liturgical identity-formation independent of cognitive assent, respectively. Which locus is structurally real for a given body, and how would this classification move if another locus prevailed there?',
    'Comparative classification of the sibling stories against the same standing arrangement; longitudinal tracking of which reading gains jurisdictions, liturgies, and seminaries over time.',
    'If the strict locus prevails in a body, the victim set inverts (dissenters become the sanctioned), extractiveness rises sharply, and the arrangement moves toward enforced ontology; if the habituation locus prevails, the burden becomes cognitive-formational rather than coercive and the beneficiary set shifts to boundary-maintaining majorities. This story''s low extraction is a property of this reading''s operation, not of the creed label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_locus_of_authority, conceptual, 'Kernel contest: where creedal authority actually resides across the three readings of the same text.').

omega_variable(
    discernment_majoritarian_residual,
    'Does communal discernment itself become a local pressure mechanism — a majoritarian consensus that quietly penalizes members whose discernment cannot reach the creed''s words — and how much of the measured residual cost is this rather than leftover prestige from the prior binding regime?',
    'Comparative study of congregations with and without formal non-assent accommodations (opt-out recitation, alternative affirmations): if dissenters'' reported pressure drops where accommodations exist, the residual is procedural and fixable at low cost.',
    'If the residual is procedural, the arrangement is a healthy coordination mechanism with a fixable defect; if it is intrinsic to communal discernment, the low measured cost understates a diffuse burden borne by the least powerful members, and the payer seat''s classification should harden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discernment_majoritarian_residual, empirical, 'Whether local discernment reproduces, in softened form, the pressure the reading removed at the center.').

omega_variable(
    internalized_assent_pressure,
    'How much of the residual pressure on individual assent is internalized — carried by believers formed under stricter regimes who experience recitation as binding even where no office enforces it — as opposed to structural expectation sustained by current communal practice?',
    'Post-transition cohort comparison: believers formed entirely under the discernment arrangement versus those formed under subscription regimes; if the pressure differential persists across generations of formation, the internalized component is decaying rather than structural.',
    'If largely internalized, the arrangement''s measured suppression understates the burden its history imposed, and full de-extraction requires generational turnover rather than institutional reform; if largely structural, congregational procedure reform suffices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_assent_pressure, empirical, 'Structural versus internalized residue of the prior binding regime in individual assent.').

omega_variable(
    pluralism_durability_under_revival,
    'Is the discernment arrangement a steady-state coordination form, or a transitional accommodation that reverts to binding readings under confessional revival pressure?',
    'Track jurisdictions experiencing orthodox-revival movements: if congregations that adopted discernment revert to subscription formulas under revival pressure, the arrangement is transitional; if they retain discernment-based authority, it is stable.',
    'If transitional, the arrangement resembles a scaffold whose sunset is externally imposed rather than declared, and its low measured cost is a phase property rather than a structural one; the claimed rope would then be a snapshot of a passage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pluralism_durability_under_revival, empirical, 'Durability of discernment-based creedal authority against revival pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__symbolic_confessional_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_symbolic_tr_t0, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(nicene_symbolic_tr_t0, observed).
narrative_ontology:measurement(nicene_symbolic_tr_t20, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement_basis(nicene_symbolic_tr_t20, observed).
narrative_ontology:measurement(nicene_symbolic_tr_t40, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement_basis(nicene_symbolic_tr_t40, observed).
narrative_ontology:measurement(nicene_symbolic_tr_t60, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement_basis(nicene_symbolic_tr_t60, observed).
narrative_ontology:measurement(nicene_symbolic_tr_t80, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 80, 0.11).
narrative_ontology:measurement_basis(nicene_symbolic_tr_t80, observed).
narrative_ontology:measurement(nicene_symbolic_tr_t100, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement_basis(nicene_symbolic_tr_t100, observed).
narrative_ontology:measurement(nicene_symbolic_tr_t120, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 120, 0.12).
narrative_ontology:measurement_basis(nicene_symbolic_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(nicene_symbolic_be_t0, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(nicene_symbolic_be_t0, observed).
narrative_ontology:measurement(nicene_symbolic_be_t20, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement_basis(nicene_symbolic_be_t20, observed).
narrative_ontology:measurement(nicene_symbolic_be_t40, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 40, 0.27).
narrative_ontology:measurement_basis(nicene_symbolic_be_t40, observed).
narrative_ontology:measurement(nicene_symbolic_be_t60, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 60, 0.25).
narrative_ontology:measurement_basis(nicene_symbolic_be_t60, observed).
narrative_ontology:measurement(nicene_symbolic_be_t80, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 80, 0.24).
narrative_ontology:measurement_basis(nicene_symbolic_be_t80, observed).
narrative_ontology:measurement(nicene_symbolic_be_t100, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 100, 0.23).
narrative_ontology:measurement_basis(nicene_symbolic_be_t100, observed).
narrative_ontology:measurement(nicene_symbolic_be_t120, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 120, 0.22).
narrative_ontology:measurement_basis(nicene_symbolic_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(nicene_symbolic_su_t0, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(nicene_symbolic_su_t0, observed).
narrative_ontology:measurement(nicene_symbolic_su_t20, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement_basis(nicene_symbolic_su_t20, observed).
narrative_ontology:measurement(nicene_symbolic_su_t40, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 40, 0.33).
narrative_ontology:measurement_basis(nicene_symbolic_su_t40, observed).
narrative_ontology:measurement(nicene_symbolic_su_t60, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 60, 0.28).
narrative_ontology:measurement_basis(nicene_symbolic_su_t60, observed).
narrative_ontology:measurement(nicene_symbolic_su_t80, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 80, 0.24).
narrative_ontology:measurement_basis(nicene_symbolic_su_t80, observed).
narrative_ontology:measurement(nicene_symbolic_su_t100, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 100, 0.2).
narrative_ontology:measurement_basis(nicene_symbolic_su_t100, observed).
narrative_ontology:measurement(nicene_symbolic_su_t120, nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 120, 0.18).
narrative_ontology:measurement_basis(nicene_symbolic_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__symbolic_confessional_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the authority of the Nicene Creed' covers three structurally distinct claims (per the epsilon-invariance principle): (1) binding metaphysical ontology with sanctioned deviation (strict_orthodox_reading), (2) identity boundary maintained through liturgical performance independent of assent (liturgical_habituation_reading), (3) contingent witness whose authority is discernment-based (this file). Each instantiates a different constraint with its own epsilon, beneficiary/victim structure, and classification; they form a constraint family linked here. The strict reading is the upstream member — historically prior and still dominant globally — and the spread of this reading dismantles its enforcement machinery, which is the structural influence recorded by the family edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_creed_authority__symbolic_confessional_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
