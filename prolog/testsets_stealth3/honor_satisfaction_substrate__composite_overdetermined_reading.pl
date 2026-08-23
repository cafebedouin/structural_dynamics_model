% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__composite_overdetermined_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__composite_overdetermined_reading, []).

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
 *   constraint_id: honor_satisfaction_substrate__composite_overdetermined_reading
 *   human_readable: Honor-Satisfaction Substrate of the European Affair of Honor (Composite Overdetermined Reading)
 *   domain: historical sociology/legal history/cultural anthropology
 *
 * SUMMARY:
 *   The honor-satisfaction substrate is the codified and customary
 *   arrangement by which European gentlemen — above all officers — converted
 *   insult into ritualized combat under rules of challenge, seconding, and
 *   satisfaction, from roughly the post-Napoleonic restoration to the First
 *   World War (interval t=0 maps to circa 1820, t=90 to 1914). Read through
 *   the composite_overdetermined lens, the standing arrangement carried a
 *   genuine coordination function (closing disputes without feud), presented
 *   itself as the natural law of gentlemanly existence, and ran on active
 *   enforcement (ostracism, career destruction for refusers) while imposing
 *   asymmetric costs (compelled combatants, the dead and their dependents, a
 *   commonality excluded from the channel that licensed gentry violence).
 *   This story is one reading of the kernel honor_satisfaction_substrate; its
 *   siblings — practice_decline_reading and cultural_contraction_reading —
 *   are separate constraints with their own epsilon values and stakeholder
 *   weightings, linked through the network block. The epsilon referent here
 *   is fixed: the standing dueling arrangement as this reading assesses it,
 *   never the historiography this reading would endorse in its place.
 *
 * KEY AGENTS:
 *   - aristocratic_honor_class: agenda-setting beneficiary (powerful/identity_locked) — writes and administers the code, collects standing from it, cannot exit without ceasing to be what it is
 *   - officer_corps_establishment: institutional beneficiary (institutional/constrained) — collects discipline-on-the-cheap, later administers the suppression it once fed on
 *   - junior_officers_and_gentleman_amateurs: primary payer (moderate/trapped) — bears challenge, wound, prosecution; every exit is priced as dishonor
 *   - duel_casualties_and_dependents: terminal payer (powerless/trapped) — the killed and their families, unseated in the code that disposed of them
 *   - commoners_outside_the_code: excluded payer (powerless/trapped) — barred from the satisfaction channel, absorbs its violent spillover
 *   - fencing_masters_and_professional_seconds: mobile beneficiary (moderate/mobile) — sells the practice's services, reprices into sport when the market contracts
 *   - anti_dueling_campaigners: external observer (organized/analytical) — statute, pulpit, and press against the code, never holding the administrative pen
 *   - courts_and_war_ministries: institutional observer (institutional/analytical) — nominal enemy of the code whose selective enforcement reveals the exogenous mechanism as rising opportunity cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__composite_overdetermined_reading, 0.62).
domain_priors:suppression_score(honor_satisfaction_substrate__composite_overdetermined_reading, 0.66).
domain_priors:theater_ratio(honor_satisfaction_substrate__composite_overdetermined_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__composite_overdetermined_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__composite_overdetermined_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_substrate__composite_overdetermined_reading, "Honor-Satisfaction Substrate of the European Affair of Honor (Composite Overdetermined Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__composite_overdetermined_reading, "historical sociology/legal history/cultural anthropology").

domain_priors:requires_active_enforcement(honor_satisfaction_substrate__composite_overdetermined_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__composite_overdetermined_reading, '096a8501-aaf1-456d-972c-effc58125e22').
narrative_ontology:cs_kernel_codification('096a8501-aaf1-456d-972c-effc58125e22', distributed).
narrative_ontology:cs_authority_grounding('096a8501-aaf1-456d-972c-effc58125e22', practice).
narrative_ontology:cs_interpretation_layer_present('096a8501-aaf1-456d-972c-effc58125e22').
narrative_ontology:cs_reading_relation('096a8501-aaf1-456d-972c-effc58125e22', honor_satisfaction_substrate__practice_decline_reading, forecloses).
narrative_ontology:cs_reading_relation('096a8501-aaf1-456d-972c-effc58125e22', honor_satisfaction_substrate__cultural_contraction_reading, forecloses).
narrative_ontology:cs_axiom('096a8501-aaf1-456d-972c-effc58125e22', foundational, decline_requires_joint_mechanism).
narrative_ontology:cs_axiom_status(decline_requires_joint_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('096a8501-aaf1-456d-972c-effc58125e22', decline_requires_joint_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('096a8501-aaf1-456d-972c-effc58125e22', foundational, mechanisms_causally_non_independent).
narrative_ontology:cs_axiom_status(mechanisms_causally_non_independent, holdable).
narrative_ontology:cs_axiom_grounding('096a8501-aaf1-456d-972c-effc58125e22', mechanisms_causally_non_independent, empirically_contingent).
narrative_ontology:cs_reference_frame('096a8501-aaf1-456d-972c-effc58125e22', dual_mechanism_honor_equilibrium).
narrative_ontology:cs_drift_state('096a8501-aaf1-456d-972c-effc58125e22', fin_de_siecle_pre_1914, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('096a8501-aaf1-456d-972c-effc58125e22', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_honor_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, officer_corps_establishment).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__composite_overdetermined_reading, fencing_masters_and_professional_seconds).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, junior_officers_and_gentleman_amateurs).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, duel_casualties_and_dependents).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__composite_overdetermined_reading, commoners_outside_the_code).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and revises the rules governing satisfaction: codified rulebooks (the Irish Code Duello of 1777, Chatauvillard's 1836 regulations), regimental mess custom, and accumulated precedent. Staffs the seconds and the informal tribunals that decide what counts as an insult and what repair suffices. Members convert standing in the honor economy into command, office, and marriage; individual members occasionally pay in blood, but the class as a whole administers the arrangement and collects from it. Leaving is not available as a simple choice: a member who renounces the code forfeits his place in the honor economy, and with it the self he has been raised to be.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_honor_class, agenda_setter,
    powerful, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_honor_class, beneficiary).

% Regiments and war ministries collect discipline without courts-martial: the honor economy enforces courage, deference thresholds, and internal hierarchy among officers at no budgetary cost. When the political calculus shifts, the same institutions become administrators of suppression — regulations barring officers from giving or accepting challenges, dismissal for participants — without ever formally dismantling the promotion culture the code underwrote.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, officer_corps_establishment, beneficiary,
    institutional, generational, constrained, national).

% Young men of the officer and gentry classes bear the arrangement's risks directly: the challenge, the wound, the death, the prosecution. Refusal ends careers and engagements; acceptance risks both body and neck. From inside, the choice is between two ruin paths priced differently, not between compliance and freedom. Physical exits exist — resignation, colonial transfer, emigration — but each carries the same dishonor price as refusal, and shrinks as the code's reach extends.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, junior_officers_and_gentleman_amateurs, payer,
    moderate, biographical, trapped, national).

% The killed and wounded, and the widows and orphans left with pension petitions and social stigma. They hold no seat in the code that disposed of them; their recourse is charity, the occasional unsuccessful suit against seconds, or silence. Nothing in the arrangement's machinery registers their objection.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, duel_casualties_and_dependents, payer,
    powerless, immediate, trapped, local).

% Tradesmen, rankers, rural laborers: their quarrels are brawls before a magistrate, never affairs of honor. They are barred from the satisfaction channel that lets gentlemen settle scores without criminal record, and they absorb the spillover — a gentry licensed by rulebook to forms of violence that would hang a commoner. They would object to the double standard if anyone convened them; no mechanism does.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, commoners_outside_the_code, excluded,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_substrate__composite_overdetermined_reading, commoners_outside_the_code, payer).

% Sell instruction, weapons, venue, and seconding services; the trade peaks with the practice and converts smoothly to sport fencing and pistol clubs as the affair-of-honor market contracts. Theirs is the easiest exit in the story: the skills reprice, the clientele rebrands, nothing in their identity requires the lethal version.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, fencing_masters_and_professional_seconds, beneficiary,
    moderate, biographical, mobile, national).

% Evangelical societies, utilitarian pamphleteers, reforming newspapers: they compile casualty rolls, lobby legislatures, and work to invert the code's incentive by praising the man who declines as the braver man. They hold no seat inside the arrangement; their leverage is external — statute, pulpit, press — and their campaign spans the entire interval without once holding the administrative pen.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, anti_dueling_campaigners, observer,
    organized, generational, analytical, national).

% Prosecutors, judges, courts-martial, and regulatory bureaus. Formally the code's enemy: statutes threaten transportation or death, regulations threaten dismissal. Practically, juries acquit duelists for decades and enforcement stays selective until the political wind turns — their seat records how the exogenous mechanism actually operated: not as crushing force but as a steadily rising opportunity cost that no single statute ever imposed alone.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__composite_overdetermined_reading, courts_and_war_ministries, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_satisfaction_substrate__composite_overdetermined_reading, aristocratic_honor_class).
narrative_ontology:fixing_cost_class(honor_satisfaction_substrate__composite_overdetermined_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channeled disputes among men of claimed equal standing into a bounded, rule-governed encounter with defined endpoints — satisfaction given, matter closed, no reprisals — thereby preventing unbounded feud inside the officer corps and gentry politics, and maintaining the honor hierarchy that disciplined both without state apparatus.
% TRANSFER_FUNCTION: Moved bodily risk and status capital among gentlemen: a challenger converted humiliation into a chance to reclaim standing; a loser paid in blood, limb, or life; a refuser paid in career and marriageability; the class as a whole collected the boundary-marking that reserved honorable violence to itself.
% ABSENT_VOICES: The killed, the widowed and orphaned, and the commoners barred from the satisfaction channel would all object if seated; clergy objected continuously and were structurally discounted inside the honor frame. They sit outside the mess, the code, and the jury box — the last notably acquitting the very men the statute condemned.
% DISAPPEARANCE_RATIONALE: When the arrangement lapsed, the world demonstrably rearranged: honor-economy functions were reabsorbed by party organization, professional courts, bureaucratic promotion, and a masculinity reorganized around dignity rather than standing-at-risk. Counterfactually, removing it at its height would have returned intra-elite disputes to feud, factional violence, or open defiance of superiors — the arrangement was load-bearing, not ornamental.
% FOUNDING_PROBLEM: In a world where the state neither protected gentle status nor reliably punished insult among armed men of equal rank, how could disputes be closed without endless vendetta while preserving the honor hierarchy that ordered military and gentry life?
% FOUNDING_PROBLEM_CORROBORATION: Corroboration comes from outside the benefiting parties: the historiography of honor (Nye on France, Frevert on Germany, Wyatt-Brown on the American South) independently dates the founding problem's dissolution to the consolidation of the state's dispute-resolution monopoly and the dignity turn; parliamentary debate records on the anti-duelling statutes and the memoir literature of refusers attest the same shift. No living party defends the founding problem as live.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__composite_overdetermined_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__composite_overdetermined_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__composite_overdetermined_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_substrate__composite_overdetermined_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim and metrics are authored independently. The claimed type, tangled_rope, states this reading's structural verdict: the arrangement solved a real collective-action problem (feud prevention among armed equals) AND ran identifiable extraction (compelled combat, class-exclusion rents) AND required continuous enforcement (the code collapsed wherever refusers could be tolerated). Extractiveness 0.62 and suppression 0.66 describe the mature phase (roughly t=0 to t=30): participation was coerced by the price of refusal, lethality was real, and the class boundary was policed. Theater 0.24 at maturity: the rituals mostly did what they claimed. Accessibility_collapse 0.55: within the honor frame, alternatives (apology, courts, silence) were priced as dishonor but never vanished — apology protocols were honored exits, which is precisely why collapse is partial. Resistance 0.58: evangelical and utilitarian campaigns, refusers, and abolition bills met the code continuously for a century without breaking it until the substrate itself moved. Stated assumption: base_properties describe the mature arrangement (the referent under contest), while the measurement series tracks the decline on one shared seven-point grid. The suppression_requirement series is the composite thesis in miniature: enforcement rises through mid-interval (statutes, courts-martial, regulation), peaks around t=45, then FALLS without any repeal-driven revival — because the endogenous delegitimation removed the demand the enforcement had been suppressing. Theater crosses 0.5 at t=60 (Goodhart threshold): the form outlives the function, culminating in the pre-negotiated, bloodless fin-de-siecle French duel.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setting beneficiary seat, the arrangement is civilization itself: the alternative to satisfaction is feud or dishonor, so the code reads as the thin line between order and savagery. From the trapped payer seats, the identical structure is a trap with two-priced ruins — fight and maybe die, refuse and certainly fall. The excluded commoner seat sees neither order nor honor but a license denied to him and enforced against him. The observer seats split again: the campaigner sees a slaughter to be stopped; the courtroom record shows a crime juries would not punish — the same arrangement, four incompatible experiences, all derived from the same power/exit data rather than asserted.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster at the low-d end: the honor class converts the arrangement into standing (its deepest subsidy, fused with identity), the officer corps converts it into unpaid discipline, the fencing trade converts it into fees. Payers cluster at the high-d end: junior officers are trapped because every exit is priced as dishonor; casualties and dependents are trapped absolutely; commoners bear spillover costs from an arrangement they are formally outside — the derivation reads them as targets of the boundary the code maintains even though they are excluded from its channel. Suppression is authored as a raw structural property (0.66) and is deliberately unscaled; the engine's directionality and scope handling does the rest. No directionality overrides are used: the beneficiary/victim declarations plus exit options reproduce the structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — closing disputes among armed equals in a weak-state honor economy — is dead: the state's dispute-resolution monopoly and the dignity turn dissolved it, and corroboration comes from outside the benefiting parties. The disappearance verdict is world_rearranges: the functions were reabsorbed, not orphaned. The status-x-verdict mismatch flags zombie risk, and the cross-check lands honestly: the late-period theater rise (0.16 to 0.76) shows the arrangement DID linger as performance after its mandate died — the bloodless French duel to 1914 was residual form — before expiring outright rather than persisting as permanent theater. The composite reading's specific contribution to mandatrophy resolution: a pure exogenous account would predict revival wherever enforcement lapsed (none came); a pure endogenous account would predict a smooth fade with no enforcement peak (but suppression measurably peaked mid-interval, doing real coercive work). Only the entangled account fits the recorded trajectories: law raised the price, the thinned practice starved belief, and the eroded belief made further enforcement unnecessary — mandate death dated to the entanglement, not to either mechanism alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Which account of the honor-satisfaction substrate''s fate does the evidence support: exogenous-only, endogenous-only, or entangled overdetermination?',
    'Comparative jurisdiction analysis holding honor culture roughly constant while legal suppression varies (Britain versus France versus Prussia), crossed with cases where cultural indicators moved ahead of or behind statutory change; mediation analysis of the two pathways on practice-volume data.',
    'If exogenous-only, this story collapses toward practice_decline_reading; if endogenous-only, toward cultural_contraction_reading; the composite survives only if both pathways show independent causal contribution with a nonzero interaction term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'This story is one reading of kernel honor_satisfaction_substrate; sibling adoption would restructure beneficiary/victim weighting and the modeled decay dynamics.').

omega_variable(
    pathway_separability,
    'Are the exogenous (legal/institutional) and endogenous (honor-code transformation) decline pathways empirically separable, or does the overdetermination claim render the decomposition unfalsifiable as stated?',
    'Natural experiments: jurisdictions with vigorous prosecution and intact honor demand (did dueling persist underground?), against regions where honor indicators eroded before prohibition (did practice fade without law?); timing analysis of statute, practice volume, and rhetorical register.',
    'If separable and additive, decompose into two linked stories with independent epsilon values joined by network edges; if the interaction dominates, the composite reading stands as one constraint and the additive language in the kernel label is retired.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathway_separability, empirical, 'Whether ''two mechanisms, non-independent'' is a discoverable causal structure or an unfalsifiable framing.').

omega_variable(
    compliance_mechanism_split,
    'Was duelist compliance sustained by structural coercion (career, legal, and marital consequences of refusal) or by internalized conviction (honor as constitutive of selfhood)?',
    'Private correspondence and diaries of officers who fought while professing contempt for the code; career-outcome data for known refusers; conduct comparisons under anonymity (duels fought abroad under assumed names).',
    'A higher internalized share means the scalar suppression measure understates the arrangement''s grip and predicts slow decay after statutes pass; a higher structural share predicts the sharp post-statute declines the British record actually shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_mechanism_split, empirical, 'Structural versus internalized share of the enforcement that held the arrangement together.').

omega_variable(
    natural_presentation_vs_construction,
    'Did the point of honor operate as presented — an irreducible law of gentlemanly existence — or as a constructed status technology serving identifiable beneficiaries?',
    'Codification history (Irish Code Duello 1777, Chatauvillard 1836): authored rulebooks, revisable customs, and explicit carve-outs over who may not be challenged reveal construction; set against participants'' naturalizing rhetoric across the interval.',
    'If constructed-with-beneficiaries, the arrangement''s mountain-like self-presentation is cover and its classification sits firmly in the coordination-plus-costs range; if all seats genuinely experienced it as natural law, the erosion pathway carries more explanatory weight and the beneficiary declarations understate the substrate''s autonomy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_presentation_vs_construction, conceptual, 'Whether the arrangement''s natural-law presentation was cover for a constructed status technology.').

omega_variable(
    late_theater_interpretation,
    'Does the late-period rise in performative activity indicate Goodhart drift (empty form outliving function) or deliberate de-escalation (ritual successfully converting lethal practice into harmless display)?',
    'Compare casualty rates and negotiation records of fin-de-siecle French duels against mid-century affairs; distinguish pre-arranged bloodless combats from genuine attempts on the same protocol surface.',
    'The drift reading supports a zombie-residue verdict on late dueling; the de-escalation reading recasts the remnant as a successful conversion into consensual sport — changing what the arrangement''s ending teaches about managed institutional death.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(late_theater_interpretation, conceptual, 'Two incompatible readings of the same rising theater trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__composite_overdetermined_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hss_composite_tr_t0, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(hss_composite_tr_t15, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(hss_composite_tr_t30, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(hss_composite_tr_t45, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 45, 0.43).
narrative_ontology:measurement(hss_composite_tr_t60, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 60, 0.56).
narrative_ontology:measurement(hss_composite_tr_t75, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 75, 0.67).
narrative_ontology:measurement(hss_composite_tr_t90, honor_satisfaction_substrate__composite_overdetermined_reading, theater_ratio, 90, 0.76).

% Extraction over time
narrative_ontology:measurement(hss_composite_be_t0, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 0, 0.66).
narrative_ontology:measurement(hss_composite_be_t15, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(hss_composite_be_t30, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(hss_composite_be_t45, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 45, 0.52).
narrative_ontology:measurement(hss_composite_be_t60, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 60, 0.44).
narrative_ontology:measurement(hss_composite_be_t75, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 75, 0.36).
narrative_ontology:measurement(hss_composite_be_t90, honor_satisfaction_substrate__composite_overdetermined_reading, base_extractiveness, 90, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(hss_composite_su_t0, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(hss_composite_su_t15, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(hss_composite_su_t30, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(hss_composite_su_t45, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 45, 0.73).
narrative_ontology:measurement(hss_composite_su_t60, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(hss_composite_su_t75, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 75, 0.6).
narrative_ontology:measurement(hss_composite_su_t90, honor_satisfaction_substrate__composite_overdetermined_reading, suppression_requirement, 90, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__composite_overdetermined_reading, identity_coordination).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__composite_overdetermined_reading, honor_satisfaction_substrate__cultural_contraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'why did dueling decline?' conflates three structurally distinct claims and is decomposed per the epsilon-invariance principle into three stories sharing the kernel honor_satisfaction_substrate. practice_decline_reading models the substrate as intact under exogenous enforcement (lowest contestation, strongest documentary base — statute and court records); cultural_contraction_reading models the substrate as foundationally transformed (most contested, thinnest direct evidence, richest interpretive apparatus); this composite reading models the two mechanisms as jointly operative and causally entangled. The upstream story (practice_decline, highest empirical confidence) influences the downstream cultural account because the enforcement record is routinely cited as evidence about the substrate's condition. Each file carries its own epsilon, beneficiaries, and victims; the family is linked through affects_constraints in all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
