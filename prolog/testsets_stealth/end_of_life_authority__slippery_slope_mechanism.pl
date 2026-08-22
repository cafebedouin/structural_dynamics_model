% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: Assisted-Dying Eligibility Expansion Dynamic (Slippery-Slope Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   Autonomy-based assisted-dying frameworks were founded as tightly bounded
 *   exceptions: competent adults, terminal and irremediable conditions,
 *   waiting periods, independent review. Across the leading jurisdictions the
 *   operative perimeter has since moved — to non-terminal chronic suffering
 *   (track-two criteria), to patients without contemporaneous consent
 *   (advance directives, substituted judgment, pediatric protocols in two
 *   jurisdictions), and toward mental illness as an eligible ground. This
 *   story instantiates the slippery_slope_mechanism reading of the
 *   end_of_life_authority kernel: the claim that the expansion is not
 *   accidental but structural — that once autonomy-plus-unbearable-suffering
 *   grounds access, no principled criterion holds the line at the founding
 *   boundary, and the interpretive layer (courts, review commissions,
 *   evaluation cycles) absorbs the drift without ever formally repudiating
 *   the founding terms. KEY AGENTS (by structural relationship): -
 *   competent_terminal_patients: intended beneficiary (powerless/constrained)
 *   — receives the supervised path the framework was built to provide -
 *   assisted_dying_providers: administering gatekeeper (organized/mobile) —
 *   assesses, performs, documents; dual-positioned collector -
 *   courts_and_review_commissions: expansion authority
 *   (institutional/constrained) — interprets criteria outward, refers almost
 *   nothing - founding_legislatures: containment author
 *   (institutional/constrained) — wrote bounds it has not been able to
 *   restore - incompetent_eligible_patients: primary target
 *   (powerless/trapped) — bound by determinations they cannot affirm or
 *   refuse - non_terminal_chronic_sufferers: target with dual position
 *   (moderate/constrained) — some seek admission; the class carries the
 *   reclassification - vulnerable_disabled_persons: target
 *   (powerless/trapped) — support deprivation pushes requests the criteria
 *   accept - public_health_insurers: fiscal collector
 *   (institutional/constrained) — books avoided downstream care costs -
 *   burdened_family_caregivers: incidental beneficiary (moderate/mobile) -
 *   disability_rights_advocates, palliative_care_clinicians: excluded voices
 *   (organized/mobile) — thin representation in founding design -
 *   national_bioethics_commissions: analytical observer
 *   (institutional/analytical) — documents the drift, decides nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.68).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.61).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.56).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.56).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "Assisted-Dying Eligibility Expansion Dynamic (Slippery-Slope Reading)").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, 'bfb27ee3-5f75-43cb-af5d-8afb5b55edd8').
narrative_ontology:cs_kernel_codification('bfb27ee3-5f75-43cb-af5d-8afb5b55edd8', formalized).
narrative_ontology:cs_authority_grounding('bfb27ee3-5f75-43cb-af5d-8afb5b55edd8', lineage).
narrative_ontology:cs_interpretation_layer_present('bfb27ee3-5f75-43cb-af5d-8afb5b55edd8').
narrative_ontology:cs_reading_relation('bfb27ee3-5f75-43cb-af5d-8afb5b55edd8', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('bfb27ee3-5f75-43cb-af5d-8afb5b55edd8', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_axiom('bfb27ee3-5f75-43cb-af5d-8afb5b55edd8', foundational, eligibility_boundaries_lack_principled_containment).
narrative_ontology:cs_axiom_status(eligibility_boundaries_lack_principled_containment, holdable).
narrative_ontology:cs_axiom_grounding('bfb27ee3-5f75-43cb-af5d-8afb5b55edd8', eligibility_boundaries_lack_principled_containment, empirically_contingent).
narrative_ontology:cs_axiom('bfb27ee3-5f75-43cb-af5d-8afb5b55edd8', secondary, safeguards_decay_under_expansion_pressure).
narrative_ontology:cs_axiom_status(safeguards_decay_under_expansion_pressure, holdable).
narrative_ontology:cs_axiom_grounding('bfb27ee3-5f75-43cb-af5d-8afb5b55edd8', safeguards_decay_under_expansion_pressure, empirically_contingent).
narrative_ontology:cs_reference_frame('bfb27ee3-5f75-43cb-af5d-8afb5b55edd8', bounded_terminal_exception_settlement).
narrative_ontology:cs_drift_state('bfb27ee3-5f75-43cb-af5d-8afb5b55edd8', contemporary_post_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bfb27ee3-5f75-43cb-af5d-8afb5b55edd8', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, assisted_dying_providers).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, public_health_insurers).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, burdened_family_caregivers).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_eligible_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, non_terminal_chronic_sufferers).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, vulnerable_disabled_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, non_terminal_chronic_sufferers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adults with terminal diagnoses and intact decision-making capacity who request the service the framework provides. They receive a legally supervised path to controlling the timing and manner of death that would otherwise exist only as covert practice or unassisted suicide. They may decline at any point up to administration; what they cannot do is extend the timeline their disease sets.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients, beneficiary,
    powerless, immediate, constrained, national).

% Physicians and nurse practitioners who assess eligibility, administer the procedure, and document compliance. They gatekeep access case by case, collect professional fees, and in several jurisdictions have formed dedicated provision practices. They can withdraw from provision under conscience protections, though withdrawal shifts caseloads onto colleagues and lengthens waits.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, assisted_dying_providers, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, assisted_dying_providers, beneficiary).

% Single-payer and public insurance bodies that finance the procedure and the care paths around it. Independent fiscal analyses in several jurisdictions project net savings when eligible patients choose earlier death over extended hospital or long-term care, and ministry submissions weigh these figures during eligibility debates. They do not administer the clinical program; they fund its context and book its downstream relief.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, public_health_insurers, beneficiary,
    institutional, generational, constrained, national).

% Relatives carrying years of physical and financial caregiving for chronically ill family members. Some advocate for wider eligibility partly from exhausted compassion; the framework shortens caregiving duration when an eligible member chooses death. They hold no formal role in the program's administration and bear no assessment duties.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, burdened_family_caregivers, beneficiary,
    moderate, biographical, mobile, national).

% People rendered eligible without contemporaneous consent — through advance directives written before dementia onset, substituted judgment by proxies, or (in two jurisdictions) childhood conditions assessed jointly by physicians and parents. They cannot affirm or refuse the determination at the moment it is executed; their protection rests entirely on criteria written by others years earlier, and no act of their own can revoke what their prior signature or another's judgment sets in motion.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_eligible_patients, payer,
    powerless, biographical, trapped, national).

% Patients with disabling chronic psychiatric or physical conditions whose suffering is enduring but whose deaths are not reasonably foreseeable. Track-two style criteria admit some of them, and a visible minority litigate and campaign for admission; the class as a whole carries the consequence that treatable chronic misery acquires a state-sanctioned exit competing with treatment investment. Any member may decline personally; none can decline the reclassification of their condition.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, non_terminal_chronic_sufferers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, non_terminal_chronic_sufferers, beneficiary).

% Disabled, impoverished, and socially isolated people whose documented reasons for requesting death include lack of housing, support services, and income — factors the eligibility assessment treats as background rather than disqualifying. Published cases include applicants who stated they chose death because acceptable living conditions were unavailable. Their practical alternative set is narrowed by the same support shortfalls that push them toward the request.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, vulnerable_disabled_persons, payer,
    powerless, biographical, trapped, national).

% The legislatures that wrote the original bounded statutes limiting eligibility to competent adults with terminal, irremediable conditions, with waiting periods and independent review. They retain formal amendment power but operate under constitutional rulings and sustained advocacy that make re-narrowing politically and legally costly; several have instead widened criteria, and none has durably restored its founding perimeter.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, founding_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Constitutional courts and statutory review bodies that interpret the eligibility criteria, hear challenges brought by excluded applicant classes, and decide whether to refer irregular cases for sanction. Their interpretations have moved the effective boundary from terminal to non-terminal suffering and toward advance-directive access; referral rates for irregular cases run near zero in the longest-standing jurisdictions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, courts_and_review_commissions, agenda_setter,
    institutional, generational, constrained, national).

% Organizations of disabled people who oppose eligibility expansion and were thinly represented in the founding legislative consultations. They testify, litigate, and document individual cases; they hold no seat in eligibility design or on several review bodies, which they identify as the reason criteria discount support deprivation instead of treating it as disqualifying.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, disability_rights_advocates, excluded,
    organized, generational, mobile, national).

% Hospice and palliative specialists who were promised concurrent funding investments alongside legalization in several jurisdictions. The promised investments arrived late or partially; they argue the framework's availability substitutes for adequate symptom-control infrastructure and that they were consulted late in the drafting rounds that fixed the criteria.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, palliative_care_clinicians, excluded,
    organized, biographical, mobile, national).

% Standing advisory bodies that produce periodic evaluations of practice volumes, consent modalities, and criteria application. They inform legislatures and review bodies but decide nothing; their reports are the main public record of how far practice has moved from the founding terms.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, national_bioethics_commissions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__slippery_slope_mechanism, public_health_insurers).
narrative_ontology:fixing_cost_class(end_of_life_authority__slippery_slope_mechanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels requests for hastened death through medical assessment, documentation, waiting periods, and review instead of leaving them to covert physician action or unassisted suicide — solving, once and centrally, the oversight problem that a flat prohibition never solved.
% TRANSFER_FUNCTION: Moves decision authority over the timing and manner of death from a universal legal prohibition to certified eligible patients and their assessing providers; moves procedure volume and avoided downstream care expenditure through the public health system; and, as criteria widen, moves legal protection away from classes never consulted — the incompetent, the non-terminal, and the support-deprived.
% ABSENT_VOICES: Incompetent future patients cannot appear in any consultation. Disability-rights organizations and palliative-care clinicians were thinly represented in the founding design rounds (Quebec's select committee, early Benelux drafting) and hold no seats on several review bodies; they would contest criteria that treat support deprivation as background context rather than disqualifying.
% DISAPPEARANCE_RATIONALE: Overnight disappearance dissolves dedicated provider practices, voids advance-directive instruments, stands down the review apparatus, and returns requests to covert channels and unassisted suicide; legislatures would face immediate pressure to reconstruct either prohibition or regulation, and the surrounding medical-legal economy — training, protocols, review staffing — would reorganize around whichever they chose.
% FOUNDING_PROBLEM: Clandestine physician-assisted dying and desperate unassisted suicides among terminally ill patients, occurring outside all oversight, with physicians exposed to prosecution and patients to unmanaged deaths. The founding settlement exchanged a bright-line prohibition for a tightly bounded, medically supervised exception: competent adults, terminal and irremediable conditions, waiting periods, independent review.
% FOUNDING_PROBLEM_CORROBORATION: Pre-legalization epidemiology (the van der Maas-era Dutch surveys of clandestine practice) and Oregon health-authority participation reports corroborate the founding problem from outside the benefiting parties. On status: successive Dutch five-yearly evaluations and Canadian parliamentary reviews attest that the operative perimeter now exceeds the founding scope — again attestation from outside the beneficiary set — while provider associations and autonomy advocates dispute that anything is amiss, describing the movement as evolution rather than departure.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68: high but not snare-grade, because the core service continues to be delivered as promised to the competent-terminal class while criteria drift transfers unconsented exposure and reclassification costs onto classes that never agreed to them. Suppression is authored at 0.61 as a raw structural property — constitutional entrenchment and the interpretive ratchet foreclose durable re-narrowing, and holding the expanded perimeter requires reporting regimes, review staffing, and conscience-rule management — and it is deliberately NOT scaled here; only extractiveness is scaled by the engine. Theater_ratio at 0.56 reflects a safeguard subsystem whose legitimating function increasingly outruns its binding function: near-zero referral rates for irregular cases in the longest-standing jurisdictions, and review approval patterns that track applications rather than screen them. Accessibility_collapse at 0.58: once the autonomy-plus-suffering rationale is accepted, principled containment alternatives collapse substantially — the rationale generalizes — but political containment persists in some jurisdictions, so collapse is incomplete. Resistance at 0.60: sustained organized opposition (disability advocacy, palliative sectors, religious institutions) that raises enforcement costs without reversing the drift. The three temporal series run on one shared seven-point grid (t=0..30) so every metric is authored at every examined time point; trajectories are monotonic ratchets, not cycles — the mechanism is cumulative reinterpretation, not intermittent reinforcement. The claim (tangled_rope) and the metrics are authored independently: the engine computes per-seat types from the structural data, and any divergence between this claim and computed verdicts is the measurement the corpus exists to take.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. Competent_terminal_patients experience the framework as a delivered promise — from their seat the arrangement looks like pure coordination. Incompetent_eligible_patients experience binding without consent — from their seat the same structure operates as unappealable imposition. Founding_legislatures watch their authored settlement dissolve through interpretation they did not authorize; courts_and_review_commissions experience the same movement as principled fulfillment of the rationale. Public_health_insurers see fiscal relief; vulnerable_disabled_persons see the exit that support deprivation made rational. The engine derives these divergent classifications from the declared positions and exits; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster at the low-d end: competent_terminal_patients are subsidized by the framework's existence (full service, no imposed cost); public_health_insurers collect fiscal relief passively; burdened_family_caregivers benefit incidentally. Victims cluster at the high-d end, amplified by trapped exits: incompetent_eligible_patients sit nearest the full-target position — no contemporaneous consent and no act available to revoke the determination — with vulnerable_disabled_persons close behind (trapped by the same support shortfalls that generate their requests) and non_terminal_chronic_sufferers somewhat lower because a minority of the class actively seeks admission. Assisted_dying_providers are dual-positioned: they administer and collect, bearing professional risk and moral load, placing them nearer the symmetric range than a pure collector. The excluded seats carry no directionality weight but document the consensus-provenance gap: the unanimity behind each widening was produced in rooms the disability and palliative seats never occupied.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim guards against two symmetrical mislabels. Reading the arrangement as pure rope (the autonomy advocates' framing) erases the asymmetric, unconsented costs the expansion imposes; reading it as pure snare (the abolitionist framing) denies the real, continuing delivery to the founding class — the coordination story is not cover. On mandatrophy: the founding mandate (a bounded terminal exception) has been overtaken by practice, but the founding problem retains a live residue — clandestine practice and unsupported terminal distress persist at the margins — and the parties genuinely dispute obsolescence, so mandatrophy_resolved is left undeclared and founding_problem_status is authored as contested. The R5 mismatch consumer should find status=contested crossed with verdict=world_rearranges: no zombie flag, correctly, because the function has not atrophied — it has migrated. The piton risk is real but localized: theater_ratio 0.56 approaches performative majority for the safeguard subsystem specifically, while the clinical core remains functional. If criteria widen until assessment becomes formality — theater_ratio above roughly 0.7 with the clinical core hollowed — expect a transition signal toward piton or snare, and the fiscal-capture finding (gain_flow naming a seat, fixing_cost prohibitive) is the channel through which that transition would run. Coalition note: the victim classes are individually powerless but not inherently coalition-incapable — disability-rights organization demonstrates collective capacity — yet the framework's design channels that capacity into exclusion from agenda-setting rather than veto power, which is why resistance registers at 0.60 without registering in the criteria.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates the slippery_slope_mechanism reading of the end_of_life_authority kernel; what structurally changes under the sibling readings?',
    'Compare the compiled sibling stories: autonomy_reading authors the same arrangement with a stable competent-requester beneficiary set and low epsilon; sanctity_reading authors it with a universal victim set and maximal suppression. The disagreement is located in whether eligibility boundaries are stable or ratchet-prone, and in who counts as harmed.',
    'Under autonomy_reading the victim set empties (no death without contemporaneous consent) and the computed type moves toward rope; under sanctity_reading victims universalize and the computed type moves toward snare. This reading''s tangled_rope verdict survives only while the expansion record holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings instantiate different constraints from the same commitment.').

omega_variable(
    containment_intrinsic_or_contingent,
    'Is boundary expansion intrinsic to autonomy-grounded eligibility rationales, or contingent on particular institutional designs (judicial supremacy, thin review, evaluation-driven practice norms)?',
    'Comparative jurisdiction analysis: strict-statute US states that have held terminal-only lines for decades versus constitutionalized rights (Canada) and evaluation-driven regimes (Benelux); test whether design variables predict containment.',
    'If intrinsic, the tangled_rope verdict drifts toward snare as a structural regularity; if contingent, entrenched criteria and supermajority amendment rules could stabilize a rope-type settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(containment_intrinsic_or_contingent, empirical, 'Whether the expansion dynamic is a law of the rationale or an artifact of design.').

omega_variable(
    incompetent_consent_materiality,
    'How many deaths under the framework occur without contemporaneous consent (advance directives, substituted judgment, pediatric protocols), and is that class growing?',
    'Audit review-commission case files stratified by consent modality across jurisdictions and years.',
    'If the incompetent class is marginal and static, its victim declaration overstates epsilon; if growing, epsilon understates the trajectory and the snare-drift hypothesis strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incompetent_consent_materiality, empirical, 'Materiality of the non-consenting eligible class to the extraction measure.').

omega_variable(
    fiscal_gravity_causality,
    'Does cost pressure causally drive eligibility expansion, or do fiscal savings merely accompany it?',
    'Legislative trace analysis of amendment debates (parliamentary committee records on track-two creation, Dutch evaluation cycles) coding fiscal arguments against autonomy-consistency arguments.',
    'Determines whether public_health_insurers is a driver seat shaping the constraint''s direction or an incidental collector; recalibrates the gain_flow reading and the expansion forecast.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_gravity_causality, empirical, 'Causal weight of fiscal gravity in the eligibility-ratchet mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.3).
narrative_ontology:measurement(end__tr_t5, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 5, 0.33).
narrative_ontology:measurement(end__tr_t10, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 10, 0.38).
narrative_ontology:measurement(end__tr_t15, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 15, 0.43).
narrative_ontology:measurement(end__tr_t20, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 20, 0.47).
narrative_ontology:measurement(end__tr_t25, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 25, 0.52).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 30, 0.56).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(end__be_t5, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(end__be_t10, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(end__be_t15, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(end__be_t20, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(end__be_t25, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 25, 0.63).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(end__su_t5, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 5, 0.39).
narrative_ontology:measurement(end__su_t10, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(end__su_t15, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(end__su_t20, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(end__su_t25, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 25, 0.57).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 30, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).

% DUAL FORMULATION NOTE:
% Kernel-family decomposition: the colloquial label 'the euthanasia debate' conflates three structurally distinct claims that share one contested kernel (end_of_life_authority). This file instantiates the slippery_slope_mechanism reading — an empirical claim about framework dynamics — and therefore carries its own epsilon, victim set, and type. The autonomy_reading authors the same arrangement as stable coordination with a competent-requester beneficiary set; the sanctity_reading authors it as categorically impermissible regardless of consent. Per the epsilon-invariance principle these are separate stories linked here and via cs_structure.reading_relations, not one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
