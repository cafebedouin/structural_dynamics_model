% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: Autonomy-Grounded End-of-Life Authority Framework
 *   domain: medical ethics/bioethics/end-of-life policy
 *
 * SUMMARY:
 *   The standing arrangement under contest is the family of statutory
 *   assisted-dying frameworks — Oregon-lineage US statutes, the Benelux laws,
 *   Canada's MAiD regime — that operationalize individual autonomy as the
 *   ground of authority over the circumstances and timing of death, bounded
 *   by certified eligibility (terminality or irremediability, decisional
 *   capacity, sustained voluntary request) and administered through physician
 *   gatekeeping and post-hoc review. This file authors ONE reading of the
 *   shared end_of_life_authority kernel cleanly: epsilon is authored from the
 *   autonomy seat, over this standing arrangement only, and is not averaged
 *   against sibling readings (which are separate files linked in the network
 *   block). The framework delivers a real coordinated pathway — lawful
 *   access, physician protection, state accountability — while its
 *   eligibility boundary denies the same pathway to sufferers who fail the
 *   criteria, its availability generates ambient pressure on vulnerable
 *   patients, and its enforcement machinery matures and hardens over the
 *   interval. The claim/metric gap is deliberate: the reading CLAIMS a
 *   coordination-centered structure while the authored metrics describe a
 *   hybrid with substantial, actively enforced asymmetry.
 *
 * KEY AGENTS:
 *   - eligible_dying_patients: primary beneficiary (moderate/trapped) — receives the authorized pathway the framework opens
 *   - ineligible_suffering_patients: primary target (powerless/trapped) — bears denial of the same pathway
 *   - vulnerable_pressured_patients: secondary target (powerless/trapped) — bears the ambient pressure the framework's availability generates
 *   - participating_physicians: gatekeeping beneficiary (institutional/mobile) — collects professional jurisdiction and administers eligibility at case level
 *   - conscientious_objecting_physicians: compelled contributor (moderate/constrained) — bears referral mandates and career friction
 *   - grieving_family_members: incidental beneficiary with carried burden (moderate/constrained)
 *   - disability_rights_advocates: organized opponent (organized/identity_locked) — contests expansion, absorbs the risk externalities
 *   - religious_institutions: overridden objector (institutional/identity_locked) — outvoted in adopting jurisdictions
 *   - legislative_judicial_authorities: agenda setter (institutional/mobile) — draws and redraws the eligibility boundary
 *   - oversight_review_bodies: administrative agenda setter (institutional/constrained) — runs the compliance apparatus
 *   - public_health_payers: fiscal beneficiary (institutional/mobile) — books the substitution savings
 *   - bioethics_analysts: analytical observer (analytical/analytical) — maps the structure for every other seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.58).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.62).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Autonomy-Grounded End-of-Life Authority Framework").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical ethics/bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, 'ad51010b-ae3b-4cde-8c47-4c9f83fce24c').
narrative_ontology:cs_kernel_codification('ad51010b-ae3b-4cde-8c47-4c9f83fce24c', formalized).
narrative_ontology:cs_authority_grounding('ad51010b-ae3b-4cde-8c47-4c9f83fce24c', expertise).
narrative_ontology:cs_interpretation_layer_present('ad51010b-ae3b-4cde-8c47-4c9f83fce24c').
narrative_ontology:cs_reading_relation('ad51010b-ae3b-4cde-8c47-4c9f83fce24c', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad51010b-ae3b-4cde-8c47-4c9f83fce24c', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('ad51010b-ae3b-4cde-8c47-4c9f83fce24c', foundational, self_determination_covers_timing_and_manner_of_death).
narrative_ontology:cs_axiom_status(self_determination_covers_timing_and_manner_of_death, holdable).
narrative_ontology:cs_axiom_grounding('ad51010b-ae3b-4cde-8c47-4c9f83fce24c', self_determination_covers_timing_and_manner_of_death, deontological).
narrative_ontology:cs_axiom('ad51010b-ae3b-4cde-8c47-4c9f83fce24c', secondary, regulated_access_dominates_prohibition_on_safety).
narrative_ontology:cs_axiom_status(regulated_access_dominates_prohibition_on_safety, holdable).
narrative_ontology:cs_axiom_grounding('ad51010b-ae3b-4cde-8c47-4c9f83fce24c', regulated_access_dominates_prohibition_on_safety, instrumental).
narrative_ontology:cs_reference_frame('ad51010b-ae3b-4cde-8c47-4c9f83fce24c', individual_self_determination_supremacy).
narrative_ontology:cs_drift_state('ad51010b-ae3b-4cde-8c47-4c9f83fce24c', contemporary_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ad51010b-ae3b-4cde-8c47-4c9f83fce24c', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, eligible_dying_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, participating_physicians).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, grieving_family_members).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, public_health_payers).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, ineligible_suffering_patients).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, vulnerable_pressured_patients).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, conscientious_objecting_physicians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, grieving_family_members).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, disability_rights_advocates).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, individual_autonomy_principle).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, bodily_self_determination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Carry a terminal diagnosis with a prognosis inside the statutory window. May petition for physician-prescribed life-ending medication, passing through capacity evaluations, written and oral requests, mandatory waiting periods, and residency requirements. Most approved requests end in death by the prescribed means; a minority never fill the prescription, treating approval itself as reassurance. Declining the process means enduring the underlying disease without the option.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, eligible_dying_patients, beneficiary,
    moderate, biographical, trapped, national).

% Experience suffering they judge unbearable but fall outside the criteria — chronic non-terminal illness, psychiatric torment in jurisdictions that exclude it, or lost capacity after drafting an advance directive they can no longer invoke. No lawful route to assistance exists for them; anyone who assists anyway faces prosecution. Some travel to foreign clinics where residency rules permit; most simply continue living in the condition the law declines to relieve.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, ineligible_suffering_patients, payer,
    powerless, biographical, trapped, national).

% Older, disabled, or economically precarious patients whose requests arise amid inadequate home care, isolation, or fear of being a burden. Formal voluntariness screening asks about coercion but not about the absence of good alternatives; documented cases exist of assisted death raised alongside denials of funded care or housing support.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, vulnerable_pressured_patients, payer,
    powerless, biographical, trapped, national).

% Certify eligibility, prescribe or administer, and file the mandated reports. The practice confers a recognized professional role with legal immunity inside the procedure's bounds, and in several systems a billable service. It also carries documented moral injury rates, disciplinary exposure for procedural lapses, and the gatekeeping labor of judging whose suffering qualifies.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, participating_physicians, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, participating_physicians, agenda_setter).

% Decline to participate on conscience grounds. In some jurisdictions they must still refer patients to willing colleagues; professional bodies and employers press participation, and objectors report career friction in specialties and regions where the practice is routine.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, conscientious_objecting_physicians, payer,
    moderate, biographical, constrained, national).

% Witness the death and often support the request through the process. Many describe gratitude for a peaceful ending on the patient's terms; some carry the weight of presence at the act itself, and a minority hold financial interests that conflict-of-interest screens reach only partially.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, grieving_family_members, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, grieving_family_members, payer).

% Organize against adoption and expansion, arguing that legalized assistance reshapes social attitudes toward disabled lives and converts inadequate support into tacit pressure to die. They testify at every major hearing, litigate where standing allows, and lose most expansion votes while shifting amendments at the margin.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, disability_rights_advocates, payer,
    organized, generational, identity_locked, national).

% Hold that intentional life-ending is never permissible and campaign against adoption and expansion alike. In jurisdictions that adopted the framework their position was outvoted; they retain influence through conscience protections, funding of palliative alternatives, and electoral politics in non-adopting jurisdictions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, religious_institutions, excluded,
    institutional, civilizational, identity_locked, global).

% Enacted the statutes and continue to redraw the boundaries: courts have struck down blanket prohibitions and struck down or narrowed individual safeguards; legislatures have added non-terminal tracks, shortened waiting periods for the imminently dying, and scheduled further widening. Each revision restarts the contest.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, legislative_judicial_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Collect compliance reports, audit cases after the fact, and publish annual statistics. Their reviews almost never find violations; critics read the near-perfect compliance record as evidence that the review function confirms rather than scrutinizes.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, oversight_review_bodies, agenda_setter,
    institutional, biographical, constrained, national).

% Finance the health system. Budget analyses in adopting jurisdictions project savings where an earlier assisted death substitutes for weeks of acute or long-term care; the savings accrue without any corresponding earmark for the alternatives patients lack.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, public_health_payers, beneficiary,
    institutional, generational, mobile, national).

% Track the jurisprudence, the eligibility creep, and the request-motivation literature across jurisdictions. They produce the comparative datasets legislatures cite and the critiques advocates quote; they hold no vote anywhere.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, bioethics_analysts, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__autonomy_reading, participating_physicians).
narrative_ontology:fixing_cost_class(end_of_life_authority__autonomy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Makes intentional death jointly possible and accountable: patients need a lawful route, physicians need protection from homicide liability, and the state needs a reviewable record — the framework supplies one procedure satisfying all three instead of leaving each party to clandestine improvisation.
% TRANSFER_FUNCTION: Moves decisional authority over the timing and manner of death from the state's criminal-law monopoly to qualifying individuals acting through certified physicians; moves compliance labor (requests, assessments, waiting periods, reporting) from patients into the medical-legal apparatus; and, as a fiscal side-effect, moves end-of-life expenditure from extended acute care toward earlier death, accruing to public payers.
% ABSENT_VOICES: Religious institutions hold a formal seat in hearings but no vote in adopting jurisdictions — their position was settled by prior majorities. Future cohorts of the currently-ineligible (psychiatric sufferers in excluding jurisdictions, dementia patients drafting directives they may never invoke) have no seat when criteria are drawn. Residents of non-adopting jurisdictions who would travel abroad are represented nowhere in the destination's review.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, requests would not stop: patients would return to violent suicide, covert overdoses, and unregulated underground assistance; physicians would face homicide exposure for acts of mercy; the review statistics, case law, and professional norms built over three decades would unwind, and every jurisdiction would re-run the founding fight from the beginning.
% FOUNDING_PROBLEM: The cruelty of prohibited escape from unbearable terminal suffering: before lawful pathways existed, terminal patients died by violent suicide or covert overdose, and the relatives and physicians who helped them faced prosecution.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: pre-statute mortality reviews (Oregon health-department baseline surveys, Dutch regional review archives) document terminal patients dying violently or by covert overdose before lawful pathways existed; hospice and palliative-care journals attest refractory suffering as a clinical reality; and institutional opponents — national bishops' conferences, disability-rights litigation filings — concede the occurrence of unbearable suffering while disputing the lawful response. No corroborating source attests that the framework's CURRENT scope matches the founding problem; that gap is the live contest.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the framework's residue of extraction has shifted composition over the interval — early denial-of-choice extraction narrows as criteria widen, while inclusion-side extraction (procedural burden on all seekers, ambient pressure on the powerless, criminalization of every extra-framework route) grows with the framework's reach; the net rises modestly and plateaus. Suppression 0.62: persistence depends on active machinery — criminalization of unauthorized assistance, referral mandates on objectors, professional discipline, reporting regimes — not on participant preference. Theater 0.40 and rising: the review layer increasingly confirms rather than scrutinizes (near-zero violation findings), while capacity assessment retains real filtering function. Accessibility_collapse 0.5: palliative care, hospice, and treatment refusal remain live alternatives, but every assistance route outside the framework is closed. Resistance 0.6: each expansion is re-litigated against organized religious and disability opposition. The three temporal series share one eight-point grid (T0=1997 Oregon implementation, Tn=2025) so every metric is authored at every examined time point; the trajectories are monotonic, not cyclical — no intermittent-reinforcement mechanism is alleged. The suppression_requirement series is authored because this story specifically tracks enforcement-capacity build-up (review committees, reporting infrastructure, referral rules maturing over the interval), not merely extraction shift. Receipt surface: the decisional-control gain demonstrably accrues to the medical profession as gatekeeper (named seat), with a parallel fiscal stream to public payers discussed in directionality_logic; fixing_cost is 'cheap' because the adjustment mechanism is proven — legislatures and courts have repeatedly amended the boundary at bearable political cost, each amendment relieving part of the denial extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural inputs. Two patients in adjacent beds, differing only in diagnosis code, occupy opposite positions: one receives the pathway, one is denied it — same powerlessness, same trapped exit, opposite directionalities. The physician seat holds both collection and administration: the profession gains jurisdiction over death while individual practitioners absorb moral injury and disciplinary exposure. The payer seat experiences the entire arrangement as a budget line. The agenda-setter seat experiences the boundary as a dial it turns; the ineligible sufferer experiences the same dial as a wall. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionalities: eligible patients (trapped, so amplified toward the beneficiary end despite weak individual power), participating physicians, families, and public payers all sit near the subsidized end. Victim declarations drive high directionalities: ineligible sufferers and vulnerable pressured patients are powerless and trapped — the strongest target profile available — and objecting physicians are targets of the referral mandate specifically. Suppression is authored as a raw structural property and is deliberately NOT scaled; only extractiveness is scaled by directionality and scope in the engine's computation. Known compression, accepted rather than overridden: the derivation reads grieving_family_members purely from the beneficiary tag, understating their carried burden and minority financial entanglement, and reads participating_physicians likewise, understating their exposure and gatekeeping labor; power-atom-keyed overrides were rejected because both atoms (moderate, institutional) are shared by agents with opposite true positions, so an override would contaminate the seats it does not name.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — prohibited escape from unbearable terminal suffering — is live and externally corroborated, so this is not a vestigial arrangement kept alive by inertia; no mandatrophy resolution is declared. The theater signal is real but localized: it marks drift inside the oversight sublayer (review becoming confirmation), not atrophy of the whole structure, and the piton signature is blocked by the concentrated professional capture of gains. The classification work this story performs is bidirectional: it prevents mislabeling the framework as pure extraction (the coordination core — lawful pathway, physician protection, accountable record — is genuine and heavily used) and prevents mislabeling it as pure coordination (the boundary denial, the pressure climate on the powerless, and the criminalized exits are equally structural). The rising theater_ratio functions as an early-warning tripwire: if the safeguard layer completes its drift to performance while the boundary keeps ratcheting outward, the hybrid degrades toward the mechanism sibling's predicted configuration, and the temporal series will date that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the end_of_life_authority kernel; which structural elements would the sibling readings replace, and where exactly does the disagreement bite?',
    'Side-by-side compilation of the three readings'' victim sets and authority loci: the sanctity reading relocates wrongdoing to the assisting physician and protection to the requesting patient; the mechanism reading keeps this reading''s structure but predicts its boundary dissolves. The disagreement is located in the locus of judgment over unbearable suffering and in whether a principled eligibility line exists at all.',
    'Adopting the sanctity reading inverts this file''s victim set (assisting physicians become the harmed class, requesting patients the protected one); confirming the mechanism reading merges this file''s victim set with ever-wider classes and pushes the arrangement toward the mechanism reading''s own configuration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three readings of a shared kernel; sibling substitutions and the locus of disagreement.').

omega_variable(
    suffering_authority_location,
    'Who decides that suffering is unbearable — the patient''s report alone, as the autonomy premise implies, or clinician certification, as every operating statute requires?',
    'Compare outcomes where assessment standards differ (patient-reported scales versus clinician-judged irremediability) across jurisdictions; track overturned physician assessments and refused requests on report-alone grounds.',
    'If clinician judgment is load-bearing, the framework''s practice departs from its own foundational premise and the practice-drift finding deepens; if patient report suffices in effect, the drift is nominal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suffering_authority_location, conceptual, 'Location of definitional authority over unbearable suffering — the precise seam between this reading''s premise and its statutory operation.').

omega_variable(
    eligibility_boundary_stability,
    'Is the competency-plus-terminality-plus-voluntariness boundary a principled moral line or an administrative artifact that erodes under litigation and precedent?',
    'Cross-jurisdiction panel over the full interval (Benelux, Oregon-lineage US states, Canada): catalogue every boundary element, date each relaxation, and test whether any element has ever been tightened after being loosened.',
    'If no element survives unrelaxed anywhere, the boundary is transitional and the framework''s long-run shape approaches unconditional permission; durable elements would stabilize the hybrid configuration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_boundary_stability, empirical, 'Whether the eligibility boundary is a stable line or a ratchet.').

omega_variable(
    request_motivation_decomposition,
    'Are requests traceable to autonomous settled preference, or to perceived burden, untreated symptoms, and unavailable care — and in what proportion?',
    'Structured psychosocial follow-up of requesters and matched non-requesters, controlling for palliative access; motivation inventories administered at first and final request.',
    'A large burden-driven share raises effective extraction on powerless seats above what formal voluntariness shows, and indicates the safeguards screen the wrong variable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(request_motivation_decomposition, empirical, 'Decomposition of request motivations between autonomy and circumstance.').

omega_variable(
    safeguard_filter_vs_rubber_stamp,
    'Do capacity assessments, waiting periods, and second opinions filter out unsuitable cases, or do they ritually confirm nearly all requests?',
    'Audit rejection and amendment rates across review bodies; compare case outcomes before and after safeguard-tightening episodes; interview assessors outside the reporting chain.',
    'High confirmation rates would push the theater ratio above the authored end-state value, dating a drift toward performative maintenance inside the enforcement layer specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safeguard_filter_vs_rubber_stamp, empirical, 'Functionality of the safeguard apparatus versus its confirmatory performance.').

omega_variable(
    expansion_endpoint_equilibrium,
    'Does the recorded expansion sequence converge to a stable equilibrium (all competent adults, any condition) or continue past volition into non-voluntary practice?',
    'Long-horizon Benelux cohort data plus monitoring for any jurisdiction extending to minors or incompetence-without-request; pre-registered indicators distinguishing equilibrium from continuation.',
    'Equilibrium stabilizes this file''s classification; continuation validates the mechanism sibling''s prediction and forces re-authoring of the victim set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(expansion_endpoint_equilibrium, empirical, 'Terminal state of the eligibility-expansion trajectory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eola_autonomy_tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(eola_autonomy_tr_t0, observed).
narrative_ontology:measurement(eola_autonomy_tr_t4, end_of_life_authority__autonomy_reading, theater_ratio, 4, 0.27).
narrative_ontology:measurement_basis(eola_autonomy_tr_t4, observed).
narrative_ontology:measurement(eola_autonomy_tr_t8, end_of_life_authority__autonomy_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(eola_autonomy_tr_t8, observed).
narrative_ontology:measurement(eola_autonomy_tr_t12, end_of_life_authority__autonomy_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement_basis(eola_autonomy_tr_t12, observed).
narrative_ontology:measurement(eola_autonomy_tr_t16, end_of_life_authority__autonomy_reading, theater_ratio, 16, 0.34).
narrative_ontology:measurement_basis(eola_autonomy_tr_t16, observed).
narrative_ontology:measurement(eola_autonomy_tr_t20, end_of_life_authority__autonomy_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(eola_autonomy_tr_t20, observed).
narrative_ontology:measurement(eola_autonomy_tr_t24, end_of_life_authority__autonomy_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement_basis(eola_autonomy_tr_t24, observed).
narrative_ontology:measurement(eola_autonomy_tr_t28, end_of_life_authority__autonomy_reading, theater_ratio, 28, 0.4).
narrative_ontology:measurement_basis(eola_autonomy_tr_t28, observed).

% Extraction over time
narrative_ontology:measurement(eola_autonomy_be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(eola_autonomy_be_t0, observed).
narrative_ontology:measurement(eola_autonomy_be_t4, end_of_life_authority__autonomy_reading, base_extractiveness, 4, 0.47).
narrative_ontology:measurement_basis(eola_autonomy_be_t4, observed).
narrative_ontology:measurement(eola_autonomy_be_t8, end_of_life_authority__autonomy_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement_basis(eola_autonomy_be_t8, observed).
narrative_ontology:measurement(eola_autonomy_be_t12, end_of_life_authority__autonomy_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(eola_autonomy_be_t12, observed).
narrative_ontology:measurement(eola_autonomy_be_t16, end_of_life_authority__autonomy_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement_basis(eola_autonomy_be_t16, observed).
narrative_ontology:measurement(eola_autonomy_be_t20, end_of_life_authority__autonomy_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(eola_autonomy_be_t20, observed).
narrative_ontology:measurement(eola_autonomy_be_t24, end_of_life_authority__autonomy_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement_basis(eola_autonomy_be_t24, observed).
narrative_ontology:measurement(eola_autonomy_be_t28, end_of_life_authority__autonomy_reading, base_extractiveness, 28, 0.58).
narrative_ontology:measurement_basis(eola_autonomy_be_t28, observed).

% Suppression requirement over time
narrative_ontology:measurement(eola_autonomy_su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(eola_autonomy_su_t0, observed).
narrative_ontology:measurement(eola_autonomy_su_t4, end_of_life_authority__autonomy_reading, suppression_requirement, 4, 0.56).
narrative_ontology:measurement_basis(eola_autonomy_su_t4, observed).
narrative_ontology:measurement(eola_autonomy_su_t8, end_of_life_authority__autonomy_reading, suppression_requirement, 8, 0.57).
narrative_ontology:measurement_basis(eola_autonomy_su_t8, observed).
narrative_ontology:measurement(eola_autonomy_su_t12, end_of_life_authority__autonomy_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(eola_autonomy_su_t12, observed).
narrative_ontology:measurement(eola_autonomy_su_t16, end_of_life_authority__autonomy_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement_basis(eola_autonomy_su_t16, observed).
narrative_ontology:measurement(eola_autonomy_su_t20, end_of_life_authority__autonomy_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(eola_autonomy_su_t20, observed).
narrative_ontology:measurement(eola_autonomy_su_t24, end_of_life_authority__autonomy_reading, suppression_requirement, 24, 0.61).
narrative_ontology:measurement_basis(eola_autonomy_su_t24, observed).
narrative_ontology:measurement(eola_autonomy_su_t28, end_of_life_authority__autonomy_reading, suppression_requirement, 28, 0.62).
narrative_ontology:measurement_basis(eola_autonomy_su_t28, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The colloquial label 'the end-of-life debate' conflates three structurally distinct claims sharing one kernel (end_of_life_authority). Decomposed per the epsilon-invariance principle: this file authors the autonomy reading — the permission framework's structure, with denied-choice sufferers in the victim set; end_of_life_authority__sanctity_reading authors the prohibition arrangement, whose victim set inverts to requesting patients and aiding physicians; end_of_life_authority__slippery_slope_mechanism authors the expansion dynamic itself as a constraint. Edges run both directions: this framework's operation supplies the mechanism reading's evidence base, and the sanctity reading supplies the organized resistance that shapes this framework's safeguards.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
