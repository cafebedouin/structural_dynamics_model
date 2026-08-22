% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Autonomy-Grounded Assisted-Dying Framework (End-of-Life Authority, Autonomy Reading)
 *   domain: medical ethics/bioethics/end-of-life policy
 *
 * SUMMARY:
 *   This story instantiates the autonomy_reading of the end_of_life_authority
 *   kernel. The standing arrangement under assessment is the statutory
 *   assisted-dying framework as it operates in enacting jurisdictions
 *   (Oregon-model US states, the Benelux countries, Canada, Colombia):
 *   eligibility gates, capacity and voluntariness assessment, waiting
 *   periods, physician prescription or administration, oversight reporting,
 *   and criminal liability for assistance outside the pathway. From this
 *   reading's own lights the arrangement solves a real collective problem —
 *   it replaces clandestine suicide, prosecuted helpers, and cross-border
 *   desperation with a supervised, verified pathway — while simultaneously
 *   extracting from identifiable classes: sufferers outside the eligibility
 *   lines are denied by the same statutes that serve their neighbors,
 *   procedural burdens tax every seeker, and each expansion of scope exposes
 *   new populations to implicit pressure. Claim and metrics are authored
 *   independently: the reading claims tangled_rope because it holds both
 *   facts true at once; the engine computes per-seat classifications from the
 *   structural data. The colloquial label 'the euthanasia debate' conflates
 *   three structurally distinct claims with different epsilon values; this
 *   file carries only one of them, with siblings linked through the network.
 *   KEY AGENTS (by structural relationship): - eligible_dying_patients:
 *   primary intended beneficiary (powerless/trapped) — receives the
 *   supervised pathway; the option itself functions as relief -
 *   ineligible_suffering_patients: primary target (powerless/constrained) —
 *   bears denial of the same relief at the eligibility line -
 *   maid_providing_physicians: gatekeeper-beneficiary (organized/mobile) —
 *   operates the assessments, collects role and fees, holds a conscience exit
 *   - disabled_people_facing_expansion: exposed class (organized/constrained)
 *   — bears the pressure externalities of each scope expansion -
 *   religious_institutions: displaced authority (organized/identity_locked) —
 *   bears a normative displacement its doctrine forbids it to accept -
 *   enacting_jurisdictions: agenda setter (institutional/constrained) —
 *   writes and amends the gates, receives the centralized decision authority
 *   - parliamentary_review_committees: analytical observer
 *   (institutional/analytical) — gathers evidence, holds no vote
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.46).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.58).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Autonomy-Grounded Assisted-Dying Framework (End-of-Life Authority, Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical ethics/bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, 'add63ac7-80b6-4339-a4c3-bad7ddf53f2f').
narrative_ontology:cs_kernel_codification('add63ac7-80b6-4339-a4c3-bad7ddf53f2f', formalized).
narrative_ontology:cs_authority_grounding('add63ac7-80b6-4339-a4c3-bad7ddf53f2f', expertise).
narrative_ontology:cs_interpretation_layer_present('add63ac7-80b6-4339-a4c3-bad7ddf53f2f').
narrative_ontology:cs_reading_relation('add63ac7-80b6-4339-a4c3-bad7ddf53f2f', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('add63ac7-80b6-4339-a4c3-bad7ddf53f2f', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('add63ac7-80b6-4339-a4c3-bad7ddf53f2f', foundational, self_possession_grounds_death_choice).
narrative_ontology:cs_axiom_status(self_possession_grounds_death_choice, holdable).
narrative_ontology:cs_axiom_grounding('add63ac7-80b6-4339-a4c3-bad7ddf53f2f', self_possession_grounds_death_choice, deontological).
narrative_ontology:cs_axiom('add63ac7-80b6-4339-a4c3-bad7ddf53f2f', secondary, state_may_not_override_competent_end_of_life_choice).
narrative_ontology:cs_axiom_status(state_may_not_override_competent_end_of_life_choice, holdable).
narrative_ontology:cs_axiom_grounding('add63ac7-80b6-4339-a4c3-bad7ddf53f2f', state_may_not_override_competent_end_of_life_choice, conventional).
narrative_ontology:cs_reference_frame('add63ac7-80b6-4339-a4c3-bad7ddf53f2f', competent_individual_death_authority).
narrative_ontology:cs_drift_state('add63ac7-80b6-4339-a4c3-bad7ddf53f2f', post_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('add63ac7-80b6-4339-a4c3-bad7ddf53f2f', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, eligible_dying_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, maid_providing_physicians).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, ineligible_suffering_patients).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, disabled_people_facing_expansion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, religious_institutions).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, bodily_autonomy_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, informed_consent_principle).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, dignity_in_dying_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adults with a qualifying diagnosis who face unbearable suffering and want control over the timing and manner of their death. The framework gives them a lawful route: petitions, capacity assessments, waiting periods, and a physician-supervised death. Access requires satisfying every gate; many report that the option itself functions as relief whether or not they ultimately use it. Leaving the situation is not possible — the alternatives (declining treatment, palliative sedation, travel abroad, unassisted suicide) are the ones they came to the pathway to avoid.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, eligible_dying_patients, beneficiary,
    powerless, immediate, trapped, national).

% People whose suffering is serious and persistent but who fall outside the eligibility lines — non-terminal prognoses, psychiatric suffering where excluded, or residency and procedural disqualifications. Their requests are defined as out of bounds by the same statutes that serve their neighbors, and anyone who would help them faces criminal exposure. Wealthy patients can travel to permissive jurisdictions; the rest choose among continued suffering, refusing food or treatment, and unsafe improvised methods.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, ineligible_suffering_patients, payer,
    powerless, biographical, constrained, national).

% Physicians who assess requests, confirm capacity and voluntariness, and prescribe or administer. The work carries fees, legal protection, and for many a sense of fidelity to patient wishes; it also carries documentation duties, institutional restrictions, and moral residue. They can decline or stop participating under conscience provisions, and many do.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, maid_providing_physicians, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, maid_providing_physicians, agenda_setter).

% People living with disabilities and chronic conditions who are not asking to die but live downstream of each eligibility debate. Advocacy organizations report members describing implicit pressure — messages that their lives are costly or burdensome, care shortages framed as reasons to consider the option. They organize testimony, litigation, and expansion blockage; they cannot exit the society whose rules are being decided.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, disabled_people_facing_expansion, payer,
    organized, generational, constrained, national).

% Bodies committed to the inviolability of life. They oppose legalization and each expansion, seek conscience protections, and run hospice and chaplaincy inside the same systems. Their opposition follows from doctrine they cannot revise without dissolving their own identity, and they experience each jurisdiction that legalizes as a further displacement of their historic authority over death.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, religious_institutions, payer,
    organized, civilizational, identity_locked, global).

% Legislatures, courts, and oversight agencies that write and amend the eligibility lines, waiting periods, and reporting duties, and that defend the statutes in constitutional challenge. They hold the discretionary authority the gate system centralizes, absorb the political cost of every expansion fight, and weigh testimony from every other seat.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, enacting_jurisdictions, agenda_setter,
    institutional, generational, constrained, national).

% Expert panels, royal commissions, and legislative committees that gather evidence, hear witnesses from every camp, and publish recommendations on eligibility and safeguards. They hold no vote and administer nothing; their reports feed the enacting seats.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, parliamentary_review_committees, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__autonomy_reading, enacting_jurisdictions).
narrative_ontology:fixing_cost_class(end_of_life_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Operates a single verified pathway for voluntary death under unbearable suffering: capacity and voluntariness assessment, prescribing and administration standards, waiting periods, and mandatory reporting are solved once, centrally, instead of leaving each dying person to improvise alone and each helper to face prosecution. Third parties gain lawful certainty, and abuse screening is pooled rather than per-case.
% TRANSFER_FUNCTION: Moves decision authority over death from the state, religious authorities, and medical paternalism to the diagnosed individual; moves assessment and administration services through licensed physicians under fee schedules; and, as eligibility widens, moves an increasing share of end-of-life care demand away from prolonged treatment toward planned, supervised death.
% ABSENT_VOICES: Applicants who die during mandatory waiting periods leave no testimony and appear only as statistics; future patients whose conditions pending expansions would cover have no seat yet; severely disabled people unable to self-advocate are represented by organizations rather than speaking directly; and in prohibiting jurisdictions the sufferers whose cases motivate reform are heard posthumously through relatives and court records.
% DISAPPEARANCE_RATIONALE: If the framework vanished overnight, eligible patients would revert to clandestine suicide, unofficially tolerated helpers, or cross-border travel priced beyond most households; physicians would lose a lawful role they currently exercise; oversight apparatuses would dissolve; and the political contest would rearrange around reinstating or re-legislating the pathway rather than calibrating it.
% FOUNDING_PROBLEM: Dying people facing unbearable suffering had no lawful way to control the timing and manner of their death: assistance was criminal everywhere, so the choices were prolonged suffering against their will, violent and solitary suicide, or exposing whoever helped them to prosecution.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: constitutional-court records and legislative hearings preserve first-person sufferer and family testimony; the palliative-medicine literature documents refractory symptom burdens independent of any advocacy; and opponent briefs from religious and disability organizations concede the reality of the suffering while disputing the remedy. No party in the record denies the founding problem exists — the dispute is over the response.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).
:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.46 reflects concentration: the served class receives precisely what it seeks, while the denied class bears the full cost of continued suffering against its will and every seeker pays procedural tolls — waiting periods during which some applicants die, duplicated attestations, residency bars. Suppression 0.58 measures the framework's own coercive force: assistance outside the pathway remains criminal, exercising the right requires state permission layered through multiple officials, and some jurisdictions impose referral obligations on objectors — scored deliberately well below total-prohibition levels because the framework replaced a harsher suppression. Theater 0.32: capacity and voluntariness verification do protective work, while residency requirements and signature layering function largely as obstacle theater. Accessibility collapse 0.45: treatment refusal and palliative sedation remain fully available, travel and improvised methods persist at the margins, but the specific good — a safe, local, medically supervised death — exists only behind the gate. Resistance 0.62: sustained organized opposition from religious bodies, disability-rights organizations, and parts of palliative medicine, recurring repeal attempts, and repeated expansion blockage. The three temporal series share one grid (1997, 2003, 2009, 2015, 2021, 2025) per the alignment rule. Rising base_extractiveness tracks scope expansion: each widening of eligibility grows the governed population faster than it retires gate-denial, and pressure externalities scale with the option's social salience. The suppression_requirement series is authored because enforcement capacity is the traced dynamic: oversight commissions, prior-review requirements, and documentation regimes were built out as scope widened, and the criminal boundary around the pathway is policed more, not less, as the framework grows. On the receipt surface: the value the gates take from the denied class is received as centralized decision authority by enacting_jurisdictions, which is why gain_flow names that seat; physicians collect service fees, not the extracted authority. Fixing is prohibitive for the seat that could fix it: every eligibility widening consumes enormous legislative capital against a small direct constituency while perceived risks are diffuse and vivid — the pattern visible in repeated expansion delays and failed bills.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical statutes. From the eligible patient's seat the arrangement is close to pure coordination — it is the thing that makes the choice real — while from the ineligible patient's seat the same gates are the wall between them and relief, with criminal law holding helpers back. Physicians occupy a dual position: paid, protected gatekeepers with a working exit. Religious institutions experience a displacement they cannot doctrinally accept, and their identity_locked exit keeps their opposition structurally permanent. Enacting jurisdictions hold the authority and the political cost together. The engine derives these divergent classifications from power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for eligible_dying_patients (the framework subsidizes exactly their aim) and for maid_providing_physicians (role, fees, legal protection, with mobility damping further). Victim declarations drive high directionality for ineligible_suffering_patients — nearest the full-target end, since their exit is constrained and the denial is total within their jurisdiction — and for disabled_people_facing_expansion, who bear diffuse pressure costs without seeking the service at all. Religious_institutions derive high directionality from payer status compounded by identity_locked exit: the framework costs them authority they cannot stop contesting. Enacting_jurisdictions sit nearer the middle: they capture discretionary authority and fiscal flexibility (contested in magnitude) while absorbing oversight burden and political cost. Note the asymmetry the structural delta predicts: the framework's suppressive force is aimed outward at paternalistic rivals — courts striking prohibitions, statutes preempting local bans — while its own coercive force lands on the ineligible and the out-of-pathway helper.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unbearable suffering joined to criminalized assistance — remains live wherever the framework operates, so no mandatrophy declaration is warranted and none is authored. The classification guards against two opposite mislabels: reading the arrangement as pure coordination ignores the measurable class paying in suffering at the eligibility line; reading it as pure extraction ignores the documented substitution of supervised deaths for clandestine ones and the relief reported by the served class. Tangled rope holds both. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges — no zombie flag; the arrangement's persistence tracks a problem that still exists. The live risk is forward drift: if expansion continues while the founding class (terminal, competent, requesting) shrinks as a share of users, the arrangement's center of gravity migrates and the extraction question reopens — the expansion-trajectory omega tracks exactly this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the autonomy_reading instantiation of the end_of_life_authority kernel; what structural differences would the sibling readings (sanctity_reading, slippery_slope_mechanism) introduce if authored?',
    'Author the sibling stories separately: sanctity_reading relocates the victim set (intentional life-ending becomes the harm; requesting patients move from the served class to the protected class) and re-references epsilon to the prohibition arrangement; slippery_slope_mechanism treats expansion rate as the load-bearing variable and predicts victim-set growth dominating coordination gains.',
    'The same statutory landscape classifies differently per reading: this reading computes the framework as coordination-serving with bounded extraction, sanctity_reading would compute it as harm-permitting, and the slippery-slope sibling treats the expansion series as the primary signal. Cross-reading comparison is valid only through the network edges, never by averaging epsilon across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this file is one of three readings of the end-of-life authority kernel.').

omega_variable(
    voluntariness_contamination_source,
    'Does the implicit pressure reported by disabled and elderly patients originate in the framework''s operation itself, or in surrounding care scarcity and cost pressure that the framework merely reveals?',
    'Comparative natural experiment across jurisdictions that expanded eligibility while simultaneously guaranteeing palliative and disability-support services versus those that expanded without such guarantees; compare expressed-pressure rates and crisis-service utilization.',
    'If the source is care scarcity, the framework''s measured extraction falls as support investment rises and the fixing-cost picture improves; if the pressure is intrinsic to offering death as a treatment option, each expansion structurally raises extraction and strengthens the slippery-slope sibling''s predictive claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_contamination_source, empirical, 'Whether pressure externalities are framework-intrinsic or environmental.').

omega_variable(
    expansion_logic_internal_vs_ratchet,
    'Does the observed expansion of eligibility criteria follow an internal logic of autonomy-consistency (each extension honoring the same founding principle for newly situated sufferers), or an external ratchet of advocacy momentum, fiscal incentive, and precedent creep?',
    'Longitudinal comparison of expansion debates and votes across jurisdictions: which arguments carry majorities, whether extensions are justified by principle or by accumulated practice, and whether fiscal analyses appear in committee records.',
    'An internal-logic finding supports this reading''s coherence and predicts convergence at principled boundaries; a ratchet finding strengthens the slippery-slope sibling and raises projected extraction growth beyond the authored trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_logic_internal_vs_ratchet, empirical, 'Mechanism driving the empirical eligibility-expansion pattern.').

omega_variable(
    safeguard_functional_share,
    'What share of safeguard activity produces actual protection (detected coercion, corrected capacity errors) versus compliance artifact (duplicated signatures, residency bars, redundant attestations)?',
    'Oversight-report audits computing detection yield per safeguard type against the administrative volume each generates.',
    'A high artifact share would raise the effective burden borne by every seeker without purchasing protection, increasing measured extraction and signaling performative maintenance drifting in at the framework''s margins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safeguard_functional_share, empirical, 'Functional versus theatrical composition of the safeguard apparatus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 1997, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eola_autonomy_tr_t1997, end_of_life_authority__autonomy_reading, theater_ratio, 1997, 0.22).
narrative_ontology:measurement_basis(eola_autonomy_tr_t1997, observed).
narrative_ontology:measurement(eola_autonomy_tr_t2003, end_of_life_authority__autonomy_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement_basis(eola_autonomy_tr_t2003, observed).
narrative_ontology:measurement(eola_autonomy_tr_t2009, end_of_life_authority__autonomy_reading, theater_ratio, 2009, 0.26).
narrative_ontology:measurement_basis(eola_autonomy_tr_t2009, observed).
narrative_ontology:measurement(eola_autonomy_tr_t2015, end_of_life_authority__autonomy_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(eola_autonomy_tr_t2015, observed).
narrative_ontology:measurement(eola_autonomy_tr_t2021, end_of_life_authority__autonomy_reading, theater_ratio, 2021, 0.31).
narrative_ontology:measurement_basis(eola_autonomy_tr_t2021, observed).
narrative_ontology:measurement(eola_autonomy_tr_t2025, end_of_life_authority__autonomy_reading, theater_ratio, 2025, 0.32).
narrative_ontology:measurement_basis(eola_autonomy_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(eola_autonomy_be_t1997, end_of_life_authority__autonomy_reading, base_extractiveness, 1997, 0.34).
narrative_ontology:measurement_basis(eola_autonomy_be_t1997, observed).
narrative_ontology:measurement(eola_autonomy_be_t2003, end_of_life_authority__autonomy_reading, base_extractiveness, 2003, 0.37).
narrative_ontology:measurement_basis(eola_autonomy_be_t2003, observed).
narrative_ontology:measurement(eola_autonomy_be_t2009, end_of_life_authority__autonomy_reading, base_extractiveness, 2009, 0.39).
narrative_ontology:measurement_basis(eola_autonomy_be_t2009, observed).
narrative_ontology:measurement(eola_autonomy_be_t2015, end_of_life_authority__autonomy_reading, base_extractiveness, 2015, 0.41).
narrative_ontology:measurement_basis(eola_autonomy_be_t2015, observed).
narrative_ontology:measurement(eola_autonomy_be_t2021, end_of_life_authority__autonomy_reading, base_extractiveness, 2021, 0.44).
narrative_ontology:measurement_basis(eola_autonomy_be_t2021, observed).
narrative_ontology:measurement(eola_autonomy_be_t2025, end_of_life_authority__autonomy_reading, base_extractiveness, 2025, 0.46).
narrative_ontology:measurement_basis(eola_autonomy_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(eola_autonomy_su_t1997, end_of_life_authority__autonomy_reading, suppression_requirement, 1997, 0.4).
narrative_ontology:measurement_basis(eola_autonomy_su_t1997, observed).
narrative_ontology:measurement(eola_autonomy_su_t2003, end_of_life_authority__autonomy_reading, suppression_requirement, 2003, 0.45).
narrative_ontology:measurement_basis(eola_autonomy_su_t2003, observed).
narrative_ontology:measurement(eola_autonomy_su_t2009, end_of_life_authority__autonomy_reading, suppression_requirement, 2009, 0.49).
narrative_ontology:measurement_basis(eola_autonomy_su_t2009, observed).
narrative_ontology:measurement(eola_autonomy_su_t2015, end_of_life_authority__autonomy_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement_basis(eola_autonomy_su_t2015, observed).
narrative_ontology:measurement(eola_autonomy_su_t2021, end_of_life_authority__autonomy_reading, suppression_requirement, 2021, 0.56).
narrative_ontology:measurement_basis(eola_autonomy_su_t2021, observed).
narrative_ontology:measurement(eola_autonomy_su_t2025, end_of_life_authority__autonomy_reading, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(eola_autonomy_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial 'euthanasia debate' decomposes into three structurally distinct claims per the epsilon-invariance principle. This story (autonomy_reading) authors the entitlement-shaped constraint: a gated rights framework with genuine coordination function and asymmetric extraction at the eligibility line. sanctity_reading authors the prohibition-shaped constraint (different victim set: intentional life-ending as the harm). slippery_slope_mechanism authors the mechanism-shaped constraint (expansion dynamics as the load-bearing fact). This upstream story influences the slippery-slope sibling because its actual expansion behavior supplies that reading's empirical substrate; it coexists with the sanctity sibling across opposing factions without either eliminating the other as a live position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
