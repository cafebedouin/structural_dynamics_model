% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__institutional_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__institutional_displacement_reading, []).

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
 *   constraint_id: dueling_disappearance_mechanism__institutional_displacement_reading
 *   human_readable: Dueling Protocol as Outcompeted Elite Dispute-Resolution Coordination
 *   domain: historical sociology/legal history/cultural anthropology
 *
 * SUMMARY:
 *   For over a century the code duello operated as the standing
 *   dispute-resolution protocol of the European gentleman class: a mutually
 *   recognized procedure — challenge, seconding, codified rules, satisfaction
 *   terms — that converted insults, quarrels, and debts of honor into
 *   bounded, consensual single combat where formal law could not or would not
 *   go. This story instantiates the institutional_displacement_reading of the
 *   dueling_disappearance_mechanism kernel: the protocol declined because
 *   courts, banking and credit instruments, and libel law outcompeted it as
 *   dispute-resolution infrastructure, not primarily because states banned it
 *   or because honor axioms dissolved. On this reading the protocol was a
 *   genuine coordination mechanism — participants were net beneficiaries
 *   relative to unregulated vendetta — and its decline was voluntary
 *   migration to superior substitutes, leaving dueling as an
 *   available-but-disfavored option in institutional gaps, most durably the
 *   officer corps. The ε referent is the standing arrangement itself: the
 *   dueling protocol as an operating dispute-resolution institution, assessed
 *   by this reading's own lights. Sibling readings (contraction_reading,
 *   overdetermined_composite_reading) are separate constraints linked via
 *   network.affects_constraints; their ε values are authored in their own
 *   files. KEY AGENTS (by structural relationship): - european_gentry:
 *   Primary beneficiary (powerful/constrained) — the class the protocol
 *   coordinated; migrates to substitutes as they mature - army_officer_corps:
 *   Enclave beneficiary (organized/identity_locked) — holds the protocol
 *   longest where professional honor fuses with commission-holding -
 *   elected_politicians: Secondary beneficiary (powerful/constrained) —
 *   earliest switchers to press, libel action, and court remedy -
 *   state_judiciary: Excluded substitute provider (institutional/constrained)
 *   — formally available, culturally barred from the honor conversation; its
 *   maturation is the displacement mechanism - clergy_and_moral_reformers:
 *   Excluded resisters (organized/constrained) — objected throughout; never
 *   admitted to the protocol's deliberations - historical_sociologists:
 *   Analytical observer (analytical/analytical) — sees the full substitution
 *   structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__institutional_displacement_reading, 0.38).
domain_priors:suppression_score(dueling_disappearance_mechanism__institutional_displacement_reading, 0.16).
domain_priors:theater_ratio(dueling_disappearance_mechanism__institutional_displacement_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 0.16).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__institutional_displacement_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__institutional_displacement_reading, rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__institutional_displacement_reading, "Dueling Protocol as Outcompeted Elite Dispute-Resolution Coordination").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__institutional_displacement_reading, "historical sociology/legal history/cultural anthropology").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__institutional_displacement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__institutional_displacement_reading, 'ef2d809f-257a-4810-9908-6c395197c3df').
narrative_ontology:cs_kernel_codification('ef2d809f-257a-4810-9908-6c395197c3df', distributed).
narrative_ontology:cs_authority_grounding('ef2d809f-257a-4810-9908-6c395197c3df', expertise).
narrative_ontology:cs_interpretation_layer_present('ef2d809f-257a-4810-9908-6c395197c3df').
narrative_ontology:cs_reading_relation('ef2d809f-257a-4810-9908-6c395197c3df', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef2d809f-257a-4810-9908-6c395197c3df', dueling_disappearance_mechanism__overdetermined_composite_reading, influences).
narrative_ontology:cs_axiom('ef2d809f-257a-4810-9908-6c395197c3df', foundational, institutional_substitution_sufficient_for_decline).
narrative_ontology:cs_axiom_status(institutional_substitution_sufficient_for_decline, holdable).
narrative_ontology:cs_axiom_grounding('ef2d809f-257a-4810-9908-6c395197c3df', institutional_substitution_sufficient_for_decline, empirically_contingent).
narrative_ontology:cs_axiom('ef2d809f-257a-4810-9908-6c395197c3df', secondary, dispute_mechanism_selection_is_voluntary).
narrative_ontology:cs_axiom_status(dispute_mechanism_selection_is_voluntary, holdable).
narrative_ontology:cs_axiom_grounding('ef2d809f-257a-4810-9908-6c395197c3df', dispute_mechanism_selection_is_voluntary, empirically_contingent).
narrative_ontology:cs_reference_frame('ef2d809f-257a-4810-9908-6c395197c3df', dueling_dominant_dispute_protocol).
narrative_ontology:cs_drift_state('ef2d809f-257a-4810-9908-6c395197c3df', post_substitution_consolidation, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('ef2d809f-257a-4810-9908-6c395197c3df', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__institutional_displacement_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, european_gentry).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, army_officer_corps).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__institutional_displacement_reading, elected_politicians).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, honor_as_private_jurisdiction).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__institutional_displacement_reading, personal_courage_as_credibility_collateral).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary landholding and office-holding families across Europe for whom the code duello was the standing procedure for settling slights, debts of honor, and political quarrels. Participation purchased credibility within the class; refusal invited ostracism. As courts, credit instruments, and libel action matured, the same families migrated their quarrels to those channels, keeping dueling as a fallback for grievances the newer institutions could not reach.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, european_gentry, beneficiary,
    powerful, generational, constrained, continental).

% Commissioned officers in European armies, where the honor code fused with commission-holding itself: regimental tribunals, promotion boards, and mess culture treated challenge-and-satisfaction as professional duty. Dueling persisted here decades after civilian abandonment — including in states with modern courts and banking — because stepping outside the honor frame meant stepping outside the career. Exit from the protocol and exit from the profession were the same door.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, army_officer_corps, beneficiary,
    organized, biographical, identity_locked, continental).

% Parliamentarians, ministers, and party leaders who used the protocol to answer press attacks and floor insults when litigation seemed slow or dishonorable. Newspaper libel actions and expanding court capacity gave them a cheaper vindication path as the interval progressed; political dueling survived longest where press freedom outran legal remedy.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, elected_politicians, beneficiary,
    powerful, biographical, constrained, national).

% Courts and the legal professions, formally open to any plaintiff but culturally barred from the honor conversation: a gentleman who filed suit over an insult marked himself as afraid. For most of the interval the judiciary sat outside elite dispute resolution, and that exclusion was the space the protocol occupied; as procedural dignity and reliable adjudication spread, the bar lowered and grievances migrated in.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, state_judiciary, excluded,
    institutional, generational, constrained, national).

% Church authorities and anti-dueling societies that condemned the practice throughout and were dismissed by the honor class as cowardice or sectarian interference. They mounted sermons, pamphlet campaigns, and legislative lobbying; their objections never entered the protocol's own deliberations, which recognized only martial and gentlemanly authority.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, clergy_and_moral_reformers, excluded,
    organized, generational, constrained, national).

% Later scholars reconstructing the decline from archival incident records, regimental orders, and legal statistics; they weigh institutional-substitution evidence against cultural-change and prohibition accounts and maintain the comparative datasets this reading rests on.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__institutional_displacement_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__institutional_displacement_reading, diffuse).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__institutional_displacement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplied a mutually recognized, bounded procedure for resolving disputes of honor among elites who could not or would not carry such disputes to formal law: challenge protocols, seconds, codified rules, and satisfaction terms converted private vendetta and indefinite feud into single, consensual, rule-governed combat with a legible outcome.
% TRANSFER_FUNCTION: Moved honor-standing between disputants (victory standing, cowardice penalty), placed the risk of death or injury on both parties' mutual consent, and consumed the time, attendance, and arms of principals and seconds; no money or goods changed hands by design — the protocol deliberately kept settlement outside both the market and the state.
% ABSENT_VOICES: Clergy and moral reformers objected continuously and stood outside the honor conversation — their objections were dismissed as cowardice or sectarian meddling and never entered the protocol's deliberations. The judiciary was formally open but culturally barred: filing suit over an insult marked a gentleman as afraid, so the courts' voice was absent from elite dispute resolution for most of the interval; its eventual admission is the displacement mechanism this reading documents.
% DISAPPEARANCE_RATIONALE: At the interval's end the protocol is vestigial: its dispute-resolution, credit-signaling, and insult-response functions had already been absorbed by courts, bankers' references, and press litigation, so an overnight disappearance would remove ritual residue, not load-bearing structure. The asymmetry is the point of this reading — the world rearranged before and because the protocol disappeared, gradually, as substitutes matured; by 1900 the rearrangement was complete and nothing depended on the protocol remaining.
% FOUNDING_PROBLEM: In weak-state environments where formal law lacked the reach or the legitimacy to adjudicate elite disputes — and where submitting a grievance to lawyers meant confessing fear — the gentleman class needed a dispute-resolution mechanism that was binding, reputationally legible, and independent of state override. Unregulated vendetta was the default alternative; the code duello was built to bound and civilize it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: legal-historical scholarship on early modern state formation documents that courts lacked reach and legitimacy over elite disputes in the protocol's founding era; anthropological work on honor societies (Corsican vendetta, Albanian kanun, frontier communities) independently attests the same founding condition arising wherever state adjudication is absent. Contemporary records — regimental orders, diplomatic correspondence, court registers declining honor cases — attest the gap the protocol filled. The gentry itself never issued a formal attestation; it practiced around the problem, which is itself consistent with the founding condition being real rather than asserted.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__institutional_displacement_reading, world_unchanged).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__institutional_displacement_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__institutional_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__institutional_displacement_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).
:- end_tests(dueling_disappearance_mechanism__institutional_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored modest (0.38 at interval end) and rising gently: the protocol's absolute costs — participant risk, deaths, seconds' labor — were roughly stable, but its relative burden grew as courts and credit instruments improved the outside option, so a gentleman choosing dueling in 1890 bore a higher opportunity cost than his 1770 counterpart. Suppression is low and falling (0.52 to 0.16): the protocol ran on social enforcement (challenge obligation, ostracism of refusers), and that machinery attrited as substitutes absorbed the function — this is enforcement decay, the dynamic the suppression_requirement series exists to trace. Theater rises from 0.18 to 0.48: ritual increasingly outlived function in the tail (ceremonial challenges, air-fired pistols, pre-arranged reconciliations), approaching but not crossing the proxy-goal threshold — consistent with a coordination mechanism being absorbed rather than one maintained by performance alone. Accessibility_collapse is low (0.22): the defining fact of this reading is that alternatives flourished — the opposite of closed exits. Resistance is moderate-high (0.55): clerical campaigns, anti-dueling societies, and legislative bans were real and continuous, even though this reading attributes the decline to competition rather than coercion. Claim and metrics are authored independently: claimed_type=rope follows from the reading's structure (voluntary coordination, no victim set, minimal coercive overhead relative to vendetta), while the metric values describe observed operation. All three tracked series share one six-point grid (1770, 1815, 1840, 1865, 1890, 1900); no cyclical dynamics are claimed — the trajectories are monotonic, driven by substitute maturation rather than oscillating enforcement. Suppression is authored as a raw structural property; only extractiveness is scaled downstream by directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   There are no payer seats, so the classical payer-versus-beneficiary divergence does not arise; the operative divergence runs between beneficiary seats. From the gentry seat the protocol reads as a service that was fairly priced and peacefully outcompeted — exit was available and taken. From the officer-corps seat the same protocol reads as career obligation: identity fusion with commission-holding made refusal professionally ruinous long after civilian alternatives matured, so the officer's experienced position sits far nearer the target end than the gentry's, despite the identical nominal role. The excluded seats diverge hardest: the judiciary experienced the protocol as a jurisdictional obstacle (its exclusion was the space the protocol occupied), and the clergy as a moral offense their authority could not touch. The engine computes these per-seat classifications from the power, exit, and role data; this prose only locates them.
 *
 * DIRECTIONALITY LOGIC:
 *   All three declared beneficiary groups derive low directionality (subsidy-side): the protocol delivered credible dispute resolution and honor-standing to them at the price of participation risk. The derivation differentiates within the beneficiary set through exit modulation: european_gentry and elected_politicians carry constrained exit (costly but real, improving across the interval), while army_officer_corps carries identity_locked exit, which pulls its effective extraction upward relative to the other beneficiaries — identity-locked agents sit nearer the target end than mobile ones even when their declared role is beneficiary. No victims are declared: under this reading the protocol's costs were borne by consenting participants purchasing a service, and the losses that did occur (risk, death, effort) were consumed within the practice rather than transferred to any seat — hence gain_flow='diffuse', an affirmative claim that no named seat collects the extraction. No directionality overrides are used: the beneficiary declarations plus the exit atoms produce the intended differentiation without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim guards against the snare misreading: because dueling's costs fell on consenting participants buying real coordination, treating it as pure extraction with identifiable victims would fabricate a victim set the historical record does not support. Equally, the temporal data guard against rope complacency: theater_ratio climbing toward 0.5 and a dead founding problem show the mandate being retired by absorption rather than renewed. The mandatrophy question resolves as completion, not capture: the founding problem — credible dispute resolution where formal law could not reach — was solved by the substitutes, the protocol's enforcement machinery decayed rather than hardened, and by interval end nothing load-bearing depends on it; hence founding_problem_status='dead' paired with disappearance_verdict='world_unchanged', a combination the mismatch consumer reads as completed retirement rather than the dead-plus-rearranges zombie signature. The residual theatrical tail in the officer corps is documented in the theater series and the officer_corps_anomaly omega rather than forced into a piton reclassification the reading's structure does not warrant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is dueling''s decline best explained by institutional substitution alone (this reading), by dignity-culture displacement of honor axioms (contraction_reading), or by an overdetermined conjunction of independent sufficient causes (overdetermined_composite_reading)?',
    'Comparative-historical analysis across regions matched on institutional capacity but differing in cultural exposure, and vice versa: if dueling persisted wherever institutions lagged regardless of cultural contact, substitution dominates; if it collapsed uniformly ahead of institutional maturation, the cultural reading dominates; if neither alone predicts timing, the composite wins.',
    'This file''s rope classification and no-victim structure hold only under substitution dominance; a cultural-dominance result relocates the constraint to the axiom structure (contraction_reading''s referent), and an overdetermination result dissolves this reading into the composite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which sibling mechanism carries the causal weight for dueling''s decline.').

omega_variable(
    prohibition_confound,
    'How much of dueling''s decline is attributable to competitive substitution versus contemporaneous legal prohibition, given that bans and institutional maturation advanced together across the interval?',
    'Within-jurisdiction comparison of enforcement intensity: jurisdictions with enacted bans but lax state enforcement versus periods before and after ban enactment holding court quality constant; survival analysis of duel incidence against ban dates.',
    'If prohibition carries the decline, this reading''s voluntary-substitution structure and no-victim delta are wrong — the protocol''s tail was suppressed practice rather than disfavored option, pushing the computed classification toward suppressed-practice dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibition_confound, empirical, 'Separating competitive displacement from legal prohibition effects.').

omega_variable(
    officer_corps_anomaly,
    'Why did dueling persist deepest precisely where institutional substitution was most complete — modern courts, banking, and libel law all present in the German, Austrian, and French officer corps?',
    'Compare civilian and military uptake curves of substitute mechanisms within the same jurisdictions and period; test whether regimental governance (internal honor tribunals, promotion dependency, mess culture) insulated officers from the substitute channels civilians used.',
    'If corporate identity-lock rather than institutional absence explains the tail, this reading''s persistence-in-institutional-gaps claim fails for corporate bodies, and the officer seat''s effective burden is understated by a beneficiary-side derivation that misses the identity lock.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(officer_corps_anomaly, empirical, 'The strongest anomaly against pure institutional substitution.').

omega_variable(
    voluntariness_of_participation,
    'Was participation voluntary enough to sustain the no-victim declaration, given that refusal carried social-death penalties for much of the interval?',
    'Refusal-rate trajectories against court-access expansion: if refusal rates rose smoothly and penalties for refusal attenuated as substitutes matured, consent was price-elastic; if refusal continued to be punished long after courts were available, structural coercion persisted beneath the voluntary surface.',
    'Sustained punishment of refusers would constitute an implicit victim set of compelled participants, raising effective extraction and undermining the voluntary-coordination character of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_of_participation, empirical, 'Whether the no-victim structure survives the refusal-penalty record.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__institutional_displacement_reading, 1770, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_disp_dueling_tr_t1770, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1770, 0.18).
narrative_ontology:measurement_basis(inst_disp_dueling_tr_t1770, observed).
narrative_ontology:measurement(inst_disp_dueling_tr_t1815, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1815, 0.23).
narrative_ontology:measurement_basis(inst_disp_dueling_tr_t1815, observed).
narrative_ontology:measurement(inst_disp_dueling_tr_t1840, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1840, 0.29).
narrative_ontology:measurement_basis(inst_disp_dueling_tr_t1840, observed).
narrative_ontology:measurement(inst_disp_dueling_tr_t1865, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1865, 0.35).
narrative_ontology:measurement_basis(inst_disp_dueling_tr_t1865, observed).
narrative_ontology:measurement(inst_disp_dueling_tr_t1890, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1890, 0.43).
narrative_ontology:measurement_basis(inst_disp_dueling_tr_t1890, observed).
narrative_ontology:measurement(inst_disp_dueling_tr_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, theater_ratio, 1900, 0.48).
narrative_ontology:measurement_basis(inst_disp_dueling_tr_t1900, observed).

% Extraction over time
narrative_ontology:measurement(inst_disp_dueling_be_t1770, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1770, 0.26).
narrative_ontology:measurement_basis(inst_disp_dueling_be_t1770, observed).
narrative_ontology:measurement(inst_disp_dueling_be_t1815, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1815, 0.29).
narrative_ontology:measurement_basis(inst_disp_dueling_be_t1815, observed).
narrative_ontology:measurement(inst_disp_dueling_be_t1840, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1840, 0.32).
narrative_ontology:measurement_basis(inst_disp_dueling_be_t1840, observed).
narrative_ontology:measurement(inst_disp_dueling_be_t1865, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1865, 0.34).
narrative_ontology:measurement_basis(inst_disp_dueling_be_t1865, observed).
narrative_ontology:measurement(inst_disp_dueling_be_t1890, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1890, 0.37).
narrative_ontology:measurement_basis(inst_disp_dueling_be_t1890, observed).
narrative_ontology:measurement(inst_disp_dueling_be_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement_basis(inst_disp_dueling_be_t1900, observed).

% Suppression requirement over time
narrative_ontology:measurement(inst_disp_dueling_su_t1770, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1770, 0.52).
narrative_ontology:measurement_basis(inst_disp_dueling_su_t1770, observed).
narrative_ontology:measurement(inst_disp_dueling_su_t1815, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1815, 0.45).
narrative_ontology:measurement_basis(inst_disp_dueling_su_t1815, observed).
narrative_ontology:measurement(inst_disp_dueling_su_t1840, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1840, 0.37).
narrative_ontology:measurement_basis(inst_disp_dueling_su_t1840, observed).
narrative_ontology:measurement(inst_disp_dueling_su_t1865, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1865, 0.28).
narrative_ontology:measurement_basis(inst_disp_dueling_su_t1865, observed).
narrative_ontology:measurement(inst_disp_dueling_su_t1890, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1890, 0.2).
narrative_ontology:measurement_basis(inst_disp_dueling_su_t1890, observed).
narrative_ontology:measurement(inst_disp_dueling_su_t1900, dueling_disappearance_mechanism__institutional_displacement_reading, suppression_requirement, 1900, 0.16).
narrative_ontology:measurement_basis(inst_disp_dueling_su_t1900, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__institutional_displacement_reading, resource_allocation).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__institutional_displacement_reading, overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'why dueling disappeared' decomposes into three structurally distinct constraints sharing one kernel. This story (institutional_displacement_reading) authors epsilon for the dueling protocol as a functioning coordination mechanism losing a competitive race against courts, credit instruments, and libel law — modest epsilon, no victim set, voluntary migration. contraction_reading authors epsilon for the honor-axiom structure displaced by dignity-culture norms — a cultural constraint with no competitive dynamics. overdetermined_composite_reading authors epsilon over the conjunctural process in which prohibition, institutional modernization, cultural shift, and war trauma act simultaneously. The upstream/downstream gradient runs from this reading (cleanest identification, highest empirical confidence) into the composite, which subsumes its evidence; the contraction reading stands apart on the cultural axis. All three are linked via affects_constraints; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
