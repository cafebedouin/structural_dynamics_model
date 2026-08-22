% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__national_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__national_primacy_reading, []).

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
 *   constraint_id: article_17_complementarity__national_primacy_reading
 *   human_readable: Article 17 Complementarity — National Primacy Reading (Presumption of Domestic Adequacy)
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute makes the International Criminal Court a
 *   court of last resort: it may act only where national courts are
 *   'unwilling or unable' genuinely to carry out proceedings. This story
 *   instantiates the national-primacy reading of that clause — the reading
 *   favored by sovereignty-maximizing states and embedded in much of the
 *   Court's own admissibility jurisprudence — under which national courts are
 *   presumptively adequate unless proven sham, the burden of demonstrating
 *   inadmissibility falls on the side seeking international action, and the
 *   practical victim set shrinks to cases of complete judicial collapse. It
 *   is one of two readings of the article_17_complementarity kernel; the
 *   sibling (international_oversight_reading) allocates the burden the other
 *   way and is authored as a separate constraint with its own epsilon. Claim
 *   and metrics are authored independently: the reading is claimed as
 *   tangled_rope — a genuine jurisdiction-allocation function carrying
 *   asymmetric costs — while the metrics describe an arrangement whose
 *   deference has hardened and whose performative share has grown over the
 *   interval. The epsilon referent is the standing admissibility arrangement
 *   as this reading maintains it, assessed by the reading's own lights.
 *
 * KEY AGENTS:
 *   - national_judiciaries: primary beneficiary (institutional / identity_locked) — retain first jurisdiction over atrocity cases and certify their own sufficiency
 *   - sovereignty_maximizing_states: primary beneficiary (institutional / constrained) — convert domestic control of criminal process into immunity from supranational process
 *   - protected_sitting_officials: concentrated beneficiary (powerful / mobile) — receive the operative good, non-prosecution abroad, behind minimally adequate domestic files
 *   - great_power_nonparty_states: beneficiary outside the obligation web (powerful / arbitrage) — champion strong complementarity while bearing no statute obligations
 *   - victims_of_partial_domestic_accountability: primary target (powerless / trapped) — remedy foreclosed by the mere existence of inadequate proceedings
 *   - victims_of_witness_tampered_proceedings: primary target (powerless / trapped) — their collapsed cases become the certificate barring any other forum
 *   - icc_office_of_the_prosecutor: burden-bearing enforcer seat (institutional / constrained) — must prove the negative against a presumption, with evidence the state controls
 *   - icc_appeals_chamber: agenda setter (institutional / constrained) — calibrates the gate's practical height through admissibility rulings
 *   - civil_society_accountability_organizations: excluded voice (organized / constrained) — document sham proceedings without standing to trigger review
 *   - un_security_council: episodic observer (institutional / analytical) — holds referral and deferral powers it exercises politically, not remedially
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, 0.65).
domain_priors:suppression_score(article_17_complementarity__national_primacy_reading, 0.62).
domain_priors:theater_ratio(article_17_complementarity__national_primacy_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article_17_complementarity__national_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__national_primacy_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__national_primacy_reading, "Article 17 Complementarity — National Primacy Reading (Presumption of Domestic Adequacy)").
narrative_ontology:topic_domain(article_17_complementarity__national_primacy_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__national_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__national_primacy_reading, 'ef7e85e2-a436-421c-bac9-5113febda62f').
narrative_ontology:cs_kernel_codification('ef7e85e2-a436-421c-bac9-5113febda62f', fixed_text).
narrative_ontology:cs_authority_grounding('ef7e85e2-a436-421c-bac9-5113febda62f', lineage).
narrative_ontology:cs_interpretation_layer_present('ef7e85e2-a436-421c-bac9-5113febda62f').
narrative_ontology:cs_reading_relation('ef7e85e2-a436-421c-bac9-5113febda62f', article_17_complementarity__international_oversight_reading, forecloses).
narrative_ontology:cs_axiom('ef7e85e2-a436-421c-bac9-5113febda62f', foundational, national_courts_presumptively_genuine).
narrative_ontology:cs_axiom_status(national_courts_presumptively_genuine, holdable).
narrative_ontology:cs_axiom_grounding('ef7e85e2-a436-421c-bac9-5113febda62f', national_courts_presumptively_genuine, conventional).
narrative_ontology:cs_axiom('ef7e85e2-a436-421c-bac9-5113febda62f', foundational, inadmissibility_burden_on_the_seeking_party).
narrative_ontology:cs_axiom_status(inadmissibility_burden_on_the_seeking_party, holdable).
narrative_ontology:cs_axiom_grounding('ef7e85e2-a436-421c-bac9-5113febda62f', inadmissibility_burden_on_the_seeking_party, conventional).
narrative_ontology:cs_reference_frame('ef7e85e2-a436-421c-bac9-5113febda62f', sovereign_judicial_primacy_order).
narrative_ontology:cs_drift_state('ef7e85e2-a436-421c-bac9-5113febda62f', contemporary_admissibility_jurisprudence, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('ef7e85e2-a436-421c-bac9-5113febda62f', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__national_primacy_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, national_judiciaries).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, protected_sitting_officials).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_of_partial_domestic_accountability).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, victims_of_witness_tampered_proceedings).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_17_complementarity__national_primacy_reading, great_power_nonparty_states).
narrative_ontology:constraint_victim(article_17_complementarity__national_primacy_reading, icc_office_of_the_prosecutor).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, sovereign_equality_doctrine).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, subsidiarity_principle).
narrative_ontology:constraint_vindicates(article_17_complementarity__national_primacy_reading, positive_complementarity_capacity_building).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judiciaries of states facing atrocity allegations. They retain first claim on every case arising on their territory: under this arrangement their proceedings count as adequate unless someone proves otherwise, and they decide what to charge, whom to arrest, and when to stop. Running the domestic forum is also how they exercise the very prerogative they defend — a court that certifies its own sufficiency rarely invites external audit. Renouncing the arrangement would mean conceding that their justice cannot be trusted, which no national court can do without repudiating its constitutional role.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, national_judiciaries, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(article_17_complementarity__national_primacy_reading, national_judiciaries, agenda_setter).

% State parties that treat domestic control of criminal process as a core attribute of statehood. They gain a durable shield: as long as some proceeding exists at home, the international court stays out, and they press this reading in assembly sessions, funding negotiations, and admissibility litigation. Withdrawal from the treaty is available but costly — several states have tried it and watched cooperation networks, aid relationships, and diplomatic standing degrade — so they work the system from inside.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, sovereignty_maximizing_states, beneficiary,
    institutional, generational, constrained, global).

% Sitting presidents, ministers, senior commanders, and security chiefs implicated in atrocity allegations. The operative good they receive is non-prosecution abroad: a domestic file opened against a mid-level subordinate, a narrowly framed trial, or a truth-commission report is enough to keep the international court away from them personally. They can shape the domestic record through appointments, budgets, and prosecutorial discretion, and can wait out limitation periods — a freedom of movement inside the arrangement that victims cannot match.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, protected_sitting_officials, beneficiary,
    powerful, biographical, mobile, national).

% Major military powers outside the treaty, and their close allies, that champion strong complementarity precisely because it keeps the court away from their nationals and operations. They bear no obligations under the statute yet benefit from the norm it encodes, and they can fund, withhold, or pressure cooperation case by case — the widest freedom of any seat in the arrangement.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, great_power_nonparty_states, beneficiary,
    powerful, generational, arbitrage, global).

% Survivors and families in states that prosecute some perpetrators — typically foot soldiers or opposition figures — while the architects of the campaign remain in office or in politics. From where they stand, the existence of any domestic proceeding closes the international door: they cannot reopen the case abroad, and the domestic process they are pointed back to is the one that already declined to reach the top. Their remedy is exhausted by a proceeding they did not choose and could not shape.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_of_partial_domestic_accountability, payer,
    powerless, biographical, trapped, national).

% Survivors whose cases turned on testimony that was subsequently bought, intimidated, or made to disappear — witnesses recant, flee, or die before trial, and the resulting acquittal or collapse counts as a genuine outcome of a genuine process. They watch the record of their own case become the certificate that bars anyone else from hearing it.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, victims_of_witness_tampered_proceedings, payer,
    powerless, biographical, trapped, national).

% The organ that must investigate and, increasingly, prove the negative: that a state's proceedings are not merely imperfect but sham. Every admissibility fight consumes investigative resources, depends on evidence the state controls, and casts the prosecutor as the party arguing against a presumption. A loss narrows the office's reachable docket; even a win tends to yield a case the state will not cooperate with.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_office_of_the_prosecutor, payer,
    institutional, generational, constrained, global).

% The bench that writes the operative threshold case by case: what counts as the same conduct, how much unfairness a proceeding may contain before it stops being genuine, whether the prospect of state cooperation matters to the analysis. Each ruling binds later panels and shifts the practical height of the gate; the bench cannot amend the treaty text it applies, only calibrate the reading of it.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, icc_appeals_chamber, agenda_setter,
    institutional, generational, constrained, global).

% Human rights organizations, victim-representation bodies, and documentation projects that compile the evidence of sham proceedings. They have no standing to initiate or intervene in most admissibility determinations; their filings enter as amicus material at a chamber's discretion, and their findings are routinely answered with the observation that the question belongs to states and the court, not to advocates.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, civil_society_accountability_organizations, excluded,
    organized, generational, constrained, global).

% Holds referral and deferral powers over situations and can reshape the court's reach by resolution, but engages episodically and along great-power lines. It watches the admissibility regime operate and intervenes when permanent-member interests align, not when victims' access to a forum turns on the outcome.
narrative_ontology:constraint_stakeholder(article_17_complementarity__national_primacy_reading, un_security_council, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__national_primacy_reading, protected_sitting_officials).
narrative_ontology:fixing_cost_class(article_17_complementarity__national_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates prosecutorial jurisdiction over genocide, crimes against humanity, and war crimes between national courts and the international court: prevents duplicative and conflicting proceedings, preserves state ownership of justice for the overwhelming majority of cases, creates an incentive for states to build domestic capacity, and conserves the international court's scarce resources for genuine gaps.
% TRANSFER_FUNCTION: Moves adjudicative authority — and the immunity protection bundled with it — from victims and the international forum toward national institutions and the officials those institutions answer to; moves the burden of proof onto whoever seeks international intervention; and moves the practical cost of impunity gaps onto survivors in states whose proceedings are weak but formally genuine.
% ABSENT_VOICES: Victims in weak-but-genuine-proceeding states have no seat in admissibility proceedings — victim participation attaches to cases, and evaporates once a case is declared inadmissible. Civil society organizations that document sham proceedings lack standing to trigger review. Future victims of atrocities deterred-or-not by the narrowed docket are unrepresented by anyone. They are in affected communities and NGO case files, outside the courtroom where the gate's height is set.
% DISAPPEARANCE_RATIONALE: If the complementarity gate vanished overnight, the international court would face concurrent jurisdiction over every situation within its remit, national proceedings would duplicate and collide with international ones, states would escalate non-cooperation, and the entire division of labor the Rome system rests on would have to be renegotiated — dozens of national justice strategies, cooperation agreements, and situation-country relationships would rearrange around the new allocation.
% FOUNDING_PROBLEM: How to create a permanent international criminal court acceptable to sovereign states that jealously guarded their monopoly on criminal jurisdiction: the Rome Conference could only deliver a court by conceding that it would act only where national systems failed, making complementarity the price of ratification itself.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Rome Conference negotiating record shows delegations across every bloc conditioning acceptance on a complementarity threshold; the preceding ad hoc tribunals' primacy model had been rejected by states precisely because it displaced domestic courts; and international-law scholarship independent of both courts and foreign ministries documents the sovereignty-accountability bargain as the constitutive compromise of the Rome system. No beneficiary-set attestation is required to establish it.
narrative_ontology:disappearance_verdict(article_17_complementarity__national_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__national_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__national_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_17_complementarity__national_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__national_primacy_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__national_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__national_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__national_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.65 at interval end: the arrangement performs a real filtering function, but the good it allocates (access to an impartial forum) is decoupled from the people who paid for the underlying crimes, and the 2013-2021 jurisprudence (same-conduct test, Al-Senussi deference, cooperation-futility reasoning) raised the gate's practical height monotonically after the 2011 low. Suppression is 0.62 as a raw structural property, unscaled by power or scope: the gate does not coerce persons directly, it forecloses forums — once inadmissibility is confirmed, no alternative international route remains for those victims, though universal-jurisdiction statutes and hybrid tribunals keep partial alternatives alive (hence accessibility_collapse at 0.5, not higher). Theater_ratio of 0.45 reflects genuineness review drifting toward formulaic certification: the inquiry is real adjudication, but a growing share of it consists of accepting state assurances at face value. Resistance of 0.58 captures sustained scholarly and advocacy contestation and occasional assertive rulings. Seat divergence is structural, not rhetorical: the judiciary and state seats experience comity and respect for capacity; the victim seats experience foreclosure; the prosecutor seat experiences an evidentially loaded burden. The temporal series shows one full oscillation — the 2010-2011 Kenya authorization briefly lowered measured extraction, and the subsequent witness-tampering collapses (2013-2016) became the strongest argument FOR the primacy frame. That intermittence is itself functional: periodic assertiveness acts as a legitimacy release valve that lets the deference baseline ratchet upward afterward. Victim coalition potential is real but procedurally thin: collective victim participation exists inside ongoing cases and largely evaporates once a case is declared inadmissible, which is precisely where these victims sit.
 *
 * PERSPECTIVAL GAP:
 *   From the national judiciary seat, the arrangement is respect: recognition that justice done at home is more legitimate than justice imposed abroad, and a shield against neocolonial adjudication. From the sovereignty-maximizing state seat, it is constitutional architecture: criminal jurisdiction as a core attribute of statehood that no treaty should dilute. From the victim seats, the same structure is a locked door: a proceeding they did not choose, could not shape, and cannot escape is treated as sufficient to extinguish their claim anywhere else. From the prosecutor's seat, it is an unwinnable assignment — proving unwillingness with evidence the allegedly unwilling state controls. None of these perceptions is wrong; they are computed from different positions in the same structure, and the engine should return different types per seat from the structural data alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place national_judiciaries, sovereignty_maximizing_states, and protected_sitting_officials at the low-directionality end; victim declarations place both victim groups at the high end, and their trapped exit pushes them toward the full-target pole — they cannot arbitrage, relocate their claim, or wait out the arrangement. Exit options differentiate same-power seats: great_power_nonparty_states sit at the extreme beneficiary end because they enjoy the norm's protection with zero obligation exposure (arbitrage), while treaty-bound sovereignty_maximizing_states accept reciprocal exposure (constrained). The prosecutor seat derives a mid-to-high directionality as a payer that administers nothing it controls. No directionality_overrides are authored: the institutional power atom spans seats with genuinely opposed relationships (judiciary-beneficiary, prosecutor-payer, chamber-agenda-setter), so an atom-level override would misfire across them; the role-plus-exit declarations carry the differentiation instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling sovereign judicial monopoly with the political necessity of a permanent court — remains live, and the arrangement still performs its coordinating work, so this is not a resolved-mandatrophy case and the founding_problem_status x disappearance_verdict pair (live x world_rearranges) raises no zombie flag. The drift risk is forward-looking: theater_ratio climbing from 0.15 to 0.45 over the interval tracks genuineness review becoming a certification ritual. If positive complementarity's capacity-building promise continues to fail while the review grows more formulaic, the coordination half atrophies and the arrangement trends toward inertial maintenance administered by a bench that could recalibrate it but bears little of the cost of not doing so — the classic cost-asymmetry signature. Watching that trajectory is the point of the temporal series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_article17,
    'This constraint instantiates the national_primacy_reading of the article_17_complementarity kernel; how would the sibling reading (international_oversight_reading) change the structural classification?',
    'Author the sibling story with its own epsilon, beneficiary/victim sets, and metrics; compare computed per-seat classifications across the pair.',
    'Under the oversight reading the burden shifts to states to demonstrate willingness and ability, the victim set expands from complete-judicial-collapse cases to all impunity gaps, and the beneficiary set contracts — state-facing seats would compute far more costly, while victim seats would compute relief.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_article17, conceptual, 'Committer structure: this story is one of two readings of the Article 17 kernel; the sibling is a separate constraint, not a parameter of this one.').

omega_variable(
    genuineness_standard_indeterminacy,
    'Where is the line between ''genuine'' proceedings that bar international action and sham proceedings that permit it — and is that line determinate enough for the burden allocation to track anything real?',
    'Comparative analysis of admissibility rulings (Al-Senussi deference versus the Kenya authorization) against post-hoc outcomes: case collapses, continued violence, resumed atrocities in certified-willing states.',
    'If genuineness is indeterminate, the presumption of adequacy operates as near-automatic deference and effective burden on victims exceeds the measured extractiveness; if determinate, the gate filters accurately and part of the measured cost is the price of a workable standard.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuineness_standard_indeterminacy, empirical, 'Indeterminacy of the genuineness standard under a pro-state burden allocation.').

omega_variable(
    practical_unmeetability_of_burden,
    'Can the international court ever practically meet its burden to prove unwillingness, given that the decisive evidence (witness interference, prosecutorial instructions) sits inside the state''s control?',
    'Audit of failed admissibility challenges in which witness tampering or executive interference was documented by NGOs but unprovable to the governing standard.',
    'If the burden is practically unmeetable, the presumption functions as near-absolute immunity for compliant-on-paper states and the arrangement trends toward pure extraction for affected victims regardless of its coordination merits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_unmeetability_of_burden, empirical, 'Evidentiary asymmetry built into the burden allocation.').

omega_variable(
    positive_complementarity_offset,
    'Does the primacy arrangement''s capacity-building effect (positive complementarity) reduce net harm to victims over time by strengthening domestic justice systems?',
    'Longitudinal study of domestic atrocity prosecutions in situation countries before and after international-court engagement, controlling for donor assistance.',
    'If capacity gains materialize, part of the measured cost is transitional investment rather than rent, and the arrangement acquires a transition justification it currently lacks; if gains fail to appear, the coordination half of the account is hollow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positive_complementarity_offset, empirical, 'Whether capacity-building offsets the foreclosure of international remedy.').

omega_variable(
    domestic_consent_to_primacy,
    'Is the sovereignty protection this reading provides a good that affected populations themselves endorse, or an elite preference imposed without popular consent?',
    'Victim-preference and public-opinion surveys in situation countries regarding domestic versus international prosecution of atrocity crimes.',
    'If endorsed domestically, part of the cost borne by victims is consented coordination expense and the beneficiary structure is less purely elite; if not, the arrangement protects state elites against their own populations, sharpening the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_consent_to_primacy, preference, 'Whether primacy reflects popular or elite preference in affected states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__national_primacy_reading, 2002, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(art17np_tr_t2002, article_17_complementarity__national_primacy_reading, theater_ratio, 2002, 0.15).
narrative_ontology:measurement_basis(art17np_tr_t2002, observed).
narrative_ontology:measurement(art17np_tr_t2008, article_17_complementarity__national_primacy_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement_basis(art17np_tr_t2008, observed).
narrative_ontology:measurement(art17np_tr_t2011, article_17_complementarity__national_primacy_reading, theater_ratio, 2011, 0.21).
narrative_ontology:measurement_basis(art17np_tr_t2011, observed).
narrative_ontology:measurement(art17np_tr_t2014, article_17_complementarity__national_primacy_reading, theater_ratio, 2014, 0.27).
narrative_ontology:measurement_basis(art17np_tr_t2014, observed).
narrative_ontology:measurement(art17np_tr_t2017, article_17_complementarity__national_primacy_reading, theater_ratio, 2017, 0.32).
narrative_ontology:measurement_basis(art17np_tr_t2017, observed).
narrative_ontology:measurement(art17np_tr_t2020, article_17_complementarity__national_primacy_reading, theater_ratio, 2020, 0.37).
narrative_ontology:measurement_basis(art17np_tr_t2020, observed).
narrative_ontology:measurement(art17np_tr_t2023, article_17_complementarity__national_primacy_reading, theater_ratio, 2023, 0.41).
narrative_ontology:measurement_basis(art17np_tr_t2023, observed).
narrative_ontology:measurement(art17np_tr_t2025, article_17_complementarity__national_primacy_reading, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(art17np_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(art17np_be_t2002, article_17_complementarity__national_primacy_reading, base_extractiveness, 2002, 0.48).
narrative_ontology:measurement_basis(art17np_be_t2002, observed).
narrative_ontology:measurement(art17np_be_t2008, article_17_complementarity__national_primacy_reading, base_extractiveness, 2008, 0.46).
narrative_ontology:measurement_basis(art17np_be_t2008, observed).
narrative_ontology:measurement(art17np_be_t2011, article_17_complementarity__national_primacy_reading, base_extractiveness, 2011, 0.43).
narrative_ontology:measurement_basis(art17np_be_t2011, observed).
narrative_ontology:measurement(art17np_be_t2014, article_17_complementarity__national_primacy_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement_basis(art17np_be_t2014, observed).
narrative_ontology:measurement(art17np_be_t2017, article_17_complementarity__national_primacy_reading, base_extractiveness, 2017, 0.56).
narrative_ontology:measurement_basis(art17np_be_t2017, observed).
narrative_ontology:measurement(art17np_be_t2020, article_17_complementarity__national_primacy_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement_basis(art17np_be_t2020, observed).
narrative_ontology:measurement(art17np_be_t2023, article_17_complementarity__national_primacy_reading, base_extractiveness, 2023, 0.63).
narrative_ontology:measurement_basis(art17np_be_t2023, observed).
narrative_ontology:measurement(art17np_be_t2025, article_17_complementarity__national_primacy_reading, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement_basis(art17np_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(art17np_su_t2002, article_17_complementarity__national_primacy_reading, suppression_requirement, 2002, 0.35).
narrative_ontology:measurement_basis(art17np_su_t2002, observed).
narrative_ontology:measurement(art17np_su_t2008, article_17_complementarity__national_primacy_reading, suppression_requirement, 2008, 0.4).
narrative_ontology:measurement_basis(art17np_su_t2008, observed).
narrative_ontology:measurement(art17np_su_t2011, article_17_complementarity__national_primacy_reading, suppression_requirement, 2011, 0.44).
narrative_ontology:measurement_basis(art17np_su_t2011, observed).
narrative_ontology:measurement(art17np_su_t2014, article_17_complementarity__national_primacy_reading, suppression_requirement, 2014, 0.49).
narrative_ontology:measurement_basis(art17np_su_t2014, observed).
narrative_ontology:measurement(art17np_su_t2017, article_17_complementarity__national_primacy_reading, suppression_requirement, 2017, 0.53).
narrative_ontology:measurement_basis(art17np_su_t2017, observed).
narrative_ontology:measurement(art17np_su_t2020, article_17_complementarity__national_primacy_reading, suppression_requirement, 2020, 0.57).
narrative_ontology:measurement_basis(art17np_su_t2020, observed).
narrative_ontology:measurement(art17np_su_t2023, article_17_complementarity__national_primacy_reading, suppression_requirement, 2023, 0.6).
narrative_ontology:measurement_basis(art17np_su_t2023, observed).
narrative_ontology:measurement(art17np_su_t2025, article_17_complementarity__national_primacy_reading, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(art17np_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__national_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_17_complementarity__national_primacy_reading, international_oversight_reading).

% DUAL FORMULATION NOTE:
% article_17_complementarity decomposes into two readings with distinct epsilon values and distinct beneficiary/victim structures. This file (national_primacy_reading) concentrates the cost on victims in states with weak-but-genuine proceedings, restricting the effective victim set to complete judicial collapse; the sibling (international_oversight_reading) reallocates the burden to states, contracting the beneficiary set and expanding the victim set. The family link runs through shared jurisprudence: both readings cite the same clause and the same case law, so doctrinal drift in one propagates to the other — each new admissibility ruling is precedent that whichever reading wins will cite.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
