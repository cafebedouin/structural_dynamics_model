% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: 1890 Manifesto as Federally-Coerced Practice Reversal (Doctrine Unrevised)
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint models the 1890 Manifesto ending public solemnization of
 *   plural marriage in the LDS Church as an act of exogenous coercion rather
 *   than internal doctrinal revision. From the Edmunds Act (1882) through the
 *   Edmunds-Tucker Act (1887) — which disincorporated the church and
 *   authorized escheat of its property above a statutory ceiling — federal
 *   pressure escalated to the point of existential institutional threat. This
 *   reading holds that the Manifesto's carefully worded text ('I hereby
 *   declare my intention to submit to those laws, and to use my influence
 *   with the members of the Church over which I preside to have them do
 *   likewise') is a statement of practical submission, not a revelatory
 *   abrogation of Section 132, which remained (and remains) canonized
 *   scripture. This is ONE of three linked readings of the same kernel: the
 *   endogenous_reinterpretation_reading treats the same event as authentic
 *   internal revelation (Woodruff's vision), and practice_doctrine_gap treats
 *   the persistent coexistence of unrenounced doctrine and suspended practice
 *   as the structurally interesting fact in itself, independent of which
 *   causal story explains the suspension. Each reading carries its own
 *   epsilon; this one is authored highest because from this reading's lights
 *   the 'coordination' achieved is coordination of federal territorial
 *   control, not of any genuine internal church interest, and the extraction
 *   (institutional and personal autonomy, property, capacity to act on
 *   canonized belief) is severe and largely one-directional.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.81).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.87).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "1890 Manifesto as Federally-Coerced Practice Reversal (Doctrine Unrevised)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, 'f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0').
narrative_ontology:cs_kernel_codification('f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0', fixed_text).
narrative_ontology:cs_authority_grounding('f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0', lineage).
narrative_ontology:cs_interpretation_layer_present('f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0').
narrative_ontology:cs_reading_relation('f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0', foundational, practice_reversal_did_not_require_or_receive_doctrinal_revision).
narrative_ontology:cs_axiom_status(practice_reversal_did_not_require_or_receive_doctrinal_revision, holdable).
narrative_ontology:cs_axiom_grounding('f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0', practice_reversal_did_not_require_or_receive_doctrinal_revision, empirically_contingent).
narrative_ontology:cs_axiom('f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0', foundational, external_coercive_threat_was_sufficient_cause_of_reversal).
narrative_ontology:cs_axiom_status(external_coercive_threat_was_sufficient_cause_of_reversal, holdable).
narrative_ontology:cs_axiom_grounding('f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0', external_coercive_threat_was_sufficient_cause_of_reversal, empirically_contingent).
narrative_ontology:cs_reference_frame('f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0', plural_marriage_as_divinely_commanded_practice).
narrative_ontology:cs_drift_state('f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0', manifesto_and_statehood_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('f5ebf6fd-55ea-4e1b-8940-3c2f43731fb0', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, anti_polygamy_political_coalition).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_practicing_households).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, church_property_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_church_leadership).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, federal_supremacy_over_territorial_religious_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and escalates anti-polygamy statutes (Edmunds Act, Edmunds-Tucker Act), disincorporates the church as a legal entity, and moves to seize its property and disenfranchise practicing members. Sets the terms under which the territory can proceed toward statehood. Collects the concession — public renunciation of the practice — as the price of institutional survival, without needing to touch or adjudicate the underlying doctrine.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_government, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_government, agenda_setter).

% Issues the 1890 Manifesto under Wilford Woodruff's signature, publicly advising members to conform to federal marriage law. Retains formal ecclesiastical authority to interpret Section 132 and does not rescind it as scripture. Faces the choice between continued asset seizure and mass imprisonment of leadership, or a public statement whose wording carefully declares intent to submit to law rather than declaring the underlying revelation abrogated. From this reading's vantage the reversal is compliance under duress, not internal doctrinal correction.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_church_leadership, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_church_leadership, payer).

% Existing plural families face the practical unwinding of arrangements they entered as religious covenant, under threat of prosecution, property loss, and social exile if they continue publicly. Many households persist privately for years while the institution manages public compliance; the individuals bear the direct cost of a policy reversal they had no hand in choosing and that their own scripture had authorized.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, plural_marriage_practicing_households, payer,
    powerless, biographical, trapped, regional).

% Hold church-controlled assets subject to seizure proceedings under the Edmunds-Tucker Act's escheat provisions. Their institutional and personal economic stability is used as leverage in the federal government's coercive calculus; asset forfeiture threat is the direct mechanism forcing the Manifesto's issuance.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, church_property_holders, payer,
    moderate, biographical, constrained, regional).

% National reformist and Protestant-aligned political actors campaign against plural marriage as a moral and political threat, supplying the legislative pressure that manufactures the coercive leverage. They achieve a public capitulation without needing the church to renounce the underlying theology, and their movement's aims are satisfied by visible compliance rather than doctrinal concession.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, anti_polygamy_political_coalition, beneficiary,
    organized, generational, mobile, national).

% The canonized revelation authorizing plural marriage remains part of the Doctrine and Covenants throughout and after the Manifesto period. It is not consulted, revised, or repudiated in the political process that produces the reversal — its persistence as unaltered doctrine is precisely what this reading holds distinguishes external coercion from internal revision.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, section_132_as_scripture, excluded,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__exogenous_override_reading, section_132_as_scripture).

% Assess archival correspondence, court records, and the Manifesto's actual drafting history to evaluate whether the 1890 declaration reflects genuine revelatory change of heart or documented capitulation to imminent federal seizure. Their scholarship supplies the corroborating record this reading rests on.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, later_church_historians, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_government).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None genuine from this reading's vantage: no collective-action problem is solved by the reversal itself. The only 'coordination' achieved is the federal government's coordination of territorial legal uniformity around monogamous marriage, imposed on an unwilling religious institution rather than negotiated with it.
% TRANSFER_FUNCTION: Moves institutional autonomy, property, and doctrinal authority to act on stated belief from the LDS Church and its practicing members to the federal government and the political coalition that lobbied for suppression; the church retains formal ownership of the doctrine but forfeits the practical capacity to act on it publicly.
% ABSENT_VOICES: Plural marriage households themselves were not party to the negotiation between church leadership and federal authorities; their lived arrangements were the bargaining chip, not a represented interest. Female participants in plural households in particular had no institutional voice in either federal legislative debate or church leadership's response.
% DISAPPEARANCE_RATIONALE: Had the federal coercive apparatus (disincorporation, escheat, disenfranchisement) not existed or not been applied, there is no internal institutional pressure recorded in this reading's evidentiary basis sufficient to produce the 1890 declaration — the practice would very plausibly have continued as doctrinally authorized. Removing the coercion removes the reversal; the doctrine was never the obstacle.
% FOUNDING_PROBLEM: The federal government needed political and legal grounds to bring Utah Territory into the union on terms acceptable to national public opinion, which required eliminating polygamy as a visible practice; escalating criminal and property sanctions were built to force exactly this outcome.
% FOUNDING_PROBLEM_CORROBORATION: Congressional debate records and federal prosecutorial correspondence from outside the church attest that the pressure campaign was explicitly designed to force public renunciation as a precondition for statehood, and that the campaign's own architects considered the problem solved once the Manifesto issued — corroboration exists outside the beneficiary set (the church did not originate or celebrate the coercive framing; it is documented by the coercing party and by later independent historians of the territorial period).
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply and monotonically from 1862 through 1890 (0.28 to 0.81), tracking the escalating federal statutory and enforcement regime, then eases only slightly by 1896 once statehood is achieved and confiscated property is partially restored — this reading holds the extraction was substantially completed by the coercive act itself and does not fully reverse. Suppression follows a similar arc and peaks with the Edmunds-Tucker enforcement wave (0.87 at 1890), reflecting active federal marshaling, disenfranchisement, and property seizure as the mechanisms making resistance costly. Theater ratio is authored as rising (0.10 to 0.48) because from this reading's vantage an increasing share of the institution's public posture after 1890 — periodic reaffirmations of compliance, subsequent 1904 Second Manifesto, excommunications of continuing practitioners — functions as performative demonstration of submission to a federal audience, layered atop a doctrine that was never doctrinally revised.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's seat, this looks like successful law enforcement resolving a settled question of national marriage policy — clean, closed, non-extractive from its own frame. From the LDS institutional leadership's seat under this reading, it is compliance purchased at the price of core autonomy, with the underlying revelation left formally intact — an asymmetry the engine should register as tangled_rope from the payer side (real coordination of a territorial legal order exists, but it rides on genuine extraction of a specific institution's self-governance) while the beneficiary seat may compute closer to ordinary successful enforcement. The gap is the story's point: whether this event reads as principled reform or coerced capitulation depends entirely on which reading of the kernel is in view, and this file deliberately holds only the coercion reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal territorial government and the anti-polygamy coalition are coded as beneficiaries: they obtain the practical outcome they sought (public suppression, then statehood on their terms) without needing to engage or defeat the underlying theological claim. LDS institutional leadership sits as both agenda-setter (it drafts and issues the Manifesto, retaining formal authority over doctrine) and payer (it forfeits property, public practice, and unconstrained institutional autonomy under duress) — this dual role is exactly why the seat computes differently depending on which lens is applied. Practicing households and property holders are coded as high-d targets with trapped or constrained exit: they bear the concrete costs (forced separation from covenant relationships, property loss, social stigma) of a decision made above them under pressure they did not create and could not resist.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists collapsing the event into either 'pure natural moral progress' (which would erase the coercive mechanism and its victims) or 'pure conspiracy of extraction' (which would erase the genuine national interest in uniform civil marriage law that motivated the coalition). Classifying it as tangled_rope — coordination function (uniform territorial marriage law, a path to statehood) genuinely present, but bound to asymmetric extraction from a specific institution and its members enforced through active federal coercion — keeps both halves visible rather than letting either the coordination story or the extraction story stand alone as the whole account.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_revelation_causal_priority,
    'Was the September 1890 vision reported by Woodruff a genuine independent causal factor, or was it a retrospective theological framing applied to a decision already compelled by the imminence of total property forfeiture and additional imprisonments?',
    'Close comparative reading of Woodruff''s private journal entries against the timeline of federal legal proceedings (particularly the pending Supreme Court case threatening total escheat) and correspondence among church leadership in the weeks immediately preceding the Manifesto''s issuance; assessment of whether the vision account appears in contemporaneous records or is elaborated later.',
    'If the coercive timeline dominates and the vision account is a later theological gloss, this exogenous_override_reading is the historically dominant account and the sibling endogenous_reinterpretation_reading is best read as retrospective legitimation. If the vision account is substantiated as contemporaneous and causally prior to the final coercive escalation, the sibling reading gains ground and this reading''s high extraction figure would be an overstatement of the causal mechanism, though not of the coercive context in which the decision was made.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_revelation_causal_priority, empirical, 'Whether coercion or revelation was causally prior in producing the 1890 Manifesto.').

omega_variable(
    doctrine_never_renounced_significance,
    'Does Section 132''s continued canonical status (never formally rescinded, only administratively superseded by later church policy and the 1904 Second Manifesto) indicate that this reading''s coercion account is correct — because a genuine internal revelatory reversal would be expected to revise the underlying doctrine — or is doctrinal non-revision simply how this religious tradition typically handles superseded revelation (retained as historical record without current operative force)?',
    'Comparative analysis of how the LDS tradition has handled other instances of doctrinal supersession (e.g., other revelations later treated as historically bounded) to establish whether non-rescission is diagnostic of coercion specifically or a general feature of the tradition''s textual practice.',
    'If non-rescission is a general tradition-wide practice unrelated to coercion, the doctrine-practice gap is better explained by the sibling practice_doctrine_gap reading as a structural feature independent of causal mechanism, weakening this reading''s inference from non-rescission to coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_never_renounced_significance, conceptual, 'Whether Section 132''s non-rescission is diagnostic of coercion or a general textual practice of the tradition.').

omega_variable(
    institutional_sovereignty_beneficiary_of_statehood,
    'Did the church''s institutional leadership, despite bearing the immediate coercive cost, ultimately benefit from the arrangement via the political stability, property restoration, and social legitimacy statehood provided by 1896 — complicating a clean victim characterization?',
    'Trace church financial and political position from 1890 through 1900s statehood-era normalization, including returned property and subsequent political integration of church-aligned candidates into Utah state government.',
    'If substantial secondary benefit accrued to the institution post-coercion, the leadership seat may be better modeled with a directionality override reflecting partial beneficiary status by the end of the interval, rather than a pure payer role throughout.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_sovereignty_beneficiary_of_statehood, empirical, 'Whether the coerced institution later captured compensating benefits from statehood, complicating its victim classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1862, 1896).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1862, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1862, 0.1).
narrative_ontology:measurement_basis(marr_tr_t1862, observed).
narrative_ontology:measurement(marr_tr_t1874, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1874, 0.15).
narrative_ontology:measurement_basis(marr_tr_t1874, observed).
narrative_ontology:measurement(marr_tr_t1882, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1882, 0.22).
narrative_ontology:measurement_basis(marr_tr_t1882, observed).
narrative_ontology:measurement(marr_tr_t1887, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1887, 0.33).
narrative_ontology:measurement_basis(marr_tr_t1887, observed).
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1890, 0.42).
narrative_ontology:measurement_basis(marr_tr_t1890, observed).
narrative_ontology:measurement(marr_tr_t1896, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1896, 0.48).
narrative_ontology:measurement_basis(marr_tr_t1896, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1862, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1862, 0.28).
narrative_ontology:measurement_basis(marr_be_t1862, observed).
narrative_ontology:measurement(marr_be_t1874, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1874, 0.41).
narrative_ontology:measurement_basis(marr_be_t1874, observed).
narrative_ontology:measurement(marr_be_t1882, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1882, 0.58).
narrative_ontology:measurement_basis(marr_be_t1882, observed).
narrative_ontology:measurement(marr_be_t1887, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1887, 0.76).
narrative_ontology:measurement_basis(marr_be_t1887, observed).
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1890, 0.81).
narrative_ontology:measurement_basis(marr_be_t1890, observed).
narrative_ontology:measurement(marr_be_t1896, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1896, 0.79).
narrative_ontology:measurement_basis(marr_be_t1896, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1862, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1862, 0.35).
narrative_ontology:measurement_basis(marr_su_t1862, observed).
narrative_ontology:measurement(marr_su_t1874, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1874, 0.5).
narrative_ontology:measurement_basis(marr_su_t1874, observed).
narrative_ontology:measurement(marr_su_t1882, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1882, 0.68).
narrative_ontology:measurement_basis(marr_su_t1882, observed).
narrative_ontology:measurement(marr_su_t1887, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1887, 0.85).
narrative_ontology:measurement_basis(marr_su_t1887, observed).
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1890, 0.87).
narrative_ontology:measurement_basis(marr_su_t1890, observed).
narrative_ontology:measurement(marr_su_t1896, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1896, 0.79).
narrative_ontology:measurement_basis(marr_su_t1896, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the marriage_commitment_reversal kernel (the 1890 Manifesto and Section 132's fate). endogenous_reinterpretation_reading treats the reversal as authentic internal revelation with correspondingly low extraction (an institution freely reinterpreting its own doctrine extracts nothing from itself by that act). practice_doctrine_gap treats the coexistence of unrenounced doctrine and suspended practice as the analytically central structural fact, independent of causal mechanism. This file (exogenous_override_reading) authors the highest extraction of the three, reflecting its premise that the reversal was substantially compelled by federal coercive machinery rather than freely chosen. All three share the same underlying event and kernel_id but diverge in epsilon, beneficiary/victim sets, and computed type, per the epsilon-invariance principle — they are not the same constraint measured three ways, but three distinct constraints sharing one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
