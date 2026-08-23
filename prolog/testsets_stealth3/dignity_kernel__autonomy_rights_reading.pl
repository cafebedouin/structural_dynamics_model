% ============================================================================
% CONSTRAINT STORY: dignity_kernel__autonomy_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignity_kernel__autonomy_rights_reading, []).

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
 *   constraint_id: dignity_kernel__autonomy_rights_reading
 *   human_readable: Autonomy-Rights Ground of Human Dignity (AI Governance Reading)
 *   domain: theological ethics / technology governance / philosophical anthropology
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the dignity_kernel: the
 *   autonomy-rights reading, on which human worth is grounded in autonomy,
 *   rationality, and rights rather than in divine image or in a revisable
 *   human nature. As an operative constraint, the reading coordinates AI
 *   governance around transparency, accountability, labor protection, and
 *   privacy; licenses enhancement cautiously inside a rights envelope; and
 *   makes dignity justiciable. Its structural asymmetry: protection is
 *   indexed to capacities that are unevenly distributed and unevenly
 *   exercisable, so the same standard that empowers capacitated adults
 *   mediates — and can fail — the profoundly impaired, while the consent
 *   apparatus shifts protective labor onto individuals and interpretation
 *   rents onto the professions. KEY AGENTS (by structural relationship):
 *   liberal_regulatory_institutions (institutional/constrained) —
 *   agenda-setter administering and interpreting the standard;
 *   capacitated_adult_persons (moderate/constrained) — primary beneficiary
 *   carrying consent burdens; rights_compliance_professions
 *   (organized/mobile) — secondary beneficiary collecting the apparatus's
 *   fees; enhancement_frontier_adopters (powerful/mobile) — beneficiary of
 *   the rights envelope; ai_developers (institutional/arbitrage) — payer
 *   gaining legitimacy in return; consent_overloaded_users and
 *   algorithmically_managed_workers (moderate/constrained) — payers bearing
 *   burden-shift and enforcement lag; severely_cognitively_impaired_persons
 *   (powerless/trapped) — primary target via proxy mediation;
 *   religious_dignity_traditions (organized/identity_locked) — excluded rival
 *   ground; disability_rights_advocates (organized/analytical) — observer
 *   supplying the evidentiary base. Claim and metrics are independent
 *   authored facts: the tangled_rope claim states what I believe structurally
 *   true; the metric values state what I believe descriptively true of the
 *   regime's operation; the engine computes per-seat types from the
 *   structural data and any divergence is the datum.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignity_kernel__autonomy_rights_reading, 0.54).
domain_priors:suppression_score(dignity_kernel__autonomy_rights_reading, 0.62).
domain_priors:theater_ratio(dignity_kernel__autonomy_rights_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dignity_kernel__autonomy_rights_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignity_kernel__autonomy_rights_reading, tangled_rope).
narrative_ontology:human_readable(dignity_kernel__autonomy_rights_reading, "Autonomy-Rights Ground of Human Dignity (AI Governance Reading)").
narrative_ontology:topic_domain(dignity_kernel__autonomy_rights_reading, "theological ethics / technology governance / philosophical anthropology").

domain_priors:requires_active_enforcement(dignity_kernel__autonomy_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignity_kernel__autonomy_rights_reading, 'cdc8aee2-25a7-4387-b095-51fbda7822fc').
narrative_ontology:cs_kernel_codification('cdc8aee2-25a7-4387-b095-51fbda7822fc', fixed_text).
narrative_ontology:cs_authority_grounding('cdc8aee2-25a7-4387-b095-51fbda7822fc', lineage).
narrative_ontology:cs_interpretation_layer_present('cdc8aee2-25a7-4387-b095-51fbda7822fc').
narrative_ontology:cs_reading_relation('cdc8aee2-25a7-4387-b095-51fbda7822fc', dignity_kernel__imago_dei_reading, coexists_with).
narrative_ontology:cs_reading_relation('cdc8aee2-25a7-4387-b095-51fbda7822fc', dignity_kernel__posthumanist_reading, influences).
narrative_ontology:cs_axiom('cdc8aee2-25a7-4387-b095-51fbda7822fc', foundational, autonomy_grounds_intrinsic_worth).
narrative_ontology:cs_axiom_status(autonomy_grounds_intrinsic_worth, holdable).
narrative_ontology:cs_axiom_grounding('cdc8aee2-25a7-4387-b095-51fbda7822fc', autonomy_grounds_intrinsic_worth, deontological).
narrative_ontology:cs_axiom('cdc8aee2-25a7-4387-b095-51fbda7822fc', secondary, rights_justiciability_of_dignity).
narrative_ontology:cs_axiom_status(rights_justiciability_of_dignity, holdable).
narrative_ontology:cs_axiom_grounding('cdc8aee2-25a7-4387-b095-51fbda7822fc', rights_justiciability_of_dignity, conventional).
narrative_ontology:cs_reference_frame('cdc8aee2-25a7-4387-b095-51fbda7822fc', autonomy_grounded_equal_moral_status).
narrative_ontology:cs_drift_state('cdc8aee2-25a7-4387-b095-51fbda7822fc', contemporary_algorithmic_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cdc8aee2-25a7-4387-b095-51fbda7822fc', '').
narrative_ontology:cs_kernel_id(dignity_kernel__autonomy_rights_reading, dignity_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, capacitated_adult_persons).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, rights_compliance_professions).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, enhancement_frontier_adopters).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, severely_cognitively_impaired_persons).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, consent_overloaded_users).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, algorithmically_managed_workers).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, ai_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignity_kernel__autonomy_rights_reading, ai_developers).
narrative_ontology:constraint_victim(dignity_kernel__autonomy_rights_reading, capacitated_adult_persons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Constitutional courts, data-protection authorities, and legislatures that codify and adjudicate the autonomy-rights standard. They define what counts as a violation, staff the enforcement bodies, and justify budgets and jurisdiction by the caseload the standard generates. They cannot exit the legal orders they administer, but they control how the standard is interpreted and where its boundaries sit.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, liberal_regulatory_institutions, agenda_setter,
    institutional, generational, constrained, continental).

% Adults with unimpaired decision-making capacity whose claims under the standard are strongest and easiest to enforce. They hold consent rights over their data and treatment and can litigate violations; they are the paradigm case the doctrine was drafted for. They also carry the routine work of exercising those rights — reading notices, managing settings, filing objections — which falls hardest on those with the least time and expertise.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, capacitated_adult_persons, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__autonomy_rights_reading, capacitated_adult_persons, payer).

% Privacy lawyers, data-protection officers, auditors, and ethics consultants paid to operate the apparatus the standard requires. Every new obligation expands billable scope; their livelihoods are tied to the standard's complexity and continued enforcement. Credentials price out across jurisdictions, so exit is easy.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, rights_compliance_professions, beneficiary,
    organized, biographical, mobile, global).

% Individuals and clinics using permitted cognitive, reproductive, and biological enhancement inside the rights envelope. The standard's cautious openness licenses their activity while requiring consent documentation and safety compliance. They benefit from the permissiveness and can relocate to favorable jurisdictions when limits tighten.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, enhancement_frontier_adopters, beneficiary,
    powerful, biographical, mobile, global).

% Firms building and deploying AI systems. They bear documentation, transparency, and audit obligations and face liability when systems override user autonomy. They also gain from the standard: certification signals legitimacy and clear rules shield them from open-ended moral accusation. Jurisdiction shopping gives them leverage over where obligations bind.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, ai_developers, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__autonomy_rights_reading, ai_developers, beneficiary).

% Ordinary users of digital services who are formally protected through consent and objection rights but face hundreds of opaque agreements a year. Meaningful refusal is practically unavailable without abandoning the services altogether, so protection depends on attention and expertise most users do not have.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, consent_overloaded_users, payer,
    moderate, biographical, constrained, global).

% Gig workers, warehouse staff, and content moderators managed by automated scheduling, scoring, and termination systems. The standard grants them labor and privacy claims on paper, but enforcement lags deployment; their working conditions are set by systems they cannot inspect or negotiate with directly.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, algorithmically_managed_workers, payer,
    moderate, biographical, constrained, global).

% People with profound intellectual disability, advanced dementia, or comparable conditions, whose protection runs through guardians, capacity assessments, and best-interest procedures rather than through their own exercised autonomy. Their claims are mediated at every step, and the record of guardianship abuse shows the mediation can fail precisely where vulnerability is greatest.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, severely_cognitively_impaired_persons, payer,
    powerless, biographical, trapped, national).

% Communities whose accounts of human worth rest on grounds other than autonomy and rights. Public-reason conventions in most constitutional jurisdictions treat their premises as inadmissible in adjudication: they may testify to harms but not argue grounds. Leaving the tradition would mean abandoning the community and worldview that constitute them, so they contest from outside the room.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, religious_dignity_traditions, excluded,
    organized, civilizational, identity_locked, global).

% Scholars and organizations documenting how capacity-indexed protection performs for the people they represent. They analyze guardianship records, sit on ethics bodies, and publish critiques. They hold no enforcement power but supply the evidence base on which other seats act.
narrative_ontology:constraint_stakeholder(dignity_kernel__autonomy_rights_reading, disability_rights_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignity_kernel__autonomy_rights_reading, rights_compliance_professions).
narrative_ontology:fixing_cost_class(dignity_kernel__autonomy_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, secularly admissible standard for when a person may be overridden — enabling legislation, adjudication, medical-ethics procedure, and cross-border AI governance without appeal to sectarian premises, and making dignity enforceable through rights rather than leaving it to the discretion of institutions.
% TRANSFER_FUNCTION: Moves decision-authority over personal data, labor conditions, and bodily modification from institutions toward individual rightsholders via consent and claim-rights; simultaneously moves compliance costs onto developers and deployers, and interpretation work — and its fees — onto the legal and auditing professions.
% ABSENT_VOICES: Religious dignity traditions are procedurally excluded from adjudication by public-reason convention — they may report harms but not argue grounds. Severely cognitively impaired persons appear only through proxies and capacity assessors. Future persons shaped by today's enhancement norms have no seat anywhere in the process, nor do populations governed by AI systems built in other jurisdictions.
% DISAPPEARANCE_RATIONALE: If the autonomy-rights ground vanished overnight, post-war human-rights law, data-protection regimes, AI acts, research-ethics review, and guardianship law would lose their operative foundation simultaneously. Protection of persons would collapse into ad hoc institutional discretion or migrate wholesale to whichever rival ground could be mobilized fastest — the entire justificatory architecture of contemporary governance depends on it.
% FOUNDING_PROBLEM: After state atrocities justified by racial and eugenic hierarchy, the drafters needed a ground of human worth that no state could deny, that could anchor enforceable rights without sectarian premises, and that would extend to the medically dependent and cognitively impaired. The problem was later extended to algorithmic systems as opaque automated decisions began to govern employment, credit, and care.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: disability-rights scholars attest the founding problem (protection of the profoundly vulnerable) remains live while disputing whether capacity-indexed grounding serves it; religious ethicists attest the problem is live while rejecting the secular exclusivity of the solution; UN treaty-body jurisprudence and national constitutional case law independently document continuing reliance on the standard. No corroborating source defends the capacity-grounding itself — they confirm the problem, not the reading.
narrative_ontology:disappearance_verdict(dignity_kernel__autonomy_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(dignity_kernel__autonomy_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignity_kernel__autonomy_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignity_kernel__autonomy_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dignity_kernel__autonomy_rights_reading, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignity_kernel__autonomy_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignity_kernel__autonomy_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignity_kernel__autonomy_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.54: the regime's burdens are real but side-effects of a protective design — capacity-indexing exposes the impaired to proxy failure, consent shifts protective labor onto users, and enforcement lag leaves algorithmically managed workers holding paper rights; none of these is the regime's purpose, which is why epsilon sits mid-range rather than high. Suppression 0.62: the regime actively excludes rival dignity grounds from adjudication via public-reason convention, mandates participation in consent-governed data economies, and enforces through an extensive court-and-authority apparatus; suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater_ratio 0.38: ethics boards, impact assessments, and documentation regimes deliver some real protection but a growing share of activity is compliance performance — the rising series tracks professionalization of paperwork. Accessibility_collapse 0.35: understanding the standard does not collapse alternatives — the whole point of the kernel contest is that rival grounds remain live and articulable, so alternatives persist at unusually high visibility for a governing norm. Resistance 0.50: sustained pushback from religious traditions, disability scholarship, and regulated industry, none decisive. The measurement series run on ONE shared time grid (all three metrics authored at all eight points, 1948–2026) so no metric row borrows another's end-state values; the trajectory is monotonic professionalization drift, not cyclical, so no oscillation analysis applies; base_properties values are the end-state (2026) phase of the series. Enforcement capacity is tracked via suppression_requirement because the story's dynamic IS enforcement build-out: from declaratory treaties (1948) through treaty bodies, constitutional entrenchment, data-protection authorities, and AI-act enforcement machinery.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural inputs. From the regulator's seat the arrangement is a coordination achievement it built and staffs — caseload justifies jurisdiction. From the compliance professions' seat it is a livelihood. From the capacitated adult's seat it is mostly background assurance punctuated by consent chores. From the impaired person's seat — reached only through proxies — it is a contingent grant revocable by assessment. From the worker's seat it is a widening gap between declared rights and deployed systems. From the excluded religious seat it is a procedural wall that admits testimony but forbids grounds. The engine computes these divergences from power, exit, and directional position; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: severely_cognitively_impaired_persons derive near the full-target end (trapped, powerless, all protection mediated); consent_overloaded_users and algorithmically_managed_workers derive high-d (constrained exit, burdens concentrated); rights_compliance_professions and enhancement_frontier_adopters derive near the beneficiary end (collect fees and permissiveness respectively, mobile exit); liberal_regulatory_institutions derive low-d as administrators. Two overrides correct derivations the structural data alone cannot produce: (1) capacitated_adult_persons are declared beneficiaries, which derives a strongly subsidized d (~0.15), but their consent-burden payer role is genuine and continuous — override to 0.40 at the moderate power atom reflects the dual position; (2) ai_developers are declared payers, deriving a strongly targeted d (~0.75), but certification legitimacy, liability clarity, and jurisdiction-shopping leverage return real value to them — override to 0.55 at the powerful power atom. Overrides are corrections to wrong derivations, not substitutes for the underlying beneficiary/victim declarations, which remain the structural source.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents mislabeling in both directions. Read as pure rope (the regime's own self-presentation: a neutral rights framework benefiting everyone), the capacity gradient, consent regressivity, and professional rent collection disappear from the ledger — the coordination story would launder asymmetric extraction. Read as pure snare (the abolitionist temptation: all rights-apparatus as domination), the genuine coordination function vanishes — the post-war settlement demonstrably solved a real collective problem, and the founding problem is still live. The founding-problem interview anchors the middle: status=live, corroborated from outside the beneficiary set (disability scholars and religious ethicists confirm the problem while disputing the ground), paired with disappearance_verdict=world_rearranges — no dead-mandate mismatch, so no zombie flag fires; this is not a piton wearing dignity's clothing. The identity-lock dynamics sit with the excluded seat: religious_dignity_traditions are held in place ideologically — the ground IS the community's self-concept — so their exclusion persists without force; were that identity frame to break (secularized theology accepting rights-grounding wholesale), the excluded seat would empty and measured resistance would drop without anything being resolved. Identity_coordination is declared as the Boltzmann type because the regime's dominant function is boundary maintenance — determining who counts as a full rightsholder against evolving criteria — and the corpus warning applies with full force here: identity framing is exactly where this regime's extraction hides (the capacity gradient is a membership-boundary effect), so the coupling diagnostics deserve scrutiny rather than the offset's leniency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_split,
    'This constraint is one reading of the dignity_kernel (autonomy_rights_reading); the imago-dei and posthumanist readings instantiate different constraints with different victim sets and governance deltas. Which ground actually governs when readings conflict inside a single adjudication?',
    'Citation analysis of judicial and regulatory decisions in capacity, end-of-life, and AI-accountability cases: count which ground is invoked when the readings diverge in outcome.',
    'If imago-dei reasoning silently persists inside doctrine while the autonomy-rights reading supplies the stated justification, this story''s epsilon is partly misattributed and the effective victim set is narrower than authored; if the readings never collide in outcomes, the family decomposition is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment_split, conceptual, 'Committer structure: which sibling reading governs when readings conflict in practice.').

omega_variable(
    capacity_floor_derivativity,
    'Does the regime protect capacity-marginal persons through a stable equal-status floor independent of capacity, or only derivatively through capacity proxies and guardian mediation?',
    'Comparative review of guardianship, disability, and end-of-life law: test whether protections survive total incapacity and contested proxy authority, using the disability-rights literature as the external evidentiary base.',
    'If protection is purely derivative, the victim set widens sharply and the payer seat''s effective burden approaches the full-target end — pushing the computed type toward the extractive pole for that seat; a stable floor would support the coordination-heavy reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_floor_derivativity, empirical, 'Whether the dignity floor is capacity-independent or proxy-mediated for the impaired.').

omega_variable(
    consent_burden_regressivity,
    'Is consent-based protection a genuine exercise of the autonomy it invokes, or a cost-shift that only the resourced can cash?',
    'Audit actual opt-out and objection rates, comprehension studies on notice documents, and litigation access stratified by income and education.',
    'If burden-shifting dominates, effective extraction on ordinary users is materially higher than the authored scalar suggests and the capacitated-adult beneficiary seat is closer to symmetric than the declaration implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_burden_regressivity, empirical, 'Regressivity of the consent apparatus across the user population.').

omega_variable(
    enhancement_boundary_mobility,
    'Where exactly do the ''rights limits'' on enhancement sit, and are they stable — or does the boundary migrate under commercial and posthumanist pressure?',
    'Track regulatory decisions on germline editing, cognitive enhancement, and neural interfaces over successive review cycles.',
    'Boundary migration changes the beneficiary set (enlarging enhancement_frontier_adopters) and can pull this reading''s structure toward the posthumanist sibling''s, altering the family''s classification geometry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enhancement_boundary_mobility, preference, 'Stability of the rights-envelope around permitted enhancement.').

omega_variable(
    public_reason_exclusion_status,
    'Is the procedural exclusion of theological dignity grounds from adjudication a neutral condition of shared deliberation, or suppression of a rival ground that inflates this reading''s persistence?',
    'Compare jurisdictions with different public-reason settlements: if outcomes and protections track the exclusion itself rather than the harms it filters, the exclusion functions as competitive suppression; if outcomes are invariant, it is procedural neutrality.',
    'If suppressive, the authored suppression scalar understates the mechanism holding this reading in place and the excluded seat''s position approaches the target end despite holding no formal payer role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_reason_exclusion_status, conceptual, 'Neutral procedure versus rival-ground suppression in public-reason exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignity_kernel__autonomy_rights_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dignity_kernel_ar_tr_t1948, dignity_kernel__autonomy_rights_reading, theater_ratio, 1948, 0.08).
narrative_ontology:measurement(dignity_kernel_ar_tr_t1966, dignity_kernel__autonomy_rights_reading, theater_ratio, 1966, 0.11).
narrative_ontology:measurement(dignity_kernel_ar_tr_t1975, dignity_kernel__autonomy_rights_reading, theater_ratio, 1975, 0.14).
narrative_ontology:measurement(dignity_kernel_ar_tr_t1989, dignity_kernel__autonomy_rights_reading, theater_ratio, 1989, 0.18).
narrative_ontology:measurement(dignity_kernel_ar_tr_t2000, dignity_kernel__autonomy_rights_reading, theater_ratio, 2000, 0.23).
narrative_ontology:measurement(dignity_kernel_ar_tr_t2010, dignity_kernel__autonomy_rights_reading, theater_ratio, 2010, 0.29).
narrative_ontology:measurement(dignity_kernel_ar_tr_t2018, dignity_kernel__autonomy_rights_reading, theater_ratio, 2018, 0.34).
narrative_ontology:measurement(dignity_kernel_ar_tr_t2026, dignity_kernel__autonomy_rights_reading, theater_ratio, 2026, 0.38).

% Extraction over time
narrative_ontology:measurement(dignity_kernel_ar_be_t1948, dignity_kernel__autonomy_rights_reading, base_extractiveness, 1948, 0.22).
narrative_ontology:measurement(dignity_kernel_ar_be_t1966, dignity_kernel__autonomy_rights_reading, base_extractiveness, 1966, 0.27).
narrative_ontology:measurement(dignity_kernel_ar_be_t1975, dignity_kernel__autonomy_rights_reading, base_extractiveness, 1975, 0.31).
narrative_ontology:measurement(dignity_kernel_ar_be_t1989, dignity_kernel__autonomy_rights_reading, base_extractiveness, 1989, 0.36).
narrative_ontology:measurement(dignity_kernel_ar_be_t2000, dignity_kernel__autonomy_rights_reading, base_extractiveness, 2000, 0.41).
narrative_ontology:measurement(dignity_kernel_ar_be_t2010, dignity_kernel__autonomy_rights_reading, base_extractiveness, 2010, 0.47).
narrative_ontology:measurement(dignity_kernel_ar_be_t2018, dignity_kernel__autonomy_rights_reading, base_extractiveness, 2018, 0.51).
narrative_ontology:measurement(dignity_kernel_ar_be_t2026, dignity_kernel__autonomy_rights_reading, base_extractiveness, 2026, 0.54).

% Suppression requirement over time
narrative_ontology:measurement(dignity_kernel_ar_su_t1948, dignity_kernel__autonomy_rights_reading, suppression_requirement, 1948, 0.18).
narrative_ontology:measurement(dignity_kernel_ar_su_t1966, dignity_kernel__autonomy_rights_reading, suppression_requirement, 1966, 0.26).
narrative_ontology:measurement(dignity_kernel_ar_su_t1975, dignity_kernel__autonomy_rights_reading, suppression_requirement, 1975, 0.31).
narrative_ontology:measurement(dignity_kernel_ar_su_t1989, dignity_kernel__autonomy_rights_reading, suppression_requirement, 1989, 0.38).
narrative_ontology:measurement(dignity_kernel_ar_su_t2000, dignity_kernel__autonomy_rights_reading, suppression_requirement, 2000, 0.44).
narrative_ontology:measurement(dignity_kernel_ar_su_t2010, dignity_kernel__autonomy_rights_reading, suppression_requirement, 2010, 0.51).
narrative_ontology:measurement(dignity_kernel_ar_su_t2018, dignity_kernel__autonomy_rights_reading, suppression_requirement, 2018, 0.57).
narrative_ontology:measurement(dignity_kernel_ar_su_t2026, dignity_kernel__autonomy_rights_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignity_kernel__autonomy_rights_reading, identity_coordination).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, dignity_kernel__imago_dei_reading).
narrative_ontology:affects_constraint(dignity_kernel__autonomy_rights_reading, dignity_kernel__posthumanist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'human dignity' conflates three structurally distinct constraints and is decomposed per the epsilon-invariance principle into a constraint family: this autonomy-rights reading (dignity grounded in capacity and rights; moderately extractive through capacity-indexing and consent-burden shifting; governs most positive law), the imago-dei reading (dignity as capability-independent divine-image status; different victim set — no capacity gradient), and the posthumanist reading (the human as revisable limit; different beneficiary set — enhancement frontier). The imago-dei reading is historically upstream (its premises were live in mid-century drafting even where not codified); this reading currently supplies the operative legal standard; the posthumanist reading is downstream, pressing against the fixed-human premise this reading stabilizes. Each member carries its own epsilon, beneficiaries, and victims; edges here implement the family-linkage rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignity_kernel__autonomy_rights_reading, moderate, 0.4).
constraint_indexing:directionality_override(dignity_kernel__autonomy_rights_reading, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
