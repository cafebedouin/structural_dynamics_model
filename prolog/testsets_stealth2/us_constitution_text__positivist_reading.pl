% ============================================================================
% CONSTRAINT STORY: us_constitution_text__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__positivist_reading, []).

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
 *   constraint_id: us_constitution_text__positivist_reading
 *   human_readable: Positivist Validity Criterion for U.S. Constitutional Law (Source-Validity Reading)
 *   domain: legal/constitutional/interpretive
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   us_constitution_text: the positivist reading, under which constitutional
 *   validity derives exclusively from formal enactment procedures — ratified
 *   text and validly enacted Article V amendments — and neither moral content
 *   nor historical meaning carries independent legal authority. Operatively,
 *   the reading is a procedural constraint on American constitutional
 *   interpretation: judges administering it must refuse relief grounded
 *   solely in unenacted justice claims, while enacting coalitions gain
 *   supremacy for their products without owing anyone a moral defense. The
 *   arrangement has a genuine coordination function (a shared, publicly
 *   checkable validity test that lets authority disputes terminate) and a
 *   genuine extraction asymmetry (people outside the enactment channels
 *   absorb systematic legal nullity through the same structure that
 *   stabilizes insiders). Per the epsilon-invariance principle, the
 *   colloquial label 'constitutional interpretation' decomposes into three
 *   structurally distinct constraint stories: this file (positivist reading,
 *   epsilon ~0.52, victims = unenacted claimants), the originalist reading
 *   (adds historical-meaning recovery; different epsilon and victim
 *   emphasis), and the living-constitutionalist reading (admits
 *   social-evolution authority; different extraction profile). The siblings
 *   are separate files linked via network.affects_constraints; their epsilon
 *   values differ by a wide margin, which is why they are not merged here.
 *   Claim and metrics are authored independently: the claimed type reflects
 *   the structure I believe true (real coordination plus real asymmetric
 *   extraction under active enforcement), and the metrics reflect the
 *   operation I believe descriptively accurate — the engine computes per-seat
 *   classifications from the structural data, and any divergence between
 *   claim and computed verdict is the datum the corpus exists to collect.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Agenda-setting administrator (institutional/identity_locked) — applies and enforces the source-validity test; absorbs the professional discipline of foreclosing outcome-based reasoning
 *   - article_v_amending_coalitions: Primary beneficiary (institutional/mobile) — supermajority enactments become supreme law without moral re-justification
 *   - enacting_legislatures: Primary beneficiary (institutional/mobile) — ordinary enactments gain binding force through the validity channel; the seat conferred authority accrues to
 *   - commercial_and_creditor_interests: Secondary beneficiary (powerful/arbitrage) — purchase predictability; can lobby for enactment when rules disfavor them
 *   - legal_profession: Dual-positioned beneficiary/payer (organized/constrained) — clear standards sustain practice; members absorb the daily cost of declaring unenacted claims legally worthless
 *   - unenacted_rights_claimants: Primary target (powerless/trapped) — justice claims without enactment carry zero legal weight
 *   - politically_excluded_minorities: Primary target (powerless/trapped) — locked out of the coalitions whose products alone bind
 *   - social_movements_seeking_enactment: Excluded actor (organized/constrained) — would weigh unenacted claims now; kept out of the rooms where validity is decided
 *   - general_citizenry: Near-symmetric participant (organized/constrained) — receives predictable rule and pays when enacted law cuts against them
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — maps the validity criterion's operation and its costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__positivist_reading, 0.52).
domain_priors:suppression_score(us_constitution_text__positivist_reading, 0.6).
domain_priors:theater_ratio(us_constitution_text__positivist_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_text__positivist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__positivist_reading, "Positivist Validity Criterion for U.S. Constitutional Law (Source-Validity Reading)").
narrative_ontology:topic_domain(us_constitution_text__positivist_reading, "legal/constitutional/interpretive").

domain_priors:requires_active_enforcement(us_constitution_text__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__positivist_reading, 'c0578318-630b-491a-9e45-e67f66bfc9d1').
narrative_ontology:cs_kernel_codification('c0578318-630b-491a-9e45-e67f66bfc9d1', fixed_text).
narrative_ontology:cs_authority_grounding('c0578318-630b-491a-9e45-e67f66bfc9d1', lineage).
narrative_ontology:cs_interpretation_layer_present('c0578318-630b-491a-9e45-e67f66bfc9d1').
narrative_ontology:cs_reading_relation('c0578318-630b-491a-9e45-e67f66bfc9d1', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c0578318-630b-491a-9e45-e67f66bfc9d1', us_constitution_text__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('c0578318-630b-491a-9e45-e67f66bfc9d1', foundational, enactment_exclusivity_of_legal_validity).
narrative_ontology:cs_axiom_status(enactment_exclusivity_of_legal_validity, holdable).
narrative_ontology:cs_axiom_grounding('c0578318-630b-491a-9e45-e67f66bfc9d1', enactment_exclusivity_of_legal_validity, conventional).
narrative_ontology:cs_axiom('c0578318-630b-491a-9e45-e67f66bfc9d1', secondary, moral_content_legally_inadmissible).
narrative_ontology:cs_axiom_status(moral_content_legally_inadmissible, holdable).
narrative_ontology:cs_axiom_grounding('c0578318-630b-491a-9e45-e67f66bfc9d1', moral_content_legally_inadmissible, conventional).
narrative_ontology:cs_reference_frame('c0578318-630b-491a-9e45-e67f66bfc9d1', formal_enactment_validity_order).
narrative_ontology:cs_drift_state('c0578318-630b-491a-9e45-e67f66bfc9d1', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c0578318-630b-491a-9e45-e67f66bfc9d1', '').
narrative_ontology:cs_kernel_id(us_constitution_text__positivist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, enacting_legislatures).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, article_v_amending_coalitions).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, commercial_and_creditor_interests).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, legal_profession).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, unenacted_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, politically_excluded_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__positivist_reading, general_citizenry).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, legal_profession).
narrative_ontology:constraint_victim(us_constitution_text__positivist_reading, general_citizenry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staffs the courts that decide which norms count as constitutional law. Applies the enactment test: a provision binds because it sits in the ratified text or arrived through a valid amendment, and arguments grounded only in moral appeal or unrecorded intention are treated as legally weightless. Members are selected, confirmed, and promoted inside a profession whose training and case law reward fidelity to enacted sources; setting that fidelity aside means abandoning the role's own self-understanding, not merely changing jobs.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% The temporary supermajorities — two-thirds of both houses of Congress plus three-quarters of the states — that can write new supreme law. When assembled, their product binds everyone without needing a moral defense; the rarity of assembly is the price of admission. Between assemblies they fall back on ordinary legislation, so the capacity to act persists even when the amendment route stalls.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, article_v_amending_coalitions, beneficiary,
    institutional, generational, mobile, national).

% Congress and the state legislatures, whose statutes and resolutions acquire legal force by being enacted. Whatever they pass binds without further justification of its wisdom; opponents must defeat it through the same enactment machinery or persuade a court that it conflicts with superior enacted law. The conferred authority accrues to them continuously, session by session.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, enacting_legislatures, beneficiary,
    institutional, biographical, mobile, national).

% Businesses, lenders, and property holders who plan on the assumption that enacted rules will be applied as written; predictability lowers their cost of contracting and lending. When enacted rules cut against them, they can lobby for new enactments, relocate operations, or shop for friendlier jurisdictions — the validity channel responds to organized influence.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, commercial_and_creditor_interests, beneficiary,
    powerful, biographical, arbitrage, national).

% Lawyers, judges, and teachers who make their living operating the validity test. Clear source-based standards make advice defensible and outcomes explainable. The same standards require members to tell clients that meritorious moral claims without enactment are legally worthless, and to forgo outcome-based argument in their own advocacy and opinions — a professional cost absorbed daily.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, legal_profession, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, legal_profession, payer).

% Everyone subject to the resulting body of law. They receive a legal order they can learn, predict, and hold officials to, and they pay when enacted rules injure them and no unenacted remedy is available. Most have no realistic exit from the jurisdiction's legal order; their protection is participation in the enactment channels or reliance on those who speak for them there.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, general_citizenry, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__positivist_reading, general_citizenry, payer).

% People with serious claims of injustice — unequal treatment, denied subsistence, unprotected liberty — that have not been written into enacted law. Under the validity test their claims carry no legal weight however strong their moral case; the only door is enactment, which requires the political strength whose absence defines their situation. They wait, sometimes for generations, for a coalition strong enough to write their claims into law.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, unenacted_rights_claimants, payer,
    powerless, generational, trapped, national).

% Groups systematically unable to assemble or join the coalitions that produce enacted law — historically disenfranchised populations whose votes, wealth, or representation are insufficient to move Article V or ordinary legislation. For them the validity test converts political weakness into legal invisibility: what cannot be enacted on their behalf does not exist for them, and emigration is the only exit most will never take.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, politically_excluded_minorities, payer,
    powerless, generational, trapped, national).

% Organized campaigns pressing to have their claims enacted — marching, litigating test cases, drafting amendments. Inside courtrooms their moral arguments are inadmissible as sources of law, and inside legislatures they face the same supermajority arithmetic that excluded them; they are present in the streets and absent from the rooms where validity is decided.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, social_movements_seeking_enactment, excluded,
    organized, generational, constrained, national).

% Academics and commentators who map how the validity criterion works, where it came from, and what it costs. They publish critiques and reconstructions, train the next cohort of officials, and supply the vocabulary in which the profession debates its own foundations. They hold no enforcement power and collect no conferred authority; their seat is observational.
narrative_ontology:constraint_stakeholder(us_constitution_text__positivist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__positivist_reading, enacting_legislatures).
narrative_ontology:fixing_cost_class(us_constitution_text__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a shared, publicly checkable test for what counts as constitutional law: a norm binds if and only if it traces to the ratified text or to an amendment enacted through Article V's supermajority procedure. Disputes over authority resolve by inspecting the enactment chain rather than re-litigating first principles, giving legislators, courts, and citizens a common answer to the question 'by what right does this rule govern?'
% TRANSFER_FUNCTION: Moves binding legal authority toward formally enacted instruments and away from moral argument and historical reconstruction: whatever a supermajority enactment produces becomes supreme without further justification, while claims lacking enactment — however widely endorsed as just — carry no legal weight in constitutional adjudication. Decisive voice shifts from claimants to enactors.
% ABSENT_VOICES: People whose substantive justice claims lack enactment would object that the criterion converts their exclusion from political power into permanent legal nullity. They are absent from courtrooms, where their moral arguments are inadmissible as sources of law, and from Article V, where supermajority thresholds place amendment beyond their reach. They appear in this story only as the excluded seat and the trapped payer seats.
% DISAPPEARANCE_RATIONALE: If the enactment-validity criterion vanished overnight, every exercise of constitutional authority would need re-justification from first principles: precedents would lose their transmission chain, legislative products would need fresh moral defense to bind, and the profession would fracture over which successor criterion, if any, governs — the interpretive order would reorganize around some new validity test or fragment along regional and ideological lines.
% FOUNDING_PROBLEM: Making collective commitments binding and knowable without depending on each interpreter's private judgment of their justice: the written-ratification design and, later, formalist jurisprudence sought a validity test that any trained official could apply uniformly, so that law could command allegiance by its pedigree rather than by re-deriving its wisdom each generation.
% FOUNDING_PROBLEM_CORROBORATION: General jurisprudence across traditions attests the underlying problem from outside the benefiting parties: Hart's analysis of the rule of recognition, comparative constitutional scholarship on amendment rigidity, and the reading's sharpest critics — Dworkinian and critical-theory writers, who dispute the answer rather than the problem — all treat 'what makes a norm legally authoritative' as a real and unresolved coordination question. The attestation is not monopolized by the arrangement's beneficiaries.
narrative_ontology:disappearance_verdict(us_constitution_text__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__positivist_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: substantial but bounded — much of the harm the arrangement produces is transmitted enacted content attributable to enactment politics, while the criterion's own distinctive work is blocking non-enacted remedies; that blockage is real and falls hardest on the powerless, but it is disciplined by the documentary ease of checking enactment chains. Suppression 0.60: rival validity criteria are excluded inside legal practice through legal education, precedent discipline, and professional sanction, though they remain live outside it — suppression is structural at the courtroom door, partly internalized in professional formation. Theater 0.35: enactment-checking is real work, but a growing share of 'we merely apply the text' rhetoric performs neutrality over interpretive choices the criterion does not itself determine. Accessibility_collapse 0.60: once the criterion is understood, alternative criteria collapse as sources of legal validity inside the system while surviving as political and academic programs. Resistance 0.58: sustained pushback from movements, theorists, and periodically from courts themselves. The measurement series runs on one shared time grid (every tracked metric authored at all nine points) and shows one full cycle: formalist consolidation (1870-1910, rising extraction and enforcement), realist/New Deal/Warren relaxation (1930-1970, trough in all three series), late-century proceduralist revival (1990-2026). Base_properties reflect the interval-end (2026) state. The oscillation is documented rather than smoothed; whether it is externally driven or intermittently reinforcing is carried as an omega.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats compute different types from the same structure. From unenacted_rights_claimants and politically_excluded_minorities, the criterion operates as enforced denial: their strongest claims are legally weightless by construction, and their exit options are trapped, so effective extraction lands near the full-target end. From enacting_legislatures and article_v_amending_coalitions, the same criterion operates as conferred supremacy: their products bind without moral contest, so the structure reads as subsidy. The federal_judiciary sits between — it administers the test and is fused with it professionally, bearing the discipline it imposes. The engine computes this per-seat divergence from the structural data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to enactment-channel insiders: legislatures and amending coalitions receive bindingness, commercial interests purchase predictability and retain arbitrage-grade influence over the channel, and the profession collects a workable practice — all derive low directionality (subsidy-side). Victim declarations map to channel outsiders: unenacted claimants and politically excluded minorities derive high directionality, amplified by trapped exit and generational time horizons. General citizenry sits near symmetric: genuine predictability received, diffuse payment when enacted law injures. No directionality_overrides are authored: the derivation from beneficiary/victim structure plus exit options captures the relationships, and because overrides key on power atoms, an override tuned for the judiciary would misfire across the three other institutional seats sharing that atom. Spatial scope is national, and verification of the criterion's core question ('was this validly enacted?') is documentary and comparatively cheap, which limits scope amplification relative to diffuse-scope constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both error directions. Calling this a pure-coordination arrangement would erase the systematic denial of unenacted claimants — the same structure that terminates authority disputes also renders the politically weak legally invisible. Calling it pure extraction would erase the coordination function — without a shared validity test, every constitutional question reopens as a contest over first principles and no ruling commands allegiance across disagreement. The hybrid classification holds both facts. On obsolescence: the founding problem (making collective commitments binding and knowable without re-deriving their justice each generation) remains live — every legal system needs a validity criterion — so no resolved-mandatrophy declaration is authored and no sunset clause exists. The drift risk to monitor is decay toward inertial performance: if enactment-checking becomes ritual while real decisions migrate to unacknowledged criteria, the theater_ratio series is the tripwire, and its late-interval elevation (0.34-0.38) is worth watching without yet being dispositive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the positivist reading the correct delimitation of this constraint, or do the sibling readings (originalist, living constitutionalist) carve the same interpretive practice so differently that this story''s epsilon, beneficiaries, and victims misattribute the structure?',
    'Generate the sibling readings as separate stories and compare computed per-seat classifications; divergence in victim sets and epsilon locates where the readings partition the practice differently.',
    'If the living-constitutionalist partition is adopted, the victim set shifts toward those harmed by frozen text and epsilon falls; if the originalist partition is adopted, historical-meaning recovery enters the constraint and the victim set shifts toward those disadvantaged by changed circumstances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the constitutional-text kernel correctly partitions the interpretive practice.').

omega_variable(
    transmission_vs_blockage_attribution,
    'How much of the measured extraction belongs to the validity criterion itself versus to the enacted content it transmits?',
    'Counterfactual comparison across jurisdictions or eras holding enacted content constant while varying the validity criterion, plus doctrinal analysis separating harms that required blocking non-enacted remedies from harms the enacted text alone produced.',
    'If transmission dominates, epsilon falls toward coordination-cost levels and the classification relaxes toward pure coordination; if blockage dominates, extraction concentrates on the criterion''s own work and pressure builds toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_vs_blockage_attribution, conceptual, 'Attributing measured extraction between the validity rule and the content it carries.').

omega_variable(
    descriptive_normative_ambiguity,
    'Is the enactment-validity criterion a descriptive social fact (officials simply do treat enactment as conclusive) or a normative commitment that someone chose and actively maintains?',
    'Trace official discourse: if courts and scholars present the criterion as inevitable fact while sanctioning deviants, it functions normatively under descriptive cover; check whether any organ ever deliberated and adopted it as policy.',
    'If normative, the constraint has identifiable maintainers who could revise it and its suppression is chosen policy; if purely descriptive, there is no author to petition and reform must route through the criterion''s own enactment channels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(descriptive_normative_ambiguity, conceptual, 'Descriptive cover over a maintained normative commitment.').

omega_variable(
    judicial_identity_lock_mechanism,
    'Is the judiciary''s fidelity to source-validity structural (career, confirmation, precedent obligations) or internalized professional identity that would survive removal of the incentives?',
    'Post-exit trajectory: examine judges who move to academic or emeritus seats; if they continue to reason source-validly absent sanction, the lock is internalized; if they immediately reason outcome-validly, it was structural.',
    'If internalized, effective suppression exceeds the structural measure — the criterion travels inside its administrators and outlives its enforcement machinery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_identity_lock_mechanism, empirical, 'Structural versus internalized nature of judicial fidelity to the validity criterion.').

omega_variable(
    cycle_driver_ambiguity,
    'What drives the formalist-consolidation, realist-relaxation, proceduralist-revival cycle visible in the measurement series — external political shocks, or intermittent reinforcement internal to the profession?',
    'Correlate cycle turning points with external events (depression, war, electoral realignment) versus internal professional events (casebook generations, bar-canon shifts); if turns precede external shocks, the oscillation is internally driven.',
    'If internally driven intermittent reinforcement, the oscillation is itself an extraction mechanism — moral reasoning alternately admitted and withdrawn — and the constraint''s effective extraction exceeds any single-phase measurement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cycle_driver_ambiguity, empirical, 'Driver of the observed enforcement oscillation across the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__positivist_reading, 1870, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1870, us_constitution_text__positivist_reading, theater_ratio, 1870, 0.12).
narrative_ontology:measurement(us_c_tr_t1890, us_constitution_text__positivist_reading, theater_ratio, 1890, 0.18).
narrative_ontology:measurement(us_c_tr_t1910, us_constitution_text__positivist_reading, theater_ratio, 1910, 0.24).
narrative_ontology:measurement(us_c_tr_t1930, us_constitution_text__positivist_reading, theater_ratio, 1930, 0.3).
narrative_ontology:measurement(us_c_tr_t1950, us_constitution_text__positivist_reading, theater_ratio, 1950, 0.22).
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_text__positivist_reading, theater_ratio, 1970, 0.26).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_text__positivist_reading, theater_ratio, 1990, 0.34).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_text__positivist_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(us_c_tr_t2026, us_constitution_text__positivist_reading, theater_ratio, 2026, 0.35).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1870, us_constitution_text__positivist_reading, base_extractiveness, 1870, 0.58).
narrative_ontology:measurement(us_c_be_t1890, us_constitution_text__positivist_reading, base_extractiveness, 1890, 0.63).
narrative_ontology:measurement(us_c_be_t1910, us_constitution_text__positivist_reading, base_extractiveness, 1910, 0.6).
narrative_ontology:measurement(us_c_be_t1930, us_constitution_text__positivist_reading, base_extractiveness, 1930, 0.5).
narrative_ontology:measurement(us_c_be_t1950, us_constitution_text__positivist_reading, base_extractiveness, 1950, 0.44).
narrative_ontology:measurement(us_c_be_t1970, us_constitution_text__positivist_reading, base_extractiveness, 1970, 0.46).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_text__positivist_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_text__positivist_reading, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement(us_c_be_t2026, us_constitution_text__positivist_reading, base_extractiveness, 2026, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1870, us_constitution_text__positivist_reading, suppression_requirement, 1870, 0.48).
narrative_ontology:measurement(us_c_su_t1890, us_constitution_text__positivist_reading, suppression_requirement, 1890, 0.58).
narrative_ontology:measurement(us_c_su_t1910, us_constitution_text__positivist_reading, suppression_requirement, 1910, 0.62).
narrative_ontology:measurement(us_c_su_t1930, us_constitution_text__positivist_reading, suppression_requirement, 1930, 0.5).
narrative_ontology:measurement(us_c_su_t1950, us_constitution_text__positivist_reading, suppression_requirement, 1950, 0.38).
narrative_ontology:measurement(us_c_su_t1970, us_constitution_text__positivist_reading, suppression_requirement, 1970, 0.36).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_text__positivist_reading, suppression_requirement, 1990, 0.46).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_text__positivist_reading, suppression_requirement, 2010, 0.56).
narrative_ontology:measurement(us_c_su_t2026, us_constitution_text__positivist_reading, suppression_requirement, 2026, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__positivist_reading, us_constitution_text__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'constitutional interpretation' conflates three structurally distinct constraints that differ on the source of constitutional authority and therefore on epsilon, beneficiaries, and victims. This file instantiates the positivist reading (validity from enactment procedures; victims are unenacted claimants). The originalist reading adds historical-meaning recovery to source-fixation (different epsilon; victims shift toward those disadvantaged by changed circumstances). The living-constitutionalist reading admits social-evolution authority (different extraction profile; victims shift toward those harmed by frozen text). The three are linked as a family via affects_constraints; each is authored separately because measuring one through another's observable changes epsilon, which marks them as different constraints sharing a kernel, not one constraint viewed from angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
