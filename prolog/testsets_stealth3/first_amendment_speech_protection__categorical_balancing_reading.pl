% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__categorical_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__categorical_balancing_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__categorical_balancing_reading
 *   human_readable: First Amendment Speech Protection — Categorical Balancing Reading (Judicial Category-Creation Regime)
 *   domain: constitutional law/political philosophy/speech regulation
 *
 * SUMMARY:
 *   The standing arrangement under contest is the judicial regime in which
 *   the First Amendment's protected/unprotected boundary is constituted
 *   case-by-case: courts weigh the value of specific expression against
 *   asserted harms, and the accumulated decisions harden into named
 *   categories (obscenity, incitement, true threats, and allied exclusions)
 *   that legislatures, prosecutors, and lower courts then apply. This file
 *   instantiates the categorical_balancing_reading of the
 *   first_amendment_speech_protection kernel and authors nothing about the
 *   sibling readings' merits. The epsilon referent is this standing
 *   categorical-balancing arrangement as this reading's own lights assess it:
 *   a regime with a real administrative function (a workable,
 *   precedent-backed map of regulable speech) that nonetheless concentrates
 *   interpretive authority in the judiciary, erodes ex ante predictability
 *   for speakers, and exposes minority expression to categorization out of
 *   protection. The claim/metric gap is deliberate: the regime CLAIMS itself
 *   as faithful administration of an absolute guarantee while the authored
 *   metrics describe moderately extractive, actively enforced operation whose
 *   extraction has accumulated over the interval — the engine measures that
 *   divergence; the claim is not reconciled to the metrics. KEY AGENTS (by
 *   structural relationship): see key_agents; the structural center is a
 *   judiciary that both runs the boundary-drawing machinery and collects its
 *   product.
 *
 * KEY AGENTS:
 *   - - institutional_judiciary: Agenda-setter and principal beneficiary (institutional/identity_locked) — administers the protected/unprotected boundary case-by-case and collects interpretive authority as the standing product
 *   - - incumbent_legislative_majorities: Secondary beneficiary (powerful/mobile) — receives a judicially certified regulatory domain inside the excluded categories
 *   - - political_dissidents_and_radicals: Primary target (powerless/trapped) — expression sits nearest the incitement and threat boundaries; bears prosecution risk when lines are redrawn
 *   - - sexual_expression_communities: Primary target (powerless/trapped) — obscenity classification removes protection entirely, with criminal and financial-exposure consequences
 *   - - prospective_speakers: Diffuse target (moderate/constrained) — ordinary speakers who carry the ex ante uncertainty cost and cannot afford the litigation that would clarify their position
 *   - - civil_liberties_litigators: Observer (organized/analytical) — litigate boundary cases and map the doctrine without setting or collecting from it
 *   - - absolutist_scholars_and_textualist_justices: Excluded (moderate/analytical) — holders of the rival fixed-text reading, present in scholarship and dissents but outside the operative framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, 0.62).
domain_priors:suppression_score(first_amendment_speech_protection__categorical_balancing_reading, 0.52).
domain_priors:theater_ratio(first_amendment_speech_protection__categorical_balancing_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(first_amendment_speech_protection__categorical_balancing_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__categorical_balancing_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__categorical_balancing_reading, "First Amendment Speech Protection — Categorical Balancing Reading (Judicial Category-Creation Regime)").
narrative_ontology:topic_domain(first_amendment_speech_protection__categorical_balancing_reading, "constitutional law/political philosophy/speech regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__categorical_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__categorical_balancing_reading, 'a6794326-b836-4107-a7d9-11eafeb6b92c').
narrative_ontology:cs_kernel_codification('a6794326-b836-4107-a7d9-11eafeb6b92c', fixed_text).
narrative_ontology:cs_authority_grounding('a6794326-b836-4107-a7d9-11eafeb6b92c', lineage).
narrative_ontology:cs_interpretation_layer_present('a6794326-b836-4107-a7d9-11eafeb6b92c').
narrative_ontology:cs_reading_relation('a6794326-b836-4107-a7d9-11eafeb6b92c', first_amendment_speech_protection__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('a6794326-b836-4107-a7d9-11eafeb6b92c', first_amendment_speech_protection__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('a6794326-b836-4107-a7d9-11eafeb6b92c', foundational, judicial_balancing_creates_protected_set).
narrative_ontology:cs_axiom_status(judicial_balancing_creates_protected_set, holdable).
narrative_ontology:cs_axiom_grounding('a6794326-b836-4107-a7d9-11eafeb6b92c', judicial_balancing_creates_protected_set, conventional).
narrative_ontology:cs_axiom('a6794326-b836-4107-a7d9-11eafeb6b92c', foundational, category_exclusion_precedes_case_specific_harm_proof).
narrative_ontology:cs_axiom_status(category_exclusion_precedes_case_specific_harm_proof, holdable).
narrative_ontology:cs_axiom_grounding('a6794326-b836-4107-a7d9-11eafeb6b92c', category_exclusion_precedes_case_specific_harm_proof, empirically_contingent).
narrative_ontology:cs_reference_frame('a6794326-b836-4107-a7d9-11eafeb6b92c', judicial_category_stewardship).
narrative_ontology:cs_drift_state('a6794326-b836-4107-a7d9-11eafeb6b92c', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a6794326-b836-4107-a7d9-11eafeb6b92c', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__categorical_balancing_reading, incumbent_legislative_majorities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, political_dissidents_and_radicals).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, sexual_expression_communities).
narrative_ontology:constraint_victim(first_amendment_speech_protection__categorical_balancing_reading, prospective_speakers).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, judicial_supremacy_doctrine).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, stare_decisis_principle).
narrative_ontology:constraint_vindicates(first_amendment_speech_protection__categorical_balancing_reading, case_by_case_adjudication_method).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Supreme Court and the lower federal courts define, refine, and enforce the boundary between protected and unprotected speech through successive case adjudications; each decision extends or trims the category map that legislatures, prosecutors, and speakers must then live inside. The institution collects the standing product of its own machinery — interpretive authority, the acknowledged power to say what the amendment permits — and its docket, prestige, and self-conception are bound up with remaining the boundary-setter. Leaving would mean renouncing category-creation for a fixed-text or harm-triggered test, dissolving the institution's distinctive function in this domain.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary, beneficiary).

% Receive a judicially certified regulatory domain — obscenity, incitement, true threats, and allied exclusions — inside which their statutes enjoy presumption of validity and survive review that would strike comparable regulation of protected expression. They also face the forbidden zone: the protected categories they may not touch regardless of majority preference. Their net position is favorable and adjustable; they can redirect regulatory attention among the sanctioned categories as politics move.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, incumbent_legislative_majorities, beneficiary,
    powerful, biographical, mobile, national).

% Their expression sits nearest the incitement and true-threat boundaries, and the historical record shows disproportionate enforcement against them whenever the lines are drawn or redrawn — wartime sedition prosecutions, Cold War loyalty cases, modern material-support and threat prosecutions. They cannot relocate their speech outside the jurisdiction, lack the resources to litigate boundary definitions, and bear prosecution risk as the price of speaking at all.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, political_dissidents_and_radicals, payer,
    powerless, biographical, trapped, national).

% Expression classified as obscene loses protection entirely, and the obscenity category is applied with locally variable standards atop federal criminal statutes, so no jurisdiction reliably shelters them. Classification carries direct criminal exposure and cascades into platform exclusion, payment-processing refusal, and financial de-risking. Their exit options are effectively nil: the category follows the expression wherever it is distributed domestically.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, sexual_expression_communities, payer,
    powerless, biographical, trapped, national).

% Ordinary citizens, journalists, protesters, and online posters deciding whether to speak. Because protection is determined case-by-case, it is knowable only retrospectively: the ex ante question 'will this be protected?' has no reliable answer short of litigation they cannot fund. They carry the uncertainty cost directly — self-censorship near the category boundaries — and their numbers make them the broadest bearer of the arrangement's predictability cost.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, prospective_speakers, payer,
    moderate, immediate, constrained, national).

% Public-interest law organizations and first-amendment academics litigate boundary cases, publish doctrine maps, and supply the evidentiary record on enforcement patterns. They neither administer the arrangement nor collect its product; their function is adversarial clarification, and their access runs entirely through the cases the agenda-setter chooses to hear.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, civil_liberties_litigators, observer,
    organized, biographical, analytical, national).

% Holders of the rival fixed-text account: they maintain that the amendment's guarantee is categorical apart from narrow historical exclusions and that judicial creation of the protected/unprotected boundary is itself the deviation. They publish, dissent, and testify, but they stand outside the operative framework that generates binding doctrine — the conversation that defines the categories happens without them, and their objections enter the record only at the margins.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__categorical_balancing_reading, absolutist_scholars_and_textualist_justices, excluded,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(first_amendment_speech_protection__categorical_balancing_reading, institutional_judiciary).
narrative_ontology:fixing_cost_class(first_amendment_speech_protection__categorical_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a workable, precedent-backed map of which speech the state may regulate, so that legislatures draft, prosecutors charge, lower courts decide, and speakers plan without relitigating the amendment's first principles in every case; routes the recurring collision between expression and security, order, and dignity interests through administrable categories rather than ad hoc political judgment.
% TRANSFER_FUNCTION: Moves interpretive authority over the amendment's scope from the text's readers — speakers, legislators, citizens — to the federal judiciary; returns regulatory discretion to legislative majorities within the judicially excluded categories; and moves uncertainty and litigation costs onto speakers, concentrated on those whose expression sits nearest the category boundaries.
% ABSENT_VOICES: Speakers whose expression falls nearest the boundaries — dissidents, sexual-minority publishers, online posters facing threat charges — bear the doctrine's costs but had no seat when the categories were drawn and have none as they are refined. Absolutist scholars hold a complete rival account of the kernel but operate outside binding practice. Citizens and jurors who ultimately judge speech crimes never participate in defining the categories they are asked to apply.
% DISAPPEARANCE_RATIONALE: If the category apparatus vanished overnight, every speech regulation and prosecution would require first-principles adjudication: legislatures would re-code statutes against either near-categorical protection or harm-triggered limits, thousands of convictions under the excluded categories would become doctrinally unmoored, prosecutors would lose their certified safe harbors, and the judiciary would lose its distinctive speech-law function. The arrangements of every named seat depend on the constraint existing in approximately its current form.
% FOUNDING_PROBLEM: Giving an absolute guarantee ('no law' abridging speech) operative content amid World War I sedition prosecutions, obscenity enforcement, and later Cold War loyalty regimes: how to make the text governable without either gutting it or paralyzing all regulation of expression.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians documenting the Espionage Act prosecutions and the doctrinal scramble of 1919–1957 corroborate the founding problem from outside the benefiting parties; prosecuted speakers and defense-case records attest its persistence; comparative constitutional scholarship confirms every rights-guaranteeing polity faces the same administration problem. No source outside the benefiting parties attests that judicial category-creation specifically is the necessary solution — that element rests on the judiciary's own attestation, and the rival readings dispute it.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__categorical_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__categorical_balancing_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__categorical_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(first_amendment_speech_protection__categorical_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__categorical_balancing_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__categorical_balancing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__categorical_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.62: the arrangement delivers a genuine service (administrable doctrine) while the judiciary retains the standing power to define the service's scope, and the costs — predictability erosion, categorization risk, litigation burden — fall on those with the least capacity to contest them. Suppression is 0.52: the constraint operates through stare decisis discipline, certiorari gatekeeping, and deference norms that foreclose rival readings inside judicial practice, plus a chilling gradient on speakers near boundaries; it coerces interpretation more than expression. Theater is 0.38 and rising: much modern balancing rhetoric performs open-minded weighing while delivering category outcomes reached by other means, though the categories themselves are applied genuinely. Accessibility collapse is 0.40: alternatives persist — absolutist scholarship, state constitutional protections, comparative harm-trigger regimes — so understanding the constraint does not eliminate exits from it. Resistance is 0.50: sustained textualist and absolutist opposition (historic dissents, academic first-amendment absolutism, periodic calls to abandon balancing) that has never displaced the framework. The measurement series run on ONE shared grid of nine points (interval units are years, t0=1945 to tn=2025): base_extractiveness climbs monotonically as categories ossify into interpretive property; theater_ratio climbs as balancing rhetoric detaches from outcome determination; suppression_requirement rises steeply through the mid-century hardening of the category apparatus (Roth through Miller) and plateaus once the enforcement machinery — precedent hierarchy, gatekeeping, deference norms — reaches maturity. All three metrics are authored at every shared time point; no metric borrows another's grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the agenda-setter seat, the arrangement is stewardship: the judiciary experiences category-drawing as the faithful administration of an absolute text against hard cases, and its identity is fused with that role. From the payer seats, the same structure is exposure: dissidents and sexual-expression communities experience the categories as the mechanism by which their expression loses protection, and prospective speakers experience the method as retrospective certainty — protection knowable only after litigation they cannot fund. Incumbent legislative majorities occupy a dual position the derivation must register: beneficiaries of a certified regulatory domain, yet excluded from the larger protected zone. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is declared beneficiary and holds the agenda-setter role with identity-locked exit: its directionality sits nearest the beneficiary pole, and its identity lock amplifies persistence independent of function. Incumbent legislative majorities are secondary beneficiaries — the excluded categories are precisely the space where their statutes enjoy presumption of validity — but they also bear the forbidden-zone cost, placing them modestly off the beneficiary pole. Political dissidents, sexual-expression communities, and prospective speakers are declared victims with weak or absent exit: dissidents and sexual-expression communities sit near the full-target pole (their expression is the direct object of category enforcement, and no jurisdiction reliably shelters them); prospective speakers bear a diffuse version of the same extraction (uncertainty rather than prosecution), placing them high but not maximal. Civil-liberties litigators and absolutist scholars are observer/excluded seats: they neither collect nor pay, and their exclusion from binding practice is itself part of the arrangement's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — giving an absolute text operative content amid sedition, obscenity, and loyalty prosecutions — remains live, so no mandatrophy is declared and none should be inferred from the rising theater ratio alone. The classification work this story performs is bidirectional mislabel-resistance. Against the judiciary's self-description (pure coordination: neutral administration protecting liberty), the declared victims and the accumulating extraction show the coordination carries asymmetric payment. Against the absolutist caricature (pure usurpation: courts seizing the text), the genuine coordination function — no speech regime avoids boundary-drawing, and precedent-backed categories solve a real collective-action problem for legislatures, prosecutors, and speakers — blocks a snare verdict. Tangled rope is the structurally honest claim. The trajectory data flag the forward risk: if balancing rhetoric continues detaching from outcome determination while the category map persists, the arrangement drifts toward theatrical maintenance of an atrophied method — the omega balancing_rhetoric_vestigiality tracks exactly that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (categorical_balancing_reading) of the first_amendment_speech_protection kernel; would instantiating the absolutist_reading or harm_limited_reading instead yield a different epsilon, beneficiary set, and type for the same constitutional text?',
    'Generate the sibling stories as separate constraints and compare computed classifications; divergence in epsilon and victim sets locates the disagreement in what constitutes the protected set (text-fixed vs judicially-created vs harm-triggered).',
    'If the siblings classify differently (expected), the colloquial label ''First Amendment speech protection'' is ambiguous across readings and corpus consumers must never pool metrics across the family; this story''s epsilon is a property of this reading, not of the amendment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification over a shared kernel; committer structure routed here per the kernel-authoring rules.').

omega_variable(
    category_harm_tracking,
    'Do the judicially maintained excluded categories (obscenity, incitement, true threats) track demonstrable harms, or do they sweep in expression whose harm is speculative — is the category boundary doing harm-prevention work or interpretive-territory work?',
    'Systematic audit of prosecutions and regulations under the excluded categories against evidence of concrete harm, comparing enforcement patterns for minority-speaker versus majority-speaker expression.',
    'If the categories over-sweep, effective extraction on the minority payer seats rises above the authored epsilon and the classification trends toward snare; if they track harms tightly, more of the measured extraction is coordination price.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_harm_tracking, empirical, 'Whether the excluded categories are harm-tracking or territory-marking.').

omega_variable(
    predictability_cost_attribution,
    'Is the unpredictability speakers bear intrinsic to any protected/regulable boundary (a coordination cost any regime must pay), or is it amplified specifically by the case-by-case method (an extractive institutional choice)?',
    'Compare ex ante clarity under counterfactual regimes: historical-exclusion tests as administered in state jurisdictions with stronger textual protections, and harm-trigger tests in comparative constitutional systems.',
    'If most unpredictability is method-specific, the balancing method itself is the extraction mechanism and removal is cheaper than the judiciary claims; if intrinsic, fixing_cost stays prohibitive and measured extraction sits nearer the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predictability_cost_attribution, conceptual, 'Attribution of the predictability cost between inherent boundary-drawing and the chosen method.').

omega_variable(
    chilling_effect_magnitude,
    'How large is the ex ante chilling effect on prospective speakers near category boundaries, and is it structural (prosecution and litigation risk) or internalized (self-censorship habits that persist absent active enforcement)?',
    'Behavioral and survey data on self-censorship correlated with proximity to category boundaries; natural experiments from decisions that narrowed or widened the excluded categories.',
    'If largely internalized, suppression persists after doctrinal liberalization and the scalar suppression understates steady-state suppression; if structural, doctrinal change rapidly lowers it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_magnitude, empirical, 'Structural versus internalized suppression mechanism for the speaker-side costs.').

omega_variable(
    balancing_rhetoric_vestigiality,
    'Is the case-by-case balancing rhetoric still outcome-determinative, or has it become theatrical cover for category outcomes reached by other means (historical tradition, institutional deference, intuitive severity judgments)?',
    'Code modern speech-clause opinions for whether the balancing analysis ever plausibly could have flipped the outcome, comparing opinion rhetoric against vote patterns and concurrence behavior.',
    'If mostly vestigial, theater_ratio is understated and the constraint is drifting toward inertial rhetorical maintenance of an atrophied method; if determinative, the authored theater_ratio stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_rhetoric_vestigiality, empirical, 'Whether the balancing method remains functional or is increasingly performed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__categorical_balancing_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t0, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(firs_tr_t0, observed).
narrative_ontology:measurement(firs_tr_t10, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(firs_tr_t10, observed).
narrative_ontology:measurement(firs_tr_t20, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(firs_tr_t20, observed).
narrative_ontology:measurement(firs_tr_t30, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement_basis(firs_tr_t30, observed).
narrative_ontology:measurement(firs_tr_t40, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(firs_tr_t40, observed).
narrative_ontology:measurement(firs_tr_t50, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement_basis(firs_tr_t50, observed).
narrative_ontology:measurement(firs_tr_t60, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement_basis(firs_tr_t60, observed).
narrative_ontology:measurement(firs_tr_t70, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 70, 0.36).
narrative_ontology:measurement_basis(firs_tr_t70, observed).
narrative_ontology:measurement(firs_tr_t80, first_amendment_speech_protection__categorical_balancing_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement_basis(firs_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(firs_be_t0, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(firs_be_t0, observed).
narrative_ontology:measurement(firs_be_t10, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(firs_be_t10, observed).
narrative_ontology:measurement(firs_be_t20, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement_basis(firs_be_t20, observed).
narrative_ontology:measurement(firs_be_t30, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement_basis(firs_be_t30, observed).
narrative_ontology:measurement(firs_be_t40, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement_basis(firs_be_t40, observed).
narrative_ontology:measurement(firs_be_t50, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement_basis(firs_be_t50, observed).
narrative_ontology:measurement(firs_be_t60, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 60, 0.59).
narrative_ontology:measurement_basis(firs_be_t60, observed).
narrative_ontology:measurement(firs_be_t70, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 70, 0.61).
narrative_ontology:measurement_basis(firs_be_t70, observed).
narrative_ontology:measurement(firs_be_t80, first_amendment_speech_protection__categorical_balancing_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement_basis(firs_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t0, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(firs_su_t0, observed).
narrative_ontology:measurement(firs_su_t10, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement_basis(firs_su_t10, observed).
narrative_ontology:measurement(firs_su_t20, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 20, 0.37).
narrative_ontology:measurement_basis(firs_su_t20, observed).
narrative_ontology:measurement(firs_su_t30, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement_basis(firs_su_t30, observed).
narrative_ontology:measurement(firs_su_t40, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 40, 0.47).
narrative_ontology:measurement_basis(firs_su_t40, observed).
narrative_ontology:measurement(firs_su_t50, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 50, 0.5).
narrative_ontology:measurement_basis(firs_su_t50, observed).
narrative_ontology:measurement(firs_su_t60, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 60, 0.51).
narrative_ontology:measurement_basis(firs_su_t60, observed).
narrative_ontology:measurement(firs_su_t70, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 70, 0.52).
narrative_ontology:measurement_basis(firs_su_t70, observed).
narrative_ontology:measurement(firs_su_t80, first_amendment_speech_protection__categorical_balancing_reading, suppression_requirement, 80, 0.52).
narrative_ontology:measurement_basis(firs_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__categorical_balancing_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__categorical_balancing_reading, first_amendment_speech_protection__harm_limited_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'First Amendment speech protection.' The label conflates three structurally distinct claims about what CONSTITUTES the protected set: (1) absolutist_reading — the set is fixed by text and historical exclusion; (2) categorical_balancing_reading (this file) — the set is constituted by judicial weighing into named categories; (3) harm_limited_reading — the set yields only to case-specifically demonstrated unconsented harm. Each claim has its own epsilon, its own beneficiary/victim structure, and its own failure modes, so each is a separate story linked through network edges rather than one story with a measurement parameter. This reading is the operative judicial regime and therefore shapes the operating environment of both siblings: it sets the legitimacy conditions under which absolutist proposals are heard (as displacement of settled practice) and harm-limited proposals are absorbed (as refinements inside categories).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
