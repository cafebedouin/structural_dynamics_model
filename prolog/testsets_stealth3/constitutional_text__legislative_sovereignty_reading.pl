% ============================================================================
% CONSTRAINT STORY: constitutional_text__legislative_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__legislative_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__legislative_sovereignty_reading
 *   human_readable: Legislative Sovereignty with Advisory Judicial Review
 *   domain: constitutional/political/legal
 *
 * SUMMARY:
 *   The constitutional text vests final authority over its own meaning in the
 *   elected chamber. Courts hear challenges and publish reasons, but their
 *   determinations arrive as advice; the legislature responds with a
 *   notwithstanding resolution or by simply re-enacting the disputed
 *   provision, and its choice stands. The arrangement is presented as the
 *   democratic completion of constitutionalism: contested values are settled
 *   by those answerable at the ballot box. Its operation carries a standing
 *   asymmetry — the security of rights depends on the continuing forbearance
 *   of whichever coalition holds the chamber, and minorities hold no
 *   institutional recourse once forbearance ends. This file instantiates ONE
 *   reading of the constitutional_text kernel, the
 *   legislative_sovereignty_reading, as a clean epsilon-invariant constraint;
 *   the sibling readings (judicial_supremacy_reading,
 *   popular_sovereignty_reading) instantiate different constraints in their
 *   own files. Epsilon's referent is the standing legislature-final
 *   arrangement itself, assessed by this reading's own lights — not the
 *   arrangement any sibling would install. The claim (tangled_rope) and the
 *   metrics are authored independently: the claim states what I believe the
 *   structure is; the metrics state what I believe its operation
 *   descriptively is.
 *
 * KEY AGENTS:
 *   - - parliamentary_majority: Agenda-setter and principal collector ([institutional]/[arbitrage]) — writes, invokes, and profits from the override; its exit is rewriting the rule itself
 *   - - majoritarian_electorate: Beneficiary ([organized]/[mobile]) — holds final say through its representatives; exits via the ballot
 *   - - minority_rights_holders: Primary target ([moderate]/[constrained]) — protection is conditional on majority forbearance; no binding backstop
 *   - - constitutional_apex_judiciary: Demoted authority ([institutional]/[identity_locked]) — advises, never concludes; professionally fused with the advisory role
 *   - - subnational_legislatures: Parallel collectors ([institutional]/[arbitrage]) — same override power at regional level, costs shifted to their residents
 *   - - international_human_rights_bodies: Excluded critic ([organized]/[constrained]) — objects from outside the conversation with no domestic channel
 *   - - comparative_constitutional_scholars: Analytical observer — sees the full structure across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, 0.52).
domain_priors:suppression_score(constitutional_text__legislative_sovereignty_reading, 0.62).
domain_priors:theater_ratio(constitutional_text__legislative_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(constitutional_text__legislative_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__legislative_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__legislative_sovereignty_reading, "Legislative Sovereignty with Advisory Judicial Review").
narrative_ontology:topic_domain(constitutional_text__legislative_sovereignty_reading, "constitutional/political/legal").

domain_priors:requires_active_enforcement(constitutional_text__legislative_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__legislative_sovereignty_reading, 'd74c990f-77be-4978-afa1-0ae926d58445').
narrative_ontology:cs_kernel_codification('d74c990f-77be-4978-afa1-0ae926d58445', fixed_text).
narrative_ontology:cs_authority_grounding('d74c990f-77be-4978-afa1-0ae926d58445', lineage).
narrative_ontology:cs_interpretation_layer_present('d74c990f-77be-4978-afa1-0ae926d58445').
narrative_ontology:cs_reading_relation('d74c990f-77be-4978-afa1-0ae926d58445', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('d74c990f-77be-4978-afa1-0ae926d58445', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('d74c990f-77be-4978-afa1-0ae926d58445', foundational, elected_representatives_decide_fundamental_values).
narrative_ontology:cs_axiom_status(elected_representatives_decide_fundamental_values, holdable).
narrative_ontology:cs_axiom_grounding('d74c990f-77be-4978-afa1-0ae926d58445', elected_representatives_decide_fundamental_values, conventional).
narrative_ontology:cs_axiom('d74c990f-77be-4978-afa1-0ae926d58445', secondary, advisory_review_disciplines_without_binding).
narrative_ontology:cs_axiom_status(advisory_review_disciplines_without_binding, holdable).
narrative_ontology:cs_axiom_grounding('d74c990f-77be-4978-afa1-0ae926d58445', advisory_review_disciplines_without_binding, instrumental).
narrative_ontology:cs_reference_frame('d74c990f-77be-4978-afa1-0ae926d58445', westminster_parliamentary_omnicompetence).
narrative_ontology:cs_drift_state('d74c990f-77be-4978-afa1-0ae926d58445', contemporary_entrenched_rights_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('d74c990f-77be-4978-afa1-0ae926d58445', '').
narrative_ontology:cs_kernel_id(constitutional_text__legislative_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, parliamentary_majority).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, majoritarian_electorate).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, subnational_legislatures).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, minority_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text__legislative_sovereignty_reading, constitutional_apex_judiciary).
narrative_ontology:constraint_victim(constitutional_text__legislative_sovereignty_reading, constitutional_apex_judiciary).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__legislative_sovereignty_reading, counter_majoritarian_objection_to_judicial_review).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the chamber and sets the terms under which judicial determinations are received: it may adopt a notwithstanding resolution sheltering a statute for a renewable term, or simply re-enact a provision the court has criticized, and its choice stands. Collects unrestricted discretion over the disputed policy whenever it invokes the override. Its cost of overriding is ordinary electoral accountability, administered through the same majority that already holds the chamber.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, parliamentary_majority, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, parliamentary_majority, beneficiary).

% Governs contested constitutional questions through its elected representatives and receives assurance that its settled choices cannot be displaced by judges it did not choose. Bears a diffuse, usually unnoticed exposure: any right this majority relies upon remains available to a future majority to withdraw by the same procedure.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, majoritarian_electorate, beneficiary,
    organized, generational, mobile, national).

% Pursue recognition of their claims through litigation and public argument; receive reasoned judicial judgments that carry moral and political weight but conclude nothing. When a majority invokes the override or re-enacts over a declaration, their protection lapses for the sheltered term and they resume persuasion election by election, with no forum whose output binds the other side.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, minority_rights_holders, payer,
    moderate, generational, constrained, national).

% Hears constitutional challenges and publishes the authoritative-styled reasons that establish the text's meaning for public debate; transmits its conclusions to the legislature as advice. Retains agenda-setting influence because governments hesitate to be seen dismissing its reasons outright, but its determinations lapse wherever the chamber differs. The court's standing and its members' careers are invested in performing the advisory function well enough to be heeded.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, constitutional_apex_judiciary, payer,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__legislative_sovereignty_reading, constitutional_apex_judiciary, beneficiary).

% Hold a parallel override power within their jurisdiction and use it to pursue programs a national court has criticized, sometimes invoking it preemptively and blanket across entire statutes. Receive the same discretionary shelter as the national majority; their residents carry the corresponding exposure to unreviewable local policy.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, subnational_legislatures, beneficiary,
    institutional, biographical, arbitrage, regional).

% Review the state's treaty compliance and publish criticisms of override practice; hold no domestic enforcement channel and no seat in the constitutional conversation where overrides are decided. Their findings register only as diplomatic correspondence and reputational cost.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, international_human_rights_bodies, excluded,
    organized, generational, constrained, global).

% Track override usage, legislative responses to adverse declarations, and cross-jurisdictional variation in how advisory review disciplines legislatures; supply the vocabulary in which the arrangement's defenders and critics conduct their argument.
narrative_ontology:constraint_stakeholder(constitutional_text__legislative_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__legislative_sovereignty_reading, parliamentary_majority).
narrative_ontology:fixing_cost_class(constitutional_text__legislative_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Places conclusive settlement of contested constitutional questions with the electorally accountable branch: political conflict over rights-versus-majority tradeoffs is coordinated through elections, deliberation, and legislative judgment instead of through litigation whose outcome no voter can reverse. Prevents any coalition from locking its preferred interpretation beyond democratic reach.
% TRANSFER_FUNCTION: Moves final interpretive authority, and the policy discretion riding on it, from the apex court to the sitting legislative majority whenever the two diverge; correspondingly moves the security of rights protections from protected classes to the discretionary forbearance of current majorities.
% ABSENT_VOICES: International human rights bodies sit entirely outside the conversation: they would argue treaty obligations require enforceable domestic protection but hold no seat where override decisions are made. Inside the conversation, minority litigants are heard fully at the advisory stage, but their objections terminate there — no participant speaks with a vote that survives an adverse majority, and constituencies with no electoral weight have no channel whose output binds.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — finality passing to the courts — every outstanding override would lapse, past overrides would become immediately reviewable, and legislation would queue behind anticipated rulings. Cabinet business, provincial programs shielded by prior invocations, and the partisan balance built on override availability would all reorganize around the new conclusive seat within a single legislative session. Visible arrangements depend on it.
% FOUNDING_PROBLEM: Reconcile rights-protective review with democratic accountability and the Westminster inheritance: the founders needed minority-protective adjudication acceptable to majorities that refused unelected final say, so the text granted review while reserving a conclusive override to the elected chamber.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: comparative constitutional scholarship, among both critics and defenders of judicial review, documents the counter-majoritarian difficulty as a live cross-jurisdictional problem; senior judges in jurisdictions with the rival reading publicly concede the democratic objection even while defending their own finality; minority-rights organizations attest the problem is real while denying it justifies leaving them without a binding backstop. No serious party disputes that the founding problem existed; the dispute is over whether this remedy answers it justly.
narrative_ontology:disappearance_verdict(constitutional_text__legislative_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__legislative_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__legislative_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__legislative_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__legislative_sovereignty_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__legislative_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__legislative_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__legislative_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.52 at interval end) because the transfer it authorizes — conclusive policy discretion over rights objections — is real but episodic: most of the time the mechanism sleeps and courts' advice shapes outcomes voluntarily. Suppression (0.62) exceeds extractiveness because the constraint's persistence requires actively maintaining the advisory ceiling: courts must be structurally prevented from converting advice into conclusion, and the override must remain exercisable regardless of content. Theater ratio (0.30) reflects the advisory ritual — judgments styled as authoritative that bind no one — alongside a genuinely functional deliberative channel. Accessibility_collapse (0.50): alternatives exist (formal amendment entrenching judicial review, treaty escalation, federal restructuring) but every path runs through the assent or acquiescence of the very majority the arrangement empowers, so the option set narrows, without vanishing, to persuasion inside majority-controlled channels. Resistance (0.60) is chronic — scholarly critique, bar-association opposition, minority-organization campaigning, periodic electoral fights over invocation — and never structurally effective, because the beneficiary holds the pen. TEMPORAL CYCLE: the series shows three phases on one shared grid — founding-era heavy invocation with high salience (t0–t4, extraction peaks early), a dormancy phase in which a taboo convention suppresses invocation and enforcement needs decay (trough at t8–t12), and a revival phase of normalized, sometimes preemptive blanket invocation in which extraction climbs past the founding peak (t20–t30) while political cost per invocation falls. The oscillation tracks political attention cycles rather than engineered intermittency, but the dormancy phase functions to reset resistance: each quiet stretch lowers perceived stakes until the next invocation lands against a softened opposition — adjacent to intermittent reinforcement, though not designed as it. Scalar base_properties reflect the end state (revival plateau, t=30). The suppression_requirement series is authored deliberately: the story specifically traces enforcement-capacity change (active defense of the mechanism early, convention-carried dormancy, hardened proactive shielding late), which a static scalar cannot capture.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the agenda-setter/beneficiary seats the arrangement is self-government: final say resting with the accountable branch is liberty, and the override is democracy working as advertised. From the payer seat the same structure is the permanent conditionality of protection — every right held is held at the pleasure of the next coalition. From the judiciary's seat it is professional subordination with residual voice: the work of judging continues, but its product was demoted from conclusion to input. The engine derives these per-seat classifications from the power/exit asymmetries (arbitrage versus constrained versus identity_locked) and the declared beneficiary/victim positions; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (parliamentary_majority, majoritarian_electorate, subnational_legislatures) derive low directionality — subsidized or negatively-charged effective extraction — matching seats that collect discretion and face no countervailing forum. The victim declaration (minority_rights_holders) with constrained exit drives that seat toward the full-target end: trapped in the persuasion channel, its effective extraction amplifies. The judiciary is seated as a payer through its stakeholder role (interpretive authority flows from it to the chamber) while retaining advisory income, placing it between the poles; its identity_locked exit — the court cannot abandon its guardianship vocation without ceasing to be a court — holds it nearer the target end than its partial benefit alone would suggest. National scope modestly amplifies verification difficulty, as scoped in the engine. Suppression is authored raw at 0.62 and left unscaled: it is a structural property of the arrangement (the advisory ceiling plus the always-open override), not an extraction quantity, and only extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling rights-protective review with democratic accountability — is live, not dead: the counter-majoritarian difficulty recurs in every jurisdiction where courts invalidate legislation, and the arrangement's mandate has not outlived its function, so mandatrophy_resolved is not declared. The tangled_rope classification is what keeps both faces visible. Reading the arrangement as pure extraction (snare) erases its genuine coordination achievement — accountable finality, no coalition locked out of self-government — which is real and load-bearing. Reading it as pure coordination (rope) erases the standing exposure of minorities, who finance the majority's discretion with their own security. The temporal series supplies the drift watchlist: a migration toward snare would announce itself as routine preemptive blanket invocation, collapsing resistance, and rising theater — the signature of a coordination story becoming cover. The current profile shows early movement along that vector (theater 0.15 to 0.30 across the interval) without crossing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_final_authority_locus,
    'This constraint is the legislative_sovereignty_reading of the constitutional_text kernel. What changes structurally if a sibling reading displaces it?',
    'Observe jurisdictional switches between readings (adoption of entrenched judicial review, or of constituent-power mechanisms such as referendum entrenchment and citizens'' assemblies) and measure which seats gain or lose protection and how rigidity changes at each switch.',
    'Under the judicial_supremacy_reading the beneficiary/victim sets invert — majorities become the exposed seat and minorities the shielded one — and rigidity rises sharply. Under the popular_sovereignty_reading institutional finality dissolves entirely and the arrangement becomes a rotating trusteeship answerable to amendment and convention. This file''s epsilon, beneficiary set, and victim set are valid only for the legislative reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_final_authority_locus, conceptual, 'Committer structure: the locus of final interpretive authority determines which seat is exposed.').

omega_variable(
    constructed_vs_inherent_authority,
    'Is legislative finality an inherent requirement of accountable representative government, or a constructed allocation maintained because it protects incumbent discretion?',
    'Compare long-run democratic stability and rights outcomes across Westminster override systems and entrenched-review systems; test whether accountable government survives conclusive judicial review without the majoritarian breakdown the arrangement''s defenders predict.',
    'If inherent, part of the measured extraction is the irreducible price of democratic accountability and should not be counted against the arrangement. If constructed, the arrangement is a maintained advantage of officeholders, its naturality framing is cover, and false-summit-style scrutiny of the beneficiary structure applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_inherent_authority, conceptual, 'Whether majoritarian finality is natural law or maintained construction.').

omega_variable(
    override_chilling_effect,
    'Does the mere availability of the override suppress rights assertion by minorities even during periods when it is never invoked?',
    'Difference-in-differences on rights-claim filings, litigation rates, and outcomes across jurisdictions before and after adopting or abolishing override clauses, controlling for case mix.',
    'A measurable chilling effect raises true extractiveness above the invocation-based measure and reframes the dormancy-phase dip in the temporal series as suppressed demand rather than reduced harm — meaning the trough understates the constraint''s standing cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_chilling_effect, empirical, 'Latent extraction via chilling, invisible to invocation counting.').

omega_variable(
    advisory_soft_entrenchment_drift,
    'Are advisory declarations converging on de facto bindingness through legislative deference conventions — soft entrenchment that migrates operative practice toward the judicial sibling without any formal change?',
    'Track legislative response rates and amendment follow-through on adverse declarations over time; code whether governments engage the court''s reasons or dismiss them wholesale.',
    'High sustained compliance indicates the operative constraint has drifted toward the judicial sibling''s structure; this file''s classification would then lag reality and should be split or superseded, with the beneficiary/victim sets migrating accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisory_soft_entrenchment_drift, empirical, 'Soft-entrenchment drift of advisory review toward conclusive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__legislative_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__legislative_sovereignty_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t4, constitutional_text__legislative_sovereignty_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(cons_tr_t4, observed).
narrative_ontology:measurement(cons_tr_t8, constitutional_text__legislative_sovereignty_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement_basis(cons_tr_t8, observed).
narrative_ontology:measurement(cons_tr_t12, constitutional_text__legislative_sovereignty_reading, theater_ratio, 12, 0.15).
narrative_ontology:measurement_basis(cons_tr_t12, observed).
narrative_ontology:measurement(cons_tr_t16, constitutional_text__legislative_sovereignty_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement_basis(cons_tr_t16, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__legislative_sovereignty_reading, theater_ratio, 20, 0.23).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t24, constitutional_text__legislative_sovereignty_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(cons_tr_t24, observed).
narrative_ontology:measurement(cons_tr_t28, constitutional_text__legislative_sovereignty_reading, theater_ratio, 28, 0.29).
narrative_ontology:measurement_basis(cons_tr_t28, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__legislative_sovereignty_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(cons_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t4, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 4, 0.47).
narrative_ontology:measurement_basis(cons_be_t4, observed).
narrative_ontology:measurement(cons_be_t8, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement_basis(cons_be_t8, observed).
narrative_ontology:measurement(cons_be_t12, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement_basis(cons_be_t12, observed).
narrative_ontology:measurement(cons_be_t16, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement_basis(cons_be_t16, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 20, 0.43).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t24, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 24, 0.49).
narrative_ontology:measurement_basis(cons_be_t24, observed).
narrative_ontology:measurement(cons_be_t28, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 28, 0.51).
narrative_ontology:measurement_basis(cons_be_t28, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_text__legislative_sovereignty_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(cons_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t4, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement_basis(cons_su_t4, observed).
narrative_ontology:measurement(cons_su_t8, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement_basis(cons_su_t8, observed).
narrative_ontology:measurement(cons_su_t12, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement_basis(cons_su_t12, observed).
narrative_ontology:measurement(cons_su_t16, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement_basis(cons_su_t16, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t24, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement_basis(cons_su_t24, observed).
narrative_ontology:measurement(cons_su_t28, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 28, 0.6).
narrative_ontology:measurement_basis(cons_su_t28, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_text__legislative_sovereignty_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(cons_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__legislative_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__legislative_sovereignty_reading, popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constitutional_text is a contested kernel decomposing into three reading-stories linked by network edges: judicial_supremacy_reading, legislative_sovereignty_reading (this file), and popular_sovereignty_reading. The decomposition follows the epsilon-invariance principle: the colloquial label 'who decides what the constitution means' conflates structurally distinct arrangements with distinct beneficiary/victim sets and distinct epsilon values — the judicial reading exposes majorities to entrenched review, this reading exposes minorities to override, the popular reading exposes both branches to constituent revision. The fixed text is upstream of all three; each reading story links its siblings here, and each sibling file reciprocates. This reading forecloses the judicial sibling within any single framework (the conclusive-authority slot admits one holder) and coexists with the popular sibling (parliamentary supremacy is routinely grounded as the demos' proximate exercise).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
