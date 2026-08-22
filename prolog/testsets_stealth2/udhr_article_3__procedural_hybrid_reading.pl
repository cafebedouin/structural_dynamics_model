% ============================================================================
% CONSTRAINT STORY: udhr_article_3__procedural_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__procedural_hybrid_reading, []).

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
 *   constraint_id: udhr_article_3__procedural_hybrid_reading
 *   human_readable: UDHR Article 3 — Procedural Hybrid Reading (Due Process Floor)
 *   domain: legal/political/philosophical
 *
 * SUMMARY:
 *   This story instantiates the procedural hybrid reading of UDHR Article 3:
 *   the standing arrangement under contest is the international human-rights
 *   regime's treatment of Article 3 as a due-process floor — habeas-style
 *   review availability for persons in custody, a categorical torture
 *   prohibition, and judicial review of detention — layered over, and
 *   deliberately silent on, the unresolved question of whether 'security of
 *   person' means freedom from state violence or provision of material
 *   conditions. The arrangement is administered by treaty bodies and regional
 *   courts, financed and populated by state participation, and consumed by
 *   detained persons whose access to its protections varies enormously by
 *   jurisdiction. Its characteristic failure mode, visible in its own terms,
 *   is the decoupling of formal participation from actual delivery:
 *   membership prices reputational credit identically for scrupulous and
 *   violating states, so the arrangement's largest steady return flows to
 *   participants who honor the letter and breach the substance. Claimed type
 *   and metrics are authored independently: the claim is tangled_rope (a
 *   genuine coordination floor plus asymmetric extraction through the same
 *   machinery, actively enforced); the metrics describe moderate extraction
 *   with a heavy performative component. Family links to the two sibling
 *   readings are declared in network.affects_constraints. KEY AGENTS (by
 *   structural relationship): - habeas_corpus_claimants: primary beneficiary
 *   (powerless/trapped) - administratively_detained_persons: primary target
 *   (powerless/trapped) - state_executive_branches: principal powerful payer
 *   (institutional/arbitrage) - non_compliant_state_parties: silent collector
 *   (institutional/arbitrage) - regional_human_rights_courts: administering
 *   beneficiary (institutional/identity_locked) - human_rights_treaty_bodies:
 *   administering beneficiary (organized/identity_locked) -
 *   compliant_state_parties: dual-positioned participant
 *   (institutional/constrained) - civil_society_litigators: derivative
 *   beneficiary (organized/mobile) - human_rights_scholars: analytical
 *   observer (analytical/analytical)
 *
 * KEY AGENTS:
 *   - habeas_corpus_claimants: primary beneficiary (powerless/trapped) — obtain contestable review of custody where courts function
 *   - administratively_detained_persons: primary target (powerless/trapped) — bear the promise-delivery gap in deferred or rubber-stamped review
 *   - state_executive_branches: principal powerful payer (institutional/arbitrage) — surrender detention discretion, retain emergency dilution levers
 *   - non_compliant_state_parties: silent collector (institutional/arbitrage) — convert formal membership into reputational rent
 *   - regional_human_rights_courts: administering beneficiary (institutional/identity_locked) — binding jurisdiction, politically mediated enforcement
 *   - human_rights_treaty_bodies: administering beneficiary (organized/identity_locked) — agenda-setting without compulsion
 *   - compliant_state_parties: dual-positioned participant (institutional/constrained) — pays compliance, collects mutual assurance
 *   - civil_society_litigators: derivative beneficiary (organized/mobile) — docket depends on reviewable claims
 *   - human_rights_scholars: analytical observer (analytical/analytical) — maps the structure, bears none of its costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__procedural_hybrid_reading, 0.46).
domain_priors:suppression_score(udhr_article_3__procedural_hybrid_reading, 0.38).
domain_priors:theater_ratio(udhr_article_3__procedural_hybrid_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, extractiveness, 0.46).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(udhr_article_3__procedural_hybrid_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__procedural_hybrid_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__procedural_hybrid_reading, "UDHR Article 3 — Procedural Hybrid Reading (Due Process Floor)").
narrative_ontology:topic_domain(udhr_article_3__procedural_hybrid_reading, "legal/political/philosophical").

domain_priors:requires_active_enforcement(udhr_article_3__procedural_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__procedural_hybrid_reading, '80fc5c34-3c7f-4a03-a085-8aedf878c800').
narrative_ontology:cs_kernel_codification('80fc5c34-3c7f-4a03-a085-8aedf878c800', fixed_text).
narrative_ontology:cs_authority_grounding('80fc5c34-3c7f-4a03-a085-8aedf878c800', lineage).
narrative_ontology:cs_interpretation_layer_present('80fc5c34-3c7f-4a03-a085-8aedf878c800').
narrative_ontology:cs_reading_relation('80fc5c34-3c7f-4a03-a085-8aedf878c800', udhr_article_3__negative_liberty_reading, coexists_with).
narrative_ontology:cs_reading_relation('80fc5c34-3c7f-4a03-a085-8aedf878c800', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_axiom('80fc5c34-3c7f-4a03-a085-8aedf878c800', foundational, procedural_content_primacy).
narrative_ontology:cs_axiom_status(procedural_content_primacy, holdable).
narrative_ontology:cs_axiom_grounding('80fc5c34-3c7f-4a03-a085-8aedf878c800', procedural_content_primacy, conventional).
narrative_ontology:cs_axiom('80fc5c34-3c7f-4a03-a085-8aedf878c800', foundational, torture_prohibition_admits_no_balancing).
narrative_ontology:cs_axiom_status(torture_prohibition_admits_no_balancing, holdable).
narrative_ontology:cs_axiom_grounding('80fc5c34-3c7f-4a03-a085-8aedf878c800', torture_prohibition_admits_no_balancing, deontological).
narrative_ontology:cs_reference_frame('80fc5c34-3c7f-4a03-a085-8aedf878c800', procedural_protection_floor).
narrative_ontology:cs_drift_state('80fc5c34-3c7f-4a03-a085-8aedf878c800', contemporary_emergency_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('80fc5c34-3c7f-4a03-a085-8aedf878c800', '2026-08-05T09:30:00Z').
narrative_ontology:cs_kernel_id(udhr_article_3__procedural_hybrid_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, habeas_corpus_claimants).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, human_rights_treaty_bodies).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, regional_human_rights_courts).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, civil_society_litigators).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, compliant_state_parties).
narrative_ontology:constraint_beneficiary(udhr_article_3__procedural_hybrid_reading, non_compliant_state_parties).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, administratively_detained_persons).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, state_executive_branches).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(udhr_article_3__procedural_hybrid_reading, compliant_state_parties).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, habeas_corpus_doctrine).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, absolute_torture_prohibition_norm).
narrative_ontology:constraint_vindicates(udhr_article_3__procedural_hybrid_reading, judicial_review_of_detention_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons in custody who petition a court to review the legality of their detention. Where the judiciary is independent, a hearing arrives within days or weeks and unlawful custody ends; the guarantee converts their confinement from a fact into a contestable case. Where courts are captured or emergencies suspend review, the same petition stalls for years, and the claimant's only lever is repetition.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, habeas_corpus_claimants, beneficiary,
    powerless, immediate, trapped, national).

% Persons held under immigration rules, emergency decrees, or security-certificate regimes where review is deferred, rubber-stamped, or routed to bodies that almost never order release. The guarantee promises them the same hearing everyone else gets; in practice they wait out multi-year committee procedures they rarely reach, and custody continues throughout. Their position marks the measurable distance between the promise and the delivery.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, administratively_detained_persons, payer,
    powerless, immediate, trapped, national).

% Committees of independent experts that receive state reports, issue concluding observations, decide individual communications, and request interim measures. They set the interpretive agenda for what adequate review and prohibited treatment mean, but possess no marshal: compliance depends on state cooperation they cannot compel. Their calendar, staffing, and doctrinal output all scale with the reporting cycle.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, human_rights_treaty_bodies, agenda_setter,
    organized, generational, identity_locked, global).

% Courts with binding jurisdiction over detention and ill-treatment cases in their regions. They issue judgments, pilot judgments, and interim measures; enforcement runs through political committees composed of the same states they judge. Decades of backlog coexist with landmark rulings that reshaped domestic detention law.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, regional_human_rights_courts, agenda_setter,
    institutional, generational, identity_locked, continental).

% Police ministries, interior ministries, intelligence services, and immigration authorities that run the custodial systems the guarantee reaches. They surrender detention discretion to judges, absorb litigation exposure, and answer reporting requests, while retaining emergency powers that let them dilute review precisely when custody expands. They administer domestic implementation and lead resistance to international oversight, often simultaneously.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, state_executive_branches, payer,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, state_executive_branches, agenda_setter).

% States that maintain functioning review and honor the torture prohibition. They collect reputational capital, rule-of-law investment signals, and mutual-assurance benefits from the common floor; they also fund the machinery, submit to scrutiny, and accept constraints on their own executives. Their recurring grievance is fellow members who collect the same reputational credit at a fraction of the compliance cost.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, compliant_state_parties, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__procedural_hybrid_reading, compliant_state_parties, payer).

% States that maintain formal membership — ratification, periodic reports, delegation appearances — while practicing prolonged administrative detention, diluted review, or custodial mistreatment. Membership prices reputational credit identically for them and for scrupulous compliers, so they collect the signal at a fraction of the cost. Adverse observations are the main consequence they face, and they can dismiss those at will.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, non_compliant_state_parties, beneficiary,
    institutional, biographical, arbitrage, national).

% Public-interest lawyers and advocacy organizations that build dockets on the procedural hooks: habeas petitions, ill-treatment complaints, detention-condition challenges. Their funding, victories, and institutional growth track the availability of reviewable claims. A shift of the article's operative content toward substantive entitlements would restructure their entire caseload.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, civil_society_litigators, beneficiary,
    organized, biographical, mobile, global).

% Academic lawyers and philosophers who map how the article's text, drafting history, and institutional practice fit together. They publish the critiques each reading levels at the others and supply the doctrinal vocabulary courts and committees borrow. They hold no enforcement role and bear none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(udhr_article_3__procedural_hybrid_reading, human_rights_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__procedural_hybrid_reading, non_compliant_state_parties).
narrative_ontology:fixing_cost_class(udhr_article_3__procedural_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared procedural floor for the treatment of persons in state custody: any state detaining someone must offer timely independent review of the detention and must not torture. This solves a real collective-action problem — states facing security pressures would otherwise face domestic incentives to race downward on detention safeguards, and a common floor lets them bind themselves mutually at acceptable cost.
% TRANSFER_FUNCTION: Moves detention-review authority from executives to independent judges and expert bodies; moves physical security during custody from detainees' exposure to a legal assurance; moves reputational capital toward states that participate formally; and moves mandates, dockets, and budgets toward the monitoring institutions. The transfers land unevenly: review authority shifts on paper everywhere, in practice chiefly where courts are independent.
% ABSENT_VOICES: Persons held incommunicado or disappeared — the people the guarantee most directly addresses — cannot petition any body. Communities pressing for material security provisions have no seat inside this reading's frame: their claims are redirected to other instruments rather than heard as Article 3 claims. Imprisoned populations without counsel are similarly voiceless in treaty processes designed around written submissions.
% DISAPPEARANCE_RATIONALE: If the procedural guarantee vanished overnight, detention review would default to executive discretion in most jurisdictions, the torture prohibition would lose its principal legal anchor, regional courts would lose their detention docket, the monitoring complex would lose its mandate core, and compliant states would lose a reputational currency they currently spend. Custodial practice would reorganize around whatever domestic brakes remained.
% FOUNDING_PROBLEM: State atrocities committed under color of law in the 1930s and 1940s: secret police detention, torture, and extrajudicial killing, and the discovery that domestic law offered no brake when the state itself was the predator. The drafters sought a floor no government could lawfully cross with its own citizens.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: ICRC detention-visit reporting documents persistent custodial abuse across conflicts; investigative journalism has documented secret-detention programs operated by states that simultaneously filed compliant treaty reports; and state executives' own recurring assertions that emergency detention powers remain indispensable attest the underlying problem from the adversary's side. The liveness finding does not rest on testimony from treaty bodies, courts, or advocacy organizations, all of which hold mandates that a live problem sustains.
narrative_ontology:disappearance_verdict(udhr_article_3__procedural_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__procedural_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__procedural_hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_article_3__procedural_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__procedural_hybrid_reading, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__procedural_hybrid_reading_tests).
:- end_tests(udhr_article_3__procedural_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.46 at interval end) because the arrangement simultaneously delivers real protection — where courts are independent, custody becomes contestable and the torture prohibition anchors prosecutions — and collects real rents: discretion surrendered by executives, mandates and dockets accumulated by administrators, and reputational credit paid out identically to complying and violating members. Suppression (0.38) is structural and soft: the arrangement's coercive force is reputational and jurisdictional, not physical; derogation channels, reservations, denunciation, and the two rival readings all remain open exits, so alternatives are nowhere near collapsed (accessibility_collapse 0.28). Resistance (0.55) is sustained and organized — sovereignist backlash, non-cooperation with special procedures, withdrawals — because the arrangement's costs concentrate on exactly the actors with the most exit capacity. Theater (0.52) reflects a reporting-and-review cycle in which roughly half the activity is ritual exchange: states file, committees observe, nothing in the custodial system moves. The temporal series shows one full securitization cycle across the interval: institutional buildup through the Cold War decades, a stress spike around 2001 when emergency detention expanded faster than review could follow, and a partial correction thereafter as litigation and interim measures clawed back ground. The oscillation is partly the mechanism itself: crises are when violating states most need reputational cover, so each emergency purchases fresh legitimation rents, and each cycle's settlement leaves residue — theater and extraction settle higher than the previous trough. Base_properties were measured at the 2026 endpoint: late-correction phase, elevated-theater plateau. Coalition note: the powerless payer seats cannot coordinate directly — they are, by definition, in custody — so their coalition channel runs entirely through civil_society_litigators; where civil society is suppressed, those seats lose even indirect aggregation and sit nearest the pure-target end.
 *
 * PERSPECTIVAL GAP:
 *   The same text computes as three different arrangements depending on seat. From the administrator seats (treaty bodies, regional courts) the arrangement is a life's work: identity-locked institutions whose mandate, staffing, and doctrinal authority are constituted by the procedural frame, experiencing near-zero extraction and high stakes in its continuation — if the frame broke, these organizations would not merely lose function, they would lose self-definition. From the executive seat the arrangement is a sovereignty cost with an arbitrage escape hatch: strong directional pressure toward the target end, heavily damped by derogation, delay, and forum-shifting, so the effective burden lands far below the nominal constraint. From the trapped detainee seats the arrangement is promise-or-nothing: full-weight exposure wherever delivery fails, with no alternative forum to reach. Compliant and non-compliant state parties occupy identical power atoms with opposite relationships to enforcement exposure — the differentiation is entirely constraint-specific, not global-power-specific.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: habeas_corpus_claimants, civil_society_litigators, and the administrator seats are subsidized by the arrangement. Victim declarations map to high directionality: administratively_detained_persons (full weight, trapped — amplified) and state_executive_branches (full weight, arbitrage — damped). Compliant_state_parties straddle: they pay compliance and collect assurance, landing near symmetric. Non_compliant_state_parties are declared beneficiaries and are the affirmative gain_flow seat: the arrangement's extraction demonstrably accrues to them as reputational rent priced independently of conduct. No directionality_overrides are authored: the derivation from declared roles plus exit options already separates every seat that matters, and an override keyed to power_atom would wrongly homogenize the institutional seats (courts, executives, compliant states) whose divergence is precisely the finding. Inter-institutional dynamics: courts hold binding jurisdiction but politically mediated enforcement; treaty bodies hold agenda-setting but no compulsion; executives hold implementation and the emergency levers — three institutional seats with different exit structures from the same framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state predation under color of law — is live and externally corroborated, so no mandatrophy resolution is declared. The atrophy risk is concentrated in the international reporting layer, where theater has risen monotonically across the interval while delivery moved only with litigation; if the domestic incorporation layer ever hollowed out the way the reporting layer already has, the arrangement would complete a drift toward inertial maintenance. The classification disciplines both mislabelings: reading the arrangement as pure coordination would erase the legitimation-rent asymmetry that is its largest steady transfer; reading it as pure extraction would erase the documented protection delivered where courts function and the torture prohibition's anchoring effect on prosecutions. Tangled rope preserves both facts and locates the tension in the same machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (procedural_hybrid_reading) of kernel udhr_article_3; how would classification change under the sibling readings negative_liberty_reading or positive_entitlement_reading?',
    'Cross-file comparison of the three family stories: each sibling authors its own epsilon, beneficiaries, and victims; per-seat classifications in this file are valid only under this reading.',
    'Under positive_entitlement_reading the protected class becomes materially deprived populations and the cost-bearers become fiscal capacity; under negative_liberty_reading the target set narrows to state violence itself. The beneficiary/victim structure and epsilon authored here do not transfer across readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one reading of a contested kernel; sibling readings are separate constraints, not hedges inside this one.').

omega_variable(
    substantive_bracket_stability,
    'Is Article 3''s refusal to resolve the substantive liberty/welfare contest principled neutrality, or evasion that entrenches whichever substantive order prevails?',
    'Comparative analysis of jurisdictions where the procedural guarantees operate under sharply different substantive regimes: if protection outcomes are invariant to the substantive order, the bracket is principled; if outcomes track the prevailing order, the bracket launders it.',
    'If evasion, epsilon rises (proceduralism functioning as cover for substantive extraction) and the arrangement drifts snare-ward; if principled neutrality, the current tangled_rope reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substantive_bracket_stability, conceptual, 'Whether the reading''s defining bracket is neutral or load-bearing for extraction.').

omega_variable(
    enforcement_delivery_gap,
    'What fraction of detained persons in state parties can actually obtain timely, effective review of their detention?',
    'Audit of time-to-hearing distributions, interim-measure compliance rates, and individual-communication outcome data across jurisdictions.',
    'A wide delivery gap pushes theater_ratio toward inertial-maintenance territory for the international layer; a narrow gap confirms the coordination function is substantively delivered and supports the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_delivery_gap, empirical, 'Promise-versus-delivery fraction for the review guarantee.').

omega_variable(
    administrator_capture_ambiguity,
    'Do the administering institutions (treaty bodies, regional courts) discipline state custodial practice, or expand institutional mandate and doctrinal territory independently of protection delivered?',
    'Track protection outcomes against institutional activity volume: if docket growth and output volume decouple from measurable change in detention practice, mandate expansion dominates.',
    'Capture dominance concentrates receipts in the administrator seats and pushes the arrangement snare-ward; discipline dominance supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrator_capture_ambiguity, empirical, 'Administrator seat: disciplining organ or mandate-expanding bureaucracy.').

omega_variable(
    derogation_scope_drift,
    'How far does emergency derogation and non-application practice stretch the torture prohibition and the review requirement in operation, despite their formal absoluteness?',
    'Systematic comparison of declared derogations, unofficial practices surfaced by litigation and investigative journalism, and jurisprudential responses to both.',
    'Wide operational stretching undermines the practical force of the absolute-prohibition axiom and raises epsilon; tight practice confirms the floor holds under stress.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derogation_scope_drift, empirical, 'Operational gap between the categorical prohibition and emergency-era practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__procedural_hybrid_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_a3_proc_hyb_tr_t1948, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_tr_t1948, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_tr_t1966, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1966, 0.28).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_tr_t1966, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_tr_t1984, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1984, 0.36).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_tr_t1984, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_tr_t1994, udhr_article_3__procedural_hybrid_reading, theater_ratio, 1994, 0.4).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_tr_t1994, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_tr_t2001, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2001, 0.46).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_tr_t2001, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_tr_t2008, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2008, 0.49).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_tr_t2008, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_tr_t2015, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2015, 0.51).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_tr_t2015, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_tr_t2026, udhr_article_3__procedural_hybrid_reading, theater_ratio, 2026, 0.52).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(udhr_a3_proc_hyb_be_t1948, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1948, 0.25).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_be_t1948, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_be_t1966, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1966, 0.32).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_be_t1966, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_be_t1984, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1984, 0.4).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_be_t1984, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_be_t1994, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 1994, 0.44).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_be_t1994, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_be_t2001, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2001, 0.54).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_be_t2001, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_be_t2008, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_be_t2008, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_be_t2015, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2015, 0.47).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_be_t2015, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_be_t2026, udhr_article_3__procedural_hybrid_reading, base_extractiveness, 2026, 0.46).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_a3_proc_hyb_su_t1948, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1948, 0.15).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_su_t1948, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_su_t1966, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1966, 0.24).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_su_t1966, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_su_t1984, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1984, 0.35).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_su_t1984, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_su_t1994, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 1994, 0.39).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_su_t1994, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_su_t2001, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2001, 0.44).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_su_t2001, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_su_t2008, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2008, 0.42).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_su_t2008, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_su_t2015, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2015, 0.4).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_su_t2015, observed).
narrative_ontology:measurement(udhr_a3_proc_hyb_su_t2026, udhr_article_3__procedural_hybrid_reading, suppression_requirement, 2026, 0.38).
narrative_ontology:measurement_basis(udhr_a3_proc_hyb_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__procedural_hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__negative_liberty_reading).
narrative_ontology:affects_constraint(udhr_article_3__procedural_hybrid_reading, udhr_article_3__positive_entitlement_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'UDHR Article 3 — right to life, liberty and security of person' decomposes into three structurally distinct readings of one kernel text. This file instantiates the procedural hybrid reading: enforceable content equals review availability plus the torture prohibition, with the substantive question bracketed. Sibling files instantiate the negative liberty reading (target set: state deprivation itself) and the positive entitlement reading (protected class: materially deprived populations; cost bearer: fiscal capacity). The siblings' epsilon values, victim sets, and classifications differ; citation runs in both directions (each reading cites the text and the others' failures), so edges are recorded as mutual family membership rather than a strict upstream chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
