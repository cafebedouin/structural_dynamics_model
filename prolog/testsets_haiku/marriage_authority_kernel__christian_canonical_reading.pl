% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Marriage Authority (India, ICMA 1872 reading)
 *   domain: religious_governance/constitutional_pluralism/family_law
 *
 * SUMMARY:
 *   In India, the Christian Marriage Act 1872 codifies marriage authority for
 *   Indian Christians, grounded in Christian canonical law as interpreted by
 *   the church hierarchy and applied by Indian civil courts. This constraint
 *   story instantiates ONE reading of the contested marriage authority kernel
 *   — the Christian canonical reading: marriage is a sacrament indissoluble
 *   except by annulment under narrow ecclesiastical grounds, administered by
 *   church tribunals and civil courts applying canonical doctrine. This
 *   reading coexists with four sibling readings in the same kernel: Hindu
 *   codified (civil courts interpreting Hindu Marriage Act 1955 with
 *   mutual-consent divorce), Muslim Shariat (qazis and Muslim personal law
 *   boards interpreting Islamic law, talaq authority), Parsi communal (Parsi
 *   Marriage and Divorce Act 1936 with Reformed Parsi custom), and secular
 *   civil (Special Marriage Act 1954 grounded in constitutional individual
 *   rights, available to all Indians). Each reading produces a different
 *   constraint with different ε, beneficiary/victim structure, and type. This
 *   story is ONLY the Christian canonical reading. The sibling readings are
 *   other constraint stories, linked via network.affects_constraints. The
 *   foundational tension: the Christian reading grounded itself originally in
 *   sacramental indissolubility as absolute doctrine, but post-independence
 *   constitutional values (Article 14 equality) and legislative reforms
 *   (Special Marriage Act 1954 providing escape routes) have created a gap
 *   between the reading's reference frame (indissolubility) and observed
 *   practice (increasing access to exit via civil law). The canonical
 *   authority persists partly by inertia (church institutional interest) and
 *   partly by continued acceptance among conservative Christian communities
 *   who value the sacramental framing.
 *
 * KEY AGENTS:
 *   - christian_church_authority — Institutional custodian of canonical doctrine; sets and enforces marriage dissolution standards via church tribunals and doctrinal interpretation; holds power to define valid marriage and grounds for annulment. Institutional power, identity-locked to the reading.
 *   - divorced_women_without_fault — Primary targets of extraction; trapped by fault-based divorce bar; cannot exit without proving cruelty, adultery, or desertion (often absent or impossible to document); identity-locked to Christian community; face biological clock constraints and social stigma as 'abandoned wives' rather than divorcees.
 *   - interfaith_couples — Secondary targets; trapped by asymmetric regimes (Christian partner's rights constrained, non-Christian partner's possibly less so); face bargaining imbalance and legal fragmentation.
 *   - christian_civil_courts — Enforcement and interpretation machinery; apply the statute but increasingly recognize post-independence constitutional values and alternative readings.
 *   - special_marriage_act_1954 — (not an agent but a rule) — Represents the secular civil reading: available escape route for all Indians (including Christians) seeking mutual-consent divorce and no-fault grounds. Creates competition for authority; underground pressure on the canonical reading's scope.
 *   - indian_constitutional_law — (not an agent) — Article 14 (equality) and Article 25 (freedom of religion, bounded by laws of general application) represent the secular civil reading's authority grounding; formally excluded from personal law review, but jurisprudence increasingly questions the compartmentalization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.62).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.58).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Marriage Authority (India, ICMA 1872 reading)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "religious_governance/constitutional_pluralism/family_law").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, '7320a3c6-4a12-46ee-91a2-452dd4bf9d8f').
narrative_ontology:cs_kernel_codification('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', fixed_text).
narrative_ontology:cs_authority_grounding('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', extraction).
narrative_ontology:cs_interpretation_layer_present('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f').
narrative_ontology:cs_reading_relation('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', foundational, marriage_sacramental_indissolubility).
narrative_ontology:cs_axiom_status(marriage_sacramental_indissolubility, holdable).
narrative_ontology:cs_axiom_grounding('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', marriage_sacramental_indissolubility, deontological).
narrative_ontology:cs_axiom('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', foundational, ecclesiastical_authority_over_marriage_adjudication).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_over_marriage_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', ecclesiastical_authority_over_marriage_adjudication, deontological).
narrative_ontology:cs_reference_frame('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', canonical_indissolubility_with_ecclesiastical_authority).
narrative_ontology:cs_drift_state('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', contemporary_post_special_marriage_act, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7320a3c6-4a12-46ee-91a2-452dd4bf9d8f', '2026-06-19T14:32:18Z').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_church_authority).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, conservative_family_structures).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, divorced_women_esp_fault_absent).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, interfaith_couples).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, lgbtq_christians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, christian_married_couples_conservative).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, sacramental_indissolubility_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, ecclesiastical_jurisdiction_over_marriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and adjudicates Christian marriage doctrine under ICMA 1872. Maintains authority over marriage validation, annulment procedures (via church tribunals), and ecclesiastical divorce standards. Defends the reading that marriage is a sacrament indissoluble except by annulment on narrow canonical grounds. Faces no institutional exit — the church is the custodian of this reading by doctrinal definition.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_church_authority, agenda_setter,
    institutional, civilizational, constrained, national).

% Benefit from the framing of marriage as sacrament and from communal structures (church-mediated reconciliation, social stability norms, property devolution certainty) that the canonical reading supports. Exit for them means adopting a different reading (civil law) or leaving the Christian community frame altogether.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_married_couples_conservative, beneficiary,
    moderate, biographical, constrained, national).

% Cannot dissolve marriages under the canonical reading without proving fault (cruelty, adultery, desertion). Even with proven fault, church annulment procedures are slow, expensive, and apply narrow canonical criteria. Women without fault grounds remain legally married despite separation, unable to remarry under ICMA 1872, trapped in the identity category of 'Christian wife.' Exit options: prove fault (if false or absent, structural barrier); pursue civil courts via Special Marriage Act (procedurally available but reads as apostasy in church framing); leave the Christian community (identity-locked — costs include family rupture, social isolation in Christian enclaves).
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, divorced_women_esp_fault_absent, payer,
    powerless, biographical, identity_locked, national).

% Couples where one partner is Christian: the Christian partner's marriage dissolution rights are constrained by ICMA 1872 (fault-based), while the non-Christian partner may have easier dissolution rights under their own personal law (e.g., Hindu Succession Act permits mutual consent divorce). Asymmetry creates bargaining leverage for the constrained partner and uncertainty for the unconstrained partner. Exit: formal conversion of one partner (expensive identity cost), legal fragmentation (each partner pursues their own law separately — costly, incomplete), or accepting the most restrictive regime (de facto choice imposed).
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, interfaith_couples, payer,
    moderate, biographical, constrained, national).

% Are excluded from marriage recognition under ICMA 1872 (marriage is defined as heterosexual sacramental union). Those already in same-sex partnerships have no legal recognition path under this reading; those seeking recognition must exit Christian legal identity (adopt civil law via Special Marriage Act, which requires legal separation from ICMA frame). Exit is identity-locked: remaining Christian and remaining legally married under this constraint are structurally incompatible.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, lgbtq_christians, payer,
    powerless, biographical, identity_locked, national).

% Apply ICMA 1872 per statute but increasingly recognize alternative framings (Special Marriage Act 1954, mutual-consent divorce under reformed interpretations of ICMA). Their interpretive scope has expanded to incorporate post-independence constitutional values (equality, individual autonomy) while preserving the canonical structure. They are the enforcement machinery but also the site of doctrinal drift away from pure ecclesiastical reading. Tension is growing between canonical strictness and constitutional egalitarianism.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, christian_civil_courts, observer,
    institutional, generational, analytical, national).

% Would assert that marriage dissolution rights, especially fault grounds, are subject to constitutional equality review (Article 14, right to equality before law). They are structurally excluded from canonical authority (church-administered tribunals do not report to constitutional review), but increasingly their jurisprudence recognizes the tension and questions whether personal law compartmentalization can shelter restrictions incompatible with constitutional equality. Multiple petitions before courts challenging ICMA 1872 fault-based divorce as unconstitutional.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, indian_constitutional_court, excluded,
    institutional, generational, analytical, national).

% Would argue for reinterpretation of canonical law to permit mutual-consent or no-fault divorce consistent with Christian teaching (Christian feminism, liberation theology, imago dei as basis for autonomy) and constitutional equality. They hold a reading of Christian doctrine that conflicts with the ecclesiastical hierarchy's reading. Their exclusion is enforced: formal church authority (bishops, tribunals, synods) does not seat feminist Christian voices in canonical adjudication; reform proposals circulate in advocacy spaces but have not penetrated formal church authority structures.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, feminist_christian_advocates, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__christian_canonical_reading, christian_church_authority).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, church-administered marriage regime for Indian Christians: validates unions according to Christian sacramental doctrine, resolves disputes through ecclesiastical tribunals, ensures property and succession certainty within Christian family structures, and maintains communal coherence around Christian marriage norms and sexual ethics grounded in canonical teaching.
% TRANSFER_FUNCTION: Moves marriage dissolution rights (restricted access to divorce) from individuals to the church hierarchy, and extracts conformity costs (identity fusion with the canonical reading, acceptance of fault-based dissolution bars) from those who wish to exit Christian marriage without leaving Christian identity. Resources and authority flow from payers (divorced women without fault, interfaith couples, LGBTQ Christians) to beneficiaries (church authority, conservative Christian communities) in the form of deferred or denied divorce access and enforced subordination of individual autonomy to ecclesiastical judgment.
% ABSENT_VOICES: Feminist Christian reinterpretations of doctrine (arguing for mutual-consent divorce as compatible with Christian liberation theology), LGBTQ Christians (excluded from marriage recognition), and post-independence constitutional actors asserting Article 14 equality principles (formally excluded from personal law review under the compartmentalization doctrine but increasingly asserting claims to jurisdiction). These voices would argue the constraint is unnecessarily restrictive relative to Christian theology itself and incompatible with Indian constitutional values; they remain structurally outside canonical adjudication.
% DISAPPEARANCE_RATIONALE: If the Christian canonical marriage authority (ICMA 1872 reading) disappeared, Christian marriage in India would reorganize under either the Special Marriage Act 1954 (civil registration, mutual-consent divorce, no-fault grounds, available to all Indians regardless of religion) or a reformed Christian personal law reading (some Christian denominations have moved toward mutual-consent models in advocacy, though not yet in formal law). Thousands of separated Christians currently trapped under fault-based law would gain immediate exit access; interfaith couples would resolve asymmetries via unified civil law; LGBTQ Christians would gain recognition routes. Property and succession regimes would shift to follow secular or reformed personal law frameworks. Church authority over marriage would cease to operate as a binding legal constraint. The world would not revert to pre-ICMA fragmentation (matrimonial law is now expected everywhere) but would shift toward either unified civil law or multiple reformed-personal-law readings rather than resting on a single canonical ecclesiastical authority.
% FOUNDING_PROBLEM: Pre-colonial Indian Christians lacked unified marriage law; Portuguese and British colonialism imposed Christian canon law on Christian communities via statutory codification. ICMA 1872 resolved fragmentation by establishing a single statutory regime grounded in ecclesiastical authority and canonical doctrine, replacing colonial canon law directly with statute while preserving church interpretive authority and sacramental framework. The founding problem was: how to provide unified Christian marriage law in a colonial legal pluralism where each community (Hindu, Muslim, Christian, Parsi) had separate personal law?
% FOUNDING_PROBLEM_CORROBORATION: Church authorities attest the founding problem is still live: unified Christian marriage law grounded in canonical authority is necessary to prevent doctrinal chaos and community division; ICMA 1872 remains the authoritative expression of Christian commitment to marriage as sacrament. Feminist Christian advocates, constitutional scholars, and legislative reform advocates (2010, 2012, 2017 reform bills) attest the founding problem (fragmentation prevention) was functionally solved by the Special Marriage Act 1954 (mutual-consent divorce available to all Indians regardless of religion, providing unified exit route) and that ICMA 1872 now persists as mandatrophy — a dead founding problem shadowed by an institutional church interest in preserving authority. Multiple parliamentary committee reports and judicial dicta have framed the founding problem as solved and the canonical restriction as a vestigial authority structure. The corroboration for 'dead' status comes from outside the benefiting parties: legislative bodies, constitutional courts (obiter dicta questioning personal law compartmentalization), and Christian reform advocates (not institutional church hierarchy).
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: The constraint extracts substantially from divorced women (fault-based bar, years of litigation, social stigma) and from LGBTQ Christians (complete exclusion from recognition). It extracts less uniformly from interfaith couples (asymmetry creates leverage but not complete trapping). The extraction is not total — the Special Marriage Act provides an exit route, though one that reads as apostasy in canonical framing. Suppression 0.58: The suppression mechanism is partly structural (legally narrow grounds for divorce, expensive church tribunal procedures) and partly internalized (Christian identity fusion with indissolubility doctrine, community shame around divorce, fear of excommunication in close-knit communities). The suppression has weakened modestly over time (post-1950 constitutional values, visible divorce among peers) but remains substantial because church authority retains social enforcement power. Theater ratio 0.41 and rising: The constraint's functional purpose (sacramental coherence, community stability) remains real but increasingly shares space with performative defense of ecclesiastical authority against encroaching constitutional equality norms. Post-1950, the theater component rises (church defending its turf against Special Marriage Act options) while the functional component shrinks (divorce becomes socially acceptable, even among conservative Christians). The measurements reflect this drift: extractiveness stable (the bar itself does not shift, but alternatives erode its effective scope), theater rising (performance of ecclesiastical authority becomes more prominent), suppression stable (structural and internalized mechanisms persist without major reform).
 *
 * PERSPECTIVAL GAP:
 *   The church agenda-setter and conservative Christian beneficiaries experience this constraint as genuine coordination (sacramental stability, communal coherence, defense of Christian doctrine against secular encroachment). They author the founding problem as 'live' (indissolubility as binding Christian teaching) and the constraint as timeless. From the payer seats (divorced women, LGBTQ Christians, constitutional observers), the same constraint operates as enforced extraction: the church uses sacramental doctrine as a cover story for preserving institutional authority over Christian marriage against post-independence egalitarian values. They author the founding problem as 'dead' (legal pluralism problem was solved by Special Marriage Act; ICMA 1872 persists as mandatrophy). The engine computes this divergence from structural data: high power asymmetry (institutional church vs. powerless divorced women), restricted exit options (identity-locked for women; trapped for interfaith couples), and measurable harm concentration (divorce access denial, LGBTQ exclusion). The computed type will likely show tangled_rope from the beneficiary seat (genuine coordination + active enforcement) and snare or worse from the payer seats (pure extraction with constrained alternatives), with the divergence itself being the diagnostic signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Church authority: d ≈ 0.1–0.2 (full beneficiary — collects authority, administers rules, faces no exit). Conservative Christian beneficiaries: d ≈ 0.2–0.3 (strong beneficiary — sacramental framing aligns with their values, exit costs are high but available). Divorced women without fault: d ≈ 0.85–0.95 (full target — extraction bars them from exit, high suppression, identity-locked). Interfaith couples: d ≈ 0.65–0.75 (strong target — asymmetric constraint, constrained exit). LGBTQ Christians: d ≈ 0.9+ (extreme target — complete exclusion, identity-locked, no legal exit within Christian frame). Civil courts: d ≈ 0.5 (near-symmetric — enforcing rules, but increasingly recognizing alternative framings; growing tension). Constitutional law: not authored in stakeholders (non-agent), but represents d ≈ 0.0 beneficiary position to the secular civil reading (fully beneficiary to constitutional equality; fully target to canonical authority). The directionality derivation flows from beneficiary/victim declarations + exit options: victims + identity-locked or trapped = high d; beneficiaries + arbitrage or analytical exit = low d.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy test examines whether the founding problem is dead but the constraint persists. Evidence for mandatrophy: (1) Founding problem: unified Christian marriage law to prevent fragmentation. Status: dead. The Special Marriage Act 1954 solved fragmentation by offering all Indians a common escape route. (2) Constraint persistence: ICMA 1872 remains in force, enforced by church tribunals and civil courts. (3) Persistence mechanism: inertia + institutional self-interest (church preserves authority, conservative communities preserve sacramental framing) rather than solving the founding problem. (4) Theater signature: theater_ratio rising from 0.25 (1872, genuine functional defense) to 0.41 (2026, increasingly performative — defending ecclesiastical authority against constitutional equality rather than solving coordination problems). The classification should shift toward piton if theater dominates, or remain tangled_rope if the coordination function retains real beneficiary support. The measurement series show theater rising while extractiveness and suppression stay stable, consistent with inertial persistence (the constraint is not collapsing but is increasingly theatrical). The mandatrophy verdict: YES, the founding problem is dead; the constraint persists as a combination of institutional inertia (piton signature) and genuine conservative-community preference for sacramental framing (tangled_rope signature). The two signatures coexist: from the beneficiary seat, it is tangled_rope (real coordination + active enforcement); from the payer seat and observer seat, it is piton (dead founding problem + theatrical maintenance by institutional inertia).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_vs_contractual_grounding,
    'Is the Christian canonical reading''s grounding in sacramental indissolubility doctrine (deontological, ecclesial) genuinely binding on Christians in post-independence secular India, or has it become a performative authority claim preserved mainly by institutional inertia?',
    'Comparative analysis of Christian denomination responses to reform bills: if reform-forward denominations reinterpret indissolubility doctrine as compatible with mutual-consent divorce (citing updated theology: marriage as covenant of equals, not sacrament of subjection), the deontological grounding remains contestable within Christian tradition itself. Survey data on Christian women''s perception of ICMA 1872 (is it experienced as binding doctrine or as institutional constraint?) would clarify whether suppression is internalized or structural.',
    'If reinterpretation is widespread within Christian tradition and suppression is mostly structural (not internalized), the constraint reclassifies from tangled_rope (coordination + enforcement) toward snare (extraction masked by institutional authority). If the deontological grounding retains strong believer acceptance, tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacramental_vs_contractual_grounding, conceptual, 'Whether the canonical reading''s sacramental grounding is living doctrine or institutional cover story.').

omega_variable(
    identity_lock_mechanism,
    'For Christian women seeking divorce outside ICMA 1872 (via Special Marriage Act), what is the actual cost structure of exit? Is the identity-lock real (community rupture, family disownment, church exclusion) or repairable (exit is costly but leaves identity available)?',
    'Ethnographic study of Christian women who exited via Special Marriage Act: did they face permanent excommunication or community reintegration? Post-exit, do they retain Christian identity or does conversion/secularization follow? Institutional data from church archdiocese on formal excommunication policy (is it automatic for SMA adoption or discretionary / rare?).',
    'If identity-lock is permanent and enforced (automatic excommunication), suppression and extractiveness increase; the constraint approaches snare (victims have no viable exit that preserves identity). If exit is costly but identity-repairable (reintegration post-divorce, no formal excommunication), suppression decreases slightly and the constraint stays tangled_rope with moderate extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether Christian community enforcement of divorce bar includes permanent identity loss or reversible reputational cost.').

omega_variable(
    special_marriage_act_erosion,
    'Has the availability of Special Marriage Act 1954 (mutual-consent divorce, no-fault grounds) functionally superseded ICMA 1872 for Indian Christians seeking exit, or does the canonical reading retain sufficient social enforcement power to maintain its extraction?',
    'Statistical analysis: what percentage of Christian divorces are filed under SMA vs. ICMA 1872, by decade (1950–2026)? If SMA adoption among Christians is 70%+, the effective scope of ICMA 1872 has collapsed and the constraint is piton (dead in practice, maintained theatrically by church authority). If ICMA 1872 divorces remain majority or substantial, the canonical reading retains enforcement power.',
    'If SMA has effectively superseded ICMA 1872 (high SMA adoption), the constraint shifts to piton classification and theater_ratio explanation becomes primary (performance of institutional authority despite functional obsolescence). If ICMA 1872 divorces remain substantial, tangled_rope classification holds and extraction remains real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(special_marriage_act_erosion, empirical, 'Whether the canonical reading''s fault-based regime remains functionally binding or has been eroded by competing legal alternatives.').

omega_variable(
    reading_kernel_ambiguity,
    'Is the marriage authority kernel genuinely contested across Indian Christianity (different Christian denominations holding different readings), or is the contest primarily between Christian canonical authority and secular constitutional law (Christians vs. the secular Indian state)?',
    'Survey of Christian denominations in India: do they endorse ICMA 1872 as canonical, or have some adopted reformed readings closer to Special Marriage Act principles? If Reformed/Progressive Christian bodies adopt mutual-consent divorce positions, the kernel contest is intra-Christian; if only church hierarchy defends ICMA 1872 while lay Christians defect to SMA, the contest is structural (institutional authority vs. individual choice) rather than truly two readings of one kernel.',
    'If the contest is intra-Christian, the reading classification is correct and the sibling readings within Christianity (e.g., a progressive Christian reading advocating reformed mutual-consent interpretation) would add granularity. If the contest is institutional vs. individual, the kernel structure may need recasting: not ''which authority grounding'' but ''authority itself vs. individual autonomy as the foundational question,'' which would reframe the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether the Christian canonical reading contests with other Christian readings or primarily with secular law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 1872, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1872, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1872, 0.25).
narrative_ontology:measurement_basis(marr_tr_t1872, observed).
narrative_ontology:measurement(marr_tr_t1950, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement_basis(marr_tr_t1950, observed).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement_basis(marr_tr_t1975, observed).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2000, 0.39).
narrative_ontology:measurement_basis(marr_tr_t2000, observed).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement_basis(marr_tr_t2015, observed).
narrative_ontology:measurement(marr_tr_t2026, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 2026, 0.41).
narrative_ontology:measurement_basis(marr_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1872, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1872, 0.58).
narrative_ontology:measurement_basis(marr_be_t1872, observed).
narrative_ontology:measurement(marr_be_t1950, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1950, 0.61).
narrative_ontology:measurement_basis(marr_be_t1950, observed).
narrative_ontology:measurement(marr_be_t1975, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 1975, 0.64).
narrative_ontology:measurement_basis(marr_be_t1975, observed).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2000, 0.63).
narrative_ontology:measurement_basis(marr_be_t2000, observed).
narrative_ontology:measurement(marr_be_t2015, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement_basis(marr_be_t2015, observed).
narrative_ontology:measurement(marr_be_t2026, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(marr_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1872, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1872, 0.52).
narrative_ontology:measurement_basis(marr_su_t1872, observed).
narrative_ontology:measurement(marr_su_t1950, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1950, 0.54).
narrative_ontology:measurement_basis(marr_su_t1950, observed).
narrative_ontology:measurement(marr_su_t1975, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 1975, 0.56).
narrative_ontology:measurement_basis(marr_su_t1975, observed).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement_basis(marr_su_t2000, observed).
narrative_ontology:measurement(marr_su_t2015, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2015, 0.58).
narrative_ontology:measurement_basis(marr_su_t2015, observed).
narrative_ontology:measurement(marr_su_t2026, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(marr_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__christian_canonical_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested marriage authority kernel in India. The kernel is: 'What authority grounds marriage dissolution rights?' Different communities and constitutional frameworks offer different answers, each producing a different constraint with different ε, beneficiary structure, and type. The Christian canonical reading (ICMA 1872) grounded in ecclesiastical authority and sacramental indissolubility coexists with four sibling readings: Hindu codified (civil courts, mutual-consent), Muslim Shariat (qazi/personal-law authority), Parsi communal (community custom), and secular civil (constitutional individual rights). Each reading is authored as a separate constraint story linked here via network.affects_constraints. The family structure enables comparative analysis of how different authority groundings produce different extraction profiles over the same referent (marriage dissolution in India). Per ε-invariance principle (DP-001), each reading authorsits own ε from its own epistemic perspective: the canonical reading sees moderate extractiveness (sacramental coordination + ecclesial enforcement); the secular civil reading sees the canonical regime as highly extractive (obstacle to equality rights). These are not views of one constraint from different angles — they are genuinely different constraints instantiating different readings of the kernel. The ε value is reading-indexed; the family structure preserves that separation while enabling cross-reading comparison.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority_kernel__christian_canonical_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
