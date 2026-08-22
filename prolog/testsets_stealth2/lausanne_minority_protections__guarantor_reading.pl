% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__guarantor_reading, []).

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
 *   constraint_id: lausanne_minority_protections__guarantor_reading
 *   human_readable: Lausanne Minority Protections — Guarantor-State Supervision Reading
 *   domain: international_law/minority_rights/religious_governance
 *
 * SUMMARY:
 *   The 1923 Treaty of Lausanne (Sections III-IV, Articles 37-45) protects
 *   non-Muslim minorities in Turkey. This story instantiates the GUARANTOR
 *   READING of that kernel: the protections are internationally supervised
 *   obligations, enforceable through guarantor-state diplomacy (Greece's
 *   treaty standing) and European human-rights mechanisms, not solely through
 *   domestic Turkish interpretation. The story models the standing
 *   arrangement under contest — the supervision pathway as it has actually
 *   operated (League-era design, post-1946 lapse, post-1990 Strasbourg
 *   revival) — assessed by this reading's own lights; the reading's endorsed
 *   alternative is not the referent. The restrictive and expansive readings
 *   are separate constraint stories linked through
 *   network.affects_constraints; per the epsilon-invariance principle, each
 *   carries its own epsilon, beneficiaries, and classification. Claim/metric
 *   independence is preserved: the claimed type is authored from structural
 *   belief (a transitional supervision architecture whose enforcement is
 *   episodic and whose justification is a bridge toward general mechanisms),
 *   while the metrics describe observed operation (low extraction, moderate
 *   theater, enforcement capacity that decayed and partially revived).
 *
 * KEY AGENTS:
 *   - greek_orthodox_minority_turkey: primary intended beneficiary (powerless/identity_locked) — holds the treaty's protective content; cannot invoke it directly in domestic courts
 *   - armenian_minority_turkey: secondary beneficiary (powerless/constrained) — covered by the provisions, rarely exercises the channel
 *   - jewish_minority_turkey: secondary beneficiary (powerless/mobile) — covered; prefers quiet relations; real exit via emigration
 *   - hellenic_republic_guarantor: agenda-setter and collector (institutional/mobile) — alone holds standing to invoke; converts invocation into bilateral leverage
 *   - turkish_state: primary target (institutional/constrained) — obligated party; bears costs when the pathway fires; resists third-party invocation
 *   - council_of_europe_human_rights_organs: analytical observer (institutional/analytical) — adjudicates when seized; decides on Convention grounds, feeding results back into diplomacy
 *   - protected_minority_members_without_standing: excluded voice (powerless/trapped) — the individuals whose schools, foundations, and worship are the arrangement's subject, present only through institutional and capital-city filters
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__guarantor_reading, 0.3).
domain_priors:suppression_score(lausanne_minority_protections__guarantor_reading, 0.3).
domain_priors:theater_ratio(lausanne_minority_protections__guarantor_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(lausanne_minority_protections__guarantor_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__guarantor_reading, scaffold).
narrative_ontology:human_readable(lausanne_minority_protections__guarantor_reading, "Lausanne Minority Protections — Guarantor-State Supervision Reading").
narrative_ontology:topic_domain(lausanne_minority_protections__guarantor_reading, "international_law/minority_rights/religious_governance").

domain_priors:requires_active_enforcement(lausanne_minority_protections__guarantor_reading).
narrative_ontology:has_sunset_clause(lausanne_minority_protections__guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__guarantor_reading, 'a422900b-c4c8-47f4-9c15-77db12223c40').
narrative_ontology:cs_kernel_codification('a422900b-c4c8-47f4-9c15-77db12223c40', fixed_text).
narrative_ontology:cs_authority_grounding('a422900b-c4c8-47f4-9c15-77db12223c40', lineage).
narrative_ontology:cs_interpretation_layer_present('a422900b-c4c8-47f4-9c15-77db12223c40').
narrative_ontology:cs_reading_relation('a422900b-c4c8-47f4-9c15-77db12223c40', lausanne_minority_protections__restrictive_reading, coexists_with).
narrative_ontology:cs_reading_relation('a422900b-c4c8-47f4-9c15-77db12223c40', lausanne_minority_protections__expansive_reading, influences).
narrative_ontology:cs_axiom('a422900b-c4c8-47f4-9c15-77db12223c40', foundational, external_supervision_essential_to_validity).
narrative_ontology:cs_axiom_status(external_supervision_essential_to_validity, holdable).
narrative_ontology:cs_axiom_grounding('a422900b-c4c8-47f4-9c15-77db12223c40', external_supervision_essential_to_validity, conventional).
narrative_ontology:cs_axiom('a422900b-c4c8-47f4-9c15-77db12223c40', secondary, guarantor_state_invocation_standing).
narrative_ontology:cs_axiom_status(guarantor_state_invocation_standing, holdable).
narrative_ontology:cs_axiom_grounding('a422900b-c4c8-47f4-9c15-77db12223c40', guarantor_state_invocation_standing, conventional).
narrative_ontology:cs_reference_frame('a422900b-c4c8-47f4-9c15-77db12223c40', internationally_supervised_treaty_obligation).
narrative_ontology:cs_drift_state('a422900b-c4c8-47f4-9c15-77db12223c40', contemporary_post_league_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a422900b-c4c8-47f4-9c15-77db12223c40', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, greek_orthodox_minority_turkey).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, armenian_minority_turkey).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, jewish_minority_turkey).
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__guarantor_reading, hellenic_republic_guarantor).
narrative_ontology:constraint_victim(lausanne_minority_protections__guarantor_reading, turkish_state).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, lausanne_reciprocal_guarantee_doctrine).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, international_supervision_of_minority_treaties).
narrative_ontology:constraint_vindicates(lausanne_minority_protections__guarantor_reading, third_party_treaty_guarantee_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A reduced Istanbul-centered community (from over one hundred thousand at the republic's founding to a few thousand today) whose schools, charitable foundations, and worship depend on the treaty's protective articles. It cannot invoke the provisions directly in Turkish courts; its grievances travel only through its foundations' petitions and through Athens' diplomacy. Its patriarchal see is canonically anchored where it stands, so exit means emigration — the path most of the community has already taken, leaving the institutions as the remaining stake.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, greek_orthodox_minority_turkey, beneficiary,
    powerless, civilizational, identity_locked, regional).

% The largest remaining covered community, concentrated in Istanbul with institutions nationwide. It uses the minority-school and foundation provisions routinely but rarely politicizes the guarantor channel: no patron state holds standing for it, and diaspora politics make invocation fraught. It benefits from the pathway's existence as background assurance more than from its exercise.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, armenian_minority_turkey, beneficiary,
    powerless, generational, constrained, national).

% The smallest of the three covered communities. Its leadership generally avoids treaty-based advocacy, preferring quiet relations with the state; its schooling provisions were historically the most-used benefit. Emigration is a real and frequently exercised option, which thins the constituency for invocation year over year.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, jewish_minority_turkey, beneficiary,
    powerless, biographical, mobile, national).

% Holds the treaty standing to raise minority conditions bilaterally and in European fora, publishes recurring reports on the covered communities, and decides unilaterally when to escalate to Strasbourg or de-escalate for bilateral detente. Invocation costs it little and yields negotiating capital across the wider Aegean agenda; it can also trade minority concerns away, as occurred in periods of rapprochement. It collects the leverage the pathway generates; the communities receive whatever remedies filter down.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, hellenic_republic_guarantor, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__guarantor_reading, hellenic_republic_guarantor, beneficiary).

% The obligated party. It affirms the treaty's validity as the foundation of its international settlement while insisting that interpretation and application are domestic matters, and it rejects third-party invocation as interference. It bears concrete costs when the pathway fires: property restitutions ordered under Strasbourg pressure, adverse findings feeding reputational cost, and diplomatic friction. Denouncing the minority section outright would endanger the broader settlement the treaty anchors, so its practical course is de facto domesticization of interpretation while remaining formally bound.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, turkish_state, payer,
    institutional, generational, constrained, national).

% Receives individual applications touching minority life and adjudicates them on Convention grounds, engaging Lausanne arguments when the parties plead them but owing no duty to enforce the treaty as such. Its findings feed back into guarantor diplomacy and into the obligated state's compliance calculations, making it the pathway's most consequential modern venue without making it a supervisor.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, council_of_europe_human_rights_organs, observer,
    institutional, generational, analytical, continental).

% Individual members of the covered communities whose schools, foundations, cemeteries, and worship are the arrangement's subject matter. They appear in the process only as beneficiaries of record or as complainants filtered through their institutions and through capitals; they hold no seat at the bilateral tables where their situation is discussed, and the controlling precedent for their treatment — the 1923 population exchange — was negotiated entirely over their heads.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__guarantor_reading, protected_minority_members_without_standing, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__guarantor_reading, hellenic_republic_guarantor).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__guarantor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives minority-protection promises an external address: observation, protest, and adjudication of violations by parties other than the obligated state's own courts, converting unilateral domestic discretion into a monitored commitment and giving grievances a venue outside the accused state's jurisdiction.
% TRANSFER_FUNCTION: Moves grievance-handling and adjudicative standing from the obligated state's domestic legal order to the guarantor state's chancelleries and European human-rights organs, and moves the resulting reputational, legal, and bargaining costs back onto the obligated state when findings go against it.
% ABSENT_VOICES: The protected individuals themselves. The arrangement was negotiated over the heads of the communities — the population exchange is the controlling precedent — and they enter it only through institutional intermediaries and through the filter of the guarantor state's national priorities, which have at times been traded for bilateral detente. A community member with a direct seat would ask why enforcement depends on another government's convenience.
% DISAPPEARANCE_RATIONALE: The obligated state maintains the provisions are reciprocal, largely executed, and that their loss would change nothing, since domestic law governs application regardless. The communities and the guarantor state maintain the pathway is the only external brake, pointing to the documented record of the supervision-weakest years (the 1942 capital tax, the 1955 pogrom, the 1964 expulsions, the 1971 seminary closure) and arguing that general Convention mechanisms do not reach the treaty's specific subject matter — foundation legal personality, the patriarchate's status, minority schooling. Both positions are live. The likely truth is split: general mechanisms would absorb part of the function, and the treaty-specific remainder would go unprotected.
% FOUNDING_PROBLEM: Making the post-Ottoman settlement internationally tolerable. The rejected Sevres regime had imposed intrusive great-power tutelage; Lausanne substituted narrower reciprocal guarantees for the remaining non-Muslim populations in Turkey (and Muslims in Greece), with supervisory hooks so the compromise would hold between governments that did not trust each other's goodwill.
% FOUNDING_PROBLEM_CORROBORATION: The published negotiating record and mainstream diplomatic history attest the founding problem and its design. Turkish official positions attest the problem's historical reality while declaring it resolved and the supervision spent — corroboration from the paying seat, not the benefiting set. Corroboration for continued liveness comes almost entirely from inside the benefiting set (the guarantor state's recurring reports, the communities' petitions) plus sporadic Strasbourg engagement; no disinterested standing body currently attests that the founding problem remains open, and that absence is itself signal.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__guarantor_reading, contested).
narrative_ontology:founding_problem_status(lausanne_minority_protections__guarantor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__guarantor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__guarantor_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__guarantor_reading_tests).
:- end_tests(lausanne_minority_protections__guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.30 at interval end) because the pathway bites only episodically: it imposes costs on the obligated state when guarantor diplomacy or Strasbourg proceedings succeed, and lies dormant otherwise. Suppression is correspondingly low (0.30): the arrangement coerces no one continuously; its persistence depends on voluntary invocation, not on suppressing exits — indeed the domestic-interpretation alternative remains fully available in practice, which is why accessibility_collapse is low (0.30). Theater is moderate-high (0.50): a large share of supervisory activity (annual reports, parliamentary questions, démarches, resolutions) produces no behavioral change, though a real functional core exists (adverse judgments with implemented remedies, property restitutions following Strasbourg pressure circa 2008-2011). Resistance is high (0.65): the obligated state systematically contests third-party invocation, treats the provisions as reciprocal and largely executed, and confines adjudication to general Convention grounds. The temporal series show a full decay-revival cycle across the eight-point shared grid: enforcement capacity built at 1923 (0.42) decayed with the League's dissolution (0.30 by 1946) to near-abandonment after the 1955 pogrom and 1971 seminary closure went unanswered (0.23-0.24), then partially rebuilt through individual petition rights (1990) and accession conditionality (1999, 0.37), now fading again as accession leverage receded (0.30). The oscillation tracks exogenous leverage windows, not intermittent reinforcement — it is a side effect of great-power attention cycles, not itself an extraction mechanism. End-state scalar values equal the final grid points by construction.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience structurally different constraints under the same text. From the obligated state's seat the arrangement is an imposed external obligation — interference dressed as supervision, felt acutely precisely when it fires. From the community seats it is a lifeline: the only address a grievance has outside the state accused of the violation. From the guarantor state's seat it is a discretionary asset — invoked when useful, shelved for rapprochement, never costly to hold. From the Strasbourg seat it is a docket: arguments pleaded, weighed on Convention grounds, decided without any duty to enforce the treaty as such. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the community seats and the guarantor seat toward the beneficiary pole (low d, damped or inverted effective extraction); the victim declaration drives the obligated state toward the target pole (high d, amplified effective extraction when the pathway activates). Exit modulation differentiates within the poles: the Greek Orthodox community's identity_locked position (a patriarchal see canonically anchored where it is; emigration already consumed most of the flock) and the guarantor's arbitrage-grade mobility (invoke or not, at will, at negligible cost) sit at opposite extremes of the beneficiary side; the Jewish community's real emigration option pulls it further toward pure subsidy than the constrained Armenian seat. The obligated state's constrained exit — formally bound, since the treaty anchors its international settlement, yet de facto self-interpreting on scope — keeps it below the full-target end: it bears real but avoidable-in-practice costs. No directionality overrides are used: the derivation from declarations plus exit options is accepted as the structural first approximation, with the noted residual that the obligated state's successful domesticization of interpretation may soften its effective position below the derived value.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold framing prevents mislabeling in both directions. Reading the arrangement as pure coordination ignores that its supervisory function has twice atrophied into performance (post-1946, post-1955) — a rope verdict would launder the theater. Reading it as pure extraction from the obligated state mistakes episodic diplomatic friction for systematic extraction, when the pathway's actual bite is minimal and intermittent — a snare verdict would inflate a dormant channel into an operating machine. The transitional truth sits between: the supervision was designed to hold a post-imperial settlement until general mechanisms matured, and the live question is whether that transition has completed. The founding problem's status is contested, and the disappearance verdict is contested with it: the mismatch consumer should watch the (dead-status x world-rearranges) cell here, since the strongest evidence of mandate outliving function is the obligated state's position that the provisions are executed and the supervision spent — a position corroborated by the pathway's own failure to answer 1955 and 1971, and resisted by the communities' documented deterioration in exactly the years the supervision was weakest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lausanne_kernel_reading_contest,
    'This constraint is one reading of kernel lausanne_minority_protections (instantiating guarantor_reading). What would adopting a sibling reading change structurally?',
    'Comparative classification across the three reading-stories: restrictive_reading removes the external pathway entirely (the matter becomes a domestic administrative question); expansive_reading loads the pathway with institutional-continuity claims (property, clergy formation, self-administration) that raise both its coordination value and its contest intensity.',
    'Under restrictive adoption the protected communities lose their external address and this arrangement decays toward irrelevance; under expansive adoption the supervision question becomes the decisive battleground and both resistance and effective extraction rise sharply.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lausanne_kernel_reading_contest, conceptual, 'Committer structure: which reading of the Lausanne kernel this constraint instantiates and what sibling adoptions would change.').

omega_variable(
    enforceability_vs_leverage,
    'Does the guarantor-plus-Strasbourg pathway constitute genuine enforceability of the obligations, or only diplomatic leverage that stops short of compelling compliance?',
    'Track implementation after adverse Strasbourg outcomes that engage Lausanne arguments versus outcomes resting solely on Convention provisions; compare Turkish implementation latency across the two classes.',
    'If leverage-only, the supervision function is substantially theatrical and the arrangement drifts toward inertial maintenance; if enforceable, the arrangement is functioning transitional coordination and the coordination-first reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforceability_vs_leverage, empirical, 'Whether external supervision compels compliance or merely pressures.').

omega_variable(
    reciprocity_symmetry_ambiguity,
    'Lausanne''s minority section is reciprocal (mirror obligations for the Muslim minority of Western Thrace). Does symmetric obligation sustain the guarantor frame, or does asymmetric invocation convert supervision into a one-sided leverage instrument?',
    'Compare invocation frequency, supervisory intensity, and compliance pressure applied to each side''s minority regime; assess whether the guarantor state accepts parallel external scrutiny of Western Thrace.',
    'Asymmetric invocation would harden the obligated state''s resistance and push the arrangement toward contested leverage rather than shared supervision; demonstrated symmetry would stabilize the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_symmetry_ambiguity, empirical, 'Reciprocity''s effect on the supervision frame''s legitimacy.').

omega_variable(
    general_mechanism_substitutability,
    'Can general European human-rights mechanisms fully substitute for Lausanne-specific supervision, completing the arrangement''s transition, or does treaty-specific content (foundation legal personality, the Patriarchate''s status, minority schools) exceed what Convention provisions reach?',
    'Doctrinal comparison of Strasbourg outcomes on Lausanne-pleaded versus Convention-only theories; identify protection gaps that survive when the treaty hook is absent.',
    'Full substitutability means the transitional justification is complete and the reading can retire without loss; non-substitutability means the arrangement remains load-bearing indefinitely and its temporary character is fictional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(general_mechanism_substitutability, conceptual, 'Whether the transitional justification has completed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__guarantor_reading, 1923, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lausanne_guarantor_tr_t1923, lausanne_minority_protections__guarantor_reading, theater_ratio, 1923, 0.15).
narrative_ontology:measurement(lausanne_guarantor_tr_t1946, lausanne_minority_protections__guarantor_reading, theater_ratio, 1946, 0.34).
narrative_ontology:measurement(lausanne_guarantor_tr_t1955, lausanne_minority_protections__guarantor_reading, theater_ratio, 1955, 0.56).
narrative_ontology:measurement(lausanne_guarantor_tr_t1971, lausanne_minority_protections__guarantor_reading, theater_ratio, 1971, 0.51).
narrative_ontology:measurement(lausanne_guarantor_tr_t1990, lausanne_minority_protections__guarantor_reading, theater_ratio, 1990, 0.43).
narrative_ontology:measurement(lausanne_guarantor_tr_t1999, lausanne_minority_protections__guarantor_reading, theater_ratio, 1999, 0.34).
narrative_ontology:measurement(lausanne_guarantor_tr_t2010, lausanne_minority_protections__guarantor_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(lausanne_guarantor_tr_t2026, lausanne_minority_protections__guarantor_reading, theater_ratio, 2026, 0.5).

% Extraction over time
narrative_ontology:measurement(lausanne_guarantor_be_t1923, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1923, 0.45).
narrative_ontology:measurement(lausanne_guarantor_be_t1946, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1946, 0.26).
narrative_ontology:measurement(lausanne_guarantor_be_t1955, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1955, 0.2).
narrative_ontology:measurement(lausanne_guarantor_be_t1971, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1971, 0.22).
narrative_ontology:measurement(lausanne_guarantor_be_t1990, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(lausanne_guarantor_be_t1999, lausanne_minority_protections__guarantor_reading, base_extractiveness, 1999, 0.36).
narrative_ontology:measurement(lausanne_guarantor_be_t2010, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(lausanne_guarantor_be_t2026, lausanne_minority_protections__guarantor_reading, base_extractiveness, 2026, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(lausanne_guarantor_su_t1923, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1923, 0.42).
narrative_ontology:measurement(lausanne_guarantor_su_t1946, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1946, 0.3).
narrative_ontology:measurement(lausanne_guarantor_su_t1955, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1955, 0.24).
narrative_ontology:measurement(lausanne_guarantor_su_t1971, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1971, 0.23).
narrative_ontology:measurement(lausanne_guarantor_su_t1990, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1990, 0.31).
narrative_ontology:measurement(lausanne_guarantor_su_t1999, lausanne_minority_protections__guarantor_reading, suppression_requirement, 1999, 0.37).
narrative_ontology:measurement(lausanne_guarantor_su_t2010, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2010, 0.34).
narrative_ontology:measurement(lausanne_guarantor_su_t2026, lausanne_minority_protections__guarantor_reading, suppression_requirement, 2026, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__restrictive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__guarantor_reading, lausanne_minority_protections__expansive_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Lausanne minority protections' conflates three structurally distinct claims: the SCOPE of covered rights (restrictive vs expansive) and the LOCUS OF ENFORCEMENT (international supervision vs domestic interpretation). This story isolates the enforcement-locus claim; the sibling stories isolate the scope claims. Causal structure within the family: the guarantor reading is upstream of the expansive reading (it supplies the external channel through which institutional-continuity claims travel) and coexists with the restrictive reading as opposed live positions. Epsilon differs across the family by construction: the guarantor arrangement as operated extracts weakly and episodically (eps ~0.30); the expansive program would concentrate contest on high-value institutional assets; the restrictive regime concentrates interpretive discretion domestically. Each member links to the others via network.affects_constraints; no orphan stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
