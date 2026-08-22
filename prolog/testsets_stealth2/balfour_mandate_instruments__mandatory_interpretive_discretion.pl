% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: Balfour Mandate Instruments — Mandatory Interpretive Discretion Reading
 *   domain: international law/colonial administration/state formation
 *
 * SUMMARY:
 *   Between 1920 and 1947 the British mandatory administration governed
 *   Palestine under a charter whose key terms — 'a national home for the
 *   Jewish people,' protection of the 'civil and religious rights' of
 *   'existing non-Jewish communities,' facilitation of Jewish immigration and
 *   'close settlement' — admitted irreconcilable readings. The arrangement
 *   under contest here is not any particular policy outcome but the
 *   adjudication arrangement itself: the mandatory's claim to final,
 *   externally-unreviewable interpretive authority, exercised through White
 *   Papers (1922, 1930, 1939), successive land regimes (the 1920 settlement
 *   versus the 1940 Land Transfer Regulations), and emergency legislation.
 *   Each exercise re-baselined the contest: reliance built under one
 *   determination was destroyed by the next, and neither community could
 *   appeal to fixed textual meaning or external arbitration. This story is
 *   one reading of the kernel balfour_mandate_instruments (see
 *   kernel_context); its epsilon (0.58) is authored for THIS reading's
 *   referent — the discretion arrangement as such — and differs from the
 *   epsilon the substantive sibling stories would carry, because their
 *   victim/beneficiary structures invert. KEY AGENTS (by structural
 *   relationship): - british_colonial_administrators: Primary beneficiary and
 *   agenda-setter (institutional/arbitrage) — holds final interpretive
 *   authority; collects policy flexibility and divide-and-rule capacity;
 *   personally mobile across postings - colonial_office_establishment:
 *   Agenda-setter and beneficiary (institutional/arbitrage) — drafts the
 *   reinterpretation instruments in London; institutional continuity beyond
 *   individual careers - palestinian_arab_community: Primary target
 *   (powerless/trapped) — bears every reinterpretation; no legislature,
 *   leadership exiled during the revolt, no appeal to fixed meaning -
 *   jewish_yishuv_community: Primary target (organized/identity_locked) —
 *   bears reversals (especially 1939) with strong institutions but no
 *   external recourse; exit would abandon the national project itself -
 *   permanent_mandates_commission: Analytical observer
 *   (institutional/analytical) — hears petitions, questions the mandatory
 *   annually, cannot compel - league_council: Excluded reviewer
 *   (institutional/analytical) — nominal supervisor whose review powers never
 *   became binding
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.58).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.64).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.58).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "Balfour Mandate Instruments — Mandatory Interpretive Discretion Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international law/colonial administration/state formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, 'eb7acf5d-04dc-46ed-acc1-1911e5d05222').
narrative_ontology:cs_kernel_codification('eb7acf5d-04dc-46ed-acc1-1911e5d05222', fixed_text).
narrative_ontology:cs_authority_grounding('eb7acf5d-04dc-46ed-acc1-1911e5d05222', extraction).
narrative_ontology:cs_interpretation_layer_present('eb7acf5d-04dc-46ed-acc1-1911e5d05222').
narrative_ontology:cs_reading_relation('eb7acf5d-04dc-46ed-acc1-1911e5d05222', balfour_mandate_instruments__jewish_national_home_primacy, influences).
narrative_ontology:cs_reading_relation('eb7acf5d-04dc-46ed-acc1-1911e5d05222', balfour_mandate_instruments__dual_obligation_indigenous_rights, influences).
narrative_ontology:cs_axiom('eb7acf5d-04dc-46ed-acc1-1911e5d05222', foundational, mandatory_adjudicative_supremacy).
narrative_ontology:cs_axiom_status(mandatory_adjudicative_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('eb7acf5d-04dc-46ed-acc1-1911e5d05222', mandatory_adjudicative_supremacy, conventional).
narrative_ontology:cs_axiom('eb7acf5d-04dc-46ed-acc1-1911e5d05222', secondary, adaptive_interpretation_fiduciary_duty).
narrative_ontology:cs_axiom_status(adaptive_interpretation_fiduciary_duty, holdable).
narrative_ontology:cs_axiom_grounding('eb7acf5d-04dc-46ed-acc1-1911e5d05222', adaptive_interpretation_fiduciary_duty, instrumental).
narrative_ontology:cs_reference_frame('eb7acf5d-04dc-46ed-acc1-1911e5d05222', mandatory_plenary_interpretive_authority).
narrative_ontology:cs_drift_state('eb7acf5d-04dc-46ed-acc1-1911e5d05222', mandate_end_un_referral_1947, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('eb7acf5d-04dc-46ed-acc1-1911e5d05222', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, colonial_office_establishment).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, palestinian_arab_community).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_yishuv_community).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__mandatory_interpretive_discretion, plenary_mandatory_trusteeship).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__mandatory_interpretive_discretion, administrative_finality_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the day-to-day government of Palestine: issue ordinances, set immigration schedules under quota authority, administer land registration and transfer controls, and publish the policy statements that fix what the Mandate's terms mean for the current period. Each determination is theirs alone to make and theirs alone to revise; no external body can overturn one. Officials serve limited tours and rotate onward to other posts or London, carrying careers that do not depend on any single determination surviving.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, agenda_setter,
    institutional, biographical, arbitrage, national).

% In London, draft and defend the policy instruments — the 1922, 1930, and 1939 statements, the land regulations — and answer to Parliament and to Geneva for them. The office's continuity outlasts any secretary of state, and its working method presumes that the meaning of the Mandate's terms is settled administratively, case by case, rather than judicially or by treaty amendment. Its flexibility to reframe commitments is a standing institutional asset.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, colonial_office_establishment, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__mandatory_interpretive_discretion, colonial_office_establishment, beneficiary).

% Farming families, town notables, and national committees living under successive land-registration regimes, transfer restrictions, and quota decisions issued without their consent. They hold no elected legislature; during the 1936-39 revolt their leadership was detained or deported and villages subjected to collective penalties. Petition to Geneva is possible but advisory only. Leaving means forfeiting land and livelihood; staying means accepting whatever the next determination brings.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, palestinian_arab_community, payer,
    powerless, generational, trapped, national).

% Immigrant-settler institutions — the Agency, elected assemblies, labor federations, defense organizations — building towns, farms, and industry under immigration certificates and land consents that arrive by administrative schedule and can be cut off by the next statement, as 1939 demonstrated. They lobby London and Washington effectively and maintain extensive institutions, but no court or council can compel a determination in their favor. Their entire national project is anchored in the territory; departure would dissolve it.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, jewish_yishuv_community, payer,
    organized, generational, identity_locked, national).

% Meets in Geneva to examine the mandatory's annual report, hear petitions from both communities, and put written questions to the administering power. Its findings are advisory: it can record doubts about consistency between policy statements and the Mandate's terms — as it did over the 1939 statement — but cannot compel revision, and its sessions end with recommendations the mandatory may accept or set aside.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% Holds formal supervisory authority over the Mandate and must approve amendments, but its practical review never becomes binding: debates are deferred, consensus deferred to the mandatory's assurances, and its one assertive move — the 1939 decision to request an International Court opinion on the White Paper's consistency — was overtaken by the war and never answered. The decisions that actually govern are taken in London and Jerusalem.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_council, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__mandatory_interpretive_discretion, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Someone must issue binding decisions — on land tenure, immigration, public order — while the governing instrument's key terms admit irreconcilable readings; the arrangement solves the deadlock of contested meaning by vesting final interpretive authority in the administering power, so governance continues while the meaning-contest stays open.
% TRANSFER_FUNCTION: Moves interpretive finality and policy-setting initiative from both resident communities to the mandatory administration; each community's reliance interests — land titles, immigration expectations, communal rights — become assets priced and repriced by successive administrative determinations rather than secured by fixed text.
% ABSENT_VOICES: Both communities lacked a binding voice: Palestinian Arabs had no elected legislature, and their leadership was detained or exiled during the revolt; the Yishuv's consultative access never bound outcomes. External reviewers — the Permanent Mandates Commission and the League Council — could question but not compel. The seats that would have demanded fixed textual meaning or external arbitration stood structurally outside the decision loop.
% DISAPPEARANCE_RATIONALE: If the discretion arrangement vanished overnight — replaced by fixed-meaning adjudication or a binding external tribunal — both communities' strategies would reorganize around enforceable textual commitments: land transactions would price certainty instead of administrative goodwill, immigration planning would anchor to treaty text instead of quota schedules, and constitutional demands would shift from petitioning the mandatory to litigating the charter. The administration would lose the flexibility rent and the divide-and-rule capacity simultaneously.
% FOUNDING_PROBLEM: The Mandate inherited a territory whose governing charter incorporated the Balfour Declaration's promise of a 'national home' alongside protections for existing communities' rights — terms pointing in incompatible directions for two peoples — and the administering power needed a working method to govern at all before anyone agreed what the charter meant.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Permanent Mandates Commission's annual reports repeatedly recorded the impossibility of reconciling the charter's dual commitments and questioned the mandatory's shifting interpretations; the 1937 Peel Commission report documented that the ambiguity made stable administration impossible; both communities' leaderships attested the problem from opposed directions. No corroborating source outside the administration treats the discretion arrangement itself as the solution — external attestations concern the problem, not the arrangement's adequacy.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type snare is authored from structure, independently of the metrics: the arrangement's coordination story (someone must decide while the charter's terms admit irreconcilable readings) is real but partial — the specific form, self-adjudication without external review, exceeds what coordination requires, and that excess is precisely what persisted unchanged through every policy reversal. Persistence depended on coercive enforcement (emergency regulations from 1936, military suppression of the Arab Revolt, interdiction of unauthorized immigration) and on suppressing exits (no binding appeal channel existed at any point in the interval); victims are identifiable on both sides. Metrics are authored independently as descriptive facts: epsilon 0.58 (moderate — the mandatory supplied real governance services and particular determinations sometimes ran in each community's favor, but every favorable determination was a revocable prerogative rather than an entitlement, which is the specific injury this reading identifies); suppression 0.64 as a raw structural property, unscaled by power or scope (only extractiveness is scaled, by the engine); theater_ratio 0.44 (the inquiry-commission cycle — Shaw, Hope Simpson, Peel, Woodhead, Anglo-American — performed deliberation whose conclusions bound nothing); accessibility_collapse 0.60 (petition channels to Geneva existed but were advisory; courts could review ordinances for ultra vires defects but never the interpretive core itself); resistance 0.72 (general strike and armed revolt 1936-39; organized unauthorized immigration and paramilitary defiance after 1939). Temporal note: surface policy oscillated violently while the underlying metric trajectories deepen monotonically — the oscillation is the mechanism, and its effect on both communities compounds rather than cancels, because each reversal repriced reliance built under its predecessor. The suppression_requirement series is authored because this narrative specifically tracks enforcement-capacity change: machinery built up through the revolt, partially demobilized after 1945, then redeployed against immigration — a net hardening. Coalition check: joint victim action was structurally foreclosed by the arrangement itself — under oscillating discretion each community's rational strategy was to bid for the next determination rather than jointly demand fixed rules; the abortive Legislative Council proposals of the 1930s foundered on exactly this, with each community rejecting power-sharing designed for the other's containment.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the same arrangement presents as prudent stewardship: flexibility as fiduciary duty, adaptation to circumstance, refusal to be bound by predecessors' errors. From either payer seat it presents as unappealable power: the identical discretion that reads as prudence from London and Jerusalem reads as arbitrariness from Jaffa and Tel Aviv. Same-level divergence: the two communities occupy the same nominal position — subjects of the same mandatory under the same instruments — yet compute differently. The Yishuv's organized institutions (the Agency, elected assemblies, labor federations, London and Washington leverage) and its identity-locked exit give it bargaining presence inside the discretion system; the Arab community's powerlessness and trapped exit leave it exposed to each determination without offsetting leverage. The engine computes this divergence from the power and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the administrators collect interpretive finality, managerial autonomy, and the capacity to play commitments off each other — directionality near the beneficiary end, reinforced by arbitrage-grade personal exit (tours, rotation, promotion, retirement elsewhere) even as they administer trapped populations. Both communities sit near the full-target end: each bears the transfer of interpretive finality, and neither holds an appeal channel. The derivation differentiates them by power and exit: the Yishuv's organization and identity-lock place it somewhat nearer the middle than the Arab community, since organized pressure could shape the timing and packaging of determinations even though it could never touch the prerogative itself. Episodic favorable determinations (pro-facilitation readings in the 1920s running toward the Yishuv; the 1930 and 1939 protective readings running toward Arab landholders) do not create beneficiary positions in this constraint, because each such gain was denominated in the mandatory's continuing goodwill and revocable at the next statement — the asset itself was the uncertainty. No directionality overrides are authored: the derivation from declared beneficiaries, victims, power, and exit produces the correct structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing under an irreducibly ambiguous charter — stayed live for the entire interval, so the arrangement cannot be dismissed as a mandate outliving its function; the status=live and verdict=world_rearranges pairing should read as consistent, not as a zombie flag. The classification discipline cuts both ways: labeling the arrangement a rope would erase the asymmetric extraction — the flexibility the administrator collected was paid for by both communities' certainty; labeling it a structure with no coordination function at all would erase the real adjudication burden any administrator of that charter faced. The snare claim preserves the structure: a genuine coordination need covered by an arrangement whose durable core — unreviewability — served the administrator rather than the governed. The arrangement was not resolved by atrophy: it was terminated by external dissolution (the 1947 referral to the United Nations), which is why no mandatrophy_resolved declaration is authored.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates one reading (mandatory_interpretive_discretion) of the kernel balfour_mandate_instruments; the sibling readings jewish_national_home_primacy and dual_obligation_indigenous_rights assign opposite beneficiary/victim structures — under primacy the Arab community is the target and the Yishuv the beneficiary, under dual-obligation the reverse. Where is the disagreement located?',
    'Classify the two sibling stories separately and compare computed types and directionalities across the family; the disagreement is located in the locus of interpretive finality — fixed textual meaning (either substantive sibling) versus operative content constituted anew by each mandatory adjudication (this reading).',
    'Under either substantive sibling, this story''s victim set splits into a single target and a single beneficiary seat and epsilon redistributes accordingly; the discretion arrangement itself would demote from the operative constraint to a mere enforcement instrument of whichever substantive reading won.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel contest: three readings of the same instruments assign incompatible beneficiary/victim structures.').

omega_variable(
    discretion_necessity_vs_construction,
    'Does governing an ambiguously-worded charter require self-adjudication by the administering power, or were external-review mechanisms (binding League arbitration, International Court referral, mixed tribunals) practically available and declined?',
    'Archival assessment of declined review proposals at each juncture: the early-1920s land-dispute commission proposals, the 1929 Western Wall commission precedent, and the 1939 League Council vote to seek an International Court advisory opinion (overtaken by war) — evaluated for feasibility had the mandatory cooperated.',
    'If external review was feasible and declined, the adjudication-necessity cover thins and the arrangement moves toward pure extraction with higher effective epsilon; if genuinely unavailable, part of the measured extraction is the unavoidable price of governing at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_necessity_vs_construction, empirical, 'Whether unreviewable discretion was structurally necessary or a chosen construction.').

omega_variable(
    oscillation_intent_ambiguity,
    'Was the policy oscillation (White Papers 1922/1930/1939; land regimes 1920 versus 1940) an intended management strategy — keeping both communities dependent on the next determination — or the by-product of sincere but successive attempts to balance irreconcilable commitments?',
    'Cabinet and Colonial Office minutes across the 1929-1939 sequence: did officials weigh the dependence effects of reversal on each community, or only the substantive merits of each policy?',
    'Intended oscillation consolidates the snare reading and attributes the full flexibility rent to the administrator seat; sincere balancing shifts weight toward a hybrid picture in which a real coordination burden carries extractive side-effects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oscillation_intent_ambiguity, empirical, 'Intent behind the interpretive oscillations that produced path-dependent lock-in.').

omega_variable(
    authority_ground_ambiguity,
    'Is the adjudicative authority''s operative warrant the League conferral (a lineage claim: San Remo, Mandate instrument, Article 22 trusteeship) or the benefit the administration draws from unreviewability (an extraction claim)?',
    'Counterfactual test against the record: would the administration have accepted binding external review had the League offered it with full legitimacy guarantees? Observed resistance to the 1939 referral attempt and to every binding-review proposal supplies the behavioral evidence.',
    'Lineage grounding would support the reading''s own legitimacy frame and soften the cs_structure classification; extraction grounding confirms the classification and predicts the authority''s behavior under review pressure, which is what the 1939-1947 record shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_ground_ambiguity, conceptual, 'Whether the discretion regime''s authority rests on conferred trust or on retained advantage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 1920, 1947).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1920, 0.18).
narrative_ontology:measurement_basis(balf_tr_t1920, observed).
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1922, 0.2).
narrative_ontology:measurement_basis(balf_tr_t1922, observed).
narrative_ontology:measurement(balf_tr_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1929, 0.26).
narrative_ontology:measurement_basis(balf_tr_t1929, observed).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1930, 0.3).
narrative_ontology:measurement_basis(balf_tr_t1930, observed).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1936, 0.34).
narrative_ontology:measurement_basis(balf_tr_t1936, observed).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1939, 0.38).
narrative_ontology:measurement_basis(balf_tr_t1939, observed).
narrative_ontology:measurement(balf_tr_t1945, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1945, 0.42).
narrative_ontology:measurement_basis(balf_tr_t1945, observed).
narrative_ontology:measurement(balf_tr_t1947, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1947, 0.44).
narrative_ontology:measurement_basis(balf_tr_t1947, observed).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1920, 0.36).
narrative_ontology:measurement_basis(balf_be_t1920, observed).
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1922, 0.4).
narrative_ontology:measurement_basis(balf_be_t1922, observed).
narrative_ontology:measurement(balf_be_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1929, 0.44).
narrative_ontology:measurement_basis(balf_be_t1929, observed).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1930, 0.47).
narrative_ontology:measurement_basis(balf_be_t1930, observed).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1936, 0.51).
narrative_ontology:measurement_basis(balf_be_t1936, observed).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1939, 0.58).
narrative_ontology:measurement_basis(balf_be_t1939, observed).
narrative_ontology:measurement(balf_be_t1945, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1945, 0.56).
narrative_ontology:measurement_basis(balf_be_t1945, observed).
narrative_ontology:measurement(balf_be_t1947, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1947, 0.58).
narrative_ontology:measurement_basis(balf_be_t1947, observed).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1920, 0.25).
narrative_ontology:measurement_basis(balf_su_t1920, observed).
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1922, 0.28).
narrative_ontology:measurement_basis(balf_su_t1922, observed).
narrative_ontology:measurement(balf_su_t1929, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1929, 0.4).
narrative_ontology:measurement_basis(balf_su_t1929, observed).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1930, 0.38).
narrative_ontology:measurement_basis(balf_su_t1930, observed).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1936, 0.62).
narrative_ontology:measurement_basis(balf_su_t1936, observed).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1939, 0.68).
narrative_ontology:measurement_basis(balf_su_t1939, observed).
narrative_ontology:measurement(balf_su_t1945, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement_basis(balf_su_t1945, observed).
narrative_ontology:measurement(balf_su_t1947, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1947, 0.64).
narrative_ontology:measurement_basis(balf_su_t1947, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the Mandate' conflates three structurally distinct constraints. Two are substantive readings of what the instruments direct (jewish_national_home_primacy; dual_obligation_indigenous_rights); one is a meta-reading of who decides what they direct (this story). Measuring 'the Mandate' with a substantive observable yields one epsilon and one victim set; measuring it with the adjudicative observable yields another. The upstream/downstream structure runs through this story: whichever substantive reading prevailed at a given moment prevailed BY OPERATION of the discretion arrangement, so this constraint influences both siblings' operating environments. All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
