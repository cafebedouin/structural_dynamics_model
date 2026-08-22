% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__localized_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__localized_practice_reading, []).

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
 *   constraint_id: jati_practice_norm__localized_practice_reading
 *   human_readable: Jati Boundary Coordination Norms — Localized Practice Reading
 *   domain: social_anthropology/political_economy
 *
 * SUMMARY:
 *   In thousands of towns and villages across South Asia, jati
 *   (sub-caste/community) boundaries organize everyday life: whom one may
 *   marry, which families exchange grain and services at life-cycle rituals,
 *   which households apprentice children into trades, who receives first
 *   honors at temple festivals, and how local disputes are settled. The
 *   arrangement modeled here is the practice-grounded form of that boundary
 *   order: norms maintained by marriage-pooling, reciprocity, and
 *   reputational sanction rather than by a dedicated coercive apparatus;
 *   administered case by case by local councils and assemblies; and
 *   continuously renegotiated — jatis fission and merge, ranks are contested
 *   and re-ranked, and enumerations exceed 3000 distinct communities
 *   precisely because the category system keeps generating new members.
 *   Enforcement is diffuse and frequently defied; exit through migration,
 *   new-community formation, or defiant union is costly but real. KEY AGENTS
 *   (by structural relationship): - jati_panchayat_elders: agenda-setting
 *   administration (organized/identity_locked) — polices boundaries, approves
 *   marriages, mediates disputes - dominant_landowning_jatis: primary
 *   beneficiary (powerful/constrained) — tops the local ritual and marriage
 *   order, captures residual deference - merchant_and_artisan_jatis:
 *   beneficiary (organized/mobile) — runs the credit circles and
 *   apprenticeship chains the boundaries pool - endogamy_constrained_women:
 *   primary target (powerless/trapped) — bears the sharpest edge of the
 *   marriage rules - hereditary_service_jatis: target with reciprocal
 *   cushioning (powerless/constrained) — supplies hereditary services, owes
 *   public deference - urban_jati_sabhas: secondary coordinator and collector
 *   (organized/mobile) — converts shared origin into scholarships, hostels,
 *   matrimonial listings - intercaste_marriage_advocates: excluded voice
 *   (moderate/mobile) — boundary-crossers and their counselors, outside the
 *   council process - social_anthropologists: analytical observer
 *   (analytical/analytical) — records how the norms actually operate
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__localized_practice_reading, 0.3).
domain_priors:suppression_score(jati_practice_norm__localized_practice_reading, 0.24).
domain_priors:theater_ratio(jati_practice_norm__localized_practice_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(jati_practice_norm__localized_practice_reading, resistance, 0.26).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__localized_practice_reading, rope).
narrative_ontology:human_readable(jati_practice_norm__localized_practice_reading, "Jati Boundary Coordination Norms — Localized Practice Reading").
narrative_ontology:topic_domain(jati_practice_norm__localized_practice_reading, "social_anthropology/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__localized_practice_reading, '27c8db0f-a804-4cbd-95a6-a1eb41bc7403').
narrative_ontology:cs_kernel_codification('27c8db0f-a804-4cbd-95a6-a1eb41bc7403', distributed).
narrative_ontology:cs_authority_grounding('27c8db0f-a804-4cbd-95a6-a1eb41bc7403', practice).
narrative_ontology:cs_interpretation_layer_present('27c8db0f-a804-4cbd-95a6-a1eb41bc7403').
narrative_ontology:cs_reading_relation('27c8db0f-a804-4cbd-95a6-a1eb41bc7403', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_reading_relation('27c8db0f-a804-4cbd-95a6-a1eb41bc7403', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('27c8db0f-a804-4cbd-95a6-a1eb41bc7403', foundational, boundary_legitimacy_from_local_negotiation).
narrative_ontology:cs_axiom_status(boundary_legitimacy_from_local_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('27c8db0f-a804-4cbd-95a6-a1eb41bc7403', boundary_legitimacy_from_local_negotiation, conventional).
narrative_ontology:cs_axiom('27c8db0f-a804-4cbd-95a6-a1eb41bc7403', secondary, renegotiation_is_not_deviation).
narrative_ontology:cs_axiom_status(renegotiation_is_not_deviation, holdable).
narrative_ontology:cs_axiom_grounding('27c8db0f-a804-4cbd-95a6-a1eb41bc7403', renegotiation_is_not_deviation, conventional).
narrative_ontology:cs_reference_frame('27c8db0f-a804-4cbd-95a6-a1eb41bc7403', locally_negotiated_coordination_order).
narrative_ontology:cs_drift_state('27c8db0f-a804-4cbd-95a6-a1eb41bc7403', post_reform_urbanization_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('27c8db0f-a804-4cbd-95a6-a1eb41bc7403', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__localized_practice_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, merchant_and_artisan_jatis).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, dominant_landowning_jatis).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, jati_panchayat_elders).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, endogamy_constrained_women).
narrative_ontology:constraint_victim(jati_practice_norm__localized_practice_reading, hereditary_service_jatis).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, hereditary_service_jatis).
narrative_ontology:constraint_beneficiary(jati_practice_norm__localized_practice_reading, urban_jati_sabhas).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Preside over boundary questions: approve or refuse proposed marriages, adjudicate precedence at festivals, mediate disputes between member households, and occasionally sanction boundary-crossings with temporary boycott or fine. Their standing in the town rests on being seen as fair stewards of these norms; the deference they receive is payment for the role, and withdrawing from it would cost them the public identity the role constitutes.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, jati_panchayat_elders, agenda_setter,
    organized, generational, identity_locked, local).

% Hold the top ranks of the local marriage and ritual order: their daughters marry into comparable households in neighboring towns, their heads receive first honors at processions, and tenant and service households owe them customary deference. They contribute land, festival sponsorship, and dispute-settlement guarantees that keep the local order running. Leaving the locality would mean surrendering accumulated standing for anonymous urban life, so they stay and maintain what maintains them.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, dominant_landowning_jatis, beneficiary,
    powerful, generational, constrained, regional).

% Run the credit circles, apprenticeship chains, and trade networks that the boundary system pools: members lend to each other below market rate, train each other's sons, and share market information because membership certifies trustworthiness. The same boundaries confine them to inherited trade niches, but their capital and clientele travel well, and branches in distant towns keep the option of relocation open.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, merchant_and_artisan_jatis, beneficiary,
    organized, generational, mobile, national).

% Carry the sharpest edge of the marriage rules: a household's honor rides on whom its daughters marry, courtship is negotiated by parents, and marriage typically moves a woman into her husband's household away from her own kin. When a boundary is crossed, the ostracism falls hardest on her. Education and city employment open exits for a minority, but most exit routes require severing the kin support that structures daily life, so the ordinary position is staying inside terms she did not set.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, endogamy_constrained_women, payer,
    powerless, biographical, trapped, local).

% Provide hereditary services — barbering, washing, drumming, grave-digging — to patron households in return for customary payments and ritual rations, and sit at the foot of local precedence. The reciprocal obligations cushion scarcity and guarantee clients, but they fix the occupational slot and require public deference. Seasonal migration to cities now offers wages outside the arrangement, and younger members take it, loosening the old binding year by year.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, hereditary_service_jatis, payer,
    powerless, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, hereditary_service_jatis, beneficiary).

% Urban welfare associations organized by jati: they collect dues from scattered urban members and convert shared origin into concrete services — scholarship funds, hostel beds, matrimonial listings, lobbying for educational grants. Their officers set association agendas and can pivot purpose if members stop paying, so their attachment to the boundary categories is instrumental and revisable.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, urban_jati_sabhas, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__localized_practice_reading, urban_jati_sabhas, agenda_setter).

% Couples who married across jati lines and the activists who counsel them. They hold that the boundaries should not govern marriage at all, and they campaign for legal protection and social acceptance. They have no seat in the council deliberations that set local rules; they rely on courts, employers, and city anonymity rather than community consent, and they bear the ostracism that the councils decline to lift.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, intercaste_marriage_advocates, excluded,
    moderate, biographical, mobile, national).

% Field researchers who record how the boundary arrangements actually operate: genealogies, marriage ledgers, council minutes, service contracts. They publish analyses of how the norms are negotiated, enforced, and evaded, and their accounts are available to every party, but they collect nothing from the arrangement and direct nothing in it.
narrative_ontology:constraint_stakeholder(jati_practice_norm__localized_practice_reading, social_anthropologists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__localized_practice_reading, dominant_landowning_jatis).
narrative_ontology:fixing_cost_class(jati_practice_norm__localized_practice_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools marriage alliances into trusted sets whose members are vetted by generations of interaction; certifies creditworthiness inside occupational networks; transmits skills through apprenticeship chains; insures members against scarcity through reciprocal obligation; and settles local disputes and precedence where formal state reach is thin.
% TRANSFER_FUNCTION: Moves marriageability, ritual precedence, customary deference, and occupational opportunity along boundary lines: from outsiders and lower-ranked members toward in-group members and higher-ranked jatis, in modest per-household amounts aggregated across many transactions.
% ABSENT_VOICES: Intercaste couples and their advocates would object that the boundaries should not govern marriage, and they are structurally outside the council process that sets local rules. Service jatis attend the councils but speak from the foot of the room; women most affected by marriage decisions are represented by their households rather than seated. The unanimity of local consent is partly an artifact of who was never given a seat.
% DISAPPEARANCE_RATIONALE: If the boundary norms vanished overnight, marriage markets would reorganize around new trust signals (class, religion, region, education), the intra-jati credit and apprenticeship networks would need replacement institutions or dissolve into costlier formal finance, festival precedence and service reciprocities would lose their schedule, and local status politics would regroup around other axes. Nothing physical breaks, but the social plumbing of thousands of towns rearranges within a generation.
% FOUNDING_PROBLEM: Pre-colonial local societies needed trusted marriage pools, reliable skill transmission, mutual insurance under scarcity, and dispute settlement in settings where state authority was distant or intermittent; jati organization bundled those solutions into one boundary-and-membership system.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration from outside the beneficiary set exists: anti-caste reformist scholarship and movement archives (which document both the protective functions and their costs), anthropological fieldwork recording service-jati and women's own accounts of what the arrangement gave and took, and development-economics studies of caste-network credit confirming the live coordination half. Dominant-jati elders self-attest the founding problem as fully live; their testimony is not relied upon here, and the contested status reflects the split between the corroborated-live halves (marriage pooling, mutual aid) and the corroborated-dead half (hereditary occupational binding).
narrative_ontology:disappearance_verdict(jati_practice_norm__localized_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__localized_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__localized_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__localized_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__localized_practice_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__localized_practice_reading_tests).
:- end_tests(jati_practice_norm__localized_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low-to-moderate (0.30 at interval end) because most participants are net beneficiaries — the boundaries pool marriage alliances, certify creditworthiness inside occupational networks, and insure members against scarcity — while the real costs concentrate on two seats: women bound by endogamy and service jatis owing deference. Suppression is low (0.24) because enforcement is diffuse reputational sanctioning plus occasional council penalties, not a dedicated coercive machine; the proliferation of categories is the visible signature of weak central enforcement. Theater is low (0.18) but rising slowly: as jajmani-style functional interdependence decays, a growing share of activity is status performance (origin-myth claims, honorific titles, matrimonial prestige filtering) rather than working coordination. Accessibility_collapse is low (0.32) because alternatives remain substantially open — new jati formation, urban anonymity, inter-jati union at the price of ostracism — which is exactly what a weakly enforced coordination order looks like. Resistance is low-moderate (0.26): defiant marriages, rank-contestation campaigns, and quiet occupational defection occur constantly but rarely as organized mass resistance, because most participants consent. On the identity-coordination gaming risk: the identity frame here is load-bearing but genuine — the boundary system actually delivers trust certification, marriage pooling, and mutual aid, not merely a cover story; the rising theater series is the early-warning indicator that would expose cover-story drift if the functional half finished decaying. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine (by directionality and scope). All three temporal series run on one shared six-point grid (1951–2021) so no metric borrows another's end-state value at earlier times.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the merchant and artisan jatis' position the boundary order is infrastructure: it certifies the trustworthiness that makes below-market intra-jati lending possible. From the dominant landowning jatis' position it is a precedence order they staff and sponsor. From the women's seat the same rules are a narrowed life: marriage choice made by others, post-marital relocation, and ostracism that lands hardest on the woman when a boundary is crossed. From the service jatis' seat it is a slot assignment with a ration attached. The elders' seat adds an identity-lock dynamic of the institutional kind: the council's role constitutes its members' public standing, so even their perception of exit cost is fused with the arrangement they administer — if that identity frame broke (standing decoupled from stewardship), their seat would compute far less invested. The engine derives these per-seat classifications from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: merchant_and_artisan_jatis (mobile exit, organized) sit near the beneficiary end; dominant_landowning_jatis sit low-d but capture the residual gains — deference, first honors, hypergamous marriage inflows — which is why gain_flow names that seat; jati_panchayat_elders derive low d as administrators whose compensation is standing. Victim declarations drive the target end: endogamy_constrained_women combine the highest cost concentration with trapped exit, placing them nearest the full-target pole; hereditary_service_jatis are also high-d but their improving exit (seasonal urban migration) damps effective extraction relative to a fully trapped seat. Urban sabhas sit near symmetric — they collect dues and deliver services in roughly balanced flow. Intercaste advocates are excluded rather than coordinated: they are outside the derivation by design, and their absence is recorded in absent_voices. Scope effects are modest here: the arrangement operates at local-to-regional scope, so the engine's verification-difficulty amplification is mild compared to a universal-scope constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim prevents mislabeling this arrangement as pure extraction: the coordination function is real and measurable (marriage pooling, credit certification, apprenticeship pipelines, dispute settlement where state reach is thin), and suppressing that fact would misread deference asymmetries as a snare. The converse guard matters equally: the falling suppression_requirement series documents genuine enforcement decay (council boycott powers eroded by courts and out-migration), and the slowly rising theater_ratio documents functional atrophy — if the functional half finishes decaying while boundary-policing persists as performance, the arrangement drifts piton-ward and the corpus should expect a future story to catch that transition. Mandatrophy is deliberately NOT declared resolved: the founding problem is contested (marriage pooling and mutual aid remain live demands; hereditary occupational binding is dead), so the arrangement is neither zombie nor obsolete. The R5 mismatch consumer reads founding_problem_status=contested x disappearance_verdict=world_rearranges as no capture/zombie flag, consistent with the computed path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is one reading of the jati_practice_norm kernel. Where exactly do the three readings disagree structurally, and what would adopting a sibling change?',
    'Comparative reclassification across the three stories: hold the referent (the standing local boundary arrangement) fixed and re-author epsilon, beneficiary/victim structure, and enforcement profile under each reading''s epistemic premises.',
    'Under orthodox_textual_reading the same arrangement acquires doctrinal enforcement and a pollution-sanction victim set (epsilon rises sharply); under colonial_census_reading extraction concentrates in administrative legibility rents and the beneficiary set shifts toward the enumerating state. This file''s rope verdict holds only within the practice-grounded reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Committer-frame location of the kernel contest among the three declared readings of jati_practice_norm.').

omega_variable(
    proliferation_enforcement_inference,
    'Does proliferation to 3000+ enumerated categories evidence weak enforcement, or does jati fission preserve endogamy by relocating it into smaller, more intensely policed units?',
    'Compare marriage-endogamy rates and sanction incidence across jatis that have and have not undergone recent fission; trace whether splinter groups tighten or relax their marriage rules relative to the parent body.',
    'If fission is enforcement-preserving, measured suppression understates the arrangement''s coercive floor and the identity-coordination coupling warrants stricter review; if splinters relax rules, the weak-enforcement inference behind the low suppression score stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proliferation_enforcement_inference, empirical, 'Whether category proliferation indicates weak enforcement or decentralized strong enforcement.').

omega_variable(
    endogamy_suppression_mechanism,
    'Is the suppression that maintains endogamy structural (family economic dependency, community sanction) or internalized (preference formation that treats same-jati marriage as simply natural)?',
    'Post-exit trajectory of women who leave via education and urban employment: if endogamous preference persists after structural pressure is removed, reclassify as partially internalized; supplement with cohort attitude surveys across generations.',
    'If substantially internalized, effective suppression exceeds the structural measure and travels with the agent after exit; the payer seat''s computed classification hardens accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(endogamy_suppression_mechanism, empirical, 'Structural versus internalized suppression mechanism sustaining the marriage boundary.').

omega_variable(
    participant_scope_net_benefit,
    'Whose net benefit grounds the coordination verdict — does counting only male household heads as ''participants'' manufacture the favorable result?',
    'Recompute the household ledger weighting each member''s costs and benefits separately, including the marriage-choice value lost by women and the deference costs borne by service jatis, against their shares of pooled credit, mutual aid, and dispute protection.',
    'If women''s and service jatis'' costs outweigh their benefit shares, the arrangement fails the net-beneficiary test for those seats and per-seat classifications should diverge from the story-level rope claim — which is signal, not error.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(participant_scope_net_benefit, conceptual, 'Scope-of-participation ambiguity underlying the net-beneficiary assessment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__localized_practice_reading, 1951, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t1951, jati_practice_norm__localized_practice_reading, theater_ratio, 1951, 0.1).
narrative_ontology:measurement(jati_tr_t1965, jati_practice_norm__localized_practice_reading, theater_ratio, 1965, 0.11).
narrative_ontology:measurement(jati_tr_t1979, jati_practice_norm__localized_practice_reading, theater_ratio, 1979, 0.13).
narrative_ontology:measurement(jati_tr_t1993, jati_practice_norm__localized_practice_reading, theater_ratio, 1993, 0.14).
narrative_ontology:measurement(jati_tr_t2007, jati_practice_norm__localized_practice_reading, theater_ratio, 2007, 0.16).
narrative_ontology:measurement(jati_tr_t2021, jati_practice_norm__localized_practice_reading, theater_ratio, 2021, 0.18).

% Extraction over time
narrative_ontology:measurement(jati_be_t1951, jati_practice_norm__localized_practice_reading, base_extractiveness, 1951, 0.38).
narrative_ontology:measurement(jati_be_t1965, jati_practice_norm__localized_practice_reading, base_extractiveness, 1965, 0.36).
narrative_ontology:measurement(jati_be_t1979, jati_practice_norm__localized_practice_reading, base_extractiveness, 1979, 0.34).
narrative_ontology:measurement(jati_be_t1993, jati_practice_norm__localized_practice_reading, base_extractiveness, 1993, 0.33).
narrative_ontology:measurement(jati_be_t2007, jati_practice_norm__localized_practice_reading, base_extractiveness, 2007, 0.31).
narrative_ontology:measurement(jati_be_t2021, jati_practice_norm__localized_practice_reading, base_extractiveness, 2021, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1951, jati_practice_norm__localized_practice_reading, suppression_requirement, 1951, 0.36).
narrative_ontology:measurement(jati_su_t1965, jati_practice_norm__localized_practice_reading, suppression_requirement, 1965, 0.33).
narrative_ontology:measurement(jati_su_t1979, jati_practice_norm__localized_practice_reading, suppression_requirement, 1979, 0.3).
narrative_ontology:measurement(jati_su_t1993, jati_practice_norm__localized_practice_reading, suppression_requirement, 1993, 0.27).
narrative_ontology:measurement(jati_su_t2007, jati_practice_norm__localized_practice_reading, suppression_requirement, 2007, 0.25).
narrative_ontology:measurement(jati_su_t2021, jati_practice_norm__localized_practice_reading, suppression_requirement, 2021, 0.24).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__localized_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, orthodox_textual_reading).
narrative_ontology:affects_constraint(jati_practice_norm__localized_practice_reading, colonial_census_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'jati/caste boundaries' decomposes into three structurally distinct constraints sharing one kernel, per the epsilon-invariance principle: the practice-grounded coordination order (this file — low epsilon, diffusely enforced, rope-shaped), the scriptural-doctrinal order (orthodox_textual_reading — doctrinal enforcement, pollution sanctions, sharply higher epsilon), and the administrative-legibility order (colonial_census_reading — enumeration-stabilized categories serving governance, extraction accruing to legibility rents). Each carries its own epsilon, beneficiary/victim structure, and enforcement profile. Causal texture across the family: colonial enumeration historically hardened the categories that local practice now renegotiates within (upstream census -> downstream practice), while this reading's proliferation evidence feeds back as pressure on the census reading's stability premise (downstream practice -> upstream census, influences-edge authored in cs_structure.reading_relations). Linking all three lets contamination analysis track how reification upstream hardens practice downstream.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
