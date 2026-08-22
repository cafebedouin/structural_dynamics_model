% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Treaty Stewardship Substrate (Relational Reading)
 *   domain: legal/anthropological/constitutional
 *
 * SUMMARY:
 *   The stewardship reading of the historical treaty substrate interprets
 *   treaties as relational covenants establishing mutual obligations for
 *   territorial coexistence, not as property transactions completing
 *   Indigenous dispossession. Under this reading, Indigenous nations retain
 *   territorial jurisdiction and co-management authority; the settler state
 *   accepts binding duties to seek consent and honor shared governance. The
 *   reading is contested: extinguishment interpretations (treaties as
 *   completed land sales) and nation-to-nation interpretations (treaties as
 *   bilateral sovereign agreements) occupy different institutional and
 *   jurisprudential spaces. This constraint story instantiates only the
 *   stewardship reading as a clean ε-invariant structure: extraction (0.38,
 *   moderate), suppression (0.62, substantial), theater (0.51, near-parity).
 *   The temporal series show extraction rising slightly through the interval
 *   then stabilizing (learning curve effect: stewardship governance initially
 *   costly to implement, then routinized), suppression declining slightly
 *   (resistance increasing as Indigenous nations build institutional
 *   capacity), and theater holding near 0.5 (neither side's governance is
 *   pure performance; both elements are operational).
 *
 * KEY AGENTS:
 *   - Indigenous nations: moderate power, identity-locked, territorial stewards and decision-makers in joint governance
 *   - Settler state: institutional power, arbitrage-enabled, obligated to recognize Indigenous jurisdiction but structurally incentivized to extract unilaterally
 *   - Resource extractors (mining, logging, hydro, agriculture): organized power, constrained exit, operationally subject to shared stewardship approvals
 *   - Settler ecology departments: institutional power, analytical exit, beneficiaries of superior ecological outcomes from joint governance
 *   - Competing treaty-reading institutional bases (extinguishment law, bilateral treaty doctrine): excluded by the stewardship reading's ontology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.38).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.62).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Treaty Stewardship Substrate (Relational Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal/anthropological/constitutional").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, 'e7fe55be-2c17-469a-9fd8-952bc4c02f59').
narrative_ontology:cs_kernel_codification('e7fe55be-2c17-469a-9fd8-952bc4c02f59', fixed_text).
narrative_ontology:cs_authority_grounding('e7fe55be-2c17-469a-9fd8-952bc4c02f59', lineage).
narrative_ontology:cs_interpretation_layer_present('e7fe55be-2c17-469a-9fd8-952bc4c02f59').
narrative_ontology:cs_reading_relation('e7fe55be-2c17-469a-9fd8-952bc4c02f59', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('e7fe55be-2c17-469a-9fd8-952bc4c02f59', historical_treaty_substrate__nation_to_nation_reading, influences).
narrative_ontology:cs_axiom('e7fe55be-2c17-469a-9fd8-952bc4c02f59', foundational, relational_sovereignty_no_cession).
narrative_ontology:cs_axiom_status(relational_sovereignty_no_cession, holdable).
narrative_ontology:cs_axiom_grounding('e7fe55be-2c17-469a-9fd8-952bc4c02f59', relational_sovereignty_no_cession, deontological).
narrative_ontology:cs_axiom('e7fe55be-2c17-469a-9fd8-952bc4c02f59', foundational, coexistence_obligation_perpetual).
narrative_ontology:cs_axiom_status(coexistence_obligation_perpetual, holdable).
narrative_ontology:cs_axiom_grounding('e7fe55be-2c17-469a-9fd8-952bc4c02f59', coexistence_obligation_perpetual, deontological).
narrative_ontology:cs_reference_frame('e7fe55be-2c17-469a-9fd8-952bc4c02f59', treaty_as_relational_covenant).
narrative_ontology:cs_drift_state('e7fe55be-2c17-469a-9fd8-952bc4c02f59', contemporary_settler_state_encroachment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e7fe55be-2c17-469a-9fd8-952bc4c02f59', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state_ecological_stability).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, indigenous_nations_rights_non_recognition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_ecology_institutions).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, settler_resource_extractors).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, relational_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, coexistence_obligation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under the stewardship reading, Indigenous nations retain territorial jurisdiction and co-management authority over lands and resources within treaty areas. They benefit from the treaty's recognition of coexistence and mutual obligation, and from the documented ecological and cultural outcomes of shared stewardship governance. However, they also bear continuous costs: enforcing the settler state's compliance with shared governance obligations requires institutional resources; they must perpetually defend their authority against settler state encroachment and reinterpretation; and their ability to renegotiate or exit is constrained by historical power asymmetries and the fact that territorial stewardship is constitutive of Indigenous nationhood — exit would mean abandoning the ancestral relationship to place that defines them as nations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_nations, beneficiary,
    moderate, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, indigenous_nations, payer).

% Under the stewardship reading, the settler state is bound by mutual obligations to recognize Indigenous jurisdiction, consult on resource use, and manage shared territorial stewardship. The state administers the treaty framework and its enforcement mechanisms through court interpretation, legislative action, and executive policy. It benefits from ecological stability and resource predictability through joint management; it bears costs in limiting unilateral resource extraction, funding joint governance institutions, and in the overhead of ongoing consent-seeking and dispute resolution. Its exit options are substantial and asymmetric: it can unilaterally reinterpret the treaty via court decision, restrict Indigenous governance through legislation, or simply enforce stewardship obligations selectively — this arbitrage-capacity is the persistent source of extraction from Indigenous nations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Mining, logging, hydroelectric, and agricultural industries operate under the constraint that they cannot extract unilaterally from treaty territories. They must seek consent from Indigenous nations as co-managers, operate under environmental and cultural standards set by joint governance, and accept resource limitations. They pay through reduced extraction rates (resource caps set by stewardship governance), operational delays (consultation and consent processes), and compliance costs (monitoring, restoration, cultural accommodation). Their exit is constrained: relocating extraction often means moving to equivalent treaty territories with identical constraints, or to international jurisdictions with increasing treaty-like environmental governance requirements.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_resource_extractors, payer,
    organized, biographical, constrained, regional).

% Environmental and conservation departments benefit from stewardship reading implementation: joint governance with Indigenous nations produces superior long-term ecological outcomes (documented in comparative studies of forest management, fishery sustainability, and wildfire prevention across North America and Oceania). They derive legitimacy and technical effectiveness from recognizing Indigenous ecological knowledge and enforcing mutual stewardship obligations. They have no extraction conflict and observe the constraint as alignment with their institutional mandate and evidence base.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_ecology_institutions, beneficiary,
    institutional, generational, analytical, national).

% The settler state's constitutional property law and historical-legal regimes that interpret treaties as completed land transactions (extinguishment) are structurally excluded from this stewardship reading's operation. They would argue that treaties legally ended Indigenous jurisdiction and that the settler state's unilateral control of treaty territories is the established legal order. Their exclusion is enforced by the stewardship reading's ontological commitment: if treaties are relational covenants requiring perpetual shared stewardship and no cession of sovereignty, then the extinguishment reading becomes incoherent within the same legal framework.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, extinguishment_reading_institutional_base, excluded,
    institutional, generational, trapped, national).

% International law regimes interpreting treaties as bilateral agreements between sovereign equals (nation-to-nation reading) are structurally excluded from pure stewardship reading operation, though they have begun to influence it through incorporation of consent-requiring principles. The nation-to-nation reading would argue that treaties are instruments of international law binding two sovereign states, not ongoing relational covenants binding populations to shared territory. The stewardship reading's relational sovereignty ontology (territorial coexistence prior to sovereignty separation) makes the bilateral framework incoherent within a single legal order, though both can coexist if applied to different treaty contexts or sequentially across institutional domains.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, bilateral_sovereignty_reading_institutional_base, excluded,
    institutional, generational, trapped, national).

% Indigenous nations that were never party to formal treaties (or whose treaties were broken without renegotiation or restoration) are structurally excluded from stewardship reading benefits and governance participation. The constraint applies only to nations that have signed treaties; it does nothing to restore jurisdiction or co-management rights to dispossessed peoples outside the treaty substrate. Their exclusion is not choice but historical accident: they would benefit from the same stewardship principles and relational sovereignty recognition but have no treaty anchor to deploy in claiming it.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, non_treaty_indigenous_peoples, excluded,
    powerless, civilizational, trapped, regional).

% Courts and legislatures interpret and enforce the treaty substrate, deciding whether a given settler action violates stewardship obligations, whether Indigenous jurisdiction is recognized, and whether shared governance is honored. They hold the interpretive power to shift between readings (stewardship, extinguishment, nation-to-nation) through judgment and statute, though they are formally bound to respect treaty text. They have analytical distance from the constraint (they do not directly extract or coordinate resources) but structural power over its operation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__stewardship_reading, settler_state).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Joint territorial stewardship and resource management: the settler state and Indigenous nations coordinate on land use, resource extraction, ecological preservation, and spatial planning. This coordination solves the foundational problem of managing shared territory where unilateral extraction by either party degrades ecological stability and social coexistence. Relational sovereignty doctrine holds that this coordination does not require territorial cession from either party — only mutual obligation to recognize jurisdiction, consult decisions, and honor resource limitations set by joint stewardship.
% TRANSFER_FUNCTION: The constraint transfers decision-making authority and resource-access rights from unilateral settler control to shared Indigenous-settler governance. It transfers obligations: the settler state accepts binding duties to seek Indigenous consent, recognize Indigenous territorial jurisdiction, and honor resource limitations and environmental standards set by joint stewardship. Indigenous nations receive recognized authority and power to veto resource extraction in treaty territories. Settler state extractive operatives (mining, logging, hydroelectric, agricultural companies) transfer operational authority to shared governance bodies that include Indigenous decision-makers.
% ABSENT_VOICES: The extinguishment reading institutional base (settler constitutional property law regimes treating treaties as completed land sales) is excluded from this stewardship reading's operation — it would argue that treaties legally ended Indigenous sovereignty and that unilateral settler control is the established legal order. The nation-to-nation reading institutional base (international treaty law doctrine treating treaties as bilateral sovereign agreements) is also excluded, though it has begun to influence stewardship through consent-incorporation — it would argue treaties are interstate agreements, not ongoing relational covenants binding populations to shared territory. Non-treaty Indigenous peoples are excluded by historical accident: they have no treaty anchor and thus no standing in stewardship reading governance.
% DISAPPEARANCE_RATIONALE: If the stewardship reading disappeared overnight — if treaties reverted in law and practice to extinguishment interpretation or bilateral framing without stewardship obligations — territorial governance and resource allocation would collapse into unilateral settler control. Indigenous nations would lose recognized jurisdiction and co-management authority, becoming consultative advisors at most. Resource extraction would accelerate in treaty territories. Ecological management would degrade (documented outcomes from abandonment of joint stewardship in historical cases). The settler state would lose the institutional legitimacy and demonstrated ecological outcomes stewardship governance produces. Land use patterns, resource allocation, political authority, and environmental condition would all reorganize toward unilateral settler dominance and maximum resource extraction.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).
:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The stewardship reading is classified as tangled_rope because it combines genuine coordination (joint territorial stewardship produces ecological outcomes neither party could achieve unilaterally) with asymmetric extraction (the settler state extracts institutional legitimacy, resource access, and political power despite accepting formal obligation-parity; Indigenous nations bear the continuous cost of enforcing settler compliance). Extraction is moderate (0.38) rather than high because the constraint genuinely coordinates ecological management and because Indigenous nations retain jurisdiction — neither party unilaterally dominates. However, extraction is not negligible because the settler state's arbitrage-enabling power means it can reinterpret, restrict, or violate stewardship obligations without immediate consequences; Indigenous nations must continuously defend shared governance against encroachment. Suppression is substantial (0.62) because the constraint's persistence depends on actively excluding or marginalizing the competing readings (extinguishment, bilateral) from institutional effect. Theater at 0.51 (near-parity) reflects that stewardship governance has genuine operational content (resource decisions are actually made jointly, ecological management is actually improved) alongside performative elements (settler states often ceremonialize Indigenous consent while operationally maintaining dominance). The temporal series show extractiveness stabilizing after rising through the learning phase: as joint governance becomes institutionalized, the initial overhead costs subside but the underlying structural asymmetry persists. Suppression declines as Indigenous institutional capacity grows and resistance increases. Theater remains near 0.5: neither pure coordination nor pure theater, but a mixed regime.
 *
 * PERSPECTIVAL GAP:
 *   The Indigenous nations seat and the settler state seat should compute differently in per-seat classification. From the Indigenous perspective, the stewardship reading is genuine coordination offering partial protection of jurisdiction against historical dispossession — a rope-like constraint with asymmetric enforcement costs but real functional benefit. From the settler state perspective, the constraint limits unilateral resource access and obliges consent-seeking — a snare-like extraction masked as coordination. The engine computes this divergence from the structural data: Indigenous nations as moderate power with identity-locked exit face a constraint that benefits them (shared governance) while extracting continuous enforcement labor; the settler state as institutional power with arbitrage-exit faces a constraint that obliges it formally but enables unilateral reinterpretation. Neither seat sees the constraint the same way because neither bears the same structural relationship to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are entered as both beneficiary (they retain and can exercise territorial jurisdiction under the stewardship reading) and as victim in the form of 'indigenous_nations_rights_non_recognition' (the settler state's persistent breach of stewardship obligations and reinterpretation of treaties as extinguishment is an extractive violation). This captures the dual structural relationship: stewardship reading recognition benefits Indigenous nations; settler state non-compliance extracts from them. The settler state's ecology departments are entered as beneficiary (they achieve conservation goals through stewardship that unilateral settler extraction would prevent). Resource extractors are entered as payer (they bear costs through constrained access). The settler state itself is agenda_setter: it administers the treaty framework, sets enforcement policy, and can shift between readings (extinguishment, bilateral, stewardship) depending on institutional interests. Directionality flows from these role assignments: Indigenous nations approach d=0.5 (equal beneficiary-victim tension); extractors approach d=1.0 (targets of the shared-governance constraint); settler state approaches d=0.3 (beneficiary through legitimacy and ecology, but obligated — not fully exempted).
 *
 * MANDATROPHY ANALYSIS:
 *   The stewardship reading avoids classification as pure snare by the genuine coordination function it performs: joint territorial stewardship and resource management is not cover for extraction but its actual function. However, the reading is vulnerable to a mandatrophy charge: if the founding problem (unsustainable territorial conflict, ecological degradation) were genuinely solved by widespread adoption of the stewardship reading, the reading would have transformed from constraint (ongoing obligation) to background norm (uncontested practice). Empirically, the reading remains contested and embattled — settler states and extinguishment law repeatedly reassert dominance, Indigenous nations must continuously defend stewardship against encroachment, and the theater ratio near 0.5 indicates that performative compliance is substantial. This suggests the founding problem is LIVE but the reading's protective power is LIMITED — stewardship governance works where implemented but is not self-sustaining against settler-state reinterpretation. The tangled_rope classification captures this: the coordination works, but its persistence requires active enforcement (the settler state's compliance machinery and Indigenous institutional capacity are both necessary), and the settler state's asymmetric exit options mean it can unilaterally shift the reading (via court reinterpretation, legislative amendment, or simply refusing enforcement) without triggering countervailing power sufficient to restore stewardship. No mandate is dead; the constraint persists. But the mandate is under continuous erasure pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    settler_state_reinterpretation_risk,
    'Can the settler state unilaterally shift from stewardship reading interpretation to extinguishment or bilateral reading through court reinterpretation, constitutional amendment, or enforcement non-compliance?',
    'Observing whether settler state institutions accept stewardship governance as binding across multiple electoral cycles and institutional turnovers, or whether each change in government or court composition permits reinterpretation toward extraction.',
    'If the settler state can shift readings at will, stewardship is a piton (inertial theater without enforcement capacity), not a tangled_rope. If shifting requires constitutional or treaty-amendment processes that entrench stewardship, the constraint is genuinely active. Classification would shift to piton if unilateral reinterpretation is discovered to be routine practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(settler_state_reinterpretation_risk, empirical, 'Whether settler state commitment to stewardship reading is structurally bound or reversible at institutional discretion.').

omega_variable(
    indigenous_institutional_capacity_asymmetry,
    'Is the suppression requirement (0.62) being borne primarily by Indigenous nations (enforcing settler compliance) or by settler extractive industries (constrained by shared governance)?',
    'Analysis of institutional resource allocation: what fraction of treaty-enforcement labor and cost falls on Indigenous governance bodies versus settler state and extractive-industry compliance machinery?',
    'If Indigenous nations bear most enforcement cost, the constraint is more extractive (victims paying to enforce obligations against the beneficiary) and approaches snare classification. If both parties bear enforcement cost proportional to benefit, classification as tangled_rope is sustained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_institutional_capacity_asymmetry, empirical, 'Distribution of enforcement labor burden between Indigenous nations and settler institutions.').

omega_variable(
    coexistence_vs_domination_ontology,
    'Is the stewardship reading genuinely committed to relational coexistence (both parties structurally necessary to governance), or is it a settler-state concession that still treats Indigenous participation as consultative rather than decisional?',
    'Examining whether Indigenous nations have veto power (rejection of resource extraction, constitutional amendment) or only voice (input into decisions made by settler institutions). Veto = relational coexistence; voice = settler-dominated consultation.',
    'If Indigenous nations have structural veto, the reading is relational and coexistence-committed. If they have voice-only, the reading is itself a tangled_rope masking settler domination under relational language — it would be reclassified as snare from the Indigenous seat. This is an axiom-disambiguation: does the reading instantiate genuine relational sovereignty or just Indigenous consultation?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_vs_domination_ontology, conceptual, 'Whether stewardship reading commits to relational coexistence with genuine decision-parity or to settler-dominated consultation with Indigenous advisory role.').

omega_variable(
    non_treaty_peoples_exclusion,
    'Is the stewardship reading''s limitation to treaty peoples a feature (treaties as the legitimate anchor for stewardship governance) or a bug (exclusion of dispossessed peoples without treaty substrate)?',
    'Examining whether stewardship principles are extended to non-treaty Indigenous peoples through reparative treaties, land-back programs, or co-governance of un-ceded territories; or whether the reading actively denies non-treaty peoples the same stewardship status.',
    'If stewardship principles are extended to non-treaty peoples, the reading''s universalizability is intact. If stewardship applies only to treaty peoples, the reading enables a two-tier Indigenous status (treaty stewards / non-treaty dispossessed) and is more extractive at the system level (it protects some Indigenous nations while abandoning others to unilateral settler dominance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_treaty_peoples_exclusion, empirical, 'Whether stewardship reading extends beyond treaty peoples to support claims of non-treaty Indigenous nations.').

omega_variable(
    relational_sovereignty_vs_bilateral_convergence,
    'Is the distinction between the stewardship reading (relational sovereignty with no cession) and the nation-to-nation reading (bilateral sovereignty with ongoing consent) a substantive legal and political difference, or do they converge operationally once both commit to consent-requiring governance?',
    'Comparing court decisions, governance outcomes, and institutional power-sharing under stewardship framing versus nation-to-nation framing in the same territory; assessing whether the readings produce different resource-allocation patterns, dispute-resolution mechanisms, or Indigenous-settler power distributions.',
    'If the readings converge operationally, the distinction is doctrinal theater (both readings mask settler state reinterpretation risk). If they diverge in outcomes (stewardship produces more Indigenous-centered governance, nation-to-nation produces more symmetric bilateral negotiation), the readings are genuinely distinct constraints. Theater_ratio may be higher than 0.51 if the readings are substantially convergent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(relational_sovereignty_vs_bilateral_convergence, empirical, 'Whether stewardship and nation-to-nation readings are distinct constraints or doctrinal variants of the same bilateral-consent logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.58).
narrative_ontology:measurement_basis(hist_tr_t0, observed).
narrative_ontology:measurement(hist_tr_t5, historical_treaty_substrate__stewardship_reading, theater_ratio, 5, 0.56).
narrative_ontology:measurement_basis(hist_tr_t5, observed).
narrative_ontology:measurement(hist_tr_t10, historical_treaty_substrate__stewardship_reading, theater_ratio, 10, 0.54).
narrative_ontology:measurement_basis(hist_tr_t10, observed).
narrative_ontology:measurement(hist_tr_t15, historical_treaty_substrate__stewardship_reading, theater_ratio, 15, 0.53).
narrative_ontology:measurement_basis(hist_tr_t15, observed).
narrative_ontology:measurement(hist_tr_t20, historical_treaty_substrate__stewardship_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement_basis(hist_tr_t20, observed).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__stewardship_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement_basis(hist_tr_t25, observed).
narrative_ontology:measurement(hist_tr_t30, historical_treaty_substrate__stewardship_reading, theater_ratio, 30, 0.51).
narrative_ontology:measurement_basis(hist_tr_t30, observed).
narrative_ontology:measurement(hist_tr_t35, historical_treaty_substrate__stewardship_reading, theater_ratio, 35, 0.51).
narrative_ontology:measurement_basis(hist_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.29).
narrative_ontology:measurement_basis(hist_be_t0, observed).
narrative_ontology:measurement(hist_be_t5, historical_treaty_substrate__stewardship_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement_basis(hist_be_t5, observed).
narrative_ontology:measurement(hist_be_t10, historical_treaty_substrate__stewardship_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(hist_be_t10, observed).
narrative_ontology:measurement(hist_be_t15, historical_treaty_substrate__stewardship_reading, base_extractiveness, 15, 0.37).
narrative_ontology:measurement_basis(hist_be_t15, observed).
narrative_ontology:measurement(hist_be_t20, historical_treaty_substrate__stewardship_reading, base_extractiveness, 20, 0.39).
narrative_ontology:measurement_basis(hist_be_t20, observed).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__stewardship_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement_basis(hist_be_t25, observed).
narrative_ontology:measurement(hist_be_t30, historical_treaty_substrate__stewardship_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement_basis(hist_be_t30, observed).
narrative_ontology:measurement(hist_be_t35, historical_treaty_substrate__stewardship_reading, base_extractiveness, 35, 0.38).
narrative_ontology:measurement_basis(hist_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__stewardship_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(hist_su_t0, observed).
narrative_ontology:measurement(hist_su_t5, historical_treaty_substrate__stewardship_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(hist_su_t5, observed).
narrative_ontology:measurement(hist_su_t10, historical_treaty_substrate__stewardship_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(hist_su_t10, observed).
narrative_ontology:measurement(hist_su_t15, historical_treaty_substrate__stewardship_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement_basis(hist_su_t15, observed).
narrative_ontology:measurement(hist_su_t20, historical_treaty_substrate__stewardship_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement_basis(hist_su_t20, observed).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__stewardship_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(hist_su_t25, observed).
narrative_ontology:measurement(hist_su_t30, historical_treaty_substrate__stewardship_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(hist_su_t30, observed).
narrative_ontology:measurement(hist_su_t35, historical_treaty_substrate__stewardship_reading, suppression_requirement, 35, 0.62).
narrative_ontology:measurement_basis(hist_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__stewardship_reading, 0.14).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% The stewardship_reading is one reading of the historical_treaty_substrate kernel. It coexists with the extinguishment_reading (treaties as completed property transactions) and influences the nation_to_nation_reading (treaties as bilateral agreements subject to modern treaty law). All three readings share the same kernel (the stabilized commitment: treaties) but instantiate different constraint structures with different beneficiary/victim relationships and extraction profiles. The stewardship reading treats territorial jurisdiction and co-management as the constraint's core; extinguishment treats territorial cession as the constraint's core; nation-to-nation treats bilateral sovereignty and ongoing consent as the constraint's core. Each reading carries a different ε: stewardship at 0.38 (moderate extraction from Indigenous nations' non-recognition of settler obligations), extinguishment at higher ε (Indigenous nations as victims of dispossession), nation-to-nation at intermediate ε (symmetric bilateral obligations with persistent power asymmetry). This decomposition follows ε-invariance: changing how the treaty relationship is interpreted changes what extraction looks like, so each reading is a distinct constraint with its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
