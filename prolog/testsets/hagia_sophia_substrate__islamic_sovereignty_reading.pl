% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__islamic_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__islamic_sovereignty_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hagia_sophia_substrate__islamic_sovereignty_reading
 *   human_readable: Hagia Sophia Islamic Sovereignty Constraint (Islamic Reading)
 *   domain: cultural_heritage/political_sovereignty/religious_authority
 *
 * SUMMARY:
 *   This constraint instantiates the Islamic sovereignty reading of the
 *   hagia_sophia_substrate kernel. On 10 July 2020, the Turkish government
 *   issued a decree and court-ordered ruling that converted Hagia Sophia from
 *   a museum (status since 1934) back to a functioning mosque under Islamic
 *   waqf authority, effective under Turkish state sovereignty. The reading
 *   asserts that legitimacy flows from the 1453 Ottoman conquest and the
 *   continuous Islamic religious endowment (waqf) that persisted through
 *   Turkish republican administration until 1934 abolition. The constraint
 *   enforces this reading against competing framings: universal cosmopolitan
 *   heritage (UNESCO reading) and Orthodox Christian restitution
 *   (ecclesiastical reading). The authored metrics describe substantially
 *   extractive, actively enforced operation: beneficiaries include the AKP
 *   coalition (political consolidation), Turkish Islamic constituency
 *   (religious identity restoration), and broader Sunni symbolic sphere.
 *   Victims include non-Muslim visitors (access restrictions), UNESCO regime
 *   (jurisdiction denial), and secular Turkish opposition (ideological
 *   defeat). The claim/metric independence is deliberate: this constraint is
 *   CLAIMED as tangled_rope (coordination of Islamic identity + extraction of
 *   political legitimacy and cultural authority) while the metrics reflect
 *   moderate-high extractiveness (0.68), substantial suppression (0.72), and
 *   moderate theater (0.41). The engine will measure whether the coordination
 *   frame (shared Islamic worship identity) can be sustained against the
 *   structural data showing asymmetric extraction (political power
 *   consolidation).
 *
 * KEY AGENTS:
 *   - akp_political_coalition: Institutional agenda-setter; controls executive and judiciary; issues and enforces the decree; collects political legitimacy from Islamic base.
 *   - turkish_islamic_constituency: Organized beneficiary; gain mosque access and symbolic restitution of Ottoman Islamic sovereignty; participate in reconstructed religious identity.
 *   - sunni_ummah_symbolic: Organized beneficiary (global level); gain transnational soft-power signal that Turkey recognizes Islamic authority; symbolic prestige of restitution.
 *   - non_muslim_visitors: Powerless payer; face access restrictions, prayer-time closures, modest-dress enforcement; treated as guests in sacred space rather than equal stakeholders.
 *   - unesco_jurisdiction: Institutional payer; loses de facto authority over site's universal heritage status; cannot enforce mandate; role reduced to protest and delisting threats.
 *   - secular_turkish_opposition: Moderate power payer; face ideological and symbolic defeat; lose influence over site's meaning within Turkey; objections are politically marginalized.
 *   - orthodox_christian_diaspora: Excluded; structurally barred from participation in status governance; have no constitutional standing in Turkish framework; claim historical and spiritual title but lack institutional lever.
 *   - international_heritage_bodies: Observer seat; document and monitor constraint operation; lack enforcement power; role is diagnostic and normative commentary.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__islamic_sovereignty_reading, 0.72).
domain_priors:theater_ratio(hagia_sophia_substrate__islamic_sovereignty_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(hagia_sophia_substrate__islamic_sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__islamic_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__islamic_sovereignty_reading, "Hagia Sophia Islamic Sovereignty Constraint (Islamic Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__islamic_sovereignty_reading, "cultural_heritage/political_sovereignty/religious_authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__islamic_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__islamic_sovereignty_reading, '2fe564f9-266e-4c59-8676-1a523fe2c214').
narrative_ontology:cs_kernel_codification('2fe564f9-266e-4c59-8676-1a523fe2c214', fixed_text).
narrative_ontology:cs_authority_grounding('2fe564f9-266e-4c59-8676-1a523fe2c214', extraction).
narrative_ontology:cs_reading_relation('2fe564f9-266e-4c59-8676-1a523fe2c214', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_reading_relation('2fe564f9-266e-4c59-8676-1a523fe2c214', hagia_sophia_substrate__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('2fe564f9-266e-4c59-8676-1a523fe2c214', foundational, conquest_establishes_perpetual_islamic_title).
narrative_ontology:cs_axiom_status(conquest_establishes_perpetual_islamic_title, holdable).
narrative_ontology:cs_axiom_grounding('2fe564f9-266e-4c59-8676-1a523fe2c214', conquest_establishes_perpetual_islamic_title, conventional).
narrative_ontology:cs_axiom('2fe564f9-266e-4c59-8676-1a523fe2c214', foundational, waqf_endowment_persists_across_regime_change).
narrative_ontology:cs_axiom_status(waqf_endowment_persists_across_regime_change, holdable).
narrative_ontology:cs_axiom_grounding('2fe564f9-266e-4c59-8676-1a523fe2c214', waqf_endowment_persists_across_regime_change, conventional).
narrative_ontology:cs_reference_frame('2fe564f9-266e-4c59-8676-1a523fe2c214', ottoman_islamic_sovereignty_restored).
narrative_ontology:cs_drift_state('2fe564f9-266e-4c59-8676-1a523fe2c214', contemporary_international_governance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2fe564f9-266e-4c59-8676-1a523fe2c214', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_jurisdiction).
narrative_ontology:constraint_victim(hagia_sophia_substrate__islamic_sovereignty_reading, secular_turkish_opposition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the Turkish executive and judiciary; issued the 2020 decree returning Hagia Sophia to mosque status, reversed the 1934 secular museum decree, validated the reversal through courts, and oversees daily enforcement via security and religious authority structures. Collects political legitimacy from the base Islamic constituency through the symbolic act of reclaiming the site.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition, agenda_setter,
    institutional, generational, mobile, national).

% Gain restored access to the site as a functioning mosque and regain it as a symbol of Ottoman Islamic sovereignty reestablished. The constraint validates their religious identity claim and reverses what they frame as a colonial-era secularization. They participate in prayer and witness the state's recognition of Islamic authority over the physical site.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, turkish_islamic_constituency, beneficiary,
    organized, generational, mobile, national).

% Gain a symbolic restitution of Islamic prestige through the return of a historically significant mosque to Islamic worship. The constraint's enforcement sends a signal across the Muslim world that Turkey recognizes Islamic sovereignty over a site of transnational religious significance, strengthening Turkey's soft power in the broader Islamic sphere.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, sunni_ummah_symbolic, beneficiary,
    organized, civilizational, constrained, global).

% Face restricted access (prayer times, modest dress codes, shoes removed, no tourist photo in certain areas). Non-Muslim worship is prohibited. They are treated as guests in Islamic sacred space rather than as equal visitors to a shared cultural site. Exit involves accepting exclusion from the site or traveling at inconvenient hours outside prayer times.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, non_muslim_visitors, payer,
    powerless, immediate, trapped, global).

% Loses de facto authority to oversee the site's status as a universal heritage site. UNESCO's authority to monitor the site, convene stakeholder dialogue, and protect its cosmopolitan character is denied by the Turkish state's assertion of sovereign religious use. UNESCO cannot enforce any mandate; its role is reduced to protest and possible delisting threats.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, unesco_jurisdiction, payer,
    institutional, generational, constrained, global).

% Face an ideological and symbolic defeat: their framing of Hagia Sophia as a museum representing secular Turkish modernism is overruled by the state. They lose influence over the site's symbolic meaning within Turkey. Their objections to the decree are politically marginalized; they have no institutional lever to reverse the constraint.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, secular_turkish_opposition, payer,
    moderate, biographical, constrained, national).

% Are structurally excluded from participation in or governance of the site's status. Claim historical and spiritual title based on Byzantine Christian founding and Ottoman Christian millet history, but have no standing in the Turkish constitutional or political framework to contest the decree. Their objections are treated as external interference.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, orthodox_christian_diaspora, excluded,
    powerful, civilizational, trapped, global).

% Document the shift from museum to mosque, record restrictions on non-Muslim access, assess the constraint's impact on universal heritage preservation norms. They monitor but lack enforcement power; their role is diagnostic and normative commentary on the constraint's operation.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__islamic_sovereignty_reading, international_heritage_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__islamic_sovereignty_reading, akp_political_coalition).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__islamic_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single authoritative frame for the site's religious and cultural identity: Islamic worship and Ottoman sovereignty claim are coordinated under state authority, resolving ambiguity about whether the site belongs to universal heritage, Christian restitution, or Islamic restoration claims.
% TRANSFER_FUNCTION: Transfers symbolic authority, political legitimacy, and soft power from the secular Turkish state and cosmopolitan UNESCO regime to the AKP coalition and Turkish Islamic constituency. Non-Muslim visitors and international heritage bodies lose access/authority; Turkish Islamists and the broader Sunni symbolic sphere gain prestige and validation.
% ABSENT_VOICES: Orthodox Christian leadership (the Ecumenical Patriarch, Eastern Orthodox ecclesiastical bodies) would argue for shared Christian heritage or neutral preservation status, but are excluded from Turkish constitutional standing. Greek government interests are similarly excluded from direct stake in the decision. UNESCO voices concerns but cannot participate in the Turkish executive or judicial process that produced the decree.
% DISAPPEARANCE_RATIONALE: If the constraint (the decree re-establishing mosque status and Islamic sovereignty) vanished, the site would revert to museum status within weeks, access restrictions would be lifted, non-Muslim visitors would be treated as full stakeholders, UNESCO would regain advisory authority, and the symbolic victory for Turkish Islam would be erased. The political coalition's base would face delegitimization; the site's meaning within Turkey would reorganize around secular modernity or cosmopolitan heritage.
% FOUNDING_PROBLEM: Ottoman conquest of Constantinople (1453) established Islamic sovereignty over the site and continuous waqf endowment (Islamic religious trust). The founding problem, in this reading's frame, is the 1934 secular abolition of the mosque status and waqf by Atatürk's regime—the constraint restores the original Islamic status against that secular usurpation.
% FOUNDING_PROBLEM_CORROBORATION: The AKP coalition and Turkish Islamic constituency attest the founding problem is live: the site's Islamic identity was suppressed and must be restored. Ottoman historical records confirm continuous waqf endowment from 1453 until 1934. The secular opposition and international heritage bodies attest the founding problem is dead or superseded: the secular museum was a legitimate evolution suited to cosmopolitan modernity, and returning to 1453 framing is revanchist rather than restorative. Byzantine historians and Orthodox Christian sources dispute the legitimacy of the 1453 conquest as a grounding for permanent Islamic title.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__islamic_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__islamic_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__islamic_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__islamic_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__islamic_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__islamic_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness of 0.68 (rising from 0.58 over 24 months and plateauing) reflects the constraint's operation as asymmetric transfer of cultural authority and political legitimacy from secular-cosmopolitan framing to Islamic-national framing. The initial rise indicates entrenchment as the AKP consolidated the decree against legal challenge and international pressure; the plateau suggests the main extraction is complete (the frame is established) and residual extraction holds at the achieved level. The suppression of 0.72 (rising from 0.64 and stabilizing) reflects active enforcement machinery: security screening of non-Muslim visitors, prayer-time access restrictions, dress-code enforcement, and the administrative denial of UNESCO's authority to participate in site governance. Suppression is higher than extractiveness because alternative visions (secular museum, joint governance, Orthodox restitution) are actively foreclosed—the constraint persists not because participants freely choose the Islamic reading, but because competing framings are structurally eliminated. The theater_ratio of 0.41 (rising from 0.28 to 0.41 by month 6, then stable) indicates growing performative component: the initial weeks show genuine organizational restructuring (Quranic recitation reinstated, ablution facilities refurbished, prayer schedule re-established), but by month 6 and beyond, a growing share of enforcement activity is devoted to managing international diplomatic fallout, rebutting UNESCO criticism, and staging ceremonies that signal Islamic authority to the base while asserting Turkish sovereignty to the international audience. The measurement series are authored on one shared time grid (every metric at every examined point) to enable proper lifecycle analysis. The coercion_grid tracks how the constraint operates differently at each social level: at the organizational level (AKP, religious institutions), accessibility collapse and stakes inflation are highest (0.71–0.74 at t24)—institutional actors face clear choices and high consequences; at the class level (secular Turks, Muslim community, tourist populations), stakes inflation is moderately high (0.74) and resistance is sustained (0.72), indicating sustained class-level friction; at the individual visitor level, accessibility collapse is lower (0.63) and stakes feel immediate but diffuse (0.51), consistent with individual travelers having modest bargaining power and stakes that feel real but temporary.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (AKP) computes this constraint as coordination—reestablishing shared Islamic identity and cultural sovereignty after secular suppression—and reads it as mandated restitution. The beneficiary seats (Turkish Islamic constituency, Sunni ummah symbolic) compute genuine coordination benefit: restored access to a culturally significant mosque and reaffirmed Islamic identity. The payer seats compute it very differently: non-Muslim visitors see access denial; UNESCO sees jurisdiction denial; secular Turks see ideological defeat. The excluded seat (Orthodox diaspora) sees foreclosure of their own restitution claim. The engine will compute per-seat type from power/exit/directionality data; substantial divergence between agenda-setter and payer seats is expected. The AKP has institutional power and full exit options (could revert, could compromise, could maintain), so d is near symmetric or favorable; non-Muslim visitors have powerless status and trapped exit (cannot meaningfully contest from outside Turkey), so d is near full-target; UNESCO has institutional power but constrained exit (can protest, cannot enforce in Turkish sovereignty), so d is moderate-target; secular opposition has moderate power and constrained exit (political marginalization without exit option), so d is moderate-target. These directionality differences should produce computed type divergence between institutional and payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The AKP coalition occupies the agenda-setter role with institutional power and mobile exit options—they could reverse the decree if political costs rose (e.g., if EU membership became conditional on reversion), and they could compromise on joint governance. Their directionality is near beneficiary end (d ≈ 0.2–0.3): they benefit materially (political legitimacy, base consolidation, soft power) and their exit is genuinely open. The Turkish Islamic constituency has organized power and mobile exit options (they could accept a compromise or accept the museum status); they benefit from the constraint, so d is favorable (d ≈ 0.15–0.25). The Sunni ummah symbolic benefits from soft-power signal but has constrained exit (cannot reshape Turkish policy unilaterally from outside); d is moderate-favorable (d ≈ 0.3–0.4). Non-Muslim visitors have powerless status and trapped exit (individual tourists cannot contest state decree; collective exit is slow and uncoordinated); they bear costs with no meaningful choice, so d is near full-target (d ≈ 0.85–0.95). UNESCO has institutional power (can withdraw recognition, can organize international pressure) but constrained exit in Turkish sovereign territory; d is moderate-target (d ≈ 0.65–0.75). Secular Turkish opposition has moderate power but constrained exit (political marginalization within Turkey; cannot organize competing governance without state permission); d is moderate-target (d ≈ 0.60–0.70). These directionality values are derived from beneficiary/victim declaration + power + exit options per the derivation chain, with no overrides needed—the structural data directly yields the divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—Ottoman conquest establishing Islamic endowment—is CONTESTED in status (live vs. dead): the AKP coalition and Turkish Islamic constituency attest it is live (Islamic identity was suppressed and must be restored); the secular opposition and international heritage bodies attest it is dead or superseded (the secular museum was a legitimate evolution suited to cosmopolitan modernity). The disappearance verdict is world_rearranges: if the constraint vanished, the site would revert to museum status within weeks, non-Muslim visitors would regain full access, UNESCO would regain advisory authority, and the political coalition's symbolic victory would be erased. The mismatch detection flags potential mandatrophy: founding_problem_status = contested (not 'live' with unified corroboration) + disappearance_verdict = world_rearranges (strong organizational dependency) suggests the constraint depends on ongoing political power to sustain a contested founding narrative, rather than on genuine coordination or natural necessity. The theater_ratio rising to 0.41 by month 6 and plateauing supports this: the initial reorganization was structurally real (mosque functions returned), but sustained operation increasingly relies on performative and diplomatic maintenance (managing international criticism, staging authority signals). The constraint is NOT a classical mandatrophy case (where the original function has completely atrophied and only theatrical performance remains)—the mosque genuinely operates as mosque, and Turkish Islamic participants genuinely use it. However, the constraint carries mandatrophy risk: if the political coalition loses power or if international pressure forces negotiation, the founding narrative's contestability would become acute. A stable mandatrophy resolution would require either: (1) unified corroboration of founding problem status from outside the beneficiary coalition (does not exist; Orthodox and secular sources deny it), or (2) shift to pure coordination framing without the restitution narrative (unlikely given political investment in the sovereignty claim).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ottoman_legitimacy_contestation,
    'Does the 1453 Ottoman conquest establish permanent Islamic sovereignty claim over the site, or does it represent historical possession without contemporary legitimacy transfer?',
    'International law examination of conquest-to-title transfer in cultural heritage disputes; comparative analysis of competing historical claims and theological interpretations of waqf endowment persistence across regime changes.',
    'If conquest establishes permanent title, the constraint''s legitimacy claim is structural and carries transnationally across Ottoman successor states. If conquest represents historical fact without permanent title transfer, the constraint is a modern political assertion backed by state power rather than inherited Islamic law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ottoman_legitimacy_contestation, conceptual, 'Whether Ottoman conquest grounds permanent sovereignty or represents historical possession only.').

omega_variable(
    secular_interregnum_legitimacy,
    'Does the 1934 secular museum status constitute a legitimate reorganization of the site''s governance under Turkish national law, or is it an illegitimate usurpation of Islamic endowment rights?',
    'Constitutional analysis of the 1934 decree and 2020 reversal; examination of Islamic legal doctrine on waqf persistence through regime change; corroboration from Ottoman law scholars and Turkish constitutional historians outside the AKP coalition.',
    'If the secular period is legitimate, the 2020 return is a policy choice, not a restitution—extraction rises because it reasserts a specific reading against a valid alternative. If the secular period is usurpation, the 2020 return is restoration—extraction may be re-framed as coordination recovery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_interregnum_legitimacy, conceptual, 'Whether the secular museum period was legitimate or illegitimate usurpation of Islamic title.').

omega_variable(
    kernel_reading_containment,
    'This constraint instantiates ONE reading of a contested kernel (hagia_sophia_substrate). The sibling readings—universal_heritage_reading and orthodox_restitution_reading—offer alternative legitimacy framings. Which reading''s core premise could coexist with the others within a single state framework, and which would foreclose alternatives?',
    'Hypothetical constitutional design: could Turkey''s legal system hold both the islamic_sovereignty_reading and the universal_heritage_reading simultaneously (e.g., as mosque AND UNESCO site with joint governance)? Could Turkish law hold both islamic_sovereignty_reading and orthodox_restitution_reading (shared Christian-Muslim stewardship)? Analysis of actual Turkish constitutional constraints and comparative multi-faith heritage governance models.',
    'If readings coexist, the constraint is one stable position among live alternatives, and power determines which dominates. If readings foreclose (one premise logically rules out the other), then this reading''s victory is not purely political but reflects a fundamental incompatibility—the constraint carries different normative weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_containment, conceptual, 'Whether the islamic_sovereignty_reading''s axioms foreclose or coexist with sibling readings'' core premises.').

omega_variable(
    suppression_mechanism_internalization,
    'Are the access restrictions and behavioral norms (modest dress, prayer time closures, photograph limits) structurally imposed by security and enforcement machinery, or have they been internalized into visitor expectations and self-policing?',
    'Post-barrier tracking: if restrictions were suddenly removed, would non-Muslim visitors'' comfort and access patterns remain constrained (internalized suppression) or normalize toward pre-constraint levels (purely structural suppression)? Survey analysis of visitor self-reported agency in access decisions.',
    'If suppression is purely structural, removing enforcement machinery could restore access within weeks. If partially internalized, the constraint persists through internalized norms even after structural barriers weaken, and true exit becomes harder than the measured suppression suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether access suppression is structural or internalized in visitor behavior and norms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__islamic_sovereignty_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(hagi_tr_t0, observed).
narrative_ontology:measurement(hagi_tr_t3, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement_basis(hagi_tr_t3, observed).
narrative_ontology:measurement(hagi_tr_t6, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement_basis(hagi_tr_t6, observed).
narrative_ontology:measurement(hagi_tr_t12, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(hagi_tr_t12, observed).
narrative_ontology:measurement(hagi_tr_t18, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 18, 0.41).
narrative_ontology:measurement_basis(hagi_tr_t18, observed).
narrative_ontology:measurement(hagi_tr_t24, hagia_sophia_substrate__islamic_sovereignty_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(hagi_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(hagi_be_t0, observed).
narrative_ontology:measurement(hagi_be_t3, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 3, 0.61).
narrative_ontology:measurement_basis(hagi_be_t3, observed).
narrative_ontology:measurement(hagi_be_t6, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 6, 0.64).
narrative_ontology:measurement_basis(hagi_be_t6, observed).
narrative_ontology:measurement(hagi_be_t12, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 12, 0.67).
narrative_ontology:measurement_basis(hagi_be_t12, observed).
narrative_ontology:measurement(hagi_be_t18, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement_basis(hagi_be_t18, observed).
narrative_ontology:measurement(hagi_be_t24, hagia_sophia_substrate__islamic_sovereignty_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(hagi_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 0, 0.64).
narrative_ontology:measurement_basis(hagi_su_t0, observed).
narrative_ontology:measurement(hagi_su_t3, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 3, 0.68).
narrative_ontology:measurement_basis(hagi_su_t3, observed).
narrative_ontology:measurement(hagi_su_t6, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement_basis(hagi_su_t6, observed).
narrative_ontology:measurement(hagi_su_t12, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement_basis(hagi_su_t12, observed).
narrative_ontology:measurement(hagi_su_t18, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement_basis(hagi_su_t18, observed).
narrative_ontology:measurement(hagi_su_t24, hagia_sophia_substrate__islamic_sovereignty_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement_basis(hagi_su_t24, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=24
narrative_ontology:measurement(hagi_grid_01, hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse(class), 0, 0.65).
narrative_ontology:measurement(hagi_grid_02, hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse(class), 24, 0.67).
narrative_ontology:measurement(hagi_grid_03, hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse(individual), 0, 0.6).
narrative_ontology:measurement(hagi_grid_04, hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse(individual), 24, 0.63).
narrative_ontology:measurement(hagi_grid_05, hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse(organizational), 0, 0.71).
narrative_ontology:measurement(hagi_grid_06, hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse(organizational), 24, 0.74).
narrative_ontology:measurement(hagi_grid_07, hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(hagi_grid_08, hagia_sophia_substrate__islamic_sovereignty_reading, accessibility_collapse(structural), 24, 0.62).
narrative_ontology:measurement(hagi_grid_09, hagia_sophia_substrate__islamic_sovereignty_reading, resistance(class), 0, 0.71).
narrative_ontology:measurement(hagi_grid_10, hagia_sophia_substrate__islamic_sovereignty_reading, resistance(class), 24, 0.72).
narrative_ontology:measurement(hagi_grid_11, hagia_sophia_substrate__islamic_sovereignty_reading, resistance(individual), 0, 0.7).
narrative_ontology:measurement(hagi_grid_12, hagia_sophia_substrate__islamic_sovereignty_reading, resistance(individual), 24, 0.71).
narrative_ontology:measurement(hagi_grid_13, hagia_sophia_substrate__islamic_sovereignty_reading, resistance(organizational), 0, 0.74).
narrative_ontology:measurement(hagi_grid_14, hagia_sophia_substrate__islamic_sovereignty_reading, resistance(organizational), 24, 0.75).
narrative_ontology:measurement(hagi_grid_15, hagia_sophia_substrate__islamic_sovereignty_reading, resistance(structural), 0, 0.68).
narrative_ontology:measurement(hagi_grid_16, hagia_sophia_substrate__islamic_sovereignty_reading, resistance(structural), 24, 0.7).
narrative_ontology:measurement(hagi_grid_17, hagia_sophia_substrate__islamic_sovereignty_reading, stakes_inflation(class), 0, 0.72).
narrative_ontology:measurement(hagi_grid_18, hagia_sophia_substrate__islamic_sovereignty_reading, stakes_inflation(class), 24, 0.74).
narrative_ontology:measurement(hagi_grid_19, hagia_sophia_substrate__islamic_sovereignty_reading, stakes_inflation(individual), 0, 0.48).
narrative_ontology:measurement(hagi_grid_20, hagia_sophia_substrate__islamic_sovereignty_reading, stakes_inflation(individual), 24, 0.51).
narrative_ontology:measurement(hagi_grid_21, hagia_sophia_substrate__islamic_sovereignty_reading, stakes_inflation(organizational), 0, 0.68).
narrative_ontology:measurement(hagi_grid_22, hagia_sophia_substrate__islamic_sovereignty_reading, stakes_inflation(organizational), 24, 0.71).
narrative_ontology:measurement(hagi_grid_23, hagia_sophia_substrate__islamic_sovereignty_reading, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(hagi_grid_24, hagia_sophia_substrate__islamic_sovereignty_reading, stakes_inflation(structural), 24, 0.59).
narrative_ontology:measurement(hagi_grid_25, hagia_sophia_substrate__islamic_sovereignty_reading, suppression(class), 0, 0.68).
narrative_ontology:measurement(hagi_grid_26, hagia_sophia_substrate__islamic_sovereignty_reading, suppression(class), 24, 0.7).
narrative_ontology:measurement(hagi_grid_27, hagia_sophia_substrate__islamic_sovereignty_reading, suppression(individual), 0, 0.62).
narrative_ontology:measurement(hagi_grid_28, hagia_sophia_substrate__islamic_sovereignty_reading, suppression(individual), 24, 0.64).
narrative_ontology:measurement(hagi_grid_29, hagia_sophia_substrate__islamic_sovereignty_reading, suppression(organizational), 0, 0.74).
narrative_ontology:measurement(hagi_grid_30, hagia_sophia_substrate__islamic_sovereignty_reading, suppression(organizational), 24, 0.76).
narrative_ontology:measurement(hagi_grid_31, hagia_sophia_substrate__islamic_sovereignty_reading, suppression(structural), 0, 0.66).
narrative_ontology:measurement(hagi_grid_32, hagia_sophia_substrate__islamic_sovereignty_reading, suppression(structural), 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__islamic_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__islamic_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__orthodox_restitution_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__islamic_sovereignty_reading, hagia_sophia_substrate__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% The hagia_sophia_substrate kernel decomposes into three constraint stories, each instantiating a different reading of the site's legitimacy. The islamic_sovereignty_reading (this story) holds that legitimacy flows from Ottoman conquest and continuous Islamic endowment. The orthodox_restitution_reading holds legitimacy flows from Byzantine Christian founding. The universal_heritage_reading holds legitimacy derives from cosmopolitan shared humanity. The three readings compete for authority over the site's status; their ε values differ substantially because the readings' contestability differs: Islamic sovereignty is actively contested (moderate-high ε ≈ 0.68), Orthodox restitution is excluded from Turkish constitutional standing (very high ε ≈ 0.80+), and universal heritage is denied jurisdiction by Turkish state (high ε ≈ 0.75+). Each reading is a clean constraint with stable ε and distinct beneficiary/victim structure. Links are bidirectional in affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
